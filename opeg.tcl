package req nx::test

package req nx
package require pt::rde::nx

set fh [open [file join [file dirname [info script]] "opeg.peg"] r]
set g [read $fh]
catch {close $fh}

namespace eval ::pt::rde {

  #
  # PARAM/NX runtime: pt::rde::nx
  #
  
  nx eval {

    :public method parset {script} {
      :reset {}
      :data $script
      :MAIN ; # Entrypoint for the generated code.
      :complete
    }

    :public object method pgen {frontendPeg} {

      # We might also use opeg::Rewriter here, as the OO wrapper, but
      # this would render pgen dependent on the opeg package.
      set ser [pt::peg::from::peg convert $frontendPeg]
      
      ## initialize to NX/PEG backend defaults or dummies
      pt::tclparam::configuration::nx def _ _ _  {pt::peg::to::tclparam configure}
      
      ## strip down to just the core script fragment
      pt::peg::to::tclparam configure -template {@code@}
      puts stderr ser=$ser
      set body [pt::peg::to::tclparam convert $ser]
      set cls [nx::Class new -superclasses [self] -- $body]
      return $cls
    }
  
    #
    # An auxiliary tree printer facility, for all NX-based parsers.
    #

    :public method print {input} {
      set ast [:parset $input]
      :printNode {*}$ast
    }
    
    :method printNode {{-indent ""} -last:switch symbol start end args} {
      set nrChildren [llength $args]
      set parent [expr {$nrChildren ? "+" : "-"}]
      set pipe [expr {$indent ne "" ? "|" : ""}]
      set lastChild  [expr {$last ? "\\" : $pipe}]
      set output [string cat $indent $lastChild "-" $parent "="]
      append indent [expr {$last ? "  " : "$pipe "}]
      
      puts "$output $symbol :: $start $end"
      
      for {set i 0} {$i < $nrChildren} {incr i} {
        set pargs [list -indent $indent]
        if {$i == $nrChildren-1} {
          lappend pargs -last
        }
        :printNode {*}$pargs {*}[lindex $args $i]
      }
    } 
  }

}

namespace eval ::opeg {  
  #
  # opeg::Parser
  #
  # TODO: switch to one-time generation, once the OPEG grammar itself
  # has stabilized; and we support bootstrapping.
  # 

  package require pt::pgen
  try [pt::pgen peg $g nx -class Parser -name "OPEG Grammar"] on return {} {;}

  #
  # opeg::Rewriter
  #
  # This is a component for rewriting a PEG grammar from the parsed
  # PEG frontend notation (AST) into the "serial" PEG notation. It is
  # a component wrapper around the PT (pseudo-)ensemble
  # pt::peg::from::peg::GEN.
  #

  nx::Class create Rewriter {

    foreach p [info commands ::pt::peg::from::peg::GEN::*] {
      :alias "input [namespace tail $p]" $p
    }

    :public method rewrite {frontendAst input} {
      set ::pt::peg::from::peg::input $input
      set backendAst [::pt::ast::Bottomup 1 [list [current] walk] $frontendAst]
      unset -nocomplain ::pt::peg::from::peg::input
      return $backendAst
    }

    :public method walk {ast} {
      # TODO: this fails by not building up the [next] chain, why?
      # :input {*}$ast
      puts stderr AST=$ast
      : input {*}$ast
    }

  }
  
  nx::Class create BuilderGenerator -superclasses Rewriter {

    :property {parser:object,substdefault {[Parser new]}}

    nx::Class create [self]::Class -superclass nx::Class {
      :property -accessor public generator
      :property -accessor public factory
    }

    :method rewrite {opegAst input} {
      set pegAst [next]
      ## add ctors to OPEG structure
      if {[info exists :ctors]} {
        set :ctors [dict map {nt ctors} ${:ctors} {
          if {![llength [concat {*}$ctors]]} {
            continue
          }
          set ctors
        }]
      }
      return $pegAst
    }


    :public method print {opegScript} {
      ${:parser} print $opegScript
    }
    
    :public method bgen {opegScript {modelFactory:substdefault {[ModelFactory new]}}} {
      # 1) Transform OPEG grammar into OPEG "AST"
      set opegAst [${:parser} parset $opegScript]
      # 2) Downshape OPEG "AST" into serial PEG "AST"
      set ser [:rewrite $opegAst $opegScript]
      # 3) Generate PEG+ parser bundle

      ## initialize to NX/PEG backend defaults or dummies
      pt::tclparam::configuration::nx def _ _ _  {pt::peg::to::tclparam configure}
      
      ## strip down to just the core script fragment
      pt::peg::to::tclparam configure -template {@code@}

      set body [pt::peg::to::tclparam convert $ser]
      set cls [[current class]::Class new -superclasses [namespace current]::Builder \
                   -factory $modelFactory \
                   -generator [self] -- $body]
      return $cls
    }
    
    :method "input Ctor" {s e args} {
      return [lindex $args 0 1]
    }

    :method "input Field" {s e args} {
      set args [lassign $args field]
      lappend :fields [lindex $field 1]
      puts stderr FIELDARGS=$args
      if {0} {
        ## TODO: recognize and handle ?/+/* operators
        puts stderr FIELDARGS=$args
      }
      return [lindex $args 0]
    }

    ## pt::peg::from::peg::GEN::Identifier
    # :method "input Identifier" {s e args} {
    # # args = list/1 (symbol)       | <-  Ident(ifier)
    # # args = list/n (field symbol) | <-  Field Ident(ifier)
    # if {[llength $args] == 2} {
    #      }
    # next [list $s $e {*}$args]
    # }

    :method "input Sequence" {s e args} {
      # args = list/1 (class) 
      # args = list/n (list/1 ...) (gtor prefix ...)
      set ctor [lindex $args 0]
      puts stderr <CTOR>=$ctor
      if {[llength $args] > 1 && [llength $ctor] == 1} {
        set args [lrange $args 1 end]
        if {[info exists :fields] && [llength ${:fields}]} {
          puts stderr COLLECT=${:fields}
          lappend ctor {*}${:fields}
          unset :fields
        }
      } else {
        set ctor ""
      }
      list $ctor {*}[next [list $s $e {*}$args]]
    }

    :method "input Expression" {s e args} {
      set rargs [list]
      set :choices [list]
      foreach i $args {
        set resid [lassign $i ctor]
        # TODO: stack them up for validation, over multiple levels of
        # (sub-)expressions!
        lappend :choices $ctor; 
        lappend rargs $resid
      }
      next [list $s $e {*}$rargs]
    }
        
    :method "input Definition" {s e args} {
      set def [next]
      if {[info exists :choices]} {
        dict set :ctors [lindex $def 0] ${:choices}
        unset :choices
      }
      return $def
    }
    
  }


  nx::Class create ModelFactory {
    
    :variable sourcecode
    
    :public method postOrder {varName ast script {level 0}} {
      upvar [incr level] $varName var
      set ast [lassign $ast current start end]
    
      # default to the leaf/literal value?
      foreach c $ast {
        lappend targs [:postOrder $varName $c $script $level]
      }

      if {![info exists targs]} {
        set targs [string range ${:sourcecode} $start $end]
      }
      lassign $current nt objspec      

      if {$objspec ne ""} {
        set fieldspec [lassign $objspec cls]
        set fields ""
        if {[llength $fieldspec]} {
          set fields [join [concat {*}[lmap f ${fieldspec} v $targs {list -$f $v}]]]
        }
        puts stderr FIELDS={*}$fields
        set :current [$cls new {*}$fields]
      }

      set v ""
      
      if {[:info lookup method "input $nt"] ne ""} {
        set v [: input $nt $start $end {*}$targs]
      }
      
      if {[info exists :current]} {
        set v ${:current}
        unset :current
      }
      
      if {$v eq ""} {
        # error "Either an objspec or a mapping method must be provided for non-terminal '$nt'."
        set v $targs
      }

      set var $v
      uplevel $level $script
      return $v
      
    }
  }
  
  nx::Class create Builder -superclasses pt::rde::nx {

    :public method parse {script} {

      set ast [:parset $script]

      set list {}
      set factory [[:info class] factory get]
      $factory eval [list set :sourcecode $script]
      $factory postOrder v $ast {
        if {$v ne "" && [::nsf::object::exists $v]} {
          lappend list $v
        }
      }
      $factory eval [list unset :sourcecode]
      # return START concept
      return [lindex $list end]
    }

    ## si:valuevalue_branch si:valuevoid_branch si:voidvalue_branch si:voidvoid_branch

    foreach m [[lindex [:info superclasses] end] info methods -callprotection all *_branch] {
      :method $m {} {
        set mark [${:mystackmark} peek]
        if {![info exists :choices($mark)]} {
          # init
          set :choices($mark) 0
        } 
        try {set r [next]} on return {} {return -code return}; # ok
        incr :choices($mark);
        return $r
      }
    }

    ## si:value_leaf_symbol_end si:void_leaf_symbol_end si:value_leaf_symbol_end si:value_clear_symbol_end si:reduce_symbol_end
    
    foreach m [[lindex [:info superclasses] end] info methods -callprotection all *_symbol_end] {
      :method $m {sym} {
        # si:value_symbol_start: pushes on AST stack & sets a mark
        set mark [expr {[${:mystackmark} size]?[${:mystackmark} peek]:0}];
        # cache key
        set k [list [${:mystackloc} peek] $sym]
        next; # deletes the mark
        if {${:myok}} {
          if {[info exists :choices($mark)]} {
            set idx [expr {[set :choices($mark)]}]
          } else {
            set idx 0
          }
          unset -nocomplain :choices($mark)
          # inject the ctor
          set ctors [[[:info class] generator get] eval {set :ctors}]
          if {[dict exists $ctors $sym]} {
            set ctor [lindex [dict get $ctors $sym] $idx]
            if {$ctor ne ""} {
              set ast [${:mystackast} pop]
              lset ast 0 1 $ctor
              ${:mystackast} push $ast
              # update cache entry, if any
              if {[info exists :mysymbol($k)]} {
                lassign [set :mysymbol($k)] myloc myok myerror _
                set :mysymbol($k) [list $myloc $myok $myerror $ast]
              }
            }
          }
        }
      }
    }
  }; # Builder

  namespace export Parser BuilderGenerator ModelFactory
  
}



namespace import ::opeg::*

#
# An examplary domain model
#

nx::Class create Binary {
  :property -accessor public lhs:object,type=::Const
  :property op
  :property -accessor public rhs:object,type=::Const
}

nx::Class create Const {
  :property value
}

#
# A corresponding Object PEG (OPEG)
#

set g {
  OPEG Calculator (Term)
  Term  <- `Binary` lhs:Prim ' '* op:AddOp ' '* rhs:Prim / Prim;
  leaf: Prim      <- `Const` value:Num;
  Num <- Sign? Digit+                      ;
  Digit       <- '0'/'1'/'2'/'3'/'4'/'5'/'6'/'7'/'8'/'9'   ;
  Sign        <- '-' / '+'                                 ;
  AddOp       <- '+' / '-'                                 ;
  END;
}

#
# One may refine (or entirely override) the built-in object-conctruction logic by providing a custom factory.
#

nx::Class create CalculatorFactory -superclasses ModelFactory {
  :method "input AddOp" {startIdx endIdx args} {
    return [string range ${:sourcecode} $startIdx $endIdx];
  }
}

#
# An instance of ```BuilderGenerator``` is provided the OPEG and,
# optionally, a ```ModelFactory``` to generate a combined
# parser+builder for this OPEG.
#

set builderGen [BuilderGenerator new]
set builderClass [$builderGen bgen $g [CalculatorFactory new]]
set builder [$builderClass new]

#
# Using the ```Builder``` ```parset``` method, sentences in the
# language described by the OPEG can be processed into instances of
# the domain model directly.
#

set rObj [$builder parse {1+2}]

? {$rObj info class} ::Binary
? {[$rObj lhs get] info class} ::Const
? {[$rObj lhs get] cget -value} 1
? {[$rObj rhs get] info class} ::Const
? {[$rObj rhs get] cget -value} 2
? {$rObj cget -op} "+"

set rObj [$builder parse {5}]
? {$rObj info class} ::Const
? {$rObj cget -value} "5"

set rObj [$builder parse {-0}]
? {$rObj info class} ::Const
? {$rObj cget -value} "-0"

set rObj [$builder parse {4-3}]

? {$rObj info class} ::Binary
? {[$rObj lhs get] info class} ::Const
? {[$rObj lhs get] cget -value} 4
? {[$rObj rhs get] info class} ::Const
? {[$rObj rhs get] cget -value} 3
? {$rObj cget -op} "-"


set g1 {
PEG Coordinate (P)
  P           <- '(' Digit+ ',' Digit ')';
#  Digit       <- '0'/'1'/'2'/'3'/'4'/'5'/'6'/'7'/'8'/'9';
  Digit       <- <digit> <digit>;
END;}


set coordParser [[pt::rde::nx pgen $g1] new]
$coordParser print {(1122,23)}

nx::Class create Point {
  :property x:integer
  :property y:integer
}


# {
#   P {generator Point fields {x y}}
# }

set g2a {
OPEG Coordinate (P)
       P           <- `Point` '(' x:Digit1 ',' y:Digit2 ')';
leaf:  Digit2       <- '0'/'1'/'2'/'3'/'4'/'5'/'6'/'7'/'8'/'9';
leaf:  Digit1       <- <digit>+;
END;}

# set g2 {
# OPEG Coordinate (P)
#   P           <- @Point '(' x:<digit> ',' y:<digit> ')';
#   Digit       <- '0'/'1'/'2'/'3'/'4'/'5'/'6'/'7'/'8'/'9'      ;
# END;}


set builderGen [BuilderGenerator new]
set builderClass [$builderGen bgen $g2a]
# $builderGen print $g2
set coordBuilder [$builderClass new]

? {[$coordBuilder parse {(11,2)}] info class} ::Point
$coordBuilder print {(1,2)}
? {[$coordBuilder parse {(3,4)}] cget -y} 4


# {
#   P  {generator Point}
#   XY {fields {x y}}
# }

# set g2b {
# OPEG Coordinate (P)
#        XY          <- x:Digit ',' y:Digit;
#        P           <- `Point` '(' XY ')';
# leaf:  Digit       <- <digit>+;
# END;}

# puts stderr <<<<<
# set builderClass [$builderGen bgen $g2b]
# # $builderGen print $g2b
# set coordBuilder [$builderClass new]

# puts stderr >>>>>
# $coordBuilder print {(1,2)}
# ? {[$coordBuilder parse {(1,2)}] info class} ::Point
# ? {[$coordBuilder parse {(3,4)}] cget -y} 4



## NEXT STEPS

## 1) -> Check alternative syntaxes for ObjSpecs (not @, EBNF ideas, curly braces?)
## EBNF: special sequence (? ?)
## EBNF: () next to identifier?
## REGEX: named back-references notation (?=<name> ...)
## annotation-like: --> @IDENTIFIER
## FOR NOW: grave accent `...`

set g2 {
  OPEG Coordinate (P)
  P           <- `::Point` '(' x:<digit>+ ',' y:<digit>+ ')' ;
  END;}

## 
## 2) -> Revise field notation to support built-in (PT, string is) ranges as well as non-terminal values for fields.
## DONE


## 3) -> Support for collections



## 4) -> Complex graphs vs. metamodeling structures: challenging objspecs
## 5) -> Sanity checks at all steps

exit

set s {
  PEG Calculator (Expression)
  Expression  <- Term (' '* AddOp ' '* Term)*                 ;
  Term        <- Factor (' '* MulOp ' '* Factor)*             ;
  Fragment    <- '(' ' '* Expression ' '*  ')' / Number / Var ;
  Factor      <- Fragment (' '* PowOp ' '* Fragment)*         ;
  Number      <- Sign? Digit+                                 ;
  Var         <- '$' ( 'x'/'y'/'z' )                          ;
  
  Digit       <- '0'/'1'/'2'/'3'/'4'/'5'/'6'/'7'/'8'/'9'      ;
  Sign        <- '-' / '+'                                    ;
  MulOp       <- '*' / '/'                                    ;
  AddOp       <- '+' / '-'                                    ;
  PowOp       <- '**'                                         ;
  END;
}

set s2 {
  PEG Calculator (Term)
  Term  <- `Binary` lhs:Prim ' '* op:AddOp ' '* rhs:Prim / `Binary` (A/B);
  A <-       'a';
  B <-       Prim / Sign  ;
  leaf: Prim      <- `Const` value:Num;
  Num <- Sign? Digit+                      ;
  Digit       <- '0'/'1'/'2'/'3'/'4'/'5'/'6'/'7'/'8'/'9'   ;
  Sign        <- '-' / '+'                                 ;
  #	MulOp       <- '*' / '/'                                 ;
  AddOp       <- '+' / '-'                                 ;
  END;}

set s2 {
  PEG Calculator (Term)
  Term  <- `Binary` lhs:Prim ' '* op:AddOp ' '* rhs:Prim / Prim;
  leaf: Prim      <- `Const` value:Num;
  Num <- Sign? Digit+                      ;
  Digit       <- '0'/'1'/'2'/'3'/'4'/'5'/'6'/'7'/'8'/'9'   ;
  Sign        <- '-' / '+'                                 ;
  #	MulOp       <- '*' / '/'                                 ;
  AddOp       <- '+' / '-'                                 ;
  END;}


# Local variables:
#    mode: tcl
#    tcl-indent-level: 2
#    indent-tabs-mode: nil
# End:
