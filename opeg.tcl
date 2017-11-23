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
      # puts stderr ser=$ser
      set body [pt::peg::to::tclparam convert $ser]
      # puts BODY=$body
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
      :input {*}$ast
      # puts stderr AST=$ast
      # : input {*}$ast
    }

  }
  
  nx::Class create BuilderGenerator -superclasses Rewriter {

    :property {parser:object,substdefault {[Parser new]}}

    nx::Class create [self]::Class -superclass nx::Class {
      :property -accessor public generator
      :property -accessor public factory
    }

    :method rewrite {opegAst input} {
      set :defCounter 0
      set pegAst [next]
      unset :defCounter
      ## add ctors to OPEG structure
      # puts specs=${:specs}
      if {[info exists :specs]} {
        set :specs [dict map {nt specs} ${:specs} {
          if {![llength [concat {*}$specs]]} {
            continue
          }
          set specs
        }]
      }
      # puts specs=${:specs}
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
      puts ser=$ser
      ## initialize to NX/PEG backend defaults or dummies
      pt::tclparam::configuration::nx def _ _ _  {pt::peg::to::tclparam configure}
      
      ## strip down to just the core script fragment
      pt::peg::to::tclparam configure -template {@code@}

      set body [pt::peg::to::tclparam convert $ser]
      # puts stderr body=$body
      set cls [[current class]::Class new \
                   -superclasses [namespace current]::Builder \
                   -factory $modelFactory \
                   -generator [self] -- $body]
      return $cls
    }

    :method "input Grammar" {s e args} {
      if {[info exists :fieldDefs]} {

        set tmp [dict map {fieldDef defs} ${:fieldDefs} {
          if {[llength $defs] > 1} {
            lindex $defs end 
          } else {
            lindex $defs 0
          }
          
        }]
        # lappend args {*}[concat {*}[dict values ${:fieldDefs}]]
        lappend args {*}[dict values $tmp]
        unset :fieldDefs
      }
      next [list $s $e {*}$args]
    }
    
    :method "input Ctor" {s e args} {
      return [list c [lindex $args 0 1]]
    }

    :method "input Command" {s e args} {
      # operates like Ident
      return [:input Ident $s $e]
    }


    # :method "input Field" {s e args} {
    #   set args [lassign $args field]
    #   lappend :fields [lindex $field 1]
    #   # puts stderr FIELDARGS=$args
    #   if {0} {
    #     ## TODO: recognize and handle ?/+/* operators
    #     puts stderr FIELDARGS=$args
    #   }
    #   return [lindex $args 0]
    # }

    :method "input Field" {s e args} {
      set args [lassign $args field]
      if {0} {
        ## TODO: recognize and handle ?/+/* operators
        puts stderr FIELDARGS=$args
      }
      set ntIdent "_FIELD_${:defCounter}_[lindex $field 1]"
      # 1) compile + register 'field' definitions.
      #
      # _1_x {is {n Digit} mode value} _1_y {is {n Digit} mode value}
      
      dict lappend :fieldDefs $ntIdent [pt::peg::from::peg::GEN::Definition $s $e "value" [list n $ntIdent] [lindex $args 0]]
      # 
      # 2) inject reference to 'field' definition identifiers
      #
      # {n x} {n Digit} -> n _1_x

      #
      # 3) Are there "fixes" (paths) to consider later on? This ressembles the
      # behavior in "input Definition" -> refactor?
      #
      set f [lindex $field 1]
      if {[info exists :choices]} {
        # puts stderr choices=${:choices}
        set c [lindex ${:choices} end]
        # puts stderr c=${:choices}
        # lappend f $c
        # puts stderr f=$f
        dict set :specs $ntIdent $c
        ## piggyback onto :spec
        unset :choices
      }
      # lappend :fields $f
      
      return [list n $ntIdent]
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
      set spec [dict create]
      if {[llength $args] > 1 && [lindex $ctor 0] eq "c"} {
        # TODO: can there be more than one fix at a time? Test: dict
        # set spec generator [lrange $ctor 1 end]
        dict set spec generator [lindex $ctor 1]
        set args [lrange $args 1 end]
      }

      # TODO: Remove?
      if {[info exists :fields] && [llength ${:fields}]} {
        dict set spec fields ${:fields}
        unset :fields
      }
      
      list $spec {*}[next [list $s $e {*}$args]]
    }

    :method "input Expression" {s e args} {
      set rargs [list]
      set choices [list]
      foreach i $args {
        set resid [lassign $i spec]
        # TODO: stack them up for validation, over multiple levels of
        # (sub-)expressions!
        lappend choices $spec; 
        lappend rargs $resid
      }
      lappend :choices $choices
      next [list $s $e {*}$rargs]
    }
        
    :method "input Definition" {s e args} {
      incr :defCounter
      set def [next]
      if {[info exists :choices]} {
        set c [lindex ${:choices} end]
        dict set :specs [lindex $def 0] $c
        unset :choices
      }
      return $def
    }
    
  }
  
  nx::Class create ModelFactory {
    
    :variable sourcecode 

    #
    # TODO: the flow in postOrder must be consolidated and
    # streamlined; get rid of smelly LONG METHOD.
    #
    :public method postOrder {varName ast script {level 0}} {
      upvar [incr level] $varName var
      # puts stderr ast=$ast
      set ast [lassign $ast current start end]
      set childrenFlds [list]
      set fixes [list]
      if {[llength $ast]} {
        foreach c $ast {
          set kidz [:postOrder $varName $c $script $level]
          lassign $kidz pkg cArgs
          lassign $pkg cFields cFixes
          lappend childrenFlds {*}$cFields
          lappend fixes {*}$cFixes
          if {[llength $cArgs]==1} {
            lappend targs $cArgs
          } else {
            lappend targs {*}$cArgs
          }
        }

        # TODO: Is this really necessary? Only run loop when c > 1?
        if {[llength $targs] == 1} {
          set targs [lindex $targs 0]
        }
        
      } else {
        set targs [string range ${:sourcecode} $start $end]
      }

      # coalesce fields

      #
      # TODO: Irgh! merging with escaped (protected) single words ==
      # singelton lists produces an additional nesting level, each
      # time, so we have to drop that extra nesting level in a
      # postprocessing step (dict map)? can this be avoided?
      #

      set flds [dict create]
      if {[llength $childrenFlds]} {
        foreach {f v} $childrenFlds {
          if {![string is list $v] || [llength $v]==1} {
            dict lappend flds $f $v
          } else {
            dict lappend flds $f {*}$v
          }
        }
      }

      set flds [dict map {k v} $flds {
        if {[llength $v] == 1} {
          lindex $v 0
        } else {
          set v
        }
      }]

      # if {![info exists targs]} {
      #   set targs [string range ${:sourcecode} $start $end]
      # } else {
      # }
      
      lassign $current nt objspec
      

      # TODO: Can one get rid of NT-encoded field name resolution
      # (some reverse map _FIELD_* -> p1)? This introduces potential
      # conflicts between O/PEG Identifiers and NX variable/method
      # names, which are less restricted.
      
      if {[string first "_FIELD_" $nt] > -1} {
        set f [lindex [split $nt _] end]
        # dict lappend :fargs -$f $targs
        # dict lappend flds -$f {*}$targs
        # dict lappend flds -$f {*}$targs

        if {$objspec ne ""} {
          lappend fixes [list $f [dict get $objspec generator] $targs]
        } else {
          dict set flds -$f $targs
        }

      } else {
      
        if {$objspec ne ""} {
          dict with objspec {      
            if {[info exists generator]} {
              set :current [$generator new {*}$flds]
              set flds [list]
              if {[llength $fixes]} {
                lappend :fixes ${:current} $fixes
                set fixes [list]
              }
            }
          }
        }
      }

      set v ""
      
      if {[:info lookup method "input $nt"] ne ""} {
        set v [:input $nt $start $end {*}$targs]
      } elseif {[info exists :current]} {
        set v ${:current}
      } else {
        # error "Either an objspec or a mapping method must be provided for non-terminal '$nt'."
        set v $targs
      }
      unset -nocomplain :current


      set var $v
      uplevel $level $script
      return [list [list $flds $fixes] $v]
      
    }

    :public method wireUp {cmds} {
      set changed [llength $cmds]
      while {$changed} {
        set later [list]
        set changed 0
        foreach cmd $cmds {
          try $cmd on error {e} {lappend later $cmd} on ok {} {set changed 1}
        }
        set cmds $later
      }
      if {[info exists later] && [llength $later]} {
        return -code error "Unable to run fixes: $later"
      }
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
      $factory eval {unset :sourcecode}

      set root [lindex $list end]

      # TODO: relocate into factory object and turn it into a fixup
      # method as in Enso
      if {[$factory eval {info exists :fixes}]} {
        # expand fixes into commands
        set fldFixes [$factory eval {set :fixes}]
        foreach {obj fixes} $fldFixes {
          foreach fix $fixes {
            lassign $fix field path val
            if {[llength $path] > 1} {
              lassign $path objEl fieldEl valEl
              set lambda "$objEl $fieldEl get $valEl"
            } else {
              #
              # TODO: This is clearly disproportionate: Can be set
              # eagerly, and does not require apply call etc. Fix when
              # appropriate.
              #
              set lambda "return $path"
            }
            lappend fixCmds [list $obj eval ":configure -$field \[[list apply [list {0 root} $lambda] $val $root]\]"]
          }
        }
        # evaluate fixCmds
        $factory wireUp $fixCmds
      }

      unset -nocomplain :symStack; # TODO: relocate
      array unset -nocomplain :choices; # TODO: relocate
      $factory eval {unset -nocomplain :fixes}; # TODO: relocate
      
      return $root; # root
    }

    # TODO: make the symStack thingie more elegant.
    ## si:valuevalue_branch si:valuevoid_branch si:voidvalue_branch si:voidvoid_branch

    foreach m [[lindex [:info superclasses] end] info methods -callprotection all *_branch] {
      :method $m {} {
        # set mark [${:mystackmark} peek]; puts mark(insym)=$mark
        # set mark [${:mystackmark} size]; puts mark(insym)=$mark
        set mark [llength ${:symStack}]
        
        if {![info exists :choices($mark)]} {
          # init
          set :choices($mark) 0
        }
        try {set r [next]} on return {} {
          return -code return
        }; # ok
        incr :choices($mark); # puts stderr BUMP([lindex ${:symStack} end],$mark)
        return $r
      }
    }

    ## si:value_leaf_symbol_end si:void_leaf_symbol_end si:value_leaf_symbol_end si:value_clear_symbol_end si:reduce_symbol_end
    ## [[lindex [:info superclasses] end] info methods -callprotection all *_symbol_end]

    foreach m [[lindex [:info superclasses] end] info methods -callprotection all *_symbol_start] {
      :method $m {sym} {
        # push
        lappend :symStack $sym
        # puts stderr START([self],$sym)
        try {next} on return {} {set :symStack [lrange ${:symStack} 0 end-1]; return -code return}
      }
    }
    
    foreach m [[lindex [:info superclasses] end] info methods -callprotection all *_symbol_end] {
      :method $m {sym} {
        # si:value_symbol_start: pushes on AST stack & sets a mark
        # set mark [expr {[${:mystackmark} size]?[${:mystackmark} peek]:0}];
        # set mark [llength ${:mystackmark} size]

        set k [list [${:mystackloc} peek] $sym]
        set mark [llength ${:symStack}]
        set :symStack [lrange ${:symStack} 0 end-1]
        # puts stderr END($sym)
        next; # deletes the mark

        # puts stderr C($sym)=[array get :choices]
        if {${:myok}} {
          if {[info exists :choices($mark)]} {
            set idx [set :choices($mark)]
          } else {
            set idx 0
          }
          # unset -nocomplain :choices($mark)
          # inject the ctor
          set ctors [[[:info class] generator get] eval {set :specs}]
          # if {[string match _FIELD_* $sym]} {
          #   puts stderr "---FIELD($sym),$idx,$ctors"
          # }

          if {[dict exists $ctors $sym]} {
            set spec [lindex [dict get $ctors $sym] $idx]
            if {$spec ne ""} {
              set ast [${:mystackast} pop]
              # TODO: FIX this here!
              # lset ast 0 1 [concat {*}[dict values $spec]];# $ctor
              lset ast 0 1 $spec
              ${:mystackast} push $ast
              # update cache entry, if any
              if {[info exists :mysymbol($k)]} {
                lassign [set :mysymbol($k)] myloc myok myerror _
                set :mysymbol($k) [list $myloc $myok $myerror $ast]
              }
            }
          }
        }
        unset -nocomplain :choices($mark)
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

# validation rules: no definition with field declarations must be in
# mode 'leaf'.

set g {
OPEG Calculator (Term)
      Term        <- `Binary` lhs:Prim ' '* op:AddOp ' '* rhs:Prim / Prim;
      Prim        <- `Const` value:Num;
leaf: Num         <- Sign? Digit+                      ;
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
puts stderr [string index $g 52]
set builderClass [$builderGen bgen $g [CalculatorFactory new]]
set builder [$builderClass new]

#
# Using the ```Builder``` ```parset``` method, sentences in the
# language described by the OPEG can be processed into instances of
# the domain model directly.
#

# $builder print {1+2}
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
P            <- OPENP Digit+ ',' Digit CLOSEP;
void: OPENP  <- '(';
void: CLOSEP <- ')';
Digit        <- <digit> <digit>;
END;}

# set g1 {
# PEG Coordinate (DigitPairs)
#    DigitPairs  <-  Digit (',' DigitPairs)?;
#    Digit       <- <digit> <digit>;
# END;}


set coordParser [[pt::rde::nx pgen $g1] new]
# $coordParser print {(11,22)}
puts stderr [$coordParser parset {(11,22)}]


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


nx::Class create Point {
  :property x:integer
  :property y:integer
}

set builderGen [BuilderGenerator new]
set builderClass [$builderGen bgen $g2a]
# $builderGen print $g2
set coordBuilder [$builderClass new]

? {[$coordBuilder parse {(11,2)}] info class} ::Point
# $coordBuilder print {(1,2)}
? {[$coordBuilder parse {(3,4)}] cget -y} 4


# {
#   P  {generator Point}
#   XY {fields {x y}}
# }

set g2b {
OPEG Coordinate (P)
       XY          <- x:Digit ',' y:Digit;
       P           <- `Point` '(' XY ')';
leaf:  Digit       <- <digit>+;
END;}

set builderClass [$builderGen bgen $g2b]
# $builderGen print $g2b
set coordBuilder [$builderClass new]

# $coordBuilder print {(1,2)}
? {[$coordBuilder parse {(1,2)}] info class} ::Point
? {[$coordBuilder parse {(3,4)}] cget -y} 4

set g2c {
OPEG Coordinate (P)
P                  <- `Point` '(' XY ')';
XY                 <- A ',' B;
A                  <- x:Digit;
B                  <- y:Digit;
leaf:  Digit       <- <digit>+;
END;}

set builderClass [$builderGen bgen $g2c]
set coordBuilder [$builderClass new]

# $coordBuilder print {(1,2)}
? {[$coordBuilder parse {(1,2)}] info class} ::Point
? {[$coordBuilder parse {(3,4)}] cget -y} 4



## NEXT STEPS

## 1) -> Check alternative syntaxes for ObjSpecs (not @, EBNF ideas, curly braces?)
## EBNF: special sequence (? ?)
## EBNF: () next to identifier?
## REGEX: named back-references notation (?=<name> ...)
## annotation-like: --> @IDENTIFIER
## FOR NOW: grave accent `...`

## 
## 2) -> Revise field notation to support built-in (PT, string is) ranges as well as non-terminal values for fields.
## DONE


## 3) -> Support for collections

set g3 {
OPEG Coordinate (P)
       P  <- `HyperPoint` '(' (x:<digit>)+ ',' (x:<digit> y:<digit>)? ')';
END;}

# puts stderr [string index $g3 41]
# $builderGen print $g3
set builderClass [$builderGen bgen $g3]
set coordBuilder [$builderClass new]


nx::Class create HyperPoint {
  :property x:integer,1..*
  :property y:integer
}

# $coordBuilder print {(92,32)}; #$coordBuilder print {(987,2)}
set p [$coordBuilder parse {(92,32)}]; #set p [$coordBuilder parse {(987,2)}]
? {$p info class} ::HyperPoint
? {$p cget -x} "9 2 3";
? {$p cget -y} "2";

# C ::= [Call] fun:id "(" args:Exp* @"," ")"

nx::Class create Call {
  :property fun
  :property args
}

set g4 {
OPEG CallDecl (C)
      C    <- `Call` fun:ID '(' (args:Exp)? (',' args:Exp)* ')';
      ID   <- 'foo';
      Exp  <- <digit>;
END;}

# $builderGen print $g4
set builderClass [$builderGen bgen $g4]
set callBuilder [$builderClass new]

set c [$callBuilder parse {foo(1,1,2,2)}];
? {$c info class} ::Call
? {$c cget -fun} "foo";
? {$c cget -args} "1 1 2 2";

set c [$callBuilder parse {foo()}];
? {$c info class} ::Call
? {$c cget -fun} "foo";
? {$c cget -args} {can't read "args": no such variable};

# TODO:
# ARGS <- (args:Exp)? (',' args:Exp)* a top-level list -> flatten!

set g5 {
OPEG CallDecl (C)
      C    <- `Call` fun:ID '(' ARGS ')';
      ARGS <- (args:Exp)? (',' args:Exp)*;
      ID   <- 'foo';
      Exp  <- <digit>;
END;}

# $builderGen print $g5
set builderClass [$builderGen bgen $g5]
set callBuilder [$builderClass new]

set c [$callBuilder parse {foo(1,1,2,2)}];
? {$c info class} ::Call
? {$c cget -fun} "foo";
? {$c cget -args} "1 1 2 2";

# ARGS <- args:Exp (','ARGS)* adds cascaded of lists

set g6 {
OPEG CallDecl (C)
      C    <- `Call` fun:ID '(' ARGS ')';
      ARGS <- (args:Exp)? (',' ARGS)*;
      ID   <- 'foo';
      Exp  <- <digit>;
END;}

# $builderGen print $g6
set builderClass [$builderGen bgen $g6]
set callBuilder [$builderClass new]

set c [$callBuilder parse {foo(2,1,2,5)}];
? {$c info class} ::Call
? {$c cget -fun} "foo";
? {$c cget -args} "2 1 2 5";

## 4) -> Enso examples

# start Drawing
# Drawing ::= [Drawing] "drawing" /> lines:Line* @/ /
# Line ::= [Line] "line" label:str /> points:Point2D* @/ < Adj? 
# Adj ::= / "adj" adj:<root.lines[it]>
# Point3D ::= [Point3D] "point" x:int y:int z:int
# Point2D ::= [Point2D] "point" x:int y:int

nx::Class create Drawing {
  :property lines:0..n,object,type=::Line
}

nx::Class create Line {
  :property label
  :property points:0..n,object,type=::Point2D
  # :property adj:object,type=::Line
  :property adj
}

nx::Class create Point2D {
  :property x:integer
  :property y:integer
}

set geom {
OPEG Drawing (Drawing)
      Drawing    <- `Drawing` 'drawing' (<space>+ lines:Line)+;
      Line       <- `Line` 'line' ' '+ DAPOSTROPH label:Str DAPOSTROPH (' '+ points:Point2D)* ' '+ Adj?;
Adj        <- 'adj' ' '+ DAPOSTROPH adj:Str DAPOSTROPH;
# paths:
#      Adj        <- 'adj' ' '+ DAPOSTROPH adj:(`$root {lines $current} points` Str) DAPOSTROPH;
      Str        <- !DAPOSTROPH <alnum>*;
      Point2D    <- `Point2D` 'point' ' '+ x:<digit>+ ' '+ y:<digit>+;
void:      DAPOSTROPH    <- '\"' ;
END;}

set aDraw {drawing
  line "Flamingo" point 1 1 point 2 2 point 20 20 point 21 21 point 3 3 point 4 4 point 22 22 point 23 23 adj "Stork"
  line "Stork" point 1 1 point 10 10 adj "Flamingo"}

set builderGen [BuilderGenerator new]
set builderClass [$builderGen bgen $geom]
set diagramBuilder [$builderClass new]

set c [$diagramBuilder parse $aDraw]
? {$c info class} ::Drawing
set lines [$c cget -lines]
? {llength $lines} 2
lassign $lines l1 l2
? {$l1 info class} ::Line
? {$l1 cget -label} "Flamingo"
? {$l1 cget -adj} "Stork"
? {llength [$l1 cget -points]} 8
? {$l2 info class} ::Line
? {$l2 cget -label} "Stork"
? {$l2 cget -adj} "Flamingo"
? {llength [$l2 cget -points]} 2

## TODOS (DONE):
## - fix value passing for quoted, braced values ... too many nesting levels

nx::Class create C {
  :property p1
}

set bGram {OPEG MyPEG (D)
          D <- `C` p1:E;
  leaf:   E <- '\"' <alnum>+ '\"';
END;
}

set b [[$builderGen bgen $bGram] new]

set out [$b parse {"abc"}]

? {$out info class} ::C
? {$out cget -p1} {"abc"}

set b [[$builderGen bgen "
  OPEG MyPEG (D)
          D <- `C` p1:E (<space>+ E)+;
  leaf:   E <- '\"' (<alnum> / '\{' / '\}')+ '\"';
END;
"] new]

set x \{}c

? {string is list $x} 0

set out [$b parse "\"$x\" \"{}xz\""]

? {$out info class} ::C
? {$out cget -p1} "\"$x\""

##
## TODOS: Fix choice propagation
##

nx::Class create Base {
  :property -accessor public p0 {
    :public object method value=get {obj prop in} {
      return [nx::Object create [$obj info class]::$in]
    }
  }
}

nx::Class create XX -superclasses Base {
  :property p1
}

nx::Class create YY -superclasses Base {
  :property p2
}


set pathGr {OPEG MyPEG (D)
       D <- `XX` p1:A / `YY` p2:B; # TODO: top-level expression nesting is not supported (flattening): ((`XX` p1:A / `YY` p2:B))
leaf:  A <-  'A' <digit>+;
leaf:  B <-  'B' <digit>+;
END;
}

# puts [$builderGen print $pathGr]

set d [[$builderGen bgen $pathGr] new]

? {[$d parse {B1}] info class} ::YY
? {[$d parse {A1}] info class} ::XX

## TODO: choice propagation for field paths?

set pathGr {OPEG MyPEG (D)
         D <- '1' / `XX` p1:(`$root p0 $0` A / '0' / `$root p0 $0` C) / '3' / `YY` p2:(`$root p0 $0` B);
  leaf:  A <-  'A' <digit>+;
  leaf:  B <-  'B' <digit>+;
  leaf:  C <-  'C' <digit>+;
  END;}

set d [[$builderGen bgen $pathGr] new]

? {[set b1 [$d parse {B1}]] info class} ::YY
? {[$b1 cget -p2] info name} B1
? {[set a1 [$d parse {A1}]] info class} ::XX
? {[$a1 cget -p1] info name} A1
? {[set c1 [$d parse {C1}]] info class} ::XX
? {[$c1 cget -p1] info name} C1

## TODO Complete ENSO geom example

nx::Class create Drawing {
  :property -accessor public lines:0..n,object,type=::Line {
    :public object method value=get {obj value in:optional} {
      set lines [next [list $obj $value]]
      if {[info exists in]} {
        foreach l $lines {
          if {[$l cget -label] eq $in} {
            return $l
          }
        }
      } else {
        return $lines
      }
    }
  }
}
  
::Line property adj:object,type=::Line

# no paths:
# Adj        <- 'adj' ' '+ DAPOSTROPH adj:Str DAPOSTROPH;
# paths:

set geom2 {
OPEG Drawing (Drawing)
      Drawing    <- `Drawing` 'drawing' (<space>+ lines:Line)+;
      Line       <- `Line` 'line' ' '+ DAPOSTROPH label:Str DAPOSTROPH (' '+ points:Point2D)* ' '+ Adj?;
      Adj        <- 'adj' ' '+ DAPOSTROPH adj:(`$root lines $0` Str) DAPOSTROPH;
      Str        <- !DAPOSTROPH <alnum>*;
      Point2D    <- `Point2D` 'point' ' '+ x:<digit>+ ' '+ y:<digit>+;
void: DAPOSTROPH    <- '\"' ;
END;}

set builderClass [$builderGen bgen $geom2]
set diagramBuilder [$builderClass new]

# puts stderr [time {set c2 [$diagramBuilder parse $aDraw]} 1000]; # ~7ms unoptimized

set c2 [$diagramBuilder parse $aDraw]

? {$c2 info class} ::Drawing
set lines [$c2 cget -lines]
? {llength $lines} 2
lassign $lines l1 l2
? {$l1 cget -adj} $l2
? {[$l1 cget -adj] cget -label} [$l2 cget -label]
? {$l2 cget -adj} $l1
? {[$l2 cget -adj] cget -label} [$l1 cget -label]

##
## TODO: boolean assignments based on simple token structures? -->
## predicates needed? IMO, not really, we need to be more pretentious
## on the path/knit expressions, though.
##

nx::Class create Bool {
  :property value:boolean
}

set boolGram {OPEG MyPEG (B)
          B <- `Bool` value:('true' / 'false');
END;
}

set b [[$builderGen bgen $boolGram] new]

set out [$b parse {true}]
? {$out info class} ::Bool
? {$out cget -value} "true"
? {string is boolean [$out cget -value]} 1

set out [$b parse {false}]
? {$out info class} ::Bool
? {string is boolean [$out cget -value]} 1

set boolGram {OPEG MyPEG (B)
          B <- `Bool` value:('true' / 'false');
END;}

set boolGram {OPEG MyPEG (B)
          B <- `Bool` value:(`true` '#' / `false` '##');
END;}

set b [[$builderGen bgen $boolGram] new]

set out [$b parse {##}]
? {$out info class} ::Bool
? {string is boolean [$out cget -value]} 1

set out [$b parse {#}]
? {$out info class} ::Bool
? {$out cget -value} "true"
? {string is boolean [$out cget -value]} 1

## TODO: What to do with non-fields in Sequences?
## -- What does ENSO do: ENSO has field-only sequences?
## ... Should we disallow also?
## ... Is that a case-in-point for mappings? Inject into "mapping"
## operations post-object construction?

nx::Class create BooleanFactory -superclasses ModelFactory {
  :public method "input B" {startIdx endIdx args} {
    if {[info exists :current]} {
      return ${:current}
    } else {
      return [Bool new -value [expr {[lindex $args 0] > [lindex $args 1]}]]
    }
  }
}

set nf [[$builderGen bgen {
  OPEG MyPEG (B)
        # TODO: Maybe allow for writing `Bool` value:(`$0 > $1` Digit ' '+ Digit)?
        B    <- `Bool` value:('true' / 'false') / Digit ' '+ Digit;
        Digit  <- <digit>+;
  END;
} [BooleanFactory new]] new]

set out [$nf parse {true}]
? {$out info class} ::Bool
? {$out cget -value} "true"
set out [$nf parse {20 30}]
? {$out info class} ::Bool
? {$out cget -value} "0"
set out [$nf parse {30 20}]
? {$out info class} ::Bool
? {$out cget -value} "1"

## TODO next: Merge scenario:
## 2D Points, 3D Points, grammar + language model, plus feature model?

nx::Class create Point2D {
  :public method equal {anotherPoint} {
    return [expr {${:x} == [$anotherPoint x get] && ${:y} == [$anotherPoint y get]}]
  }
}

nx::Class create Point3D -superclasses Point2D {
  :property -accessor public z:integer
  :public method equal {anotherPoint} {
    return [expr {[next] && ${:z} == [$anotherPoint z get]}]
  }
}

set geom2 {
OPEG Drawing (Drawing)
      Drawing    <- `Drawing` 'drawing' (<space>+ lines:Line)+;
      Line       <- `Line` 'line' ' '+ DAPOSTROPH label:Str DAPOSTROPH (' '+ points:Point2D)* ' '+ Adj?;
      Adj        <- 'adj' ' '+ DAPOSTROPH adj:(`$root lines $0` Str) DAPOSTROPH;
      Str        <- !DAPOSTROPH <alnum>*;
      Point2D    <- `Point2D` 'point' ' '+ x:<digit>+ ' '+ y:<digit>+;
void: DAPOSTROPH    <- '\"' ;
END;}

## 5) -> Sanity checks at all steps (OPEG validate)?

## 6) pattern matching for testing:

if {0} {
  [Pattern new {Dict {
    a {Bool -value true}
    b {Bool -value false}
  }}] match $out
}

##
## Error detection and reporting; feat. Tiny example from:
##
## Maidl, A. M., Mascarenhas, F., & Ierusalimschy,
## R. (2013). Exception Handling for Error Reporting in Parsing
## Expression Grammars. In: Proc. 17th Brazilian Symposium
## Programming Languages (SBLP 2013) (pp. 1--15). Springer.
##

set fh [open [file join [file dirname [info script]] "tiny.peg"] r]
set tiny [read $fh]
catch {close $fh}

## minimal FFP support

nx::Class create FFP {
  :property -accessor public {ffp:substdefault {[list]}}
  
  # FFP: available on [complete]
  :public method complete {} {
    # puts FFP=${:ffp}
    next
  }

  # FFP: actual FFP recording in the spirit of "i_error_pop_merge",
  # without popping, naturally. At this point, "myerror", if
  # available, carries a previously popped error-stack element.
  :method updateFFP {} {
    if {![info exists :ffp] || ![llength ${:ffp}]} {
      set :ffp ${:myerror}; return
    }
    
    if {![llength ${:myerror}]} {
      return
    }
    
    lassign ${:myerror} currentErrPos currentErrMsg
    lassign ${:ffp} prevFfpPos prevFfpMsg
    
    if {$prevFfpPos > $currentErrPos} { return; }
    if {$currentErrPos > $prevFfpPos} {
      set :ffp ${:myerror}
    }
    # Equal locations, merge the message lists
    set :ffp [list $currentErrPos [lsort -uniq [list {*}$prevFfpMsg {*}$currentErrMsg]]]
  }

  # Instrumentation (1): end-of-choice
  
  foreach m {si:void_state_merge si:value_state_merge} {
    :method $m {} {
      :updateFFP
      next      
    }
  }
  
  # Instrumentation (2): mid-of-choice
  foreach m {si:valuevoid_branch si:valuevalue_branch si:voidvoid_branch si:voidvalue_branch} {
    :method $m {} {
      try {set r [next]} on return {} {
        # caught a backtracking return, update ffp
        :updateFFP 
        return -code return
      }
      return $r
    }
  }
}


set builderGen [BuilderGenerator new]
set builderClass [$builderGen bgen $tiny]
set tinyParser [$builderClass new]


set p {n := 1;}
? {lassign [$tinyParser parset $p] _ start end; set end} [expr {[string length $p]-1}]; # fully consumed?

set tinyProg {
  n := 5;
  f := 1;
  repeat
    f := f * n;
    n := n - 1
  until n < 1;
  write f;
}

$builderClass mixins add FFP

try {
  lassign [$tinyParser parset $tinyProg] root start end
} on error {e} {
  # with EOI in Tiny grammar
  lassign $e _ loc
  ? {set loc} 23; # points to repeat
  ? {lindex [$tinyParser ffp get] 0} 63;  # points to begin of u(ntil).
} on ok {r} {
  # TODO: complete, cases without EOI
  # prefix
  ? {string range $tinyProg $start $end} $tinyProg
  # suffix
  ? {string range $tinyProg [expr {$end+1}] [string length $tinyProg]} ""
} finally {
  $tinyParser destroy
}

$builderClass mixins delete FFP

#
# Minimal combination support for NX/PARAM parsers (parser combinators)
#


pt::rde::nx public method cswitch {parentParser {instr MAIN}} {

  # puts "SWITCH: $parentParser -($instr)-> [self]"
  # puts STACKPARENT=[$parentParser eval "\${:mystackast} size"]
  :reset {}
  ## dump current parsing state in sub-parser
  set myloc [$parentParser eval {:location}]

  incr myloc
  set mtoken [$parentParser eval {set :mytoken}]

  if {$myloc >= [string length $mtoken]} {
    $parentParser eval {set :myok 0}
    # set myerror [list $myloc [list [list t $tok]]]
    return
  }

  set dat [string range $mtoken $myloc end]

  :data $dat
  #puts myloc=$myloc,dat=$dat


  # TODO: needed?
  # $parentParser eval "\${:mystackloc} push ${:myloc}"
  
  : $instr

  $parentParser eval [list set :myok ${:myok}]
  
  if {${:myok}} {
    set subAst [:complete]
    lassign $subAst nt start end
    # puts subAst=$myloc,${:myloc},$end,$subAst
    # TODO: in nested sub ASTs, all range markers must be bumped!
    set subAst [list $nt $myloc [expr {$myloc + $end}]]
    $parentParser eval "\${:mystackast} push [list $subAst]"
    $parentParser eval [list set :myloc [expr {$myloc + $end}]]
  } else {
    #
    # TODO: fix loc on error (in the error report and for the pparser)
    #
    set parentErr [$parentParser eval {set :myerror}]
    lassign $parentErr oldPos oldMsg
    lassign ${:myerror} newPos newMsg
    set newLoc [expr {$myloc + $newPos}]
    
    if {$oldPos > $newLoc} {
      return
    }
    if {$newLoc > $oldPos} {
      set msg $newMsg
    } else {
      set msg [list {*}$oldMsg {*}$newMsg]
    }
    
    $parentParser eval [list set :myerror [list $newLoc $msg]]
    $parentParser eval [list set :myloc $newLoc]
  }

}

set digit [[pt::rde::nx pgen {PEG DIGIT (digit) digit <- [0-9]; END;}] new]
set letter [[pt::rde::nx pgen {PEG LETTER (letter) letter <- [a-z] / [A-Z]; END;}] new]

? {$digit parset {1}} {digit 0 0}
? {$letter parset {a}} {letter 0 0}

set identifier [[pt::rde::nx pgen {PEG ID (id) digit <- .; letter <- .; id <- letter (letter / digit)*; END;}] new]



? {$identifier parset {1}} {id 0 0 {letter 0 0}}

$identifier object forward sym_letter $letter cswitch %self %method
$identifier object forward sym_digit $digit cswitch %self %method

# debug on pt/rdengine
? {$identifier parset {a}} {id 0 0 {letter 0 0}}
? {$identifier parset {1}} {pt::rde 0 {{cl abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ}}}
? {$identifier parset {abc}} {id 0 2 {letter 0 0} {letter 1 1} {letter 2 2}}
? {$identifier parset {a1}} {id 0 1 {letter 0 0} {digit 1 1}}
# debug off pt/rdengine

set dletters [[pt::rde::nx pgen {PEG LETTER (dletters) dletters <- [a-z][a-z] / [A-Z][A-Z]; END;}] new]
set identifier2 [[pt::rde::nx pgen {PEG ID2 (id) digit <- .; dletters <- .; id <- dletters (dletters / digit)*; END;}] new]
$identifier2 object forward sym_dletters $dletters cswitch %self %method
$identifier2 object forward sym_digit $digit cswitch %self %method
? {$identifier2 parset {aabb1}} {id 0 4 {dletters 0 1} {dletters 2 3} {digit 4 4}}
? {$identifier2 parset {aab11}} {id 0 1 {dletters 0 1}}; # partial parse
$identifier2 object mixins add FFP
? {$identifier2 parset {aab11}} {id 0 1 {dletters 0 1}}; # partial parse + FFP
$identifier2 object mixins delete FFP

## pt_peg_to_tclparam.tcl
proc ::pt::peg::to::tclparam::Op::n {modes symbol} {
    # symbol mode determines AST generation
    # void       => non-generative,
    # leaf/value => generative.

    Asm::Start
    Asm::ReTerminal n $symbol

    # TODO: limit to Parser instances, allow for late binding (cyclic
    # relationships in the nested parser structure).
    if {[::nsf::is object $symbol]} {
      Asm::GenAST [list gen 1]; # TODO: get this from parser obj
      Asm::Direct {
        # TODO: collect the needed parent infos here, or use a
        # redirector on the parent.
        Asm::Tcl $symbol cswitch \[self\]
      }
    } else {
      if {![dict exists $modes $symbol]} {
        # Incomplete grammar. The symbol has no definition.
        Asm::Direct {
          Asm::Ins i_status_fail "; # Undefined symbol '$symbol'"
        }
      } else {
        Asm::GenAST [list gen [expr { [dict get $modes $symbol] ne "void" }]]
        Asm::Direct {
          Asm::Self sym_$symbol
        }
      }
    }
    Asm::Done
}

# set letter [[pt::rde::nx pgen {[a-z] / [A-Z]}] new]


[pt::rde::nx pgen {PEG DIGIT (digit) digit <- [0-9]; END;}] create ::digit
[pt::rde::nx pgen {PEG LETTER (letter) letter <- [a-z] / [A-Z]; END;}] create ::letter


set identifier [[[BuilderGenerator new] bgen "OPEG ID (id) id <- ::letter (::letter / ::digit)*; END;"] new]

? {$identifier parset {a}} {id 0 0 {letter 0 0}}
? {$identifier parset {1}} {pt::rde 0 {{cl abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ}}}
? {$identifier parset {abc}} {id 0 2 {letter 0 0} {letter 1 1} {letter 2 2}}
? {$identifier parset {a1}} {id 0 1 {letter 0 0} {digit 1 1}}

## TODOS:
##
## 1) Parser-based builder interface (parser combination)
##
## 2) Implement parser-as-symbol in the intermediate (canonical) PEG
## representation: method-call frontend to intermediate PEG.

## ser=pt::grammar::peg {rules {id {is {x {n ::letter} {* {/ {n ::letter} {n ::digit}}}} mode value}} start {n id}}

pt::rde::nx public method asPeg {} {
  if {[info exists :myPeg]} {
    return ${:myPeg}
  } else {
    return [list n [self]]
  }
}

pt::rde::nx public method fromPeg {ser} {

  pt::tclparam::configuration::nx def _ _ _  {pt::peg::to::tclparam configure}
  
  ## strip down to just the core script fragment
  pt::peg::to::tclparam configure -template {@code@}
  
  set body [pt::peg::to::tclparam convert $ser]
  
  return [pt::rde::nx ]
}


pt::rde::nx public method {,} {args} {
  set :myPeg [list x {*}[lmap p [list [self] {*}$args] {$p asPeg}]]
  return [self]
}

pt::rde::nx public method / {args} {
  set :myPeg [list / {*}[lmap p [list [self] {*}$args] {$p asPeg}]]
  return [self]
}

pt::rde::nx public method * {} {
  return [list * [:asPeg]]
}



pt::rde::nx public method -> {args} {
  
}


? {[::letter , ::letter ::letter] asPeg} {x {n ::letter} {n ::letter} {n ::letter}}

? {[::letter / ::digit] asPeg} {/ {n ::letter} {n ::digit}}

? {[::letter *] asPeg} {* {n ::letter}}

? {[[::letter / ::digit] *] asPeg} {* {/ {n ::letter} {n ::digit}}}

? {[[::letter / ::digit] -> identifier] info has type "pt::rde::nx"} 1

##
## 3) fix debug support in NX engine class (use apply wrapper to set
## the namespace context correctly)
##
## 4) minimize pgen/bgen interface (default to some HEADER etc.)
##
## 5) fit object generators into builder interface
##
## 6) introduce an operator other than n? to generalize the injection
## and to avoid interactions with other PEG extension modifying the
## syntax of identifiers/symbols?
##

exit


## letter (letter / digit letter / digit )*
set identifier [$letter , {{{{$letter : p1} / {$digit $letter} / $digit} *}}]
set identifier [$letter , [[$letter / [$digit , $letter] / $digit] *] , $letter]
set identifier [$letter , [[$letter / [$digit , $letter] / $digit] *] , {[0-9]+ / 'aaa'} : p2]

$letter , 
set identifier [then $letter then $letter then choice $letter $digit]
set identifier [$letter then $letter choice ]

# package req nx::serializer; this stumbles over the TclOO alien :(
# puts identifier=[$identifier serialize]


# Local variables:
#    mode: tcl
#    tcl-indent-level: 2
#    indent-tabs-mode: nil
# End:
