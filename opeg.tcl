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
      # puts stderr AST=$ast
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

      ## initialize to NX/PEG backend defaults or dummies
      pt::tclparam::configuration::nx def _ _ _  {pt::peg::to::tclparam configure}
      
      ## strip down to just the core script fragment
      pt::peg::to::tclparam configure -template {@code@}

      set body [pt::peg::to::tclparam convert $ser]
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
      return [lindex $args 0 1]
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
      lappend :fields [lindex $field 1]
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
      if {[llength $args] > 1 && [llength $ctor] == 1} {
        dict set spec generator $ctor
        set args [lrange $args 1 end]
      }
      
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

        
      # if {0} {
      #   if {$objspec ne ""} {
      #     set fargs ""
      #     dict with objspec {
      #       if {[info exists fields] && [llength $fields]} {
      #         set fargs [join [concat {*}[lmap f $fields v $targs {list -$f $v}]]]
      #       }
            
      #       if {[info exists generator]} {
      #         if {[info exists :fargs]} {
      #           set fargs [list {*}${:fargs} {*}$fargs]
      #           unset :fargs
      #         }
      #         puts "FORMULA($nt)=$generator new {*}$fargs"
      #         set :current [$generator new {*}$fargs]
      #       } else {
      #         # store current fields for later evaluation
      #         lappend :fargs {*}$fargs
      #         puts "FORMULA($nt)=${:fargs}"
      #       }
      #     }
      #   }
      # }

  nx::Class create ModelFactory {
    
    :variable sourcecode
    
    :public method postOrder {varName ast script {level 0}} {
      upvar [incr level] $varName var
      set ast [lassign $ast current start end]
      set childrenFlds [list]
      # default to the leaf/literal value?
      foreach c $ast {
        lassign [:postOrder $varName $c $script $level] cFields cArgs
        lappend childrenFlds {*}$cFields
        lappend targs $cArgs
      }

      # coalesce fields

      set flds [dict create]
      if {[llength $childrenFlds]} {
        foreach {f v} $childrenFlds {
          dict lappend flds $f {*}$v
        }
      }


      if {![info exists targs]} {
        set targs [string range ${:sourcecode} $start $end]
      }
      lassign $current nt objspec      

      if {[string first "_FIELD_" $nt] > -1} {
        set f [lindex [split $nt _] end]
        # dict lappend :fargs -$f $targs
        dict lappend flds -$f {*}$targs
      }
      
      if {$objspec ne ""} {
        dict with objspec {
          if {[info exists generator]} {
            set :current [$generator new {*}$flds]
            set flds [list]

          }
        }
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
      return [list $flds $v]
      
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
          set ctors [[[:info class] generator get] eval {set :specs}]
          if {[dict exists $ctors $sym]} {
            set spec [lindex [dict get $ctors $sym] $idx]
            if {$spec ne ""} {
              set ast [${:mystackast} pop]
              # puts spec=[dict values $spec]
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
set builderClass [$builderGen bgen $g [CalculatorFactory new]]
set builder [$builderClass new]

#
# Using the ```Builder``` ```parset``` method, sentences in the
# language described by the OPEG can be processed into instances of
# the domain model directly.
#

$builder print {1+2}
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
$coordParser print {(11,22)}
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
$coordBuilder print {(1,2)}
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

$coordBuilder print {(1,2)}
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

$coordBuilder print {(1,2)}
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
$builderGen print $g3
set builderClass [$builderGen bgen $g3]
set coordBuilder [$builderClass new]


nx::Class create HyperPoint {
  :property x:integer,1..*
  :property y:integer
}

$coordBuilder print {(92,32)}; #$coordBuilder print {(987,2)}
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

$builderGen print $g4
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

$builderGen print $g5
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

$builderGen print $g6
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
#      Adj        <- 'adj' ' '+ DAPOSTROPH adj:(`$root lines $current` Str) DAPOSTROPH;
      Str        <- !DAPOSTROPH <alnum>*;
      Point2D    <- `Point2D` 'point' ' '+ x:<digit>+ ' '+ y:<digit>+;
void:      DAPOSTROPH    <- '\"' ;
END;}

set d {drawing
  line "Flamingo" point 1 1 point 2 2 point 20 20 point 21 21 point 3 3 point 4 4 point 22 22 point 23 23 adj "Stork"
  line "Stork" point 1 1 point 10 10 adj "Flamingo"}

set builderGen [BuilderGenerator new]
set builderClass [$builderGen bgen $geom]
set diagramBuilder [$builderClass new]

set c [$diagramBuilder parse $d]
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

## TODOS:
## - fix value passing for quoted, braced values ... too many nesting levels
## - add support for lazily acquired references/paths for fields --> adj:(`$root lines $current` Str)

## 5) -> Sanity checks at all steps

# Local variables:
#    mode: tcl
#    tcl-indent-level: 2
#    indent-tabs-mode: nil
# End:
