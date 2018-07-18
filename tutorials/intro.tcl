# -*- Tcl -*-

package req Tcl 8.6

apply {{version code {test ""}} {
  set script [info script]
  set modver [file root [file tail $script]]
  lassign [split $modver -] ns relVersion
  if {$relVersion ne ""} {
    set version $relVersion
  }

  package provide $ns $version
  namespace eval $ns $code

  if {[info exists ::argv0] && $::argv0 eq [uplevel 1 {info script}]} {
    if {"--release" in $::argv} {
      try {
        file copy -force -- $script ${ns}-${version}.tm
      } on error {e} {
        puts stderr "Failed to create release file: '$e'"
      } finally {
        set ::argv [lsearch -glob -inline -all -not $::argv "--release"]
      }
    } elseif {"--print" in $::argv} {
      try {
        if {$test ne ""} {
          puts stdout [string trim [regsub -line -all {^[ \t][ \t]} $test ""]]
        }
        puts stdout [string trim [regsub -line -all {^[ \t][ \t]} $code ""]]
      } finally {
        set ::argv [lsearch -glob -inline -all -not $::argv "--print"]
      }
    } else {
      if {$test ne ""} {
        package req tcltest
        namespace eval ::${ns}::test {
          namespace import ::tcltest::*
          
          ::proc ? {script expected} {
            set ctr [incr [namespace current]::counter]
            uplevel [list test test-$ctr "" -body $script -result $expected \
                         -returnCodes {0 1 2}]
          }
        }
        
        namespace eval ::${ns}::test [list namespace import ::${ns}::*]
        namespace eval ::${ns}::test $test
        
        namespace eval ::${ns}::test cleanupTests
        namespace delete ::${ns}::test
      }
    }
  }
} ::} 0.1 {

  #
  # == Implementation
  #


  package req nx

  #
  # === Abstract syntax
  #
  
  #// lm //
  nx::Class create Expr
  nx::Class create Add -superclasses Expr {
    :property leftExpr:object,type=Expr,required
    :property rightExpr:object,type=Expr,required
  }
  nx::Class create Lit -superclasses Expr {
    :property -accessor public value:double,required
  }
  #// end //
  
  namespace export Expr Lit Add

  
  nx::MetaSlot create ::nx::AssertionSlot -superclasses ObjectParameterSlot {

    :property settername; # TODO: PULL UP into ObjectParameterSlot?
    :public alias value=set ::nsf::method::assertion; # TODO: ::nsf::method::assertion::set
    :public alias value=get ::nsf::method::assertion; # TODO: ::nsf::method::assertion::get

    :public method value=add {obj prop value {pos 0}} {
      set old [:value=get $obj $prop]
      set new [linsert $old $pos $value]
      :value=set $obj $prop $new
      return $new
    }
    
    :method init {} {
      next
      if {${:accessor} ne ""} {
        :makeForwarder
      }
    }
    
  }
  
  
  ::nx::AssertionSlot create ::nx::Class::slot::invariants \
      -multiplicity 0..n \
      -defaultmethods {} \
      -disposition slotset \
      -forwardername "class-invar"
  
  ::nx::AssertionSlot create ::nx::Object::slot::object-invariants \
      -multiplicity 0..n \
      -defaultmethods {} \
      -disposition slotset \
      -forwardername "object-invar" \
      -settername "object invariants"


  nx::Class public method addInvariants {_ invariant args} {
    :invariants add $invariant
    if {[llength $args]} {
      :addInvariants {*}$args
    }
  }

  interp alias {} context: {} apply {{context args} {$context addInvariants {*}$args}}

  #// builder //
  nx::Class create AleBuilder {
    
    :forward + %self operator Add
    
    :method operator {class} {
      if {[llength ${:opds}] == 2} {
        lassign ${:opds} l r
        set :opds [$class new -childof [self] -leftExpr $l -rightExpr $r]
      } else {
        return -code error "Invalid number of operands for binary operator '$class'."
      }
    }
    # DYNAMIC RECEPTION
    :method unknown {v args} {
      lappend :opds [Lit new -childof [self] -value $v]
    }
    :public method from {expr} {
      foreach element [lreverse $expr] {
        puts el=$element
        :$element
      }
      set r [lindex ${:opds} 0]
      unset :opds
      return $r
    }
  }
  #// end //


  nx::Class create ExternalBuilder {
    :property parser:object,type=::pt::rde::nx,required
    :method expression {start end args} {
      if {[llength $args] == 2} {
        return [Add new -childof [self] -leftExpr [lindex $args 0] -rightExpr [lindex $args 1]]
      } elseif {[llength $args] == 1} {
        return [lindex $args 0]; # throw-away current token level
      } else {
        return -code error "Invalid number of operands for expression."
      }
    }
    
    :method number {start end args} {
      return [Lit new -childof [self] -value [string range ${:input} $start $end]]
    }
    
    :method term {start end args} {
      return [lindex $args 0]; # throw-away 'term' token level
    }

    :private method fromAst {ast} {
      set children [lassign $ast nt start end]
      set c [list]
      foreach el [lreverse $children] {
        lappend c [: -local fromAst $el]
      }
      return [:$nt $start $end {*}$c]
    }
    
    :public method from {input} {
      set :input $input
      set ast [${:parser} parset $input]
      set r [: -local fromAst $ast]
      unset :input
      return $r
    }
  }
  
  namespace export AleBuilder ExternalBuilder

  #
  # PARAM/NX runtime: pt::rde::nx
  #

  package require pt::pgen
  package require pt::rde::nx
  
  pt::rde::nx eval {
    
    :public method parset {script} {
      :reset {}
      :data $script
      :MAIN ; # Entrypoint for the generated code.
      set c [:complete]
      lassign $c _ __ endIdx
      # puts >>$c
      # puts >>[string length $script]
      # puts endIdx=$endIdx
      if {($endIdx+1) != [string length $script]} {
        return -code error "Parsing failed."
      }
      return $c
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
      unset -nocomplain :out
      :printNode {*}$ast
      set out [join ${:out} "\n"]
      unset :out
      return $out
    }
    
    :method printNode {{-indent ""} -last:switch symbol start end args} {
      set nrChildren [llength $args]
      set parent [expr {$nrChildren ? "+" : "-"}]
      set pipe [expr {$indent ne "" ? "|" : ""}]
      set lastChild  [expr {$last ? "\\" : $pipe}]
      set output [string cat $indent $lastChild "-" $parent "="]
      append indent [expr {$last ? "  " : "$pipe "}]
      
      lappend :out "$output $symbol :: $start $end"
      
      for {set i 0} {$i < $nrChildren} {incr i} {
        set pargs [list -indent $indent]
        if {$i == $nrChildren-1} {
          lappend pargs -last
        }
        :printNode {*}$pargs {*}[lindex $args $i]
      }
    } 
  }

  #
  # A "little" testing language (akin to SPT & friends)
  #

  proc ::check {description fragment condition args} {
    set ctr [incr [namespace current]::checkCounter]
    set returnCodes {0 1 2}
    set script ""
    switch -- $condition {
      build {
        set args [lassign [lreverse $args] builder _]
        set script [list catch [list $builder from $fragment]]
        set result [expr {$args eq "succeeds" ? 0 : 1}]
      }
      default {error "check condition '$condition' unsupported"}
    }
    set t [list test check-$ctr $description -result $result -body $script \
               -returnCodes $returnCodes]
    puts t=$t
    uplevel $t
  }
  
} {

  #
  # == Doctests
  #
  
  #
  # === Abstract-syntax constraints
  #

  #// constr //
  Lit invariants set {{${:value} >= 10 && ${:value} <= 100}}
  #// end //

  ? {Lit invariants get} {{${:value} >= 10 && ${:value} <= 100}}

  #// constr2 //
  context: intro::Lit \
      inv: {${:value} >= 10 && ${:value} <= 100} \
      inv: {${:value} >= 20 && ${:value} <= 50}
  #// end //

  ? {llength [Lit invariants get]} 3

  #
  # === Concrete syntax(es)
  #

  # Direct instantiation:
  
  #// inst //
  Add new \
      -leftExpr [Lit create one -value 1] \
      -rightExpr [Lit create two -value 2]
  #// end //
  ? {llength [Expr info instances -closure]} 3

  # Internal DSL (indirect instantiation):
  
  #// builder2 //
  set internalBuilder [AleBuilder new]
  set expr1 [$internalBuilder from {+ 1 + 2 4}]
  #// end //
  ? {$expr1 info class} ::intro::Add 

  ? {llength [Expr info instances -closure ${internalBuilder}::*]} 5

  # External DSL (indirect instantiation)

  package require pt::pgen
  package require pt::rde::nx

  set leaGrammar {
    #// leag //
    PEG lea (expression)
    	  expression 	<- _ term (_ '+' _ term)?;
    	  term		<- number / '(' expression ')';
    	  number	<- <digit>+;
    void: _		<- <space>*;
    #// end //
    END;}

  #// parser //
  set leaParser [pt::rde::nx pgen $leaGrammar]
  set lp [$leaParser new]
  $lp print {(2 + 4) + 1}
  #// end //
  
  ? {$lp print {(2 + 4) + 1}} {-+= expression :: 0 10
 |-+= term :: 0 6
 | \-+= expression :: 1 5
 |   |-+= term :: 1 1
 |   | \--= number :: 1 1
 |   \-+= term :: 5 5
 |     \--= number :: 5 5
 \-+= term :: 10 10
   \--= number :: 10 10}


  #// builderExt //
  set externalBuilder [ExternalBuilder new -parser $lp]
  set expr2 [$externalBuilder from {(2 + 4) + 1}]
  #// end //
  ? {$expr2 info class} ::intro::Add 
  ? {llength [Expr info instances -closure ${externalBuilder}::*]} 5

  #
  # === Integration and execution
  #

  #
  # (Host) Interpreter
  #

  #// interp //
  Add public method evaluate {} {
    return [expr {[${:leftExpr} evaluate] +
                  [${:rightExpr} evaluate]}]
  }

  Lit public method evaluate {} {
    return [expr {${:value}}]
  }
  #// end //

  ? {$expr1 evaluate} 7
  ? {$expr2 evaluate} 7

  #
  # (Visitor-based) Generator
  #

  nx::Class create Visitor {
    :public method visit args {
      error "Implement in subclass!"
    }
  }

  nx::Class create CExprVisitor -superclasses Visitor {

    :public method init {} {
      # :require namespace; TODO: type=Expr does not resolve correctly with this, wenn -as is set.
    }
       
    :public method evaluate {{-as integer} expr:object,type=Expr} {

      array set promotions {
        double float
        integer int
      }

      set :promoteTo $promotions($as)
      
      set :opnds [list]
      $expr accept [self]
      set body [string cat "return " {*}${:opnds} ";"]
      set procName [self]::cexpr
      # puts stderr >>>$body,$procName
      try {
        package req critcl
        critcl::cproc $procName {} ${:promoteTo} $body
        puts "critcl::cproc $procName {} ${:promoteTo} $body"
        critcl::load
        set res [$procName]
      } on error msg {
        error "Preparing and executing C call failed: '$msg'."
      } on ok res {
        return $res
      } finally {
        # if {[info commands $procName] ne ""} {
          # rename $procName ""
          # ::critcl::clean_cache
        # }
        unset :opnds
      }
    }
    
    :public method visit {expr:object,type=Expr} {
      :convert [namespace tail [$expr info class]] $expr
    }

    #// convert //
    :method "convert Add" {e} {
      set :opnds [lassign ${:opnds} a b]
      lappend :opnds "($a + $b)"
    }
    :method "convert Lit" {e} {
      set :opnds [list "(${:promoteTo})[$e value get]" {*}${:opnds}]
    }
    #// end //
  }


  Expr public method accept {visitor} {
    error "Implement in subclass!"
  }
  
  Lit public method accept {visitor} {
    $visitor visit [self]
  }

  Add public method accept {visitor} {
    ${:leftExpr} accept $visitor
    ${:rightExpr} accept $visitor
    $visitor visit [self]
  }

  #// cgen //
  set visitor [CExprVisitor new]
  $visitor evaluate $expr2
  #// end //
  
  set r [$visitor evaluate $expr1]
  
  ? {string is integer $r} 1
  ? {set r} 7
  ? {set r [$visitor evaluate -as double $expr1]; string is double $r} 1
  ? {set r} 7.0

  #
  # === Testing
  #

  #// testingInt //
  check "Basic LEA expression (left-associative)" \
      {+ 3 + 1 2} build "succeeds" using $internalBuilder
  
  check "LEA expressions don't support subtraction." \
      {- 3 + 1 2} build "fails" using $internalBuilder
  #// end //

  #// testingExt //  
  check "Basic LEA expression (left-associative)" \
      {(1 + 2) + 3} build "succeeds" using $externalBuilder
  
  check "LEA expressions don't support subtraction." \
      {(1 + 2) - 3} build "fails" using $externalBuilder
  #// end //

  # Next step: pattern matching
  # check "left-associate expression is built consistently" \
  #    {+ 3 + 1 2} build to [$externalBuilder from {(1 + 2) + 3}] \
  #    using $internalBuilder


}

# Local variables:
#    mode: tcl
#    tcl-indent-level: 2
#    indent-tabs-mode: nil
# End:
