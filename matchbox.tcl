package req nx

namespace eval ::matchbox {
  
  nx::Class create Pattern {
    :property -accessor public variables:0..*,object,type=::nx::Object
    :public method match {term:object,type=::nx::Object} {
      return "Not implemented for [:info class]"
    }
    :public method substitute {bindings} {
      return "Not implemented for [:info class]"
    }
  }
  
  nx::Class create ObjectPattern -superclasses Pattern {
    :property pattern:object,type=::nx::Object
    
    :public method match {term:object,type=::nx::Object} {
      return [expr {${:pattern} eq $term}]
    }
    
  }

  nx::Class create ListPattern -superclasses Pattern {
    :property pattern
    
    :public method match {term} {
      if {[llength ${:pattern}] != [llength $term]} {
        return 0
      }
      
      foreach p ${:pattern} t $term {
        if {![[ObjectPattern new -pattern $p] match $t]} {
          return 0
        }
      }
      return 1
    }
  }

  nx::Class create OTPattern -superclasses ListPattern {

    :property -accessor public {bindings:substdefault {[dict create]}} {
      :public object method value=has {obj prop var} {
        set bindings [:value=get $obj $prop]
        return [dict exists $bindings $var]
      }
      :public object method value=set {obj prop var term:optional} {
        if {![info exists term]} {
          next
        } else {
          set bindings [:value=get $obj $prop]
          puts bindings=$bindings
          dict set bindings $var $term
          next [list $obj $prop $bindings]
        }
      }
      :public object method value=get {obj prop {var *}} {
        set bindings [next [list $obj $prop]]
        return [dict filter $bindings key $var]
      }
    }
    
    :public method bind {pattern term rootPattern} {
      if {![$rootPattern eval {info exists :variables}]} {return 0}
      puts stderr "$pattern -> [$rootPattern variables get]"
      if {$pattern in [$rootPattern variables get]} {
        if {[$rootPattern bindings has $pattern]} {
          return [expr {[$rootPattern bindings get $pattern] eq $term}]
        } else {
          $rootPattern bindings set $pattern $term
          return 1
        }
      } else {
        return 0
      }
    }

    :public method matchHeads {ph th} {

      if {[$ph info class] ne [$th info class]} {
        return 0
      }

      foreach phv [$ph info vars] {
        if {![$th eval [list info exists :$phv]]} {
          return 0
        }
        if {[$th eval [list set :$phv]] ne [$ph eval [list set :$phv]]} {
          return 0
        }
      }
      return 1
    }
    
    :public method match {term {rootPattern:substdefault {[self]}}} {
      # term <- list<object>/2
      # :pattern <- list<object>/2
      lassign $term ti tj
      lassign ${:pattern} pi pj
      puts stderr "{$pi $ti}"
      if {[:bind $pi $ti $rootPattern]} {
        return 1
      }
      
      if {![:matchHeads $pi $ti]} {
        return 0
      }
      
      if {[llength $pj] != [llength $tj]} {
        return 0
      }
      
      if {[llength $pj] == 2} {
        return [[[current class] new -pattern $pj] match $tj $rootPattern]
      }
      
      foreach {p1 p2} $pj {t1 t2} $tj {
        if {![[[current class] new -pattern [list $p1 $p2]] match [list $t1 $t2] $rootPattern]} {
          return 0
        }
      }
      return 1
    }

    nx::Class create [self]::Class {
      
      :public object method on {pattern} {
        nx::Class mixins add [self]
        set :children [dict create]
        interp alias {} $ {} apply {{p args} {
          try $args on ok {obj} { $p variables add $obj; return $obj }
        }} $pattern
      }
      :public object method off {} {
        nx::Class mixins delete [self]
        interp alias {} $ {}
        set out {*}[dict values ${:children}]
        # puts stderr [lmap o ${:objects} {$o info class}]
        unset -nocomplain :children
        unset -nocomplain :rootPattern
        set :parent 0
        return $out
      }
      :public method create args {
        set p [[current class] eval {incr :parent}]
        set inst [next]
        [current class] eval {incr :parent -1}
        # TODO: refactor into mixin class
        if {[[current class] eval "dict exists \${:children} $p"]} {
          lappend inst [[current class] eval "dict get \${:children} $p"]
          [current class] eval [list dict unset :children $p]
        }
        [current class] eval "dict lappend :children \${:parent} $inst"
        return $inst
      }
    }
    
    :public object method newFromScript {cmds} {
      set p [:new]
      [self]::Class on $p
      # TODO: use the caller's namespace?
      apply [list {} $cmds]
      $p configure -pattern [[self]::Class off]
      return $p
    }
    
  }
  
  namespace export Pattern ObjectPattern ListPattern OTPattern
  
}

package req nx::test
namespace import ::matchbox::*

set a [nx::Object create ::a]
set pattern [ObjectPattern new -pattern $a]

? {$pattern match $a} 1

set l [list [nx::Object create ::b] [nx::Object create ::c] [nx::Object create ::d]]
set p [ListPattern new -pattern $l]
? {$p match [list $a [nx::Object create ::c] [nx::Object create ::d]]} 0
? {$p match $l} 1
? {$p match [list $a [nx::Object create ::c]]} 0



##
## O(bject)T(ree)Pattern
##
## list-encoded, object-only syntax-directed parsing result (tree)

nx::Class create ::Drawing {
  :create ::d
}
nx::Class create ::Line {
  :create ::l1
  :create ::l2
  :create ::l3
}
nx::Class create ::Point {

  :property x:integer
  :property y:integer

  :create ::p1 -x 1 -y 2
  :create ::p2 -x 3 -y 4
  :create ::p3 -x 5 -y 6
  :create ::p4 -x 7 -y 9
  :create ::p5 -x 10 -y 11
  :create ::p6 -x 7 -y 9; # equal to p4
  :create ::p7 -x 7;
  
}

set aTerm [list ::d [list \
                         ::l1 [list ::p1 ::p2] \
                         ::l2 [list ::p3 ::p4]]]

set aPattern [OTPattern new -pattern {::d {::l1 {::p1 ::p2} ::l2 {::p3 ::p4}}}]
? {$aPattern match $aTerm} 1
? {$aPattern match {::d {::l2 {::p3 ::p4}} ::l1 {::p1 ::p2}}} 0
? {$aPattern match {::d {::l2 ::l1}}} 0

? {$aPattern match {::d {::l1 {::p1 ::p2} ::l2 {::p3 ::p5}}}} 0
? {$aPattern match {::d {::l1 {::p1 ::p2} ::l2 {::p3 ::p6}}}} 1

set bPattern [OTPattern new -pattern {::d {::l1 {::p1 ::p2} ::l3 {::p3 ::p7}}}]
# $bPattern variables add ::p7; # fetch ::p6
$bPattern variables add ::l3; # fetch ::l2
? {$bPattern match {::d {::l1 {::p1 ::p2} ::l2 {::p3 ::p6}}}} 1
# ? {$bPattern bindings get} "::p7 ::p6"
? {$bPattern bindings get} "::l3 ::l2"

set cPattern [OTPattern newFromScript {
  Drawing new {
    Line new {
      Point new -x 1 -y 2
      Point new -x 3 -y 4
    }
    Line new {
      Point new -x 5 -y 6
      Point new -x 7 -y 9
    }
  }
}]

? {$cPattern match {::d {::l1 {::p1 ::p2} ::l2 {::p3 ::p6}}}} 1
? {$cPattern match {::d {::l1 {::p1 ::p2} ::l2 {::p3 ::p7}}}} 0

## DONE: add $ marker to newFromScript notation -> /pattern/ variable add

set dPattern [OTPattern newFromScript {
  Drawing new {
    Line new {
      Point new -x 1 -y 2
      Point new -x 3 -y 4
    }
    Line new {
      $ Point new -x 5
      Point new -x 7 -y 9
    }
  }
}]

? {$dPattern match {::d {::l1 {::p1 ::p2} ::l2 {::p3 ::p6}}}} 1
? {lindex [$dPattern bindings get] 1} ::p3
? {[lindex [$dPattern bindings get] 1] cget -y} 6
? {$dPattern match {::d {::l1 {::p1 ::p2} ::l2 {::p3 ::p7}}}} 0


##
## TODO: refactor
## TODO: integrate with OPEG + test suite (test helper?)
## TODO: visitor support (to enter matching at arbitrary level)
##

## 
## Check: pattern matching using tagged lists in http://wiki.tcl.tk/9547
##

##
# Local variables:
#    mode: tcl
#    tcl-indent-level: 2
#    indent-tabs-mode: nil
# End:




