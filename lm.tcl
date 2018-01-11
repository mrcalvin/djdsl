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
        set ::argv [lsearch -exact -inline -all -not $::argv "--release"]
      }
    } elseif {"--print" in $::argv} {
      try {
        if {$test ne ""} {
          puts stdout [string trim [regsub -line -all {^[ \t][ \t]} $test ""]]
        }
        puts stdout [string trim [regsub -line -all {^[ \t][ \t]} $code ""]]
      } finally {
        set ::argv [lsearch -exact -inline -all -not $::argv "--print"]
      }
    } else {
      if {$test ne ""} {
        package req tcltest
        namespace eval ::${ns}::test {
          namespace import ::tcltest::*

          customMatch stripNs [list apply {{testNs expected actual} {
            set strippedActual [string map [list ${testNs} ""] $actual]
            expr {$strippedActual eq $expected}
          }} [namespace current]]

          
          ::proc ? {script expected} {
            set ctr [incr [namespace current]::counter]
            uplevel [list test test-$ctr "" -body $script -match stripNs -result $expected \
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

  package req nx
  
  nx::Class create Collaboration -superclass ::nx::Class {
    :public method create args {
      if {[:info class] eq [current class]} {
        throw {DJDSL ABSTRACT} "Collaboration [self] cannot be instantiated directly"
      }
      next
    }
  }
  
  nx::Class create LanguageModel -superclass Collaboration {
    :property {owning:object,type=Asset,substdefault "[:info parent]"}
    :public method init {} {
      set body "[self] new {*}\$args"
      ${:owning} public object method "new [string tolower [namespace tail [self]]]" args $body
      foreach c [:info children -type ::nx::Class] {
        :createFactory $c
      }
      next
    }

    :public method createFactory {nested:class} {
      # Create accessors for the collaboration parts
      set name [namespace tail $nested]
      :public method "new [string tolower $name]" args \
          [subst {[self]::$name new {*}\$args}]
    }
  }

  
  nx::Class create Asset 
  
  nx::Class create Composition -superclasses Asset {
    :property binds:object,type=Asset,1..*
    :property {base:class,required}
    :property {features:0..n,type=Collaboration ""}

    :private method computeExtensionHierarchy {} {
      set baseClass ${:base}
      set featureModules ${:features}

      dict set d extension $baseClass ""
      # Create an extension structure for the base class.
      foreach childclass [$baseClass info children -type ::nx::Class] {
        set name [$childclass info name]
        dict set d extension $name ""
        dict set d class $name $childclass
      }
      
      # For each collaboration (feature), 
      # (1) add the collaboration class to the extension list of the language model and 
      # (2) create/extend the refinements list for the nested role classes.
      foreach collaboration $featureModules {
        dict lappend d extension $baseClass $collaboration
        
        foreach roleClass [$collaboration info children -type ::nx::Class] {
          set name [$roleClass info name]
          if {[dict exists $d class $name]} {
            # known role class
            dict lappend d extension $name $roleClass
          } else {
            # unknown role class
            dict set d class $name $roleClass
            dict set d extension $name ""
          }
        }
      }
      return $d
    }

    :private method weave {-baseClass -featureModules -context} {
      set d [: -local computeExtensionHierarchy]
      set collaborationClassNames [dict keys [dict get $d class]]
      # Let the resulting language model (context) inherit from the extension classes and the base class.
      set superclasses [list {*}[dict get $d extension ${:base}] ${:base}]
      nsf::relation::set $context superclass [list {*}$superclasses {*}[$context info superclasses]]
      
      foreach name $collaborationClassNames {
        set supers [list {*}[dict get $d extension $name] [dict get $d class $name]]
        set cls [nx::Class create ${context}::$name -superclasses $supers]
        $context createFactory $cls
      }
    }

    :public method init {} {
      set ctx [LanguageModel create [self]::[namespace tail ${:base}]]
      : -local weave -baseClass ${:base} \
          -featureModules ${:features} \
          -context $ctx
    }
  }

  nx::Class create Testable {
    :public method "info precedence" {} {
      set p [lsearch -exact -inline -all -not [next] [current class]]
      string map [list [uplevel 1 {namespace current}] ""] $p
    }

    :public method "info class" {} {
      string map [list [uplevel 1 {namespace current}] ""] [next]
    }
  }
  
  namespace export Asset Composition Collaboration LanguageModel Testable
} {

  # Leads to "::nsf::log Warning {cycle in the mixin graph list detected for class ::nx::Object}"
  # nx::Object mixins add Testable
  
  set ctx [Asset new]
  ? {[Collaboration new -childof $ctx] new} "Collaboration ::nsf::__#0::__#1 cannot be instantiated directly"
  ? {catch {[LanguageModel create ${ctx}::C] new}} 0
  ? {[$ctx new c] info class} ${ctx}::C

  #// assets //#
  Asset create Graphs {
    
    LanguageModel create [self]::Graph {
      :property name
      :property -incremental edges:0..n
      Class create [self]::A
      Class create [self]::Node
      Class create [self]::Edge {
        :property -accessor public from
        :property -accessor public to
      }
    }
    
    Collaboration create weighted {
      Class create [self]::Weight {
        :property -accessor public {value 0}
      }
      Class create [self]::A
      Class create [self]::Edge -superclasses [self]::A {
        :property -accessor public weight:object,type=Weight
      }
    }
  }
  #// end //#
  
  ? {[Graphs new graph -name "g"] info class} "::Graphs::Graph"
  ? {[Graphs new weighted] info class} {unable to dispatch sub-method "weighted" of ::Graphs new; valid are: new graph}
  ? {[[Graphs new graph -name "g2"] new node] info class} "::Graphs::Graph::Node"

  #// comp1 //#
  Composition create WeightedGraphs \
      -binds Graphs \
      -base [Graphs::Graph] \
      -features [weighted]
  #// end //#

  #// comp2 //#
  set wg [WeightedGraphs new graph -name "wg"]
  set n1 [$wg new node]
  set n2 [$wg new node]
  set e [$wg new edge -from $n1 -to $n2 -weight [$wg new weight -value 1]]
  #// end //#

  ? {$wg info precedence} \
      "::WeightedGraphs::Graph ::weighted ::Graphs::Graph ::nx::Object"

  ? {$n1 info precedence} \
      "::WeightedGraphs::Graph::Node ::Graphs::Graph::Node ::nx::Object"

? {$e info precedence} \
      "::WeightedGraphs::Graph::Edge ::weighted::Edge ::weighted::A ::Graphs::Graph::Edge ::nx::Object"
  
  Asset create Colours {
    Collaboration create coloured {
      nx::Class create [self]::Color {
        :property -accessor public {value 0}
      }
      nx::Class create [self]::B
      nx::Class create [self]::Edge -superclasses [self]::B {
      :property -accessor public colour:object,type=[namespace current]::Color
      }
      :public method colored {} {return 1}
    }
  }
  
  set ccomp [Composition new -binds [Graphs] \
                 -base [Graphs::Graph] \
                 -features [coloured]]
  
  set cg [$ccomp new graph -name "cg"]
  ? {$cg info precedence} \
      "${ccomp}::Graph ::coloured ::Graphs::Graph ::nx::Object"
}
  
