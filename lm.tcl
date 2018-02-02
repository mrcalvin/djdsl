# -*- Tcl -*-

package req Tcl 8.6

apply {{version code {test ""}} {
    set script [file normalize [info script]]
  set modver [file root [file tail $script]]
  lassign [split $modver -] ns relVersion
  set prj [file tail [file dirname $script]]
  
  if {$relVersion ne ""} {
    set version $relVersion
  }

  package provide ${prj}::$ns $version
  namespace eval ${prj}::$ns $code

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
        namespace eval ::${prj}::${ns}::test {
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
        
        namespace eval ::${prj}::${ns}::test [list namespace import ::${prj}::${ns}::*]
        namespace eval ::${prj}::${ns}::test $test
        
        namespace eval ::${prj}::${ns}::test cleanupTests
        namespace delete ::${prj}::${ns}::test
      }
    }
  }
} ::} 0.1 {

  package req nx


  nx::Class create AssetElement -superclasses nx::Class
  nx::Class create Role -superclasses AssetElement
  nx::Class create Classifier -superclasses Role
  
  nx::Class create Collaboration -superclasses AssetElement {
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
      set body "[self] new -childof ${:owning} {*}\$args"
      ${:owning} public object method "new [string tolower [namespace tail [self]]]" args $body
      foreach c [:info children -type Role] {
        :createFactory $c
      }
      next
    }

    :public method createFactory {nested:class} {
      # Create accessors for the collaboration parts
      set name [namespace tail $nested]
      :public method "new [string tolower $name]" args \
          [subst {[self]::$name new -childof \[self\] {*}\$args}]
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
        # puts stderr "dict lappend d extension $baseClass $collaboration"
        # dict lappend d extension $baseClass $collaboration
        dict with d extension { lappend $baseClass $collaboration }
        foreach roleClass [$collaboration info children -type ::nx::Class] {          
          set name [$roleClass info name]
          if {[dict exists $d class $name]} {
            # known role class
            dict with d extension { lappend $name $roleClass }
            # dict set d extension $name $roleClass
          } else {
            # unknown role class
            dict set d class $name $roleClass
            # dict lappend d extension $name ""
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
      set superclasses [list {*}[concat {*}[dict get $d extension ${:base}]] ${:base}]
      nsf::relation::set $context superclass [list {*}$superclasses {*}[$context info superclasses]]

      foreach name $collaborationClassNames {
        set supers [list {*}[dict get $d extension $name] [dict get $d class $name]]
        set cls [Classifier create ${context}::$name -superclasses $supers]
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
  
  namespace export Asset AssetElement Composition Collaboration LanguageModel \
      Classifier Role
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

      Classifier create [self]::A
      Classifier create [self]::Node
      Classifier create [self]::Edge {
        :property -accessor public from
        :property -accessor public to
      }
    }
    
    Collaboration create weighted {
      Classifier create [self]::Weight {
        :property -accessor public {value 0}
      }
      Role create [self]::A
      Role create [self]::Edge -superclasses [self]::A {
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
      Classifier create [self]::Color {
        :property -accessor public {value 0}
      }
      Classifier create [self]::B
      Role create [self]::Edge -superclasses [self]::B {
        :property -accessor public colour:object,type=[namespace current]::Color
      }
      :public method colored {} {return 1}
    }
  }
  
  set ccomp [Composition new -binds [list [Graphs] [Colours]] \
                 -base [Graphs::Graph] \
                 -features [coloured]]
  
  set cg [$ccomp new graph -name "cg"]
  ? {$cg info precedence} \
      "${ccomp}::Graph ::coloured ::Graphs::Graph ::nx::Object"
}

# Local variables:
#    mode: tcl
#    tcl-indent-level: 2
#    indent-tabs-mode: nil
# End:
