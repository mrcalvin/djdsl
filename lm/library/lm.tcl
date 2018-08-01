# -*- Tcl -*-
#
# MIT License
#
# Copyright (c) 2017, 2018 Stefan Sobernig <stefan.sobernig@wu.ac.at>
#
# Permission is hereby granted, free of charge, to any person obtaining a copy
# of this software and associated documentation files (the "Software"), to deal
# in the Software without restriction, including without limitation the rights
# to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
# copies of the Software, and to permit persons to whom the Software is
# furnished to do so, subject to the following conditions:
#
# The above copyright notice and this permission notice shall be included in all
# copies or substantial portions of the Software.
#
# THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
# IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
# FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
# AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
# LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
# OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
# SOFTWARE.

package req Tcl 8.6

apply {{version prj code {test ""}} {
  set script [file normalize [info script]]
  set modver [file root [file tail $script]]
  lassign [split $modver -] ns relVersion
  
  if {$relVersion ne ""} {
    set version $relVersion
  }

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
        ::tcltest::configure {*}$::argv
        ::tcltest::loadTestedCommands
        
        namespace eval ${prj}::$ns $code
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
  } else {
    package provide ${prj}::$ns $version
    namespace eval ${prj}::$ns $code
  }
} ::} 0.1 djdsl {

  #
  # == Implementation
  #
  
  package req nx

  nx::Class create Container {
    :protected method __object_configureparameter {} {
      set spec [next]
      lreplace $spec[set spec {}] end end contains:alias,optional
    }
    ::nsf::parameter::cache::classinvalidate [current]
    :public method contains args {
      namespace eval [self] { namespace path ::djdsl::lm }
      next
    }
  }
  
  nx::Class create Asset -superclasses Container

  nx::Class create AssetElement -superclasses {Container nx::Class}
  
  nx::Class create Role -superclasses AssetElement {
    :public object method create args {
      set container [uplevel [current callinglevel] {namespace current}]
      if {[$container info has type LanguageModel]} {
        throw {DJDSL LM INVALIDEL} "Invalid element: Language models cannot contain roles."
      }
      # TODO: overriding [create] breaks namespace setting for
      # new/create calls, e.g., in contains.  set r [next]; use
      # [apply] for the time being to correct the namespace context.
      set r [apply [list {} {next} $container]]
      return $r
    }
  }
  
  nx::Class create Classifier -superclasses Role
  
  nx::Class create Collaboration -superclasses AssetElement {
    :public method create args {
      if {[:info class] eq [current class]} {
        throw {DJDSL ABSTRACT} "Collaboration [self] cannot be instantiated directly"
      }
      next
    }
  }
  
  nx::Class create LanguageModel -superclasses Collaboration {
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
      set self [self]::$name
      set vargs [string cat "{*}" $ args]
      :public method "new [string tolower $name]" args \
          [subst -nocommands {
            if {[:info lookup method mk$name] ne ""} {
              :mk$name $self $vargs
            } else {
              $self new -childof [self] $vargs
            }
          }]
    }
  }

   
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


    :private method patch {context ancestors compositionClasses} {
      if {[lindex $ancestors 0] ne "::nx::Object"} {
        if {[lindex $ancestors end] eq "::nx::Object"} {
          set ancestors [lreplace $ancestors end end]
        }
        # patch any base-level sub-/superclass relationships using
        # the corresponding composition classes.
        set revMap [lreverse $compositionClasses]
          set ancestors [lmap e $ancestors {
            if {[dict exists $revMap $e]} {
              string cat $context :: [$e info name]
            } else {
              set e
            }
          }]
        return $ancestors
      } else {
        list
      }
    }
    
    :private method weave {-baseClass -featureModules -context} {
      set d [: -local computeExtensionHierarchy]

      set collaborationClassNames [dict keys [dict get $d class]]
      # Let the resulting language model (context) inherit from the extension classes and the base class.
      set superclasses [list {*}[concat {*}[dict get $d extension ${:base}]] ${:base}]
      nsf::relation::set $context superclass \
          [list {*}$superclasses {*}[$context info superclasses]]

      # batch create the composition classes, so they can be used
      # directly in patching the generalisations below.
      foreach name $collaborationClassNames {
        Classifier create ${context}::$name
      }
      
      foreach name $collaborationClassNames {
        set expansion [[dict get $d class $name] info superclasses -closure]
        set expansion [: -local patch $context $expansion [dict get $d class]]

        set extension [list]
        foreach r [dict get $d extension $name] {
          lappend extension $r
          lappend extension {*}[: -local patch $context \
                                    [$r info superclasses -closure] \
                                    [dict get $d class]]
        }

        set supers [list {*}$extension \
                        [dict get $d class $name] \
                        {*}$expansion]
        
        nsf::relation::set ${context}::$name superclass $supers
        $context createFactory ${context}::$name
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

  #
  # == Doctests
  #

  #// Leads to "::nsf::log Warning {cycle in the mixin graph list detected for class ::nx::Object}"
  #// nx::Object mixins add Testable

  # An `Asset` is the mandatory container element for both
  # `LanguageModel` and `Collaboration` instances. When defining
  # compositions, in a subsequent step, an `Asset` is referenced (or,
  # bound) by a `Composition`.
  # 
  # The running example roughly follows the storyline behind a Graph
  # Product Line (GPL), as used in <<FOSPL>> (see, e.g., Chapter X for
  # some background).
  #
  # The `Graphs` asset contains a language model `Graph` and a
  # collaboration `weighted`.  From the point of view of a GPL
  # variability model (not shown), the language model maps to and
  # implements the root or base feature. The collaboration maps to and
  # implements the option feature `weighted`.
  
  #// assets //#
  Asset create Graphs {    
    LanguageModel create Graph {
      :property name:alnum
      :property -incremental edges:object,type=Edge,0..n
      Classifier create A
      Classifier create Node
      Classifier create Edge {
        :property -accessor public a:object,type=Node
        :property -accessor public b:object,type=Node
      }
    }
    Collaboration create weighted {
      Classifier create Weight {
        :property -accessor public {value:integer 0}
      }
      Role create A
      Role create Edge -superclasses A {
        :property -accessor public weight:object,type=Weight
      }
    }
  }
  #// end //#

  # A language model itself contains only `Classifier` instances that
  # describe the main elements of the primary abstract syntax of a
  # given DSL (e.g., `Node` and `Edge`).
  #
  # A collaboration contains both `Classifier` instances (i.e.,
  # introducing new abstract-syntax elements such as `Weight`) and
  # `Role` instances. Roles are refinements of previously introduced
  # or already present abstract-syntax elements (e.g., `Edge`). Roles
  # cannot be instantiated directly.
  #
  # A `Classifier` or a `Role` instance can contain structural and
  # behavioural elements, i.e., properties and methods. From the
  # perspective of an abstract-syntax definition, methods are not
  # necessary. However, collaborations can be also be used also to
  # structure the behavioural implementations as optional (composable)
  # units that back a DSL (e.g., an interpreter or visitor-based
  # transformations).
  #
  # Technically, this conceptual nesting between collaborations and
  # classifiers/ roles is implemented as nesting the NX objects
  # representing them. Watch:
  
  ? {[Graphs new graph -name "g"] info class} "::Graphs::Graph"
  ? {[Graphs new weighted] info class} {unable to dispatch sub-method "weighted" of ::Graphs new; valid are: new graph}
  ? {[[Graphs new graph -name "g2"] new node] info class} "::Graphs::Graph::Node"

  #
  # The definitional content of language models and collaborations is
  # open for extension using method `contains`.
  #

  ? {Graphs::Graph contains {
    Classifier create Label
  }} "::Graphs::Graph::Label"

  #
  # Standard NX introspection methods can be used to spell out the
  # content of a container (assets or collaborations), for example:
  #
  
  ? {lsort [Graphs::Graph info children -type AssetElement]} "::Graphs::Graph::A ::Graphs::Graph::Edge ::Graphs::Graph::Label ::Graphs::Graph::Node"

  #
  # However, the abstract-syntax content is defined, important
  # consistency conditions are tested and established. For example,
  # language models must not contain roles.
  #

  ? {Graphs::Graph contains {
    Role create Label
  }} "Invalid element: Language models cannot contain roles."

  # Object nesting has a number of benefits. It facilitates product-bound
  # quantification, object cleanup, and it guarantees name-based
  # qualification for classifiers and roles (`Graphs::Graph::Edge`
  # vs. `weighted::Edge`). Also note that each container
  # (collaboration or language model) provides factory methods (`new
  # graph`, `new weighted`) for its contained classifiers or roles.

  #
  # A `Composition` is defined to implement one product, mapping to
  # one configuration valid under the variability model. A composition
  # itself realises an asset and binds some assets as provides of
  # language models and collaborations (incl. other compositions!). 
  #
  # The GPL configuration for weighted graphs is implemented as
  # follows:
  
  #// comp1 //#
  Composition create WeightedGraphs \
      -binds Graphs \
      -base [Graphs::Graph] \
      -features [Graphs::weighted]
  #// end //#

  # A composition such as `WeightedGraphs` can then be used then to
  # instantiate a configured language model (using a factory method
  # `new graph`). This language model, in turn, provides factories for
  # creating configured abstract-syntax elements (e.g.,
  # weight-labelled edges):
  
  #// comp2 //#
  set wg [WeightedGraphs new graph -name "wg"]
  set n1 [$wg new node]
  set n2 [$wg new node]  
  set e [$wg new edge \
             -a $n1 \
             -b $n2 \
             -weight [$wg new weight -value 1]]
  #// end //#

  #
  # Basic NX introspection techniques such as
  # https://next-scripting.org/xowiki/docs/nx/api/Object/man#27[`info
  # precedence`] can be used to reveal the composition order between
  # roles and classifiers, starting from a given composition:
  #

  ? {$wg info precedence} \
      "::WeightedGraphs::Graph ::Graphs::weighted ::Graphs::Graph ::nx::Object"

  ? {$n1 info precedence} \
      "::WeightedGraphs::Graph::Node ::Graphs::Graph::Node ::nx::Object"

  ? {$e info precedence} \
      "::WeightedGraphs::Graph::Edge ::Graphs::weighted::Edge ::Graphs::weighted::A ::Graphs::Graph::Edge ::nx::Object"

  #
  # The pool of collaborations that implement optional features
  # (`coloured`) can be organised across several separate assets
  # (`Colours`):
  #
  
  Asset create Colours {
    Collaboration create coloured {
      Classifier create Color {
        :property -accessor public {value 0}
      }
      Classifier create B
      Role create Edge -superclasses B {
        :property -accessor public colour:object,type=Color
      }
      :public method colored {} {return 1}
    }
  }

  #
  # When defining a composition, all providing assets must be passed
  # as arguments to `-binds`:
  #
  
  set ccomp [Composition new -binds [list [Graphs] [Colours]] \
                 -base [Graphs::Graph] \
                 -features [Colours::coloured]]
  
  set cg [$ccomp new graph -name "cg"]
  ? {$cg info precedence} \
      "${ccomp}::Graph ::Colours::coloured ::Graphs::Graph ::nx::Object"

  #
  # For a more general background on NX support for
  # collaboration-based designs, see our paper presented at
  # <<FOSD>>. Note that the implementation used as part of DjDSL goes
  # beyond the fundamentals presented in this paper; and deviates in
  # some details.
  #

  # [bibliography]
  # == References
  # 
  # - [[[FOSPL]]] Apel, S., Batory, D., Kästner, C., & Saake, G. (2013). Feature-Oriented Software Product Lines (1st). Springer.
  # - [[[FOSD]]] Sobernig, S., Neumann, G., & Adelsberger, S. (2012). Supporting Multiple Feature Binding Strategies in NX. In Proc. 4th International Workshop on Feature-Oriented Software Development (FOSD'12) (pp. 45--53). ACM.

}

# Local variables:
#    mode: tcl
#    tcl-indent-level: 2
#    indent-tabs-mode: nil
# End:
