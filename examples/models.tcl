# -*- Tcl -*-

package req Tcl 8.6

apply {{version code {test ""}} {
  set script [file normalize [info script]]
  set modver [file root [file tail $script]]
  lassign [split $modver -] ns relVersion
  # set prj [file tail [file dirname $script]]
  set paths [dict create]
  set scriptPath [file split [file dirname $script]]
  dict set paths {*}$scriptPath $ns
  set pwdPath [file split [pwd]]
  if {![dict exists $paths {*}$pwdPath]} {
    throw {NSF MODULE OUTSIDE} "NSF module called outside the project tree."
  }
  set pwdPathLength [llength $pwdPath]
  set scriptPathLength [llength $scriptPath]
  if {$pwdPathLength == $scriptPathLength} {
    set prj [lindex $pwdPathLength end]
  } else {
    set prj [join [lrange $scriptPath [expr {$pwdPathLength-1}] end] "::"]
  }
  
  if {$relVersion ne ""} {
    set version $relVersion
  }

  package provide ${prj}::$ns $version
  # namespace eval ${prj}::$ns $code

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
    namespace eval ${prj}::$ns $code
  }
} ::} 0.1 {

  package req djdsl::lm
  namespace import ::djdsl::lm::*

  #
  # == Language models for Chapter 7
  #
  
  Asset create Graphs {
    
    LanguageModel create Graph {
      :property name:alnum
      :property -incremental edges:object,type=Graph::Edge,0..n
      
      Classifier create Node {
        :property -accessor public name:required
      }
      Classifier create Edge {
        :property -accessor public a:object,type=Node
        :property -accessor public b:object,type=Node
      }
    }
    
    Collaboration create weighted {
      Classifier create Weight {
        :property -accessor public {value:integer 0}
      }
      Role create Edge {
        :property -accessor public weight:object,type=Weight {
          :public object method value=isSet {obj prop} {
            ::nsf::var::exists $obj $prop
          }
        }
      }
    }
  }
  
  Composition create WeightedGraphs \
      -binds [Graphs] \
      -base [Graphs::Graph] \
      -features [Graphs::weighted]

  Asset create Colours {
    Collaboration create coloured {
      Classifier create Colour {
        :property -accessor public {value "#fff"}
      }
      Role create Edge {
        :property -accessor public \
            colour:object,type=Colour {
              :public object method value=isSet {obj prop} {
                ::nsf::var::exists $obj $prop
              }
            }
      }
    }
  }; # Colours
  
  Composition create MultiFeatGraph \
      -binds {Graphs Colours} \
      -base [Graphs::Graph] \
      -features [list [Colours::coloured] [Graphs::weighted]]

  namespace export {*}[namespace export] Graphs WeightedGraphs \
      Colours MultiFeatGraph
  
  #
  # === Miss Grant's Controller
  #

  Asset create Behaviours {
    LanguageModel create StateMachine {
      :property -accessor public start:object,type=StateMachine::State

      Classifier create State {
        :property -accessor public name
        :property -accessor public actions:0..*,object,type=Command

        :property -accessor public {transitions:substdefault "[dict create]"} {
          :public object method value=set {obj prop arg} {
            if {[string is list $arg] && ([llength $arg] & 1) == 0} {
              foreach {event transition} $arg {
                :value=add $obj $prop $event $transition
              }
            }
          }
          
          :public object method value=add {obj prop event transition} {
            $obj eval [list dict set :transitions [$event name get] $transition]
          }
          :public object method value=delete {obj prop event} {
            $obj eval [list dict unset :transitions [$event name get]]
          }
          :public object method value=get {obj prop event} {
            set transitions [next [list $obj $prop]]
            set key [$event name get]
            if {[dict exists $transitions $key]} {
              return [dict get $transitions $key]
            } else {
              return -code error "There is no transition for a trigger event '$key'."
            }
          }
        }
      }

      Classifier create Transition {
        :property -accessor public source:object,type=State
        :property -accessor public target:object,type=State
        :property -accessor public trigger:object,type=Event
      }

      Classifier create AbstractEvent {
        :property -accessor public name
        :property -accessor public code:alnum
      }

      Classifier create Event -superclasses AbstractEvent
      Classifier create Command -superclasses AbstractEvent
    }; # StateMachine
  }; # Behaviours

  Asset create GuardedBehaviours {
    Collaboration create StateMachine {
      Role create Transition {
        :property -accessor public guard:object,type=EvaluableExpr::Model::Expression
      }
    }
  }
  
  namespace export {*}[namespace export] Behaviours

  #
  # === Boolean and comparison expression language (BCEL)
  #
  
  Asset create Expressions {
    LanguageModel create Model {
      Classifier create Expression
      Classifier create BooleanOrComparison -superclasses Expression {
        :property operator:required; # =, <>, >=, <=, >, <, &&, ||
        :property leftExpr:object,type=Expression,required
        :property rightExpr:object,type=Expression,required
      }

      Classifier create Atomic -superclasses Expression
      Classifier create Number -superclasses Atomic {
        :property -accessor public value:double,required
      }
      Classifier create VariableRef -superclasses Atomic {
        :property -accessor public variableName:alnum,required
      }
    }; # Model

    Collaboration create Eval {
      Role create BooleanOrComparison {
        :public method evaluate {context} {
          tcl::mathop::${:operator} [${:leftExpr} evaluate $context] \
              [${:rightExpr} evaluate $context]
          
        }
      }
      Role create Number {
        :public method evaluate {context} {
          return ${:value}
        }
      }
      Role create VariableRef {
        :public method evaluate {context} {
          dict with context {
            set ${:variableName}
          }
        }
      }
    }; # Eval
  }; # Expressions
  
  Composition create EvaluableExpr \
      -binds [Expressions] \
      -base [Expressions::Model] \
      -features [Expressions::Eval]

  namespace export {*}[namespace export] Expressions EvaluableExpr


  #// gSMDL //
  Asset create GuardedBehaviours {
    Collaboration create StateMachine {
      Role create Transition {
        namespace import ::djdsl::examples::models::Expressions;
        :property -accessor public \
            guard:object,type=[Expressions]::Model::Expression {
              :public object method value=isSet {obj prop} {
                ::nsf::var::exists $obj $prop
              }
            }
      }
    }
  }

  Composition create GuardableStateMachine \
      -binds [list [Behaviours] [Expressions]] \
      -base [Behaviours::StateMachine] \
      -features [GuardedBehaviours::StateMachine]
  #// end //
  
  namespace export {*}[namespace export] GuardedBehaviours GuardableStateMachine

  #
  # A skeleton hinting at full ansible playbooks:
  # See http://docs.ansible.com/ansible/latest/user_guide/playbooks_intro.html#playbook-language-example
  # 
  
  Asset create Ansible {
    
    LanguageModel create Playbook {
      :property -incremental plays:object,type=Playbook::Play,1..*
      
      Classifier create Play {
        :property -accessor public tasks:object,type=Task,1..*
        :property -accessor public hosts
        :property -accessor public remote_user
      }
      
      Classifier create Task {
        :property -accessor public name
        # modules
        :property -accessor public service
        :property -accessor public yum
      }
    }
  }

  namespace export {*}[namespace export] Ansible

  
} {
  #
  # == Doctests
  #

  package req djdsl::lm
  namespace import ::djdsl::lm::*

  set StateMachine [Behaviours info children -type LanguageModel]
  ? {llength $StateMachine} 1
  ? {llength [$StateMachine info children -type Classifier]} 5
  
  ? {lmap cl [lsort [$StateMachine info children -type Classifier]] {
    $cl info name
  }} "AbstractEvent Command Event State Transition"

}

# Local variables:
#    mode: tcl
#    tcl-indent-level: 2
#    indent-tabs-mode: nil
# End:
#
