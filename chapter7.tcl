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

  package req nx
  package req djdsl::lm

  namespace import ::djdsl::lm::*

  #
  # == DSL extension (ex.: DOT definition of graphs)
  #
  
  #
  # === Abstract syntax
  #

  #// assets //#
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
        :property -accessor public weight:object,type=Weight
      }
    }
  }
  #// end //#
  
  nx::Class create GraphBuilder {
    :property -accessor public output:object,type=Graphs::Graph

    # Chain of Responsibility 
    :property -accessor public successor:object,type=[self]
    :public method handleUnknown {args} {
      if {[info exists :successor]} {
         ${:successor} {*}$args
       } else {
         next
       }
     }

    :public method run {script} {
      if {[info commands [self]::runner] eq ""} {
        interp create [self]::runner -safe
        [self]::runner eval {namespace delete ::}
        [self]::runner alias unknown [self] build
        [self]::runner alias // [self] build //
        [self]::runner alias weight [self] build weight
        [self]::runner alias graph [self] run
      }
      [self]::runner eval $script
      # interp delete [self]::runner
    }
  }

  #
  # === Concrete-syntax definition and processing (Builders)
  #

  nx::Class create BaseGraphBuilder -superclasses GraphBuilder {
    
    :method init args {
      if {![info exists :output]} {
        set :output [Graphs new graph]
      }
    }

    :protected method createNodeFromScript {name} {
      if {[info exists :nodes] && [dict exists ${:nodes} $name]} {
        return [dict get ${:nodes} $name]
      } else {
        dict set :nodes $name [set r [${:output} new node -name $name]]
        return $r
      }
    }
    
    :protected method createEdgeFromScript {n1 n2 args} {
      set e [${:output} new edge -a $n1 -b $n2 {*}[concat {*}$args]]
      ${:output} edges add $e
    }

    :public method handleUnknown {_ arg1 args} {
      set n1 [:createNodeFromScript $arg1]
      if {[llength $args]} {
        set args [lassign $args _ name2]
        if {$_ ni {"->" "--"}} {
          throw {GPL DOT UNSUPPORTED} "Unsupported operator '$_' for edge definition."
        }
        set n2 [:createNodeFromScript $name2]
        :createEdgeFromScript $n1 $n2 {*}$args
      }
      return
    }
    
    set mh [:public method "build //" {args} {}]; # discard comments as NOOP

    set slot [namespace qualifiers $mh]
    $slot object mixins add [::nx::Class new {
      :public method "unknown" {callInfo args} {
        set path [lrange $callInfo 1 end-1]
        set m [lindex $callInfo end]
        set obj [lindex $callInfo 0]
        tailcall $obj handleUnknown {*}$path $m {*}$args
      }
    }]

    :public method "build graph" {script} {
      set caller [current callingobject]
      if {$caller ne "" && [$caller info has type GraphBuilder]} {
        $caller eval $script
      } else {
        :eval $script
      }
    }
    
  }; # BaseGraphBuilder


  Composition create WeightedGraphs \
      -binds Graphs \
      -base [Graphs::Graph] \
      -features [Graphs::weighted]
  
  nx::Class create WeightedGraphBuilder -superclasses GraphBuilder {

    :public method output {args} {
      ${:successor} output {*}$args
    }
    
    :method init args {
      ${:successor} output set [WeightedGraphs new graph]
    }
    
    set mh [:public method "build weight" {op value} {
      if {$op ne "="} {
        throw {GPL DOT UNSUPPORTED} "Unsupported operator '$op' in attribute."
      }
      return [list -weight [[${:successor} output get] new weight -value $value]]
    }]

    if {1} {
      set slot [namespace qualifiers $mh]
      $slot object mixins add [::nx::Class new {
        :public method "unknown" {callInfo args} {
          set obj [lindex $callInfo 0]
          set path [lrange $callInfo 1 end]
          tailcall $obj handleUnknown {*}$path {*}$args
        }
      }]
    }
  }

  namespace export GraphBuilder BaseGraphBuilder WeightedGraphBuilder


  #
  # == DSL unification (ex.: State-machine definition language and expr language)
  #
  
  #
  # === Abstract syntax 
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
   
  #
  # === Concrete Syntax
  #

  nx::Class create StateMachineBuilder {
    
    :property -accessor public sm:object,type=Behaviours::StateMachine
    :property -accessor public onEnter:alnum,required

    :property -accessor public currentState:object,type=State

    :public method init args {
      if {![info exists :sm]} {
        set :sm [Behaviours new statemachine]
      }
      set :currentState [${:sm} start set [${:sm} new state -name ${:onEnter}]]
      dict set :states ${:onEnter} ${:currentState}
      
      :object mixins set [current class]::when
    }

    :public method run {script} {
      if {[info commands [self]::runner] eq ""} {
        interp create [self]::runner -safe
        [self]::runner eval {namespace delete ::}
        [self]::runner alias when [self] when
      }
      [self]::runner eval $script
      # interp delete [self]::runner
    }

    nx::Class create [self]::when {
      :public method when args {
        # default to 'when and', on entering the ensemble.
        set args [list "and" {*}$args]
        while {[llength $args]} {
          set args [next $args]
        }
      }
    }

    :public method "when and" {eventName args} {
      set :currentEvent [${:sm} new event -name $eventName]; # -code $eventCode
      return $args
    }

    :public method "when goto" {targetStateName script:optional} {
      if {[info exists :currentState] && [info exists :currentEvent]} {
        if {![dict exists ${:states} $targetStateName]} {
          set tgt [${:sm} new state -name $targetStateName]
          dict set :states $targetStateName $tgt
        } else {
          set tgt [dict get ${:states} $targetStateName]
        }
        # set tgt [${:sm} new state -name $targetStateName]
        set transition [${:sm} new transition -source ${:currentState} -target $tgt]
        ${:currentState} transitions add ${:currentEvent} $transition
        unset :currentEvent
        set :currentState $tgt
      }
      if {[info exists script]} {
        :run $script
      }
      return
    }
  }

  namespace export Behaviours StateMachineBuilder
  
  #
  # === Abstract syntax (boolean and comparison expressions)
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
      -binds Expressions \
      -base [Expressions::Model] \
      -features [Expressions::Eval]
  
  #
  # === Concrete syntax (expr lang)
  #

  nx::Class create ExprBuilder {
    :property model
    :public method init {} {
      if {![info exists :model]} {
        set :model [Expressions new model]
      }
    }

    # :public method run {script} {
    #   if {[info commands [self]::runner] eq ""} {
    #     interp create [self]::runner -safe
    #     [self]::runner eval {namespace delete ::}
    #     [self]::runner alias = [self] build =
    #   }
    #   [self]::runner eval $script
    #   # interp delete [self]::runner
    # }
    
    :forward "build =" %self operator ==
    :forward "build <>" %self operator !=
    :forward "build and" %self operator &
    :forward "build or" %self operator |
    :forward "build >" %self operator >
    :forward "build <" %self operator <
    set mh [:forward "build or" %self operator |]

    set slot [namespace qualifiers $mh]
    $slot object mixins add [::nx::Class new {
      :public method "unknown" {callInfo args} {
        set obj [lindex $callInfo 0]
        set path [lrange $callInfo 1 end]
        tailcall $obj handleUnknown {*}$path {*}$args
      }
    }]

    
    :method operator {op} {
      if {[llength ${:opds}] >= 2} {
        set :opds [lassign ${:opds} l r]
        set :opds [linsert ${:opds}[set :opds {}] 0 \
                       [${:model} new booleanorcomparison \
                            -operator $op \
                            -leftExpr $l \
                            -rightExpr $r]]
      } else {
        throw {BCEL WRONG OPNDS '$op'} "Invalid number of operands for binary operator '$op'."
      }
    }
    # DYNAMIC RECEPTION
    :method handleUnknown {_ v args} {

      if {[info exists :opds] && [llength ${:opds}] >= 3} {
        # We end up here, unknown operator?
        throw {BCEL UNKNOWN OP $v} "Invalid operator '$v'."
      }
      
      if {[string is double $v]} {
        set :opds [linsert ${:opds}[set :opds {}] 0 [${:model} new number -value $v]]
      } else {
        set :opds [linsert ${:opds}[set :opds {}] 0 [${:model} new variableref -variableName $v]]
      }
    }
    
    :public method from {expr} {
      if {[lindex $expr 0] eq "#"} {
        set expr [lassign $expr _ cmd]
        : {*}$cmd
      }
      set :opds [list]
      foreach element [lreverse $expr] {
        :build $element
      }
      set r [lindex ${:opds} 0]
      unset :opds
      return $r
    }
  }

  namespace export Expressions ExprBuilder EvaluableExpr
  
} {

  namespace import ::djdsl::lm::*
  
  #
  # == DSL extension (ex.: DOT-like graph definitions)
  #
  # === An initial internal DSL syntax for basic graphs (w/o weight)
  #
  
  set gb [BaseGraphBuilder new]
  $gb eval {
    :build graph {
      :build // node definitions
      :build "1st Edition";
      :build "2nd Edition";
      :build "3rd Edition";
      :build // edge definitions
      :build "1st Edition" -- "2nd Edition";
      :build "2nd Edition" -- "3rd Edition";
    }
  }
    
  ? {llength [[$gb output get] edges get]} 2

  set gb2 [BaseGraphBuilder new]
  $gb2 run {
    #// dot1 //
    graph {
      // node definitions
      "1st Edition";
      "2nd Edition";
      "3rd Edition";
      // edge definitions
      "1st Edition" -- "2nd Edition";
      "2nd Edition" -- "3rd Edition";
    }
    #// end //
  }

  ? {llength [[$gb2 output get] edges get]} 2

  #
  # === An extended internal DSL syntax, including +weight+ options for edges.
  #

  set wgb [WeightedGraphBuilder new -successor [BaseGraphBuilder new]]
  $wgb eval {
    :build graph {
      :build // node definitions
      :build "1st Edition";
      :build "2nd Edition";
      :build "3rd Edition";
      :build // edge definitions
      :build "1st Edition" -- "2nd Edition" [:build weight = 5];
      :build "2nd Edition" -- "3rd Edition" [:build weight = 10];
    }
  }

  ? {llength [[$wgb output get] edges get]} 2
  
  ? {[[lindex [[$wgb output get] edges get] 0] cget -weight] cget -value} 10
  ? {[[lindex [[$wgb output get] edges get] 1] cget -weight] cget -value} 5

  set wgb2 [WeightedGraphBuilder new -successor [BaseGraphBuilder new]]
  $wgb2 run {
    #// dot2 //
    graph {
      // node definitions
      "1st Edition";
      "2nd Edition";
      "3rd Edition";
      // edge definitions
      "1st Edition" -- "2nd Edition" [weight = 5];
      "2nd Edition" -- "3rd Edition" [weight = 10];
    }
    #// end //
  }
    
  ? {llength [[$wgb2 output get] edges get]} 2
  
  ? {[[lindex [[$wgb2 output get] edges get] 0] cget -weight] cget -value} 10
  ? {[[lindex [[$wgb2 output get] edges get] 1] cget -weight] cget -value} 5

  nx::Class create Censor -superclasses GraphBuilder {
    :public method output {args} {
      ${:successor} output {*}$args
    }
    set mh [:public method "build weight" {args} {}]
    set slot [namespace qualifiers $mh]
    $slot object mixins add [::nx::Class new {
      :public method "unknown" {callInfo args} {
        set obj [lindex $callInfo 0]
        set path [lrange $callInfo 1 end]
        tailcall $obj handleUnknown {*}$path {*}$args
      }
    }]
  }

  set wgb3 [Censor new -successor [WeightedGraphBuilder new -successor [BaseGraphBuilder new]]]
  $wgb3 run {
    graph {
      // node definitions
      "1st Edition";
      "2nd Edition";
      "3rd Edition";
      // edge definitions
      "1st Edition" -- "2nd Edition" [weight = 5];
      "2nd Edition" -- "3rd Edition" [weight = 10];
    }
  }
    
  ? {llength [[$wgb3 output get] edges get]} 2
  
  ? {[lindex [[$wgb3 output get] edges get] 0] eval {info exists :weight}} 0
  ? {[lindex [[$wgb3 output get] edges get] 1] eval {info exists :weight}} 0

  #
  # == DSL unification (ex.: Miss Grant's Controller plus guarded transitions)
  #
  
  set StateMachine [Behaviours info children -type LanguageModel]
  ? {llength $StateMachine} 1
  ? {llength [$StateMachine info children -type Classifier]} 5
  
  ? {lmap cl [lsort [$StateMachine info children -type Classifier]] {$cl info name}} \
      "AbstractEvent Command Event State Transition"

  StateMachineBuilder create ::smb -onEnter "idle"
  ::smb run {
        # commands {
        # "drawerOpened" "D2OP"
        # "lightOn" "L1ON"
        # }
        #// smdl1 //
        when "doorClosed" goto "active" {
          when "lightOn" goto "waitingForDrawer" {
            when "drawerOpened" goto "unlockedPanel" {
              when "panelClosed" goto "idle"
            }
          }
          when "drawerOpened" goto "waitingForLight" {
            when "lightOn" goto "unlockedPanel"
          }
        }        
        #// end //
      }

  ? {llength [[::smb sm get] info children -type ${StateMachine}::Event]} 6
  ? {llength [[::smb sm get] info children -type ${StateMachine}::Transition]} 6
  ? {llength [[::smb sm get] info children -type ${StateMachine}::State]} 5

  set exprBuilder [ExprBuilder new]
  ? {[$exprBuilder from {= counter 3}] info class} \
      ::djdsl::chapter7::Expressions::Model::BooleanOrComparison

  # package req nx::serializer
  # puts [[$exprBuilder from {= counter 3}] serialize]

  set exprBuilder [ExprBuilder new -model [EvaluableExpr new model]]
  ? {[$exprBuilder from {= counter 3}] info class} \
      ::djdsl::chapter7::EvaluableExpr::Model::BooleanOrComparison


  ? {[$exprBuilder from {= counter 3}] evaluate {counter 4}} 0
  ? {[$exprBuilder from {= counter 3}] evaluate {counter 3}} 1

  # (counter = 3) and (counter > -1)


  if {0} {
    #// bcel1 //
    and > counter -1 = counter 3
    #// end //
    #// bcel2 //
    and > counter -1 = counter 3
    #// end //
    #// bcel3 //
    or > counter -1 <> counter -1
    #// end //
    #// bcel4 //
    or > counter -1 = counter -1
    #// end //
    #// bcel5 //
    # {object forward >= %self %method}
    or >= counter -1 = counter -1
    #// end //
  }

  
  
  ? {[$exprBuilder from {
    and > counter -1 = counter 3
  }] evaluate {counter 3}} 1
  
  ? {[$exprBuilder from {
    and > counter -1 = counter 3
  }] evaluate {counter -1}} 0
  
  
  ? {[$exprBuilder from {
    or > counter -1 <> counter -1
  }] evaluate {counter -1}} 0
  ? {[$exprBuilder from {
    or > counter -1 = counter -1
  }] evaluate {counter -1}} 1

  ? {[$exprBuilder from {
    or >= counter -1 = counter -1
  }] evaluate {counter -1}} "Invalid operator '>='."

  
  ? {[$exprBuilder from {
    # {object forward "build >=" %self operator %method}
    or > counter -1 = counter -1
  }] evaluate {counter -1}} "1"

  ? {[$exprBuilder from {
    # {object forward "build >=" %self operator %method}
    or >= counter -1 = counter -1
  }] evaluate {counter -1}} 1

  ? {[$exprBuilder from {
    # {object forward "build >=" %self operator %method}
    or >= counter -1 = counter -1
  }] evaluate {counter -2}} 0

  Asset create GuardedBehaviours {
    Collaboration create StateMachine {
      Role create Transition {
        :property -accessor public guard:object,type=EvaluableExpr::Model::Expression
      }
    }
  }

  Composition create GuardableStateMachine \
      -binds {Behaviours Expressions} \
      -base $StateMachine \
      -features [GuardedBehaviours::StateMachine]

  nx::Class create GuardableStateMachineBuilder -superclasses StateMachineBuilder {
    :property exprBuilder:required
    # :public method "when goto" {targetStateName args} {
    #   if {[llength $args] <= 1} {
    #     next
    #   } else {
    #     next [list $targetStateName]
    #     return $args
    #   }
    # }
    :public method "when if" {exprBody args} {
      puts exprBody=$exprBody
      set exprObj [${:exprBuilder} from $exprBody]
      puts exprObj=[$exprObj info class]
      # TODO: transition get exprObj set
      # TODO: reorganize in a way that transition can be build up,
      # irrespective of the syntax flow (-> TransitionBuilder!)
      return $args
    }
  }
  
  GuardableStateMachineBuilder create ::smb2 \
      -exprBuilder $exprBuilder -onEnter "idle"
  
  ::smb2 run {
    # commands {
    # "drawerOpened" "D2OP"
    # "lightOn" "L1ON"
    # }
    # ----%<-----
    #// gSmdl1 //
    when "doorClosed" goto "active" {
      when "lightOn" if {= counter 3} goto "waitingForDrawer" {
        when "drawerOpened" goto "unlockedPanel" {
          when "panelClosed" goto "idle"
        }
      }
      when "drawerOpened" goto "waitingForLight" {
        when "lightOn" if {= counter 3} goto "unlockedPanel"
      }
    }
    #// end //
    # ----%<-----
  }

  ? {llength [[::smb2 sm get] info children -type ${StateMachine}::Event]} 6
  ? {llength [[::smb2 sm get] info children -type ${StateMachine}::Transition]} 6
  ? {llength [[::smb2 sm get] info children -type ${StateMachine}::State]} 5


  if {0} {
    computer(processor(cores(2) speed(2500)))
  }
  
}


# Local variables:
#    mode: tcl
#    tcl-indent-level: 2
#    indent-tabs-mode: nil
# End:
