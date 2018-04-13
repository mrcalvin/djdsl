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

  #
  # == Implementation
  #

  package req nx


  #
  # === BUILDER foundations
  #

  nx::Class create Builder {

    :public object method create args {
      throw {DJDSL ABSTRACT} \
          "Instantiate a concrete subclass of [self]"
    }
    
    :property interp:object,type=Interp

    :property -accessor public -incremental predecessors:class,0..* {
      :public object method value=add {obj prop pred:class} {
        set r [next]
        $obj object mixins add $pred
        if {[::nsf::object::property $obj initialized]} {
          $obj setAliases $pred
        }
        return $r
      }

      :public object method value=set {obj prop preds:class,1..*} {
        set r [next]
        $obj object mixins set $preds
        if {[::nsf::object::property $obj initialized]} {
          foreach p $preds {
            $obj setAliases $p
          }
        }
        return $r
      }
    }

    :property -accessor public output:object

    :method init {} {
      if {[info exists :interp]} {
        :setAliases
      }
      :setUnknownHandler
    }

    :public method dispatchUnknown {args} {
      if {[info exists :interp]} {
        set args [${:interp} dispatchUnknown {*}$args]
      }
      puts stderr "      :handleUnknown {*}$args"
      :handleUnknown {*}$args
    }
    
    :protected method setUnknownHandler {} {
      set slot [:info lookup slots \
                    -type ::nx::EnsembleObject \
                    -source application "*<-"]
      if {$slot ne ""} {
        $slot object mixins add [::nx::Class new {
          :public method "unknown" {callInfo args} {
            set obj [lindex $callInfo 0]
            set path [lrange $callInfo 1 end]
            tailcall $obj dispatchUnknown {*}[lrange $path 1 end] {*}$args
          }
        }]
        if {[info exists :interp]} {
          ${:interp} register [list [self] <-] ::unknown
        }
      } else {
        if {[info exists :interp]} {
          ${:interp} register [list [self] dispatchUnknown] ::unknown
        }
      }
    }
    
    :public method setAliases {pred:class,optional} {
      if {![info exists pred]} {
        set subs [:info lookup methods -path "<- *"]
        set subs [lsort -unique [lmap m $subs {lindex $m 1}]]
        if {[llength $subs]} {
          foreach m $subs {
            # lassign $subm _ m
            # TODO: handle subm as list/2+
            ${:interp} register [list [self] <- $m] $m
          }
          # ${:interp} register [list [self] <-] ::unknown
        }
        # :setUnknownHandler
      } else {
        set subs [$pred info methods -path "<- *"]
        set subs [lsort -unique [lmap m $subs {lindex $m 1}]]
        if {[llength $subs]} {
          foreach m $subs {
            # lassign $subm _ m
            # TODO: handle subm as list/2+
            # ${:interp} register [list [self] {*}$subm] $m
            ${:interp} register [list [self] <- $m] $m
          }
        }
      }
    }

    :public method handleUnknown {args} {
      throw {DJDSL DADA UNKNOWN} "Unknown handler for builder (DSL) invocations reached."
    }

    :public method get {script} {
      try {
        set r [${:interp} run $script]
        if {[info exists :output]} {
          return ${:output}
        } else {
          return $r
        }
      } finally {
        :reset
      }
    }

    :protected method reset {} {
      unset -nocomplain :output
    }
  }
  
  #
  # [interp] wrappers
  #

  nx::Class create Interp {

    :public object method create args {
      throw {DJDSL ABSTRACT} \
          "Instantiate a concrete subclass of [self]"
    }

    :property {cmdName:substdefault "[string cat [self]::box]"}
    # :property builder:object,type=Builder
    
    :protected method require {} {
      set interpCmd ${:cmdName}
      if {[info commands $interpCmd] eq ""} {
        interp create $interpCmd -safe
        :prepare $interpCmd
      }
      return $interpCmd
    }

    :protected method dispose {} {
      set interpCmd ${:cmdName}
      if {[info commands $interpCmd] ne ""} {
        rename $interpCmd ""
      }
    }

    # [interp] construction

    :protected method prepare {args} {}
    
    # public API

    :public method dispatchUnknown {args} {
      return $args
    }

    :public method register {tgtPrefix srcPrefix} {
      set interpCmd [:require]
      puts "HERE=$interpCmd $srcPrefix {} {*}$tgtPrefix"
      interp alias $interpCmd $srcPrefix {} {*}$tgtPrefix
      return
    }

    :public method run {script} {
      set interpCmd [:require]
      $interpCmd eval $script
    }

  }

  nx::Class create EmptyInterp -superclasses Interp {
    :protected method prepare {interp} {
      $interp eval {namespace delete ::}
    }
  }

  nx::Class create ExprInterp -superclasses EmptyInterp {

    :protected method prepare {interp} {
      interp hide $interp expr expr
      # $interp eval {namespace delete ::}
      next
    }

    :public method dispatchUnknown {unknown args} {
      return [list [namespace tail $unknown] {*}$args]
    }


    :public method run {script} {
      set interpCmd [:require]
      $interpCmd invokehidden expr $script
      # next [list [list ::expr $script]]
    }

    :public method register {tgtPrefix srcPrefix} {
      if {![string match "::*" $srcPrefix]} {
        set srcPrefix "tcl::mathfunc::$srcPrefix"
      }
      next [list $tgtPrefix $srcPrefix]
    }
  }


  nx::Class create InstanceBuilder {

    nx::Class create [self]::Object {
      :method __object_configureparameter {} {
        return setObjVars:alias,optional,args
      }

      ::nsf::parameter::cache::classinvalidate [current]

      :method setObjVars {args} {
        # TODO: provide mset, or better cset!
        foreach {k v} $args {
          if {[string match "-*" $k]} {
            set k [string trimleft $k "-"]
          }
          set :$k $v
        }
      }

    }
    
    :property [list \
               factoryPrefix \
               "[list [self]::Object new]"]
    
    :property -accessor public -incremental \
        {properties:substdefault "[dict create]"} {
          :public object method value=set {obj prop value} {
            if {[$obj $prop isSet]} {
              set value [dict merge [$obj $prop get] $value]
            }
            next [list $obj $prop $value]
          }
          
          :public object method value=isSet {obj prop p:optional} {
            set isDictSet [$obj eval [list info exists :$prop]]
            if {![info exists p]} {
              return $isDictSet
            } else {
              return [expr {$isDictSet && [dict exists [$obj $prop get] $p]}]
            }
          }
          
          :public object method value=get {obj prop p:optional} {
            set properties [next [list $obj $prop]]
            if {[info exists p]} {
              dict filter $rules key $p
            } else {
              return $properties
            }
          }
          
          :public object method value=add {obj prop p value} {
            $obj eval [list dict set :$prop $p $value]
          }
          
          :public object method value=delete {obj prop p} {
            $obj eval [list dict unset :$prop $p]
          }
        }
    
    :public method get {args} {
      # TODO: make any properties passed into as args override the
      # collected ones -> [dict merge]?
      try {
        {*}${:factoryPrefix} {*}${:properties} {*}$args
      } on error {e opts} {
        throw [list DJDSL DADA [namespace tail [current class]] GET] $e]
      }
    }
  }; # InstanceBuilder

  namespace export Builder Interp EmptyInterp ExprInterp InstanceBuilder
  
} {

  #
  # == Doctests
  #

  package req nx

  package req djdsl::examples::models
  namespace import ::djdsl::examples::models::*

  #
  # Introductory example: A configuration DSL à la Ansible playbooks.
  #

  #// playbook1 //
  nx::Class create PlaybookBuilder -superclasses Builder {

    # entry point
    :public method get {script} {
      set script [string cat "playbook" "(" $script ")"]
      next [list $script]
    }

    # invocation handlers
    :variable tasks [list]
    :public method "<- task" {args} {
      lappend :tasks [${:output} new task {*}[concat {*}$args]]
      return
    }
    
    :public method "<- play" {args} {
      set p [${:output} new play -tasks ${:tasks} {*}[concat {*}$args]]
      set :tasks [list]
      return $p
    }
     
    :public method "<- playbook" {args} {
      puts HERE!!!!!
      ${:output} plays set $args
    }

    # dynamic reception
    :public method handleUnknown {key args} {
      return [list -$key [concat {*}$args]]
    }

    # instantiation incl. interp wrapper and language-model instance
    :create pbb \
        -interp [ExprInterp new] \
        -output [Ansible new playbook]
    
  }; # PlaybookBuilder
  #// end //
  
  set playbook {
#// playbook2 //
play(
  hosts("webservers"),
  remote_user("admin"),
  task(
    name("is webserver running?"),
    service(
      name("http"),
      state("started")))),
play(
  hosts("databases"),
  remote_user("admin"),
  task(
    name("is postgresql at the latest version?"),
    yum(
      name("postgresql"),
      state("latest")
      )))
#// end //
}

  regsub -all -line {^\s*#//.*//\s*$} $playbook {} playbook
  
  set pb [pbb get $playbook]

  ? {$pb info class} ::djdsl::examples::models::Ansible::Playbook
  ? {llength [$pb plays get]} 2
  ? {[[lindex [$pb plays get] 1] tasks get] yum get} \
      "-name postgresql -state latest"
  
  #
  # == DSL extension (ex.: DOT definition of graphs)
  #
  
  #
  # === Abstract syntax
  #
  # The language models are imported from the auxiliary
  # +djdsl::examples::models+ package.

  
  #
  # === Concrete-syntax definition and processing (Builders)
  #

  nx::Class create BaseGraphBuilder -superclasses Builder {
    
    :public method init {} {
      if {![info exists :output]} {
        set :output [Graphs new graph]
      }
      next
    }
    
    #
    # BUILDER internals
    #

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

    #
    # BUILDER construction API
    #
    :public method handleUnknown {arg1 args} {
      set n1 [:createNodeFromScript $arg1]
      if {[llength $args]} {
        set args [lassign $args _ name2]
        if {$_ ni {"->" "--"}} {
          throw {GPL DOT UNSUPPORTED} "Unsupported operator '$_' for edge definition."
        }
        set n2 [:createNodeFromScript $name2]
        puts stderr "createEdgeFromScript {*}$args"
        :createEdgeFromScript $n1 $n2 {*}$args
      }
      return
    }
    
    :public method "<- //" {args} {}

    :public method "<- graph" {script} {
      set caller [current callingobject]
      if {$caller ne "" && [$caller info has type Builder]} {
        $caller eval $script
      } else {
        ${:interp} run $script
      }
    }

    #
    # BUILDER lifecycle API
    #
    
    :public method get args {
      if {![info exists :output]} {
        set :output [Graphs new graph]
      }
      next
    }

    :method reset {} {
      next
      unset -nocomplain :nodes
    }

  }; # BaseGraphBuilder

  BaseGraphBuilder create bgb -interp [EmptyInterp new]
  
  set g [bgb get {
    #// dot1a //
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
  }]

  ? {llength [$g edges get]} 2

  #
  # The above indirected DSL invocations (via the builder's
  # interpreter) correspond to the following direct invocation(s):
  #

  bgb output set [Graphs new graph]
  bgb eval {
    #// dot1b //
    :<- graph {
      :<- // node definitions
      :<- "1st Edition";
      :<- "2nd Edition";
      :<- "3rd Edition";
      :<- // edge definitions
      :<- "1st Edition" -- "2nd Edition";
      :<- "2nd Edition" -- "3rd Edition";
    }
    #// end //
  }

  ? {llength [[bgb output get] edges get]} 2
  bgb output unset
    
  nx::Class create WeightedGraphBuilder -superclasses Builder {

    :variable weightObj

    :public method handleUnknown {args} {
      if {[info exists :weightObj]} {
        lappend args -weight ${:weightObj}
        unset :weightObj
      }
      next $args
    }
    
    :public method "<- weight" {op value} {
      if {$op ne "="} {
        throw {GPL DOT UNSUPPORTED} \
            "Unsupported operator '$op' in attribute."
      }
      # return [list -weight [${:output} new weight -value $value]]
      set :weightObj [${:output} new weight -value $value]
      return
    }
  }

  # WeightedGraphBuilder > BaseGraphBuilder
  BaseGraphBuilder create ::wgb \
      -interp [EmptyInterp new] \
      -predecessors [WeightedGraphBuilder] \
      -output [WeightedGraphs new graph]
  
  set g [::wgb get {
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
  }]
    
  ? {llength [$g edges get]} 2
  
  ? {[[lindex [$g edges get] 0] cget -weight] cget -value} 10
  ? {[[lindex [$g edges get] 1] cget -weight] cget -value} 5

  #
  # Syntax restriction can be implemented by chaining another builder
  # (+Censor+) that consumes method calls resulting from DSL
  # invocations silently, or by throwing an exception.
  #

  nx::Class create Censor -superclasses Builder {
    :public method "<- weight" {args} {}; # NOOP
  }

  # Censor > WeightedGraphBuilder > BaseGraphBuilder
  BaseGraphBuilder create ::wgb \
      -interp [EmptyInterp new] \
      -predecessors [list [Censor] [WeightedGraphBuilder]] \
      -output [WeightedGraphs new graph]
  
  set g [::wgb get {
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
  }]
  
  ? {llength [$g edges get]} 2
  ? {[lindex [$g edges get] 0] weight isSet} 0
  ? {[lindex [$g edges get] 1] weight isSet} 0
  
  
  #
  # == DSL unification (ex.: Miss Grant's Controller plus guarded transitions)
  #
  # First, we provide a [pattern]#BUILDER# for the
  # +Behaviours::StateMachine+ language model.
  #
  
  nx::Class create TransitionBuilder -superclasses InstanceBuilder {

    :property sm:object,type=[Behaviours]::StateMachine
    :property -accessor public event:object

    :method init {} {
      set :factoryPrefix [list ${:sm} new transition]
    }
  }

  nx::Class create SMDBuilder -superclasses Builder {
    
    :property -accessor public sm:object,type=[Behaviours]::StateMachine
    :property -accessor public onEnter:alnum,required

    :property -accessor public currentState:object,type=State

    :variable currentTransitionBuilder [list]

    :public method init args {
      if {![info exists :sm]} {
        set :sm [Behaviours new statemachine]
      }
      set :currentState [${:sm} start set [${:sm} new state -name ${:onEnter}]]
      dict set :states ${:onEnter} ${:currentState}
      
      :object mixins add [current class]::when
      next
    }

    # :public method run {script} {
    #   if {[info commands [self]::runner] eq ""} {
    #     interp create [self]::runner -safe
    #     [self]::runner eval {namespace delete ::}
    #     [self]::runner alias when [self] when
    #   }
    #   [self]::runner eval $script
    #   # interp delete [self]::runner
    # }

    nx::Class create [self]::when {
      :public method "<- when" args {
        # default to 'when and', on entering the ensemble.
        set args [list "and" {*}$args]
        set tb [[namespace qualifiers [[current class] info parent]]::TransitionBuilder new -sm ${:sm}]
        set :currentTransitionBuilder [linsert ${:currentTransitionBuilder} 0 $tb]
        while {[llength $args]} {
          # TODO [current nextmethod],[next] does not work with mixins
          # for ensemble methods with 3+ submethod levels.
          # puts args=[current nextmethod]=$args
          set args [next $args]
        }
        set :currentTransitionBuilder [lassign ${:currentTransitionBuilder} tb]
        set transition [$tb get]
        [$transition cget -source] transitions add [$tb event get] $transition
      }
    }

    :public method "<- when and" {eventName args} {
      set tb [lindex ${:currentTransitionBuilder} 0]
      $tb event set [${:sm} new event -name $eventName];
      # set :currentEvent [${:sm} new event -name $eventName]; # -code $eventCode
      # puts ARGS=$args
      return $args
    }

    :public method "<- when goto" {targetStateName script:optional} {
      if {[info exists :currentState]} {
        if {![dict exists ${:states} $targetStateName]} {
          set tgt [${:sm} new state -name $targetStateName]
          dict set :states $targetStateName $tgt
        } else {
          set tgt [dict get ${:states} $targetStateName]
        }
        # set tgt [${:sm} new state -name $targetStateName]
        set tb [lindex ${:currentTransitionBuilder} 0]
        $tb properties set [list -source ${:currentState} -target $tgt]
        # ${:currentState} transitions add ${:currentEvent} $transition
        # unset :currentEvent
      }
      if {[info exists script]} {
        set oldState ${:currentState}
        set :currentState $tgt
        ${:interp} run $script
        set :currentState $oldState
      }
      return
    }

    
  }

  
  SMDBuilder create ::smb -interp [EmptyInterp new] -onEnter "idle"
  ::smb get {
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

  ? {llength [[::smb sm get] info children -type [Behaviours]::StateMachine::Event]} 6
  ? {llength [[::smb sm get] info children -type [Behaviours]::StateMachine::Transition]} 6
  ? {llength [[::smb sm get] info children -type [Behaviours]::StateMachine::State]} 5

  nx::Class create BCEBuilder -superclasses Builder {
    :property model
    :public method init {} {
      if {![info exists :model]} {
        set :model [Expressions new model]
      }
      next
    }
    
    :forward "<- =" %self operator ==
    :forward "<- <>" %self operator !=
    :forward "<- and" %self operator &
    :forward "<- or" %self operator |
    :forward "<- >" %self operator >
    :forward "<- <" %self operator <
    :forward "<- or" %self operator |
    
    :method operator {op} {
      if {[llength ${:opds}] >= 2} {
        set :opds [lassign ${:opds} l r]
        set :opds [linsert ${:opds}[set :opds {}] 0 \
                       [${:model} new booleanorcomparison \
                            -operator $op \
                            -leftExpr $l \
                            -rightExpr $r]]
      } else {
        throw {BCEL WRONG OPNDS '$op'} \
            "Invalid number of operands for binary operator '$op'."
      }
    }
    # DYNAMIC RECEPTION
    :method handleUnknown {v args} {

      if {[info exists :opds] && [llength ${:opds}] >= 3} {
        # We end up here, unknown operator?
        throw {BCEL UNKNOWN OP $v} "Invalid operator '$v'."
      }
      
      if {[string is double $v]} {
        set :opds [linsert ${:opds} 0 \
                       [${:model} new number -value $v]]
      } else {
        set :opds [linsert ${:opds} 0 \
                       [${:model} new variableref -variableName $v]]
      }
    }
    
    :public method get {expr} {
      if {[lindex $expr 0] eq "#"} {
        set expr [lassign $expr _ cmd]
        : {*}$cmd
      }
      set :opds [list]
      foreach element [lreverse $expr] {
        :<- $element
      }
      set r [lindex ${:opds} 0]
      unset :opds
      return $r
    }
  }
  
  set exprBuilder [BCEBuilder new]
  ? {[$exprBuilder get {= counter 3}] info class} \
      [Expressions]::Model::BooleanOrComparison

  # package req nx::serializer
  # puts [[$exprBuilder from {= counter 3}] serialize]

  set exprBuilder [BCEBuilder new -model [EvaluableExpr new model]]
  ? {[$exprBuilder get {= counter 3}] info class} \
      [EvaluableExpr]::Model::BooleanOrComparison


  ? {[$exprBuilder get {= counter 3}] evaluate {counter 4}} 0
  ? {[$exprBuilder get {= counter 3}] evaluate {counter 3}} 1

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

  
  
  ? {[$exprBuilder get {
    and > counter -1 = counter 3
  }] evaluate {counter 3}} 1
  
  ? {[$exprBuilder get {
    and > counter -1 = counter 3
  }] evaluate {counter -1}} 0
  
  
  ? {[$exprBuilder get {
    or > counter -1 <> counter -1
  }] evaluate {counter -1}} 0
  ? {[$exprBuilder get {
    or > counter -1 = counter -1
  }] evaluate {counter -1}} 1

  ? {[$exprBuilder get {
    or >= counter -1 = counter -1
  }] evaluate {counter -1}} "Invalid operator '>='."

  
  ? {[$exprBuilder get {
    # {object forward "<- >=" %self operator %method}
    or > counter -1 = counter -1
  }] evaluate {counter -1}} "1"

  ? {[$exprBuilder get {
    # {object forward "<- >=" %self operator %method}
    or >= counter -1 = counter -1
  }] evaluate {counter -1}} 1

  ? {[$exprBuilder get {
    # {object forward "<- >=" %self operator %method}
    or >= counter -1 = counter -1
  }] evaluate {counter -2}} 0

  nx::Class create GuardableSMDBuilder -superclasses SMDBuilder {
    :property -accessor public exprBuilder:object,type=BCEBuilder

    #// gSmdl2 //
    :public method "<- when if" {ifBody args} {
      # 1) Get (build) if-expression.
      set exprObj [${:exprBuilder} get $ifBody]

      # 2) Populate the guard reference of the transition under
      # construction:
      set tb [lindex ${:currentTransitionBuilder} 0]
      $tb properties add -guard $exprObj

      # 3) Return any unprocessed arguments to the method chain.
      return $args
    }
    #// end //
  }
  
  # StateMachineBuilder create ::smb2 -interp [EmptyInterp new] \
      #    -predecessors [GuardableStateMachineBuilder] \
      #    -onEnter "idle"; # -exprBuilder $exprBuilder
  # ::smb2 eval [list set :exprBuilder $exprBuilder]
  # puts [smb2 info precedence]
  
  
  GuardableSMDBuilder create ::smb2 \
      -sm [GuardableStateMachine new statemachine] \
      -interp [EmptyInterp new] \
      -exprBuilder $exprBuilder -onEnter "idle"
  
  ::smb2 get {
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
  
  ? {llength [[::smb2 sm get] info children -type [Behaviours]::StateMachine::Event]} 6

  set transitions [[::smb2 sm get] info children -type [GuardedBehaviours]::StateMachine::Transition]
  
  ? {llength $transitions} 6
  ? {llength [lmap t $transitions {if {![$t guard isSet]} continue}]} 2
  ? {llength [[::smb2 sm get] info children -type [Behaviours]::StateMachine::State]} 5

    #
  # == DSL extension composition
  #
  # === Incremental extension composition: (Graphs::Graph <| weighted) <| coloured
  #

  nx::Class create ColouredGraphBuilder -superclasses Builder {

    :variable colourObj

    #// extcomp2 //
    :public method handleUnknown {args} {
      if {[info exists :colourObj]} {
        lappend args -colour ${:colourObj}
        unset :colourObj
      }
      next $args
    }
    
    :public method "<- colour" {op value} {
      if {$op ne "="} {
        throw {GPL DOT UNSUPPORTED} \
            "Unsupported operator '$op' in attribute."
      }
      set :colourObj [${:output} new colour -value $value]
      return
    }
    #// end //
  }

  # WeightedGraphBuilder > BaseGraphBuilder
  BaseGraphBuilder create ::wcgb \
      -interp [EmptyInterp new] \
      -predecessors [list [WeightedGraphBuilder] [ColouredGraphBuilder]] \
      -output [MultiFeatGraph new graph]

    set g [::wcgb get {
    #// extcomp1 //
    graph {
      // node definitions
      "1st Edition";
      "2nd Edition";
      "3rd Edition";
      // edge definitions
      "1st Edition" -- "2nd Edition" [weight = 5; colour = "#eee"];
      "2nd Edition" -- "3rd Edition" [colour = "#00f"];
      "1st Edition" -- "3rd Edition";
    }
    #// end //
  }]
    
  ? {llength [$g edges get]} 3
  
  ? {[lindex [$g edges get] 0] weight isSet} 0
  ? {[lindex [$g edges get] 0] colour isSet} 0
  ? {[[lindex [$g edges get] 1] cget -colour] cget -value} "#00f"
  ? {[lindex [$g edges get] 1] weight isSet} 0
  ? {[[lindex [$g edges get] 2] cget -colour] cget -value} "#eee"
  ? {[[lindex [$g edges get] 2] cget -weight] cget -value} 5

  #
  # === Extension unification: ( weighted <| coloured) <| Graphs::Graph
  #

  #// extunif1 //
  # 1) extension unification
  WeightedGraphBuilder mixins add ColouredGraphBuilder

  # 2) composition with base
  BaseGraphBuilder create ::wcgb2 \
      -interp [EmptyInterp new] \
      -predecessors [WeightedGraphBuilder] \
      -output [MultiFeatGraph new graph]
  #// end //
  
  set g [::wcgb2 get {
    #// dot3 //
    graph {
      // node definitions
      "1st Edition";
      "2nd Edition";
      "3rd Edition";
      // edge definitions
      "1st Edition" -- "2nd Edition" [weight = 5; colour = "#eee"];
      "2nd Edition" -- "3rd Edition" [colour = "#00f"];
      "1st Edition" -- "3rd Edition";
    }
    #// end //
  }]
    
  ? {llength [$g edges get]} 3
  
  ? {[lindex [$g edges get] 0] weight isSet} 0
  ? {[lindex [$g edges get] 0] colour isSet} 0
  ? {[[lindex [$g edges get] 1] cget -colour] cget -value} "#00f"
  ? {[lindex [$g edges get] 1] weight isSet} 0
  ? {[[lindex [$g edges get] 2] cget -colour] cget -value} "#eee"
  ? {[[lindex [$g edges get] 2] cget -weight] cget -value} 5

  WeightedGraphBuilder mixins delete ColouredGraphBuilder

  #// extunif2 //
  # 1) derivative extension
  nx::Class create AttributedGraphBuilder -superclasses Builder
  AttributedGraphBuilder mixins set [list WeightedGraphBuilder ColouredGraphBuilder]

  # 2) derivative composition with base
  BaseGraphBuilder create ::wcgb3 \
      -interp [EmptyInterp new] \
      -predecessors [AttributedGraphBuilder] \
      -output [MultiFeatGraph new graph]
  #// end //

  
  set g [::wcgb3 get {
    #// dot3 //
    graph {
      // node definitions
      "1st Edition";
      "2nd Edition";
      "3rd Edition";
      // edge definitions
      "1st Edition" -- "2nd Edition" [weight = 5; colour = "#eee"];
      "2nd Edition" -- "3rd Edition" [colour = "#00f"];
      "1st Edition" -- "3rd Edition";
    }
    #// end //
  }]
    
  ? {llength [$g edges get]} 3
  
  ? {[lindex [$g edges get] 0] weight isSet} 0
  ? {[lindex [$g edges get] 0] colour isSet} 0
  ? {[[lindex [$g edges get] 1] cget -colour] cget -value} "#00f"
  ? {[lindex [$g edges get] 1] weight isSet} 0
  ? {[[lindex [$g edges get] 2] cget -colour] cget -value} "#eee"
  ? {[[lindex [$g edges get] 2] cget -weight] cget -value} 5
  
  puts [::wcgb3 info precedence]
  
  if {0} {
    ComputerBuilder create cb
    # ? {cb get {
    #   wide(wide(1))
    # }} 1
    
    ? {cb get {
      computer(
               proc(2),
               disks(4)
               )
    }} 24
    
    ? {cb get {
      computer(
               disks(4),
               proc(2)      
               )
    }} 42
  }
}

# Local variables:
#    mode: tcl
#    tcl-indent-level: 2
#    indent-tabs-mode: nil
# End:
#
