# An NX/Tcl companion to (internal) "Domain-specific Languages" by Martin Fowler
# ==============================================================================
# Stefan Sobernig <stefan.sobernig@wu.ac.at>
# v2.1.0, January 2017:
# :Author Initials: SS
# :toc:
# :toclevels: 3
# :icons:
# :numbered:
# :website: https://next-scripting.org/
# :data-uri:
#
# .Abstract
# *****************************************************************************
# ...
# *****************************************************************************
#

package req nx
package req nx::test

# .TODO list
# - How to implement sentential structures like: when_from_and_airline etc. (see Section 41.3)
# - Add literature references ...


#
# How to read this tutorial
# -------------------------
# ...

#
# Prequisites
# -----------
# ...

# 
# === Internal domain-specific languages (DSLs)
# ...


#
# === Semantic Model (159)
#
# The model to populated or created by a DSL, i.e. by evaluating a DSL
# program (script). Also known as [pattern]#DOMAIN MODEL# which
# contains domain classes.

#
# One prominent and well recognized example from the DSL book is the
# "State Machine Model" (see Chapter 1.2; esp. Figure 1.2). I will use
# and refer to it throughout the tutorial. It is built around six
# domain classes: +StateMachine+, +State+, +Transition+,
# +AbstractEvent+, +Event+, and +Command+.
#

nx::Class create StateMachine {
  :property -accessor public start:object,type=::State
}

nx::Class create State {
  :property -accessor public name
  :property -accessor public actions:0..*,object,type=::Command
}

nx::Class create Transition {
  :property -accessor public source:object,type=::State
  :property -accessor public target:object,type=::State
  :property -accessor public trigger:object,type=::Event
}

nx::Class create AbstractEvent {
  :property -accessor public name
  :property -accessor public code
}

nx::Class create Event -superclasses AbstractEvent
nx::Class create Command -superclasses AbstractEvent

#
# In Fowler's example, transitions are added into a state machine via
# a dedicated setter method +addTransition+ owned by +State+. Each
# transition is registered for the scope of a given trigger event (its
# event code). This is realized using some sort of map, with the event
# code as key and the corresponding (triggered) transition as
# value. In NX, it is more natural to teach this behaviour to the
# +transitions+ slot directly, which manages the property itself.
#
# . The internal event/transition store is implemented as a Tcl
#   +dict+.
# . The slot +transitions+ supports an interface to +set+ (a bulk),
#   +add+, +delete+, and +get+ transitions, wrapping around the +dict+
#   interface.
#

State property -accessor public {transitions:substdefault "[dict create]"} {

  :public object method value=set {obj prop arg} {
    if {[string is list $arg] && ([llength $arg] & 1) == 0} {
      foreach {event transition} $arg {
        :value=add $obj $prop $event $transition
      }
    }
  }
  
  :public object method value=add {obj prop event transition} {
    $obj eval [list dict set :transitions [$event code get] $transition]
  }
  :public object method value=delete {obj prop event} {
    $obj eval [list dict unset :transitions [$event code get]]
  }
  :public object method value=get {obj prop event} {
    set transitions [next [list $obj $prop]]
    set key [$event code get]
    if {[dict exists $transitions $key]} {
      return [dict get $transitions $key]
    } else {
      return -code error "There is no transition for a trigger event '$key'."
    }
  }
}

#
# The method +addTransition+ provides a slim facade to +transitions
# add|set|delete|get+ to match more closely the example listing from
# the book.
#

State public method addTransition {event:object,type=::Event targetState:object,type=::State} {
  set transition [Transition new -source [self] -target $targetState]
  :transitions add $event $transition
}

#
# A concrete state machine (Miss Grant's controller for a secret bed
# compartment; see Chapter 1.1) is then programmed by constructing and
# associating a collection of instantiations from this
# [pattern]#SEMANTIC MODEL# (see Chapter 1.3):
#

set doorClosed [Event new -name "doorClosed" -code "D1CL"]
set drawerOpened [Event new -name "drawerOpened" -code "D2OP"]
set lightOn [Event new -name "lightOn" -code "L1ON"]
set doorOpened [Event new -name "doorOpened" -code "D1OP"]
set panelClosed [Event new -name "panelClosed" -code "PNCL"]


set unlockPanelCmd [Command new -name "unlockPanelCmd" -code "PNUL"]
set lockPanelCmd [Command new -name "lockPanelCmd" -code "PNLK"]
set lockDoorCmd [Command new -name "lockDoorCmd" -code "D1LK"]
set unlockDoorCmd [Command new -name "unlockDoorCmd" -code "D1UL"]


set idle [State new -name "idle"]
set activeState [State new -name "activeState"]
set waitingForLightState [State new -name "waitingForLight"]
set waitingForDrawerState [State new -name "waitingForDrawer"]
set unlockedPanelState [State new -name "unlockedPanel"]

set machine [StateMachine new -start $idle]

# Add transitions and actions between/ to states:

$idle addTransition $doorClosed $activeState

$idle actions add $unlockDoorCmd
$idle actions add $lockPanelCmd

$activeState addTransition $drawerOpened $waitingForLightState
$activeState addTransition $lightOn $waitingForDrawerState

$waitingForLightState addTransition $lightOn $unlockedPanelState
$waitingForDrawerState addTransition $drawerOpened $unlockedPanelState

$unlockedPanelState actions add $unlockPanelCmd
$unlockedPanelState actions add $lockDoorCmd
$unlockedPanelState addTransition $panelClosed $idle

# Run some consistency checks:

? {llength [State info instances]} 5
? {llength [Event info instances]} 5
? {llength [Command info instances]} 4
? {llength [StateMachine info instances]} 1
? {[$machine start get] name get} "idle"
? {llength [Transition info instances]} 6
? {llength [Command info instances]} 4
? {[[$idle transitions get $doorClosed] target get] name get} "activeState"

# Smoke-test the +transitions+ interface:

? {[[$idle transitions get $doorClosed] target get] name get} "activeState"
? {[[$idle transitions get $drawerOpened] target get] name get} "There is no transition for a trigger event 'D2OP'."
? {$idle transitions delete $doorClosed} ""
? {[[$idle transitions get $doorClosed] target get] name get} "There is no transition for a trigger event 'D1CL'."
$idle addTransition $doorClosed $activeState

# [NOTE] 
# .NX features 
# ==== 
# . +public+ modifier: ...
# . +substdefault+ conversion constraint: ...
# . +property+ accessors and mutators: ...
# . +/propertySlot/ value=set|value=get|...+: ...
# . +/cls/ info instances+: ...
# ====
#
# === Expression Builder (343)
#
# An [pattern]#EXPRESSION BUILDER# separates the concerns of (a)
# specifying a DSL program/ script using some internal-DSL
# (a.k.a. "fluent") interface and (b) of building up the underlying
# instantiation of the [pattern]#SEMANTIC MODEL# using the standard NX
# class/object interfaces (see above). In essence, this is achieved by
# providing separate builders (+StateMachineBuilder+) for the domain classes
# (+StateMachine+). The builders then mix a number of techniques to define
# the actual DSL interface and to process a script written using that
# interface (e.g., [pattern]#METHOD CHAINING#).
#

#
# In our running example, a builder class [[smb,StateMachineBuilder]]+StateMachineBuilder+
# defines an alternative interface to construct a state machine from a
# start state (+onEnter+) and to add transitions (+when+, +goto+):
#

nx::Class create StateMachineBuilder {
  # context variables
  :property -accessor public {stateMachine:object,type=::StateMachine,substdefault "[StateMachine new]"}
  :property -accessor protected currentState:object,type=::State
  :property -accessor protected currentEvent:object,type=::Event

  # methods implementing the fluent interface
  :public method onEnter {stateName} {
    set :currentState [${:stateMachine} start set [State new -childof ${:stateMachine} -name $stateName]]
    return [self]
  }
  :public method when {eventName eventCode} {
    set :currentEvent [Event new -childof ${:stateMachine} -name $eventName -code $eventCode]
    return [self]
  }
  :public method goto {targetStateName} {
    if {[info exists :currentState] && [info exists :currentEvent]} {
      set tgt [State new -childof ${:stateMachine} -name $targetStateName]
      set transition [Transition new -childof ${:stateMachine} -source ${:currentState} -target $tgt]
      ${:currentState} transitions add ${:currentEvent} $transition
      unset :currentEvent
      set :currentState $tgt
    }
    return [self]
  }
  
  # provide a builder instance
  :create ::builder
}

#
# The above implementation of a +StateMachineBuilder+ allows to
# retrofit the equivalent of the following script excerpt (from above) ...
#
# [source,tcl]
# --------------------------------------------------
# set doorClosed [Event new -name "doorClosed" -code "D1CL"]
# set idle [State new -name "idle"]
# set activeState [State new -name "activeState"]
# set machine [StateMachine new -start $idle]
# $idle addTransition $doorClosed $activeState
# --------------------------------------------------
#
# as a one-liner:

[[::builder onEnter "idle"] when "doorClosed" "D1CL"] goto "activeState"

# Run some checks:

set sm [builder stateMachine get]
? {[$sm start get] name get} "idle"
? {llength [State info instances ${sm}::*]} 2
? {llength [Event info instances ${sm}::*]} 1
? {llength [Transition info instances ${sm}::*]} 1

# Clean up (this will remove all +State+, +Event+, and +Transition+ specific to this +StateMachine+ instance, along with the latter):

$sm destroy

? {llength [State info instances ${sm}::*]} 0
? {llength [Event info instances ${sm}::*]} 0
? {llength [Transition info instances ${sm}::*]} 0
? {llength [StateMachine info instances ${sm}]} 0

# This toy implementation of an [pattern]#EXPRESSION BUILDER# is only
# meant to deliver the key message and is obviously limited in a
# number of ways. Also, it showcases only one possible implementation
# technique available (native [pattern]#METHOD CHAINING# in NX/Tcl),
# which comes with its own limitations.
#
# [NOTE] 
# .NX features 
# ==== 
# . +/cls/ new -childof+: ...
# . +protected+ modifier: ...
# ====

#
# NX implementations of key patterns
# ----------------------------------
# ...

#
# === Method Chaining (373)
#
# [pattern]#METHOD CHAINING# uses a sequence of method calls to
# implement sentencial expressions, with each call being executed on
# an object returned as a result of previous calls, to implement the
# DSL interface. Technically, each method as part of a method chain
# returns the current host object (a.k.a. method cascading) or another
# object to become the host of the subsequent method call (chaining in
# the strictest sense).
#
# In the context of an [pattern]#EXPRESSION BUILDER#, the host objects
# are typically builder objects (+StateMachineBuilder+) responsible
# for constructing structures of related domain objects
# (+StateMachine+, +State+ etc.).
#
# [source,tcl]
# --------------------------------------------------
# StateMachineBuilder create builder
# builder onEnter "idle"
# builder when "doorClosed" "D1CL"
# builder goto "activeState"
#
# # cascading (same host)
#
# [[builder onEnter "idle"] when "doorClosed" "D1CL"] goto "activeState"
# --------------------------------------------------
#
# When calls to different host (builder) objects are involved, then
# this is referred to chaining. For example, +onEnter+ might return a
# +StateBuilder+ object responsible for +when+/ +goto+ sequences:
#
# [source,tcl]
# --------------------------------------------------
# StateMachineBuilder create builder
# set stateBuilder [builder onEnter "idle"]
# $stateBuilder when "doorClosed" "D1CL"
# $stateBuilder goto "activeState"
#
# # chaining (varying hosts)
#
# [[builder onEnter "idle"] when "doorClosed" "D1CL"] goto "activeState"
# --------------------------------------------------
#
# (Although the difference of cascading/chaining remains ideally hidden
# from the DSL client.)
#
# There are two main ingredients to implementing [pattern]#METHOD
# CHAINING#:
#
# . Have the methods forming the DSL interface keep returning host (builder) objects only.
# . Organize the method chains syntactically as (hierarchical)
#   sentences or expressions appealing to the domain expert.
#
# Depending on the number of builder methods needed, rather than
# making each builder-method body by a +return [self]+ or similar (see
# <<smb>>), NX/Tcl allows for defining a filter +cascade+ for the
# scope of builder objects, which intercepts each and every call to
# the builder objects and guards the builder protocol.

nx::Class create Builder {
  :method cascade args {
    set r [next]
    set m [current calledmethod]
    if {[[:info class] info methods -callprotection public $m] eq $m && \
            (![::nsf::is object $r] || ![$r info has type [current class]])} {
      return [self]
    } else {
      return $r
    }
  }
  :filters add cascade
}

nx::Class create StateBuilder -superclasses Builder {
  :property -accessor public {stateMachine:object,type=::StateMachine,substdefault "[StateMachine new]"}
  :public method when {eventName eventCode} {
    set :currentEvent [Event new -childof ${:stateMachine} -name $eventName -code $eventCode]
    return "foo"; # ignored!
  }
  :public method goto {targetStateName} {
    if {[info exists :currentState] && [info exists :currentEvent]} {
      set tgt [State new -childof ${:stateMachine} -name $targetStateName]
      set transition [Transition new -childof ${:stateMachine} -source ${:currentState} -target $tgt]
      ${:currentState} transitions add ${:currentEvent} $transition
      unset :currentEvent
      set :currentState $tgt
    }
    # return [self]; # not needed anymore!
  }
}

[[StateBuilder new] when "doorClosed" "D1CL"] goto "activeState"

#
# Filters such as +cascade+ come with an overhead in terms of
# invocation time, but this penality of indirection must be related to
# the costs of object construction within the builder methods
# themselves. The benefit is a builder protocol independent from
# misplaced, repeated, or erroneously missing [return] commands, which
# is implemented (and extensible) in one spot: the method body of
# +cascade+. The implementation above can be easily extended to cover
# hierarchies of Builder classes (e.g., subclasses of +StateBuilder+).
#

# The native Tcl method-chaining syntax follows from the basic Tcl
# rules of command substitution (as indicated by sequences of nested
# open "[" and close brackets "]") and command separation: newlines
# (unless backslash-escaped) and semi-colons separate commands; so
# they must be avoided (or escaped) in method chaining:
#
# [source,tcl]
# --------------------------------------------------
# # 1:
# [[StateBuilder new] when "doorClosed" "D1CL"] goto "activeState"; # OK
#
# # 2:
# [[StateBuilder new]
# 	when "doorClosed" "D1CL"]
#	goto "activeState"; # NOT OK.
#
# # 3:
# [[StateBuilder new] \
    # 	when "doorClosed" "D1CL"] \
    #	goto "activeState"; # OK.
#
# --------------------------------------------------
#
# Syntax sketch 3 might be acceptable (and should be so for code
# editors aware of Tcl), but it will become hard to track the
# balancedness of brackets across multiple lines of code, leaving
# aside the readability. One might then consider a slim facade using
# Tcl lists to represent chains (cascades) of method calls; see
# [pattern]#LITERAL LIST# below.
#

set dsl {
  ::builder {
    onEnter "idle"
    when {"doorClosed" "D1CL"}
    goto "activeState"
  }
}

? {llength [lindex $dsl 1]} 6

#
# Tcl lists have a number of convenient properties, for instance,
# indicating an expression hierarchy using whitespace (indentation
# levels) does not affect building up and processing this data
# structure (whitespace is simply swallowed). For more details, see
# the excursus on Tcl lists.
#
# Using NX/Tcl's means to [pattern]#OBJECT SCOPING#, one can rewrite
# the +StateMachineBuilder+ and/or +StateBuilder+ to support a DSL
# syntax such as:
#
# [source,tcl]
# --------------------------------------------------
# [StateMachineBuilder new] onEnter "idle" {
#    :when "doorClosed" "D1CL"
#    :goto "activeState" {
#        :when "..." "..."
#        :goto "..."
#    }
# }
# --------------------------------------------------
#
# This is already close to what can be achieved using a more generic Tcl-list facade.
#
# In the context of [pattern]#METHOD CHAINING#, another NX feature is
# handy: +ensemble methods+. Akin to Tcl ensembles, which build on Tcl
# namespaces, NX provides ensembles of methods having common anchors
# as part of the message selectors. Rather than a single Tcl word
# identifying the method to invoked, a sequence of words is mapped to
# a method ensemble. This way, NX provides for a variant of
# [pattern]#METHOD CHAINING# avoiding nesting evaluations to a large
# extent, while maintaining a method-based interface in the
# [pattern]#EXPRESSION BUILDER# implementation:
#
# [source,tcl]
# --------------------------------------------------
# # ...
#    :when "drawerOpened" "" goto "waitingForLightState"
#    :when "lightOn" "" and "someOtherEvent" "" goto "waitingForDrawerState"
# # ...
# --------------------------------------------------
#
# Were these messages matched directly a non-ensemble method named
# +on+, the other details were mere arguments to one method. The
# +method+ implementation of +on+ would have to provide for argument
# checking etc. Ensemble methods allow for the following layout of
# builder methods.
#
# This following variant of a +StateBuilder+ combines builder methods
# organized as an ensemble of methods (+when and+, +when goto+) and a
# mixin +StateBuilder::When+ as a special-purpose of [pattern]#MESSAGE
# REDIRECTOR# in the context of the ensemle.
#

nx::Class create StateBuilder {
  :property -accessor public currentState:object,type=::State

  :public method "when and" {eventName eventCode args} {
    set :currentEvent [Event new -childof [self] -name $eventName -code $eventCode]
    return $args
  }
  :public method "when goto" {targetStateName args} {
    if {[info exists :currentState] && [info exists :currentEvent]} {
      set tgt [State new -childof [self] -name $targetStateName]
      set transition [Transition new -childof [self] -source ${:currentState} -target $tgt]
      ${:currentState} transitions add ${:currentEvent} $transition
      unset :currentEvent
      set :currentState $tgt
    }
    return $args
  }
}

#
# The +when+ ensemble of methods implements the actual builder
# methods. When evaluated, the DSL expressions +:when ...+ will be
# split up and processed by a sequence of calls to these ensemble
# methods (a.k.a. submethods).
#
#
# To turn the eventual argument vector into a sequence of nested
# methods calls, one can benefit from the special interaction of NX
# decorator mixins and ensemble methods: A mixin can act as a
# [pattern]#MESSAGE REDIRECTOR# for an entire ensemble (+when+),
# rather than the individual submethods (+when and+, +when goto+)
# only.
# 

nx::Class create StateBuilder::When {
  :public method when args {
    # default to 'when and', on entering the ensemble.
    set args [concat "and" $args]
    while {[llength $args]} {
      set args [next $args]
    }
  }
}

StateBuilder mixins add StateBuilder::When

#
# The mixin's method +when+ implements this simple protocol between
# [pattern]#MESSAGE REDIRECTOR# and ensemble.
#
# . +next+ is used to invoke the next method in method-resolution
#   order. The NX MRO takes into account submethods.
# . +when goto+, +when and+: each submethod processes a specific
#   number of arguments from the argument vector (passed on its
#   method-parameter spec) and returns the residual, to be further
#   processed into a possible +next+ submethod call (up to the point
#   that the entire argument vector has been consumed).
#

# This skeleton implementation will process the following DSL script as expected:

StateBuilder create ::sb -currentState [State new -name "activeState"]
::sb eval {
  # ----%<-----
  :when "drawerOpened" "D2OP" goto "waitingForLightState"
  :when "lightOn" "L1ON" goto "waitingForDrawerState"
  # ----%<-----
}

? {llength [Event info instances ::sb::*]} 2
? {llength [Transition info instances ::sb::*]} 2
? {llength [State info instances ::sb::*]} 2

#
# This ensemble/mixin idiom has numerous advantages for
# [pattern]#METHOD CHAINING# over a native approach, or a filter-based
# one.
#
# . It avoids nesting command evaluations to formulate DSL sentences/
#   expressions, a (composite) DSL expression is realized as a
#   (nested) Tcl command evaluated at once.
# . It avoids the overhead (and intricacies) of filters, message
#   interception is clearly limited to one ensemble of methods
#   (+when+).
# . The DSL clauses map to submethods and their parameter specs. The
#   conventional programming model of builder methods is preserved.
# . Dispatch into an ensemble and parameter checking by submethods
#   helps implement the DSL construction semantics. For instance, an
#   invalid DSL term will be reported by built-in means in a
#   consumable manner (which can also be refined, certainly).
# . ...

? {::sb eval {:when "lightOn" "L1ON" to "waitingForDrawerState"}} \
    {unable to dispatch sub-method "to" of ::sb when; valid are: when and, when goto}

#
# The latter aspect highlights the link to [pattern]#DYNAMIC RECEPTION# using
# +unknown+ handlers or filters. Messages dispatched via the
# mixin/ensemble dispatch are auto-prefixed with the ensemble method
# paths. This is not only convenient, for the above reasons, but also
# avoids problems of [pattern]#DYNAMIC RECEPTION# when it comes to
# the ambiguity between builder and NX built-in methods ([pattern]#DYNAMIC RECEPTION#).
#
#
# .TODO list
# - properties/variables for method chaining
# - intro example for ensembles
#
# [NOTE] 
# .NX features 
# ==== 
# . filter: ...
# . callstack introspection: +current calledmethod|calledclass+
# . +protected+ modifier: ...
# . +/cls/ info methods+ modifier: ...
# . ensembles
# ====

# === Object Scoping (385)
#
# To avoid use of global state to organize the control and data flow
# in an internal DSL and to avoid cluttering some namespace with
# DSL-specific names, use (builder) objects as scope of executing a
# DSL script. Typically, this is achieved by subclassing a
# base-builder class, with the subclass (and its instances) becoming
# the isolating object scope. This is also suggested by Fowler to
# selectively the DSL using custom builders.This option is also available in NX/Tcl,
# but is also provides a form of per-object command-evaluation using
# +/obj/ eval /script/+.
#
# Object evaluation cares for a given NX/Tcl script, stored as Tcl
# value, being evaluated a given selected (domain or builder)
# object. NX's +eval+ also provides dedicated call frame to maintain
# script-local variables, which will not clutter any other scope
# (global or the caller's).
#

set script {
  set x 10; # script-local variable
  set :y 20; # object variable
  set z [:foo]; # method call
  return $x-$z
}

#
# This +script+ can now be evaluated (reused) for the scope of
# different objects using +eval+: +o1+ and +o2+.
#

nx::Class create C {
  :method foo {} {return ${:y}-[self]}
  :create ::o1
  :create ::o2
}

? {o1 eval $script} "10-20-::o1"
? {o2 eval $script} "10-20-::o2"

#
# [pattern]#OBJECT SCOPING# is most useful when combined with other
# patterns to implement [pattern]#EXPRESSION BUILDER#. To pick up
# another example, consider the following [pattern]#SEMANTIC MODEL#
# from Fowler's book:
#

nx::Class create Computer {
  :property -accessor public processor:object,type=::Processor
  :property -accessor public disk:1..*,object,type=::Disk
}

nx::Class create Processor {
  :property -accessor public cores:integer
  :property -accessor public speed:double
}
nx::Class create Disk {
  :property -accessor public size:integer
}

#
# A corresponding +ComputerBuilder+ using a mix of [pattern]#OBJECT SCOPING# and [pattern]#METHOD CHAINING#.
#
nx::Class create ComputerBuilder {
  :property -accessor public result:object,type=::Computer
  :public method computer {script} {
    :result set [Computer new]
    :eval $script
    return [self]
  }
  :public method processor {script} {
    set proc [Processor new -childof ${:result}]
    $proc eval $script
    ${:result} processor set $proc
    return [self]
  }
  :public method disk {script} {
    set disk [Disk new -childof ${:result}]
    $disk eval $script
    ${:result} disk add $disk
    return [self]
  }
}

#
# Note that [pattern]#OBJECT SCOPING# can be applied to both builder
# or domain objects, as needed, by calling +eval+ on the respective
# object and passing along the script: +$proc eval $script+ vs.
# +:eval $script+.
#
# [pattern]#OBJECT SCOPING# using NX/Tcl's +eval+ also allows for
# on-demand (lazy) evaluation, which gives the opportunity to set up
# some context first ([pattern]#CONTEXT VARIABLE#) and to provide some
# cleanup hooks in the builder methods. Another plus is that the
# domain-object methods do *not* have to be public.
#
# The above builder implementation allows for specifying a computer
# setup as follows:
#

[ComputerBuilder new] computer {
  :processor {
    :cores set 2
    :speed set 2.2
  }
  :disk {
    :size set 75
  }
  :disk {
    :size set 160
  }
}

# .TODO list
# - Strip ":"
# - avoid property interface ... (separate builders? mixins to domain objects?), no-arg setters
# - filter to eval trailing script by default (or not clutter the builder-method interfaces).

#
# === Annotation (445)
#
# An annotation is data about program elements (i.e., meta-data on
# classes, objects, methods, and parameters) which is stored using
# first-class data structures and can so be processed during
# runtime. An annotation (a.k.a. as "attribute" as in
# attribute-oriented programming) marks program elements with
# application- or domain-specific data. Moreover, meta-data and
# program elements should maintain a navigatable link. Key to
# [pattern]#ANNOTATION# is that defining and processing meta-data is
# clearly separated (effectuated at different binding times). The
# definition is the tangible (frontend, syntax) part of an internal
# DSL (building up its [pattern]#SEMANTIC MODEL#), the code for
# processing annotations realizes the DSL behavior (walking the
# [pattern]#SEMANTIC MODEL#).
#
# Fowler's chapter on [pattern]#ANNOTATION# does not continue on the
# State Machine example, it rather introduces a fresh one (field
# validation; see further down).
#
# For this tutorial, I prefer to extend the State Machine example
# because it adds to a more coherent picture. Figure <<fig-sm>> gives
# a bird's eye overview of the State Machine example. Also, the
# program elements to be annotated are solely methods (although
# objects, classes, and properties might also be important.)
#
# [[fig-sm]]
# image::sm.svg[Overview of the State Machine Example]
#
# Most of Fowler's story telling centers on the [pattern]#EXPRESSION
# BUILDER# aspect of the example and, therefore, ways of implementing
# builders for +StateMachine+. In the overall picture, +StateMachine+
# manages the internal state of a +Controller+, which interacts with
# the actual compartment +Device+ via a +CommandChannel+ (see Section ...
# of the book for the details). On the one hand, +Controller+ receives
# event codes as triggers to state transitions in the
# +StateMachine+. On the other hand, on entering a new state, the
# controller emits command codes to the +Device+ (see
# Fig. <<fig-sm>>); based on +Commands+ registered as a +State+'s
# actions. So far, +CommandBuilder+ strategies allow for specifying
# commands in a number of ways, e.g.:
#
# [source,tcl]
# --------------------------------------------------
# # ...
#  :command unlockPanel "PNUL"
#  :command lockPanel "PNLK"
#  :command lockDoor "D1LK"
#  :command unlockDoor "D1UL"
# # ...
# --------------------------------------------------
# 
# To equip the +Device+ (e.g., a device simulator) with behavior
# corresponding to the command codes ("PNUL", "PNLK"), it could define
# methods named after the codes:
#

nx::Class create Device {
  :public method PNUL {} {;}
  :public method PNLK {} {;}
  # ...
}

# In a basic scenario, the +CommandChannel+ would provide an instance
# of +Device+ and turn command codes sent to it by the +Controller+
# into method calls onto the +Device+ instance.
#
# As for +Commands+, their definition ends up separated between a
# +CommandBuilder+ and a +Device+, with establishing links by
# separately provided identifiers for command codes matching method
# names. [pattern]#ANNOTATION# can help consolidate the command
# specification and can help avoid redundancy of specification
# details.
#
# ==== Defining annotations
#
# Neither Tcl nor NX have a special-purpose, built-in annotation or
# attribute syntax. Their means of meta-programming (for NX a
# meta-object protocol incl. classes as objects and metaclasses),
# however, offer different ways of realizing annotation programming.
#
# The objective is to specify commands (in the sense of a
# +CommandBuilder+) along with the commands behavior located in the
# +Device+ class:
#
# [source,tcl]
# --------------------------------------------------
#    :@ command "unlockPanelCmd" {
#	:public method PNUL {} {
#	    # ...
#	}
#    }
#    :@ command "lockDoorCmd" {
#	:public method D1LK {} {
#	    # ...
#	}
#    }
# --------------------------------------------------
#
# Fowler exemplifies the definition of annotations using e.g. Ruby
# class methods, i.e., per-object methods defined for class objects,
# used in the class body. In NX, a +metaclass+ can be used to
# implement a +CommandBuilder+ (as part of an [pattern]#EXPRESSION
# BUILDER#) and provide annotation methods (+@+) to the classes (+Device+)
# created from it.
#

StateMachineBuilder property -accessor public cmds:0..*,object,type=::Command

nx::Class public method "info method name" {mh} {
  lassign [:info method definition $mh] _ _ _ name
  return $name
}

#
# A +metaclass+ is a class in its own right and has nx::Class as a
# superclass. This way, metaclass instances become classes
# themselves. Metaclass methods (+@ command+) are available to the
# instantiated classes (+Device+).
#

nx::Class create CommandBuilder -superclasses nx::Class {
  :property parent:object,type=::StateMachineBuilder
  :public method "@ command" {name block} {
    try {
      :eval $block
    } on ok mh {
      set code [:info method name $mh]
      Command new -childof ${:parent} -name $name -code $code
    }
  }
}

#
# The annotation method (+@ command+) is realized using a variant of
# [pattern]#OBJECT SCOPING#. The +block+ contains a script, which
# promises to return a handle to a newly created method, i.e. a method
# implementing the behavior for a given command code (+PNUL+,
# +D1LK+). The annotation method then extracts relevant details of the
# newly created program element (method) such as the name using NX's
# built-in introspection (+info+) and populates the [pattern]#SEMANTIC
# MODEL# by creating +Command+ instances. Note that, this way,
# +Command+ and +Device+ are correlated by the chosen method names or
# command codes.
#

set smb [StateMachineBuilder new]

CommandBuilder create Device -parent $smb {    
  :@ command "unlockPanelCmd" {
    :public method PNUL {} {
      # ...
      lappend :eventHistory [current method]
      # ...
    }
  }
  :@ command "lockDoorCmd" {
    :public method D1LK {} {
      # ...
      lappend :eventHistory [current method]
      # ...
    }
  }
}

set cmds [lsort [Command info instances ${smb}::*]]
? {llength $cmds} 2
? {[lindex $cmds 0] code get} "PNUL"
? {[lindex $cmds 1] code get} "D1LK"

#
# ==== Processing annotations
#
# In our running example, annotations are integral part of the
# [pattern]#EXPRESSION BUILDER# implementation. In this sense, the
# annotations are processed when the corresponding +Command+ instances
# (as elements of the [pattern]#SEMANTIC MODEL#) are accessed and used
# to dispatch command codes to the +Device+.
#
#
# The +CommunicationChannel+ is the mediator between +Controller+
# (+StateMachine+) and +Device+ (see <<fig-sm>>). It forwards events
# to the +handle+ method of +Controller+, and delegates commands to
# the +Device+.

nx::Class create CommunicationChannel {
  :property -accessor public {device:substdefault,object,type=::Device {[Device new]}}
  :property controller:object,type=::Controller
  :public method "send command" {code} {
    ${:device} $code
  }
  :public method "send event" {code} {
    ${:controller} handle $code
    return 1
  }
}

#
# The +Controller+ picks up (+handle+) events from
# +CommunicationChannel+ and triggers state transitions.
#

nx::Class create Controller {
  :property -accessor public currentState
  :property -accessor public {
    channel:substdefault,object,type=::CommunicationChannel
    {[CommunicationChannel new -controller [self]]}
  }
  :public method handle {eventCode} {
    set target [[${:currentState} transitions get $eventCode] target get]
    :transitionTo $target
  }
  :method transitionTo {target} {
    set :currentState $target
    ${:currentState} executeActions ${:channel}
  }
}

#
# Upon a transition, any +Command+ registered with the now entered
# +State+ are executed. The simulated device (+Device+) receives the
# commands as messages via the +CommandChannel+ to the same-named
# methods (+D1LK+, +PNUL+).
#

State public method executeActions {ch} {
  if {![info exists :actions]} return;
  foreach a ${:actions} {
    $ch send command [$a code get]
  }
}

#
# As a simple and testable setup, the compartment device is initiated
# into the +waitingForLightState+. The +Device+ then signals the
# +lightOn+ event, causing a state transition to
# +unlockedPanel+. This, in turn, leads to the methods +D1LK+ and
# +PNUL+ being executed on the +Device+ instance.
#

set ctrl [Controller new -currentState $waitingForLightState]

? {[[$waitingForLightState transitions get $lightOn] target get] name get} "unlockedPanel"
? {[$ctrl channel get] send event $lightOn} 1
set dev [[$ctrl channel get] device get]
? {$dev eval {set :eventHistory}} "D1LK PNUL"

# [NOTE] 
# .NX features 
# ====
# . metaclass: ...
# . callstack introspection: +[current method]+
# . method introspection: +[info method]+ ensemble
# ====
#
# ==== Excursus: Field validation
#
# Fowler's Chapter 42 on [pattern]#ANNOTATION# includes a different
# running example: How to specify a valid range for integer values to
# be enforced for object properties:
#
# [source,tcl]
# --------------------------------------------------
# # add range annotation
# :validRange height 1 120
# # annotated property
# :property -accessor public height:integer
#
# # add range annotation
# :validRange weight 1 1000
# # annotated property
# :property -accessor public weight:integer
# --------------------------------------------------
#
# While instructive, this example does not convey the benefits of
# [pattern]#ANNOTATION# in a compelling manner for NX/Tcl. This is
# partly because NX/Tcl offers native means to realize such
# value-domain checks or validation scenarios (see below).
# 
# BTAIM, below is an NX/Tcl implementation variant for Fowler's
# example, which tries to stick as closely as possible to the
# techniques discussed in Chapter 42 (class methods, use of closures).
#
#
# +DomainClass+ is a metaclass and provides a method +validRange+ to
# its instances (i.e., classes allowing for annotating
# properties). Internally, the actual validation (range-checking)
# logic is realized using Tcl's lambdas. The checking expression is
# stored as a lambda to be executed about (future) instances of
# +DomainClass+ instances.
#
nx::Class create DomainClass -superclass nx::Class {
  :variable -accessor public validations
  :public method validRange {name min max} {
    set vexpr {
      {min max property} {
        set value [:$property get]
        expr {$min <= $value && $max >= $value}
      }
    }
    dict lappend :validations [self] [list $vexpr $min $max $name]
  }
}

#
#
# +DomainObject+ a common superclass for +DomainClass+ instances. It
# provides +isValid+ which is reponsible for extracting the validation
# logic for a given class (+PatientVisit+) and to evaluate it over a
# given instance (i.e., an instance of +PatientVisit+).
#
nx::Class create DomainObject {
  :public method isValid {} {
    set domainClass [:info class]
    set validations [$domainClass validations get]
    set s [tcl::mathop::+ {*}[lmap v [dict get $validations $domainClass] {
      apply {*}$v
    }]]
    return [expr {$s == [llength $validations]}]
  }
}

#
# +PatientVisit+ is the so-annotated +DomainClass+ from Chapter 42.
# 
DomainClass create PatientVisit -superclasses DomainObject {
  # ----%<-----
  # add range annotation
  :validRange height 1 120
  # annotated property
  :property -accessor public height:integer

  # add range annotation
  :validRange weight 1 1000
  # annotated property
  :property -accessor public weight:integer
  # ----%<-----
}

? {[PatientVisit new -height 2 -weight 3] isValid} 1
? {[PatientVisit new -height 0 -weight 3] isValid} 0
? {[PatientVisit new -height 1 -weight 1001] isValid} 0

#
# As noted earlier, for this validation scenario, NX/Tcl offers alternative mechanisms:
#
# . Custom value checker +range+ for a +property+ (or a set of properties; see below).
# . Tcl variable traces can also be used on NX object variables.
# . NX provies built-in runtime assertion checking (RAC) incl. invariants on objects and classes.
#
# A value checker for the above +PatientVisit+ example could look like the following snippet:

nx::Class create PatientVisit {
  :property -accessor public height:range,arg=1-120 {
    :object method type=range {prop value minmax} {
      ::nsf::__db_show_stack; # FIX: Why is type=range executed twice in the TCL_OK case?
      lassign [split $minmax -] min max
      set isValid [expr {$min <= $value && $max >= $value}]
      if {!$isValid} {
        return -code error "value '$value' of parameter [:info name] not between $min and $max"
      }
      return $value
    }
  }
}

? {PatientVisit new -height 121} "value '121' of parameter height not between 1 and 120"
? {[PatientVisit new -height 2] cget -height} "2"

#
# === Dynamic Reception (427)
#
# Send messages to objects without providing method implementations
# for the scope of the receiving (builder) objects. This way, details
# of the undispatchable messages can be indirected and can enter the
# construction of a [pattern]#SEMANTIC MODEL# (rather than encoding
# those domain details as arguments passed into builder methods).
#
# [pattern]#DYNAMIC RECEPTION# is typically used as part of the
# [pattern]#EXPRESSION BUILDER# implementation.
#
# One alleged benefit is avoiding repetive or excessive keywords in
# the DSL scripts, e.g. +:command+ in, which might be pure artifacts
# of the underlying implementation technique such as [pattern]#OBJECT
# SCOPING#.
#
# [source,tcl]
# --------------------------------------------------
# # ...
#  :command unlockPanel "PNUL"
#  :command lockPanel "PNLK"
#  :command lockDoor "D1LK"
#  :command unlockDoor "D1UL"
# # ...
# --------------------------------------------------
#
# In NX/Tcl, there are at least two ways of realising a variant of
# [pattern]#DYNAMIC RECEPTION#:
#
# . +unknown+ handlers: One can define custom handlers which are
#   invoked when a message does not dispatch to any method known to a
#   given object. These +unknown+ methods complement Tcl's global and
#   per-namespace unknown handlers.
# . +filter+: One can define a +filter+ for the scope of a builder
#   object, to indirect any message sent to a given object (unknown
#   and known methods alike).

StateMachineBuilder property -accessor public cmds:0..*,object,type=::Command 

StateMachineBuilder public method commands {script} {
  set cb [CommandBuilder new -parent [self]]
  $cb eval $script
  return [self]
}

nx::Class create CommandBuilder {
  :property parent:object,type=::StateMachineBuilder
  :public method unknown {m args} {
    set cmd [Command new -childof ${:parent} -name $m -code [lindex $args 0]]
    ${:parent} cmds add $cmd
    return [self]
  }
}

#
# The newly introduced +CommandBuilder+ defines an +unknown+ handler for
# the scope of its instances. The handler method will trap any call
# which cannot resolved to a method defined for a given instance.
#
# In the examplary command definitions below, +unlockPanel+ etc. do
# not resolve to any defined method. This way, the generic unknown
# method becomes a builder method of domain objects: instances of
# +Command+.
#

set smb [StateMachineBuilder new]

$smb eval {
  # ----%<-----
  :commands {
    :unlockPanel "PNUL"
    :lockPanel "PNLK"
    :lockDoor "D1LK"
    :unlockDoor "D1UL"
  }
  # ----%<-----
}

? {llength [Command info instances ${smb}::*]} 4

#
# One disadvantage of using +unknown+ is that there are predefined
# methods on all NX objects and classes, which might prevent the use
# of certain terms or keywords as part of the internal DSL. In
# technical terms, the +unknown+ handler is not even fired on messages
# which match a built-in implementation (e.g., +info+, +delete+,
# +move+, or +destroy+). While the number of built-ins is
# comparatively small (i.e., 14 for instances of nx::Object), this
# might still lead to unexpected behavior or become unwieldly when the
# builder objects define some auxiliary methods etc.
#
# Using a +filter+, one can implement a similar scheme as allowed by
# the +unknown+ handler, but we can avoid conflicts with built-in
# methods by disambiguating calling contexts.
#
# Consider the challenge of allowing +destroy+ both to be a valid
# +Command+ name in the sense of the state-machine example and the
# message selector to destruct a +Command+ object.
#
# [source,tcl]
# --------------------------------------------------
# :commands {
#       # ...
#	:unlockPanel "PNUL"
# 	:destroy "EVILOP"
#       # ...
#  }
# --------------------------------------------------


nx::Class create CommandBuilder {
  :property parent:object,type=::StateMachineBuilder
  :method dispatcher args {
    set cm [current callingmethod]
    if {[info exists :on] && $cm eq "eval"} {
      set m [current calledmethod]
      set code [lindex $args 0]
      set cmd [Command new -childof ${:parent} -name $m -code [lindex $args 0]]
      ${:parent} cmds add $cmd
      return [self]
    } else {
      next
    }
  }

  :public method eval script {
    :object filters add dispatcher
    set :on 1
    next
    unset :on
    :object filters delete dispatcher
  }
}

#
# The above filter-based variant of +CommandBuilder+ has the following
# constituents:
#
# . +dispatcher+: This filter method which will indirect all calls to
#   instances of +CommandBuilder+. It uses NX's callstack
#   introspection (+current calledmethod|callingmethod+) and a
#   [pattern]#CONTEXT VARIABLE# +on+ to distinguish between "inner"
#   calls to construct +Command+ objects and +outer+ calls to be
#   forwarded to the given +CommandBuilder+ instance.
# . +eval+: This wrapper method sets up the relevant calling context
#   to handle "inner" calls. The call context is marked by setting a
#   switch +on+ as a [pattern]#CONTEXT VARIABLE# and actually
#   registering the +dispatcher+ filter method for a slim scope. The
#   use of a simple, one-level switch as [pattern]#CONTEXT VARIABLE#
#   is rather bold. To allow multiple levels of filter-enabled
#   +eval+s, with intermittent unfiltered call levels, this would
#   certainly be extended.
#
# The above allows for using +destroy+ in a double (inner vs. outer) sense:

set smb [StateMachineBuilder new]
$smb eval {
  # ----%<-----
  :commands {
    :destroy "EVILOP"; # inner destroy as valid DSL syntax element
  }
  # ----%<-----
}

? {llength [Command info instances ${smb}::*]} 1
set c [$smb cmds get]
? {$c name get} "destroy"

$c destroy; # outer destroy
? {llength [Command info instances ${smb}::*]} 0

#
# The downsides of filters are not only their overhead (which again
# must be justified as compared to building up the [pattern]#SEMANTIC
# MODEL# as such), but also their implementation that can easily
# render them fragile (e.g., indirecting unwanted or runtime-level
# calls). When done right, and clearly limited to the scope of the
# [pattern]#EXPRESSION BUILDER# (as done in the listing above), they
# are a more versatile implementation technique than +unknown+
# handlers for [pattern]#DYNAMIC RECEPTION#.
#

# [NOTE] 
# .NX features 
# ==== 
# . unknown: ...
# . filter (per-object): ...
# ====


#
# === Textual Polishing (477)
#
# Preprocess a DSL script using string manipulation techniques (and
# others) before executing the script.
#
# ...


#
# Thumbnail descriptions of referenced patterns
# ---------------------------------------------
# ...

#
# Excursus: Tcl idioms for implementing internal DSLs
# ---------------------------------------------------
#
# Many idioms and tactics to implement an internal DSL in NX/Tcl are
# more specific to Tcl than NX itself. In this section, I briefly
# iterate over important examples (e.g., literal collections based on
# Tcl lists).
#

#
# === Literal List (417), Literal Map (419), Literal Extension (481)
# ...

#
# === Closure (397), Nested Closure (403)
#
# Tcl does not have a first-class construct to capture the different
# notions of "closure", but it provides at least two means to emulate
# "closures" in the sense of Fowler's book and for their use in
# internal DSLs:
#
# . a piece of reusable NX/Tcl code which is represented as a first-class data
# structure (command, script storable as variable value),
# . which can be placed seamlessly into the flow of
# execution (e.g. as a command prefix),
# . and which can capture variables
# (values) available at the site (environment) of its definition (a.k.a. lexical closure).
#
# For internal DSLs, the availability of some [pattern]#NESTED
# CLOSURE# is considered important. If characteristic 1 above is
# sufficient, [pattern]#OBJECT SCOPING# in NX/Tcl using +eval+ is one
# approach. Most examples elaborated on in the Fowler book can be
# realized that way. A second approach, which combines characteristic
# 1 and characteristic 2 above, is to use Tcl's "lambdas"
# (+apply+). Characteristic 3 can only be approximated to some extent
# in Tcl, and is not necessarily needed for internal DSLs (at least,
# as covered by Fowler's book).
#
# In the context of an NX object, a Tcl lambda has access to the
# object environment it is executed in, like +eval+ (see
# [pattern]#OBJECT SCOPING#). In addition to +eval+, apply allows for
# passing additional (curried) arguments into the lambda.
#
# Below is a variant of the +ComputeBuilder+ example using Tcl natives
# +apply+:

nx::Class create ComputerBuilder {
  :property -accessor public result:object,type=::Computer
  :public method computer body {
    :result set [Computer new]
    apply [list {} $body]
  }
  :public method processor args {
    set proc [Processor new -childof ${:result}]
    apply $args $proc
    ${:result} processor set $proc
  }
  :public method disk args {
    set disk [Disk new -childof ${:result}]
    apply $args $disk
    ${:result} disk add $disk
  }
}

#
# The above implementation of an [pattern]#EXPRESSION BUILDER# allows
# for specifying a computer this way:
#

[ComputerBuilder new] computer {
  :processor {p} {
    $p cores set 2
    $p speed set 2.2
  }
  :disk {d} {
    $d size set 75
  }
  :disk {d} {
    $d size set 160
  }
}

#
# Notable differences to [pattern]#OBJECT SCOPING# (+eval+) plus
# [pattern]#METHOD CHAINING# are:
#
# . There is no need for returning +[self]+ from the builder methods
#   explicitly (or have a +filter+ implement such protocol)
# . One can devise explicit parameters (+d+, +p+) to identify and
#   to track builder or domain objects throughout the DSL script.
#
# One way of emulating an "environment-capturing" closure is a mix of
# a Tcl lambda (+apply+) and an NX object. The lambda realises the
# portable command prefix (plus [pattern]#OBJECT SCOPING#) and an
# inner NX object acts as a factory and, most importantly, maintains a
# Tcl +dict+, which represents the captured environment and allows for
# updating it. +dict with+ is then used to unpack the captured
# environment and stash away any changes for the subsequent evaluation
# of this closure.
#
nx::Class create closure {
  
  :property -accessor public {env:substdefault {[dict create]}}    
  :public method apply {vars body ns} {
    set defaults [concat [lmap i $vars {if {[llength $i] != 2} continue; set i}]]
    set self [self]
    set preamble "set __env__ \[dict merge \[$self env get\] $defaults\]"
    set trailer "$self env set \$__env__; return \$result"
    set lambda "$preamble; [list try [list dict with __env__ $body] on ok result $trailer]"
    return [list apply [list $vars $lambda $ns]]
  }
  :public object method new {vars body} {
    set env [dict create]
    # "info locals" would be fine, but does not allow to pass
    # values down into nested closures.
    foreach v [uplevel 1 {info vars}] {
      if {![uplevel 1 [list array exists $v]]} {
        dict set env $v [uplevel 1 [list set $v]]
      }
    }
    set cl [next [list -env $env]]
    set ns [uplevel 1 {namespace current}]
    return [$cl apply $vars $body $ns]
  }
}

# The above +closure+ construct can then be used as follows:

proc curriedAdd {x} {
  # the env to be captured
  return [closure new {y} {expr {$x + $y}}]
}

set add8 [curriedAdd 8]

? {{*}$add8 3} 11
? {{*}$add8 -3} 5
? {{*}$add8 3 3} {wrong # args: should be "apply lambdaExpr y"}

# Such closures can also be embedded into each other to realize a
# env-capturing [pattern]#NESTED CLOSURE#. In the below example, each
# closure level is closed over its parent closure.

proc makeEnsemble {{counter 0}} {
  closure new {call args} {
    set foo [closure new {msg} {puts "FOO($counter): $msg"; incr counter}]
    set bar [closure new {msg} {puts "BAR($counter): $msg"; incr counter}]
    set baz [closure new {msg {counter 0}} {puts "BAZ($counter): $msg"; incr counter}]
    set counter [{*}[set $call] {*}$args]
  }
}

set ensemble [makeEnsemble]
? {{*}$ensemble foo "Test"} 1
? {{*}$ensemble bar "Me"} 2
? {{*}$ensemble foo "Test"} 3
? {{*}$ensemble bar "Me"} 4
? {{*}$ensemble bar "Again"} 5
? {{*}$ensemble baz "Reset"} 1
? {{*}$ensemble baz "Me"} 1

#
# It is needless to say that this example of a [pattern]#NESTED
# CLOSURE# is just poor man's toy version of what is available with
# true lexical closures. For instance, it requires some discipline because one is
# actually nesting [dict with] commands across several levels, each
# operating on disconnected dicts. Therefore, each level N-1 is
# required to back-propagate value changes to level N by explicitly
# updating the N-level dict entry (in terms of +dict with+
# semantics): +set counter [{*}[set $call] {*}$args]+.
#
# Nevertheless, this might render useful for some scenarios of
# internal DSL scripts. Watch:
#

nx::Class create ComputerBuilder {
  :property -accessor public result:object,type=::Computer
  :public method computer body {
    set c [uplevel 1 [list closure new {} $body]]
    :result set [Computer new]
    {*}$c
  }
  :public method processor args {
    set c [uplevel 1 [list closure new {*}$args]]
    set proc [Processor new -childof ${:result}]
    {*}$c $proc
    ${:result} processor set $proc
  }
  :public method disk args {
    set c [uplevel 1 [list closure new {*}$args]]
    set disk [Disk new -childof ${:result}]
    {*}$c $disk
    ${:result} disk add $disk
  }
  :public method core body {
    set c [uplevel 1 [list closure new {} $body]]
    {*}$c
  }
}

[ComputerBuilder new] computer {
  :processor {p} {
    $p cores set 2
    $p speed set 2.2
    :core {
      ? [list expr [list [$p speed get] / [$p cores get]]] 1.1
    }
  }
  :disk {d} {
    $d size set 75
  }
  :disk {d} {
    $d size set 160
  }
}

#
# Values (the current processor identified by +p+) can be shared
# across the nesting levels (+processor+ and +core+) without being
# passed explicitly. This can facilitate employing a [pattern]#CONTEXT VARIABLE# in
# the context of a [pattern]#NESTED CLOSURE#.
#
# .TODO list
# - show how +filter+ can hide the boilerplate of creating and executing the closure.
#
# [NOTE] 
# .NX features 
# ==== 
# . +apply+ plus self-reference: ...
# ====

#
# === Function Sequence (351), Nested Functions (357)
#
# Tcl's function abstractions are called (named) procedures and, at
# the script level, they are defined using the +proc+ command. From
# the Tcl perspective, +proc+ defines a new script-level command.
# When a DSL expression or expression clause ressembles:
#
# . a sequence of optional elements incl. (possible) duplicates, then sequences
#   of procedure calls ([pattern]#FUNCTION SEQUENCE#) 
# . a sequence of mandatory elements, with and without duplicates, then nesting
#   function calls ([pattern]#NESTED FUNCTIONS#)
#
# are suggested to implement and to represent this fragment of an
# internal DSL syntax. A key difference between the two patterns is
# also that maintaining state must either be managed explicitly (for
# [pattern]#FUNCTION SEQUENCE# using a [pattern]#CONTEXT VARIABLE#) or
# is implicit to the stacking of procedure calls ([pattern]#NESTED
# FUNCTIONS#).
#
# While not strictly required for a Tcl-only solution, using an
# [pattern]#expression builder# suggests using [pattern]#OBJECT
# SCOPING# based on an NX builder object, to define and to resolve
# builder procedures. In this context, procedures map to NX
# methods. For sequencing, the necessary [pattern]#CONTEXT VARIABLE#
# are realised by object variables internal to the builder.

nx::Class create DiskBuilder {
  :property -accessor public size:integer
  :public method get {} {
    return [::Disk new -size ${:size}]
  }
}

nx::Class create ProcessorBuilder {
  :property -accessor public cores:integer
  :public method get {} {
    return [::Processor new -cores ${:cores}]
  }
}

nx::Class create ComputerBuilder {

  :variable processor:object,type=ProcessorBuilder
  :variable disks:object,type=DiskBuilder,1..*

  # context variables
  :variable currentProcessor:object,type=ProcessorBuilder
  :variable currentDisk:object,type=DiskBuilder
  
  :method computer {} {
    unset -nocomplain :currentProcessor
    unset -nocomplain :currentDisk
  }

  :method processor {} {
    set :currentProcessor [ProcessorBuilder new]
    set :processor ${:currentProcessor}
    unset -nocomplain :currentDisk
  }

  :method cores {arg:integer} {
    ${:currentProcessor} cores set $arg
  }

  :method disk {} {
    set :currentDisk [DiskBuilder new]
    lappend :disks ${:currentDisk}
    unset :currentProcessor
  }

  :method size {arg:integer} {
    ${:currentDisk} size set $arg
  }

  :public method get {} {
    return [::Computer new \
                -processor [${:processor} get] \
                -disk [lmap d ${:disks} {$d get}]]
  }

}

#
# The +ComputerBuilder+ class above defines a number of (protected)
# methods that can be called in sequence (+computer+, +processor+,
# +disk+) to define a computer configuration. See a snippet below. The
# intended hierarchical relationships between the builder for the
# whole and the builders for the parts (+ProcessorBuilder+,
# +DiskBuilder+) must be established explicitly by maintaining
# [pattern]#CONTEXT VARIABLES#: +currentProcessor+,
# +currentDisk+.
#
# This implementation examples corresponds to the one
# in Fowler's Section 34.3.
# 

ComputerBuilder create ::cb

::cb eval {
    # ----%<-----
    :computer
      :processor
        :cores 2
      :disk
        :size 75
    # ----%<-----
  }
#
# Note that the different indentation levels for the procedure or
# method calls is purely conventional and visual sugar, without any
# operative meaning. The result of the construction process can be
# obtained using +get+:
#
set result [::cb get]
? {$result info class} "::Computer"
? {llength [$result processor get]} "1"
? {llength [$result disk get]} "1"
? {[$result disk get] size get} "75"

#
# If one wants to capture mandatoriness of occurrence and/or
# element precedence (e.g., each computer must contain a
# processor, the processor must be defined before any disk), this
# task is facilitated by building the syntax fragment
# around nested procedure or method calls: [pattern]#NESTED FUNCTIONS#
#
# To do so, one can rewrite the +ComputerBuilder+ from above as follows:
#

nx::Class create ComputerBuilder {
  
  :public method computer {p:object,type=Processor, args:object,type=Disk} {
    return [::Computer new \
                -processor $p \
                -disk $args]
  }

  :public method processor {cores:integer} {
    return [::Processor new -cores $cores]
  }

  :public method cores {arg:integer} {
    return $arg
  }

  :public method disk {size:integer} {
    return [::Disk new -size $size]
  }

  :public method size {arg:integer} {
    return $arg
  }

}

#
# As opposed to the first +ComputerBuilder+ based on
# [pattern]#FUNCTION SEQUENCE#, [pattern]#NESTED FUNCTIONS# the
# results of inner method calls (+cores+ and +size+) are further
# processed as arguments to outer method calls (+processor+, +disk+,
# and, ultimately, +computer+). This way, [pattern]#CONTEXT VARIABLES#
# are not needed. In addition, the builder can directly populate the
# [pattern]#SEMANTIC MODEL#.
#

ComputerBuilder create ::cb

set result [::cb eval {
  # ----%<-----
  :computer \
      [:processor \
           [:cores 2]
      ] \
      [:disk \
           [:size 75]
      ] \
      [:disk \
           [:size 256]
      ]
    # ----%<-----
}]

? {$result info class} "::Computer"
? {llength [$result processor get]} "1"
? {llength [$result disk get]} "2"
? {lmap d [$result disk get] {$d size get}} "75 256"

#
# The details of the Tcl syntax (command substitution "[... [...]]",
# multi-line commands split by "\") however, result a comparably
# unwieldy notation of the computer configuration using nested method
# calls. Tcl provides an embedded, yet alternative syntax: the +expr+
# command comes with a hybrid expression syntax that is build from a
# combination of operands, operators, parentheses for grouping and to
# signal function calls, as well as commas (to separate formal
# arguments). One type of operands are (mathematical) functions and a
# [pattern]#EXPRESSION BUILDER# can add additional functions, this
# way, piggybacking onto this embedded expression syntax.
#
# One option to achieve the latter is to create command alias in the
# tcl::mathfunc namespace:

interp alias {} ::tcl::mathfunc::computer {} ::cb computer
interp alias {} ::tcl::mathfunc::processor {} ::cb processor
interp alias {} ::tcl::mathfunc::cores {} ::cb cores
interp alias {} ::tcl::mathfunc::disk {} ::cb disk
interp alias {} ::tcl::mathfunc::size {} ::cb size

#
# This allows one to rewrite the nested method calls as nested
# function calls for +expr+, without the need for backslashes
#

set result [expr {
         computer(
           processor(cores(2)),
           disk(size(75)),
           disk(size(256))
          )
       }]

? {$result info class} "::Computer"
? {llength [$result processor get]} "1"
? {llength [$result disk get]} "2"
? {lmap d [$result disk get] {$d size get}} "75 256"

#
# This piggybacking onto +expr+ does not only render a more compact
# and streamlined notation, it also closely ressembles the notational
# examples in Fowler's Section .
#

# Local variables:
#    mode: tcl
#    tcl-indent-level: 2
#    indent-tabs-mode: nil
# End:








