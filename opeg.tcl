package req nx
package require pt::rde::nx
package req pt::peg::op

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
      # puts BODY=$body
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

  nx::Class create Rewriter -superclasses ::nx::Class {

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
      :input {*}$ast
      # puts stderr AST=$ast
      # : input {*}$ast
    }

  }

  nx::Class create Grammar -superclasses Rewriter {

    :property -accessor public {name:substdefault "[namespace tail [self]]"}
    :property -accessor public {start ""};

    :object variable -accessor public parser:object [Parser new]

    :property -accessor public {merges:class,0..*,substdefault "[list]"} {
      :public object method value=set {obj prop value} {
        set m [next]
        $obj eval {:resetResulting}
        return $m
      }
    }

    :protected method __object_configureparameter {} {
      set spec [next]
      lreplace $spec[set spec {}] end end setRules:alias,optional setTransforms:alias,optional
    }

    ::nsf::parameter::cache::classinvalidate [current]


    :public method clear {} {
      unset -nocomplain :rules
      dict set :rules ${:start} [pt::pe epsilon]
      :resetResulting
      return
    }

    :protected method resetResulting {} {
      if {[::nsf::is object [self]::resulting]} {
        [self]::resulting destroy
      }
    }
    
    :protected method setTransforms {script} {
      set :transforms $script
    }

    :public method loadTransforms {script} {
      :resetResulting
      :setTransforms $script
    }

    :protected method setRules {rules} {
      set tmpl {OPEG @name@ (@start@)
        @rules@
        END;}
      if {${:start} eq ""} {
        throw [list DJDSL OPEG NOSTART [self]] \
            "Start symbol must be provided when initialising from rules set."
      }
      set mappings [list @name@ ${:name} @start@ ${:start} @rules@ $rules]
      set opegScript [string map \
                          $mappings \
                          $tmpl]
      :setScript $opegScript
      return
    }

    :protected method setScript {script} {
      set opegAst [[[current class] parser get] parset $script]
      :load $opegAst $script
      return
    }

    :public method loadRules {script} {
      :clear
      :setRules $script
      return
    }

    :public object method newFromScript {script args} {
      set g [:new {*}$args]
      $g eval [list :setScript $script]
      return $g
    }
    
    

    nx::Class create [self]::ParserClass -superclasses nx::Class {
      :property -accessor public generator
      :property -accessor public factory

      :method init {} {
        if {![info exists :factory]} {
          :factory set [::djdsl::opeg::ModelFactory new]
        }
      }
    }

    nx::Class create [self]::Container {

      :property grammar
      :property all

      #
      # Provide a subset of the pt::peg::container, as needed by the PG
      # transformations (non-recognizing? inaccessible?)
      #

      :public method start {pe:optional} {
        if {[info exists pe]} {
          set pe [expr {[llength $pe] > 1 ? [lindex $pe 1] : $pe}]
          ${:grammar} start set $pe
        } else {
          list n [${:grammar} start get]
        }
      }

      :public method nonterminals {} {
        ${:grammar} rules nts
        # return ${:all}
      }

      :public method rules {d:optional} {
        if {[info exists d]} {
          ${:grammar} rules set $d
        } else {
          ${:grammar} rules get
        }
      }

      :public method rule {nt r:optional} {
        if {[info exists r]} {
          ${:grammar} rules add $nt $r
        } else {
          ${:grammar} rules rhs $nt
        }
      }

      :public method exists {nt} {
        ${:grammar} rules isSet $nt
      }

      :public method remove {args} {
        foreach nt $args {
          dict set :removed $nt _
          ${:grammar} rules delete $nt
        }
      }

      :public method getRemoved {} {
        if {[info exists :removed]} {
          return [dict keys ${:removed}]
        }
        return
      }

      #
      # This is a modified clone of the DROP API for unrealisable NTs
      # as found in pt_peg_op.tcl. For choice expressions, we need to
      # track the alternates that become dropped. To goal is to update
      # the generators registered for the corresponding
      # rule. Otherwise, the generators get out of sync because of a
      # cleaning operation.
      #

      :public method drop {dropset serial} {
        set res [pt::pe bottomup \
                     [list [self] op Drop $dropset] \
                     $serial]
        if {$res eq "@@"} { set res [pt::pe epsilon] }
        return $res
      }
      
      :public method dropUnrealizable {} {
        set all [::pt::peg::op reachable [self]]
        lappend all {} ;
        
        set unrealizable \
            [struct::set difference \
                 $all [pt::peg::op realizable [self]]]
        
        if {![llength $unrealizable]} return
        
        if {[struct::set contains $unrealizable {}]} {
          struct::set exclude unrealizable {}
          :start epsilon
        }
        
        :remove {*}$unrealizable
                
        foreach symbol [:nonterminals] {
          :rule $symbol \
              [:drop $unrealizable \
                   [:rule $symbol]]
          if {[info exists :droppedAlternates]} {
            :removeAlternate $symbol ${:droppedAlternates}
            unset :droppedAlternates
          }
        }
        return
      }
      
      :public method "op Drop" {args} {
        lassign $args _ _ op children
        if {$op eq "/"} {
          set :droppedAlternates [lsearch -all -exact $children @@]
        }
        ::pt::pe::op::Drop {*}$args
      }

      :protected method removeAlternate {nt alternates} {
        if {[${:grammar} eval {info exists :specs}]} {
          set specs [${:grammar} eval {set :specs}]
          if {[dict exists $specs $nt]} {
            set igens [dict get $specs $nt]
            foreach a $alternates {
              set igens [lreplace $igens $a $a]
            }

            # puts IGENS=$igens
            if {[llength [concat {*}$igens]]} {
              dict set specs $nt $igens
            } else {
              dict unset specs $nt
            }
          }
          ${:grammar} eval [list set :specs $specs]
        }
      }
      
    };# Container
    

    :property -accessor public modes

    :public method qualify {rules} {
      set all [dict keys $rules]
      if {[info exists :accessed]} {
        lappend all {*}[concat {*}${:accessed}]
      }
      set all [lsort -unique $all]
      # puts QUALIFY=$all
      foreach nt $all {
        set qNt ${:name}::${nt}

        dict for {lhs rhs} $rules {
          if {$lhs eq $nt} {
            dict unset rules $lhs
            set lhs $qNt
          }
          dict set rules $lhs [pt::pe::op rename $nt $qNt $rhs]
        }
      }
      return $rules
    }

    :public method unqualify {rules nts} {
      set nts [concat {*}[lmap nt $nts {list $nt [namespace tail $nt]}]]
      # puts UQRULES=$rules
      foreach {qNt nt} $nts {
        dict for {lhs rhs} $rules {
          if {$lhs eq $qNt} {
            dict unset rules $lhs
            set lhs $nt
            if {[dict exists $rules $lhs]} {
              throw [list DJDSL OPEG DUPLICATE $lhs] "Non-terminal '$lhs' is already defined."
            }
          }
          dict set rules $lhs [pt::pe::op rename $qNt $nt $rhs]
        }
      }
      return $rules
    }

    :public method unqualify2 {rules nts} {
      set nts [concat {*}[lmap nt $nts {set out [string map {:: //} $nt]; list $out $nt}]]
      # puts UQRULES=$rules,$nts
      foreach {nt qNt} $nts {
        dict for {lhs rhs} $rules {
          if {$lhs eq $qNt} {
            dict unset rules $lhs
            set lhs $nt
            if {[dict exists $rules $lhs]} {
              throw [list DJDSL OPEG DUPLICATE $lhs] "Non-terminal '$lhs' is already defined."
            }
          }
          dict set rules $lhs [pt::pe::op rename $qNt $nt $rhs]
        }
      }
      return [list $rules $nts]
    }



    :property -accessor public -incremental rules {
      :public object method value=set {obj prop value} {        

        if {[$obj $prop isSet]} {
          set value [dict merge [$obj $prop get] $value]
        }
        
        next [list $obj $prop $value]
      }
      
      :public object method value=isSet {obj prop nt:optional} {
        set isDictSet [$obj eval [list info exists :$prop]]
        if {![info exists nt]} {
          return $isDictSet
        } else {
          return [expr {$isDictSet && [dict exists [$obj $prop get] $nt]}]
        }
      }

      #
      # get fully qualified rule set
      #

      :public object method value=fqn {obj prop nt:optional} {
        set rules [next [list $obj $prop]]
        if {[info exists nt]} {
          dict filter $rules key $nt
        } else {
          return $rules
        }
      }

      
      :public object method value=get {obj prop nt:optional} {
        set rules [next [list $obj $prop]]
        if {[info exists nt]} {
          dict filter $rules key $nt
        } else {
          return $rules
        }
      }

      :public object method value=rhs {obj prop nt:optional} {
        set rules [$obj $prop get]
        if {[info exists nt]} {
          dict get $rules $nt
        } else {
          return [dict values $rules]
        }
      }

      :public object method value=nts {obj prop nt:optional} {
        set rules [$obj $prop get]
        dict keys $rules
      }
      
      :public object method value=add {obj prop nt rhs} {
        $obj eval [list dict set :$prop $nt $rhs]
      }

      :public object method value=delete {obj prop nt} {
        $obj eval [list dict unset :$prop $nt]
        $obj eval [list dict unset :modes $nt]
      }

      :public object method value=rename {obj prop oldNt newNt} {
        $obj rename $oldNt $newNt
        $obj $prop get $newNt
      }
    }

    :method init {} {
      set supers [::nsf::relation::get [self] superclass]
      if {$supers eq "::nx::Object"} {
        set supers [linsert $supers[set supers {}] end-1 \
                        {*}${:merges} [current class]::ParserClass]
        ::nsf::relation::set [self] superclass $supers
      }
    }

    :public object method print {opegScript} {
      ${:parser} print $opegScript
    }

    #
    # Parser API
    #

    :public method new {args} {
      set cls [next [list -superclasses [namespace current]::Builder \
                         -generator [self] \
                         {*}$args]]

      # set cls [:new -superclasses [namespace current]::Builder \
        #                 -generator [self] \
         #                {*}$args]

      
      ## initialize to NX/PEG backend defaults or dummies
      pt::tclparam::configuration::nx def _ _ _  \
          {pt::peg::to::tclparam configure}
      
      ## strip down to just the core script fragment
      pt::peg::to::tclparam configure -template {@code@}

      set resulting [:resulting]
      $resulting clean
      set body [pt::peg::to::tclparam convert [$resulting asPEG]]
      # puts body=$body
      $cls eval $body

      return [$cls new]
    }

    :public method clean {} {
      set container [::djdsl::opeg::Grammar::Container new -grammar [self]]
      ::pt::peg::op flatten $container
      # puts SPECS(IN)=${:specs}

      if {0} {
        puts ------------------------
        dict for {k v} [:rules get] {
          puts "$k <- $v"
        }
        puts ------------------------
        puts REALIZABLE=[::pt::peg::op realizable $container]
        puts REACHABLE=[::pt::peg::op reachable $container]
      }

      # ::pt::peg::op drop unrealizable $container
      $container dropUnrealizable
      
      # puts RULES2=[:rules get]
      ::pt::peg::op drop unreachable $container
      # puts RULES3=[:rules get]
      
      ::pt::peg::op flatten $container

      set :all [::pt::peg::op reachable $container]
      
      set removed [$container getRemoved]
      # puts removed=$removed
      if {[llength $removed]} {
        set :specs [dict remove ${:specs} {*}$removed]
      }

      # puts SPECS(OUT)=${:specs}
      $container destroy
      return ${:all}
    }

        #     if {0} {
        #   # check for orphans
        #   set container [::djdsl::opeg::Grammar::Container new -grammar $o -all $all]
        #   set inReach [::pt::peg::op reachable $container]
        #   set defined [$container nonterminals]
        #   puts ALL=$all=VS=DEFINED=$defined
          
        #   set orhpans [list]
        #   foreach reach $inReach {
        #     if {$reach ni $defined} {
        #       lappend orphans $reach
        #     }
        #   }
          
        #   puts ORPHANS=$orphans
          
        #   if {0 && [llength $orphans]} {
        #     foreach symbol [$container nonterminals] {
        #       $container rule $symbol \
        #           [pt::pe::op drop $orphans \
        #                [$container rule $symbol]]
        #     }
        #   }
        # }


    :public method transform {opnd1 op args} {
      :TRANSFORM $op {*}$opnd1 {*}$args
    }
    
    :public method resulting {args} {
      set o [current]::resulting
      if {![::nsf::object::exists $o]} {
        set uqStart ${:start}
        lassign [:getResulting] rules start modes all specs
        [current class] create $o -name ${:name} -start $start
        # puts START=$uqStart,$start,${:start}
        # $o name set ${:name}

        $o rules set $rules
        # $o start set $start; # puts START=$start
        $o modes set $modes
        $o eval [list set :specs $specs]

        if {[info exists :transforms]} {
          if {[info commands [self]::tinterp] eq ""} {
            interp create -safe [self]::tinterp
            [self]::tinterp eval {namespace delete ::}
          }

          interp alias [self]::tinterp ::unknown {} $o transform

          # $o eval ${:transforms}
          [self]::tinterp eval ${:transforms}

          set nts [$o clean]
          # puts NTS=$nts

          if {0} {
            set r [$o unqualify [$o rules get] $nts]
            $o eval [list set :rules $r];
            $o eval [list set :start $uqStart]
            set specs [$o eval {set :specs}]
            dict for {k v} $specs {
              dict set specs [namespace tail $k] $v
              dict unset specs $k
            }
            $o eval [list set :specs $specs]
          }
        } else {
          # simple union
          $o clean
        }
      }
      $o {*}$args
    }

    #
    # TRANSFORMS
    #

    :public forward "TRANSFORM <*>" %self import -gtors -rewrite -cascade
    
    :public forward "TRANSFORM <=>" %self import -gtors -rewrite

    :public forward "TRANSFORM <->" %self import -rewrite

    :public forward "TRANSFORM <==" %self import -gtors

    :public forward "TRANSFORM <-=" %self import 
    
    :public method import {-gtors:switch -rewrite:switch -cascade:switch tgt src {position end}} {

      set qTgt ${:name}::$tgt

      if {![dict exists ${:rules} $src]} {
        throw [list DJDSL OPEG TRANSFORM NX $src] "The source non-terminal '$src' does not exist."
      }
      
      set rhs [dict get ${:rules} $src]

      set called [pt::pe::op called $rhs]
      if {$rewrite} {
        foreach c $called {
          set qNt ${:name}::[namespace tail $c]
          set rhs [pt::pe::op rename $c $qNt $rhs]

          if {$cascade && $c ne $src && [dict exists ${:rules} $c]} {
            :import -rewrite -cascade -gtors [namespace tail $c] $c
          }
        }
      }

      if {![dict exists ${:rules} $qTgt]} {
        # new rule, set RHS
        set new $rhs

        if {[dict exists ${:modes} $src]} {
          dict set :modes $qTgt [dict get ${:modes} $src]
        } else {
          # TODO: Does this make sense? Can this happen?
          dict set :modes $qTgt value
        }
      } else {
        # existing rule, add to RHS according to position
        set existing [dict get ${:rules} $qTgt]
        lassign $existing op
        if {$op eq "/"} {
          set pos [expr {[string is integer -strict $position]? 1+$position : $position}]
          set new [linsert $existing[set existing {}] $pos $rhs]
        } else {
          if {$position eq "end"} {
            set new [pt::pe choice $existing $rhs]
          } else {
            set new [pt::pe choice $rhs $existing]
          }
        }
        # TODO: What happens to MODES here, if not in accordance?
      }
      dict set :rules $qTgt $new
      # puts SPECS1=${:specs}
      if {$gtors && [dict exists ${:specs} $src]} {
        set add [dict get ${:specs} $src]
        if {[dict exists ${:specs} $qTgt]} {
          set current [dict get ${:specs} $qTgt]
          set current [linsert $current $position {*}$add]
          dict set :specs $qTgt $current
        } else {
          dict set :specs $qTgt $add
        }
      }
      # puts SPECS2=${:specs}

      if {0} {
        # TODO: Is field handling needed?
        puts CALLED=$called
        set fields [lsearch -glob -inline -all $called *::_FIELD_*]
        puts FIELDS=$fields
        foreach f $fields {
          # :import [namespace tail $f] $f
          foreach c [pt::pe::op called [dict get ${:rules} $f]] {
            # :import [namespace tail $c] $c
          }
        }
      }
      
      return
    }

    :public method "TRANSFORM ==>" {src position:optional} {
      if {[namespace tail $src] eq $src} {
        set src ${:name}::$src
      }
      if {![dict exists ${:rules} $src]} {
        throw [list DJDSL OPEG TRANSFORM NX $src] "The source non-terminal '$src' does not exist."
      }

      if {[info exists position]} {
        set rhs [dict get ${:rules} $src]
        lassign $rhs op
        if {$op eq "/"} {
          if {[llength $rhs] > 1} {
            set pos [expr {[string is integer -strict $position]? 1+$position : "end"}]
            set rhs [lreplace $rhs $pos $pos]
            dict set :rules $src $rhs
            if {[dict exists ${:specs} $src]} {
              set spec [dict get ${:specs} $src]
              set spec [lreplace $spec $position $position]
              dict set :specs $src $spec
            }
          } else {
            dict unset :rules $src
            set :specs [dict remove ${:specs} $src]
          }
        }
      } else {
        dict unset :rules $src
        set :specs [dict remove ${:specs} $src]
      }
      
      return
    }

    :public method getSpecs {} {
      if {[info commands [self]::resulting] ne ""} {
        [self]::resulting getSpecs
      } else {
        set specs [dict create]
        dict for {k v} ${:specs} {
          dict set specs [string map {:: //} $k] $v
        }
        return $specs
      }
    }
    
    :public method asPEG {} {
      set rules [dict create]

      lassign [:unqualify2 ${:rules} ${:all}] rules ntMap

      dict for {nt rhs} $rules {
        dict set rules $nt [list is $rhs mode [dict get ${:modes} [dict get $ntMap $nt]]]
        # puts "$nt <- $rhs"
      }
      set peg [list pt::grammar::peg [list rules $rules start [list n [string map {:: //} ${:start}]]]]

      pt::peg verify-as-canonical $peg
      
      return $peg
    }

    :method getUseful {rules accessed Ne} {
      #
      # Remove useless (empty + inaccessible) rules. In essence, this
      # is an implementation variant of Aho's and Ullman's algorithms
      # 2.7, 2.8, and 2.9, TPTC, Chapter 2, pp. 144. A difference is
      # that we do not maintain a set of terminals, in the strictest
      # sense, but a set of nonterminals that can recognize a terminal
      # (Ne). This is grown recursively, to include transitively dependent
      # nonterminals to arrive at a final Ne. This procedure has a
      # worst-case time complexity of O(n+1), with n being the number
      # of nonterminals defined (all LHS and RHS).
      #
      
      set N [llength [lsort -unique [concat {*}$accessed]]]

      set i 0
      while {$i <= $N} {
        incr i
        set next $Ne
        dict for {k v} $accessed {
          foreach nt $v {
            if {$nt in [dict keys $Ne]} {
              dict incr next $k
            }
          }
        }
        if {[dict size $next] != [dict size $Ne]} {
          set Ne $next
        } else {
          break
        }
      }

      set accessed [dict filter $accessed script {k v} {dict exists $Ne $k}]

      set called [list ${:start} {*}[concat {*}[dict values $accessed]]]

      return [dict filter $rules script {k v} {expr {$k in $called}}]
    }
    
    :method getResulting {} {

      #
      # inclusion (union with override, TODO: disjoint union)
      #
      
      set includes [list {*}[lreverse [:info heritage]] [self]]
      set rules [dict create]
      set accessed [dict create]
      set terminals [dict create]
      set modes [dict create]
      set specs [dict create]


      # puts includes=([self])=$includes
      foreach extra $includes {
        if {![$extra info has type [current class]]} continue;
        set extraRules [$extra rules get]
        if {[info exists :transforms]} {
          set extraRules [$extra qualify $extraRules]
        }
        set rules [dict merge $rules $extraRules]
        if {[$extra eval {info exists :terminals}]} {
          set terminals [dict merge $terminals [$extra eval {set :terminals}]]
        }

        if {[$extra eval {info exists :accessed}]} {
          set accessed [dict merge $accessed [$extra eval {set :accessed}]]
        }

        
        # set modes [dict merge $modes [$extra eval {set :modes}]]
        set m [$extra eval {set :modes}] 
        if {[info exists :transforms]} {
          dict for {k v} $m {
            dict set m [$extra name get]::$k $v
            dict unset m $k
          }
        }
        set modes [dict merge $modes $m]
        
        if {[$extra eval {info exists :specs}]} {
          set s [$extra eval {set :specs}]
          if {[info exists :transforms]} {
            dict for {k v} $s {
              dict set s [$extra name get]::$k $v
              dict unset s $k
            }
          }
          set specs [dict merge $specs $s]
        }
      }

      #puts rules=$rules
      # puts accessed=$accessed
      # puts terminals=$terminals
      # puts MERGE:modes=$modes

      # set rules [:getUseful $rules $accessed $terminals]
      set s ${:start}
      if {[info exists :transforms]} {
        set s ${:name}::$s
      }
      
      return [list $rules $s $modes [lsort -unique [concat {*}$accessed]] $specs]
      
    }

    :public method rename {oldNt newNt} {
      if {[dict exists ${:rules} $oldNt] && ![dict exists ${:rules} $newNt]} {
        dict set :rules $newNt [dict get ${:rules} $oldNt]
        dict unset :rules $oldNt
      } else {
        throw [list DJDSL OPEG GRAMMAR RENAME [self] $oldNt $newNt] \
            "Renaming a rule from '$oldNt' to '$newNt' failed."
      }

      if {[info exists :accessed]} {
        # LHS renaming
        if {[dict exists ${:accessed} $oldNt]} {
          dict set :accessed $newNt [dict get ${:accessed} $oldNt]
          dict unset :accessed $oldNt
        }

        if {0} {
          # RHS renaming
          set rewritten [dict create]
          puts BEFORE=${:accessed}
          dict for {k v} ${:accessed} {
            puts "$oldNt in $v"
            if {$oldNt in $v} {
              set v [lsearch -exact -not -all -inline $v $oldNt]
              lappend v $newNt
            }
            dict set rewritten $k $v
          }
          set :accessed $rewritten
        }
      }
      # puts ACCESSED=${:accessed}

      # puts TERMINALS=${:terminals}
      if {[info exists :terminals] && [dict exists ${:terminals} $oldNt]} {
        dict set :terminals $newNt [dict get ${:terminals} $oldNt]
        dict unset :terminals $oldNt
      }
      # puts TERMINALS=${:terminals}

      return
    } 

    
    #
    # OPEG to PEG rewriter
    #
    :public method load {opegAst input} {

      set :(defCounter) 0
      set peg [:rewrite $opegAst $input]
      set pegAst [lindex $peg 1]
      unset :(defCounter)
      ## add ctors to OPEG structure
      if {[info exists :specs]} {
        set :specs [dict map {nt specs} ${:specs} {
          if {![llength [concat {*}$specs]]} {
            continue
          }
          set specs
        }]
      }

      # puts =========
      # puts LOADSPEC([self])=${:specs}
      dict for {nt rhs} [dict get $pegAst rules] {
        array set "" $rhs
        dict set :rules $nt $(is)
        dict set :modes $nt $(mode) 
      }
      # puts LOAD=rules=${:rules},modes=${:modes}
      set :start [lindex [dict get $pegAst start] 1]
      
      array unset :{}
      
    }

    :method "input Grammar" {s e args} {
      if {[info exists :(fieldDefs)]} {

        set tmp [dict map {fieldDef defs} ${:(fieldDefs)} {
          if {[llength $defs] > 1} {
            lindex $defs end 
          } else {
            lindex $defs 0
          }
          
        }]
        # lappend args {*}[concat {*}[dict values ${:fieldDefs}]]
        lappend args {*}[dict values $tmp]
        unset :(fieldDefs)
      }
      next [list $s $e {*}$args]      
    }

    :method "input Header" {s e args} {
      if {${:name} eq [namespace tail [self]]} {
        # update name defaults from the actual grammar def
        set :name [lindex $args 0 1]
      }
      if {${:start} eq ""} {
        set :start [lindex $args 1 1]
      }
      next
    }
    
    :method "input Ctor" {s e args} {
      return [list c [lindex $args 0 1]]
    }

    # TODO: Is this needed?
    :method "input Command" {s e args} {
      # operates like Ident
      return [:input Ident $s $e]
    }

    :method "input Field" {s e args} {
      set args [lassign $args field]

      set ntIdent "_FIELD_${:(defCounter)}_[lindex $field 1]"
      # 1) compile + register 'field' definitions.
      #
      # _1_x {is {n Digit} mode value} _1_y {is {n Digit} mode value}
      
      dict lappend :(fieldDefs) $ntIdent [pt::peg::from::peg::GEN::Definition $s $e "value" [list n $ntIdent] [lindex $args 0]]
      # 
      # 2) inject reference to 'field' definition identifiers
      #
      # {n x} {n Digit} -> n _1_x

      #
      # 3) Are there "fixes" (paths) to consider later on? This ressembles the
      # behavior in "input Definition" -> refactor?
      #
      set f [lindex $field 1]
      if {[info exists :(choices)]} {
        # puts stderr choices=${:choices}
        set c [lindex ${:(choices)} end]
        # puts stderr c=${:choices}
        # lappend f $c
        # puts stderr f=$f
        dict set :specs $ntIdent $c
        ## piggyback onto :spec
        unset :(choices)
      }
      # lappend :fields $f
      
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
      if {[llength $args] > 1 && [lindex $ctor 0] eq "c"} {
        # TODO: can there be more than one fix at a time? Test: dict
        # set spec generator [lrange $ctor 1 end]
        dict set spec generator [lindex $ctor 1]
        set args [lrange $args 1 end]
      }

      # TODO: Remove?
      if {[info exists :(fields)] && [llength ${:(fields)}]} {
        dict set spec fields ${:(fields)}
        unset :(fields)
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
      lappend :(choices) $choices
      next [list $s $e {*}$rargs]
    }

    #
    # The intercepting method on Primary is meant to bookkeep about
    # the presence of nonterminals or terminals at the RHS of a given
    # rule definition. This bookkeeping data is then stored in
    # Definition (and StartExpr) along with the rule, to be used
    # latter on (perfective) transformations on the parser definition.
    #
    # As an alternative, one might use the ::pt::pe::op::* operations
    # on the canonical PE representation (e.g., ::pt::pe::op::called)
    # ; but we obtain them early to avoid extra and repeated passes
    # over the PE/PG representations.
    #
    
    :method "input StartExpr" {s e args} {
      unset -nocomplain :(accessed)
      unset -nocomplain :(terminals)
      next
    }

    
    :method "input Primary" {s e args} {
      set prim [next]
      if {[lindex $prim 0] eq "n"} {
        lappend :(accessed) [lindex $prim 1]
      } else {
        incr :(terminals)
      }
      return $prim
    }

    
    :method "input Definition" {s e args} {
      incr :(defCounter)
      set def [next]
      set nt [lindex $def 0]
      if {[info exists :(choices)]} {
        set c [lindex ${:(choices)} end]
        dict set :specs $nt $c
        unset :(choices)
      }
      
      if {[info exists :(accessed)]} {
        set notRecursive [lsearch -not -exact -inline -all ${:(accessed)} $nt]
        # puts def($nt)=${:(accessed)},NR=$notRecursive
        if {[llength $notRecursive]} {
          dict lappend :accessed $nt {*}$notRecursive
        }
      }

      if {[info exists :(terminals)]} {
        dict set :terminals $nt ${:(terminals)}
      }
      
      unset -nocomplain :(accessed)
      unset -nocomplain :(terminals)
      
      return $def
    }

  }
  
  nx::Class create BuilderGenerator -superclasses Rewriter {

    :property {parser:object,substdefault {[Parser new]}}

    nx::Class create [self]::Class -superclasses nx::Class {
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

    :public method "bgen script" {
      opegScript
      {modelFactory:substdefault {[ModelFactory new]}}} {
      
      try {
        # 1) Transform OPEG grammar into OPEG "AST"
        set opegAst [${:parser} parset $opegScript]
        # 2) Downshape OPEG "AST" into serial PEG
        set ser [:rewrite $opegAst $opegScript]
        # 3) Generate PEG+ parser bundle
        puts ser=$ser
        ## initialize to NX/PEG backend defaults or dummies
        pt::tclparam::configuration::nx def _ _ _  \
            {pt::peg::to::tclparam configure}
        
        ## strip down to just the core script fragment
        pt::peg::to::tclparam configure -template {@code@}
        
        set body [pt::peg::to::tclparam convert $ser]
        #puts stderr body=$body
        set cls [[current class]::Class new \
                     -superclasses [namespace current]::Builder \
                     -factory $modelFactory \
                     -generator [self] -- $body]
        return $cls
      } trap {PT RDE SYNTAX} {msg} {
        throw [list DJDSL OPEG SYNTAX {*}$msg] "Invalid OPEG supplied"
      } on error {e opts} {
        throw {DJDSL OPEG GENERATOR} "Generating parser from OPEG failed: '$e'"
      }
    }
    
    :public method "bgen file" {filepath} {
      set fh [open $filepath r]
      try {
        set opegScript [read $fh]
        :bgen script $opegScript {*}$args
      } finally {
        close $fh
      }
    }
    
    :public method "bgen rules" {
      {-name:substdefault {[namespace tail [self]]}}
      -start:required
      -includes
      rules
      args} {
      
      set tmpl {OPEG @name@ (@start@)
        @rules@
        END;}
      
      set mappings [list @name@ $name @start@ $start @rules@ $rules]
      set opegScript [string map \
                          $mappings \
                          $tmpl]
      
      return [:bgen script $opegScript {*}$args]
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
      return [list c [lindex $args 0 1]]
    }

    :method "input Command" {s e args} {
      # operates like Ident
      return [:input Ident $s $e]
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

      #
      # 3) Are there "fixes" (paths) to consider later on? This ressembles the
      # behavior in "input Definition" -> refactor?
      #
      set f [lindex $field 1]
      if {[info exists :choices]} {
        # puts stderr choices=${:choices}
        set c [lindex ${:choices} end]
        # puts stderr c=${:choices}
        # lappend f $c
        # puts stderr f=$f
        dict set :specs $ntIdent $c
        ## piggyback onto :spec
        unset :choices
      }
      # lappend :fields $f
      
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
      if {[llength $args] > 1 && [lindex $ctor 0] eq "c"} {
        # TODO: can there be more than one fix at a time? Test: dict
        # set spec generator [lrange $ctor 1 end]
        dict set spec generator [lindex $ctor 1]
        set args [lrange $args 1 end]
      }

      # TODO: Remove?
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
  
  nx::Class create ModelFactory {
    
    :variable sourcecode
    :variable callingNamespace
    
    :public method generate {generator asgmts} {
      
      # puts stderr GENERATE=|[list $generator new {*}$asgmts]|$asgmts|
      namespace eval ${:callingNamespace} [list $generator new {*}$asgmts]
    }

    #
    # TODO: the flow in postOrder must be consolidated and
    # streamlined; get rid of smelly LONG METHOD.
    #
    :public method postOrder {varName ast script {level 0}} {
      upvar [incr level] $varName var
      # puts stderr ast=$ast
      set ast [lassign $ast current start end]
      set childrenFlds [list]
      set fixes [list]
      if {[llength $ast]} {
        foreach c $ast {

          set kidz [:postOrder $varName $c $script $level]

          lassign $kidz pkg cArgs
          lassign $pkg cFields cFixes
          lappend childrenFlds {*}$cFields
          lappend fixes {*}$cFixes

          if {0} {
            puts -nonewline |cArgs=$cArgs|L(cArgs)=[llength $cArgs]|
            if {[info exists targs]} {
              puts targs=$targs|
            } else {
              puts "targs=|"
            }
          }
          lappend targs {*}$cArgs
        }
        
      } else {

        if {0 && $start > $end} {
          throw [list DJDSL OPEG INVALIDPARSE $start $end $ast] "Invalid parse detected."
        }
        
        set targs [string range ${:sourcecode} $start $end]

        set test [list $targs]
        if {$test ne $targs} {
          set targs $test
        }

      }

      # coalesce fields

      #
      # TODO: Irgh! merging with escaped (protected) single words ==
      # singelton lists produces an additional nesting level, each
      # time, so we have to drop that extra nesting level in a
      # postprocessing step (dict map)? can this be avoided?
      #

      set flds [dict create]
      if {[llength $childrenFlds]} {
        foreach {f v} $childrenFlds {
          if {![string is list $v] || [llength $v]==1} {
            dict lappend flds $f $v
          } else {
            dict lappend flds $f {*}$v
          }
        }
      }
      
      set flds [dict map {k v} $flds {
        if {[llength $v] == 1} {
          lindex $v 0
        } else {
          set v
        }
      }]

      # if {![info exists targs]} {
      #   set targs [string range ${:sourcecode} $start $end]
      # } else {
      # }
      
      lassign $current nt objspec
      

      # TODO: Can one get rid of NT-encoded field name resolution
      # (some reverse map _FIELD_* -> p1)? This introduces potential
      # conflicts between O/PEG Identifiers and NX variable/method
      # names, which are less restricted.
      
      if {[string first "_FIELD_" $nt] > -1} {
        set f [lindex [split $nt _] end]
        # dict lappend :fargs -$f $targs
        # dict lappend flds -$f {*}$targs
        # dict lappend flds -$f {*}$targs

        if {[llength $targs] == 1 && "$targs" ne "[list $targs]"} {
          set targs [lindex $targs 0]
        }

        if {$objspec ne ""} {
          lappend fixes [list $f [dict get $objspec generator] $targs]
        } else {
           dict set flds -$f $targs
         }

      } else {
        
        if {$objspec ne ""} {
          dict with objspec {      
            if {[info exists generator]} {
              # puts stderr "$generator new {*}$flds"
              set :current [:generate $generator $flds]
              set flds [list]
              if {[llength $fixes]} {
                lappend :fixes ${:current} $fixes
                set fixes [list]
              }
            }
          }
        }
      }

      set v ""

      set methodName [string map {// " "} $nt]
      if {[:info lookup method "input $methodName"] ne ""} {
        set v [:input {*}$methodName $start $end {*}$targs]
      } elseif {[info exists :current]} {
        set v ${:current}
      } else {
        # error "Either an objspec or a mapping method must be provided for non-terminal '$nt'."
        set v $targs
      }
      unset -nocomplain :current


      set var $v
      uplevel $level $script
      return [list [list $flds $fixes] $v]
      
    }

    :public method wireUp {cmds} {
      set changed [llength $cmds]
      while {$changed} {
        set later [list]
        set changed 0
        foreach cmd $cmds {
          try $cmd on error {e} {puts $e; lappend later $cmd} on ok {} {set changed 1}
        }
        set cmds $later
      }
      if {[info exists later] && [llength $later]} {
        return -code error "Unable to run fixes: $later"
      }
    }

    :public method getParse {args} {}

    :public method reset {args} {}
    
  }; # ModelFactory

  nx::Class create LanguageModelFactory -superclasses ModelFactory {
    :property -accessor public lm:object,type=::djdsl::lm::LanguageModel,required
    :variable context

    :public method init {} {
      set asset [namespace qualifiers ${:lm}]
      set :context [$asset new [string tolower [namespace tail ${:lm}]]]
    }
    :public method generate {generator asgmt} {
      if {[namespace tail [${:context} info class]] ne $generator} {
        # puts stderr "${:context} new [string tolower $generator] {*}$asgmt"
        ${:context} new [string tolower $generator] {*}$asgmt
      } else {
        if {[llength asgmt]} {
          ${:context} configure {*}$asgmt
        }
        return ${:context}
      }
    }
  }
  
  nx::Class create Builder -superclasses pt::rde::nx {

    :public method parse {script} {

      set ns [uplevel [current activelevel] namespace current]

      set ast [:parset $script]

      # TODO: Processing should happen in the factory, no calls back
      # and forth all the time! Remodel into a DefaultFactory with
      # getParse, reset etc.
      set list {}
      set factory [[:info class] factory get]
      $factory eval [list set :sourcecode $script]
      $factory eval [list set :callingNamespace $ns]
      # puts ast=$ast
      $factory postOrder v $ast {
        if {$v ne "" && [::nsf::object::exists $v]} {
          lappend list $v
        }
      }
      $factory eval {unset :sourcecode}

      if {[llength $list]} {
        set root [lindex $list end]
      } else {
        set root [$factory getParse]
        $factory reset
      }

      # TODO: relocate into factory object and turn it into a fixup
      # method as in Enso
      if {[$factory eval {info exists :fixes}]} {
        # expand fixes into commands
        set fldFixes [$factory eval {set :fixes}]
        foreach {obj fixes} $fldFixes {
          foreach fix $fixes {
            lassign $fix field path val
            if {[llength $path] > 1} {
              lassign $path objEl fieldEl valEl
              set lambda "$objEl $fieldEl get $valEl"
            } else {
              #
              # TODO: This is clearly disproportionate: Can be set
              # eagerly, and does not require apply call etc. Fix when
              # appropriate.
              #
              set lambda "return $path"
            }
            lappend fixCmds [list $obj eval ":configure -$field \[[list apply [list {0 root} $lambda] $val $root]\]"]
          }
        }
        # evaluate fixCmds
        $factory wireUp $fixCmds
      }

      unset -nocomplain :symStack; # TODO: relocate
      array unset -nocomplain :choices; # TODO: relocate
      $factory eval {unset -nocomplain :fixes}; # TODO: relocate
      
      return $root; # root
    }

    # TODO: make the symStack thingie more elegant.
    ## si:valuevalue_branch si:valuevoid_branch si:voidvalue_branch si:voidvoid_branch

    foreach m [[lindex [:info superclasses] end] info methods -callprotection all *_branch] {
      :method $m {} {
        # set mark [${:mystackmark} peek]; puts mark(insym)=$mark
        # set mark [${:mystackmark} size]; puts mark(insym)=$mark
        set mark [llength ${:symStack}]
        
        if {![info exists :choices($mark)]} {
          # init
          set :choices($mark) 0
        }
        try {set r [next]} on return {} {
          return -code return
        }; # ok
        incr :choices($mark); # puts stderr BUMP([lindex ${:symStack} end],$mark)
        return $r
      }
    }

    ## si:value_leaf_symbol_end si:void_leaf_symbol_end si:value_leaf_symbol_end si:value_clear_symbol_end si:reduce_symbol_end
    ## [[lindex [:info superclasses] end] info methods -callprotection all *_symbol_end]

    foreach m [[lindex [:info superclasses] end] info methods -callprotection all *_symbol_start] {
      :method $m {sym} {
        # push
        lappend :symStack $sym
        # puts stderr START([self],$sym)
        try {next} on return {} {set :symStack [lrange ${:symStack} 0 end-1]; return -code return}
      }
    }
    
    foreach m [[lindex [:info superclasses] end] info methods -callprotection all *_symbol_end] {
      :method $m {sym} {
        # si:value_symbol_start: pushes on AST stack & sets a mark
        # set mark [expr {[${:mystackmark} size]?[${:mystackmark} peek]:0}];
        # set mark [llength ${:mystackmark} size]

        set k [list [${:mystackloc} peek] $sym]
        set mark [llength ${:symStack}]
        set :symStack [lrange ${:symStack} 0 end-1]
        # puts stderr END($sym)
        next; # deletes the mark

        # puts stderr C($sym)=[array get :choices]
        if {${:myok}} {
          if {[info exists :choices($mark)]} {
            set idx [set :choices($mark)]
          } else {
            set idx 0
          }
          # unset -nocomplain :choices($mark)
          # inject the ctor
          # set ctors [[[:info class] generator get] eval {set :specs}]
          set ctors [[[:info class] generator get] getSpecs]
          # if {[string match _FIELD_* $sym]} {
          #   puts stderr "---FIELD($sym),$idx,$ctors"
          # }

          if {[dict exists $ctors $sym]} {
            set spec [lindex [dict get $ctors $sym] $idx]
            if {$spec ne ""} {
              set ast [${:mystackast} pop]
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
        unset -nocomplain :choices($mark)
      }
    }
  }; # Builder

  namespace export Parser BuilderGenerator ModelFactory Grammar LanguageModelFactory
  
} {

  #
  # == Doctests
  #

  # An examplary domain model (as an NX class model). This is a
  # variant of Fowler's state machine:

  nx::Class create StateMachine {
    :property -accessor public start:alnum; #object,type=StateMachine::State
    :property -accessor public states:object,type=State,1..*
  }
  
  nx::Class create State {
    :property -accessor public name
    :property -accessor public transitions:object,type=Transition,0..* {
      :public object method value=isSet {obj prop} {
        ::nsf::var::exists $obj $prop
      }
    }
  }

  nx::Class create Event {
    :property -accessor public name
    :property -accessor public code:alnum
  }
  
  nx::Class create Transition {
    :property -accessor public source:object,type=State
    :property -accessor public target:alnum
    :property -accessor public trigger:object,type=Event
  }

  #
  # A ```Grammar``` can be created based on this collection of OPEG
  # rules.
  #
  
  set grammar [Grammar create MissGrants -start M {
#// mgc4 //
      M  <- `StateMachine` START start:<alnum>+ states:S+ ;
      S  <- `State` STATE name:<alnum>+ transitions:T* ;
#// end //
#// mgc3 //
    T  <- `Transition` trigger:E GO target:<alnum>+ /
          `Transition` GO target:<alnum>+ trigger:E;
#// end //
#// mgc1 //
      E  <- `Event` ON name:<alnum>+ ;
void: ON <- WS 'on' WS;
#// end //
void: START <- WS 'start' WS;
void: STATE <- WS 'state' WS;
void: GO <- WS 'go' WS;
void: WS <- <space>+;
  }]

  #
  # From this ```Grammar``` instance, a parser is generated.
  #
  
  set parser1 [$grammar new]

  #
  # This parser is capable of processing (translating) input in the
  # defined concrete syntax into an instantiation of the language
  # model, without further action by the DSL developer:
  #
  
  set input {
#// mgc0 //
    start idle
    
    state idle
    	on doorClosed go active
    
    state active
        on lightOn go waitingForDrawer
        on drawerOpened go waitingForLight

    state waitingForDrawer
        on drawerOpened go unlockedPanel

    state unlockedPanel
        go idle on panelClosed

    state waitingForLight
#// end //
  }

  regsub -all -line {^\s*#//.*//\s*$} $input {} input
  
  set stateMachine [$parser1 parse $input]

  # Once can now navigate and further process the language-model
  # instantiation:

  ? {$stateMachine info class} ::StateMachine
  ? {$stateMachine start get} "idle"
  ? {llength [$stateMachine states get]} 5

  #
  # === Some Highlights
  #
  # Instantiation and assignment generators can be located in
  # different rules, to structure the syntax definition in a
  # convenient or necessary manner, or to make assignment generators
  # applicable for the scope of more than just one assignment operator.

  set opeg {
  #// mgc5 //
      M  <- `StateMachine` START start:<alnum>+ states:S states:S* ;
      S  <- `State` STATE name:<alnum>+ TRANS? ;
  TRANS  <- transitions:T TRANS*;
  #// end //
      T  <- `Transition` trigger:E GO target:<alnum>+ ;
  #// mgc2 //
      E  <- `Event` ON NAME ;
   NAME  <- name:<alnum>+;
  #// end //
     ON  <- WS 'on' WS;
   START <- WS 'start' WS;
   STATE <- WS 'state' WS;
      GO <- WS 'go' WS;
void: WS <- <space>+;}

  set grammar [Grammar new -name MissGrants -start M {
#// mgc5 //
      M  <- `StateMachine` START start:<alnum>+ states:S states:S* ;
      S  <- `State` STATE name:<alnum>+ TRANS? ;
  TRANS  <- transitions:T TRANS*;
#// end //
      T  <- `Transition` trigger:E GO target:<alnum>+ ;
#// mgc2 //
      E  <- `Event` ON NAME ;
      NAME  <- name:<alnum>+;
#// end //
void:     ON  <- WS 'on' WS;
void:   START <- WS 'start' WS;
void:   STATE <- WS 'state' WS;
void:   GO <- WS 'go' WS;
void: WS <- <space>+;
    }]

  set parser [$grammar new]
  
  set stateMachine [$parser parse {
    start idle
    
    state idle
    on doorClosed go active

    state waitingForLight
  }]

  ? {$stateMachine info class} ::StateMachine
  ? {$stateMachine start get} "idle"
  ? {llength [$stateMachine states get]} 2
  ? {llength [[lindex [$stateMachine states get] 0] transitions get]} 1
  # ? {llength [[lindex [$stateMachine states get] 1] transitions get]} 
  ? {[[[lindex [$stateMachine states get] 0] transitions get] trigger get] name get} "doorClosed"


  StateMachine property -accessor public start:object,type=State
  StateMachine property -accessor public states:object,type=State,1..* {
    :public object method value=set {obj prop value} {
      foreach s $value {
        $obj eval [list dict set :$prop [$s name get] $s]
      }
      
    }
    :public object method value=get {obj prop stateName} {
      set states [next [list $obj $prop]]
      dict get $states $stateName
    }
  }

  set grammar [Grammar new -name MissGrants -start M {
#// mgc7 //
       M <- `StateMachine` START start:(`$root states $0` <alnum>+)
            states:S+ ;
#// end //
       S <- `State` STATE name:<alnum>+ TRANS? ;
   TRANS <- transitions:T TRANS*;
       T <- `Transition` trigger:E GO target:<alnum>+ ;
       E <- `Event` ON NAME ;
    NAME <- name:<alnum>+;
      ON <- WS 'on' WS;
   START <- WS 'start' WS;
   STATE <- WS 'state' WS;
      GO <- WS 'go' WS;
void: WS <- <space>+;
     }]
               
  set parser [$grammar new]

  set input {
  #// mgc6 //  
    start idle
    state idle
  #// end //
  }

  regsub -all -line {^\s*#//.*//\s*$} $input {} input

  set stateMachine [$parser parse $input]

  ? {$stateMachine info class} ::StateMachine
  ? {[$stateMachine start get] info class} ::State
  ? {[$stateMachine start get] name get} "idle"
  
  #
  # An examplary domain model (as an NX class model)
  #

  nx::Class create ::Binary {
    :property -accessor public lhs:object,type=::Const
    :property op
    :property -accessor public rhs:object,type=::Const
  }
  
  nx::Class create ::Const {
    :property value
  }
  
  #
  # A corresponding Object Parsing-Expression Grammar (OPEG)
  #
  
  set g {
      Term        <- `Binary` lhs:Prim ' '* op:AddOp ' '* rhs:Prim / Prim      ;
      Prim        <- `Const` value:Num					       ;
leaf: Num         <- Sign? Digit+                      			       ;
      Digit       <- '0'/'1'/'2'/'3'/'4'/'5'/'6'/'7'/'8'/'9'		       ;
      Sign        <- '-' / '+'                                 		       ;
      AddOp       <- '+' / '-'                                 		       ;
  }

  #
  # An instance of ```Grammar``` is provided the OPEG and,
  # optionally, a custom ```ModelFactory``` to generate a combined
  # parser + builder for this OPEG.
  #
  
  set grammar [Grammar new -name Calculator -start Term $g]
  set parser [$grammar new]

  #
  # The method ```parse``` can be used to submit input into the
  # parsing pipeline.  The output, on success, is a valid
  # instantiation of the language model.
  #

  set rObj [$parser parse {1+2}]
  
  ? {$rObj info class} ::Binary
  ? {[$rObj lhs get] info class} ::Const
  ? {[$rObj lhs get] cget -value} 1
  ? {[$rObj rhs get] info class} ::Const
  ? {[$rObj rhs get] cget -value} 2
  ? {$rObj cget -op} "+"

 
  set rObj [$parser parse {5}]
  ? {$rObj info class} ::Const
  ? {$rObj cget -value} "5"

  set rObj [$parser parse {-0}]
  ? {$rObj info class} ::Const
  ? {$rObj cget -value} "-0"

  set rObj [$parser parse {4-3}]

  ? {$rObj info class} ::Binary
  ? {[$rObj lhs get] info class} ::Const
  ? {[$rObj lhs get] cget -value} 4
  ? {[$rObj rhs get] info class} ::Const
  ? {[$rObj rhs get] cget -value} 3
  ? {$rObj cget -op} "-"

  #
  # One may refine (or, entirely override) the built-in
  # object-construction logic by providing a custom factory.
  #

  nx::Class create CalculatorFactory -superclasses ModelFactory {
    :method "input AddOp" {startIdx endIdx args} {
      return [string range ${:sourcecode} $startIdx $endIdx];
    }
  }

  # The custom factory is then passed to the construction call for a
  # parser.
  
  set grammar [Grammar new -name Calculator -start Term $g]
  set parser [$grammar new -factory [CalculatorFactory new]]

  #
  # Another examplary domain model (as an NX class model)
  #
  
  nx::Class create ::Point {
    :property x:integer
    :property y:integer
  }


  set g2a {
    OPEG Coordinate (P)
    P           <- `Point` '(' x:Digit1 ',' y:Digit2 ')';
    leaf:  Digit2       <- '0'/'1'/'2'/'3'/'4'/'5'/'6'/'7'/'8'/'9';
    leaf:  Digit1       <- <digit>+;
    END;}

  set coordGrammar [Grammar newFromScript $g2a]
  ? {$coordGrammar name get} "Coordinate"
  set coordBuilder [$coordGrammar new]

  ? {[$coordBuilder parse {(11,2)}] info class} ::Point
  ? {[$coordBuilder parse {(3,4)}] cget -y} 4


  #
  # An alternative grammar, mapping to the same language model.
  #
  
  
  set coordGrammar [Grammar newFromScript {
  OPEG Coordinate (P)
		XY          <- x:Digit ',' y:Digit;
       		P           <- `Point` '(' XY ')';
	 leaf:  Digit       <- <digit>+;
    END;
  }

   ]
  set coordBuilder [$coordGrammar new]
  
  ? {[$coordBuilder parse {(1,2)}] info class} ::Point
  ? {[$coordBuilder parse {(3,4)}] cget -y} 4

  #
  # Yet another grammar, mapping to the same language model. The
  # alternatives demonstrate how +assignment generators+ (i.e., +x+
  # and +y+) can be distributed across different non-terminals level,
  # still yielding the same instantiation.
  #
 

  set coordGrammar [Grammar newFromScript {
    OPEG Coordinate (P)
    	P           <- `Point` '(' XY ')';
    	XY          <- A ',' B;
    	A           <- x:Digit;
    	B           <- y:Digit;
 leaf:  Digit       <- <digit>+;
    END;
  }]
  set coordBuilder [$coordGrammar new]
  
  ? {[$coordBuilder parse {(1,2)}] info class} ::Point
  ? {[$coordBuilder parse {(3,4)}] cget -y} 4

  #
  # == DSL extension (ex.: DOT definition of graphs)
  #

  #
  # === Abstract syntax
  #

  package req djdsl::lm
  
  namespace import ::djdsl::lm::*


  Asset create Graphs {
    
    LanguageModel create Graph {
      :property name:alnum
      :property -incremental edges:object,type=Graph::Edge,0..n
      :property -accessor public nodes {
        :public object method value=get {obj prop name:optional} {
          set nodes [next [list $obj $prop]]
          if {[info exists name]} {
            dict get $nodes $name
          } else {
            return [dict values $nodes]
          }
        }
        :public object method value=isSet {obj prop name} {
          set nodes [next [list $obj $prop]]
          dict exists $nodes $name
        }

        :public object method value=set {obj prop name value} {
          $obj eval [list dict set :$prop $name $value]
        }
      }

      :public method init {} {
        if {![info exists :nodes]} {
          set :nodes [dict create]
        }
      }

      :public method mkNode {classifier -name args} {
        if {[:nodes isSet $name]} {
          :nodes get $name
        } else {
          :nodes set $name [set r [$classifier new -childof [self] -name $name {*}$args]]
        }
        return $r
      }
      
      Classifier create Node {
        :property -accessor public name:required

        # :public object method new {-name -childof args} {
        #  if {[$childof nodes isSet $name]} {
        #    $childof nodes get $name
        #  } else {
        #    # puts stderr "[current nextmethod] [list -name $name -childof $childof {*}$args]"
        #    # TODO: Why is passing -childof not working here? 
        #    $childof nodes set $name [set r [next [list -name $name {*}$args]]]
        #  }
        # }
        
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

  set dotGrammar [Grammar newFromScript {
    OPEG Dot (G)
    #// gpl3Node //  
    G           <- `Graph` GRAPH OBRACKET StmtList CBRACKET;
    StmtList    <- (Stmt SCOLON)*;
    Stmt        <- edges:EdgeStmt / NodeStmt;
    #// end //  
    #// gpl2Node //  
    EdgeStmt    <- `Edge` a:(`$root nodes $0` NodeID) EDGEOP
                          b:(`$root nodes $0` NodeID);
    #// end //  
    #// gpl1Node //  
    NodeStmt    <- `Node` name:NodeID;
    NodeID      <- QUOTE Id QUOTE;
    Id          <- !QUOTE (<space>/<alnum>)+;
    #// end //
    void:  QUOTE    <- '\"';
    void:  EDGEOP   <- WS '--' WS ;
    void:  NODE     <- WS 'node' WS ;
    void:  GRAPH    <- WS 'graph' WS ;
    void:  OBRACKET <- WS '{' WS ;
    void:  CBRACKET <- WS '}' WS;
    void:  SCOLON   <- WS ';' WS;
    void:  WS       <- (COMMENT / <space>)*;
    void:  COMMENT  <- '//' (!EOL .)* EOL ;
    void:  EOL      <- '\n' / '\r' ;

    END;
  }]

  set lmf [LanguageModelFactory new \
               -lm [namespace current]::Graphs::Graph]
  
  set dotParser [$dotGrammar new -factory $lmf]
  set str {
    graph {
      // node definitions
      "1st Edition";
      "2nd Edition";
      "3rd Edition";
      // edge definitions
      "1st Edition" -- "2nd Edition";
      "2nd Edition" -- "3rd Edition";
    }
  }

  set graph [$dotParser parse $str]

  ? {$graph info class} ::Graphs::Graph
  ? {llength [$graph edges get]} 2
  ? {llength [$graph nodes get]} 3

  set str2 {
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

  regsub -all -line {^\s*#//.*//\s*$} $str2 {} str2

  set extDotGrammar {
    #// gplWeighted1 //  
    EdgeStmt    <- `Edge` a:(`$root nodes $0` NodeID) EDGEOP
                          b:(`$root nodes $0` NodeID) WeightAttr?;
    WeightAttr  <- OSQBRACKET WEIGHT EQ weight:Weight CSQBRACKET;
    Weight      <- `Weight` value:<digit>+;
    #// end //
    
    void: WEIGHT      <- WS 'weight' WS;
    void: EQ          <- WS '=' WS;
    void: OSQBRACKET  <- WS '\[' WS;
    void: CSQBRACKET  <- WS '\]' WS;
    void: COMMA       <- WS ',' WS;
  }

  #// gplExt1 //  
  Grammar create ExtDot \
      -start G \
      -merges $dotGrammar \
      $extDotGrammar
  #// end //  

  # TODO: not working
  # ODot loadRules  $extDotGrammar
  
  # AttrList    <- OSQBRACKET (Attr COMMA)* CSQBRACKET;
  # Attr       <- !CSQBRACKET 
   
  Composition create WeightedGraphs \
      -binds Graphs \
      -base [Graphs::Graph] \
      -features [Graphs::weighted]

  set lmf [LanguageModelFactory new \
               -lm [namespace current]::WeightedGraphs::Graph]
  
  set oDotParser [ExtDot new -factory $lmf]
  
  set wgraph [$oDotParser parse $str2]

  ? {$wgraph info class} ::WeightedGraphs::Graph
  ? {llength [$wgraph edges get]} 2
  ? {llength [$wgraph nodes get]} 3

  ? {[lindex [$wgraph edges get] 0] info class} ::WeightedGraphs::Graph::Edge
  ? {[lindex [$wgraph nodes get] 0] info class} ::WeightedGraphs::Graph::Node

  ? {[[lindex [$wgraph edges get] 0] cget -weight] cget -value} 5
  ? {[[lindex [$wgraph edges get] 1] cget -weight] cget -value} 10


  set oDotGrammar2 [Grammar new -name ODot2 -start G -merges $dotGrammar {
    #// gplWeighted2a //
    # a) receiving rules
    EdgeStmt    <- `Edge` CoreEdge WeightAttr ;
    WeightAttr  <- OSQBRACKET WEIGHT EQ weight:Weight CSQBRACKET;
    Weight      <- `Weight` value:<digit>+;
    #// end //
    
    void: WEIGHT      <- WS 'weight' WS;
    void: EQ          <- WS '=' WS;
    void: OSQBRACKET  <- WS '\[' WS;
    void: CSQBRACKET  <- WS '\]' WS;
    void: COMMA       <- WS ',' WS;
  } {
    #// gplWeighted2b //
    # b) transforms
    CoreEdge      <-> Dot::EdgeStmt
    G             <*> Dot::G
    {EdgeStmt end} ==>
    #// end //
  }]

  # TODO: early deletion not working. why?
  #
  # CoreEdge      <-= Dot::EdgeStmt
  # Dot::EdgeStmt ==>
  # G             <*> Dot::G

  set lmf [LanguageModelFactory new \
               -lm [namespace current]::WeightedGraphs::Graph]

  
  set oDotParser2 [$oDotGrammar2 new -factory $lmf]
  set wgraph2 [$oDotParser2 parse $str2]

  ? {$wgraph2 info class} ::WeightedGraphs::Graph
  ? {llength [$wgraph2 edges get]} 2
  ? {llength [$wgraph2 nodes get]} 3

  ? {[lindex [$wgraph2 edges get] 0] info class} ::WeightedGraphs::Graph::Edge
  ? {[lindex [$wgraph2 nodes get] 0] info class} ::WeightedGraphs::Graph::Node

  ? {[[lindex [$wgraph2 edges get] 0] cget -weight] cget -value} 5
  ? {[[lindex [$wgraph2 edges get] 1] cget -weight] cget -value} 10

  #
  # Allow for mixed graphs ...
  #
 
  $oDotGrammar2 loadTransforms {
    CoreEdge      <-> Dot::EdgeStmt
    G             <*> Dot::G
    # {EdgeStmt end} ==>
  }

  set lmf [LanguageModelFactory new \
               -lm [namespace current]::WeightedGraphs::Graph]

  set oDotParser3 [$oDotGrammar2 new -factory $lmf]

  set wgraph3 [$oDotParser3 parse {
    graph {
      // node definitions
      "1st Edition";
      "2nd Edition";
      "3rd Edition";
      // edge definitions
      "1st Edition" -- "2nd Edition";
      "2nd Edition" -- "3rd Edition" [weight = 2];
    }
  }]
  
  #
  # == DSL unification
  #
  # === Abstract syntax
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
  
  Asset create Behaviours {
    LanguageModel create StateMachine {
      :property -accessor public start; # object,type=StateMachine::State
      :property -accessor public states:object,type=StateMachine::State,1..*
      
      Classifier create State {
        :property -accessor public name
        :property -accessor public actions:0..*,object,type=Command
        
        :property -accessor public transitions:object,type=Transition,0..* {
          :public object method value=isSet {obj prop} {
            ::nsf::var::exists $obj $prop
          }
        }
      }
        
      Classifier create Transition {
        :property -accessor public source; # object,type=State
        :property -accessor public target; # object,type=State
        :property -accessor public trigger; # object,type=Event
      }
      
      Classifier create AbstractEvent {
        :property -accessor public name
        :property -accessor public code:alnum
      }
      
      Classifier create Event -superclasses AbstractEvent
      Classifier create Command -superclasses AbstractEvent
    }; # StateMachine
  }; # Behaviours
  
  set StateMachine [Behaviours info children -type LanguageModel]
  
  Composition create EvaluableExpr \
      -binds Expressions \
      -base [Expressions::Model] \
      -features [Expressions::Eval]
  
  # === Concrete syntax
  #
  # For Miss Grant's controller, the previouly defined OPEG is reused
  # as-is. For BCEL, the following OPEG will be used:
  #
  # operator:NotOp leftExpr:Term /
  # =, <>, >=, <=, >, <, &&, ||
  Grammar create BCEL -start Expression {
    #// bcel1 //
    Expression     		<- `BooleanOrComparison` leftExpr:Term BinaryOp rightExpr:Term;
    Term		 	<-  Variable / OPARENS Expression CPARENS;
    BinaryOp 		        <-  WS operator:('||' / '&&' / '=' / '<>' / '>=' / '<=' / '>' / '<') WS;
    leaf: NotOp 		<-  WS operator:'-' WS;
    Variable 		        <- `VariableRef` variableName:<alnum>+;
    void: OPARENS 		<- WS '(' WS;
    void: CPARENS 		<- WS ')' WS;
    void: WS		        <- <space>*;
    #// end //
  }

  set lmf [LanguageModelFactory new \
               -lm [namespace current]::Expressions::Model]
  
  set bcelParser [BCEL new -factory $lmf]
  set expr [$bcelParser parse {(counter > 3) && (counter < 10)}]
  
  ? {$expr info class} ::Expressions::Model::BooleanOrComparison
    ? {$expr cget -operator} "&&"

  Asset create GuardedBehaviours {
    Collaboration create StateMachine {
      Role create Transition {
        :property -accessor public guard:object;# ,type=EvaluableExpr::Model::Expression
      }
    }
  }
    
  Composition create GuardableStateMachine \
      -binds {Behaviours Expressions} \
      -base $StateMachine \
      -features [list [GuardedBehaviours::StateMachine] [namespace current]::EvaluableExpr::Model]
  

   set grammar [Grammar create MissGrants2 -start M {
     M  <- `StateMachine` START start:<alnum>+ states:S+ ;
     S  <- `State` STATE name:<alnum>+ transitions:T* ;
     T  <- `Transition` trigger:E GO target:<alnum>+ WS;
     E  <- `Event` ON name:<alnum>+ ;
void: ON <- WS 'on' WS;
void: START <- WS 'start' WS;
void: STATE <- WS 'state' WS;
void: GO <- WS 'go' WS;
void: WS <- <space>*;
  }]
  

# TODO: Why GuardedMGC//WS <- / {* space} {* space} {* space} {* space} {* space}?

  
  set unifiedGrm [Grammar new -name GuardedMGC -start GM -merges [list [namespace current]::BCEL [namespace current]::MissGrants2] {
    T  <- `Transition` OrigT OBRACKET guard:Expression CBRACKET;
    void: OBRACKET <- WS '\[' WS;
    void: CBRACKET <- WS '\]' WS;
    void: IF <- WS 'if';
  } {
    OrigT      <->  MissGrants2::T
    Expression <==  BCEL::Expression
    GM         <*>  MissGrants2::M
    # {T end}    ==>
  }]
  
  set input {
    start idle
      
    state idle
    on doorClosed go active
    
    state active
       on lightOn go waitingForDrawer
       on drawerOpened go waitingForLight [counter > 3]

    state waitingForDrawer
       on drawerOpened go unlockedPanel

    state unlockedPanel
       on panelClosed go idle
      
    state waitingForLight
  }
  
  set lmf [LanguageModelFactory new \
               -lm [namespace current]::GuardableStateMachine::StateMachine]
  
  set unifiedParser [$unifiedGrm new -factory $lmf]
  set gSm [$unifiedParser parse $input]
  ? {$gSm info class} ::GuardableStateMachine::StateMachine
  ? {llength [lmap t [$gSm info children -type GuardableStateMachine::StateMachine::Transition] {if {![$t eval {info exists :guard}]} {continue}}]} 1
    

  #
  # == Debugging
  #
  # To turn on debugging, add a ```package req debug ``` early in this
  # script (at the top), and mark a script range using:
  #
  # [source,tcl]
  # --------------------------------------------------
  # debug on pt/engine
  # # your script under debugging
  # debug off pt/engine
  # --------------------------------------------------


  #
  # == Varia
  # 

  #
  # === Plain parsing grammar: w/o generators, with custom factory
  #

  set dotPeg [Grammar newFromScript {
    OPEG Dot (G)
    #// pure1 //  
    G           <- GRAPH OBRACKET StmtList CBRACKET;
    StmtList    <- (Stmt SCOLON)*;
    Stmt        <- EdgeStmt / NodeStmt;
    EdgeStmt    <- NodeID EDGEOP NodeID;
    NodeStmt    <- NodeID;
    NodeID      <- QUOTE Id QUOTE;
    Id          <- !QUOTE (<space>/<alnum>)+;
    #// end //  
    void:  QUOTE    <- '\"';
    void:  EDGEOP   <- WS '--' WS ;
    void:  NODE     <- WS 'node' WS ;
    void:  GRAPH    <- WS 'graph' WS ;
    void:  OBRACKET <- WS '{' WS ;
    void:  CBRACKET <- WS '}' WS;
    void:  SCOLON   <- WS ';' WS;
    void:  WS       <- (COMMENT / <space>)*;
    void:  COMMENT  <- '//' (!EOL .)* EOL ;
    void:  EOL      <- '\n' / '\r' ;

    END;
  }]

  set dotParser [$dotPeg new -factory [set mf [ModelFactory new {
    :object property -accessor public result

    :public object method "input EdgeStmt" {s e node1 node2} {
      dict lappend :result edges [list $node1 $node2]
      return
    }

    # :public object method "input EdgeStmt" {s e args} {
    #   dict lappend :result edges $args
    #   return
    # }
    
    :public object method "input NodeStmt" {s e node} {
      dict lappend :result nodes $node
      return
    }

    # :public object method "input NodeStmt" {s e args} {
    #   dict lappend :result nodes {*}$args
    #   return
    # }
    
    :public object method getParse {} {
      return ${:result}
    }
    :public object method reset {} {
      unset -nocomplain :result
    }
  }]]]

  ? {$dotParser parse $str} [list \
                                 nodes {{1st Edition} {2nd Edition} {3rd Edition}} \
                                 edges {{{1st Edition} {2nd Edition}} {{2nd Edition} {3rd Edition}}}]

  #
  # === Anticipated extension
  #

  set extensibleDotGrm [Grammar newFromScript {
    OPEG Dot (G)
    G           <- `Graph` GRAPH OBRACKET StmtList CBRACKET;
    StmtList    <- (Stmt SCOLON)*;
    Stmt        <- edges:EdgeStmt / NodeStmt;
    #// proactive1 //
    EdgeStmt    <- `Edge` a:(`$root nodes $0` NodeID) EDGEOP
                          b:(`$root nodes $0` NodeID) AttrList?;
    AttrList    <- OSQBRACKET Attr (SCOLON Attr)* CSQBRACKET;
    #// end //
    NodeStmt    <- `Node` name:NodeID;
    NodeID      <- QUOTE Id QUOTE;
    Id          <- !QUOTE (<space>/<alnum>)+;

    AttrList    <- OSQBRACKET Attr (SCOLON Attr)* CSQBRACKET;
    
    void:  QUOTE    <- '\"';
    void:  EDGEOP   <- WS '--' WS ;
    void:  NODE     <- WS 'node' WS ;
    void:  GRAPH    <- WS 'graph' WS ;
    void:  OBRACKET <- WS '{' WS ;
    void:  CBRACKET <- WS '}' WS;
    void:  SCOLON   <- WS ';' WS;
    void:  WS       <- (COMMENT / <space>)*;
    void:  COMMENT  <- '//' (!EOL .)* EOL ;
    void:  EOL      <- '\n' / '\r' ;

    END;
  }]

  set oDotGrammar3 [Grammar new -name ODot3 -start G -merges $extensibleDotGrm {
    #// proactive2a //
    # a) receiving rules
    Attr        <-  weight:Weight ;
    Weight      <- `Weight` WEIGHT EQ value:<digit>+;
    #// end //
    
    void: WEIGHT      <- WS 'weight' WS;
    void: EQ          <- WS '=' WS;
    void: OSQBRACKET  <- WS '\[' WS;
    void: CSQBRACKET  <- WS '\]' WS;
  } {
    #// proactive2b //
    # b) transforms
    G             <*> Dot::G
    #// end //
  }]

  set lmf [LanguageModelFactory new \
               -lm [namespace current]::WeightedGraphs::Graph]

  
  set oDotParser3 [$oDotGrammar3 new -factory $lmf]
  set wgraph3 [$oDotParser3 parse $str2]

  ? {$wgraph3 info class} ::WeightedGraphs::Graph
  ? {llength [$wgraph3 edges get]} 2
  ? {llength [$wgraph3 nodes get]} 3

  ? {[lindex [$wgraph3 edges get] 0] info class} ::WeightedGraphs::Graph::Edge
  ? {[lindex [$wgraph3 nodes get] 0] info class} ::WeightedGraphs::Graph::Node

  ? {[[lindex [$wgraph3 edges get] 0] cget -weight] cget -value} 5
  ? {[[lindex [$wgraph3 edges get] 1] cget -weight] cget -value} 10
  
  # TODO: is this also working?
  # set g2 {
  # OPEG Coordinate (P)
  #   P           <- @Point '(' x:<digit> ',' y:<digit> ')';
  #   Digit       <- '0'/'1'/'2'/'3'/'4'/'5'/'6'/'7'/'8'/'9'      ;
  # END;}

  # set g1 {
  # PEG Coordinate (DigitPairs)
  #    DigitPairs  <-  Digit (',' DigitPairs)?;
  #    Digit       <- <digit> <digit>;
  # END;}


  set g1 {
    PEG Coordinate (P)
    P            <- OPENP Digit+ ',' Digit CLOSEP;
    void: OPENP  <- '(';
                      void: CLOSEP <- ')';
    Digit        <- <digit> <digit>;
    END;}

  set coordParser [[pt::rde::nx pgen $g1] new]
  # $coordParser print {(11,22)}
  puts stderr [$coordParser parset {(11,22)}]
}


# Local variables:
#    mode: tcl
#    tcl-indent-level: 2
#    indent-tabs-mode: nil
# End:
