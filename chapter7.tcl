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
  
} {
  
  #
  # === An initial internal DSL syntax for basic graphs
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
  
}


# Local variables:
#    mode: tcl
#    tcl-indent-level: 2
#    indent-tabs-mode: nil
# End:
