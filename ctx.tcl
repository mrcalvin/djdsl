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

    package req djdsl::lm
    namespace import ::djdsl::lm::*


    nx::Object create context {
      set :frames [list]

      :require namespace
      namespace eval [self] {
        namespace path {}
      }
      
      :public object method set {next element validators} {

        set newFrame [list $next $element $validators 0]
        set :frames [linsert ${:frames}[set :frames {}] 0 $newFrame]
        
      }
      :public object method clear {} {
        set :frames [lassign ${:frames} currentFrame]
        return [lindex $currentFrame end]
        
      }
      :public object method original args {
        # peek current frame
        set currentFrame [lindex ${:frames} 0]
        lassign $currentFrame next element validators counter
        
        incr counter
        # puts stderr "EXPLICIT($counter) $next validate $element $validators"
        try {
          if {${next} ne ""} {
            ${next} validate ${element} ${validators}
          }
          return 1
        } trap {DJDSL CTX VIOLATED} {e opts} {
          return 0
        } on error {e opts} {
          return -options $opts $e
        } finally {
          lset currentFrame 3 $counter
          lset :frames 0 $currentFrame
        }
      }
      interp alias {} [self]::next {} [self] original
    }

    nx::Class create Condition {
      :property label
      :property -accessor public bodyExpression:required
      :property {expressionType "tcl"}
      :property context:object,type=AssetElement
    }
    
    AssetElement property \
	-accessor public \
	-incremental \
	condition:0..*,object,type=[namespace current]::Condition

    AssetElement protected method compileScript {} {
      set f ""

      # add "basic" constraints
      set varSlots [:info variables]
      foreach vs $varSlots {
        set spec [$vs parameter]
        set options [::nx::MetaSlot parseParameterSpec {*}$spec]
        set name [lindex $options 0]
        set options [lindex $options end]

        if {[llength $spec] == 2} {
          set exprStr "\[info exists :$name\]"
          set thenScript [list return -level 0 -code error \
                              -errorcode [list DJDSL CTX VIOLATED $vs] \
                              "condition '$exprStr' failed"]
          append f [list if !($exprStr) $thenScript] \;
        }
        
        # Add checks for multi-valuedness == list
        
        if {[$vs eval {:isMultivalued}]} {
          set exprStr "\[::string is list \${:$name}\]"
          set thenScript [list return -level 0 -code error \
                              -errorcode [list DJDSL CTX VIOLATED $vs] \
                              "condition '$exprStr' failed"]
          append f [list if !($exprStr) $thenScript] \;
        }
        
        if {$options ne ""} {
          set nspec [::nx::MetaSlot optionsToValueCheckingSpec $options]
          set exprStr "\[::nsf::is $nspec \${:$name}\]"
          set thenScript [list return -level 0 -code error \
                              -errorcode [list DJDSL CTX VIOLATED $vs] \
                              "condition '$exprStr' failed"]
          append f [list if !($exprStr) $thenScript] \;
        }
             
      # TODO: provided that type is of type "AssetElement", check
      # also there constraints?

      }

      if {[info exists :condition] && [llength ${:condition}]} {
        foreach c ${:condition} {
          set exprStr [$c bodyExpression get]
          set thenScript [list return -level 0 -code error \
                              -errorcode [list DJDSL CTX VIOLATED $c] \
                              "condition '$exprStr' failed"]
          append f [list if !($exprStr) $thenScript] \;
        }
      }
      
      if {$f ne "" && ![info complete $f]} {
        throw [list DJDSL CTX FAILED SCRIPT [self] $f] "Validation script is not complete."
      }
      
      return $f
    }
    
    AssetElement public method validate {-or:switch e:object validators:optional} {

      if {![info exists validators]} {
        set ancestors [$e info precedence]
        if {[self] ni $ancestors} {
          throw [list DJDSL CTX FAILED ANCESTRY [self] $e] \
              "Not allowed: '[self]' is not in the refinement chain '$ancestors'"
        }
        # Skip forward to [self] as first validator
        # ::djdsl::ctx::test::Graphs::Graph', got an instance of '::nsf::__#0T::Graph
        # puts start=[self],[lsearch -exact $ancestors [self]]
        set validators [lrange $ancestors [lsearch -exact $ancestors [self]] end]
        set validators [lassign $validators assetElement]
        # puts assetElement=$assetElement,next=$validators
      }
      
      set explicitNexts 0
      set validators [lassign $validators next]
      if {$next ne "" && ![$next info has type [current class]]} {
        set next ""
      }
      
      set f [:compileScript]
      if {$f ne ""} {
        try {
          # puts stderr "([self]) ::djdsl::ctx::context set $next $e $validators"
          ::djdsl::ctx::context set $next $e $validators
          # puts stderr "[list apply [list {} $f ::djdsl::ctx::context]]"
          $e eval [list apply [list {} $f ::djdsl::ctx::context]]
        } trap {DJDSL CTX VIOLATED} {errMsg opts} {
          # propagate violation
          if {!$or || $next eq ""} {
            dict with opts {lappend -errorcode $e}
            return -options $opts $errMsg
          }
        } trap {} {errMsg opts} {
          # wrap any other error report
          puts opts=$opts
          throw {DJDSL CTX FAILED EXPR} $errMsg
        } finally {
          set explicitNexts [::djdsl::ctx::context clear]
        }
      }

      # puts stderr "+++++ explicits? $explicitNexts"
      if {!$explicitNexts && $next ne ""} {
        $next validate -or=$or $e $validators
      }
      
      return
    }

    AssetElement public method isValid {-or:switch e:object} {
      try {
        :validate -or=$or $e
        return 1
      } trap {DJDSL CTX VIOLATED} {e opts} {
        return 0
      } on error {e opts} {
        return -options $opts $e
      }
    }

    Collaboration public method validate {-or:switch e:object args} {
      set self [self]
      next
      if {![llength $args]} {
        foreach el [$e info children] {
          # TODO: -type filter for "info precedence"?
          set cl ${self}::[[$el info class] info name]
          # puts cl($self)=$cl,[$el info class]
          if {[::nsf::is object $cl] && [$cl info has type ::djdsl::lm::AssetElement]} {
            $cl validate -or=$or $el
          }
        }
      }
    }
    
    # Asset public method validate {-or:switch e:object} {
    #   # TODO: -type filter for "info precedence"?
    #   set nextValidators [lassign [$e info precedence] assetElement]
    #   if {[$assetElement info has type AssetElement]} {
    #     $assetElement validate -or=$or $e $nextValidators
    #   } else {
    #     throw {DJDSL CTX UNSUPPORTED $e} \
    #         "Validation is not supported for '$assetElement' instance."
    #   }
    # }
    
    namespace export Condition
  } {
  
  
}

# Local variables:
#    mode: tcl
#    tcl-indent-level: 2
#    indent-tabs-mode: nil
# End:
#
