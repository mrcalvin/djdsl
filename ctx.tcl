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

    package req djdsl::lm
    namespace import ::djdsl::lm::*

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

    AssetElement public method validate {e:object} {
      set f ""
      set validated 0
      if {[info exists :condition] && [llength ${:condition}]} {
        foreach c ${:condition} {
          set exprStr [$c bodyExpression get]
          append f [list if !($exprStr) [list return -code error -errorcode [list DJDSL CTX VIOLATED $c] "condition '$exprStr' failed"]] \;
        }
        
        # puts $f
        if {$f eq ""} {
          set validated 1
        } else {

          if {![info complete $f]} {
            throw [list DJDSL CTX FAILED SCRIPT $f] "Validation script is not complete."
          }
        
          try {
            $e eval $f
            set validated 1
          } trap {DJDSL CTX VIOLATED} {e opts} {
            # propagate violation
            return -options $opts $e
          } trap {} {e opts} {
            # wrap any other error report
            throw {DJDSL CTX FAILED EXPR} $e
          }
        }
      } else {
        set validated 1
      }
      set n [next]
      expr {$validated && ($n eq "" ? 1 : $n)}
    }

    Asset public method validate {e:object} {
      # instances of AssetElement instantiations
      set assetElement [e info class]
      if {[$assetElement] info has type AssetElement} {
        $assetElement validate $e
      } else {
        throw {DJDSL CTX UNSUPPORTED $e} "Validation is not supported for '$assetElement' instance."
      }
    }

    namespace export Condition
} {
  
  
}

# Local variables:
#    mode: tcl
#    tcl-indent-level: 2
#    indent-tabs-mode: nil
# End:
#
