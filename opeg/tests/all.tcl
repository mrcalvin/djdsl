package req Tcl 8.6
package require tcltest 2.2

::tcltest::configure -singleproc 1 -testdir \
        [file dirname [file normalize [info script]]]
::tcltest::configure {*}$argv

::tcltest::loadTestedCommands
::tcltest::runAllTests
