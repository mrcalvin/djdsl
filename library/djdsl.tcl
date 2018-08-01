apply {{args} {
    set script [file normalize [info script]]
    set modver [file root [file tail $script]]
    lassign [split $modver -] ns relVersion
    
    if {$relVersion ne ""} {
	set version $relVersion
    }
    
    package provide $ns $version
    namespace eval $ns {;}

    foreach {dep depVers} $args {
	package require -exact $dep $depVers
    }        
} ::} \
