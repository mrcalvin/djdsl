# -*- Tcl -*-

package req Tcl 8.6

package require pt::pgen
package require pt::rde::nx

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

  package req tclbdd
    
  nx::Class create Model {

    :property -accessor public constraints:object,type=Constraint,0..*
    :property -accessor public choices:object,type=Choice,1..*

    # TODO: make derived, without providing setters (only getters)
    :property -accessor public {root:substdefault,object,type=Feature {[:setup]}}
    :variable owned:object,type=Model::Element,1..*

    :protected method setup {} {
      set rf [:define Feature -name ""]
      set rc [:define Choice -lower 1 -upper 1 -candidates $rf]
      lappend :choices $rc
      return $rf
    }
    
    :public method define {elementType:class args} {
      try {
        set el [$elementType new -model [self] {*}$args]
      } trap {V1E SPEC INVALID} {e opts} {
        return -code error -errorcode "V1E SPEC INVALID" $e
      } on error {e opts} {
        return -code error -errorcode "V1E SPEC INVALID" "Invalid '$elementType' specification: $args."
      }
      $el register 
      lappend :owned $el
      return $el
    }

    :public method getOwnedElements {elementType:class,optional} {

      if {![info exists elementType]} {
	return ${:owned}
      }

      set res [list]
      foreach el ${:owned} {
	if {[$el info has type $elementType]} {
	  lappend res $el
	}
      }
      return $res
    }

    :public method featureLookup {name} {

      if {![info exists :feats]} {
        set :feats [dict create]
        return
      }

      if {[dict exists ${:feats} $name]} {
        return [dict get ${:feats} $name]
      }
      
      return
      
    }

    :public method featureSet {name obj} {

      if {![info exists :feats]} {
        set :feats [dict]
      }

      if {$obj in [dict values ${:feats}]} {
        foreach k [dict keys [dict filter ${:feats} value $obj]] {
          dict unset :feats $k
        }
      }
      
      dict set :feats $name $obj
      return
    }


    :public method destroy {} {
      if {[info exists :owned]} {
        foreach el ${:owned} {
          $el destroy
        }
      }
      next
    }

    ##
    ## Nesting API
    ##

    nx::Class create [self]::Factory {
      :object property -accessor public outputModel:object,type=[:info parent]
      :object property -accessor public ns
      :public method with args {
        set m [[current class] outputModel get]
        set ns [[current class] ns get]
        lassign [next $args] initArgs cmds parentAxis
        set nested [list]
        if {[llength $cmds]} {
          $m eval {lappend :kidz [dict create]}
          # $m eval {*}$cmds
          $m eval [list apply [list {} [lindex $cmds 0] $ns]]
          set nested  [$m eval {lindex ${:kidz} end}]
          $m eval {set :kidz [lrange ${:kidz} 0 end-1]}
        }

        try {
          set current [$m define [self] {*}$initArgs {*}$nested]
        } trap {V1E SPEC INVALID} e {
          return -code error $e
        }

        set up [$m eval {lindex ${:kidz} end}]
        dict lappend up $parentAxis $current
        $m eval [list lset :kidz end $up]
        return 
      }
    }

    :public object method newFromScript {-rootFeature:required script} {
      set ns [self]::ns
      namespace eval $ns {;}
      foreach elClass [[current]::Element info subclasses] {
        interp alias {} ${ns}::[namespace tail $elClass] {} $elClass with
      }
      try {
        :with -rootFeature $rootFeature -ns $ns $script
        # apply [list {} [list :with -rootFeature $rootFeature $script] $ns]
      } finally {
        namespace delete $ns
      }
    }
    
    :public object method newFromScript2 {script} {
      set box [nx::Object new -childof [self] {
        :object method root {args} {
          set :root $args
        }
        :object method feature {-name args} {
          puts stderr "          interp alias {} [self]::%$name {} Feature with -name $name {*}$args"
          set aliasName [self]::%$name
          append body [list interp alias {} [self]::%$name {}] \;
          append body [list Feature -name $name {*}$args] \;
          puts body=$body
          interp alias {} [self]::%$name {} apply [list {} $body [self]]
          # dict set :env $name $args
        }
      }]
      $box require namespace
      interp alias {} ${box}::Root {} :root
      interp alias {} ${box}::Feature {} :feature
      $box eval [list apply [list {} $script $box]]
      lassign [$box eval {set :root}] rootFeature script
      
      foreach elClass [[current]::Element info subclasses] {
        interp alias {} ${box}::[namespace tail $elClass] {} $elClass with
      }
      
      try {
        :with -rootFeature $rootFeature -ns $box $script
        # apply [list {} [list :with -rootFeature $rootFeature $script] $ns]
      } finally {
        $box destroy
      }
    }



    :public object method with {-rootFeature -ns spec} {
      if {![info exists ns]} {
        set ns [namespace current]
        namespace eval [self] {namespace import ::djdsl::v1e::*}
      }
      
      set m [:new]
      set root [$m root get]
      if {[info exists rootFeature]} {
       	$root name set $rootFeature
        $m featureSet $rootFeature $root
      }
      [self]::Factory outputModel set $m
      [self]::Factory ns set $ns
      nx::Class mixins add [self]::Factory
      try {
        $m eval {lappend :kidz [dict create]}
        # $m eval $spec
        $m eval [list apply [list {} $spec $ns]]
        if {[$m eval {info exists :kidz}]} {
          set k [$m eval {lindex ${:kidz} end}]
          if {[dict exists $k -owned]} {
            $root configure {*}[dict filter $k key -owned]
            $root register
          }
          if {[dict exists $k -constraints]} {
            # TODO: substdefault on root is called again, FIX!
            $m configure -root [$m root get] {*}[dict filter $k key -constraints]
          }
        }
      } on ok {} {
        return $m
      } on error {res opts} {
        return -code error -options $opts $res
      } finally {
        nx::Class mixins delete [self]::Factory
        [self]::Factory outputModel unset
        $m eval {unset -nocomplain :kidz}
      }

    }

    :public method isValid {} {
      set bdd [: -local requireBDD]
      return [$bdd isSatisfiable]
    }

    :public method nrValidConfigurations {} {
      set bdd [: -local requireBDD]
      return [$bdd satCount]
    }

    :public method getValidConfigurations {{n:substdefault {[:nrValidConfigurations]}}} {
      set bdd [: -local requireBDD]
      return [$bdd computeValidConfigurations $n]
    }


    

    :private method requireBDD {} {
      if {![info exists :bdd]} {
        set :bdd [[current class]::BDD new -model [self]]
      }
      return ${:bdd}
    }

    #
    # A slim component wrapper around tclbdd's TclOO facility, plus helpers.
    #
    
    nx::Class create [self]::BDD {
      :property model:object,type=[:info parent]
      :variable rootExpr C0

      :public method isSatisfiable {} {
        return [${:system} satisfiable ${:rootExpr}]
      }

      :public method satCount {} {
        return [${:system} satcount ${:rootExpr}]
      }

      :public method computeValidConfigurations {n} {
        set out [list]
        set counter 0
        ${:system} foreach_sat x ${:rootExpr} {
          bdd::foreach_fullsat v ${:varsIdx} $x {
            if {$counter == $n} { return $out; }
            lappend out [lmap i ${:varsIdx} j $v {
              set _ [expr {($i+1)*$j}]; if {$_ == 0} {
                continue
              } else {
                lindex ${:vars} [incr _ -1]}
            }]
            incr counter
          }
        }
        return $out
      }

      :public method destroy args {
        rename ${:system} ""
        unset :system
        next
      }

      :method init {} {
        
        set :system [bdd::system new]
        set feats [${:model} eval {set :feats}]
        set rootFeat [${:model} root get]
        set :vars [dict keys $feats]
        
        set pos 0
        foreach f ${:vars} {
          ${:system} nthvar $f $pos
          lappend :varsIdx $pos
          incr pos
        }

        # TODO: generalize this by treating the root Choice as the
        # others.
        ${:system} & C0 [$rootFeat name get] 1; # root feature is always TRUE
        
        set C 1
        foreach c [${:model} getOwnedElements Choice] {
          if {![$c context isSet]} continue
          set pObj [$c context get]
          set p [$pObj name get]
          
          if {[$c lower get] == 0 && [$c upper get] == 1} {
            if {[llength [$c candidates get]] == 1} {
              # optional, solitary sub-feature
              set f [[$c candidates get] name get]
              # puts "${:system} <= C$C $f $p"
              ${:system} <= C$C $f $p
            } else {
              # TODO: is this needed?
              # group of optional features
            }
          }
          
          if {[$c lower get] == 1 && [$c upper get] == 1} {
            if {[llength [$c candidates get]] == 1} {
              # mandatory, solitary sub-feature
              set f [[$c candidates get] name get]
              # ${:system} <= aC$C $p $f
              # ${:system} <= bC$C $f $p
              # ${:system} & C$C aC$C bC$C
              ${:system} == C$C $p $f
            } else {
              
              # pt 1: disjunction term 
              set cands [$c candidates get]
              set r [lassign $cands c1 c2]
              ${:system} | tmp0 [$c1 name get] [$c2 name get]
              foreach rc $r {
                ${:system} | tmp0 tmp0 [$rc name get]
              }
              # ${:system} <= aC$C tmp0 $p
              # ${:system} <= bC$C $p tmp0
              # ${:system} & C$C aC$C bC$C
              ${:system} == C$C tmp0 $p
              
              # pt 2: pairwise exclusions
              foreach comb [:comb2 $cands] {
                lassign $comb c1 c2
                ${:system} & tmp3 [$c1 name get] [$c2 name get]
                ${:system} ~ ntmp3 tmp3; # negate the term
                ${:system} & C$C C$C ntmp3 
              }
            }
          }
          if {[$c lower get] == 1 && [$c upper get] > 1 &&
              [$c upper get] == [llength [$c candidates get]]} {
            set r [lassign [$c candidates get] c1 c2]
            ${:system} | tmp1 [$c1 name get] [$c2 name get]
            foreach rc $r {
              ${:system} | tmp1 tmp1 [$rc name get]
            }
            ${:system} == C$C tmp1 $p
          }

          ${:system} & C0 C0 C$C
          
          # puts [${:system} dump C0]
          incr C
        }

        # inject the constraints feature expressions into the BDD
        # system, if any ...
        set fexprs [lmap cstr [${:model} getOwnedElements Constraint] {
          $cstr cget -expression
        }]

        if {[llength $fexprs]} {
          ${:system} & C0 C0 [:add {*}$fexprs]
        }
      }
      
      :protected method comb2 {in} {
        if {[llength $in] <= 2} {
          return [list $in]
        }
        while {[llength $in]} {
          set in [lassign $in x]
          foreach y $in {
            lappend out [list $x $y]
          }
        }
        return $out
      }
    
      ##
      ## Add BDDs into a system using "feature expressions". A "feature
      ## expression" is a propositional formula ...
      ## - whose variables represent (existing) features in the model.
      ## - which does *not* contain literal truth values (1, 0).
      ##

      set v1e {
        PEG v1e (Expression)
        Expression     		<- _ Term (_ BinaryOp _ Term)?;
        Term		 	<- NotOp? _ (Variable / '(' Expression ')');
        # leaf:	Atom            	<- Literal / Variable;
        #	Literal			<- '1' / '0';
        leaf:   BinaryOp 		<- AndOp / OrOp / ImplOp;
        AndOp 			<- 'and' / '&&';
        OrOp			<- 'or' / '||';
        ImplOp 			<- 'implies' / '->';
        NotOp 			<- 'not' / '-';
        Variable 		<- <alnum>+;
        void:	_			<- <space>*;	
        END;}

      set v1eParser [pt::rde::nx pgen $v1e]
      $v1eParser create [self]::FexprParser
      
      :public method add {fexpr args} {
        if {[llength $args]} {
          set fexpr ([join [list $fexpr {*}$args] ") and ("])
        }
        # puts >>>$fexpr
        # [current class]::FexprParser print $fexpr
        set st [lassign [[current class]::FexprParser parset $fexpr] m]

        set :fexpr ${fexpr}
        set r [:input $m {*}$st]
        unset :fexpr
        return $r

      }

      # TODO: Better check args arity than default to EmptyOp/EmptyOpnd?
      :method "input EmptyOp" {} {return &}
      :method "input EmptyOpnd" {} {return 1}
      :method "input Expression" {from to args} {
        set res "fexpr[incr :exprCounter]"
        lassign [list {*}$args EmptyOp EmptyOpnd] lhs op rhs
        # puts lhs=$lhs,op=$op,rhs=$rhs
        set lhs [:input {*}$lhs]
        set op [:input {*}$op]
        set rhs [:input {*}$rhs]
        # puts "${:system} $op $res $lhs $rhs"
        ${:system} $op $res $lhs $rhs
        return $res
      }
      
      # why does forward "input Expression" not work?
      
      :method "input Term" {from to args} {
        lassign $args prefix fexpr
        if {$fexpr eq ""} {
          set fexpr $prefix
          return [:input {*}$fexpr]
        } else {
          set op [:input {*}$prefix]
          set res [:input {*}$fexpr]
          ${:system} $op "n$res" $res
          return "n$res"
        }
      }
      
      :method "input BinaryOp" {from to args} {
        array set ops {and & or | not ~}
        return $ops([string range ${:fexpr} $from $to])
      }
      
      :method "input NotOp" args {
        return [:input BinaryOp {*}$args]
      }
      
      
      :method "input Variable" {from to args} {
        # TODO: Check for valid feature names?
        string range ${:fexpr} $from $to
      } 
    } 
  }
    
  nx::Class create Model::Element {
    :property -accessor public model:object,type=[:info parent],required
    :public method register {} {
      error "Must be implemented by each subclass!"
    }
  }


  nx::Class create Choice -superclasses Model::Element {

    :property -accessor public context:object,type=Feature {
      :public object method value=isSet {obj args} {
        ::nsf::var::exists $obj context
      }
    }
    
    :property -accessor public candidates:object,type=Feature,1..*

    :property -accessor public {upper:integer 1}
    :property -accessor public {lower:integer 1}

    :public method register {} {
      foreach c ${:candidates} {
	$c owning set [self]
      }
    }
    
    :public method isXor {} {;}
    :public method isOr {} {;}
    :public method isAnd {} {;}

    :public object method with {{-lower 1} {-upper 1} args} { 
      return [list [list -lower $lower -upper $upper] $args -owned]
    }
  }
  
  nx::Class create Feature -superclasses Model::Element {
    :property -accessor public name
    
    :property -accessor public owning:object,type=Choice
    :property -accessor public -incremental owned:object,type=Choice,0..*
    
    :public object method new {-model -name args} {
      if {[$model featureLookup $name] eq ""} {
        set f [next]
        $model featureSet $name $f
        return $f
      } else {
        return -code error -errorcode "V1E SPEC INVALID" "Features must have unique names."
      }
    }
    
    :public method register {} {
      # ${:owningModel} featureSet ${:name} [self]
      if {[info exists :owned]} {
        foreach c ${:owned} {
          $c context set [self]
        }
      }
    }
    
    :public method parentFeature {} {;}
    :public method subFeatures {} {;}
    
    :public method isMandatory {} {;}
    :public method isOptional {} {;}
    
    ##
    ## Nesting API
    ##
    
    :public object method with {-name args} {
      set initArgs [list]
      if {[info exists name]} {
        set initArgs [list -name $name]
      }
      return [list $initArgs $args -candidates]
    }

  }

  nx::Class create Constraint -superclasses Model::Element {
      :property expression
      :public method register {} {;}
      :public object method with {expr} {
	  return [list [list -expression $expr] "" -constraints]
      }
  }

  namespace export Model Choice Feature Constraint

} {

}

# Local variables:
#    mode: tcl
#    tcl-indent-level: 2
#    indent-tabs-mode: nil
# End:
#

  
