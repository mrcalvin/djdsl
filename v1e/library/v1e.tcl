# -*- Tcl -*-
#
# MIT License
#
# Copyright (c) 2017-2020 Stefan Sobernig <stefan.sobernig@wu.ac.at>
# Copyright (c) 2020 Olaf Lessenich <olaf.lessenich@wu.ac.at>
#
# Permission is hereby granted, free of charge, to any person obtaining a copy
# of this software and associated documentation files (the "Software"), to deal
# in the Software without restriction, including without limitation the rights
# to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
# copies of the Software, and to permit persons to whom the Software is
# furnished to do so, subject to the following conditions:
#
# The above copyright notice and this permission notice shall be included in all
# copies or substantial portions of the Software.
#
# THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
# IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
# FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
# AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
# LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
# OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
# SOFTWARE.

package req Tcl 8.6

apply {{version prj code {test ""}} {
  set script [file normalize [info script]]
  set modver [file root [file tail $script]]
  lassign [split $modver -] ns relVersion
  
  if {$relVersion ne ""} {
    set version $relVersion
  }

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
        
        uplevel #0 [list namespace eval ${prj}::$ns $code]
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
        uplevel #0 [list namespace eval ::${prj}::${ns}::test $test]
        
        namespace eval ::${prj}::${ns}::test cleanupTests
        namespace delete ::${prj}::${ns}::test
      }
    }
  } else {
    # set prj [file tail [file dirname $script]]
    package provide ${prj}::$ns $version
    namespace eval ${prj}::$ns $code
  }
} ::} 0.1 djdsl {
  
  #
  # == Implementation
  #

  package req nx 2.3

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
  
  nx::Class create Model {

    :property -accessor public constraints:object,type=Constraint,0..*
    :property -accessor public choices:object,type=Choice,1..*

    # TODO: make derived, without providing setters (only getters)
    :property -accessor public {root:substdefault,object,type=Feature {[:setup]}}

    # Uniqueness-constrained property based on [dict]
    :property -accessor protected -incremental owned:object,type=Model::Element,1..* {
      :public object method value=set {obj prop value:object,type=::djdsl::v1e::Model::Element,1..*} {
        dict keys [next [list $obj $prop [concat {*}[lmap a $value b {} {list $a $b}]]]]
      }
      
      :public object method value=get {obj prop} {
        dict keys [next]
      }
      :public object method value=add {obj prop value:object,type=::djdsl::v1e::Model::Element} {
        dict keys [$obj eval [list dict set :$prop $value ""]]
      }

      :public object method value=delete {obj prop value} {
        $obj eval [list dict unset :$prop $value]
      }
    }

    :protected method setup {} {
      if {![info exists :root]} {
        set rf [:define Feature -name ""]
        set rc [:define Choice -lower 1 -upper 1 -candidates $rf]
        lappend :choices $rc
        return $rf
      } else {
        return ${:root}
      }
    }

    ## PREVIOUSLY:
    ##    :public method setRoot {featName} {
    ##      # set featName [lindex $args end]
    ##      # ...    
    ## }
    
    :public method setRoot {args} {
      set featName [lindex $args end]
      ${:root} name set $featName
      :featureSet $featName ${:root}
      return ${:root}
    }

    :public method define {elementType:class args} {
      set el [:require $elementType {*}$args]
      $el register
      :owned add $el
      return $el
    }
    
    :public method require {elementType:class args} {
      try {
        $elementType new -model [self] {*}$args
      } trap {V1E SPEC INVALID} {e opts} {
        return -code error -errorcode "V1E SPEC INVALID" $e
      } on error {e opts} {
        puts OPTS=$opts
        return -code error -errorcode \
            "V1E SPEC INVALID" "Invalid '$elementType' specification: $args."
      }
    }

    :public method getOwnedElements {elementType:class,optional} {

      set owned [:owned get]
      if {![info exists elementType]} {
	return $owned
      }

      set res [list]
      foreach el $owned {
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

      set existing [dict keys [dict filter ${:feats} value $obj]]
      foreach k $existing {
        dict unset :feats $k
      }
      
      dict set :feats $name $obj
      return
    }


    :public method destroy {} {
      if {[:owned exists]} {
        foreach el [:owned get] {
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

    # :public object method newFromScript {-rootFeature:required script} {
    #   set ns [self]::ns
    #   namespace eval $ns {;}
    #   foreach elClass [[current]::Element info subclasses] {
    #     interp alias {} ${ns}::[namespace tail $elClass] {} $elClass with
    #   }
    #   try {
    #     :with -rootFeature $rootFeature -ns $ns $script
    #     # apply [list {} [list :with -rootFeature $rootFeature $script] $ns]
    #   } finally {
    #     namespace delete $ns
    #   }
    # }
    
    :public object method newFromScript {script} {
      set box [nx::Object new -childof [self] {
        :object method root {args} {
          set :root $args
        }
        :object method feature {-name args} {
          set aliasName [self]::%$name
          append body [list interp alias {} [self]::%$name {}] \;
          append body [list Feature -name $name {*}$args] \;
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

    :public method addFromScript {script ns:optional} {

      if {![info exists ns]} {
        set ns [namespace current]
        namespace eval [self] {namespace import ::djdsl::v1e::*}
      }

      set factory "[current class]::Factory"
      $factory outputModel set [self]
      $factory ns set $ns
      nx::Class mixins add $factory
      try {
        lappend :kidz [dict create]
        apply [list {} $script $ns]
        if {[info exists :kidz]} {
          set k [lindex ${:kidz} end]
          if {[dict exists $k -owned]} {
            ${:root} configure {*}[dict filter $k key -owned]
            ${:root} register
          }
          if {[dict exists $k -constraints]} {
            # TODO: substdefault on root is called again, FIX!
            :configure -root ${:root} {*}[dict filter $k key -constraints]
          }
        }
        return
      } on error {res opts} {
        return -code error -options $opts $res
      } finally {
        nx::Class mixins delete $factory
        $factory outputModel unset
        $factory ns unset
        unset -nocomplain :kidz
      }
    }



    :public object method with {-rootFeature -ns spec} {      
      set m [:new]
      if {[info exists rootFeature]} {
        $m setRoot $rootFeature
      }
      
      if {[info exists ns]} {
        $m addFromScript $spec $ns
      } else {
        $m addFromScript $spec
      }
      return $m
    }
    
    #
    # A slim component wrapper around tclbdd's TclOO facility, plus helpers.
    #

    try {
      package req tclbdd
    } trap {TCL PACKAGE UNFOUND} {} {
      # TODO: should we warn about an undetectable TclBDD installation; or is it optional
    } on ok {} {
      nx::Class create [self]::BDDSystem {
        :property model:object,type=[:info parent]
        
        :public method isSatisfiable {} {
          return [${:system} satisfiable ${:model}]
        }
        
        :public method satCount {} {
          return [${:system} satcount ${:model}]
        }
        
        :public method computeValidConfigurations {n} {
          set out [list]
          set counter 0
          ${:system} foreach_sat x ${:model} {
            bdd::foreach_fullsat v ${:varsIdx} $x {
              if {$counter == $n} { return $out; }
              lappend out [lmap i ${:varsIdx} j $v {
                set _ [expr {($i+1)*$j}];
                if {$_ == 0} {
                  continue
                } else {
                  set obj [lindex ${:vars} [incr _ -1]]
                  if {[$obj name exists]} {
                    $obj name get
                  } else {
                    continue; # $obj;
                  }
                }
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
          
          # TODOs:
          # - rework to walk spines of choices, rather than all choices as a bulk (visitor)
          # - ::djdsl::v1e::* prefixing should not be necessary, v1e.test ok, v1e.tcl not. grrr.
          # - refactor, so that we can process arbitrary choices into
          #   corresponding BDDs, given a BDD system.
          
          set :system [bdd::system new]
          set feats [${:model} eval {set :feats}]
          set rootFeat [${:model} root get]
          
          # FIX:
          # set :vars [lsort -unique [${:model} getOwnedElements ::djdsl::v1e::Feature]]
          set :vars [${:model} getOwnedElements ::djdsl::v1e::Feature]
          
          set pos 0
          foreach f ${:vars} {
            ${:system} nthvar $f $pos
            lappend :varsIdx $pos
            incr pos
          }

          ${:system} & ${:model} 1 1; # root feature is always TRUE

          # FIX:
          # puts stderr >>>[namespace current],[namespace which Choice],[uplevel 1 {namespace current}]
          foreach c [${:model} getOwnedElements ::djdsl::v1e::Choice] {
            if {[$c context exists]} {
              set p [$c context get]
            } else {
              set p ${:model}
            }

            if {[$c lower get] > [$c upper get] ||
                [$c lower get] > [llength [$c candidates get]] ||
                [$c lower get] < 0 ||
                [$c upper get] < 0} {
                throw {V1E BDD INVALID} "The multiplicity [$c lower get],[$c upper get] is invalid."
            }

            if {[$c lower get] == 0 && [$c upper get] == 1} {
              if {[llength [$c candidates get]] == 1} {
                # [0,1] n=1
                # optional, solitary sub-feature
                set f [$c candidates get]
                ${:system} <= $c $f $p
              } else {
                # [0,1] n>1
                # puts "TODO: $c"
                # TODO: is this needed? 
                
                # group of optional features

                # pt 1: disjunction term 
                set cands [$c candidates get]
                set r [lassign $cands c1 c2]
                ${:system} | tmp0 $c1 $c2
                foreach rc $r {
                  ${:system} | tmp0 tmp0 $rc
                }
                ${:system} <= $c tmp0 $p
                ${:system} unset tmp0
                
                # pt 2: pairwise exclusions
                foreach comb [:comb2 $cands] {
                  lassign $comb c1 c2
                  ${:system} & tmp1 $c1 $c2
                  ${:system} ~ ntmp1 tmp1; # negate the term
                  ${:system} & $c $c ntmp1 
                  ${:system} unset tmp1
                  ${:system} unset ntmp1
                }
              }
            } elseif {[$c lower get] == 1 && [$c upper get] == 1} {
              if {[llength [$c candidates get]] == 1} {
                # [1,1] n=1
                # mandatory, solitary sub-feature
                set f [$c candidates get]
                ${:system} == $c $p $f
              } else {
                # [1,1] n>1
                # alternative features

                # pt 1: disjunction term 
                set cands [$c candidates get]
                set r [lassign $cands c1 c2]
                ${:system} | tmp0 $c1 $c2
                foreach rc $r {
                  ${:system} | tmp0 tmp0 $rc
                }
                ${:system} == $c tmp0 $p
                ${:system} unset tmp0

                # pt 2: pairwise exclusions
                foreach comb [:comb2 $cands] {
                  lassign $comb c1 c2
                  ${:system} & tmp1 $c1 $c2
                  ${:system} ~ ntmp1 tmp1; # negate the term
                  ${:system} & $c $c ntmp1 
                  ${:system} unset tmp1
                  ${:system} unset ntmp1
                }
              }
            } elseif {[$c lower get] == 1 && [$c upper get] > 1 &&
						  [$c upper get] == [llength [$c candidates get]]} {
              # [1,n]

              set r [lassign [$c candidates get] c1 c2]
              ${:system} | tmp0 $c1 $c2
              foreach rc $r {
                ${:system} | tmp0 tmp0 $rc
              }
              ${:system} == $c tmp0 $p
              ${:system} unset tmp0
            } elseif {!([$c lower get] + [$c upper get])} {
              # [0,0] ?
              
              ${:system} & $c 1 1
              foreach cand [$c candidates get] {
                ${:system} ~ ntmp0 $cand
                ${:system} & $c $c ntmp0
              }
              ${:system} == $c $p ntmp0
              ${:system} unset ntmp0
            } else {
              if {[$c lower get] == 0 && [$c upper get] > 1 &&
						  [$c upper get] == [llength [$c candidates get]]} {
                # [0,n]
                # Technically, this is not at-most-k
                
                set r [lassign [$c candidates get] c1 c2]
                ${:system} | tmp0 $c1 $c2
                foreach rc $r {
                  ${:system} | tmp0 tmp0 $rc
                }
                ${:system} <= $c tmp0 $p
                ${:system} unset ntmp0
              } elseif {[$c lower get] >= 0 && [$c upper get] > 1} {
                # [l,k] k>1
                set cands [$c candidates get]
                
                # pt 1: disjunction term 
                if {[$c lower get] <= 1} {
                  # [0,k] k>1
                  set r [lassign $cands c1 c2]
                  ${:system} | tmp0 $c1 $c2
                  foreach rc $r {
                    ${:system} | tmp0 tmp0 $rc
                  }
                  if {[$c lower get] == 0} {
                    ${:system} <= $c tmp0 $p
                  } else {
                    ${:system} == $c tmp0 $p
                  }
                  ${:system} unset tmp0
                } else {
                  # [l,k] l>0, k>1
                  set kcomb [:combk $cands [$c lower get]]
                  set combs [lassign $kcomb comb]

                  set r [lassign $comb c1 c2]
                  ${:system} & tmp0 $c1 $c2
                  foreach rc $r {
                    ${:system} & tmp0 tmp0 $rc
                  }

                  foreach comb $combs {
                    set r [lassign $comb c1 c2]
                    ${:system} & tmp1 $c1 $c2
                    foreach rc $r {
                      ${:system} & tmp1 tmp1 $rc
                    }
                    ${:system} | tmp0 tmp0 tmp1
                    ${:system} unset tmp1
                  }
                  ${:system} == $c tmp0 $p
                  ${:system} unset tmp0
                }

                # pt 2: exclude all k+1 combinations
                set k [$c upper get]
                set kcomb [:combk $cands [expr {$k + 1}]]

                foreach comb $kcomb {
                  set r [lassign $comb c1 c2]
                  ${:system} & tmp1 $c1 $c2
                  foreach rc $r {
                    ${:system} & tmp1 tmp1 $rc
                  }
                  ${:system} ~ ntmp1 tmp1
                  ${:system} & $c $c ntmp1
                  ${:system} unset tmp1
                  ${:system} unset ntmp1
                }
              } else {
                # TODO: lower >1 (at-least-k?)
                throw {V1E BDD NOTIMPLEMENTED} "The multiplicity [$c lower get],[$c upper get] is not implemented."
              }
            }

            ${:system} & ${:model} ${:model} $c
            # puts [${:system} dump ${:model}]
            # puts >>>>[:asDot $c]
          }

          # inject the constraints feature expressions into the BDD
          # system, if any ...
          set fexprs [lmap cstr [${:model} getOwnedElements ::djdsl::v1e::Constraint] {
            if {![$cstr expression exists]} {continue} else {$cstr cget -expression};
          }]

          puts fexprs=$fexprs
          if {[llength $fexprs]} {
            ${:system} & ${:model} ${:model} [:add {*}$fexprs]
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

        :protected method combk {in k} {
          if {$k == 0} {
            return [list {}]
          } elseif {[llength $in] < $k} {
            return [list]
          } elseif {[llength $in] == $k} {
            return [list $in]
          }

          set short_in [lassign $in x]

          set out [:combk $short_in $k]

          set c [:combk $short_in [expr {$k - 1}]]
          foreach y $c {
            lappend out [concat $x $y]
          }

          return $out
        }
        
        ##
        ## Add BDDs into a system using "feature expressions". A "feature
        ## expression" is a propositional formula ...
        ## - whose variables represent (existing) features in the model.
        ## - which does *not* contain literal truth values (1, 0).
        ##

        # leaf:   BinaryOp 		<- AndOp / OrOp / ImplOp;
        #         ImplOp 			<- 'implies' / '->';
        
        set v1e {
          PEG v1e (Expression)
          #// constrL //
          Expression <- _ Term (_ BinaryOp _ Term)?;
          Term	     <- NotOp? _ (Variable / '(' Expression ')');
    leaf: BinaryOp   <- AndOp / OrOp;
          AndOp      <- 'and' / '&&';
          OrOp	     <- 'or' / '||';
          NotOp      <- 'not' / '-';
          Variable   <- <alnum>+;
          void:	_    <- <space>*;
          #// end //
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
          set name [string range ${:fexpr} $from $to]
          ${:model} featureLookup $name
        }

        #
        # Helpers
        #
        # DOT printer: `dot -Nfontname=FreeSans -Tsvg`
        :public method asDot {bdd} {
          set dump [dict create {*}[${:system} dump $bdd]]
          
          dict unset dump 0
          dict unset dump 1
          
          append dot "digraph \"$bdd\" {" \n;
          append dot "0 \[shape=box, label=\"0\", style=filled, shape=box, height=0.3, width=0.3\];" \n;
          append dot "1 \[shape=box, label=\"1\", style=filled, shape=box, height=0.3, width=0.3\];" \n
          
          set levels [dict create]
          dict for {node dat} $dump {
            lassign $dat varIdx lo hi
            set feat [lindex ${:vars} $varIdx]
            set label ""; # unnamed features (helpers) remain blank
            if {[$feat name exists]} {
              set label [$feat name get]
            }
            append dot "$node \[label=\"$label\"\];" \n
            append dot "$node -> $lo \[style=dotted\];" \n
            append dot "$node -> $hi \[style=filled\];" \n
            
            dict lappend levels $varIdx $node
          }
          
          dict for {level nodes} $levels {
            append dot "{rank = same; [join $nodes ;]}"
          }
          
          append dot "}"
          return $dot
          
        }
      }; # BDDSystem

      # BDD wrappers for Model

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
      
      # :public method equiv {that:object,type=Model} {
      #   set bdd [: -local requireBDD]
      #   return [$bdd ]
      # }
      
      :public method asDot {} {
        set bdd [: -local requireBDD]
        return [$bdd asDot [self]]
      }
      
      :private method requireBDD {} {
        if {![info exists :bdd]} {
          set :bdd [[current class]::BDDSystem new -model [self]]
        }
        return ${:bdd}
      }
    } on error {msg opts} {
      return -opts $opts -errorcode "DJDSL V1E TCLBDD FAILED" $msg
    }
  }; # Model
  
  nx::Class create Model::Element {
    :property -accessor public model:object,type=[:info parent],required
    :protected method register {} {
      error "Must be implemented by each subclass!"
    }
    # :public method init {} {
    # :register
    # }
  }


  nx::Class create Choice -superclasses Model::Element {

    :property -accessor public context:object,type=Feature
    
    :property -accessor public candidates:object,type=Feature,1..*

    :property -accessor public {upper:integer 1}
    :property -accessor public {lower:integer 1}

    :public method register {} {
      foreach c ${:candidates} {
        if {![$c owning exists]} {
          $c owning set [self]
        }
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
      if {![info exists name]} {
        set existing ""
      } else {
        set existing [$model featureLookup $name]
      }
      
      if {$existing eq ""} {
        next
      } else {
        return $existing
      }
    }
    
    :public method register {} {
      # ${:owningModel} featureSet ${:name} [self]
      if {[info exists :name]} {
        ${:model} featureSet ${:name} [self]
      }
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
    :property -accessor public expression
    :public method register {args} {}
    :public object method with {expr} {
      return [list [list -expression $expr] "" -constraints]
    }
  }

  namespace export Model Choice Feature Constraint

} {

  #
  # == Doctests
  #

  package require djdsl::v1e
  namespace import ::djdsl::v1e::*
  
  #
  # A small excerpt from the GPL feature model, defined using the v1e
  # textual notation.
  #
  set gpl [Model newFromScript {
    #// gpl1 //
    Root "Graph" {
      Choice -lower 0 -upper 1 {
        Feature -name "coloured"
      }
      Choice -lower 0 -upper 1 {
        Feature -name "weighted"
      }
    }
    #// end //
  }]

  # puts [$gpl asDot]

  ? {llength [$gpl getOwnedElements]} 6

  #
  # TVL example of flattening declaration hierarchies (Fig 2)
  #
  set tvl1 [Model newFromScript {
    #// tvl1 //
    Root "R" {
      Choice -lower 1 -upper 1 {
        Feature -name "Level1" {
          Choice -lower 1 -upper 1 {
            Feature -name "Level2" {
              Choice -lower 2 -upper 2 {
                Feature -name "Level3a"
                Feature -name "Level3b"
              }
            }
          }
        }
      }
    }
    #// end //
  }]

  ? {llength [$tvl1 getOwnedElements]} 9

  set tvl2 [Model newFromScript {
    #// tvl2 //
    Root "R" {
      Choice -lower 1 -upper 1 {
        %Level1
      }
    }
    
    Feature -name "Level1" {
      Choice -lower 1 -upper 1 {
        %Level2
      }
    }
    
    Feature -name "Level2" {
      Choice -lower 2 -upper 2 {
        Feature -name "Level3a"
        Feature -name "Level3b"
      }
    }
    #// end //
  }]

  ? {llength [$tvl2 getOwnedElements]} 9

  set constrainedModel [Model newFromScript {
    #// constrM //
    Root "Graph" {
      Choice -lower 0 -upper 1 {
        Feature -name "Algorithm" {
          Choice -lower 1 -upper 2 {
            Feature -name "MST"
            Feature -name "ShortestPath"
          }
        }
      }
      Choice -lower 0 -upper 1 {
        Feature -name "weighted"
      }
      #// end //
      #// constr2 //
      Constraint {not MST or weighted}
      #// end //
    }
  }]
  
  ? {llength [$constrainedModel getOwnedElements]} 10
  ? {$constrainedModel nrValidConfigurations} 6

  set constrainedModel2 [Model newFromScript {
    Root "Graph" {
      Choice -lower 0 -upper 1 {
        Feature -name "Algorithm" {
          Choice -lower 1 -upper 2 {
            Feature -name "MST"
            Feature -name "ShortestPath"
          }
        }
      }
      Choice -lower 0 -upper 1 {
        Feature -name "weighted"
      }
    }
  }]

  $constrainedModel2 addFromScript {
    #// constr3 //
    Choice with -lower 1 -upper 2 {
      Feature with {
        Choice with -lower 0 -upper 0 {
          Feature with -name "MST"
        }
      }
      Feature with -name "weighted"
    }
    #// end //
  }
  
  ? {$constrainedModel2 nrValidConfigurations} 6
  
  ? {$constrainedModel getValidConfigurations [$constrainedModel nrValidConfigurations]} {Graph {Graph weighted} {Graph ShortestPath Algorithm} {Graph ShortestPath Algorithm weighted} {Graph MST Algorithm weighted} {Graph MST ShortestPath Algorithm weighted}}
  
  ? {$constrainedModel2 getValidConfigurations [$constrainedModel2 nrValidConfigurations]} {Graph {Graph weighted} {Graph ShortestPath Algorithm} {Graph ShortestPath Algorithm weighted} {Graph MST Algorithm weighted} {Graph MST ShortestPath Algorithm weighted}}
  
}

# TODO:
# ? {$constrainedModel2 equiv $constrainedModel} 1; # === BDD1 BDD2
# TODO: simplify implementation using sth. akin of
#   nx::Class create Model; use the Model instance as visitor?
# nx::Class create Model::Element {
#     :protected method __object_configureparameter {} {
# 	set spec [next]
# 	lreplace $spec[set spec {}] end end foo:optional,alias
#     }
#     ::nsf::parameter::cache::classinvalidate [current]
#     :protected method foo {script} {
# 	apply [list {} $script ::]
#     # ${:model} eval $script
#     }
#     :public method init {} {
# 	puts [:info class]([self])=init
#     }
# }

# nx::Class create Feature -superclasses Model::Element {
#     :property name
#     :protected method foo {script} {
# 	next
#     }
# }

# nx::Class create Choice -superclasses Model::Element {
#     :property upper
#     :property lower
#     :protected method foo {script} {
# 	next
#     }
# }

# Feature new -name "X" {
#     Choice new -upper 1 -lower 2 {
# 	Feature new -name "Z"
#     }
# }  

# Local variables:
#    mode: tcl
#    tcl-indent-level: 2
#    indent-tabs-mode: nil
# End:
#


# vim: set ts=2 sw=2 autoindent smartindent expandtab:
