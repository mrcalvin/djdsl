# -*- Tcl -*-

package req Tcl 8.6

apply {{version prj code {test ""}} {
  set script [file normalize [info script]]
  set modver [file root [file tail $script]]
  lassign [split $modver -] ns relVersion
  # set prj [file tail [file dirname $script]]
  
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
    namespace eval ${prj}::$ns $code
  }
} ::} 0.1 "djdsl" {
  #
  # == µTVL
  #

  package req djdsl::opeg
  namespace import ::djdsl::opeg::*

  #
  # Grammar derived from Figure 3 in Dave Clarke, Radu Muschevici,
  # José Proença, Ina Schaefer, Rudolf Schlatte: Variability Modelling
  # in the ABS Language. FMCO 2010: 204-224
  # 
  # to-dos:
  # - custom multiplicity blocks in Multiplicity rule:
  #   [n1..*]|[n1..n2]
  # - re-introduce expr operators by a LR refactoring in Expr rule:
  #   / UnOp Expr / Expr BinOp Expr / OPARENS Expr CPARENS
  # - allow for v1e expr syntax to be used instead of µTVL/TVL syntax?
  #
  # limitations:
  # - AttributeDecl not supported
  # - µTVL extension not supported (CS part is covered by OPEG grammar
  #   composition, attribute part not covered at all)

  set grm {
    #// tvl1 //
    S                <- ROOT FeatureDecl;
    FeatureDeclBody  <- (OBRACKET Group? Constraint* CBRACKET);
    FeatureDecl  <- FID FeatureDeclBody?;
    Group        <- GROUP Multiplicity OBRACKET
                    OPT? FeatureDecl (COMMA OPT? FeatureDecl)*
                    CBRACKET;
    Multiplicity <- ALLOF / ONEOF / SOMEOF / OMP ('*' / <digit>+) SEPMP ('*' / <digit>+) CMP ;
    FID          <- <alnum>+ ;
    Constraint   <- Expr SCOLON / REQUIRE COLON FID SCOLON /
                    EXCLUDE COLON FID ;
    Expr         <- 'True' / 'False' / FID;
    UnOp         <- WS '!' WS;
    BinOp        <- WS ('||' / '&&' / '->' / '<->' / '==' / '!=') WS;
    #// end //
    void:  COMMA   <- WS ',' WS;
    void:  COLON   <- WS ':' WS;
    void:  SCOLON   <- WS ';' WS;
    void:  OPARENS <- WS '(' WS ;
    void:  CPARENS <- WS ')' WS ;
    void:  OBRACKET <- WS '{' WS ;
    void:  CBRACKET <- WS '}' WS;
    void:  OMP <- WS '\[' WS ;
    void:  CMP <- WS '\]' WS ;
    void:  SEPMP <- WS '..' WS ;
    void:  ROOT     <- WS 'root' WS ;
    void:  GROUP    <- WS 'group' WS ;
    void:  OPT      <- WS 'opt' WS ;
    void:  ALLOF    <- WS 'allOf' WS ;
    void:  ONEOF    <- WS 'oneOf' WS ;
    void:  SOMEOF    <- WS 'someOf' WS ;
    void:  REQUIRE  <- WS 'require' WS ;
    void:  EXCLUDE  <- WS 'exclude' WS ;
    void:  WS       <- (COMMENT / <space>)*;
    void:  COMMENT  <- '//' (!EOL .)* EOL ;
    void:  EOL      <- '\n' / '\r' ;
  }

  Grammar create TVLGrm -start S $grm

  set ogrm {
#// tvl2a //
    S          <- `Model` ROOT root:(`$root setRoot $0` FID)
                              (owned:FDeclBody)? !. ;
    FID        <- <alnum>+ ;
    FDeclInner <- `Feature` name:FID (owned:FDeclBody)? ;
#// end //
#// tvl2b //
    FDeclBody  <- OBRACKET Group? Constraint* CBRACKET;
    Group      <- MPGroup / AndGroup / XorGroup / OrGroup;

    MPGroup    <- `Choice` GROUP Multipl OBRACKET GDecls CBRACKET;
    Multipl    <- OMP lower:(`$current card` '*' / <digit>+) SEPMP
                  upper:(`$current card` '*' / <digit>+) CMP;

    GDecls     <- GDecl (COMMA GDecl)*;
    GDecl      <- OPT optionals:FDeclInner / mandatories:FDeclInner;
#// end //

#// tvl2c //
    AndGroup   <- GROUP ALLOF OBRACKET FDeclOuter (COMMA FDeclOuter)* CBRACKET ;
    FDeclOuter <- `Choice` (lower:(`0` OPT))? candidates:FDeclInner ;
    
    XorGroup   <- `Choice` GROUP ONEOF OBRACKET GDecls CBRACKET ;
    OrGroup    <- `Choice` GROUP upper:(`$current card` SOMEOF) OBRACKET GDecls CBRACKET ;
#// end //

#// tvl2d //
    Constraint <- Expr SCOLON / REQUIRE COLON FID SCOLON /
                     EXCLUDE COLON FID ;
    Expr       <- 'True' / 'False' / FID;
    UnOp       <- WS '!' WS;
    BinOp      <- WS ('||' / '&&' / '->' / '<->' / '==' / '!=') WS;
void: COMMA    <- WS ',' WS;
void: COLON    <- WS ':' WS;
void: SCOLON   <- WS ';' WS;
void: OPARENS  <- WS '(' WS ;
void: CPARENS  <- WS ')' WS ;
void: OMP      <- WS '\[' WS ;
void: CMP      <- WS '\]' WS ;
void: SEPMP    <- WS '..' WS ;
void: OBRACKET <- WS '{' WS ;
void: CBRACKET <- WS '}' WS;
void: ROOT     <- WS 'root' WS ;
void: GROUP    <- WS 'group' WS ;
void: OPT      <- WS 'opt' WS ;
void: ALLOF    <- WS 'allOf' WS ;
void: ONEOF    <- WS 'oneOf' WS ;
void: SOMEOF   <- WS 'someOf' WS ;
void: REQUIRE  <- WS 'require' WS ;
void: EXCLUDE  <- WS 'exclude' WS ;
void: WS       <- (COMMENT / <space>)*;
void: COMMENT  <- '//' (!EOL .)* EOL ;
void: EOL      <- '\n' / '\r' ;
#// end //
}

  Grammar create TVLOGrm -start S $ogrm

  namespace export TVLGrm TVLOGrm
} {

  #
  # == Doctests
  #

  namespace import ::djdsl::opeg::*

  #// parserWo //

  set mf [ModelFactory new {
    :object property -accessor public result

    :public object method "input FID" {s e featureName} {
      dict lappend :result features $featureName
      return
    }
    
    :public object method getParse {} {
      return [dict get ${:result} features]
    }
    :public object method reset {} {
      unset -nocomplain :result
    }
  }]
  
  
  set tvlParser [TVLGrm new -factory $mf]

  # set s {
  #   root MultiLingualHelloWorld {
  #     group allOf {
  #       Language {
  #         group someOf {
  #           // opt English, Dutch, German
  #           English, Dutch, German
  #         }
  #       },
  #       opt Repeat
  #     }
  #   }
  # }

  # set s {
  #   root MultiLingualHelloWorld {
  #     group allOf {
  #       Language {
  #         group [*..*] {
  #           English, Dutch, German
  #         }
  #       },
  #       opt Repeat
  #     }
  #   }
  # }

  set s {
    root MultiLingualHelloWorld {
      group allOf {
        Language {
          // oneOf
          group [1..1] {
            English, Dutch, German
          }
        },
        opt Repeat
      }
    }
  }
  
  # puts stderr [string range $s 128 end]

  $tvlParser parse $s

  ? {$tvlParser parse $s} [list {*}{
    MultiLingualHelloWorld
    Language
    English
    Dutch
    German
    Repeat
  }]


  package req djdsl::v1e
  namespace import ::djdsl::v1e::*

  nx::Class create ::djdsl::v1e::ModelFactory -superclasses ModelFactory {
    :variable context:lm:object,type=::djdsl::v1e::Model

    :public method generate {nt generator asgmt} {
      if {![info exists :context]} {
        set :context [::djdsl::v1e::Model new]
      }

      if {$generator in {"Choice" "Feature"}} {
        puts stderr "HERE1:         ${:context} define $generator {*}$asgmt"
        ${:context} define $generator {*}$asgmt
      } else {
        puts stderr "HERE2=$asgmt"
        if {[llength $asgmt]} {
          [${:context} root get] configure {*}$asgmt
          [${:context} root get] register
        }
        return ${:context}
      }
    }
  }

  Model mixins add [nx::Class new {
    :public method setRoot {args} {
      set r [next [lindex $args end]]
      $r rewriteChoices
      return $r
    }
    
    # :public method rewriteChoices {} {
    #   if {[info exists :choicesWithOpts]} {
    #     foreach choice ${:choicesWithOpts} {
    #       $choice rewrite
    #     }
    #   }
    # }
   
  }]

  Feature mixins add [nx::Class new {
    :public method register {} {
      next
      :rewriteChoices
    }
    
    :public method rewriteChoices {} {
      puts rewriteChoices([:name get])=[info exists :owned]
      if {[info exists :owned]} {
        foreach choice ${:owned} {
          $choice rewrite
          # $choice destroy
        }
      }
    }
   
  }]
  
  Choice mixins add [nx::Class new {
    :property -accessor public optionals:object,type=Feature,0..* {
      :public object method value=set {obj prop values} {
        puts stderr ===OPTS=$values
        next
        foreach v $values {
          $obj candidates add $v
        }
      }
    }
    :property -accessor public mandatories:object,type=Feature,0..* {
      :public object method value=set {obj prop values} {
        next
        foreach v $values {
          $obj candidates add $v
        }
      }
    }

    :public method rewrite {} {
      puts REWRITE-[:optionals exists]-[:lower get]-[:upper get]
      if {[:optionals exists]} {
        if {[:lower get] && [:upper get] == [:card] && [:lower get] == [:upper get]} {
          foreach cand ${:candidates} {
            set oc [${:model} define [:info class] \
                        -lower [expr {$cand in ${:mandatories}}] \
                        -upper 1 \
                        -context ${:context} \
                        -candidates $cand]
            $oc register
            lappend choices $oc
          }
          # puts ....[${:context} info class]
          ${:context} owned set $choices
          # TODO: fix cleanup
          ${:model} eval [list :owned delete [self]]
          :destroy
        } else {
          :lower set 0
        }
      }
    }

    :public method card {args} {
      if {[info exists :candidates]} {
        return [llength ${:candidates}]
      } else {
        return 0
      }
    }
  }]


  # allof: w/ and w/o optionals
  # ----------------
  # root f group [3..3] {
  #   a, opt b, c
  # }
  # {f, a, b, c}, {f, a, c}
  
  set tvlOParser [TVLOGrm new -factory [::djdsl::v1e::ModelFactory new]]
  set o [$tvlOParser parse $s]
  puts stderr O=$o
  ? {$o info class} ::djdsl::v1e::Model
  ? {[$o root get] name get} "MultiLingualHelloWorld"
  ? {$o nrValidConfigurations} 6
  # $o nrValidConfigurations

  set p [TVLOGrm new -factory [::djdsl::v1e::ModelFactory new]]
  set m [$p parse {
    root f {
      group [3..3] {
            a, b, c
      }
    }
  }]

  ? {$m info class} ::djdsl::v1e::Model
  ? {[$m root get] name get} "f"
  ? {$m nrValidConfigurations} 1
  ? {$m getValidConfigurations} {{f a b c}}

  set p [TVLOGrm new -factory [::djdsl::v1e::ModelFactory new]]
  set m [$p parse {
    root f {
      group allOf {
            a, opt b, c
      }
    }
  }]

  ? {$m info class} ::djdsl::v1e::Model
  ? {[$m root get] name get} "f"
  ? {$m nrValidConfigurations} 2
  ? {$m getValidConfigurations} {{f a c} {f a b c}}

  set p [TVLOGrm new -factory [::djdsl::v1e::ModelFactory new]]
  set m [$p parse {
    root f {
      group [3..3] {
            a, opt b, c
      }
    }
  }]

  ? {$m info class} ::djdsl::v1e::Model
  ? {[$m root get] name get} "f"
  ? {$m nrValidConfigurations} 2
  ? {$m getValidConfigurations} {{f a c} {f a b c}}

  # someof: w/o optional
  # root f group [1..3] {
  #  a, b, c
  #}
  # {f, c}, {f, b} {f, b, c}, {f, a}, {f, a, c}, {f, a, b}, {f, a, b, c}

  set p [TVLOGrm new -factory [::djdsl::v1e::ModelFactory new]]
  set m [$p parse {
    root f {
      group [1..3] {
        a, b, c
      }
    }
  }]

  ? {$m info class} ::djdsl::v1e::Model
  ? {[$m root get] name get} "f"
  ? {$m nrValidConfigurations} 7
  ? {$m getValidConfigurations} {{f c} {f b} {f b c} {f a} {f a c} {f a b} {f a b c}}

  set p [TVLOGrm new -factory [::djdsl::v1e::ModelFactory new]]
  set m [$p parse {
    root f {
      group someOf {
        a, b, c
      }
    }
  }]

  ? {$m info class} ::djdsl::v1e::Model
  ? {[$m root get] name get} "f"
  ? {$m nrValidConfigurations} 7
  ? {$m getValidConfigurations} {{f c} {f b} {f b c} {f a} {f a c} {f a b} {f a b c}}

  # someof: w/ optional
  # root f group [1..3] {
  #  a, opt b, c
  #}
  # {f}, {f, c}, {f, b} {f, b, c}, {f, a}, {f, a, c}, {f, a, b}, {f, a, b, c}

  set p [TVLOGrm new -factory [::djdsl::v1e::ModelFactory new]]
  set m [$p parse {
    root f {
      group someOf {
        a, opt b, c
      }
    }
  }]

  ? {$m info class} ::djdsl::v1e::Model
  ? {[$m root get] name get} "f"
  ? {$m nrValidConfigurations} 8
  ? {$m getValidConfigurations} {f {f c} {f b} {f b c} {f a} {f a c} {f a b} {f a b c}}

  set p [TVLOGrm new -factory [::djdsl::v1e::ModelFactory new]]
  set m [$p parse {
    root f {
      group someOf {
        a, opt b, opt c
      }
    }
  }]

  ? {$m info class} ::djdsl::v1e::Model
  ? {[$m root get] name get} "f"
  ? {$m nrValidConfigurations} 8
  ? {$m getValidConfigurations} {f {f c} {f b} {f b c} {f a} {f a c} {f a b} {f a b c}}

  set p [TVLOGrm new -factory [::djdsl::v1e::ModelFactory new]]
  set m [$p parse {
    root f {
      group [1..3] {
        a, opt b, c
      }
    }
  }]

  ? {$m info class} ::djdsl::v1e::Model
  ? {[$m root get] name get} "f"
  ? {$m nrValidConfigurations} 8
  ? {$m getValidConfigurations} {f {f c} {f b} {f b c} {f a} {f a c} {f a b} {f a b c}}

  set p [TVLOGrm new -factory [::djdsl::v1e::ModelFactory new]]
  set m [$p parse {
    root f {
      group [0..3] {
        a, opt b, c
      }
    }
  }]

  ? {$m info class} ::djdsl::v1e::Model
  ? {[$m root get] name get} "f"
  ? {$m nrValidConfigurations} 8
  ? {$m getValidConfigurations} {f {f c} {f b} {f b c} {f a} {f a c} {f a b} {f a b c}}

  # oneof w/o optionals
  # root f group [1..1] {
  #  a, b, c
  # }
  # {f, a}, {f, b}, {f, c}

  set p [TVLOGrm new -factory [::djdsl::v1e::ModelFactory new]]
  set m [$p parse {
    root f {
      group [1..1] {
        a, b, c
      }
    }
  }]

  ? {$m info class} ::djdsl::v1e::Model
  ? {[$m root get] name get} "f"
  ? {$m nrValidConfigurations} 3
  ? {$m getValidConfigurations} {{f c} {f b} {f a}}

  set p [TVLOGrm new -factory [::djdsl::v1e::ModelFactory new]]
  set m [$p parse {
    root f {
      group oneOf {
        a, b, c
      }
    }
  }]

  ? {$m info class} ::djdsl::v1e::Model
  ? {[$m root get] name get} "f"
  ? {$m nrValidConfigurations} 3
  ? {$m getValidConfigurations} {{f c} {f b} {f a}}

  # oneof w/o optionals
  # root f group [1..1] {
  #  a, opt b, c
  # }
  # {f}, {f, a}, {f, b}, {f, c}
  set p [TVLOGrm new -factory [::djdsl::v1e::ModelFactory new]]
  set m [$p parse {
    root f {
      group [1..1] {
        a, opt b, c
      }
    }
  }]

  ? {$m info class} ::djdsl::v1e::Model
  ? {[$m root get] name get} "f"
  ? {$m nrValidConfigurations} 4
  ? {$m getValidConfigurations} {f {f c} {f b} {f a}}

  set p [TVLOGrm new -factory [::djdsl::v1e::ModelFactory new]]
  set m [$p parse {
    root f {
      group oneOf {
        a, opt b, c
      }
    }
  }]

  ? {$m info class} ::djdsl::v1e::Model
  ? {[$m root get] name get} "f"
  ? {$m nrValidConfigurations} 4
  ? {$m getValidConfigurations} {f {f c} {f b} {f a}}

  set p [TVLOGrm new -factory [::djdsl::v1e::ModelFactory new]]
  set m [$p parse {
    root f {
      group oneOf {
        a, opt b, opt c
      }
    }
  }]

  ? {$m info class} ::djdsl::v1e::Model
  ? {[$m root get] name get} "f"
  ? {$m nrValidConfigurations} 4
  ? {$m getValidConfigurations} {f {f c} {f b} {f a}}

  set m1 [Model new {
    :setRoot "MultiLingualHelloWorld"
    :define Choice -context ${:root} -lower 1 -upper 1 \
        -candidates [:define Feature -name "Language" \
                         -owned [:define Choice -lower 1 -upper 1 \
                                     -candidates [list [:define Feature -name "English"] \
                                                      [:define Feature -name "German"] \
                                                      [:define Feature -name "Dutch"]]]]
    :define Choice -context ${:root} -lower 0 -upper 1 \
        -candidates [:define Feature -name "Repeat"]
  }]

  ? {$m1 nrValidConfigurations} 6
  
}

# Local variables:
#    mode: tcl
#    tcl-indent-level: 2
#    indent-tabs-mode: nil
# End:
