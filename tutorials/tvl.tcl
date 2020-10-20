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
    S            <- ROOT FeatureDecl;
    FeatureDecl  <- FID (OBRACKET Group? Constraint* CBRACKET)?;
    Group        <- GROUP Multiplicity OBRACKET
                    OPT? FeatureDecl (COMMA OPT? FeatureDecl)*
                    CBRACKET;
    Multiplicity <- ALLOF / ONEOF ;
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
    void:  ROOT     <- WS 'root' WS ;
    void:  GROUP    <- WS 'group' WS ;
    void:  OPT      <- WS 'opt' WS ;
    void:  ALLOF    <- WS 'allof' WS ;
    void:  ONEOF    <- WS 'oneof' WS ;
    void:  REQUIRE  <- WS 'require' WS ;
    void:  EXCLUDE  <- WS 'exclude' WS ;
    void:  WS       <- (COMMENT / <space>)*;
    void:  COMMENT  <- '//' (!EOL .)* EOL ;
    void:  EOL      <- '\n' / '\r' ;
  }

  Grammar create TVLGrm -start S $grm

  namespace export TVLGrm
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

  set s {
    root MultiLingualHelloWorld {
      group allof {
        Language {
          group oneof {
            English, Dutch, German
          }
        },
        opt Repeat
      }
    }
  }
  
  puts stderr [string range $s 128 end]

  $tvlParser parse $s

  ? {$tvlParser parse $s} [list {*}{
    MultiLingualHelloWorld
    Language
    English
    Dutch
    German
    Repeat
  }]
}

# Local variables:
#    mode: tcl
#    tcl-indent-level: 2
#    indent-tabs-mode: nil
# End:
