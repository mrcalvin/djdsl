# -*- Tcl -*-

package req Tcl 8.6
package req textutil 

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
  # Grammar derived from E/BNF available from:
  # https://github.com/Universal-Variability-Language/uvl-parser/blob/master/resources/uvl.bnf
  # 
  # to-dos:
  # - ...
  #
  # limitations:
  # - ...

  set grm {
    #// uvl1 //
    FeatureModel <- Ns? Imports? Features? Constraints? !.;
    Ns <- NAMESPACE Ref;

    Imports <- IMPORTS ( INDENT Import+ DEDENT )?;
    Import <- Ref;
    Features <- FEATURES Children?;
    Children <- INDENT FeatureSpec+ DEDENT;
    FeatureSpec <- Ref Attributes? Groups?;
    Attributes <- OPENATTR Attribute (COMMA Attribute)* CLOSEATTR;
    Attribute  <- Key WS Value? ;
    Groups  <- INDENT Group* DEDENT ;
    Group  <- GroupType Children? ;
    GroupType <- 'or' / 'alternative' / 'mandatory' / 'optional' ;
    Key <- Id;
    Value <- Bool / DQUOTE Str DQUOTE / <digit>+ ;
    Str <- <alpha>+;
    Bool <- 'true' / 'false';
    Constraints <- 'constraints' (INDENT Constraint+ DEDENT)?;
    Constraint <- Term WS '=>' WS Term ;
    Term <- Ref / OPENC Constraint CLOSEC ;
    Ref <- WS Id WS;
    Id <- <alnum>+;

    
    void: IMPORTS      <- WS 'imports' WS ;
    void: NAMESPACE    <- WS 'namespace' WS ;
    void: FEATURES      <- WS 'features' WS ;
    void: CONSTRAINTS   <- WS 'constraints' WS ;
    void: INDENT       <- WS '\uFFFE' WS;
    void: DEDENT       <- WS '\uFFFF' WS;
    void: OPENC        <- WS '(' WS;
    void: CLOSEC       <- WS ')' WS;
    void: OPENATTR     <- WS '{' WS;
    void: CLOSEATTR     <- WS '}' WS;
    void: COMMA         <- WS ',' WS; 
    void: WS           <- (COMMENT / <space>)*;
    void: COMMENT      <- <space>* '//' (!EOL .)* EOL ;
    void: EOL          <- '\n' / '\r' ;
    void: DQUOTE         <- WS '\"' WS; 
    #// end //
  }

  Grammar create UVLGrm -start FeatureModel $grm

  #
  # Adapted from https://wiki.tcl-lang.org/page/An+indentation+syntax+for+Tcl
  #
  # to-dos:
  #     - alternative to mixin?
  #     - review implementation
  #
  
  nx::Class create IndentationPreprocessor {

    :method emit {line} {
      if {[llength ${:last}] > 0} {
        lappend :result [join ${:last} " "]
      }
      foreach x ${:keep} {
        lappend :result $x
      }
      set :keep {}
      set :last [list $line]
    }

    :method dedent {i} {
      while {$i < [lindex ${:levels} end]} {
        set :levels [lreplace ${:levels} end end]
        lappend :last ${:rb}
      }
    }

    :method convert {text} {
      set :lb "\uFFFE"
      set :rb "\uFFFF"

      set :last {}
      set :levels 0
      set :keep {}
      set :result {}
      
      foreach x [split $text \n] {
        regexp {^(\s*)(.*)$} $x - a b
        if {$b eq "" || [string index $b 0] eq "#"} {
          lappend :keep $x
          continue
        }
        set i 0
          foreach y [split $a ""] {
            switch $y {
              " "  { incr i }
              \t { incr i [expr {8 - $i%8}]
              }
            }
          }
        if {$i < [lindex ${:levels} end]} {
          :dedent $i
          if {$i != [lindex ${:levels} end] || [regsub {^(\s*)\\:} $x {\1} x]} {
            lappend :last \\
              }
        } elseif {$i > [lindex ${:levels} end]} {
          lappend :last ${:lb}
          lappend :levels $i
        }
        :emit $x
      }
      
      :dedent 0
      :emit ""

      set r [join ${:result} "\n"]
      while {[llength $r] == 1 && [lindex $r 0] ne $r}  {
        set r [lindex $r 0] ;# strip top level indentation
      }
      return $r
    }

    :public method parse {script} {
      set s [:convert [::textutil::undent $script]]
      puts '$s'
      puts [string range $s 40 end]
      next [list $s]
    }
  }

  #
  # To-dos:
  # ... UVL: multiple roots? what are the model semantics? AND group?
  # ... V1E: [l,k] l=k and l, k > 0 is not supported as a mandatory, AND group encoding? 
  # ... UVL: Add further constraint ops
  # ... UVL: Add imports support (at least CS/AS side)
  # ... UVL: What is the notion of namespaces (multiple per model)?
  # Done:
  # ... UVL: MP groups
  # ... UVL: // not working
  # ... UVL: Attributes

  set ogrm {
    #// uvl2 //
    FeatureModel <- `Model` Ns? Imports? (includes:Includes)? Features? Constraints? !.;
    Ns <- NAMESPACE namespace:Ref;
    # ...
    Imports   <- IMPORTS ( INDENT Import+ DEDENT )?;
    Import    <- Ref; 
    Includes  <- INCLUDES ( INDENT LangLevel+ DEDENT )?;
    LangLevel <- WS Major ('.' (Minor / '*'))? WS;
    Major     <- 'SAT-level' / 'SMT-level' ;
    Minor     <- 'group-cardinality' / 'feature-cardinality' / 'aggregate-function' ;
    Features <- FEATURES root:(`$root setRoot $0` Children) ;
    Children <- INDENT FeatureSpec+ DEDENT;
    FeatureSpec <- `Feature` name:Ref (attributes:Attributes)? (owned:Groups)?;
    Attributes <- OPENATTR Attribute (COMMA Attribute)* CLOSEATTR;
    Attribute  <- `Attribute` key:Key WS (value:Value)? ;
    Groups  <- INDENT Group* DEDENT ;
    Group  <-  MandGroup / OrGroup / XorGroup / OptGroup / MultiplGroup ;
    # MandGroup <- `Choice` lower:(`$current card` MANDATORY) upper:(`$current card` '') candidates:Children ;
    MultiplGroup <- `Choice` OMP lower:(`$current card` '*' / <digit>+) SEPMP
                  upper:(`$current card` '*' / <digit>+) CMP candidates:Children;
    MandGroup <- MANDATORY MandChildren ;
    MandChildren <- INDENT MandSpec+ DEDENT;
    MandSpec <- `Choice` candidates:FeatureSpec;
    OrGroup <- `Choice` upper:(`$current card` OR) candidates:Children ;
    XorGroup <- `Choice` ALTERNATIVE candidates:Children ;
    OptGroup <- OPTIONAL OptChildren;
    OptChildren <- INDENT OptSpec+ DEDENT;
    OptSpec <- `Choice` lower:(`0` '') candidates:FeatureSpec;
    GroupType <- 'optional' ;
    Key <- Id;
    Value <- Bool / DQUOTE Str DQUOTE / <digit>+ ;
    Str <- <alpha>+;
    Bool <- 'true' / 'false';
    Constraints <- CONSTRAINTS (INDENT constraints:Constraint (constraints:Constraint)* DEDENT)?;
    Constraint <- `Constraint` expression:(Term WS IMPL WS Term) ;
    Term <- Ref / OPENC Constraint CLOSEC ;
    Ref <- WS Id WS;
    Id <- <alnum>+;

    
    void: IMPORTS      <- WS 'imports' WS ;
    void: INCLUDES     <- WS 'include' WS ;
    void: NAMESPACE    <- WS 'namespace' WS ;
    void: FEATURES     <- WS 'features' WS ;
    void: CONSTRAINTS  <- WS 'constraints' WS ;
    void: MANDATORY    <- WS 'mandatory' WS ;
    void: OR           <- WS 'or' WS ;
    void: ALTERNATIVE  <- WS 'alternative' WS ;
    void: OPTIONAL     <- WS 'optional' WS ;
    void: IMPL         <- WS '=>' WS ;
    void: INDENT       <- WS '\uFFFE' WS;
    void: DEDENT       <- WS '\uFFFF' WS;
    void: OPENC        <- WS '(' WS;
    void: CLOSEC       <- WS ')' WS;
    void: OPENATTR     <- WS '{' WS;
    void: CLOSEATTR    <- WS '}' WS;
    void: COMMA        <- WS ',' WS;
    void: OMP          <- WS '\[' WS ;
    void: CMP          <- WS '\]' WS ;
    void: DOT          <- '.' ;
    void: SEPMP        <- WS '..' WS ;
    void: WS           <- (COMMENT / <space>)*;
    void: COMMENT      <- '//' (!EOL .)* &EOL ;
    void: EOL          <- '\uFFFE' / '\uFFFF' / '\n' / '\r' ;
    void: DQUOTE         <- WS '\"' WS; 
    #// end //
  }

  Grammar create UVLOGrm -start FeatureModel $ogrm
  
  namespace export UVLGrm UVLOGrm IndentationPreprocessor
} {

  #
  # == Doctests
  #

  namespace import ::djdsl::opeg::*

  #// parserWo //

  set mf [ModelFactory new {
    :object property -accessor public result

    :public object method "input FeatureSpec" {s e args} {
      dict lappend :result features [lindex $args 0]
      return
    }

    :public object method "input Key" {s e v} {
      dict lappend :result attributes $v
      return
    }

    :public object method "input GroupType" {s e v} {
      dict lappend :result groups $v
      return
    }

    :public object method "input Str" {s e v} {
      dict lappend :result strs $v
      return
    }

    :public object method "input Constraint" {s e args} {
      dict incr :result constraints
      return
    }
    
    :public object method getParse {} {
      return [lsort -stride 2 -index 0 ${:result}]
    }
    :public object method reset {} {
      unset -nocomplain :result
    }
  }]
  
  
  set uvlParser [UVLGrm new -factory $mf]
  $uvlParser object mixins add IndentationPreprocessor
  
  set s {
    namespace Server

    features 
      Server { abstract }
        mandatory
          FileSystem
            or
              NTFS
              APFS
              EXT4
          OperatingSystem { abstract }
            alternative
              Windows
              macOS
              Debian
            optional
              Logging { default, logLevel "warn" }

    constraints
      Windows => NTFS
      macOS => APFS
  }
  
  ? {$uvlParser parse $s} [list {*}{
    attributes "abstract abstract default logLevel"
    constraints 2
    features {NTFS APFS EXT4 FileSystem Windows macOS Debian Logging OperatingSystem Server}
    groups "mandatory or alternative optional"
    strs   "warn"
  }]

  package req djdsl::v1e
  namespace import ::djdsl::v1e::*

  nx::Class create UvlModelFactory -superclasses ModelFactory {
    :variable context:object,type=::djdsl::v1e::Model

    :public method generate {nt generator asgmt} {
      if {![info exists :context]} {
        set :context [::djdsl::v1e::Model new]
      }

      if {$generator in {"Choice" "Feature" "Constraint"}} {
        ${:context} define $generator {*}$asgmt
      } elseif {$generator in {"Attribute"}} {
        $generator new {*}$asgmt
      } else {
        if {[llength $asgmt]} {
          ${:context} configure {*}$asgmt
        }
        return ${:context}
      }
    }
  }

  ::nx::ObjectParameterSlot method type=unique {name value} {
    set tmp [lsort -unique $value]
    if {$tmp ne [lsort $value]} {
      error "The list elements in '$value' of parameter $name must be unique."
    }
    return $value
  } 
  
  Model mixins add [nx::Class new {
    :property -accessor public includes:unique,0..*
    :property -accessor public namespace
    # :public method setNamespace {args} {
    #   set r [next [lindex $args end]]
    #   return $r
    # }
    :public method setRoot {args} {
      set o [lindex $args end]
      set old [:featureLookup ""]
      if {$old ne ""} {
        set choice [$old owning get]
        :owned delete $old
        $choice candidates set $o
        $old destroy
      } else {
        # multiple root branch?
        set rc [:define ::djdsl::v1e::Choice -lower 1 -upper 1 -candidates $o]
        lappend :choices $rc
      }
      return $o
    }
  }]

  Choice mixins add [nx::Class new {
    :public method card {args} {
      set r 0
      if {[info exists :candidates]} {
        set r [llength ${:candidates}]
      }
      puts CARD=$r
      return $r
    }
  }]

  Constraint mixins add [nx::Class new {
    :public method register {args} {
      next
      if {[:expression exists]} {
        lassign [:expression get] lhs rhs
        set notFeature [${:model} define ::djdsl::v1e::Feature -name $lhs]
        set notChoice [${:model} define ::djdsl::v1e::Choice -lower 0 -upper 0 -candidates $notFeature]
        set dummyFeature [${:model} define ::djdsl::v1e::Feature -owned $notChoice]
        set implFeature [${:model} define ::djdsl::v1e::Feature -name $rhs]
        set outChoice [${:model} define ::djdsl::v1e::Choice -lower 1 -upper 2 -candidates [list $dummyFeature $implFeature]]
        ${:model} choices add $outChoice
        :expression unset
      }
    }
  }]

  nx::Class create Attribute {
    :property -accessor public key:required
    :property -accessor public value:optional
  }
  
  Feature mixins add [nx::Class new {
    :property -accessor public attributes:object,type=[namespace which Attribute],0..*
  }]

  set s3 {
    namespace Server
    
    features
      Server {abstract}
  }

  set uvlOParser [UVLOGrm new -factory [UvlModelFactory new]]
  $uvlOParser object mixins add IndentationPreprocessor
  set o [$uvlOParser parse $s3]
  ? {$o info class} ::djdsl::v1e::Model
  ? {[$o root get] name get} "Server"
  ? {llength [[$o root get] attributes get]} 1
  ? {[lindex [[$o root get] attributes get] 0] key get} "abstract"
  ? {$o nrValidConfigurations} 1


  set uvlOParser [UVLOGrm new -factory [UvlModelFactory new]]
  $uvlOParser object mixins add IndentationPreprocessor
  set o [$uvlOParser parse $s]
  ? {$o info class} ::djdsl::v1e::Model
  ? {[$o root get] name get} "Server"
  ? {$o nrValidConfigurations} 30
  
  puts [$o getValidConfigurations]
  foreach f [$o getOwnedElements ::djdsl::v1e::Feature] {
    if {[$f name exists]} {
      puts [$f name get]
      if {[$f name get] eq "Logging"} {
        set attrs [$f attributes get]
        ? {llength $attrs} 2
        lassign $attrs attr1 attr2
        ? {$attr1 key get} "default"
        ? {$attr1 value exists} 0
        ? {$attr2 key get} "logLevel"
        ? {$attr2 value get} "warn"
      }
    }
  }

   set s2 {
    namespace Server
    
    features
      Server 
        mandatory
          FileSystem
            [1..*] 
              NTFS 
              APFS
              EXT4  // test
          OperatingSystem
   }

  set uvlOParser [UVLOGrm new -factory [UvlModelFactory new]]
  $uvlOParser object mixins add IndentationPreprocessor
  set o [$uvlOParser parse $s2]
  ? {$o info class} ::djdsl::v1e::Model
  ? {[$o root get] name get} "Server"
  ? {$o nrValidConfigurations} 7

   set s4 {
    namespace Server
    include
      SAT-level
      SMT-level
    features
      Server 
        mandatory
          FileSystem
            [1..*] 
              NTFS 
              APFS
              EXT4  // test
          OperatingSystem
   }

  set uvlOParser [UVLOGrm new -factory [UvlModelFactory new]]
  $uvlOParser object mixins add IndentationPreprocessor
  set o [$uvlOParser parse $s4]
  ? {$o info class} ::djdsl::v1e::Model
  ? {[$o root get] name get} "Server"
  ? {$o includes get} "SAT-level SMT-level"

  set s5 {
    namespace ns
    features
      f 
        mandatory
          a
        optional
          b
        mandatory
          c
  }

   set s5 {
    namespace ns
    features
      f 
        mandatory
          a
          c
        optional
          b
  }

  set uvlOParser [UVLOGrm new -factory [UvlModelFactory new]]
  $uvlOParser object mixins add IndentationPreprocessor
  set o [$uvlOParser parse $s5]
  ? {$o info class} ::djdsl::v1e::Model
  ? {$o nrValidConfigurations} 2
  ? {$o getValidConfigurations} {{a c f} {a c b f}}

  foreach c [$o getOwnedElements ::djdsl::v1e::Choice] {
    puts "$c ([[$c candidates get] name get]) \[[$c lower get]..[$c upper get]\]"
   
  }

  puts [$o asDot]

}

# Local variables:
#    mode: tcl
#    tcl-indent-level: 2
#    indent-tabs-mode: nil
# End:
