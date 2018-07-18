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

  #
  # == Implementation
  #
  
  package req djdsl::lm
  namespace import ::djdsl::lm::*

  #
  # === Abstract syntax
  #
  
  #// lm //
  Asset create Base {
    LanguageModel create Model {
      
      Classifier create Element
      
      Classifier create NamedElement -superclasses Element {
        :property -accessor public name:required,alnum
      }
      
      Classifier create Class -superclasses NamedElement {
        :property -accessor public {root:boolean false}
        :property -accessor public attributes:0..*,object,type=Attribute
        :property -accessor public references:0..*,object,type=Reference
      }
      
      Classifier create Attribute -superclasses NamedElement {
        :property attributeType:object,type=DataType;
      }
      
      Classifier create Reference -superclasses NamedElement {
        :property -accessor public {containment:boolean false}
        :property referenceType
      }
      
      Classifier create DataType -superclasses NamedElement {
        :create String -name "string"
        :create Boolean -name "boolean"
        :create Int -name "integer"
        :create Float -name "float"
      }
    }; # Model
  }; # Base
  #// end //

  Base::Model eval {
    Classifier create [self]::Package -superclasses [self]::NamedElement
    :public method "elements get" {name} {
      foreach c [:info children] {
        if {[$c name get] eq $name} {
          return $c
        }
      }
      return $name
    }
    :public method "datatypes get" {name} {
      :getDataType $name
    }
    :public method "mapSymbol get" {sym} {
      dict set symbols * -1
      return [dict get $symbols $sym]

    }

    # :property -accessor public packages:object,type=Model::Package
    :public method getDataType {name} {
      return "[current class]::DataType::$name"
    }
  }

  Base::Model::Class eval {
    :method init {} {
      if {![info exists :references]} {
        set :references [list]
      }
      if {![info exists :attributes]} {
        set :attributes [list]
      }
    }
  }
  

  #// backends //
  Asset create Backends {
    Collaboration create Mappable {

      Classifier create Store {
        :public method save {args} {}
        :public method delete {args} {}
        :public method get {args} {}
        :method init {} {
          set model [:info parent]
          $model bind [self]
        }
      }
      
      Classifier create Mapper {
        :property context:object
        :public method visit {element:object} {;}
      }
      
      Role create Element {
        :public method accept {mapper:object} {
          $mapper visit [self]
        }
      }
      
      Role create Class {
        :public method accept {mapper:object} {
          next
          if {[info exists :attributes]} {
            foreach attr ${:attributes} {
              $mapper visit $attr
            }
          }
          if {[info exists :references]} {
            foreach ref ${:references} {
              $mapper visit $ref
            }
          }
        }
      }

      :public method bind {context} {
        set mapper [:new mapper -context $context]
        #puts ---[$mapper info precedence]
        set pongoClasses [:info children \
                              -type ::djdsl::pongo::Base::Model::Class]
        foreach pc $pongoClasses {
          $pc accept $mapper
        }
        return [self]
      }
    }; # Mappable

    #// mongodb //
    Collaboration create MongoDB {
  
      Role create Mapper {
        :public method visit {element:object} {
          set classifier [namespace tail [$element info class]]
          :$classifier $element
        }

        # runtime generator (nx::mongo)
        :protected method "Class" {el} {
          set container ${:context}
          set mappedName ${container}::[$el name get]
          set mappedClass [nx::mongo::Class create $mappedName]
          set :currentClass $mappedClass
        }
      }
      
      Role create Store {
        :public method save {obj} {
          $obj save
        }
        :public method delete {obj} {
          $obj delete
        }
      }
    }
    #// end //

    MongoDB::Mapper eval {

        :public method "Attribute" {el} {
          if {[info exists :currentClass]} {
            ${:currentClass} property -accessor public [$el name get]
          }
        }

        :public method "Reference" {el} {
          if {[info exists :currentClass]} {
            set rt [$el cget -referenceType]
            if {[namespace tail $rt] eq $rt} {
              set rt ${:context}::$rt
            } else {
              set rt ${:context}::[$rt name get]
            }
            
            if {[$el containment get]} {
              
              ${:currentClass} property -accessor public \
                  [$el name get]:embedded,type=$rt
              
            } else {
              ${:currentClass} property -accessor public \
                  [$el name get]:reference,type=$rt
            }
          }
        }
      }
  }; # Backends

  #
  # === Structural context conditions
  #
  package req djdsl::ctx
  namespace import ::djdsl::ctx::*

  #// cc1 //
  context Base::Model {

    cond {[llength [:info children]] >= 1}

    cond {[:isRooted]}

    # model method
    op isRooted {} {
      set childType ::djdsl::pongo::Base::Model::Class
      set classes [:info children -type $childType]
      set isRoot [lmap cl $classes {expr {[$cl root get] && 1}}]
      return [expr {[tcl::mathop::+ {*}$isRoot]} == 1]
    }
  }
  #// end //
  
  #
  # === Variability model
  #

  package req djdsl::v1e
  

  #
  # === Concrete syntax(es)
  #

  package req djdsl::dada
  namespace import ::djdsl::dada::*

  #// mb //
  nx::Class create ModelBuilder -superclasses Builder {

    # context variable
    :variable currentClass
    
    # DSL invocation handlers for "class", "attr", "ref", and "val"
    
    :public method "<- class" {name block args} {
      set cl [set :currentClass [${:output} new class -name $name {*}$args]]
      ${:interp} run $block
      unset :currentClass
      return $cl
    }
    
    :public method "<- attr" {typeIdentifier name} {
      if {[info exists :currentClass]} {
        set typeIdentifier [${:output} getDataType $typeIdentifier]
        set newAttribute [${:output} new attribute \
                              -name $name \
                              -attributeType $typeIdentifier]
        ${:currentClass} attributes add $newAttribute
      }
      return
    }
     
    :public method "<- ref" {typeIdentifier name} {
      if {[info exists :currentClass]} {
        set newReference [${:output} new reference \
                              -containment false \
                              -name $name \
                              -referenceType $typeIdentifier]
        ${:currentClass} references add $newReference
        return $newReference
      }
    }
    
    :public method "<- val" {typeIdentifier name} {
      if {[info exists :currentClass]} {
        set newRef [:<- ref $typeIdentifier $name]
        $newRef configure -containment true
        return $newRef
      }
    }   
  }; # ModelBuilder
  #// end //

  ModelBuilder eval {
    #// dbhandler //
    # DSL invocation handler for "@db" annotation
    :public method "<- @db" {args} {
      set args [list {*}$args -root true]
      :<- {*}$args
    }
    #// end //
  }
  
  ModelBuilder eval {

    # entry point
    :public method get {args} {
      next
    }
    
    # dynamic reception
    :public method handleUnknown {mp args} {
      # silently drop multiplicity blocks!
      return
    }

    # instantiation incl. interp wrapper and language-model instance
    :create pongoBuilder \
        -interp [EmptyInterp new] \
        -output [Base new model]
    
  }; # ModelBuilder
  #// end //
  
  Asset create Collections {
    #// collectionsM //
    # Collections::Model
    Collaboration create Model {

      Classifier create StructuralFeature {
        :property -accessor public lowerBound:integer
        :property -accessor public {upperBound:integer 1}

        :public method isMultiValued {} {
          return [expr {${:upperBound} == -1 || ${:upperBound} > 1}]
        }
      }
      
      Role create Reference -superclasses StructuralFeature
      Role create Attribute -superclasses StructuralFeature
    }
    #// end //

    Collaboration create MongoDB+ {
  
      Role create Mapper {
        :public method "Reference" {el args} {
          if {![$el isMultiValued]} {
            next
          } else {
            if {[info exists :currentClass]} {
              set lower [$el lowerBound get]
              if {$lower > 1} {
                set lower 1
              }
              set upper "n"; # right now, nx::mongo does not have have arbitrary mp
              set mp $lower..$upper
              if {[$el containment get]} {
                set container ${:context}
                ${:currentClass} property -incremental -accessor public \
                    [$el name get]:embedded,type=${container}::[$el cget -referenceType],$mp
              } else {
                ${:currentClass} property -incremental -accessor public \
                    [$el name get]:reference,type=${container}::[$el cget -referenceType],$mp
              }
            }
          }
        }

        :public method "Attribute" {el args} {
          next
        }
      }; # Mapper
    }
    
  }

  #// cc2 //
  context Collections::Model::StructuralFeature {
    cond {![info exists :lowerBound] || ${:lowerBound} >= 0}
    cond {${:upperBound} >= -1}
    cond {
      ![info exists :lowerBound] ||
      ${:upperBound} == -1 ||
      ${:upperBound} >= ${:lowerBound}
    }
  }
  #// end //

  nx::Class create CollectionBuilder -superclasses Builder {
    # context variable
    :variable currentRange
    #// mb2 //
    # dynamic reception
    :public method handleUnknown {range args} {
      set :currentRange [string map {.. " "} $range]
      if {[llength ${:currentRange}]==1} {
        set :currentRange [list 0 {*}${:currentRange}]
      }
      if {[lindex ${:currentRange} end] eq "*"} {
        lset :currentRange end -1
      }
      return
    }
    #// end //

    :public method "<- ref" {args} {
      set ref [next]
      if {$ref ne "" && [info exists :currentRange]} {        
        $ref lowerBound set [lindex ${:currentRange} 0]
        $ref upperBound set [lindex ${:currentRange} 1]
        puts "[$ref lowerBound get] -- [$ref upperBound get]"
        unset :currentRange
      }
      return $ref
    }

    :public method "<- attr" {args} {
      set attr [next]
      if {$attr ne "" && [info exists :currentRange]} {
        $attr lowerBound set [lindex ${:currentRange} 0]
        $attr upperBound set [lindex ${:currentRange} 1]
        unset :currentRange
      }
    }
  }

  #
  # Iteration 4
  #

  package req djdsl::opeg
  namespace import ::djdsl::opeg::*


  Grammar create PongoGrm -start P {
    #// opeg1 //
    P          <- `Model` ClsStmt+;
    ClsStmt    <- `Class` root:(`true` DB / `false` !DB) CLASS name:ID
                   OBRACKET StmtList CBRACKET;
    StmtList   <- (Stmt SCOLON)*;
    Stmt       <- attributes:AttrStmt / references:RefStmt;
    RefStmt    <- `Reference` containment:(`false` REF / `true` VAL)
                   referenceType:(`$root elements $0` ID) WS name:ID;
    AttrStmt   <- `Attribute` ATTR attributeType:(`$root datatypes $0` ID)
    		  WS name:ID;
    ID         <- <alnum>+;
    #// end //
    void:  REF     <- WS 'ref' WS ;
    void:  ATTR    <- WS 'attr' WS ;
    void:  VAL     <- WS 'val' WS ;
    void:  CLASS   <- WS 'class' WS ;
    void:  DB      <- WS '@db' WS ;
    void:  OBRACKET <- WS '{' WS ;
    void:  CBRACKET <- WS '}' WS;
    void:  SCOLON   <- WS ';' WS;
    void:  WS       <- (COMMENT / <space>)*;
    void:  COMMENT  <- '//' (!EOL .)* EOL ;
    void:  EOL      <- '\n' / '\r' ;
  }


  set grm {
    #// opeg2 //
               ID <- <alnum>+ MP? ;
               MP <- SQOBRACKET (lowerBound:(<digit>+) SEP)?
        	     upperBound:(`$root mapSymbol $0` '*' / <digit>+)
        	     SQCBRACKET ;
void:  SQOBRACKET <- WS '\[' WS;
void:  SQCBRACKET <- WS '\]' WS;
void:         SEP <- WS '..' WS;
    #// end //
             }

  #// merge //
  Grammar create ExtPongoGrm \
      -start P \
      -merges [PongoGrm] $grm
  #// end //


  namespace export Base pongoBuilder ModelBuilder Backends \
      Collections CollectionBuilder PongoGrm ExtPongoGrm
} {

  #
  # == Doctests
  #

  namespace import ::djdsl::lm::*
  namespace import ::djdsl::dada::EmptyInterp
  
  # Internal DSL (indirect instantiation):
  
  #// builder2 //
  set model [pongoBuilder get {
    class Blog {
      val Post[] posts; 
      val Author[] authors; 
    }
    class Post { 
      attr String title; 
      attr String body; 
      ref Author author; 
    }
    class Author { 
      attr String name; 
      attr String email; 
    }
  }]
  #// end //
  
  ? {$model info class} ::djdsl::pongo::Base::Model
  set pongoClasses [[Base]::Model::Class info instances -closure ${model}::*]
  ? {llength $pongoClasses} 3
  ? {lsort [lmap pc $pongoClasses {$pc name get}]} "Author Blog Post"

  #// comp1 //
  Composition create MongoDBPongo \
      -binds {Backends Base} \
      -base [[Base]::Model] \
      -features [list [[Backends]::Mappable] \
                     [[Backends]::MongoDB]]
  #// end //

  # pongoBuilder output set [MongoDBPongo new model]

  package require nx::mongo
  ::nx::mongo::db connect -db "tutorial"

  ::nx::mongo::db collection tutorial.blogs


  set blogData {
    #// emfatic //
    @db class Blog {
      val Post[*] posts; 
      val Author[*] authors; 
    }
    class Post { 
      attr String title; 
      attr String body; 
      ref Author author; 
    }
    class Author { 
      attr String name; 
      attr String email; 
    }
    #// end //
  }

  #// blogInit //
  ModelBuilder create pongoBuilder \
      -interp [EmptyInterp new] \
      -output [MongoDBPongo new model]

  set blogModel [pongoBuilder get $blogData]
  #// end //
  
  ? {$blogModel info class} ::MongoDBPongo::Model
  ? {::djdsl::pongo::Base::Model isValid $model} 0
  ? {::djdsl::pongo::Base::Model isValid $blogModel} 1
  
  try {
    #// blogAction //
    # 1) obtain a store (incl. MongoDB connection)
    set store [$blogModel new store]

    # 2) create and populate application data
    set blog [${store}::Blog new]
    set post [${store}::Post new -title "A post" -body "Some text"]
    $blog posts set $post

    # 3) persist application data
    $store save $blog
    #// end //
  } finally {
    ::nx::mongo::db drop collection blogs
    
  }

  #// comp2 //
  Composition create MongoDBPongo+ \
      -binds {Collections Backends Base} \
      -base [[Base]::Model] \
      -features [list [[Collections]::Model] \
                     [[Backends]::Mappable] \
                     [[Collections]::MongoDB+] \
                     [[Backends]::MongoDB]]
  #// end //


  # ::nx::mongo::db connect -db "tutorial"
  # ::nx::mongo::db collection tutorial.blogs
  ::nx::mongo::db collection tutorial.blogs

  #// blogInit2 //  
  ModelBuilder create pongoBuilder+ \
      -interp [EmptyInterp new] \
      -predecessors [CollectionBuilder] \
      -output [MongoDBPongo+ new model]

  set blogModel [pongoBuilder+ get $blogData]
  #// end //  

  ? {$blogModel info class} ::MongoDBPongo+::Model

  ::djdsl::pongo::Collections::Model isValid $blogModel
  
  ? {::djdsl::pongo::Collections::Model isValid $blogModel} 1

  try {
    # 1) obtain a store (incl. MongoDB connection)
    set store [$blogModel new store]

    #// blogAction2 //
    # 2) create and populate application data
    set blog [${store}::Blog new]
    set post1 [${store}::Post new -title "A post" -body "Some text"]
    set post2 [${store}::Post new -title "Another post" -body "Some more text"]
    $blog posts add $post1
    $blog posts add $post2
    #// end //

    # 3) persist application data
    $store save $blog

  } finally {
    ::nx::mongo::db drop collection blogs
    # nx::mongo::db close
  }

  #
  # Iteration 4
  #

  namespace import ::djdsl::opeg::*

  #// parserWo //
  set lmf [LanguageModelFactory new \
               -lm [MongoDBPongo]::Model]
  set pongoParser [PongoGrm new -factory $lmf]
  set blogModel [$pongoParser parse {
    @db class Blog {
      val Post posts; 
      val Author authors; 
    }
    class Post { 
      attr String title; 
      attr String body; 
      ref Author author; 
    }
    class Author { 
      attr String name; 
      attr String email; 
    }
  }]
  #// end //

  
  ? {$blogModel info class} ::MongoDBPongo::Model
  set classes [$blogModel info children -type ::djdsl::pongo::Base::Model::Class]
  ? {llength $classes} 3
  ? {lmap c $classes {$c root get}} "false false true"

  ? {::djdsl::pongo::Base::Model isValid $blogModel} 1

  ::nx::mongo::db collection tutorial.blogs
  
  try {
    #// blogAction //
    # 1) obtain a store (incl. MongoDB connection)
    set store [$blogModel new store]

    # 2) create and populate application data
    set blog [${store}::Blog new]
    set post [${store}::Post new -title "A post" -body "Some text"]
    $blog posts set $post
    
    # 3) persist application data
    $store save $blog
    #// end //
  } finally {
    ::nx::mongo::db drop collection blogs
  }

  #// parserWith //
  set lmf [LanguageModelFactory new \
               -lm [MongoDBPongo+]::Model]
  set extPongoParser [ExtPongoGrm new -factory $lmf]
  set blogModel [$extPongoParser parse {
    @db class Blog {
      val Post[*] posts; 
      val Author[*] authors; 
    }
    class Post { 
      attr String title; 
      attr String body; 
      ref Author author; 
    }
    class Author { 
      attr String name; 
      attr String email; 
    }
  }]
  #// end //

  ? {$blogModel info class} ::MongoDBPongo+::Model
  set classes [$blogModel info children -type ::djdsl::pongo::Base::Model::Class]
  ? {llength $classes} 3
  ? {lmap c $classes {$c root get}} "false false true"

  
  nx::mongo::db close

  if {0} {
    nx::mongo::Class create Blog {
      :property posts:embedded,type=Post,0..n
      :property authors:embedded,type=Author,0..n
    }
  }
  
  if {0} {
    set db [Database new]
    $db drop "blog"
    set blog [${model2}::Blog new -db [$db get "blog"]]
    set post [${model2}::Post new -title "A post" -body "Some text"]
    $blog posts add $post
    $blog sync; # $blog save
  }

  if {0} {
    set db [Datastore new]
    $db drop "blog"
    set post [${model2}::Post new -title "A post" -body "Some text"]
    set blog [${model2}::Blog new]
    $blog posts add $post
    ${model2} save $blog; # $blog save
  }


  if {0} {
    set db [Database new]
    $db drop "blog"
    pongoBuilder output set [MongoDBPongo new model -db [$db get "blog"]]
    ${model2}::Blog new 
  }
  

  if {0} {
    # to-dos:
    # - reference types as objects
    # - allow for required properties, by dedicated builders
    # - add Package support, i.e., packages as containers ...
    
  }
  

  
}

# Local variables:
#    mode: tcl
#    tcl-indent-level: 2
#    indent-tabs-mode: nil
# End:
