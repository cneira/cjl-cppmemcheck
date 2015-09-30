(ns linter.ebnf-cpp
  (:require [instaparse.core :as insta]
            [clojure.string :as str ]))

 (def lexical-cpp
   (str
       "identifier =  <' '*> (#'[a-z]+' | #'[A-Z]+') | ( #'[0-9]*' | (#'[a-z]+' | #'[A-Z]+'))*  <' '*> ;
        literal = integer_literal   |
                  character_literal |
                  floating_literal  |
                  string_literal    |
                  boolean_literal ;
        integer_literal = INTEGER ;
        character_literal = CHARACTER ;
        floating_literal = FLOATING ;
        string_literal = STRING ;
        boolean_literal = TRUE | FALSE ;"))

(def cpp-translation-unit
  (str "translation_unit= declaration_seq?;"))

(def cpp-expressions
  (str
    "
        primary_expression = literal | THIS | COLONCOLON identifier | COLONCOLON operator_function_id | COLONCOLON qualified_id | '(' expression ')' | id_expression ;
        id_expression = unqualified_id | qualified_id ;
        unqualified_id = identifier | operator_function_id | conversion_function_id | '~' class_name | template_id ;
        qualified_id = nested_name_specifier TEMPLATE? unqualified_id ;
        nested_name_specifier = class_or_namespace_name COLONCOLON nested_name_specifier? ;
        class_or_namespace_name = class_name | namespace_name ;
        postfix_expression =
             primary_expression
             | postfix_expression '[' expression ']'
            | postfix_expression '(' expression_list? ')'
            | simple_type_specifier '(' expression_list? ')'
            | postfix_expression '.' TEMPLATE? COLONCOLON? id_expression
                                       | postfix_expression ARROW TEMPLATE? COLONCOLON? id_expression
                                       | postfix_expression '.' pseudo_destructor_name
                                       | postfix_expression ARROW pseudo_destructor_name
                                       | postfix_expression PLUSPLUS
                                       | postfix_expression MINUSMINUS
                                       | DYNAMIC_CAST '<' type_id '>' '(' expression ')'
              | STATIC_CAST '<' type_id '>' '(' expression ')'
              | REINTERPRET_CAST '<' type_id '>' '(' expression ')'
              | CONST_CAST '<' type_id '>' '(' expression ')'
              | TYPEID '(' expression ')'
              | TYPEID '(' type_id ')'
              ;
        expression_list = assignment_expression | expression_list ',' assignment_expression ;
        pseudo_destructor_name =
                          COLONCOLON? nested_name_specifier? type_name COLONCOLON '~' type_name
                          | COLONCOLON? nested_name_specifier? '~' type_name
                          ;
        unary_expression =
                          postfix_expression
                          | PLUSPLUS cast_expression
                          | MINUSMINUS cast_expression
                          | unary_operator cast_expression
                          | SIZEOF unary_expression
                          | SIZEOF '(' type_id ')'
                          | new_expression
                          | delete_expression
                                      ;

        unary_operator = '*' | '&' | '+' | '-' | '!' | '~' ;
        new_expression = COLONCOLON? NEW new_placement? new_type_id new_initializer? | COLONCOLON? NEW new_placement? '(' type_id ')' new_initializer? ;
        new_placement = '(' expression_list ')' ;
        new_type_id = type_specifier_seq new_declarator? ;
        new_declarator = ptr_operator new_declarator? | direct_new_declarator ;
        direct_new_declarator = '[' expression ']' | direct_new_declarator '[' constant_expression ']' ;
        new_initializer = '(' expression_list? ')' ;
        delete_expression = COLONCOLON? DELETE cast_expression | COLONCOLON? DELETE '[' ']' cast_expression ;
        cast_expression = unary_expression | '(' type_id ')' cast_expression ;
        pm_expression = cast_expression | pm_expression DOTSTAR cast_expression | pm_expression ARROWSTAR cast_expression ;
        multiplicative_expression = pm_expression | multiplicative_expression '*' pm_expression | multiplicative_expression '/' pm_expression | multiplicative_expression '%' pm_expression ;
        additive_expression = multiplicative_expression | additive_expression '+' multiplicative_expression | additive_expression '-' multiplicative_expression ;
        shift_expression = additive_expression | shift_expression SL additive_expression | shift_expression SR additive_expression ;
        relational_expression = shift_expression | relational_expression '<' shift_expression | relational_expression '>' shift_expression | relational_expression LTEQ shift_expression | relational_expression GTEQ shift_expression ;
        equality_expression = relational_expression | equality_expression EQ relational_expression | equality_expression NOTEQ relational_expression ;
        and_expression = equality_expression | and_expression '&' equality_expression ;
        exclusive_or_expression = and_expression | exclusive_or_expression '^' and_expression ;
        inclusive_or_expression = exclusive_or_expression | inclusive_or_expression '|' exclusive_or_expression ;
        logical_and_expression = inclusive_or_expression | logical_and_expression ANDAND inclusive_or_expression ;
        logical_or_expression = logical_and_expression | logical_or_expression OROR logical_and_expression ;
        conditional_expression = logical_or_expression | logical_or_expression  '?' expression ':' assignment_expression ;
        assignment_expression = conditional_expression | logical_or_expression assignment_operator assignment_expression | throw_expression ;
        assignment_operator = '=' | MULEQ | DIVEQ | MODEQ | ADDEQ | SUBEQ | SREQ | SLEQ | ANDEQ | XOREQ | OREQ ;
        expression = assignment_expression | expression ',' assignment_expression ;
        constant_expression = conditional_expression ; "
    )
  )

(def cpp-statements
  (str
    "statement =
        labeled_statement
        | expression_statement
        | compound_statement
        | selection_statement
        | iteration_statement
        | jump_statement
        | declaration_statement
        | try_block ;
        labeled_statement = identifier ':' statement
        | CASE constant_expression ':' statement
        | DEFAULT ':' statement;
        expression_statement = expression? ';' ;
        compound_statement = '{' statement_seq? '}' ;
        statement_seq = statement | statement_seq statement ;
        selection_statement = IF '(' condition ')' statement | IF '(' condition ')' statement ELSE statement | SWITCH '(' condition ')' statement ;
        condition= expression | type_specifier_seq declarator '=' assignment_expression ;
        iteration_statement = WHILE '(' condition ')' statement | DO statement WHILE '(' expression ')' ';' | FOR '(' for_init_statement condition? ';' expression? ')' statement ;
        for_init_statement = expression_statement | simple_declaration ;
        jump_statement = BREAK ';' | CONTINUE ';' | RETURN expression? ';' | GOTO identifier ';' ;
        declaration_statement = block_declaration ;"))

(def cpp-declarations

  (str
    "   <declaration_seq>=
        declaration
        | declaration_seq declaration
        ;

        declaration=
        block_declaration
        | function_definition
        | template_declaration
        | explicit_instantiation
        | explicit_specialization
        | linkage_specification
        | namespace_definition
        ;

        block_declaration=
        simple_declaration
        | asm_definition?
        | namespace_alias_definition
        | using_declaration
        | using_directive
        ;

        simple_declaration=
        decl_specifier_seq? init_declarator_list? ';'
            ;

        decl_specifier=
        storage_class_specifier
        | type_specifier
        | function_specifier
        | FRIEND
        | TYPEDEF
        ;

        decl_specifier_seq=
        decl_specifier_seq? decl_specifier
        ;

        storage_class_specifier=
        AUTO
        | REGISTER
        | STATIC
        | EXTERN
        | MUTABLE
        ;

        function_specifier=
        INLINE
        | VIRTUAL
        | EXPLICIT
        ;

        typedef_name=
        identifier
        ;

        type_specifier=
        simple_type_specifier
        | class_specifier
        | enum_specifier
        | elaborated_type_specifier
        | cv_qualifier
        ;

        simple_type_specifier=
        COLONCOLON? nested_name_specifier? type_name
        | CHAR
        | WCHAR_T
        | BOOL
        | SHORT
        | INT
        | LONG
        | SIGNED
        | UNSIGNED
        | FLOAT
        | DOUBLE
        | VOID
        ;

        type_name=
        class_name
        | enum_name
        | typedef_name
        ;

        elaborated_type_specifier=
        class_key COLONCOLON? nested_name_specifier? identifier
        | ENUM COLONCOLON? nested_name_specifier? identifier
        | TYPENAME COLONCOLON? nested_name_specifier identifier
        | TYPENAME COLONCOLON? nested_name_specifier identifier '<' template_argument_list '>'
        ;

        enum_name=
        identifier
        ;

        enum_specifier=
        ENUM identifier? '{' enumerator_list? '}'
          ;

          enumerator_list=
                                                    enumerator_definition
                              | enumerator_list ',' enumerator_definition
                              ;

                                                    enumerator_definition=
                              enumerator
                                                    | enumerator '=' constant_expression
                              ;

                              enumerator=
                              identifier
                              ;

                                                    namespace_name=
                              original_namespace_name
                                                    | namespace_alias
                              ;

                                                    original_namespace_name=
                              identifier
                              ;

                                                    namespace_definition=
                              named_namespace_definition
                                                    | unnamed_namespace_definition
                              ;

                                                    named_namespace_definition=
                              original_namespace_definition
                                                    | extension_namespace_definition
                              ;

                                                    original_namespace_definition=
                              NAMESPACE identifier '{' namespace_body '}'
          ;

          extension_namespace_definition=
                                                                      NAMESPACE original_namespace_name '{' namespace_body '}'
          ;

          unnamed_namespace_definition= NAMESPACE '{' namespace_body '}' ;
          namespace_body= declaration_seq? ;
          namespace_alias= identifier ;
          namespace_alias_definition=
                                     NAMESPACE identifier '=' qualified_namespace_specifier ';'
                                                                           ;

          qualified_namespace_specifier=
                                        COLONCOLON? nested_name_specifier? namespace_name;
          using_declaration= USING TYPENAME? COLONCOLON? nested_name_specifier unqualified_id ';' | USING COLONCOLON unqualified_id ';' ;
          using_directive= USING NAMESPACE COLONCOLON? nested_name_specifier? namespace_name ';' ;
          asm_definition= ASM '(' string_literal ')' ';' ;
          linkage_specification= EXTERN string_literal '{' declaration_seq? '}' | EXTERN string_literal declaration ;"

    )

  )

(def cpp-declarators

  (str
    "    init_declarator_list=
          init_declarator
          | init_declarator_list ',' init_declarator
          ;

          init_declarator=
          declarator initializer?
          ;

          declarator=
          direct_declarator
          | ptr_operator declarator
          ;

          direct_declarator=
          declarator_id
          | direct_declarator '('parameter_declaration_clause ')' cv_qualifier_seq? exception_specification?
                                 | direct_declarator '[' constant_expression? ']'
              | '(' declarator ')'
              ;

              ptr_operator=
                   '*' cv_qualifier_seq?
                   | '&'
                   | COLONCOLON? nested_name_specifier '*' cv_qualifier_seq?
                   ;

                   cv_qualifier_seq=
                   cv_qualifier cv_qualifier_seq?
                   ;

                   cv_qualifier=
                   CONST
                   | VOLATILE
                   ;

                   declarator_id=
                   COLONCOLON? id_expression
                   | COLONCOLON? nested_name_specifier? type_name
                   ;

                   type_id=
                   type_specifier_seq abstract_declarator?
                   ;

                   type_specifier_seq=
                   type_specifier type_specifier_seq?
                   ;

                   abstract_declarator=
                   ptr_operator abstract_declarator?
                   | direct_abstract_declarator
                   ;

                   direct_abstract_declarator=
                   direct_abstract_declarator? '(' parameter_declaration_clause ')' cv_qualifier_seq? exception_specification?
                                                     | direct_abstract_declarator? '[' constant_expression? ']'
                  | '(' abstract_declarator ')'
                  ;

                  parameter_declaration_clause=
                       parameter_declaration_list? ELLIPSIS?
                       | parameter_declaration_list ',' ELLIPSIS
                       ;

                       parameter_declaration_list=
                       parameter_declaration
                       | parameter_declaration_list ',' parameter_declaration
                       ;

                       parameter_declaration=
                       decl_specifier_seq declarator
                       | decl_specifier_seq declarator '=' assignment_expression
                       | decl_specifier_seq abstract_declarator?
                       | decl_specifier_seq abstract_declarator? '=' assignment_expression
                       ;

                       function_definition=
                       decl_specifier_seq? declarator ctor_initializer? function_body
                       | decl_specifier_seq? declarator function_try_block
                       ;

                       function_body=
                       compound_statement
                       ;

                       initializer=
                       '=' initializer_clause
                       | '(' expression_list ')'
                    ;

                    initializer_clause=
                            assignment_expression
                            | '{' initializer_list COMMA? '}'
                      | '{' '}'
                                  ;

                                  initializer_list=
                          initializer_clause
                              | initializer_list ',' initializer_clause
                          ;"

     )

  )

(def cpp-classes

  (str

    " class_name=
     identifier
     | template_id
     ;

     class_specifier=
     class_head '{' member_specification? '}'
       ;

       class_head=
                    class_key identifier? base_clause?
                  | class_key nested_name_specifier identifier base_clause?
                  ;

                  class_key=
                  CLASS
                                             | STRUCT
                                             | UNION
                  ;

       member_specification=
                  member_declaration member_specification?
                  | access_specifier ':' member_specification?
                  ;

                  member_declaration=
                                             decl_specifier_seq? member_declarator_list? ';'
                                                 | function_definition SEMICOLON?
                  | qualified_id ';'
       | using_declaration
                  | template_declaration
                  ;

                  member_declarator_list=
                  member_declarator
                  | member_declarator_list ',' member_declarator
                  ;

                  member_declarator=
                  declarator pure_specifier?
                  | declarator constant_initializer?
                  | identifier? ':' constant_expression
                  ;

                  pure_specifier=
                  '=' '0'
                  ;

                  constant_initializer=
                  '=' constant_expression
                  ;"

    ))

(def cpp-derived-classes

  (str
    "
        base_clause=
        ':' base_specifier_list
        ;

        base_specifier_list=
        base_specifier
        | base_specifier_list ',' base_specifier
        ;

        base_specifier=
        COLONCOLON? nested_name_specifier? class_name
        | VIRTUAL access_specifier? COLONCOLON? nested_name_specifier? class_name
        | access_specifier VIRTUAL? COLONCOLON? nested_name_specifier? class_name
        ;

        access_specifier=
        PRIVATE
        | PROTECTED
        | PUBLIC
        ;
    "))

(def cpp-special-member-functions

  (str
    "
        conversion_function_id=
        OPERATOR conversion_type_id
        ;

        conversion_type_id=
        type_specifier_seq conversion_declarator?
        ;

        conversion_declarator=
        ptr_operator conversion_declarator?
        ;

        ctor_initializer=
        ':' mem_initializer_list
        ;

        mem_initializer_list=
        mem_initializer
        | mem_initializer ',' mem_initializer_list
        ;

        mem_initializer=
        mem_initializer_id '(' expression_list? ')'
          ;

          mem_initializer_id=
                              COLONCOLON? nested_name_specifier? class_name
                              | identifier
                              ;"
    )
  )

(def cpp-overloading
  (str

    "
        operator_function_id=
        OPERATOR operator
        ;

        operator:
        NEW
        | DELETE
        | NEW '[' ']'
          | DELETE '[' ']'
          | '+'
                     | '_'
                     | '*'
                     | '/'
                     | '%'
          | '^'
          | '&'
                     | '|'
                     | '~'
          | '!'
                     | '='
                     | '<'
                     | '>'
                     | ADDEQ
                     | SUBEQ
                     | MULEQ
                     | DIVEQ
                     | MODEQ
                     | XOREQ
                     | ANDEQ
                     | OREQ
                     | SL
                     | SR
                     | SREQ
                     | SLEQ
                     | EQ
                     | NOTEQ
                     | LTEQ
                     | GTEQ
                     | ANDAND
                     | OROR
                     | PLUSPLUS
                     | MINUSMINUS
                     | ','
          | ARROWSTAR
                     | ARROW
                     | '(' ')'
          | '[' ']'
          ;
    "
    )
  )

(def keywords

  (str
    "
    ASM = 'asm';
    AUTO = 'auto';
    BOOL = 'bool';
    BREAK = 'break';
    CASE = 'case'  ;
    CATCH = 'catch' ;
    CHAR = 'char' ;
    CLASS = 'class' ;
    CONST = 'const' ;
    CONST_CAST = 'const_cast' ;
    CONTINUE = 'continue' ;
    DEFAULT = 'default' ;
    DELETE = 'delete' ;
    DO = 'do' ;
    DOUBLE = 'double' ;
    DYNAMIC_CAST = 'dynamic_cast' ;
    ELSE = 'else' ;
    ENUM = 'enum' ;
    EXPLICIT = 'explicit' ;
    EXPORT = 'export' ;
    EXTERN = 'extern' ;
    FALSE = 'false' ;
    FLOAT = 'float' ;
    FOR  =  'for' ;
    FRIEND = 'friend' ;
    GOTO = 'goto' ;
    IF = 'if'  ;
    INLINE = 'inline' ;
    INT = 'int' ;
    LONG = 'long' ;
    MUTABLE = 'mutable' ;
    NAMESPACE = 'namespace' ;
    NEW = 'new' ;
    OPERATOR = 'operator' ;
    PRIVATE = 'private' ;
    PROTECTED = 'protected' ;
    PUBLIC = 'public' ;
    REGISTER = 'register' ;
    REINTERPRET_CAST = 'reinterpret_cast' ;
    RETURN = 'return' ;
    SHORT = 'short' ;
    SIGNED = 'signed' ;
    SIZEOF = 'sizeof' ;
    STATIC = 'static' ;
    STATIC_CAST = 'static_cast' ;
    STRUCT = 'struct' ;
    SWITCH = 'switch' ;
    TEMPLATE = 'template' ;
    THIS = 'this' ;
    THROW = 'throw' ;
    TRUE = 'true' ;
    TRY = 'try' ;
    TYPEDEF = 'typedef' ;
    TYPEID = 'typeid' ;
    TYPENAME = 'typename' ;
    UNION = 'union' ;
    UNSIGNED = 'unsigned' ;
    USING = 'using' ;
    VIRTUAL = 'virtual' ;
    VOID = 'void' ;
    VOLATILE = 'volatile' ;
    WCHAR_T = 'wchar_t' ;
    COLONCOLON = '::' ;
    SEMICOLON = ';' ;\n
    OROR = '||' ;
    EQ = '=' ;
    SL = '<<' ;
    SR = '>>' ;
    LTEQ = '<=' ;
    GTEQ = '>=';
    ANDAND = '&&' ;
    NOTEQ = '!=';
    STRING = 'string';
    MULEQ = '*=';
    DIVEQ = '/=' ;
    MODEQ = '%=';
    ADDEQ = '+=';
    SUBEQ = '-=';
    SREQ = '>>=';
    SLEQ = '<<=';
    ANDEQ = '&=';
    XOREQ = '&|=';
    OREQ = '|=';
    INTEGER = 'int' ;
    PLUSPLUS = '++';
    MINUSMINUS = '--' ;
    COMMA = ',';
    ELLIPSIS = '...';
    DOTSTAR = '.*';
    ARROWSTAR = '->*';
    ARROW = '->' ;
    CHARACTER = #'[a-z]+' | #'[A-Z]+'
    IDENTIFIER = 'identifier';
    FLOATING = 'float';
    WHILE = 'while';"))


(def cpp-templates
  (str
    " template_declaration =
     EXPORT?  TEMPLATE '<' template_parameter_list '>' declaration
     ;

     template_parameter_list =
     template_parameter
     | template_parameter_list ',' template_parameter
     ;

     template_parameter =
     type_parameter
     | parameter_declaration
     ;

     type_parameter =
     CLASS identifier?
     | CLASS identifier? '=' type_id
     | TYPENAME identifier?
     | TYPENAME identifier? '=' type_id
     | TEMPLATE '<' template_parameter_list '>' CLASS identifier?
     | TEMPLATE '<' template_parameter_list '>' CLASS identifier? '=' template_name
     ;

     template_id =
     template_name '<' template_argument_list '>'
     ;

     template_name =
     identifier
     ;

     template_argument_list =
     template_argument
     | template_argument_list ',' template_argument
     ;

     template_argument =
     assignment_expression
     | type_id
     | template_name
     ;

     explicit_instantiation =
     TEMPLATE declaration
     ;

     explicit_specialization =
     TEMPLATE '<' '>' declaration
     ;")
  )



(def cpp-exception-handling
  (str

    "
        try_block=
        TRY compound_statement handler_seq
        ;

        function_try_block=
        TRY ctor_initializer? function_body handler_seq
        ;

        handler_seq=
        handler handler_seq?
        ;

        handler=
        CATCH '(' exception_declaration ')' compound_statement
                 ;

                 exception_declaration=
                 type_specifier_seq declarator
                 | type_specifier_seq abstract_declarator
                 | type_specifier_seq
                 | ELLIPSIS
                 ;

                 throw_expression=
                 THROW assignment_expression?
                 ;

                 exception_specification=
                 THROW '(' type_id_list? ')'
            ;

            type_id_list=
                          type_id
                          | type_id_list ',' type_id
                          ;
    "
    )

  )


(defn  init_cpp_grammar []
 (str
   cpp-declarations
   cpp-declarators
   cpp-statements
                cpp-translation-unit
                cpp-expressions
                cpp-classes
                cpp-derived-classes
                cpp-special-member-functions
                cpp-templates
                cpp-overloading
                cpp-exception-handling
                lexical-cpp
                keywords
                ))



(def cpp-parser
  (insta/parser (init_cpp_grammar)  ))

(init_cpp_grammar )

(cpp-parser " int a[23];")

(cpp-parser " int a = new; int a; int  *b = calloc(10 );")




(defn  init_cpp_grammar2 []
  (str
    ; cpp-statements
    ;cpp-expressions
    ; cpp-declarations
    ; cpp-declarators
    lexical-cpp
    keywords
    ))

(def cpp-parser
  (insta/parser (init_cpp_grammar2)  ))

(cpp-parser "int a ;")
