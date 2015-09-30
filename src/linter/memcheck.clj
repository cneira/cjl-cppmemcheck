(ns linter.memcheck
  (:require [instaparse.core :as insta]
            [clojure.string :as str ]))

;taken from http://www.nongnu.org/hcb/#nondigit
(def cpp-keyword
  (str "KEYWORD= 'alignas'  | 'alignof'  | 'asm'  | 'auto'   | 'break'  | 'case'"
       "| 'catch'   | 'class'  | 'const' |'constexpr' "
       " | 'const_cast'  | 'continue'  | 'decltype'  | 'default'  | 'delete'  | 'do' "
       " | 'double'  | 'dynamic_cast'  | 'else'  | 'enum'  | 'explicit'  | 'export' "
       " | 'extern'  | 'false'    | 'for'  | 'friend'  | 'goto'  | 'if'  | "
       " 'inline'  | 'mutable'  | 'namespace'  | 'new'  | "
       " 'noexcept'  | 'nullptr' | 'operator'  | 'private'  | 'protected'  | 'public' "
       " | 'register'  | 'reinterpret_cast' | 'return' |  'signed' | "
       " 'sizeof' | 'static' | 'static_assert' | 'static_cast' | 'struct' | "
       " 'switch' | 'template' | 'this' | 'thread_local' | 'throw' | 'true' |"
       " 'try' | 'typedef' | 'typeid' | 'typename'  | 'union' "
       " | 'unsigned'  | 'using'  | 'virtual'  | 'void'  | 'volatile'  | 'wchar_t'  | 'while' ;")
  )
(def non-digit
  (str
    "<NONDIGIT>= ('a' |'b'| 'c' | 'd' | 'e' | 'f' | 'g' | 'h' | 'i' | 'j' | 'k' | 'l' | 'm' | 'n' | 'o' | 'p' | 'q' | 'r' |"
    "'s' | 't' | 'u' | 'v' | 'w' | 'x' | 'y' | 'z' | 'A' | 'B' | 'C' | 'D' | 'E' | 'F' | 'G' | 'H' | 'I' | 'J' | 'K'"
    "| 'L' | 'M' | 'N' | 'O' | 'P' | 'Q' | 'R' | 'S' | 'T' | 'U' | 'V' | 'W' | 'X' | 'Y' | 'Z') ;"))

(def digit (str "<DIGIT>= '0' '1' | '2' | '3' | '4' | '5' | '6' | '7' | '8' | '9' ;"))

(def variable (str "VARIABLE= <' '*> (#'[a-z]+' | #'[A-Z]+') | ( #'[0-9]*' | (#'[a-z]+' | #'[A-Z]+'))*  <' '*> ;
                    <EOF>=(<';'>| <'\n'*>)+  ;
                    <SPACES>= <' '*> ;
                    ELEMENTS= <'['> (NONDIGIT*|DIGIT*) <']'> ;
                    DATATYPE= 'int'  | 'long' | 'char'  | 'char16_t' | 'char32_t' | 'bool'| 'float' | 'short'  ;
                    DECLARATION =  DATATYPE* SPACES VARIABLE SPACES EOF;
                    "
                   digit non-digit cpp-keyword ))

(def mem-alloc-grammar
  (str "

     ALLOC_HEAP = (DATATYPE*|VARIABLE) SPACES VARIABLE SPACES '=' SPACES MEM_ALLOC_OPERATOR SPACES DATATYPE* SPACES VARIABLE SPACES;
     <SPACES> = <' '*>;
     MEM_ALLOC_OPERATOR = 'new'  | 'calloc' ; " variable ))

(def cpp-parser (insta/parser mem-alloc-grammar))

(cpp-parser "int  a ;")