%{
open Values
open Syntax
%}

%token AND UNKNOWN ARRAY ASSUMES CALL CLASS DO END ENDEVENT ENDIF ENDPROPERTY ENDTRY EXCEPTION FEATURE GIVES IMPORT INVARIANT MAP NEWMAP PARALLEL PRIVATE PUBLIC REQUIRES SET STRING THROW TYPEOF VAR WITH DIV NOT UNSTABLE AS BIT CASE CONFIG DOWNTO ENDCASE ENDFOR ENDMODULE ENDRULE ENDWHILE EXPORT FOR IF INTEGER IS MODULE OF PASS PROFILE REAL RETHROW SETTER SUBTYPES TO UNION WHEN ZTYPE EOR IN OR SAMPLE ANY ASSERT ASSUME BITS BOOLEAN CAST CATCH CONSTANT DICT ELSE ELSIF ENDCATCH ENDCLASS ENDFUNC ENDGETTER ENDNAMESPACE ENDPACKAGE ENDSETTER ENDTEMPLATE ENUMERATION EVENT EXTENDS EXTERN FUNC GETTER IFF IMPLIES INTERSECT INTRINSIC LET LIST NAMESPACE NEWEVENT OTHERWISE PACKAGE PORT PRAGMA PROPERTY PROTECTED RECORD REPEAT RETURN RULE SHARED SIGNAL TEMPLATE THEN TRY TYPE UNTIL USING WHERE WHILE
%token EOF NEG COMMA LT SHR BAND IMPL SHL RBRACKET RPAR SLICING EQ LBRACE NEQ MINUS BEQ LBRACKET LPAR DOT LEQ POW MUL RDIV EQ_OP BOR PLUS COLON ARROW RBRACE CONCAT COLON_COLON GT PLUS_COLON SEMI_COLON GEQ MOD
%token <string> IDENTIFIER
%token <string> INT_LIT REAL_LIT BITVECTOR_LIT
%token <bool> BOOL_LIT

%type <value> value
%type <expr> expr
%type <stmt> stmt
%type <stmt> pgm

%start pgm

%%

value:
| INT_LIT { VInt (Z.of_string $1)}
| REAL_LIT { VReal (Q.of_string $1)}
| BOOL_LIT { VBool $1 }

unop:
| MINUS  { UMinus }
| NEG    { UBNeg }
| NOT    { UNot }

binop:
| BAND   { BAnd }
| BOR   { BOr }
| IMPL   { BImpl }
| EQ_OP   { Eq }
| NEQ   { NEq }
| GT   { GT }
| GEQ   { Geq }
| LT   { LT }
| LEQ   { Leq }
| PLUS   { Plus }
| MINUS   { Minus }
| OR   { Or }
| AND   { And }
| EOR   { EOR }
| MUL   { Mult }
| RDIV   { RDiv }
| DIV   { DIV }
| MOD   { MOD }
| SHL   { LSh }
| SHR   { RSh }
| POW   { Pow }

expr:
| value                         { ELiteral $1           }
| IDENTIFIER                    { EVar $1               }
| expr binop expr               { EBinop($1, $2, $3)    }
| unop expr                     { EUnop ($1, $2)        }
| expr LBRACKET expr RBRACKET   { EGetArray ($1, $3)    }
| LPAR expr RPAR                { $2                    }

lexpr:
| IDENTIFIER                    { LEVar $1              }
| lexpr LBRACKET expr RBRACKET  { LESetArray ($1, $3)   }

stmt:
| PASS                              { SPass }
| stmt SEMI_COLON stmt              { SThen ($1, $3) }
| lexpr EQ expr                     { SAssign ($1, $3) }
| IF expr THEN stmt ELSE stmt END   { SCond($2, $4, $6) }

pgm:
| stmt SEMI_COLON ? EOF  { $1 }