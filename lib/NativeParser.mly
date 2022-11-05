%{
open AST

let eminus e = EBinop(MINUS, ELiteral (VInt 0), e)
%}

%token AND UNKNOWN ARRAY ASSUMES CALL CLASS DO END ENDEVENT ENDIF ENDPROPERTY ENDTRY EXCEPTION FEATURE GIVES IMPORT INVARIANT MAP NEWMAP PARALLEL PRIVATE PUBLIC REQUIRES SET STRING THROW TYPEOF VAR WITH DIV NOT UNSTABLE AS BIT CASE CONFIG DOWNTO ENDCASE ENDFOR ENDMODULE ENDRULE ENDWHILE EXPORT FOR IF INTEGER IS MODULE OF PASS PROFILE REAL RETHROW SETTER SUBTYPES TO UNION WHEN ZTYPE EOR IN OR SAMPLE ANY ASSERT ASSUME BITS BOOLEAN CAST CATCH CONSTANT DICT ELSE ELSIF ENDCATCH ENDCLASS ENDFUNC ENDGETTER ENDNAMESPACE ENDPACKAGE ENDSETTER ENDTEMPLATE ENUMERATION EVENT EXTENDS EXTERN FUNC GETTER IFF IMPLIES INTERSECT INTRINSIC LET LIST NAMESPACE NEWEVENT OTHERWISE PACKAGE PORT PRAGMA PROPERTY PROTECTED RECORD REPEAT RETURN RULE SHARED SIGNAL TEMPLATE THEN TRY TYPE UNTIL USING WHERE WHILE
%token EOF NEG COMMA LT SHR BAND IMPL SHL RBRACKET RPAR SLICING EQ LBRACE NEQ MINUS BEQ LBRACKET LPAR DOT LEQ POW MUL RDIV EQ_OP BOR PLUS COLON ARROW RBRACE CONCAT COLON_COLON GT PLUS_COLON SEMI_COLON GEQ MOD
%token MEM X PSTATE
%token <string> IDENTIFIER
%token <string> INT_LIT REAL_LIT BITVECTOR_LIT
%token <bool> BOOL_LIT

%type <AST.Parsed.parsed_t> ast

%nonassoc NOT NEG
%left PLUS SHR SHL RDIV OR MUL MINUS EOR DIV BOR BAND AND
%nonassoc GT GEQ EQ_OP LT LEQ NEQ
%right SEMI_COLON

%start ast

%%

plist(X):
| xs = delimited(LPAR, separated_list(COMMA, X), RPAR)
    { xs }

value:
| INT_LIT   { VInt (int_of_string $1) }

%inline unop:
| NEG { NEG }
| NOT { NOT }

%inline binop:
| BAND { BAND }
| BOR { BOR }
| EQ_OP { EQ_OP }
| NEQ { NEQ }
| GT { GT }
| GEQ { GEQ }
| LT { LT }
| LEQ { LEQ }
| PLUS { PLUS }
| MINUS { MINUS }
| OR { OR }
| AND { AND }
| EOR { EOR }
| MUL { MUL }
| RDIV { RDIV }
| DIV { DIV }
| SHL { SHL }
| SHR { SHR }

expr:
| v=value
    { ELiteral v }
| x=IDENTIFIER
    { EVar x }
| e1=expr op=binop e2=expr
    { EBinop (op, e1, e2) }
| MINUS e=expr
    { eminus e }
| op=unop e=expr
    { EUnop (op, e) }
| IF e1=expr THEN e2=expr ELSE e3=expr END
    { ECond (e1, e2, e3) }
| x=IDENTIFIER args=plist(expr)
    { ECall (x, args) }
| LPAR e=expr RPAR
    { e }

lexpr:
| x=IDENTIFIER
    { LEVar x }

stmt:
| PASS
    { SPass }
| stmt SEMI_COLON stmt
    { SThen ($1, $3) }
| lexpr EQ expr
    { SAssign ($1, $3) }
| IF e=expr THEN s1=stmt SEMI_COLON? ELSE s2=stmt SEMI_COLON? END
    { SCond (e, s1, s2) }
| x=IDENTIFIER args=plist(expr)
    { SCall (x, args) }
| RETURN es=loption(plist(expr))
    { SReturn es }

decl:
| CONSTANT x=IDENTIFIER e=expr SEMI_COLON
    { GlobalConst (x, e) }
| TYPE IDENTIFIER OF ENUMERATION LBRACE ls=separated_list(COMMA, IDENTIFIER) RBRACE
    { Enum ls }
| FUNC x=IDENTIFIER args=plist(IDENTIFIER) body=stmt SEMI_COLON? ENDFUNC
    { Func (x, args, body) }

ast:
| ds=decl* EOF
    { ds }
