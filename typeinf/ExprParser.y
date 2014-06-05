{
{- This parser was originally provided by IS, then modified by koba-e964.-}
module ExprParser where

import CDef
import ExprToken
import ExprLexer
import Prelude hiding (EQ, LT, GT)
import qualified Data.List as List
import Control.Exception (throw)

mytrue  = EConst (VBool True)
myfalse = EConst (VBool False)

mynot e = EIf e myfalse mytrue
myand e1 e2 = EIf e1 e2 myfalse 
myor e1 e2  = EIf e1 mytrue e2


parseError :: [Token] -> Either ParseError a
parseError toks = Left $ ParseError $ "parseError: " ++ show toks

commandOfString :: String -> Either ParseError Command
commandOfString = exparse . alexScanTokens
}


%name      exparse
%tokentype {Token}
%error     {parseError} 
%monad {Either ParseError} {(>>=)} {Right}

%token
LET {LET}
EQ {EQ}
REC {REC}
DAND {DAND}
IN {IN}
IF {IF}
THEN {THEN}
ELSE {ELSE}
MATCH {MATCH}
WITH {WITH}
BAR {BAR}
FUN {FUN}
ARROW {ARROW}
TRUE {TRUE}
FALSE {FALSE}
PLUS {PLUS}
MINUS {MINUS}
TIMES {TIMES}
DIV {DIV}
CONS {CONS}
LBRACKET {LBRACKET}
RBRACKET {RBRACKET}
OR {OR}
AND {AND}
LT {LT}
LE {LE}
GT {GT}
GE {GE}
LPAR {LPAR} 
RPAR {RPAR}
EOC {EOC}
EOF {EOF}
INT {INT $$}
ID {ID $$}


%%

command:
  LET var EQ expr EOC { CLet $2 $4 }
| LET REC letrecs EOC { CRLets $3 }
| expr EOC            { CExp $1 }
| EOF                 { CQuit }
;

pat:
  simple_pat CONS pat { PCons $1 $3 }
| simple_pat          { $1 } 
;

simple_pat:
  var           { PVar $1 }
| INT           { PConst (VInt $1) }
| TRUE          { PConst (VBool True) }
| FALSE         { PConst (VBool False) }
| nil           { PNil }
| LPAR pat RPAR { $2 }
;

main_expr: 
  expr EOF { $1 }
;

expr:
  MATCH expr WITH alts         { EMatch $2 $4 }
| LET var EQ expr IN expr      { ELet $2 $4 $6 }
| LET REC letrecs IN expr      { ERLets $3 $5 }
| IF  expr THEN expr ELSE expr { EIf $2 $4 $6 }
| FUN var args ARROW expr      { EFun $2 (List.foldr EFun $5 $3) }
| expr1                        { $1 }
;

exprm: 
  LET var EQ expr IN exprm      { ELet $2 $4 $6 }
| LET REC letrecs IN exprm      { ERLets $3 $5 }
| IF  expr THEN expr ELSE exprm { EIf $2 $4 $6 }
| FUN var args ARROW exprm      { EFun $2 (List.foldr EFun $5 $3) }
| expr1                         { $1 }
;

letrecs:
  letrec               { [$1] }
| letrec DAND letrecs  { $1 : $3 }
;

letrec:
  var var args EQ expr { ($1,$2, List.foldr EFun $5 $3) }
;

args:
 var args { $1 : $2 }
|          { [] }
; 

alts:
  BAR alts1 { $2 }
| alts1     { $1 }
;

alts1:
  alt0 BAR alts1 { $1 : $3 }
| alt            { [$1] }
;

alt:
  pat ARROW expr  { ($1,$3) }
;

alt0:
  pat ARROW exprm { ($1,$3) }
;

var:
  ID { Name $1 }
;

expr1: 
  expr2 OR expr2 { myor $1 $3 }
| expr2          { $1 }
;

expr2: 
  expr3 AND expr3 { myand $1 $3 }
| expr3           { $1 }
;

expr3:
  expr4 GT expr4    { ELt $3 $1 }
| expr4 GE expr4    { mynot (ELt $1 $3) }
| expr4 LT expr4    { ELt $1 $3 }
| expr4 LE expr4    { mynot (ELt $3 $1) }
| expr4 EQ expr4    { EEq $1 $3 } 
| expr4             { $1 }
;

expr4:
  expr5 CONS expr4 { ECons $1 $3 }
| expr5            { $1 }
;

expr5: 
  expr5 PLUS  expr6 { EAdd $1 $3 }
| expr5 MINUS expr6 { ESub $1 $3 }
| expr6             { $1 }
;

expr6:
  expr6 TIMES expr7 { EMul $1 $3 }
| expr6 DIV   expr7 { EDiv $1 $3 }
| expr7             { $1 }
;

expr7:
  app_expr          { $1 }
| MINUS simple_expr { ESub (EConst (VInt 0)) $2 }
; 

app_expr:
  app_expr simple_expr { EApp $1 $2 }
| simple_expr          { $1 }
;
                        

simple_expr:
  INT               { EConst (VInt $1) }
| ID                { EVar   (Name $1) }
| TRUE              { mytrue }
| FALSE             { myfalse } 
| LPAR expr RPAR    { $2 }
| nil               { ENil }
;

nil: 
  LBRACKET RBRACKET {}
;
