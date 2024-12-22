{
{-# LANGUAGE DeriveFoldable #-}
module Parser.Parser
   where

import Data.ByteString.Lazy.Char8 (ByteString)
import Data.Maybe (fromJust)
import Data.Monoid (First (..))
import qualified AST.AST as A
import AST.AST ((<->), range)

import qualified Parser.Lexer as L
import qualified Parser.TokenTypes as T

}

%name parse program
%tokentype { L.RangedToken }
%error { parseError }
%monad { L.Alex } { >>= } { pure }
%lexer { lexer } { L.RangedToken T.EOF _ }
%token
  -- Identifiers
  identifier { L.RangedToken (T.Identifier _) _ }
  -- Constants
  string     { L.RangedToken (T.String _) _ }
  integer    { L.RangedToken (T.Integer _) _ }
  -- Keywords
  if         { L.RangedToken T.If _ }
  then       { L.RangedToken T.Then _ }
  else       { L.RangedToken T.Else _ }
  for        { L.RangedToken T.For _ }
  while      { L.RangedToken T.While _ }
  function   { L.RangedToken T.Function _ }
  null       { L.RangedToken T.Null _ }
  break      { L.RangedToken T.Break _ }
  return     { L.RangedToken T.Return _ }
  input      { L.RangedToken T.Input _ }
  output     { L.RangedToken T.Output _ }
  var        { L.RangedToken T.Var _}
  alloc      { L.RangedToken T.Alloc _}

  -- Arithmetic operators
  '+'        { L.RangedToken T.Plus _ }
  '-'        { L.RangedToken T.Minus _ }
  '*'        { L.RangedToken T.Times _ }
  '/'        { L.RangedToken T.Divide _ }
  ':='       { L.RangedToken T.Assign _}
  '.'        { L.RangedToken T.Dot _}
  ';'        { L.RangedToken T.Semi _}
  '&'        {L.RangedToken T.Ampersand _}
  -- Comparison operators
  '='        { L.RangedToken T.Eq _ }
  '<>'       { L.RangedToken T.Neq _ }
  '<'        { L.RangedToken T.Lt _ }
  '<='       { L.RangedToken T.Le _ }
  '>'        { L.RangedToken T.Gt _ }
  '>='       { L.RangedToken T.Ge _ }
  -- Logical operators
  '&&'       { L.RangedToken T.And _ }
  '||'       { L.RangedToken T.Or _ }
  -- Parenthesis
  '('        { L.RangedToken T.LPar _ }
  ')'        { L.RangedToken T.RPar _ }
  '{'        { L.RangedToken T.LBrace _} 
  '}'        { L.RangedToken T.RBrace _} 
  -- Lists
  '['        { L.RangedToken T.LBrack _ }
  ']'        { L.RangedToken T.RBrack _ }
  ','        { L.RangedToken T.Comma _ }
  -- Types
  ':'        { L.RangedToken T.Colon _ }

%right ';'
%left function var
%right else  while if
%nonassoc ':='
%left '||' '&&'
%left '+' '-'
%left '*' '/'
%left NEG
%nonassoc '=' '<>' '<' '>' '<=' '>='
%right alloc
%left '.'
%right '(' '{'
--%left ')' '}'

%%

-- some helper rules

optional(p)
  :   { Nothing }
  | p { Just $1 }

binop 
    : '*'  %shift {$1}
    | '/'  %shift {$1}
    | '-'  %shift {$1}
    | '+'  %shift {$1}
    | '='  %shift {$1}
    | '<>' %shift {$1}
    | '<'  %shift {$1}
    | '>'  %shift {$1}
    | '<=' %shift {$1}
    | '>=' %shift {$1}
    | '&&' %shift {$1}
    | '||' %shift {$1}

unop 
    : '*' {$1}
    | '&' {$1}
    | '-' {$1} 

exp_list
    :      {[]}
    | exp  {[$1]}
    | exp_list ',' exp {$3:$1}

record_list
    : identifier ':' exp  { unTok $1 (\(T.Identifier n) rng -> [A.RecField n $3 ((loc $1) <=> (A.range $3))])}
    | record_list ',' identifier ':' exp {unTok $3 (\(T.Identifier n) rng -> A.RecField n $5 ((loc $3) <=> (A.range $5))) : $1}
exp
    : identifier {unTok $1 (\(T.Identifier n) rng -> A.Id n rng)}
    | integer    {unTok $1 (\(T.Integer n) rng -> A.Number n rng)}
    | input      {unTok $1 (\_ rng -> A.Input rng)}
    | exp '+'  exp %shift {A.Binop $1 A.APlus $3 ($1 <-> $3)}
    | exp '-'  exp %shift {A.Binop $1 A.AMinus $3 ($1 <-> $3)}
    | exp '/'  exp %shift {A.Binop $1 A.ADivide $3 ($1 <-> $3)}
    | exp '*'  exp %shift {A.Binop $1 A.ATimes $3 ($1 <-> $3)}
    | exp '='  exp        {A.Binop $1 A.AEqq $3 ($1 <-> $3)}
    | exp '<>' exp        {A.Binop $1 A.ANEq $3 ($1 <-> $3)}
    | exp '<'  exp        {A.Binop $1 A.ALt $3 ($1 <-> $3)}
    | exp '>'  exp        {A.Binop $1 A.AGt $3 ($1 <-> $3)}
    | exp '<=' exp        {A.Binop $1 A.ALe $3 ($1 <-> $3)}
    | exp '>=' exp        {A.Binop $1 A.AGe $3 ($1 <-> $3)}
    | exp '&&' exp %shift {A.Binop $1 A.ALAnd $3 ($1 <-> $3)}
    | exp '||' exp %shift {A.Binop $1 A.ALOr $3 ($1 <-> $3)}
    | exp '(' exp_list ')' {A.CallExpr $1 $3 ((range $1) <=> (loc $4))}
    | alloc exp  {A.Alloc $2 ((loc $1) <=> (range $2))}
    | '&' identifier {unTok $2 (\(T.Identifier n) rng -> A.VarRef n (loc $1 <=> rng))}
    | '{' record_list '}' {A.Record (reverse $2) (loc $1 <=> (loc $3))}
    | exp '.' identifier %shift {unTok $3 (\(T.Identifier n) rng -> A.FieldAccess $1 n (range $1 <=> rng)) }
    | '*' exp  %prec NEG  {A.Unop A.ADeref $2 (loc $1 <=> (range $2))}
    | '-' exp  %prec NEG  {A.Unop A.AMinus $2 (loc $1 <=> (range $2))}
    |  null               {A.Null (loc $1)}
    | '(' exp ')'  {$2}

lexp 
    : identifier {}
    | '*' identifier {}
    | '&' identifier {}
    -- | '{' record_list '}' {}

stm 
    : lexp ':=' exp ';' {}
    | lexp '.' identifier ':=' exp ';' {}
    | output exp ';'          {}
    | stm stm           %shift{}
    | if '(' exp ')' '{' stm '}' else '{' stm '}' {}
    | if '(' exp ')' '{' stm '}'    {}
    | while '(' exp ')' '{' stm '}' {}

id_list
    :     {[]}
    | identifier {[$1]}
    | id_list ',' identifier {$3:$1}

var_list
    : var id_list ';' {reverse $2}

functionDec
    : identifier '(' id_list ')' '{' var_list stm return exp ';' '}' {}
    | identifier '(' id_list ')' '{' stm return exp ';' '}'          {}

program
    : {[]}
    | functionDec {[$1]}
    | program functionDec {$2:$1}

{

loc :: L.RangedToken -> L.Range
loc (L.RangedToken tok rng) = rng

unTok :: L.RangedToken -> (T.Token -> L.Range -> a) -> a
unTok (L.RangedToken tok rng) ctor = ctor tok rng 


-- | Performs the union of two ranges by creating a new range starting at the
-- start position of the first range, and stopping at the stop position of the
-- second range.
-- Invariant: The LHS range starts before the RHS range.
(<=>) :: L.Range -> L.Range -> L.Range
L.Range a1 _ <=> L.Range _ b2 = (L.Range a1 b2)



-- data ParserState = ParserState { errorList :: [Errors]}

parseError :: L.RangedToken -> L.Alex a
parseError (L.RangedToken rtoken rng) = do
  (L.AlexPn _ line column, _, _, _) <- L.alexGetInput
  -- L.putError (ParserError $ "Parse error at line " <> show rtoken <> " @" <> show line <> ", column " <> show column)
  L.alexError $ "Parse error at line "  <> show rtoken <> " @"<> show line <> ", column " <> show column

lexer :: (L.RangedToken -> L.Alex a) -> L.Alex a
lexer = (=<< L.alexMonadScan)


}
