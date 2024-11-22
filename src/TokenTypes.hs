module TokenTypes (Token(..)) where
import GHC.IO.Encoding (CodingProgress(InputUnderflow))

data Token
  = If
  | Then
  | Else
  | While
  | For
  | Var
  | Return
  | Null
  | Function
  | Break
  | Plus
  | Minus
  | Times
  | Divide
  | Assign
  | Dot
  | Semi
  | Ampersand
  | Eq
  | Neq
  | Lt
  | Le
  | Gt
  | Ge
  | And
  | Or
  | LPar
  | RPar
  | LBrace
  | RBrace
  | LBrack
  | RBrack
  | Comma
  | Colon
  | Arrow
  | Quote
  | Identifier String
  | Integer Int
  | String String
  | Input
  | Output
  | Alloc
  | EOF
  deriving (Eq, Show)
