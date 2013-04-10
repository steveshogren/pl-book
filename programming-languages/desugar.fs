module desugar 

open interp

type ArithS =
  | NumS of int
  | PlusS of ArithS * ArithS
  | MinusS of ArithS * ArithS
  | MultS of ArithS * ArithS
  | UMinuS of ArithS
  | MsgS of ExprS * string * ExprS // o, n, a
  | LetS of string * ExprS * ExprS // name, bind, body
  | LamS of string * ExprS // param, body
  | IdS of string // name

let rec desugar a =
  match a with
    | NumS (n) -> NumC (n)
    | PlusS (l, r) -> PlusC (desugar l, desugar r )
    | MinusS (l, r) -> PlusC (desugar l, (MultC(NumC(-1), desugar(r))))
    | MultS (l, r) -> MultC (desugar l, desugar r )
    | UMinuS (n) -> MultC(NumC(-1), desugar n )
    | MsgS(o,n,a) -> AppC(MsgC(desugar o, n), (desugar a))
    | LamS (param, body) -> LamC (param, desugar (body))
    | IdS (name) -> VarC (name)
    | LetS (name,bind,body) -> SeqC (SetC (name, desugar (bind)), desugar (body))

