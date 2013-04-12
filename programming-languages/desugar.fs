module desugar 

open interp

type ExprS =
  | NumS of int
  | PlusS of ExprS * ExprS
  | MinusS of ExprS * ExprS
  | MultS of ExprS * ExprS
  | UMinuS of ExprS
  | MsgS of ExprS * string * ExprS // o, n, a
  | LetS of string * ExprS * ExprS // name, bind, body
  | LamS of string * ExprS // param, body
  | ObjS of string list * ExprS list // param, body
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
    | ObjS (ns, functions) -> ObjC (ns, List.map desugar functions)
    | LetS (name,bind,body) -> AppC(LamC (name, desugar (body)), desugar (bind))

