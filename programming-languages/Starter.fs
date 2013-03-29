module Starter

type MispelledAnimal =
  | Yacc of int
  | Camel of int

let aCamel = Camel(5)
let good animal =
  match animal with
    | Camel(humps) -> humps >= 2
    | Yacc(height) -> height > 2
//printfn "%A is %A" aCamel (good aCamel)

// Interpreting
type ArithS =
  | NumS of int
  | PlusS of ArithS * ArithS
  | MinusS of ArithS * ArithS
  | MultS of ArithS * ArithS
  | UMinuS of ArithS

type ExprC =
  | NumC of int
  | IdC of string
  | AppC of string * ExprC
  | PlusC of ExprC * ExprC
  | MultC of ExprC * ExprC

type FunDefC =
  | Fdc of string * string * ExprC

let rec desugar a =
  match a with
    | NumS (n) -> NumC (n)
    | PlusS (l, r) -> PlusC (desugar l, desugar r )
    | MinusS (l, r) -> PlusC (desugar l, (MultC(NumC(-1), desugar(r))))
    | MultS (l, r) -> MultC (desugar l, desugar r )
    | UMinuS (n) -> MultC(NumC(-1), desugar n )

let rec subs whatC (forS : string) inC =
  match inC with
    | NumC (n) -> inC
    | IdC (s) -> if s = forS then whatC else inC
    | AppC (f, a) -> AppC (f, subs whatC forS a)
    | PlusC (l, r) -> PlusC (subs whatC forS l, subs whatC forS r)
    | MultC (l, r) -> MultC (subs whatC forS l, subs whatC forS r)

let rec interp (a : ExprC) fds =
  match a with 
    | NumC(n) -> n
    | PlusC(l, r) -> interp(l fds) + interp(r fds)
    | MultC(l, r) -> interp(l fds) * interp(r fds)
    | _ -> 0

//let interPrint sub = printfn "%A:\n%A" sub (interp(desugar(sub)))
let tester (sugar, expected) =
  printfn "%A:\nexpected: %A\n%A\n" sugar expected (interp (desugar sugar) 1)
  
tester(PlusS(NumS(4), NumS(5)), 9)
tester(MinusS(NumS(4), NumS(5)), -1)
tester(MultS(NumS(4), NumS(5)), 20)
tester(UMinuS(NumS(4)), -4)


