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

type ArithC =
  | NumC of int
  | PlusC of ArithC * ArithC
  | MultC of ArithC * ArithC

let rec desugar a =
  match a with
    | NumS (n) -> NumC (n)
    | PlusS (l, r) -> PlusC (desugar l, desugar r )
    | MinusS (l, r) -> PlusC (desugar l, (MultC(NumC(-1), desugar(r))))
    | MultS (l, r) -> MultC (desugar l, desugar r )
    | UMinuS (n) -> MultC(NumC(-1), desugar n )

let rec interp a =
  match a with 
    | NumC(n) -> n
    | PlusC(l, r) -> interp(l) + interp(r)
    | MultC(l, r) -> interp(l) * interp(r)

//let interPrint sub = printfn "%A:\n%A" sub (interp(desugar(sub)))
let tester (sugar, expected) =
  printfn "%A:\nexpected: %A\n%A\n" sugar expected (interp(desugar(sugar)))
  
tester(PlusS(NumS(4), NumS(5)), 9)
tester(MinusS(NumS(4), NumS(5)), -1)
tester(MultS(NumS(4), NumS(5)), 20)
tester(UMinuS(NumS(4)), -4)


