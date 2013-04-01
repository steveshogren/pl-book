module Starter

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

let getFundef (f:string) (fds:FunDefC list) =
  if List.isEmpty fds then
    failwith "No Functions passed"
  else
    List.tryFind (fun funElem ->
                  match funElem with
                    | Fdc(name, arg, expr) -> name = f) fds

let rec interp a (fds : FunDefC list)  =
  match a with 
    | NumC(n) -> n
    | PlusC(l, r) -> interp l fds + interp r fds 
    | MultC(l, r) -> interp l fds * interp r fds 
    | IdC(_) -> failwith "tried to interp an id"
    | AppC (f, a) ->
      match getFundef f fds with
        | Some (x) ->
          match x with
            | Fdc(fdcName, fdcArg, fdcBody) -> interp (subs a fdcArg fdcBody) fds
        | _ -> 0

let testDesugarDown (sugar, expected) =
  let wrung = interp (desugar sugar) []
  if expected = wrung then 
    printf "."
  else printfn "%A:\nexpected: %A\n%A\n" sugar expected wrung

let testInterpDown (desugared, expected, funs) =
  let wrung = interp desugared funs 
  if expected = wrung then 
    printf "."
  else printfn "%A:\nexpected: %A\n%A\n" desugared expected wrung
  
testDesugarDown(PlusS(NumS(4), NumS(5)), 9)
testDesugarDown(MinusS(NumS(4), NumS(5)), -1)
testDesugarDown(MultS(NumS(4), NumS(5)), 20)
testDesugarDown(UMinuS(NumS(4)), -4)
testInterpDown(AppC("double", NumC(2)),4,[Fdc("double", "x", PlusC(IdC("x"),IdC("x")))])


