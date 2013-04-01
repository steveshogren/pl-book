module Starter

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

type Binding =
  | Bind of string * int

let emptyEnv() : Binding list = List.Empty

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
  List.tryFind (fun funElem ->
                  match funElem with 
                    | Fdc(name, arg, expr) -> name = f) fds

let lookup n env =
  match List.tryFind (fun funElem ->
                  match funElem with 
                    | Bind(name, value) -> name = n) env with
    | Some(Bind(name, value)) -> value
    | _ -> failwithf "%A not found" n

let rec interp a (env : Binding list) (fds : FunDefC list)  =
  match a with 
    | NumC(n) -> n
    | PlusC(l, r) -> interp l env fds + interp r env fds 
    | MultC(l, r) -> interp l env fds * interp r env fds 
    | IdC(n) -> lookup n env
    | AppC (f, a) ->
      match getFundef f fds with
        | Some (Fdc(fdcName, fdcArg, fdcBody)) -> 
            let extendedEnv = Bind(fdcArg, interp a env fds) :: emptyEnv()
            interp fdcBody extendedEnv fds
        | _ -> 0

let testDesugarDown (sugar, expected) =
  let wrung = interp (desugar sugar) [] []
  if expected = wrung then 
    printf "."
  else printfn "f\n%A:\nexpected: %A\n%A\n" sugar expected wrung

let testInterpDownExp (desugared, env, funs) =
  try 
      let wrung = interp desugared env funs 
      printfn "f\n%A:\n  expection not thrown\n" desugared
   with | Failure(msg) -> printf "." 

let testInterpDown (desugared, env, funs, expected) =
  let wrung = interp desugared env funs 
  if expected = wrung then 
    printf "."
  else printfn "f\n%A:\nexpected: %A\n%A\n" desugared expected wrung
  
testDesugarDown(PlusS(NumS 4, NumS 5), 9)
testDesugarDown(MinusS(NumS 4, NumS 5), -1)
testDesugarDown(MultS(NumS 4, NumS 5), 20)
testDesugarDown(UMinuS(NumS 4), -4)
testInterpDown(AppC("double", NumC 2), emptyEnv(), 
               [Fdc("double", "x", PlusC(IdC "x",IdC "x"))], 
               4)
testInterpDown(PlusC(NumC 10, AppC("const5", NumC 10)), 
            emptyEnv(), 
            [Fdc("const5", "_", NumC 5)],
            15)

testInterpDown(PlusC(NumC 10, AppC("double", PlusC(NumC 1, NumC 2))), 
            emptyEnv(), 
            [Fdc("double", "x", PlusC(IdC "x", IdC "x"))],
            16)

testInterpDown(PlusC(NumC 10, AppC("quad", PlusC(NumC 1, NumC 2))), 
            emptyEnv(), 
            [Fdc("quad", "x", AppC("double", AppC("double", IdC "x")))
             Fdc("double", "x", PlusC(IdC "x", IdC "x"))],
            22)

testInterpDownExp(AppC("f1", NumC 3),
            emptyEnv(), 
            [Fdc("f1", "x", AppC("f2", NumC 4))
             Fdc("f2", "y", PlusC(IdC "x", IdC "y"))])

printf "\n\n "


