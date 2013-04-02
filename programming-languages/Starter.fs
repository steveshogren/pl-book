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
  | AppC of ExprC * ExprC
  | PlusC of ExprC * ExprC
  | MultC of ExprC * ExprC
  | FdC of string * string * ExprC

type Value =
  | NumV of int
  | FunV of string * string * ExprC

type Binding =
  | Bind of string * Value

let emptyEnv : Binding list = List.Empty

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

let lookup n env =
  match List.tryFind (fun funElem ->
                  match funElem with 
                    | Bind(name, value) -> name = n) env with
    | Some(Bind(name, value)) -> value
    | _ -> failwithf "%A not found" n


let rec interp a (env : Binding list) : Value =
  match a with 
    | NumC(n) -> NumV n
    | IdC(n) -> lookup n env
    | FdC(n, a, b) -> FunV(n, a, b)
    | PlusC(l, r) -> arithNum l r (fun x y -> x + y) env
    | MultC(l, r) -> arithNum l r (fun x y -> x * y) env
    | AppC (f, a) ->
      match interp f env with
        | FunV(fdcName, funVArg, funVBody) -> 
            let extendedEnv = Bind(funVArg, interp a env) :: emptyEnv
            interp funVBody extendedEnv 
        | _ -> failwith "something went wrong"
and arithNum l r func env =
  let lr = interp l env 
  let rr = interp r env 
  match lr,rr with
    | NumV (l), NumV (r) -> NumV(func l r)
    | _ -> failwith "Tried to arith something other than a number"

let testDesugarDown (sugar, expected) =
  let wrung = interp (desugar sugar) []
  if expected = wrung then 
    printf "."
  else printfn "f\n%A:\nexpected: %A\n%A\n" sugar expected wrung

let testInterpDownExp (desugared, env) =
  try 
      let wrung = interp desugared env 
      printfn "f\n%A:\n  expection not thrown\n" desugared
   with | Failure(msg) -> printf "." 

let testInterpDown (desugared, env, funs, expected) =
  let wrung = interp desugared env
  if expected = wrung then 
    printf "."
  else printfn "f\n%A:\nexpected: %A\n%A\n" desugared expected wrung
  
testDesugarDown(PlusS(NumS 4, NumS 5), NumV 9)
testDesugarDown(MinusS(NumS 4, NumS 5), NumV -1)
testDesugarDown(MultS(NumS 4, NumS 5), NumV 20)
testDesugarDown(UMinuS(NumS 4), NumV -4)
//testInterpDown(AppC("double", NumC 2), emptyEnv, 
//               [FdC("double", "x", PlusC(IdC "x",IdC "x"))], 
//               4)
//testInterpDown(PlusC(NumC 10, AppC("const5", NumC 10)), 
//            emptyEnv, 
//            [FdC("const5", "_", NumC 5)],
//            15)
//
//testInterpDown(PlusC(NumC 10, AppC("double", PlusC(NumC 1, NumC 2))), 
//            emptyEnv, 
//            [FdC("double", "x", PlusC(IdC "x", IdC "x"))],
//            16)
//
//testInterpDown(PlusC(NumC 10, AppC("quad", PlusC(NumC 1, NumC 2))), 
//            emptyEnv, 
//            [FdC("quad", "x", AppC("double", AppC("double", IdC "x")))
//             FdC("double", "x", PlusC(IdC "x", IdC "x"))],
//            22)
//
//testInterpDownExp(AppC("f1", NumC 3),
//            emptyEnv, 
//            [FdC("f1", "x", AppC("f2", NumC 4))
//             FdC("f2", "y", PlusC(IdC "x", IdC "y"))])

printf "\n\n "


