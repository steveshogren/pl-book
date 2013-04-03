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
  | LamC of string * ExprC
  | BoxC of ExprC
  | UnBoxC of ExprC
  | SetBoxC of ExprC * ExprC
  | SeqC of ExprC * ExprC
  
type Value =
  | NumV of int
  | ClosV of string * ExprC * Binding list
  | BoxV of Value
and Binding =
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


let rec interp (a : ExprC) (env : Binding list) : Value =
  match a with 
    | NumC(n) -> NumV n
    | IdC(n) -> lookup n env
    | LamC(a, b) -> ClosV(a, b, env)
    | PlusC(l, r) -> arithNum l r (fun x y -> x + y) env
    | MultC(l, r) -> arithNum l r (fun x y -> x * y) env
    | AppC (f, a) ->
      match interp f env with
        | ClosV(closVArg, closVBody, closVEnv) -> 
            let extendedEnv = Bind(closVArg, interp a env) :: closVEnv
            interp closVBody extendedEnv 
        | _ -> failwith "something went wrong"
    | BoxC (a) -> BoxV (interp a env)
    | UnBoxC (a) ->
      let BoxV(v) (interp a env)
      v
    | SeqC (b1, b2) ->
      let v = interp b1 env
      interp b2 env
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

let testInterpDown (desugared, env, expected) =
  let wrung = interp desugared env
  if expected = wrung then 
    printf "."
  else printfn "f\n%A:\nexpected: %A\n%A\n" desugared expected wrung
  
testDesugarDown(PlusS(NumS 4, NumS 5), NumV 9)
testDesugarDown(MinusS(NumS 4, NumS 5), NumV -1)
testDesugarDown(MultS(NumS 4, NumS 5), NumV 20)
testDesugarDown(UMinuS(NumS 4), NumV -4)
testInterpDown(AppC(LamC("x", PlusC(IdC "x",IdC "x")), NumC 2), 
               emptyEnv, 
               NumV 4)
testInterpDown(PlusC(NumC 10, AppC(LamC("_", NumC 5), NumC 10)), 
            emptyEnv, 
            NumV 15)

testInterpDown(PlusC(NumC 10, AppC(LamC("x", PlusC(IdC "x", IdC "x")), PlusC(NumC 1, NumC 2))), 
            emptyEnv, 
            NumV 16)

testInterpDown(PlusC(NumC 10, AppC(LamC("x", AppC(LamC( "x", PlusC(IdC "x", IdC "x")), AppC(LamC("x", PlusC(IdC "x", IdC "x")), IdC "x"))), PlusC(NumC 1, NumC 2))), 
            emptyEnv, 
            NumV 22)
testInterpDown(AppC (LamC ("x", LamC ( "x", PlusC (IdC ("x"), IdC ("x")))), NumC 2),
            emptyEnv, 
            NumV 4)

testInterpDownExp(AppC(LamC("x", AppC(LamC( "y", PlusC(IdC "x", IdC "y")), NumC 4)), NumC 3),
            emptyEnv)

printf "\n\n "


