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
  | BoxV of int
and Storage =
  | Cell of int * Value
and Binding =
  | Bind of string * int
and Result =
  | VS of Value * Storage list

let emptyEnv : Binding list = List.Empty
let emptyStore : Storage list = List.Empty

let mutable counter = 0
let newLoc () =
  counter <- counter + 1 
  counter

let lookup (n : string) (env : Binding list) : int =
  match List.tryFind (fun funElem ->
                  match funElem with 
                    | Bind(name, value) -> name = n) env with
    | Some(Bind(name, value)) -> value
    | _ -> failwithf "%A not found" n
    
let fetch (location : int) (store : Storage list) : Value =
  let x = List.tryFind (fun funElem -> match funElem with | Cell (loc, value) -> loc = location) store
  match x with
    | Some(Cell(l, v)) -> v
    | _ -> failwithf "%A not found" location

let rec desugar a =
  match a with
    | NumS (n) -> NumC (n)
    | PlusS (l, r) -> PlusC (desugar l, desugar r )
    | MinusS (l, r) -> PlusC (desugar l, (MultC(NumC(-1), desugar(r))))
    | MultS (l, r) -> MultC (desugar l, desugar r )
    | UMinuS (n) -> MultC(NumC(-1), desugar n )


let rec interp (a : ExprC) (env : Binding list) (sto : Storage list): Result =
  match a with 
    | NumC(n) -> VS(NumV n, sto)
    | LamC(a, b) -> VS (ClosV(a, b, env), sto)
    | IdC(n) -> VS(fetch (lookup n env) sto, sto)
    | SeqC (b1, b2) ->
      let b1RS = interp b1 env sto
      match b1RS with | VS (res, isto) -> interp b2 env isto
    | PlusC(l, r) -> arithNum l r (fun x y -> x + y) env sto
    | MultC(l, r) -> arithNum l r (fun x y -> x * y) env sto
    | BoxC (a) ->
      match interp a env sto with
        | VS (valueA, istore) ->
          let wheres = newLoc()
          VS (BoxV (wheres),  Cell (wheres, valueA) :: istore)
    | UnBoxC (a) ->
      match interp a env sto with
        | VS (BoxV(loc), istore) -> VS (fetch loc istore, istore)
    | AppC (f, a) ->
      match interp f env with
        | ClosV(closVArg, closVBody, closVEnv) -> 
            let extendedEnv = Bind(closVArg, interp a env) :: closVEnv
            interp closVBody extendedEnv 
        | _ -> failwith "something went wrong"
      
and arithNum l r func env sto =
  let lrs = interp l env sto
  match lrs with
    | VS (lresult, lsto) ->  
       let rrs = interp r env lsto
       match lresult,rrs with
         | NumV (l), VS (NumV (r), rsto) -> VS (NumV(func l r), rsto)
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


