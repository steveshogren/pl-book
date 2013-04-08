module interp

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
  | NumV of int // n
  | ClosV of string * ExprC * Binding list //arg, body, env
  | BoxV of int // l
and Storage =
  | Cell of int * Value //location, Value
and Binding =
  | Bind of string * int //name, value
and Result =
  | VS of Value * Storage list // v, s

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
        | _ -> failwith "box did not contain a box"
    | SetBoxC (b, v) -> 
      match interp b env sto with
        | VS (BoxV(vbl), sb) ->
          match interp v env sb with
            | VS (BoxV(vvl), sv) -> VS (BoxV(vvl), Cell(vbl, BoxV (vvl))::sv)
            | _ -> failwith "set box value was not a box"
        | _ -> failwith "set box was not on a box"
    | AppC (f, a) ->
      match interp f env sto with
        | VS (ClosV (farg, fbody, fenv), sf) ->
          match interp a env sf with
            | VS (va, sa) -> 
              let wheres = newLoc()
              let newstore = Cell (wheres, va) :: sa
              let newEnv = Bind (farg, wheres) :: fenv
              interp fbody newEnv newstore
        | _ -> failwith "did not try to appc a closure"
              
      
and arithNum l r func env sto =
  let lrs = interp l env sto
  match lrs with
    | VS (lresult, lsto) ->  
       let rrs = interp r env lsto
       match lresult,rrs with
         | NumV (l), VS (NumV (r), rsto) -> VS (NumV(func l r), rsto)
         | _ -> failwith "Tried to arith something other than a number"



