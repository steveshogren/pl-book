module interp

type ExprC =
  | NumC of int
  | VarC of string //s 
  | AppC of ExprC * ExprC // fun, arg
  | PlusC of ExprC * ExprC // l, r
  | MultC of ExprC * ExprC // l, r
  | LamC of string * ExprC // arg, body
  | SetC of string * ExprC // var, arg
  | SeqC of ExprC * ExprC // b1, b2

and Value =
  | NumV of int // n
  | ClosV of string * ExprC * Binding list //arg, body, env

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
    | VarC(n) -> VS(fetch (lookup n env) sto, sto)
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
    | PlusC(l, r) -> arithNum l r (fun x y -> x + y) env sto
    | MultC(l, r) -> arithNum l r (fun x y -> x * y) env sto
    | LamC(a, b) -> VS (ClosV(a, b, env), sto)
    | SetC(var, value) ->
      match interp value env sto with
        | VS(vval, sval) ->
          let wheres = lookup var env
          VS(vval, Cell(wheres, vval) :: sval)
    | SeqC (b1, b2) ->
      let b1RS = interp b1 env sto
      match b1RS with | VS (res, isto) -> interp b2 env isto
              
      
and arithNum l r func env sto =
  let lrs = interp l env sto
  match lrs with
    | VS (lresult, lsto) ->  
       let rrs = interp r env lsto
       match lresult,rrs with
         | NumV (l), VS (NumV (r), rsto) -> VS (NumV(func l r), rsto)
         | _ -> failwith "Tried to arith something other than a number"



