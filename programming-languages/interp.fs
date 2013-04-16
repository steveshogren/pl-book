module interp

type ExprC =
  | NumC of int
  | VarC of string //s 
  | AppC of ExprC * ExprC // func, arg
  | PlusC of ExprC * ExprC // l, r
  | MultC of ExprC * ExprC // l, r
  | LamC of string * ExprC // arg, body

and Value =
  | NumV of int // n
  | ClosV of (Value -> Value)
  | ObjV of string list * Value list // ns, vs

and Binding =
  | Bind of string * Value //name, value

let emptyEnv : Binding list = List.Empty

let mutable counter = 0
let newLoc () =
  counter <- counter + 1 
  counter

let lookup (n : string) (env : Binding list) : Value =
  match List.tryFind (fun funElem ->
                  match funElem with 
                    | Bind(name, value) -> name = n) env with
    | Some(Bind(name, value)) -> value
    | _ -> failwithf "%A not found in env" n
    
let rec interp (a : ExprC) (env : Binding list) : Value =
  match a with 
    | NumC(n) -> NumV n
    | VarC(n) -> lookup n env
    | AppC (f, a) -> //fun, arg
      let avalue = interp a env
      match interp f env with
        | ClosV (closvf) -> closvf avalue
    | PlusC(l, r) -> arithNum l r (fun x y -> x + y) env
    | MultC(l, r) -> arithNum l r (fun x y -> x * y) env
    | LamC(arg, b) -> ClosV(fun (argval) ->
                            let newenv = Bind(arg, argval)::env
                            interp b newenv)
and arithNum lo ro func env =
  let lrs = interp lo env
  let rrs = interp ro env
  match lrs,rrs with
    | NumV (l), NumV (r) -> NumV(func l r)
    | _ -> failwith "Tried to arith something other than a number"



