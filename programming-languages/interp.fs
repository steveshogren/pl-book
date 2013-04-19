module interp

//type ExprC =
//  | NumC of int
//  | VarC of string //s 
//  | AppC of ExprC * ExprC // func, arg
//  | PlusC of ExprC * ExprC // l, r
//  | MultC of ExprC * ExprC // l, r
//  | LamC of string * ExprC // arg, body
type TypeE =
  | NumT
  | FunT of TypeE * TypeE

type TyExprC =
  | NumC of int
  | VarC of string //s 
  | AppC of TyExprC * TyExprC // func, arg
  | PlusC of TyExprC * TyExprC // l, r
  | MultC of TyExprC * TyExprC // l, r
  | LamC of string * TypeE * TypeE * TyExprC // arg, body

//and Value =
//  | NumV of int // n
//  | ClosV of (Value -> Value)
//  | ObjV of string list * Value list // ns, vs
and Value =
  | NumV of int // n
  | ClosV of string * TyExprC * Env

and Binding =
  | Bind of string * Value //name, value

let emptyEnv = (fun name -> failwith "name not found")
let lookup (n : string) e = e n 
let extendEnv (b : Binding) e  : (string->Value) =
  (fun name ->
   match b with
     | Bind (bname, bvalue) ->
       if name = bname then bvalue
       else lookup name e)

let mutable counter = 0
let newLoc () =
  counter <- counter + 1 
  counter
    
let rec interp (a : ExprC) (env : (string->Value))  : Value =
  match a with 
    | NumC(n) -> NumV n
    | VarC(n) -> lookup n env
    | PlusC(l, r) -> arithNum l r ( + ) env 
    | MultC(l, r) -> arithNum l r ( * ) env
    | AppC (f, a) -> //fun, arg
      let avalue = interpk f env
        (fun fv -> interpk a env
           (fun av ->
              match fv with
                | ClosC (closvf) -> closvf av k))
      match interp f env with
        | ClosV (closvf) -> closvf avalue
    | LamC(arg, b) -> ClosV(fun (argval) ->
                            let bound = Bind(arg, argval)
                            let newenv = extendEnv bound env
                            interp b newenv)
and arithNum lo ro func env =
  let lrs = interp lo env
  let rrs = interp ro env
  match lrs,rrs with
    | NumV (l), NumV (r) -> NumV(func l r)
    | _ -> failwith "Tried to arith something other than a number"

