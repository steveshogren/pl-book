module interpk

type ExprC =
  | NumC of int
  | VarC of string //s 
  | AppC of ExprC * ExprC // func, arg
  | PlusC of ExprC * ExprC // l, r
  | MultC of ExprC * ExprC // l, r
  | LamC of string * ExprC // arg, body

and Value =
  | NumV of int // n
  | ClosV of (Value (Value -> Value) -> Value)

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

let valArith l r func =
  match l,r with
    | NumV (l), NumV (r) -> NumV(func l r)
    | _ -> failwith "Tried to arith something other than a number"
  

let rec interpk (a : ExprC) (env : (string->Value)) (k : (Value -> Value)) : Value =
  match a with 
    | NumC(n) -> NumV n |> k
    | VarC(n) -> lookup n env |> k
    | PlusC(l, r) -> arithNumk l r ( + ) env k
    | MultC(l, r) -> arithNumk l r ( * ) env k
    | AppC (f, a) -> //fun, arg
      interpk f env (fun (fv) ->
                       interpk a env (fun (av) ->
                                        match fv with | ClosV(farg) -> farg av k))
    | LamC(arg, b) -> k ClosV(fun (argval dynk) ->
                            let bound = Bind(arg, argval)
                            let newenv = extendEnv bound env
                            interpk b newenv dynk)
and arithNumk lo ro func env k =
  interpk lo env
    (fun lv ->
     interpk ro env (fun rv ->
        let res = valArith lv rv func
        k res))

let rec interp (expr : ExprC) b c : Value =
  interpk expr emptyEnv (fun (ans) -> ans)


