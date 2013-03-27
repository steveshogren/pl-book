module Starter

type MispelledAnimal =
  | Yacc of int
  | Camel of int

let aCamel = Camel(5)
let good animal =
  match animal with
    | Camel(humps) -> humps >= 2
    | Yacc(height) -> height > 2
//printfn "%A is %A" aCamel (good aCamel)
 
type ArithC =
  | NumC of int
  | PlusC of ArithC * ArithC
  | MultC of ArithC * ArithC

let parseList li =
  li
let parse s =
  match System.Int32.TryParse(s) with
    | (true, n) -> NumC(n)
    | (false, n) when n.Length > 0 -> parseList n
    | _ -> failwith "not a ArithC"

printfn "Parse 4: %A" (parse("4"))
printfn "ArithC %A" (parse("[+; 2; 4]"))

