module tests

open interp
open desugar

let testDesugarDown (sugar, expected) =
  let x = interp (desugar sugar) [] [] 
  match x with
    | VS(actualV, _) -> 
      if expected = actualV then printf "."
      else printfn "f\n%A:\nexpected: %A\n%A\n" sugar expected actualV

let testInterpDownExp (desugared, env) =
  try 
      let wrung = interp desugared env []
      printfn "f\n%A:\n  expection not thrown\n" desugared
   with | Failure(msg) -> printf "." 

let testInterpDown (desugared, env, expected) =
  let x = interp desugared env []
  match x with
    | VS(actualV, _) -> 
      if expected = actualV then printf "."
      else printfn "f\n%A:\nexpected: %A\n%A\n" desugared expected actualV
  
testDesugarDown(PlusS(NumS 4, NumS 5), NumV 9)
testDesugarDown(MinusS(NumS 4, NumS 5), NumV -1)
testDesugarDown(MultS(NumS 4, NumS 5), NumV 20)
testDesugarDown(UMinuS(NumS 4), NumV -4)
testInterpDown(AppC(LamC("x", PlusC(VarC "x",VarC "x")), NumC 2), 
               emptyEnv, 
               NumV 4)
testInterpDown(PlusC(NumC 10, AppC(LamC("_", NumC 5), NumC 10)), 
            emptyEnv, 
            NumV 15)
testInterpDown(SeqC (VarC "test", SeqC(SetC("test", NumC 5), PlusC (NumC 5, VarC "test"))), 
            emptyEnv, 
            NumV 10)

testInterpDown(PlusC(NumC 10, AppC(LamC("x", PlusC(VarC "x", VarC "x")), PlusC(NumC 1, NumC 2))), 
            emptyEnv, 
            NumV 16)

testInterpDown(PlusC(NumC 10, AppC(LamC("x", AppC(LamC( "x", PlusC(VarC "x", VarC "x")), AppC(LamC("x", PlusC(VarC "x", VarC "x")), VarC "x"))), PlusC(NumC 1, NumC 2))), 
            emptyEnv, 
            NumV 22)
testInterpDown(
               (LetS "o", ObjS (["add1", "sub1"], ))
  emptyEnv, 
            NumV 22)
//testInterpDown(AppC (LamC ("x", LamC ( "x", PlusC (VarC ("x"), VarC ("x")))), NumC 2), emptyEnv, NumV 4)

//testInterpDownExp(AppC(LamC("x", AppC(LamC( "y", PlusC(VarC "x", VarC "y")), NumC 4)), NumC 3), emptyEnv)

printf "\n\n "
