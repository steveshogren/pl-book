module tests

open interp
open desugar

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
