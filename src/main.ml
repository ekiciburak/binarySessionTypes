open Printf
open Expressions
open Processes
open Subst
open Congruence
open Types
open Typechecking

let main: unit =
  let e = Plus(EVar "x", EVar "y") in
  printExpression e;
  let s = 
  Comp(Alice,
       Send(Bob,Val(Int 42),Inaction),
       Bob,
       Receive(Alice,"x",Inaction)
      )
  in printSession s;
  let s = 
  Comp
  (Alice,
   Send(Bob,Val(Int 7),Inaction),
   Bob,
   Receive(Alice,"x",Send(Alice,Val(Str "thx"),Inaction))
  )
  in printSession s;
  let t =
  SendT(Alice,I,SendT(Alice,S,ReceiveT(Alice,I,End))) in
  printSessionType t;
  let t = 
  BranchT(Bob,[("orange",SendT(Bob,S,SendT(Bob,I,End)));
               ("cherry",ReceiveT(Bob,S,End));
               ("reject", End)
              ])
  in printSessionType t;
  let p = Recursion("X",Send(Alice,Val(Int 42),(PVar "X"))) in
  printProcess p;
  let t = typecheck (ConsT("X",(RecursionT("t",SendT(Alice,I,TVar "t"))),Nil)) p in
  (
    match t with
    | Yes t' -> printSessionType t'
    | No s   -> printf "%s\n" s
  );
  let s1 = SendT(Alice,I,RecursionT("t",ReceiveT(Alice,B,SendT(Alice,I,TVar "t")))) in
  let ds1 = duality s1 in 
  printSessionType ds1;
  let p1 = Selection(Bob,"banana",Inaction) in
  let p2 = Branch(Alice,[("apple",Inaction);("banana", Inaction)]) in
  let s = Comp(Alice,p1,Bob,p2) in
  let b = isWellTypedSession Nil s in
  printf "%b\n" b;
  let p1 = Recursion("X",Send(Alice,Val(Int 1),PVar "X")) in
  let p2 = Send(Alice,Val(Int 1),Recursion("X",Send(Alice,Val(Int 1),PVar "X"))) in
  let p3 = Send(Alice,Val(Int 1),Send(Alice,Val(Int 1),Recursion("X",Send(Alice,Val(Int 1),PVar "X")))) in
  printProcess p1;
  printProcess p2;
  printProcess p3;
  let b = isCongruenceProcess p1 p2 in
  printf "%b\n" b;
  let b = isCongruenceProcess p2 p3 in
  printf "%b\n" b;
  let p4 = congruenceProcess p1 3 in
  printProcess p4;
