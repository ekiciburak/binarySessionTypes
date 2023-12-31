open Printf
open Expressions
open Processes
open Subst
open Congruence
open Types

type 'a err =
  | Yes: 'a     -> 'a err
  | No : string -> 'a err

type ctx = 
  | Nil  : ctx
  | ConsS: (string*sort*ctx)        -> ctx
  | ConsT: (string*sessionType*ctx) -> ctx

let extendS (m: ctx) (s: string) (t: sort) =
  ConsS(s,t,m)

let extendT (m: ctx) (s: string) (t: sessionType) =
  ConsT(s,t,m)

let rec lookupS(m: ctx) (s: string): sort err =
  match m with
  | Nil             -> No "no such variable sorted in the context"
  | ConsS(s',t',xs) -> if s = s' then Yes t' else lookupS xs s
  | ConsT(s',t',xs) -> lookupS xs s 

let rec lookupT(m: ctx) (s: string): sessionType err =
  match m with
  | Nil             -> No "no such variable typed in the context"
  | ConsS(s',t',xs) -> lookupT xs s 
  | ConsT(s',t',xs) -> if s = s' then Yes t' else lookupT xs s

let rec typecheckExpr(m: ctx) (e: expression): sort err =
  match e with
  | Val v -> 
    (
      match v with
      | Int i  -> Yes I
      | Bool b -> Yes B
      | Str s  -> Yes S
    )
  | EVar s       -> lookupS m s
  | Plus(e1,e2)  -> let te1 = typecheckExpr m e1 in
                    let te2 = typecheckExpr m e2 in
                    (
                     match (te1,te2) with
                     | (Yes I, Yes I) -> Yes I
                     | _              -> No "ill-typing in plus"
                    )
  | Minus(e1,e2) -> let te1 = typecheckExpr m e1 in
                    let te2 = typecheckExpr m e2 in
                    (
                     match (te1,te2) with
                     | (Yes I, Yes I) -> Yes I
                     | _              -> No "ill-typing in minus"
                    )
  | Neg e1       -> let te1 = typecheckExpr m e1 in
                    (
                      match te1 with
                      | Yes I -> Yes I
                      | _     -> No "ill-typing in neg"
                    ) 
  | Eq(e1,e2)    -> let te1 = typecheckExpr m e1 in
                    let te2 = typecheckExpr m e2 in
                    (
                     match (te1,te2) with
                     | (Yes t1, Yes t2) when t1 = t2 -> Yes B
                     | _                             -> No "ill-typing in eq"
                    )
  | Lt(e1,e2)    -> let te1 = typecheckExpr m e1 in
                    let te2 = typecheckExpr m e2 in
                    (
                     match (te1,te2) with
                     | (Yes I, Yes I) -> Yes B
                     | _              -> No "ill-typing in lt"
                    )
  | Gt(e1,e2)    -> let te1 = typecheckExpr m e1 in
                    let te2 = typecheckExpr m e2 in
                    (
                     match (te1,te2) with
                     | (Yes I, Yes I) -> Yes B
                     | _              -> No "ill-typing in gt"
                    )
  | And(e1,e2)   -> let te1 = typecheckExpr m e1 in
                    let te2 = typecheckExpr m e2 in
                    (
                     match (te1,te2) with
                     | (Yes B, Yes B) -> Yes B
                     | _              -> No "ill-typing in and"
                    )
  | Or(e1,e2)    -> let te1 = typecheckExpr m e1 in
                    let te2 = typecheckExpr m e2 in
                    (
                     match (te1,te2) with
                     | (Yes B, Yes B) -> Yes B
                     | _              -> No "ill-typing in and"
                    )
  | Not e1       -> let te1 = typecheckExpr m e1 in
                    (
                      match te1 with
                      | Yes B -> Yes B
                      | _     -> No "ill-typing in not"
                    ) 
  | NDet(e1,e2)  -> let te1 = typecheckExpr m e1 in
                    let te2 = typecheckExpr m e2 in
                    (
                     match (te1,te2) with
                     | (Yes t1, Yes t2) when t1 = t2 -> Yes t1
                     | _                             -> No "ill-typing in ndet"
                    )

let rec typecheck(m: ctx) (p: process): sessionType err =
  match p with
  | Inaction            -> Yes End
  | PVar s              -> lookupT m s
  | Send(par,e,p1)      -> let te  = typecheckExpr m e in
                           let tp1 = typecheck m p1 in
                           (
                             match (te,tp1) with
                             | (Yes t1, Yes t2) -> Yes(SendT(par,t1,t2))
                             | _                -> No "ill-typing in send"
                           )
  | Receive(par,x,p1)   -> let tx = lookupS m x in
                           (
                            match tx with
                            | Yes tx' -> 
                             let m'  = extendS m x tx' in
                             let tp1 = typecheck m' p1 in
                             (
                               match tp1 with
                               | Yes tp1' -> Yes (ReceiveT(par,tx',tp1'))
                               | _        -> No "ill-typing in receive -- 1"
                             )
                            | _       -> No "ill-typing in receive -- 2" 
                           )
  | Selection(par,l,p1) -> let tp1 = typecheck m p1 in
                           (
                            match tp1 with
                            | Yes tp1' -> Yes(SelectionT(par,[(l,tp1')]))
                            | _        -> No "ill-typing in selection" 
                           )
  | Branch(par,pl)      -> let pl' = typecheckProcList m pl [] in
                           Yes(BranchT(par,pl'))
  | Conditional(e,p1,p2)-> let te  = typecheckExpr m e in
                           let tp1 = typecheck m p1 in
                           let tp2 = typecheck m p2 in
                           (
                            match (te,tp1,tp2) with
                            | (Yes te',Yes tp1',Yes tp2') when tp1' = tp2' -> Yes tp1'
                            | _                                            -> No "ill-typing in conditional" 
                           )
  | Recursion(x,p)       -> let tx = lookupT m x in
                            let tp = typecheck m p in
                            (
                              match (tx,tp) with
                              | (Yes tx',Yes tp') (* when tx' = tp' *) -> 
                                Yes tp' (*µt.Alice![int];t*)
                              | _                                      ->
                                No "ill-typing in mu"
                            )
                            (*
                            let tp = typecheck m p in
                            (
                              match tp with
                              | Yes tp' -> 
                                let m'   = extendT m x tp' in
                                let tp'' = typecheck m' p in
                                (
                                  match tp'' with
                                  | Yes tp''' when tp' = tp''' -> Yes tp'
                                  | _                          -> No "ill-typing in mu -- 1"
                                ) 
                              | _       -> No "ill-typing in mu -- 2"
                            )
                            *)

and typecheckProcList m pl acc =
  match pl with
  | []        -> acc
  | (l,p)::xs -> let tp = typecheck m p in
                 (
                  match tp with
                  | Yes tp' -> typecheckProcList m xs ((l,tp')::acc)
                  | _       -> failwith "ill-typing in proc list"
                 )

let rec isWellTypedSession (m: ctx) (s: session): bool =
  match s with
  | Comp(par1,p1,par2,p2) when par1 = dualityParticipant par2 -> 
    let t1 = typecheck m p1 in
    let t2 = typecheck m p2 in
    (
      match (t1,t2) with
      | (Yes t1',Yes t2') when t2' = duality t1' -> true
      | _                                         -> false 
    )  
  | _                                                         ->
    false                       

    