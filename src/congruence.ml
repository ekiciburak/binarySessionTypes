open Printf
open Expressions
open Processes
open Subst

let isCongruenceProcess(p1: process) (p2: process): bool =
  match p1 with
  | Recursion(y1,p3) -> p2 = replace p3 y1 (Recursion(y1,p3))
  | _                -> false

let rec congruenceProcessH(p: process) (n: int) (acc: process): process =
  match p with
  | Recursion(y1,p1) -> if n > 0 
                        then congruenceProcessH p (n-1) (replace acc y1 p)
                        else acc
  | _                -> p
  
let congruenceProcess(p: process) (n: int): process =
  match p with
  | Recursion(y,p1) -> congruenceProcessH p n p1
  | _               -> p

let rec isCongruenceSession(s1: session) (s2: session): bool =
  match (s1,s2) with
  | (Comp(par1,p1,par2,p2),Comp(par3,p3,par4,p4)) when par1 = par4 && par2 = par3            -> p1 = p4 && p2 = p3
  | (Comp(par1,p1,par2,p2),Comp(par3,p3,par4,p4)) when par1 = par3 && par2 = par4 && p1 = p3 -> p2 = p4
  | _                                                                                        -> false 