open Printf
open Expressions
open Processes

let rec find(y: string) (l: string list): bool =
  match l with
  | []    -> false
  | x::xs -> if x = y then true else find y xs

let rec fvH(e: process) (acc: string list): string list =
  match e with
  | PVar x          -> x :: acc
  | Recursion(y,p1) -> List.filter (fun a -> a != y) (fvH p1 acc)
  | _               -> acc

let fv(t: process): string list =
  fvH t []

let rec replace(e: process) (x: string) (s: process): process =
  match e with
  | Inaction              -> Inaction
  | PVar y                -> if x = y then s else e
  | Send(par,e1,p1)       -> Send(par,e1,replace p1 x s)
  | Receive(par,e1,p1)    -> Receive(par,e1,replace p1 x s)
  | Branch(par,l)         -> Branch(par,replaceProcList l x s)
  | Selection(par,s1,p1)  -> Selection(par,s1,replace p1 x s)
  | Conditional(e1,p1,p2) -> Conditional(e1,replace p1 x s, replace p2 x s)
  | Recursion(y,p1)       -> (* if y != x && find y (fv s) = false then *)
                             Recursion(y,replace p1 x s) (* else e *)
and replaceProcList l x s =
  match l with
  | [] -> []
  | (y,p1)::xs -> (y,replace p1 x s) :: replaceProcList xs x s


  

