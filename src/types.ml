open Printf
open Expressions
open Processes
open Subst
open Congruence

type sort = 
  | I: sort
  | B: sort
  | S: sort

let rec sort2String(s: sort): string =
  match s with
  | I -> "int"
  | B -> "bool"
  | S -> "string"

let printSort(s: sort): unit =
  printf "%s\n" (sort2String s)

type sessionType =
  | End       : sessionType
  | SendT     : (participant*sort*sessionType) -> sessionType
  | ReceiveT  : (participant*sort*sessionType) -> sessionType
  | SelectionT: (participant*labelType)        -> sessionType
  | BranchT   : (participant*labelType)        -> sessionType
  | TVar      : string                         -> sessionType
  | RecursionT: (string*sessionType)           -> sessionType
and labelType = (string*sessionType) list

let rec sessionType2String(t: sessionType): string =
  match t with
  | End                -> "end"
  | SendT(par,s,t1)    -> participant2String par ^ "![" ^ sort2String s ^ "];" ^ sessionType2String t1
  | ReceiveT(par,s,t1) -> participant2String par ^ "?[" ^ sort2String s ^ "];" ^ sessionType2String t1
  | SelectionT(par,l)  -> participant2String par ^ "⨁{\n" ^ labelType2String l ^ "\n}"
  | BranchT(par,l)     -> participant2String par ^ "&\n{\n" ^ labelType2String l ^ "\n}"
  | TVar s             -> s
  | RecursionT(s,t1)   -> "µ" ^ s ^ "." ^ sessionType2String t1
and labelType2String l =
  match l with
  | []        -> ""
  | [(x,y)]   -> "  " ^ x ^ ": " ^  sessionType2String y
  | (x,y)::xs -> "  " ^ x ^ ": " ^  sessionType2String y ^ "\n" ^ labelType2String xs

let printSessionType(s: sessionType): unit =
  printf "%s\n" (sessionType2String s)

let dualityParticipant(p: participant): participant =
  match p with
  | Alice -> Bob
  | Bob   -> Alice

let rec duality(s: sessionType): sessionType =
  match s with
  | End               -> End
  | SendT(par,u,t)    -> ReceiveT(dualityParticipant par,u,duality t)
  | ReceiveT(par,u,t) -> SendT(dualityParticipant par,u,duality t)
  | SelectionT(par,l) -> BranchT(dualityParticipant par,labeledDuality l)
  | BranchT(par,l)    -> SelectionT(dualityParticipant par,labeledDuality l)
  | TVar t            -> TVar t
  | RecursionT(s,u)   -> RecursionT(s,duality u)  
and labeledDuality l =
  match l with
  | []          -> []
  | (x,t) :: xs -> (x,duality t) :: labeledDuality xs

let rec isSubtype(s: sessionType) (s': sessionType): bool =
  match (s,s') with
  | (End,End)                                                               ->
    true
  | (SendT(par1,u1,t1),SendT(par2,u2,t2)) when par1 = par2 && u1 = u2       ->
    isSubtype t1 t2 
  | (ReceiveT(par1,u1,t1),ReceiveT(par2,u2,t2)) when par1 = par2 && u1 = u2 ->
    isSubtype t1 t2
  | (SelectionT(par1,l1),SelectionT(par2,l2)) when par1 = par2              ->
    isLabeledSubtyping l1 l2 
  | (BranchT(par1,l1),BranchT(par2,l2)) when par1 = par2                    ->
    isLabeledSubtyping l1 l2   
  | (RecursionT(s1,u1),RecursionT(s2,u2)) when s1 = s2                      ->
    isSubtype u1 u2
  | _                                                                       ->
    false
and isLabeledSubtyping l1 l2 =
  match (l1,l2) with
  | ([],[])                                  -> true
  | ((x1,t1)::xs1,(x2,t2)::xs2) when x1 = x2 -> 
    if isSubtype t1 t2 then isLabeledSubtyping xs1 xs2 else false
  | _                                        -> false

