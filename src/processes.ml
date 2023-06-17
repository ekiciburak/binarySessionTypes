open Printf
open Expressions

type participant = 
  | Alice: participant
  | Bob  : participant

let rec participant2String(p: participant): string =
  match p with
  | Alice -> "Alice"
  | Bob   -> "Bob"

let printParticipant(p: participant): unit =
  printf "%s\n" (participant2String p)
  
type process = 
  | PVar       : string                              -> process
  | Inaction   : process
  | Send       : (participant*expression*process)    -> process
  | Receive    : (participant*string*process)        -> process
  | Branch     : (participant*procList)              -> process
  | Selection  : (participant*string*process)        -> process
  | Conditional: (expression*process*process)        -> process
  | Recursion  : (string*process)                    -> process
and procList = (string*process) list

let rec process2String(p: process): string =
  match p with
  | Inaction             -> "0"
  | PVar s               -> s
  | Send(par,e,p1)       -> participant2String par ^ "!<" ^ expression2String e ^ ">." ^ process2String p1
  | Receive(par,s,p1)    -> participant2String par ^ "?(" ^ s ^ ")." ^ process2String p1
  | Branch(par,l)        -> participant2String par ^ "-> { " ^ procList2String l ^ " }"
  | Selection(par,s,p1)  -> participant2String par ^ "<-" ^ s ^ "." ^ process2String p1
  | Conditional(e,p1,p2) -> "if " ^ expression2String e ^ " then " ^ process2String p1 ^ " else " ^ process2String p2
  | Recursion(s,p1)      -> "µ" ^ s ^ "." ^ process2String p1
and procList2String l =
  match l with
  | []        -> ""
  | [(x,y)]   -> x ^ ": " ^  process2String y
  | (x,y)::xs -> x ^ ": " ^  process2String y ^ "," ^ procList2String xs

let printProcess(p: process): unit =
  printf "%s\n" (process2String p)

type session =
  | Comp: (participant*process*participant*process) -> session

let rec session2String(s: session): string =
  match s with
  | Comp(par1,p1,par2,p2) -> participant2String par1 ^ "::" ^ process2String p1 ^ " | " ^ participant2String par2 ^ "::" ^ process2String p2

let printSession(s: session): unit =
  printf "%s\n" (session2String s)
