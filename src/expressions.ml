open Printf

type value = 
  | Int : int    -> value
  | Bool: bool   -> value
  | Str : string -> value

let rec value2String(v: value): string =
  match v with
  | Int i  -> string_of_int i
  | Bool b -> string_of_bool b
  | Str s  -> "(val:" ^ s ^ ")"

let printValue(v: value): unit =
  printf "%s\n" (value2String v)

type expression = 
  | Val  : value  -> expression
  | EVar : string -> expression
  | Plus : (expression*expression) -> expression
  | Minus: (expression*expression) -> expression
  | Neg  : expression -> expression
  | Eq   : (expression*expression) -> expression
  | Lt   : (expression*expression) -> expression
  | Gt   : (expression*expression) -> expression
  | And  : (expression*expression) -> expression
  | Or   : (expression*expression) -> expression
  | Not  : expression -> expression
  | NDet : (expression*expression) -> expression
  
let rec expression2String(e: expression): string =
  match e with
  | Val v        -> value2String v
  | EVar s       -> s
  | Plus(e1,e2)  -> expression2String e1 ^ " + " ^ expression2String e2
  | Minus(e1,e2) -> expression2String e1 ^ " - " ^ expression2String e2
  | Neg e1       -> "-" ^ expression2String e1
  | Eq(e1,e2)    -> expression2String e1 ^ " = " ^ expression2String e2
  | Lt(e1,e2)    -> expression2String e1 ^ " < " ^ expression2String e2
  | Gt(e1,e2)    -> expression2String e1 ^ " > " ^ expression2String e2
  | And(e1,e2)   -> expression2String e1 ^ " && " ^ expression2String e2
  | Or(e1,e2)    -> expression2String e1 ^ " || " ^ expression2String e2
  | Not e1       -> "~" ^ expression2String e1
  | NDet(e1,e2)  -> expression2String e1 ^ " ⨁ " ^ expression2String e2

let printExpression(e: expression): unit =
  printf "%s\n" (expression2String e)