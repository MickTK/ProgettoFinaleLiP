open Ast
open Types

let string_of_val = function
  | numeric_value -> string_of_int numeric_value

(* Espressioni *)
let rec string_of_expr = function
    Const n -> string_of_int n
  | Add(e1,e2) -> string_of_expr e1 ^ "+" ^ string_of_expr e2
  | Sub(e1,e2) -> string_of_expr e1 ^ "-" ^ string_of_expr e2
  | Mul(e1,e2) -> string_of_expr e1 ^ "*" ^ string_of_expr e2
  | True -> "true"
  | False -> "false"
  | And(e1,e2) -> string_of_expr e1 ^ " and " ^ string_of_expr e2
  | Or(e1,e2) -> string_of_expr e1 ^ " or " ^ string_of_expr e2
  | Not e -> "not " ^ string_of_expr e
  | Eq(e1,e2) -> string_of_expr e1 ^ "=" ^ string_of_expr e2
  | Leq(e1,e2) -> string_of_expr e1 ^ "<=" ^ string_of_expr e2
  | Var identifier -> identifier
  | ArrVar(identifier,index) -> identifier ^ "[" ^ string_of_expr index ^ "]"

(* Comandi *)
and string_of_cmd = function
    Skip -> "skip"
  | Break -> "break"
  | Assign(x,e) -> x ^ ":=" ^ string_of_expr e
  | ArrayAssign(identifier,index,expression) -> (string_of_expr (ArrVar(identifier,index))) ^ ":=" ^ string_of_expr expression
  | Seq(c1,c2) -> string_of_cmd c1 ^ "; " ^ string_of_cmd c2
  | Repeat(command) -> "repeat " ^ (string_of_cmd command) ^ " forever"
  | If(e,c1,c2) -> "if " ^ string_of_expr e ^ " then " ^ string_of_cmd c1 ^ " else " ^ string_of_cmd c2
  | Block(declaration,command) -> string_of_dv declaration ^ string_of_cmd command
  | Call(identifier,parameter) -> identifier ^ "(" ^ string_of_pa parameter ^ ")"
  | CallExec command -> string_of_cmd command

(* Dichiarazioni *)
and string_of_dv = function
    NullVar -> "null"
  | DVSeq(decl1,decl2) -> string_of_dv decl1 ^ "; " ^ string_of_dv decl2
  | Var identifier -> "int " ^ identifier
  | Array(identifier,length) -> "array " ^ identifier ^ "[" ^ string_of_expr length ^ "]"
and string_of_dp = function
    NullProc -> "null"
  | DPSeq(decl1,decl2) -> string_of_dp decl1 ^ "; " ^ string_of_dp decl2
  | Proc(identifier,parameter,command) -> "proc " ^ identifier ^ "(" ^ string_of_pf parameter ^ ") {" ^ string_of_cmd command ^ "}"

(* Parametri delle procedure *)
and string_of_pf = function
    Val identifier -> "val " ^ identifier
  | Ref identifier -> "ref " ^ identifier
and string_of_pa = function
    expression -> string_of_expr expression

(* Ambiente *)
let string_of_env1 state identifier = match topenv state identifier with
  | IVar location -> string_of_int location ^ "/" ^ identifier
  | IArr(location,expression) -> string_of_int location ^ "/" ^ identifier ^ "[" ^ string_of_expr expression ^ "]"
  | IProc(parameter,command) -> "proc(" ^ string_of_pf parameter ^ "){" ^ string_of_cmd command ^ "}/" ^ identifier
let rec string_of_env state = function
    [] -> ""
  | [identifier] -> (try string_of_env1 state identifier with _ -> "")
  | identifier::dom' -> (try string_of_env1 state identifier ^ "," ^ string_of_env state dom'
                with _ -> string_of_env state dom')

(* Memoria *)
let string_of_mem1 (memory,location) i =
  assert (i<location);
  string_of_val (memory i) ^ "/" ^ string_of_int i
let rec range a b = if b<a then [] else a::(range (a+1) b);;
let string_of_mem (memory,location) =
  List.fold_left (fun str i -> str ^ (try string_of_mem1 (memory,location) i ^ "," with _ -> "")) "" (range 0 (location - 1))

(* Lista delle locazioni utilizzate *)
let rec getlocs environment = function (* Es: [0,1,2,3,10,11,12] *)
  [] -> []
| identifier::dom -> try (match environment identifier with
  | IVar location -> location::(getlocs environment dom)
  | IArr(location,_) -> location::(getlocs environment dom)
  | IProc(_,_) -> [])
  with _ -> getlocs environment dom

(* Rappresenta uno stato sottoforma di stringa, ad esempio: "[IVar(0)/foo], [5/0]" *)
let string_of_state state domain =
"[" ^ string_of_env state domain ^ "], " ^
"[" ^ string_of_mem (getmem state,getloc state) ^ "]" ^ ", " ^
string_of_int (getloc state)

(* Unione (insiemistica) tra due liste *)
let rec union l1 l2 = match l1 with
    [] -> l2
  | x::l1' -> (if List.mem x l2 then [] else [x]) @ union l1' l2

(* Lista degli identificatori utilizzati *)
let rec vars_of_expr = function
    Const _ -> []
  | Add(e1,e2)
  | Sub(e1,e2)
  | Mul(e1,e2) -> union (vars_of_expr e1) (vars_of_expr e2)
  | True
  | False -> []
  | And(e1,e2) 
  | Or(e1,e2) -> union (vars_of_expr e1) (vars_of_expr e2)
  | Not e -> vars_of_expr e
  | Eq(e1,e2) 
  | Leq(e1,e2) -> union (vars_of_expr e1) (vars_of_expr e2)
  | Var identifier
  | ArrVar(identifier,_) -> [identifier]
and vars_of_cmd = function
    Skip 
  | Break -> []
  | Assign(x,e) -> union [x] (vars_of_expr e)
  | ArrayAssign(x,_,e') -> union [x] (vars_of_expr e')
  | Seq(c1,c2) -> union (vars_of_cmd c1) (vars_of_cmd c2)
  | Repeat(command) -> vars_of_cmd command
  | If(e,c1,c2) -> union (vars_of_expr e) (union (vars_of_cmd c1) (vars_of_cmd c2))
  | Block(dv,command) -> union (vars_of_dv dv) (vars_of_cmd command)
  | Call(identifier,expression) -> union [identifier] (vars_of_expr expression)
  | CallExec command -> vars_of_cmd command
and vars_of_dv = function
    NullVar -> []
  | DVSeq(dv,dv') -> union (vars_of_dv dv) (vars_of_dv dv')
  | Var identifier
  | Array(identifier,_) -> [identifier]
and vars_of_dp = function
    NullProc -> []
  | DPSeq(dp,dp') -> union (vars_of_dp dp) (vars_of_dp dp')
  | Proc(identifier,pf,command) -> union (union (vars_of_pf pf) [identifier]) (vars_of_cmd command)
and vars_of_pf = function
    Val identifier
  | Ref identifier -> [identifier]
and vars_of_pa = function
    expression -> vars_of_expr expression

(* Identificatori usati nel programma *)
let vars_of_prog (Prog(dv,dp,_)) = union (vars_of_dv dv) (vars_of_dp dp)

let string_of_conf vars = function
    St state -> string_of_state state vars
  | Cmd(command,state) -> "<" ^ string_of_cmd command ^ ", " ^ string_of_state state vars ^ ">"

let rec string_of_trace vars = function
    [] -> ""
  | [identifier] -> (string_of_conf vars identifier)
  | identifier::tail -> (string_of_conf vars identifier) ^ "\n -> " ^ string_of_trace vars tail

let rec last = function
    [] -> failwith "last on empty list"
  | [identifier] -> identifier
  | _::tail -> last tail
