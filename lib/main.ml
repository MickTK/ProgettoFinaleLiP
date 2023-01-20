open Ast
open Types

exception TypeError of string
exception UnboundVar of string
exception PredOfZero
exception NoRuleApplies

(* Ambiente *)
let botenv = fun x -> failwith ("variable " ^ x ^ " unbound")

(* Memoria *)
let botmem = fun l -> failwith ("location " ^ string_of_int l ^ " undefined")

(*
  bind environment identifier environment_value -> env{identifier/environment_value}
  bind memory location memory_value -> mem{location/memory_value}
  bind environment identifier environment_value identifier -> environment_value
*)
let bind stack prefix postfix = fun y -> if y = prefix then postfix else stack y

let rec trace_int expr = match expr with
  | Const n -> n
  | Add(Const(n1),Const(n2)) -> trace_int (Const(n1+n2))
  | Sub(Const(n1),Const(n2)) -> trace_int (Const(n1-n2))
  | Mul(Const(n1),Const(n2)) -> trace_int (Const(n1*n2))
  | _ -> failwith "expression is not a number."

let is_val = function
    True -> true
  | False -> true
  | Const _ -> true
  | _ -> false

let apply state variable = fun index -> match topenv state variable with
    IVar location -> getmem state location
  | IArr(location,expr) when index < (trace_int expr) -> getmem state (location + index)
  | _ -> failwith "apply error"

let parse (s : string) : prog =
  let lexbuf = Lexing.from_string s in
  let ast = Parser.prog Lexer.read lexbuf in
  ast

(******************************************************************************)
(*                      Small-step semantics of expressions                   *)
(******************************************************************************)

(* TODO: Espressioni *)
let rec trace1_expr state = function
  | _ -> failwith "do something with " ^ state

(* TODO: Comandi *)
and trace1_cmd = function
  | _ -> failwith "do something"

let rec sem_decl (environment,location) = function
  | Value dv -> ( match dv with
    | NullVar -> (environment,location)
    | DVSeq(dv,dv') -> let (environment',location') = sem_decl (environment,location) (Value dv) in sem_decl (environment',location') (Value dv')
    | Var identifier -> let environment' = bind environment identifier (IVar location) in (environment',location+1)
    | Array(identifier, expression) -> let offset = trace_int expression in
      let environment' = bind environment identifier (IArr(location,expression)) 
      in (environment',location+offset+1)
  )
  | Procedure dp -> ( match dp with
    | NullProc -> (environment,location)
    | DPSeq(dp,dp') -> let (environment',location') = sem_decl (environment,location) (Procedure dp) in sem_decl (environment',location') (Procedure dp')
    | Proc(identifier,param,cmd) -> let environment' = bind environment identifier (IProc(Formal(param),cmd)) in (environment',location)
  )

let rec trace_rec iterations_number state =
  if iterations_number <= 0 then [state]
  else try
      let state' = trace1_cmd state
      in state::(trace_rec (iterations_number-1) state')
    with NoRuleApplies -> [state]

let trace iterations_number (Prog(dv,dp,cmd)) =
  let (environment,location) = sem_decl (botenv,0) (Value dv) in
  let (environment',location') = sem_decl (environment,location) (Procedure dp) in
  trace_rec iterations_number (Cmd(cmd,([environment'],botmem,location')))
