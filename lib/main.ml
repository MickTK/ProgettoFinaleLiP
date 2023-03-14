open Ast
open Types
open Prettyprint

let parse (s : string) : prog =
  let lexbuf = Lexing.from_string s in
  let ast = Parser.prog Lexer.read lexbuf in
  ast

exception TypeError of string
exception UnboundVar of string
exception PredOfZero
exception NoRuleApplies

(* Ambiente *)
let botenv = fun x -> failwith ("Variable " ^ x ^ " unbound")

(* Memoria *)
let botmem = fun l -> failwith ("Location " ^ string_of_int l ^ " undefined")

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
  | _ -> failwith ((string_of_expr expr) ^ " is not a number.")

let is_val = function
    True -> true
  | False -> true
  | Const _ -> true
  | _ -> false

let apply state variable = match topenv state variable with
    IVar location -> getmem state location
  | _ -> failwith "apply error"

let arr_apply state variable index = match topenv state variable with
    IArr(location,expr) when index < (trace_int expr) -> getmem state (location + index)
  | _ -> failwith "apply error"

(******************************************************************************)
(*                      Small-step semantics of expressions                   *)
(******************************************************************************)

(* Espressioni *)
let rec trace1_expr state = function
  | Add(Const(n1),Const(n2)) -> (Const(n1+n2),state)
  | Add(Const(n1),e2) -> let (e2',state') = trace1_expr state e2 in (Add(Const(n1),e2'),state')
  | Add(e1,e2) -> let (e1',state') = trace1_expr state e1 in (Add(e1',e2),state')
  | Sub(Const(n1),Const(n2)) -> (Const(n1-n2),state)
  | Sub(Const(n1),e2) -> let (e2',state') = trace1_expr state e2 in (Sub(Const(n1),e2'),state')
  | Sub(e1,e2) -> let (e1',state') = trace1_expr state e1 in (Sub(e1',e2),state')
  | Mul(Const(n1),Const(n2)) -> (Const(n1*n2),state)
  | Mul(Const(n1),e2) -> let (e2',state') = trace1_expr state e2 in (Mul(Const(n1),e2'),state')
  | Mul(e1,e2) -> let (e1',state') = trace1_expr state e1 in (Mul(e1',e2),state')
  | And(True,e) -> (e,state)
  | And(False,_) -> (False,state)
  | And(e1,e2) -> let (e1',state') = trace1_expr state e1 in (And(e1',e2),state')
  | Or(True,_) -> (True,state)
  | Or(False,e) -> (e,state)
  | Or(e1,e2) -> let (e1',state') = trace1_expr state e1 in (Or(e1',e2),state')
  | Not(True) -> (False,state)
  | Not(False) -> (True,state)
  | Not(e) -> let (e',state') = trace1_expr state e in (Not(e'),state')
  | Eq(Const(n1),Const(n2)) -> if n1=n2 then (True,state) else (False,state)
  | Eq(Const(n1),e2) -> let (e2',state') = trace1_expr state e2 in (Eq(Const(n1),e2'),state')
  | Eq(e1,e2) -> let (e1',state') = trace1_expr state e1 in (Eq(e1',e2),state')
  | Leq(Const(n1),Const(n2)) -> if n1<=n2 then (True,state) else (False,state)
  | Leq(Const(n1),e2) -> let (e2',state') = trace1_expr state e2 in (Leq(Const(n1),e2'),state')
  | Leq(e1,e2) -> let (e1',state') = trace1_expr state e1 in (Leq(e1',e2),state')
  | Var identifier -> (Const(apply state identifier), state)
  | ArrVar(identifier,e) -> (Const(arr_apply state identifier (trace_int e)), state)
  | _ -> raise NoRuleApplies

(* Comandi *)
and trace1_cmd = function
    St _ -> raise NoRuleApplies
  | Cmd(command,state) -> match command with
      Skip -> St state
    | Break -> St state
    | Assign(identifier,Const(n)) -> (match topenv state identifier with
        IVar location -> St (getenv state, bind (getmem state) location n, getloc state)
      | _ -> raise (UnboundVar("Variable " ^ identifier ^ " not defined.")))
    | Assign(identifier,expression) -> let (expression',state') = trace1_expr state expression in Cmd(Assign(identifier,expression'),state') 
    | ArrayAssign(identifier,expr_index,Const(n)) -> 
      (match topenv state identifier with                                                    (* Ottiene il valore nell'ambiente *)
        IArr(first_location,length) -> let index = (trace_int expr_index)                    (* Calcola l'indice dell'array *)
          in if index < (trace_int length) && index >= 0                                     (* Controlla che l'indice sia valido *)
          then St (getenv state, bind (getmem state) (first_location+index) n, getloc state) (* Restituisce lo stato con il nuovo valore assunto dall'array in data posizione *)
          else failwith "Index out of bounds."
        | _ -> raise (UnboundVar("Array " ^ identifier ^ " is not defined.")))
    | ArrayAssign(identifier,expr_index,expression) -> let (expression',state') = trace1_expr state expression 
      in Cmd(ArrayAssign(identifier,expr_index,expression'),state')
    | Seq(Break,Repeat(_)) -> St state (* Interrompe il repeat restituendo lo stato *)
    | Seq(command1,command2) -> (match trace1_cmd (Cmd(command1,state)) with
          St state1 -> Cmd(command2,state1)
        | Cmd(command1',state1) -> Cmd(Seq(command1',command2),state1))
    | Repeat(command') -> Cmd(Seq(command',Repeat(command')),state)
    | If(True,command1,_) -> Cmd(command1,state)
    | If(False,_,command2) -> Cmd(command2,state)
    | If(Const(n),_,_) -> raise (TypeError (string_of_int n ^ " is not a boolean"))
    | If(expression,command1,command2) -> let (expression',state') = trace1_expr state expression in Cmd(If(expression',command1,command2),state')
    | Block(NullVar,command') -> (match trace1_cmd (Cmd(command',state)) with
        St state' -> St (popenv state', getmem state', getloc state')
      | Cmd(command0,state') -> Cmd(Block(NullVar,command0),state'))
    | Block(dv,command') -> let (environment,location) = sem_decl (topenv state,getloc state) (Value dv) in 
      Cmd(Block(NullVar,command'),(environment::(getenv state),getmem state,location))
    | Call(identifier,Var(x)) -> (match topenv state identifier with        (* Guardiamo a cosa è associato l'identificatore *)
        IProc(Val(y),command') -> (match ((topenv state) x) with            (* Se è una procedura, controlliamo a cosa è associato il parametro (variabile) in ingresso *)
            IVar(location) -> let value = (getmem state) location in        (* Recuperiamo dalla memoria il valore della variabile *)
              let (environment,location) = sem_decl (topenv state,getloc state) (Value (Var(y))) in
              let state0 = (environment::(getenv state),getmem state,location) in
              (match trace1_cmd (Cmd(Assign(y,Const(value)),state0)) with
                  St state1 -> Cmd(CallExec command',state1)
                | _ -> failwith ("Non leggerete mai questo messaggio >:) bwahahahaha")
              )
          | _ -> raise (TypeError (x ^ " is not a variable."))
        )
      | IProc(Ref(y),command') -> (match (topenv state) x with                            (* Controlliamo a cosa è associato il parametro (variabile) in ingresso *)
                IVar(location) -> let (environment,_) =                                   (* Se è associato ad una variabile *)
                  sem_decl (topenv state,location) (Value (Var(y))) in                    (* Crea il nuovo ambiente della procedura definendo y e associando la locazione di x alla locazione di y *)
                  let state' = (environment::(getenv state),getmem state,getloc state) in (* Aggiunge l'ambiente della procedura alla lista, preservando la prima locazione libera *)
                  Cmd(CallExec command', state')                                          (* Passa il controllo al blocco, che si occuperà di effettuare i comandi *)
              | _ -> failwith (x ^ " is not a variable.")
            )
      | _ -> (failwith "ops")
    )
    | Call(identifier,Const(n)) -> (match (topenv state) identifier with (* Controlla a cosa corrisponde nell'ambiente l'identificatore *)
          IProc(Val(x),command') ->
            let (environment,location) = sem_decl (topenv state,getloc state) (Value (Var(x))) in (* Dichiara la variabile nel nuovo ambiente *)
            let state0 = (environment::(getenv state),getmem state,location) in                   (* Crea il nuovo stato *)
            (match trace1_cmd (Cmd(Assign(x,Const(n)),state0)) with                               (* Assegna il valore al parametro della procedura *)
                St state1 -> Cmd(CallExec command',state1)                                        (* Esegue la procedura *)
              | _ -> failwith ("Non succede mai."))
        | IProc(Ref(_),_) -> failwith(identifier ^ " accepts only referred parameters.") (* Non si possono passare espressioni per riferimento *)
        | _ -> failwith(identifier ^ " is not a procedure!")                             (* Non si può chiamare una variabile o un vettore come procedura *)
    )
    | Call(identifier,expression) -> let (expression',_) = trace1_expr state expression in Cmd(Call(identifier,expression'),state)
    | CallExec command' -> (match trace1_cmd (Cmd(command',state)) with (* Esegue la procedura *)
        St state' -> St(popenv state', getmem state', getloc state') (* Termina l'esecuzione della procedura *)
      | Cmd(command0,state') -> Cmd(CallExec(command0),state'))      (* Continua l'esecuzione della procedura *)

(* Dichiarazione di variabili e procedure *)
and sem_decl (environment,location) = function
    Value dv -> ( match dv with
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
    | Proc(identifier,param,cmd) -> let environment' = bind environment identifier (IProc(param,cmd)) in (environment',location)
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
