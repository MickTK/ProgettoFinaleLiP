open Ast

type location = int                       (* locazione di memoria *)
type param = Formal of pf | Current of pa (* parametro della procedura *)
type termination = Ok | Br                (* terminazione dei comandi *)
type declaration = Value of dv | Procedure of dp

(* Valori dell'ambiente e della memoria *)
type environment_value = 
      IVar of location        (* locazione di memoria della variabile *)
    | IArr of location * expr (* prima locazione di memoria dell'array, dimensione dell'array *)
    | IProc of param * cmd
type memory_value = int

(* Definizione di ambiente e memoria *)
type environment = identifier -> environment_value
type memory = location -> memory_value

(* Stato del programma *)
type state = environment list * memory * location

(* Operazioni con lo stato *)
let topenv (environment_list,_,_) = match environment_list with
    [] -> failwith "empty environment stack"
  | environment::_ -> environment

let popenv (environment_list,_,_) = match environment_list with
    [] -> failwith "empty environment stack"
  | _::environment_list' -> environment_list'

let getenv (environment_list,_,_) = environment_list
let getmem (_,memory,_) = memory
let getloc (_,_,location) = location


type configuration = St of state | Cmd of cmd * state

exception TypeError of string
exception UnboundVar of identifier
