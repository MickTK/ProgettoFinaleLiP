open Ast

type location = int                                 (* Locazione di memoria *)
type declaration = Variable of dv | Procedure of dp (* Tipo di dichiarazione *)

(* Valori dell'ambiente e della memoria *)
type environment_value = 
      IVar of location        (* Locazione di memoria della variabile *)
    | IArr of location * expr (* Prima locazione di memoria dell'array e dimensione dell'array *)
    | IProc of pf * cmd       (* Tipo di parametro preso in ingresso e comando (corpo) della procedura *)
type memory_value = int       (* Valore contenuto in una cella di memoria *)

(* Definizione di ambiente e memoria *)
type environment = identifier -> environment_value
type memory = location -> memory_value

(* Stato del programma *)
type state = environment list * memory * location (* Lista di ambienti, memoria, prima cella di memoria disponibile *)

(* Operazioni con lo stato *)
(* Ambiente in coda alla lista dello stato *)
let topenv (environment_list,_,_) = match environment_list with
    [] -> failwith "empty environment stack"
  | environment::_ -> environment
(* Rimuove l'ambiente in coda alla lista dello stato *)
let popenv (environment_list,_,_) = match environment_list with
    [] -> failwith "empty environment stack"
  | _::environment_list' -> environment_list'
let getenv (environment_list,_,_) = environment_list (* Lista di ambienti dello stato *)
let getmem (_,memory,_) = memory                     (* Memoria dello stato *)
let getloc (_,_,location) = location                 (* Prima locazione di memoria disponibile dello stato *)

type configuration = St of state | Cmd of cmd * state

exception TypeError of string
exception UnboundVar of string
exception PredOfZero
exception NoRuleApplies
