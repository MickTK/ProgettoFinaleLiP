type identifier = string

(* Espressioni *)
type expr =
  | Const of int
  | Add of expr * expr
  | Sub of expr * expr
  | Mul of expr * expr
  | True
  | False
  | And of expr * expr
  | Or of expr * expr
  | Not of expr
  | Eq of expr * expr
  | Leq of expr * expr
  | Var of identifier
  | ArrVar of identifier * expr

(* Parametri formali *)
type pf =
  | Val of identifier
  | Ref of identifier

(* Parametro attuale (solo come alias) *)
type pa = expr

(* Dichiarazioni variabili e array *)
type dv =
  | NullVar
  | DVSeq of dv * dv
  | Var of identifier
  | Array of identifier * expr

(* Comandi *)
type cmd =
  | Skip
  | Break
  | Assign of string * expr
  | ArrayAssign of string * expr * expr
  | Seq of cmd * cmd
  | Repeat of cmd
  | If of expr * cmd * cmd
  | Block of dv * cmd
  | Call of identifier * pa
  | CallExec of cmd

(* Dichiarazioni procedure *)
type dp =
  | NullProc
  | DPSeq of dp * dp
  | Proc of identifier * pf * cmd

type prog = Prog of dv * dp * cmd
