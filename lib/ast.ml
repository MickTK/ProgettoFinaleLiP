type ide = string

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
  | Var of ide
  | ArrVar of ide * expr

(* Parametri formali *)
type pf =
  | Val of ide
  | Ref of ide

(* Parametri attuali *)
type pa =
  | CurrentP of expr

(* Dichiarazioni variabili e array *)
type dv =
  | NullVar
  | DVSeq of dv * dv
  | Var of ide
  | Array of ide * expr

(* Comandi *)
type cmd =
  | Skip
  | Break
  | Assign of string * expr
  | ArrayAssign of string * expr * expr
  | Seq of cmd * cmd
  | Repeat of cmd
  | If of expr * cmd * cmd
  | DSeq of dv * cmd

(* Dichiarazioni procedure *)
type dp =
  | NullProc
  | DPSeq of dp * dp
  | Proc of ide * pf * cmd

type prog = Prog of dv * dp * cmd
