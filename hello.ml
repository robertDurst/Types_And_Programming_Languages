(* this is a comment *)

type term =
  | TmTrue
  | TmFalse
  | TmIf of term * term * term
  | TmZero
  | TmSucc of term
  | TmPred of term
  | TmIsZero of term

(* check whether a ter, is a numeric value *)
let rec isnumericval t = match t with
  | TmZero -> true
  | TmSucc t1 -> isnumericval t1
  | _ -> false

(* check whether a term is a value *)
let isval t = match t with
  | TmTrue -> true
  | TmFalse -> true
  | t when isnumericval t -> true
  | _ -> false

exception NoRuleApplies

(* evaluate a term *)
let rec eval1 t = match t with
  | TmIf (TmTrue, t2, t3) -> t2
  | TmIf (TmFalse, t2, t3) -> t3
  | TmIf (t1, t2, t3) -> TmIf (eval1 t1, t2, t3)
  | TmSucc t1 -> TmSucc (eval1 t1)
  | TmPred TmZero -> TmZero
  | TmPred (TmSucc nv1) when isnumericval nv1 -> nv1
  | TmPred t1 -> TmPred (eval1 t1)
  | TmIsZero TmZero -> TmTrue
  | TmIsZero (TmSucc nv1) when isnumericval nv1 -> TmFalse
  | TmIsZero t1 -> TmIsZero (eval1 t1)
  | _ -> raise NoRuleApplies

(* evaluate a term until it is a value *)
let rec eval t =
  try
    let t' = eval1 t in
    eval t'
  with
    | _ -> t

(* convert a term to a string *)
let rec debugstring t = match t with
  | TmTrue -> "true"
  | TmFalse -> "false"
  | TmIf (t1, t2, t3) -> String.concat "" ["if "; debugstring t1; " then "; debugstring t2; " else "; debugstring t3]
  | TmZero -> "zero"
  | TmSucc t1 -> Printf.sprintf "succ(%s)" (debugstring t1)
  | TmPred t1 -> Printf.sprintf "pred(%s)" (debugstring t1)
  | TmIsZero t1 -> Printf.sprintf "iszero(%s)" (debugstring t1)

let printterm t = Printf.printf "%s\n%!" (debugstring t)

let () = Printf.printf "%s\n%!" (TmIf (TmIsZero (TmPred (TmSucc (TmSucc TmZero))), TmTrue, TmFalse) |> debugstring)

