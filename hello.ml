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

let rec debugstring t = match t with
  | TmTrue -> "true"
  | TmFalse -> "false"
  | TmIf (_, _, _) -> "if"
  | TmZero -> "zero"
  | TmSucc t1 -> Printf.sprintf "succ(%s)" (debugstring t1)
  | TmPred t1 -> Printf.sprintf "pred(%s)" (debugstring t1)
  | TmIsZero t1 -> Printf.sprintf "iszero(%s)" (debugstring t1)

let printterm t = Printf.printf "%s\n%!" (debugstring t)

let () = printterm TmTrue

