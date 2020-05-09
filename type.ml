type exp =
     Integer of int         (* リテラル *)
  |  Plus of exp * exp     (* 足し算 *)
  |  Times of exp * exp    (* かけ算 *)
  |  Minus of exp * exp     (* ひき算 *)
  |  Div of exp * exp    (*わり算 *)
  |  Boolean of bool   (* boolean *)
  |  If of exp * exp * exp   (* if文 *)
  |  Eq of exp * exp        (* 等しいというやつ？e1 = e2 *)
;;

let rec eval1 e =
  match e with
  | Integer(n) -> n
  | Plus(e1,e2) -> (eval1 e1) + (eval1 e2)
  | Times(e1,e2) -> (eval1 e1) * (eval1 e2)
  | Minus(e1,e2) -> (eval1 e1) - (eval1 e2)
  | Div(e1,e2) -> if e2 <> Integer(0) then
    (eval1 e1) / (eval1 e2)
  else
    failwith "Division_by_zero"
;;

let res = eval1(Plus(Integer(6),Integer(7)));;
print_int res;;
print_char '\n';;
let res = eval1(Div(Integer(6),Integer(7)));;
print_int res;;
print_char '\n';;
let res = eval1(Times(Integer(16777216),Integer(16777216)));;
print_int res;;
print_char '\n';;
let res = eval1(Div(Integer(16777216),Integer(0)));;
print_int res;;
print_char '\n';;