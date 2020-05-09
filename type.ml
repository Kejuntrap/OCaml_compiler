type exp =
     Integer_ of int         (* リテラル *)
  |  Plus of exp * exp     (* 足し算 *)
  |  Times of exp * exp    (* かけ算 *)
  |  Minus of exp * exp     (* ひき算 *)
  |  Div of exp * exp    (*わり算 *)
  |  Boolean_ of bool   (* boolean *)
  |  If of exp * exp * exp   (* if文 *)
  |  Eq of exp * exp        (* 等しいというやつ？e1 = e2 *)
;;

(* 値の型　*)
type value =
  | Integer  of int          (* 整数の値 *)
  | Boolean of bool         (* 真理値の値 *)
;;

let rec eval1 e =
  match e with
  | Integer_(n) -> Integer(n)   (* 「整数のみが書いてある」という式の型という情報が与えられているので整数型に変更する *)
  | Plus(e1,e2) -> 
    begin match(eval1 e1 , eval1 e2) with
    | (Integer(n1),Integer(n2)) -> Integer(n1+n2)
    | _ -> failwith "Integer values expected"
    end
  | Times(e1,e2) -> 
    begin match(eval1 e1 , eval1 e2) with
    | (Integer(n1),Integer(n2)) -> Integer(n1*n2)
    | _ -> failwith "Integer values expected"
    end
  | Minus(e1,e2) -> 
    begin match(eval1 e1 , eval1 e2) with
    | (Integer(n1),Integer(n2)) -> Integer(n1-n2)
    | _ -> failwith "Integer values expected"
    end
  | Div(e1,e2) -> 
    begin match(eval1 e1 , eval1 e2) with
    | (Integer(n1),Integer(n2)) -> 
      if n2 <> 0 then
        Integer(n1/n2)
      else
        failwith "Division_by_zero"
    end
    | _ -> failwith "Integer values expected"
;;


let res = eval1(Plus( Integer_ 6 , Integer_ 7));;

let res = eval1(Minus( Integer_ 6 , Integer_ 7));;

let res = eval1(Times( Integer_ 6 , Integer_ 7));;

let res = eval1(Div( Integer_ 6 , Integer_ 0));;
