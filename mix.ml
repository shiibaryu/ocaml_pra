open Printf

let rec fact n = 
  if n == 1 then 1 else n * fact (n-1);;
let sum = fact 5 ;;
Printf.printf "%d\n" sum

let pi = 3.14;;
let circle_area radius = pi *. radius *. radius;;
let area = circle_area 5.0;;
Printf.printf "%f\n" area

(* 高階関数*)
let add5 x = x + 5;;
let fool f = (f 10) + 2;;
let sum = fool add5;;
Printf.printf "%d\n" sum

let mul10 x = x * 10;;
let foo2 f = (f (f 10)) + 2;;
let sum = foo2 mul10;;
Printf.printf "%d\n" sum

let compose f g x = g (f x);;
let sum = compose add5 mul10 10;;
Printf.printf "%d\n" sum

let rec multiple_apply n f x =
  if n = 0 then x
  else multiple_apply (n-1) f (f x);;
let sum = multiple_apply 5 mul10 10;;
Printf.printf "%d\n" sum

let arr = List.map add5 [1;2;3;4;5];;
List.iter(printf "%d\n") arr;;

let arr = List.map (fun x -> (x > 3)) [1;5;6;2;3];;

let hoo1 x y = x + y;;
let sum = List.fold_left hoo1 0 [1;2;3;4;5];;
Printf.printf "%d\n" sum

let hoo2 x y = x*2 + y;;
let sum = List.fold_left hoo2 0 [1;0;1;1];;
Printf.printf "%d\n" sum
