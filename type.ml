open Printf

type furikake = Shake | Katsuo | Nori ;;

let isVeggie f =
  match f with
  | Shake -> false
  | Katsuo -> false
  | Nori -> true
  ;;

if isVeggie Shake then printf "Yess\n" else printf "owowowowo\n";;