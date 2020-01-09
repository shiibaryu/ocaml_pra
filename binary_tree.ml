type tree =
  | Lf
  | Br of {
    left: tree;
    value: int;
    right: tree;
  }

let rec find(t,n) =
  match t with
  | Lf -> false
  | Br {left=l; value=v; right=r}->
    if n = v then true
    else if n < v then find(l,n)
    else find(r,n)

let rec insert(t,n) = 
  match t with
  | Lf -> Br {left=Lf;value=n;right=Lf}
  | Br {left=l;value=v;right=r} ->
    if n = v then t 
    else if n < v then Br {left=insert(l, n); value=v; right=r}
    else Br {left=l;value=v;right=insert(r,n)}

let rec min t =
    match t with
    | Lf -> min_int
    | Br {left=Lf;value=v;right=_} -> v
    | Br {left=l;value=_;right=_} -> min l

let rec delete(t,n) = 
  match t with
  | Lf -> t
  | Br {left=l;value=v;right=r} -> 
    if n = v then 
      match l,r with
        | Lf, Lf -> Lf
        | Br _, Lf -> l
        | Lf, Br _ -> r
        | Br _ , Br _ ->
          let m = min r in
          Br {left=l; value=m;right=delete(r,m)}
    else if n < v then Br {left = delete(l,n); value=v;right=r}
    else Br {left=l;value=v;right=delete(r,n)}
