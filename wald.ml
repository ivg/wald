type result = Accept | Decline | Defer 

type t = (float -> float) * (float -> float) * float * float * float

let create ~p0 ~p1 ~alpha ~beta =
  let x1 = (1. -. beta) /. alpha in
  let x2 = beta /. (1. -. alpha) in
  let y1 = (1. -. p0) /. (1. -. p1) in
  let y2 = (1. -. p1) /. (1. -. p0) in
  let z  = p1 /. p0 in
  let u  = log z -. log y2 in
  let q1 = log x1 /. u in
  let q2 = log x2 /. u in
  let q3 = log y1 /. u in
  let a m = q2 +. m *. q3 in
  let r m = q1 +. m *. q3 in
  let es = log z *. log y2 in
  a,r,0.,0.,log x2 *. log x1 /. es

let expected_trials (_,_,_,_,et) = et

let trial test (a,r,n,m,et) = match test with
  | true  -> a, r, n +. 1., m +. 1.,et
  | false -> a, r, n,       m +. 1.,et

let decision (a,r,n,m,_) = match a m, n, r m with
  | a, d, r when d >= r -> Decline
  | a, d, r when d <= a -> Accept
  | _                   -> Defer

let trials (_,_,_,m,_) = int_of_float m

let adr (a,r,n,m,_) = a m, n, r m
  




















