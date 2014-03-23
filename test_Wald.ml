open OUnit
open Printf

let string_of_decision = function
  | Wald.Decline -> "decline"
  | Wald.Accept  -> "accept"
  | Wald.Defer   -> "defer"

let test_tactics () = 
  let w = Wald.create ~alpha:0.02 ~beta:0.03 ~p0:0.1 ~p1:0.3 in
  let trials = [
    1 ,false; 2 ,false; 3 , true; 4 ,false; 5 ,false;
    6 ,false; 7 ,false; 8, false; 9 , true; 10,false;
    11,false; 12,false; 13, true; 14,false; 15, true;
    16, true; 17, true; 18, true] in
    
  let rec test w = function
    | (m,r)::tl -> test (Wald.trial r w) tl
    | [] -> Wald.decision w in
  let d = test w trials in
  d = Wald.Decline

let rec test w = function
  | [] -> Wald.decision w 
  | (m,r)::tl ->
      let w = Wald.trial r w in
      (* let d = Wald.decision w in *)
      (* let a,n,r = Wald.adr w in *)
      (* eprintf "%2d %2.2g < %2.2g < %2.2g -> %s\n" *)
      (*   m a n r (string_of_decision d); *)
      match Wald.decision w with
        | Wald.Defer -> test w tl
        | d -> d 

(**
   Гипотеза H0 - отметка шумовая.
   Ошибка первого рода - ложная тревога (гипотеза H0 несправедливо отвергнута).
    Ошибка второго рода - пропуск цели.
    Проверяется гипотеза о том, что вероятность пропуска [p] меньше
    некоторого значения [p'].
    
    С вероятностью [alpha] допустимо решение, что [p <= p'] (пропусков
    не так много), в то время как [p < p0] (реально пропусков много).
    Т.е. принимается решение об обнаружении ложной цели (false
    positive).

    С вероятность [beta] допустимо решение, что [p > p'] (пропусков
    слишком много) в то время, как [p > p1] (реально пропусков очень
    мало).
*)
let res = Wald.create
  ~alpha:0.01      (** допустимая вероятность ошибки I-рода *)
  ~p0:0.1          (** вероятность ложной отметки  *)
  ~beta:1e-7       (** допустимая вероятность ошибки II-рода  *)
  ~p1:0.95          (** вероятность правильной отметки  *)

let false_mark = 
  [1 ,false; 2 ,false; 3 , false; 4 ,false; 5 ,false;
    6 ,false; 7 ,false; 8, false; 9 , false; 10,false]

let true_mark = 
  [1 ,true; 2 ,true; 3 , true; 4 ,true; 5 ,true;
    6 ,true; 7 ,true; 8, true; 9 , true; 10,true]


let true_with_some_misses = 
  [1 ,true; 2 ,false; 3 , false; 4 ,true; 5 ,true;
    6 ,true; 7 ,true; 8, true; 9 , true; 10,true]

(* сначала сигнал был, затем исчез  *)
let true_then_false = 
  [1 ,false; 2 ,false; 3 , false; 4 ,false; 5 ,false;
    6 ,false; 7 ,true; 8, true; 9 , true; 10,true]


let blonde_elephant1 = 
  [1 ,true; 2 ,false; 3, true; 4 ,false; 5 ,true; 6 ,false; 
   7 ,true; 8 ,false; 9, true; 10 ,false; 11 ,true; 12 ,false]

let blonde_elephant2 = 
  [1 ,true; 2 ,false; 3, false; 4 ,true; 5 ,false; 6 ,false; 
   7 ,true; 8 ,false; 9, true; 10 ,true; 11 ,true; 12 ,false]


let eight_of_12 = 
  [1 ,true; 2 ,false; 3, true; 4 ,true; 5 ,false; 6 ,false; 
   7 ,true; 8 ,false; 9, true; 10 ,true; 11 ,true; 12 ,true]

let assert_detected mark () = 
  assert_equal (test res mark)  Wald.Decline
let assert_dropped  mark () = 
  assert_equal (test res mark)  Wald.Accept
let assert_undecided  mark () = 
  assert_equal (test res mark)  Wald.Defer


let suite =
  "Wald">::: [
    "tactics" >:: (fun () -> assert_equal (test_tactics ()) true);
    "false mark" >:: assert_dropped false_mark;
    "true mark"  >:: assert_detected true_mark;
    "true mark w/misses" >:: assert_detected true_with_some_misses;
    "true then false" >:: assert_dropped true_then_false;
    "blonde elephant1" >:: assert_undecided blonde_elephant1;
    "blonde elephant2" >:: assert_undecided blonde_elephant2;
    "9/12" >:: assert_detected eight_of_12;
  ];


















