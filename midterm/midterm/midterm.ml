(* name: Dilichi Nwankwo *)
(* email: dnwankwo@caltech.edu *)

(*A.1*)
(* 
  The time complexity of this function is O(n) because the aux function is
  called on every value of n as it decrements. Then on the final call r 
  is returned. This makes it so the function f only has a time complexity
  od O(n) in every case.
*)

(*A.2*)
(*
  The time complexity of this function is O(abs(n-m)) because if m is greater
  than n then the function is called on the decrement of m until n reached and
  then returned. Then if m is lesser than n then the function is called on the
  increment of m until n reached and then returned. Thus the worst case time
  complexity is the difference between m and n which is abs(n-m).
*)

(*A.3*)
(*
  The time complexity of this function is O(3^n) this is because for every n 
  value, trib is called three times for its n-1, n-2, and n-3 counterparts.
  This does not include when n < 2 but for most n the worst case scenario 
  time complexity is O(3^n).
*)

(*A.4*)
(*
    The time complexity of this function is O(2logn). This is becuase if you
    imagine the worst case scenario for this function it is a number n that is
    not divisible by 2 or 3 and that its n-1 counterpart is divisible by 2 or
    3 once before the product of htat division is again not divisible by 2 or
    3. This would cause a cycle of n then n-1 then n/(2 or3) = n then new n
    is n-1 then n/(2 or 3) and so on. Thus because it is being regularly 
    divided, the time complexity would be O(logn). Yet weird could
    be called on a new n term that is just 1 minuis the original. This causes
    there to be the possibility that doubles the time complexity because
    usually once you reach a undivisible number in logn time it returns but 
    since this continues on n-1 the time complexity is O(2logn).
*)

(*A.5*)
(*
   The aspect of this function that makes it impossible to analyze using the
   methods we know so far is because in out usualy functions, given an input
   the resulting recursive calls is split into smaller parts whether that be
   using a method of spliting up, incrementing, or decrementing the orignal 
   input. Yet in this example, given a simple input like 6 we can end up the 
   function gets increasinly out of hand or goes in a loop. The function works
   such that if the input is even then call the function of the input divided
   by 2 and if the input is odd make it even by multiplying it by 3 and
   adding 1. So if 6 is given it would call f on 3 which would call f on 10
   then 5 then 16 then 8 then 4 then 2 the 1. Though 6 reaches a return value 
   this is not always the case because as we can see the input jumps up then
   and down frequently which changes the outcome severely. Thus this makes it 
   impossible for us to analyze all unnatturally changing cases for this 
   function using the breaking down and pattern repetition methods we have
   learned in class.
*)

(*B.1*)
let split3 lst = 
  let rec split olst lst1 lst2 lst3 n = 
    match olst with 
    | [] -> (List.rev lst1, List.rev lst2, List.rev lst3)
    | h :: t when n = 0 -> split t (h :: lst1) lst2 lst3 (n + 1)
    | h :: t when n = 1 -> split t lst1 (h :: lst2) lst3 (n + 1)
    | h :: t when n = 2 -> split t lst1 lst2 (h :: lst3) 0
    | _ :: _ -> split olst [] [] [] 0
  in split lst [] [] [] 0

let merge3 lst1 lst2 lst3 =
  let rec small l1 l2 mlst = 
    match (l1, l2) with
      | ([], (_ :: _)) -> (List.rev mlst) @ l2
      | ((_ :: _), []) -> (List.rev mlst) @ l1
      | ((h :: t), (h1 :: _)) when h < h1 -> small t l2 (h :: mlst)
      | ((h :: _), (h1 :: t1)) when h > h1 -> small l1 t1 (h1 :: mlst)
      | ((h :: t), (h1 :: t1)) when h = h1 -> small l1 t1 (h1 :: mlst)
      | _ -> mlst
  in small lst3 (small lst1 lst2 []) []

let rec merge_sort3 lst =
    match lst with 
      | [] -> lst
      | [_] -> lst
      | _ -> match (split3 lst) with
              | (x, y, z) -> 
                merge3 (merge_sort3 x) (merge_sort3 y) (merge_sort3 z)

(*B.2*)
let smallest_index lst =
  match lst with
    | [] -> invalid_arg "Empty List"
    | [_] -> 0
    | h :: t -> let rec iter ls save lowI curI =
                  match ls with 
                    | [x] when x > save -> lowI
                    | [x] when x < save -> (curI + 1)
                    | h :: t when h < save -> iter t h (curI + 1) (curI + 1)
                    | h :: t when h >= save -> iter t save lowI (curI + 1)
                    | _ -> lowI
                in iter t h 0 0

let flip_n goal lst =
  let rec iter ogL nList rn =
    match ogL with
      | _ when rn = goal -> nList @ ogL
      | [] -> invalid_arg "flip_n: not enough elements"
      | h :: t when rn <= goal -> iter t (h :: nList) (rn + 1)
      | _ -> nList
  in iter lst [] 0

let block_sort1 lst = 
  match lst with
    | [] -> []
    | _ -> flip_n ((smallest_index lst) + 1) lst

let rec block_sort_r lst =
  match block_sort1 lst with
    | [] -> []
    | [_] -> lst
    | h :: t -> h :: block_sort_r t

let block_sort_i lst =
  let rec iter l1 nlst = 
    match (block_sort1 l1) with 
      | [] -> []
      | [x] -> List.rev (x :: nlst)
      | h :: t -> iter t (h :: nlst)
  in iter lst []

(*B.3*)
let linrec is_base on_base split combine =
  let rec f x =
    if is_base x then
      on_base x
    else
      match (split x) with
      | (x', y) -> combine x' (f y) 
  in f 

let insert_r item =
  (* two base cases: the empty list
    * and when the item < the first element of the list *)
  let is_base lst = lst = [] || item <= List.hd lst in

  (* for both base cases, put the item on the front of the list *)
  let on_base lst = item :: lst in

  (* split the list.  Hint: structural recursion. *)
  let split lst = (List.hd lst, List.tl lst) in

  (* combine to get the final result *)
  let combine first rest_after_rec = first :: rest_after_rec in

    linrec is_base on_base split combine


let insertion_sort =
  (* base case: the list is empty *)
  let is_base lst = lst = [] in

  (* if it's a base case, return the empty list *)
  let on_base _ = [] in

  (* otherwise, split (hint: structural recursion again) *)
  let split lst = (List.hd lst, List.tl lst) in

  (* then combine *)
  let combine first rest_after_rec = insert_r first rest_after_rec in

    linrec is_base on_base split combine

(*B.4*)
let binrec is_base on_base split combine =
  let rec f x =
    if is_base x then
      on_base x
    else
      match (split x) with
        | (x', y, z) -> combine x' (f y) (f z) 
  in f  

let quicksort =
  let is_base lst = lst = [] in
  let on_base _ = [] in
  let split lst =
    match lst with
      | [] -> invalid_arg "quicksort: can't split"
      | h :: t -> (h, (List.filter (fun x -> x < h)) t, 
          (List.filter (fun x -> x >= h) t)) in
  let combine pivot lt ge = (lt @ (pivot :: ge)) in
    binrec is_base on_base split combine

(*B.5*)
let tailrec is_base on_base next =
  let rec f inputs =
    if is_base inputs then
      on_base inputs
    else
      f (next inputs)
  in f  

let insert_i item lst =
  let is_base (_, rest) = rest = [] || item <= List.hd rest in
  let on_base (prev, rest) = (List.rev prev) @ (item :: rest) in
  let next (prev, rest) = ((List.hd rest) :: prev, List.tl rest) in
  let iter = tailrec is_base on_base next in
    iter ([], lst)

let insertion_sort_i lst =
  let is_base (_, rest) = rest = [] in
  let on_base (prev, _) = prev in
  let next (prev, rest) = (insert_i (List.hd rest) prev, List.tl rest) in
  let iter = tailrec is_base on_base next in
    iter ([], lst)


(*C.1*)
type tree =
  | Leaf
  | Node of int * int * tree * tree   (* level, value, left/right subtrees *)

let rec member num oak =
  match oak with
    | Leaf -> false
    | Node (_, value, _, _) when num = value -> true
    | Node (_, value, left, _) when num < value -> member num left
    | Node (_, value, _, right) when num > value -> member num right
    | Node _ -> false

(* Level of an AA tree. *)
let level = function
  | Leaf -> 0
  | Node (lvl, _, _, _) -> lvl

let skew oak = 
  match oak with
    | Node (level, value, Node (level1, value1, left1, right1), right) 
      when level = level1 -> Node (level, value1, left1,
                             Node (level, value, right1, right))
    | Leaf -> oak
    | Node _ -> oak

let split oak =
match oak with
  | Node (level, _, _, Node (_, _, _, Node (level2, _, _, _))) 
    when level <> level2 || level <> level2 -> oak
  | Node (_, _, _, Node (_, _, _, Leaf)) -> oak
  | Node (level, value, left, Node (_, value1, left1, right1)) 
    -> Node (level + 1, value1, Node (level, value, left, left1), right1)
  | Leaf -> oak
  | Node _ -> oak
    
let rec insert item t =
  match t with
    | Leaf -> Node (1, item, Leaf, Leaf)
    | Node (_, value, _, _) when value = item -> t
    | Node (lvl, v, l, r) when item < v -> 
      split (skew (Node (lvl, v, (insert item l), r)))
    | Node (lvl, v, l, r) -> split (skew (Node (lvl, v, l, (insert item r))))







