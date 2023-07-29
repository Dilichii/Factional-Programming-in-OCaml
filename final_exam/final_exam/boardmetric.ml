open Boardrep

module type BoardMetric =
  sig
    type t

    val distance : t -> t -> int
  end

let get_loc size index = (index / size, index mod size)

module Hamming(B : BoardRep) : BoardMetric with type t = B.t =
  struct
    type t = B.t
    
    let distance b1 b2 =
      if B.get_size b1 <> B.get_size b2 then
        failwith "incompatible board sizes"
      else 
      let rec iter count total = 
        if count < (B.get_size b1 * B.get_size b1) then
          let curLoc = (get_loc (B.get_size b1)count) in 
          match (B.get b1 curLoc, B.get b2 curLoc) with
            | (_, n2) when n2 = 0 -> iter (count + 1) total
            | (n1, n2) when n1 = n2 -> iter (count + 1) total
            | (_, _) -> iter (count + 1) (total + 1)
        else
          total
      in iter 0 0
  end

module Manhattan(B : BoardRep) : BoardMetric with type t = B.t =
  struct
    type t = B.t
 
    let to_array b = 
      let size = B.get_size b in 
      let arr = Array.make (size * size) (0, 0) in
      let rec iter count curVal loc = Array.set arr curVal loc;
        if count < (Array.length arr) then
          iter (count + 1) (B.get b (get_loc size count)) (get_loc size count)
        else arr
      in iter 0 (B.get b (get_loc size 0)) (get_loc size 0)

    let distance b1 b2 =
      if B.get_size b1 <> B.get_size b2 then
        failwith "incompatible board sizes"
      else
        let l1 = Array.to_list (to_array b1) 
        and l2 = Array.to_list (to_array b2) in
        let rec iter count len total lst1 lst2 =
          match (lst1, lst2) with
          | ([], _) -> total
          | (_, []) -> total
          | ((r1, c1) :: t1, (r2, c2) :: t2) when count > 0 -> 
            iter (count + 1) len (total + abs (r1 - r2) + abs (c1 - c2)) t1 t2
          | ((_, _) :: t1, (_, _) :: t2) ->
            iter (count + 1) len total t1 t2
      in iter 0 (B.get_size b1 - 1) 0 l1 l2
  end

