type loc = int * int
type move = Up | Down | Left | Right

module type BoardRep =
  sig
    type t

    exception Invalid_move
    exception Invalid_location

    val init      : int -> t
    val load      : int -> int list -> t
    val get_size  : t -> int
    val get_hole  : t -> loc
    val get       : t -> loc -> int
    val make_move : t -> move -> t
    val show      : t -> unit
  end

(* ---------------------------------------------------------------------- 
 * Helper functions.
 * ---------------------------------------------------------------------- *)

(* Make a display function given board accessors. *)
let make_show get get_size b =
  let size = get_size b in
    begin
      Printf.printf "\n%!";
      for row = 0 to size - 1 do
        for col = 0 to size - 1 do
          Printf.printf "%3d" (get b (row, col))
        done;
        Printf.printf "\n";
      done;
      Printf.printf "\n%!"
    end

(* ---------------------------------------------------------------------- 
 * Modules.
 * ---------------------------------------------------------------------- *)
let get_loc size index = (index / size, index mod size)
let get_index size (r, c)  = (r * size) + c
let make_basic size = 
  let rec iter len lst =
    if (len < (size * size)) then 
      iter (len + 1) (len :: lst)
    else lst 
  in List.rev (iter 0 [])

let rec printList lst = 
  match lst with
  | [] -> Printf.printf("\nPrinted\n")
  | h :: t -> Printf.printf("%i ") h; printList t 

module OrderedLoc =
  struct
    type t = loc
    let compare = Stdlib.compare
  end

module ArrayRep : BoardRep =
  struct
    type t = 
      { 
        acontents : int array;
        size : int;
        hole : loc
      }

    exception Invalid_move
    exception Invalid_location

    let load size lst =
      if size < 2 then
        failwith "ERROR: load: size must be at least 2"
      else
        if List.length lst <> (size * size) then
          failwith "ERROR: load: length of the input list isn't size * size"
        else if 
          not (List.for_all (fun x -> List.mem x lst) (make_basic size)) then
          failwith "invalid list contents"
        else let rec iter clst curr = 
          match clst with
          | [] -> (0, 0)
          | h :: _ when h = 0 -> get_loc size curr
          | _ :: t -> iter t (curr + 1)
        in 
        let hol = iter lst 0 in 
        {acontents = (Array.of_list lst); size = size; hole = hol}
        

    let init size = 
      if size < 2 then
        failwith "ERROR: init: size must be at least 2"
      else
        let rec iter len lst =
          if (len < (size * size)) then 
            iter (len + 1) (len :: lst)
        else lst 
      in load size ((List.rev (iter 1 [])) @ [0])
    
    let get_size b = b.size

    let get_hole b = b.hole

    let get { acontents; size = s; _ } (r, c) = 
      if r < 0 || r > s - 1 || c < 0 || c > s - 1 then
        raise Invalid_location
      else
        try
          acontents.(r * s + c)
        with (Invalid_argument _) ->
          raise Invalid_location

    let make_move b m =
      let goal = 
        match (m, b.hole) with
        | (Up, (r, c)) -> if r = 0 then 
          raise Invalid_move else  (r - 1, c)
        | (Down, (r, c)) -> if r = b.size - 1 then 
          raise Invalid_move else (r + 1, c)
        | (Left, (r, c)) -> if c = 0 then 
          raise Invalid_move else (r, c - 1)
        | (Right, (r, c)) -> if c = b.size - 1 then 
          raise Invalid_move else (r, c + 1)
      in 
      let saveVal = (get b goal) in 
      let contents1 = Array.copy b.acontents in
      Array.set contents1 (get_index b.size goal) 0;
      Array.set contents1 (get_index b.size b.hole) saveVal; 
      {b with hole = goal; acontents = contents1}

    let show = make_show get get_size
  end

module MapRep : BoardRep =
  struct
    module LocMap = Map.Make(OrderedLoc)

    type t = 
      { 
        mcontents : int LocMap.t;
        size : int;
        hole : loc
      }

    exception Invalid_move
    exception Invalid_location

    let init size =
      if size < 2 then
        failwith "ERROR: init: size must be at least 2"
      else
        let rec iter len mp =
          if (len < size * size - 1) then 
            iter (len + 1) (LocMap.add (get_loc size len) (len + 1) mp)
        else {mcontents = LocMap.add (get_loc size len ) 0 mp; 
              size = size; hole = (size - 1, size - 1) }
      in iter 0 LocMap.empty 

    let load size lst =
      if size < 2 then
        failwith "ERROR: load: size must be at least 2"
      else
        if List.length lst <> (size * size) then
          failwith "ERROR: load: length of the input list isn't size * size"
        else if 
          not (List.for_all (fun x -> List.mem x lst) (make_basic size)) then 
          failwith "invalid list contents"
        else
          let rec iter curr lst mp o =
          match lst with
          | [] -> {mcontents = mp; size = size; hole = o}
          | h :: t when h = 0 -> 
            iter (curr + 1) t 
            (LocMap.add (get_loc size curr) h mp) (get_loc size curr)
          | h :: t -> 
            iter (curr + 1) t 
            (LocMap.add (get_loc size curr) h mp) o
        in iter 0 lst LocMap.empty (0, 0)

    let get_size b = b.size

    let get_hole b = b.hole

    let get { mcontents; _ } l = 
      try
        LocMap.find l mcontents
      with Not_found ->
        raise Invalid_location

    let make_move b m =
      let goal = 
        match (m, b.hole) with
        | (Up, (r, c)) -> if r = 0 then 
          raise Invalid_move else  (r - 1, c)
        | (Down, (r, c)) -> if r = b.size - 1 then 
          raise Invalid_move else (r + 1, c)
        | (Left, (r, c)) -> if c = 0 then 
          raise Invalid_move else (r, c - 1)
        | (Right, (r, c)) -> if c = b.size - 1 then 
          raise Invalid_move else (r, c + 1)
      in 
      let saveVal = (get b goal) in 
      let b2 = LocMap.add b.hole saveVal b.mcontents in 
      {mcontents = LocMap.add goal 0 b2; size = b.size; hole = goal}

    let show = make_show get get_size
  end