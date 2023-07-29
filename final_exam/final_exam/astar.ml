open Pqueue

module type Task =
  sig
    type t

    val compare : t -> t -> int
    val eval    : t -> t -> int
    val next    : t -> t list
    val display : t -> string
  end

module AStar (T : Task) =
  struct
    (* Raised when no more task states are left on the queue. *)
    exception No_solution

    (* Raised when a solution is encountered while searching. *)
    exception Solved of T.t list

    (* The state of a search stored in the priority queue *)
    type qstate = {
      tstate : T.t;  (* a task state *)

      (* the list of task states that preceded this one in the search *)
      history : T.t list;  

      (* the number of moves to get to this state; this is just the length
         of the history list *)
      nmoves : int;   

      (* the overall fitness: a function of the tstate evaluation and nmoves *)
      fitness : int    
    }

    (* Make the priority queue.  Compare queue states by fitness. *)
    module Tqueue = MakePriorityQueue(struct
        type t = qstate
        let compare s1 s2 = Stdlib.compare s1.fitness s2.fitness
      end)

    (* A map of the best quality scores for each evaluated tstate. *)
    module Tmap = Map.Make(T)

    (* The state of the solver as a whole. *)
    type t = {
      queue : Tqueue.t;
      best  : int Tmap.t
    }

    (* The solver function. *)
    let solve goal init =
      let init_eval  = T.eval goal init in
      let init_state =
        {
           tstate  = init;
           history = [];
           nmoves  = 0;
           fitness = init_eval;
        }
      in
      let init_queue  = Tqueue.insert (Tqueue.empty) init_state in
      let init_best   = Tmap.empty in
      let init_solver = { queue = init_queue; best = init_best } in

      (* 
         The main solving loop using the priority queue.
       
         Invariant: tstates on the queue are not solved.

         1) Pop a qstate off the queue.  If the queue is empty,
            there is no solution.  Raise a No_solution exception.

         2) Check if the tstate in the popped qstate is a key in the `best`
            map: If it isn't, add it to the `best` map as a key with the number
            of moves leading to the task state (the `nmoves` field of the queue
            state) as the value.  If it is, compare it to the move count in the
            map. If the new number of moves is smaller than the one in the map,
            replace the binding in the map.  Otherwise, this task has been
            already searched for with a smaller (or at least no larger) number
            of moves, so there is no point in searching it again, so discard it
            and restart the loop.

         3) Assuming we got this far, compute the next qstates.
            Check to see if any of them contains a tstate which is a solution;
            if so, compute the final list of tasks and raise a Solved exception
            to break out of the loop.
            Otherwise, add them back into the queue and continue.  
       *)

      let rec iter { queue; best }  =
        let (qstate, newq) = Tqueue.pop_min queue in
        let b = qstate.tstate in
          let newm = 
            if not (Tmap.mem b best) then
              Tmap.add b qstate.nmoves best 
            else 
              let curMapVal = Tmap.find b best in
              if curMapVal > qstate.nmoves then
                Tmap.add b qstate.nmoves best
              else Tmap.empty
            in 
            if newm = Tmap.empty then iter { queue = newq; best = best }
          else 
            let rec run qlst pq =
              match qlst with
                | [] -> pq
                | h :: t -> 
                  let e = T.eval goal h in
                  if e = 0 then
                    raise (Solved (List.rev (h :: b :: qstate.history)))
                  else 
                    let stateh = 
                      {
                        tstate  = h;
                        history = b :: qstate.history;
                        nmoves  = qstate.nmoves + 1;
                        fitness = e + qstate.nmoves + 1;
                      }
                  in run t (Tqueue.insert pq stateh)
          in let nextbs = T.next b in
          iter { queue = run nextbs newq; best = newm }
      in
        (* The main part of the function starts here. *)
        if init_eval = 0 then
          [init]  (* handle the case when the initial state is solved. *)
        else
          try
            iter init_solver
          with 
            | Tqueue.Empty -> raise No_solution
            | Solved tlist -> tlist
  end

