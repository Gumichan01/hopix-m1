(**

   The register allocation translates a Retrolix program into an
   equivalent Retrolix program that uses at hardware registers as
   much as possible to hold intermediate results.

   Register allocation is done in two steps:

   - a static analysis called "Liveness Analysis of Variables" is
     performed to compute a graph. It approximates the interference
     relation of program variables, i.e. the intersection between the
     live ranges of variables. The nodes of the graph are the program
     variables and, in this graph, a node for 'x' and a node 'y' are
     connected iff 'x' and 'y' are in interference.

   - a graph coloring algorithm is executed on the interference graph:
     if two variables live at the same time, then their values cannot
     be carried by the same register ; thus, it suffices to use a different
     color for their nodes. Graph coloring is NP-complete. Yet, we will
     use a simple recursive algorithm that provides good results in
     practice.

*)

open RetrolixAST

module LabelMap = Map.Make (struct
  type t = label
  let compare = compare
end)

(** Liveness Analysis. *)
type location = lvalue

module LSet = Set.Make (struct
    type t = location
    let compare = compare
end)

type liveness_analysis_result = {
  live_in  : LSet.t LabelMap.t;
  live_out : LSet.t LabelMap.t;
}

let find_default d k m =
  try LabelMap.find k m with Not_found -> d

let empty_results =
  {
    live_in = LabelMap.empty;
    live_out = LabelMap.empty;
  }

(** [def i] returns the variables defined by [i]. *)
let def i =
  failwith "Student! This is your job!"

(** [use i] returns the variables defined by [i]. *)
let use i =
  failwith "Student! This is your job!"

(** [predecessors p] returns a function [p] such that [p l] returns
    the predecessors of [l] in the control flow graph. *)
let predecessors p =
  failwith "Student! This is your job!"

(** [liveness_analysis p] returns the liveness analysis of [p]. *)
let liveness_analysis p =
  ()

(** Interference graph. *)
let interference_graph p liveness =
  ()

(** Graph coloring. *)
let colorize_graph g =
  ()

(** Register allocation directed by the graph coloring. *)
let register_allocation coloring p =
  p

(** Putting all together. *)
let translate p =
  let liveness = liveness_analysis p in
  let igraph   = interference_graph p liveness in
  let coloring = colorize_graph igraph in
  register_allocation coloring p
