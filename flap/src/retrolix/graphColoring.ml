(** This module implements a generic graph coloring algorithm. *)

module type ColorsSig = sig
  type t
  (** [all] enumerates the available colors. *)
  val all : t list
  val cardinal : int
  val to_string : t -> string
end

module Make
  (EdgeLabel : sig
    include Graph.EdgeLabelSig
    (** A conflict edge imposes a distinct color on its two nodes. *)
    val conflict   : t
    (** A preference edge indicates that two nodes should have the
	same color if possible. *)
    val preference : t
  end)
  (NodeLabel : Graph.NodeLabelSig)
  (Colors : ColorsSig)
  =
struct

  module Graph = Graph.Make (EdgeLabel) (NodeLabel)

  module NodeLabelMap = Map.Make (NodeLabel)

  (** A coloring maps every node label to a color option. *)
  type t = Colors.t option NodeLabelMap.t

  (** The empty coloring. *)
  let empty = NodeLabelMap.empty

  (** Return the color of the node [n] in the coloring. *)
  let color_of_node coloring n = NodeLabelMap.find n coloring

  (** Assign the color [c] to the node [n] in the [coloring]. *)
  let assign_color n c coloring = NodeLabelMap.add n (Some c) coloring

  (** Assign no color to the node [n] in the [coloring]. *)
  let assign_no_color n coloring = NodeLabelMap.add n None coloring

  (** [InvalidColoring c] is raised if [c] is not a valid coloring. *)
  exception InvalidColoring of t


  (** [check_coloring g c] checks if [c] is a valid coloring for [g]
      i.e. that every pair of conflicting nodes have different
      colors. *)
  let check_coloring g c =
    failwith "Students! This is your job!"

  type pick_result =
    | EmptyGraph
    | SimplifiableNode of NodeLabel.t
    | MaybeSpillNode of NodeLabel.t

  (** [pick g] returns a node of degree less than the number [k] of
      colors if there is such node in [g]. Otherwise, it returns
      some node if [g] is not empty. If [g] is empty, it returns
      [EmptyGraph]. *)
  let pick g : pick_result =
  (*if Set.S.is_empy (g.degrees) then*)
    failwith "Students! This is your job!"


  (** [colorize g] returns a coloring for [g]. *)
  let rec colorize (g : Graph.t) : t =
    (*failwith "Students! This is your job!"*)
end

let test () =
  (** The test parameters

      Customize them to test your implementation in an appropriate
      way.
  *)
  let show = false in
  let nb_test = 1000 in
  let nb_color = 2 in
  let min_nodes = 10 and max_nodes = 20 in
  let freq_conflict = 0.1 and freq_preference = 0.01 in
  let random_seed = 31 in

  (** We instantiate the functor on simple nodes, edges and colors. *)
  let module NodeLabel = struct
    type t = string
    let compare = compare
    let to_string x = x
  end in
  let module EdgeLabel = struct
    type t = C | P
    let compare = compare
    let to_string = function C -> "<>" | P -> "♥"
    let preference = P
    let conflict = C
    let all = [ C; P ]
  end in
  let module Colors = struct
    type t = int
    let all = ExtStd.List.range 0 (nb_color - 1)
    let cardinal = nb_color
    let to_string = string_of_int
  end
  in
  let module GC = Make (EdgeLabel) (NodeLabel) (Colors) in GC.(

    (** A function to generate a random graph. *)

    Random.init random_seed;
    let random_graph () =
      let nb_nodes = ExtStd.Random.int_in_range min_nodes max_nodes in
      let ns =
	List.map
	  (fun i -> "n" ^ string_of_int i)
	  (ExtStd.List.range 0 (nb_nodes - 1))
      in
      let g = List.fold_left (fun g n -> Graph.add_node g [n]) Graph.empty ns in
      List.fold_left (fun g n1 ->
	List.fold_left (fun g n2 ->
	  if n1 = n2 then g
	  else if Random.float 1. < freq_conflict then
	    Graph.add_edge g n1 EdgeLabel.C n2
	  else if Random.float 1. < freq_preference then
	    Graph.add_edge g n1 EdgeLabel.P n2
	  else
	    g
	) g ns
      ) g ns
    in

    let one_test () =
      let g = random_graph () in
      (** Show the graph! *)
      if show then Graph.show g (fun _ -> None);
      (** Compute the coloring. *)
      let coloring = colorize g in
      (** Show the coloring! *)
      if show then Graph.show g (fun n ->
	try
	  Option.map Colors.to_string (color_of_node coloring n)
	with Not_found -> Some "!"
      );
      (** Check the coloring! *)
      check_coloring g coloring
    in
    for i = 0 to nb_test - 1 do
      one_test ()
    done
  )
