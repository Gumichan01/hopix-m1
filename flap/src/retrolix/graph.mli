(** A module for undirected graphs.

    This module provides a functional data structure to represent a
    graph which nodes contain a set of labels and which edges can have
    one label too.

    We maintain the invariant that two nodes always have different
    labels: thus, nodes are identified by their labels.

*)

module type EdgeLabelSig = sig
    include Set.OrderedType
    (** [all] enumerates all the possible edge labels. *)
    val all : t list
    (** [to_string e] converts [e] in a human readable value. *)
    val to_string : t -> string
end

module type NodeLabelSig = sig
  include Set.OrderedType
    (** [to_string n] converts [n] in a human readable value. *)
  val to_string : t -> string
end

module Make (EdgeLabel : EdgeLabelSig) (NodeLabel : NodeLabelSig) : sig

  (** The type for graphs. *)
  type t

  (** The empty graph. *)
  val empty : t

  (** [add_node g [n1;...;nN]] returns a new graph that extends [g] with
      a new node labelled by [n1;...;nN]. None of the [nI] can be used
      by another node in [g]. Otherwise, [InvalidNode] is raised.

      In the sequel, the new node can be identified by any [nI].
  *)
  val add_node : t -> NodeLabel.t list -> t
  exception InvalidNode

  (** [add_edge g n1 e n2] returns a new graph that extends [g] with a
      new edge between [n1] and [n2]. The edge is labelled by [e]. If [n1]
      or [n2] does not exist, then [InvalidNode] is raised. *)
  val add_edge : t -> NodeLabel.t -> EdgeLabel.t -> NodeLabel.t -> t

  (** [del_node g n] returns a new graph that contains [g] minus the
      node [n] and its edges. *)
  val del_node : t -> NodeLabel.t -> t

  (** [neighbours g e n] returns the neighbours of [n] in [g]
      that are connected with an edge labelled by [e]. One neighbour is
      characterized by all its node labels. *)
  val neighbours : t -> EdgeLabel.t -> NodeLabel.t -> NodeLabel.t list list

  (** [edges g e] returns all the edges of kind [e] in [g].
      WARNING: This function is inefficient! Use it only for debugging. *)
  val edges : t -> EdgeLabel.t -> (NodeLabel.t list * NodeLabel.t list) list


  (** [show g labels] runs [dotty] to display the graph [g]. [labels n] may
      optionally return an additional information to be display in the node
      for [n]. *)
  val show : t -> (NodeLabel.t -> string option) -> unit
  val dump : t -> string

end


