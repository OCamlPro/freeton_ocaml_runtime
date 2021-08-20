type cell_hash = string
(* sha-256 of a serialized cell *)

type cell

val serialize : cell -> bytes
val deserialize : string -> cell

module Builder : sig

  type t
  (** The type for imperative-style cell builders. Used for writing cells. *)

  val make : unit -> t
  (** Create a new empty builder. *)

  val to_cell : t -> cell
  (** Turn a finalized builder into a cell. *)

  val store_bits : t -> string -> int -> int -> unit
  (** [store_bits builder s offset length] will take the bits in [s] and append
      them to [builder]'s data. Both [offset] and [length] must be specified in
      bits. Will raise a [Failure] exception if it overflows the builder's limit
      size of 1024 bits.
  *)

  val store_cell : t -> cell_hash -> unit
  (** Add a cell reference to the builder. Will raise a [Failure] exception if
      it overflows the builder's limit of 4 references.
  *)

end

module Slice : sig

  type t
  (** The type for imperative-style cell slices. Used for reading cells. *)

  val of_cell : cell -> t
  (** Generate a fresh slice from a cell. *)

  val remaining_bits : t -> int
  (** Return the number of bits left to read in the slice *)

  val remaining_references : t -> int
  (** Return the number of references let to read in the slice *)

  val is_at_end : t -> bool
  (** Will return [true] if reading on the slice is complete. Will return
      [false] otherwise.
  *)

  val get_bits : t -> int -> string
  (** [get_bits slice length] will return the next [length] bits in [slice].
      Will raise a [Failure] exception if attempting to get more than the
      slice's remaining size. *)

  val get_ref : t -> cell_hash
  (** Return the next reference in the slice. Will raise a [Failure] exception
  if no reference is left to obtain. *)

end
