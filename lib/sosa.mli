(* Copyright (c) 1998-2007 INRIA *)

type t

val zero : t
val one : t
val eq : t -> t -> bool
val gt : t -> t -> bool
val add : t -> t -> t
val sub : t -> t -> t
val twice : t -> t
val half : t -> t
val even : t -> bool
val inc : t -> int -> t
val mul : t -> int -> t
val exp : t -> int -> t
val div : t -> int -> t
val modl : t -> int -> int
val gen : t -> int
val branch : t -> char
val sosa_gen_up : t -> t
val print : (string -> unit) -> string -> t -> unit
val of_int : int -> t
val of_string : string -> t
val to_string : t -> string
val to_string_sep : string -> t -> string
val to_string_sep_base : string -> int -> t -> string
