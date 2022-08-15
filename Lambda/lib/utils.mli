(** Copyright 2021-2022, Kakadu and contributors *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

val list_remove : string -> string list -> string list
val free_vars : string Ast.t -> string list
val is_free_in : string -> string Ast.t -> bool

type error =
  | UnknownVariable of string
  | ParsingErrorDescription

(** Smart constructors for Abstract Syntax Tree *)

val var : 'a -> 'a Ast.t
val abs : 'a -> 'a Ast.t -> 'a Ast.t
val app : 'a Ast.t -> 'a Ast.t -> 'a Ast.t

module type MONAD_FAIL = sig
  type ('a, 'e) t

  val ( >>= ) : ('a, 'e) t -> ('a -> ('b, 'e) t) -> ('b, 'e) t
  val ( >>| ) : ('a, 'e) t -> ('a -> 'b) -> ('b, 'e) t

  module Let_syntax : sig
    val return : 'a -> ('a, 'b) t
    val ( >>= ) : ('a, 'e) t -> ('a -> ('b, 'e) t) -> ('b, 'e) t
    val ( >>| ) : ('a, 'e) t -> ('a -> 'b) -> ('b, 'e) t

    module Let_syntax : sig
      val return : 'a -> ('a, 'b) t
      val bind : ('a, 'e) t -> f:('a -> ('b, 'e) t) -> ('b, 'e) t
      val map : ('a, 'e) t -> f:('a -> 'b) -> ('b, 'e) t
      val both : ('a, 'e) t -> ('b, 'e) t -> ('a * 'b, 'e) t

      module Open_on_rhs : sig end
    end
  end

  module Monad_infix : sig
    val ( >>= ) : ('a, 'e) t -> ('a -> ('b, 'e) t) -> ('b, 'e) t
    val ( >>| ) : ('a, 'e) t -> ('a -> 'b) -> ('b, 'e) t
  end

  val bind : ('a, 'e) t -> f:('a -> ('b, 'e) t) -> ('b, 'e) t
  val return : 'a -> ('a, 'b) t
  val map : ('a, 'e) t -> f:('a -> 'b) -> ('b, 'e) t
  val join : (('a, 'e) t, 'e) t -> ('a, 'e) t
  val ignore_m : ('a, 'e) t -> (unit, 'e) t
  val all : ('a, 'e) t list -> ('a list, 'e) t
  val all_unit : (unit, 'e) t list -> (unit, 'e) t
  val fail : 'e -> ('a, 'e) t
end
