(** Copyright 2021-2022, Kakadu and contributors *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

module Interpret : functor (M : Utils.MONAD_FAIL) -> sig
  val run : 'a Ast.t -> (int, Utils.error) M.t
end

val parse_and_run : string -> (int, Utils.error) result
