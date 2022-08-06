(** Copyright 2021-2022, Kakadu and contributors *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

(** Smart constructors for Abstract Syntax Tree *)

val var : 'a -> 'a Ast.t
val abs : 'a -> 'a Ast.t -> 'a Ast.t
val app : 'a Ast.t -> 'a Ast.t -> 'a Ast.t

type named = string Ast.t

val next_name : string -> old:string list -> string

(** Call [subst name ~by inside] replaces all occurences of variable named [name]
    inside lambda expresssion [inside]
    by new lambda expression [by] *)
val subst : string -> by:named -> named -> named

(** Am evaluation strategy for Untyped Lambda Calculus.
    Emulation of late binding  *)
type strat =
  { on_var : strat -> string -> named
  ; on_abs : strat -> string -> named -> named
  ; on_app : strat -> named -> named -> named
  }

(** Strategies *)

val apply_strat : strat -> named -> named
val without_strat : strat
val cbn_strat : strat
val nor_strat : strat
val cbv_strat : strat
val ao_strat : strat

(** Helpers to construct lambda exrpressions for examples *)

val a : named
val x : named
val y : named
val z : named
val f : named
val g : named
val h : named
val m : named
val n : named
val p : named
val zero : named
val one : named
val two : named
val three : named
