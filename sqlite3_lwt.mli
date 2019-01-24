(* Sqlite3_lwt
  Copyright 2018 Ramil Farkhshatov
  All rights reserved. This file is distributed under the terms of the
  GNU Lesser General Public License version 3 with OCaml linking exception *)

exception Rc of Sqlite3.Rc.t
(** *)

type transaction = Default | Immediate | Deferred | Exclusive
(** Type of transaction (see [https://sqlite.org/lang_transaction.html]). *)

val busy_num_tries: int ref
(** Number of retries of [Sqlite3.step] when it returns [BUSY]. See [next]. *)

val busy_wait_s: float ref
(** Time in seconds to wait before next try. See [next]. *)

val next:
  ?busy_num_tries:int ->
  ?busy_wait_s:float -> Sqlite3.stmt -> Sqlite3.Rc.t Lwt.t
(** Lwt-wrapped [Sqlite3.step]. When the latter returns [BUSY] it
    is recalled after [busy_wait_s] seconds. The number of tries <
    [busy_num_tries]. *)

val single:
  ?busy_num_tries:int ->
  ?busy_wait_s:float -> 
  Sqlite3.stmt -> (Sqlite3.stmt -> 'a Lwt.t) -> 'a option Lwt.t
(** A version of [next] which manages [Sqlite3.step] return values.
  [proc] is called on [DONE] and [ROW] codes. *)

val fold_left:
  ?busy_num_tries:int ->
  ?busy_wait_s:float ->
  Sqlite3.stmt -> 'a -> (Sqlite3.stmt -> 'a -> 'a Lwt.t) -> 'a Lwt.t
(** [fold_left stmt initial proc] performs 'fold left' operation on SQL
    query result rows. *)

val bind_parameter: Sqlite3.stmt -> string -> Sqlite3.Data.t -> unit
(** Binds parameter in statement by name. *)

val bind_parameters: Sqlite3.stmt -> (string * Sqlite3.Data.t) list -> unit
(** Binds parameters in statement by name. *)

val statement_of_sql_with_parameters:
  Sqlite3.db -> string -> (string * Sqlite3.Data.t) list -> Sqlite3.stmt
(** Creates [Sqlite3.stmt] of SQL string and a list of named parameters. *)

val with_statement:
  Sqlite3.db ->
  string ->
  ?parameters:(string * Sqlite3.Data.t) list ->
  (Sqlite3.stmt -> 'a Lwt.t) -> 'a Lwt.t
(** [with_statement db sql ~parameters proc] creates [Sqlite3.stmt] of [sql],
    binds named [parameters] and applies it to [proc]. [Sqlite3.stmt]
    instance is finalized after [proc] either returns or raises an exception.
    *)

val with_transaction:
  Sqlite3.db -> Lwt_mutex.t -> transaction -> (unit -> 'a Lwt.t) -> 'a Lwt.t
(** [with_transaction db mutex transaction proc] starts transaction of type
    [transaction], then calls [proc] committing the transaction after it
    returns and canceling it after it raises an exception. *)

val single_sql:
  Sqlite3.db ->
  string ->
  ?parameters:(string * Sqlite3.Data.t) list ->
  ?busy_num_tries:int ->
  ?busy_wait_s:float -> (Sqlite3.stmt -> 'a Lwt.t) -> 'a option Lwt.t
(** A version of [single] which manages the creation of [Sqlite3.stmt].
    [single_sql db sql proc] applies created [Sqlite3.stmt] to [proc].
    See [with_statement]. *)

val fold_left_sql:
  Sqlite3.db ->
  string ->
  ?parameters:(string * Sqlite3.Data.t) list ->
  ?busy_num_tries:int ->
  ?busy_wait_s:float -> 'a -> (Sqlite3.stmt -> 'a -> 'a Lwt.t) -> 'a Lwt.t
(** A version of [fold_left] which manages the creation of [Sqlite3.stmt].
    See [with_statement]. *)

val batch_sql: Sqlite3.db -> string -> ?busy_num_tries:int ->
               ?busy_wait_s:float -> unit -> unit Lwt.t
(** Executes multi expression SQL string using [Sqlite3.prepare_tail] and
    [single]. *)

val ignore_result: 'a -> unit Lwt.t
(** A convenience function to use with [single_sql]. *)

val single_column: Sqlite3.stmt -> Sqlite3.Data.t Lwt.t
(** A convenience function to use with [single_sql]. Returns the data of
    the first returned column. *)

val to_array: Sqlite3.stmt -> Sqlite3.Data.t array Lwt.t
(** A convenience function to use with [single_sql]. Returns the first
    row as array. *)
