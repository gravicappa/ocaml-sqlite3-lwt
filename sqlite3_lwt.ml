(* Sqlite3_lwt
  Copyright 2018 Ramil Farkhshatov
  All rights reserved. This file is distributed under the terms of the
  GNU Lesser General Public License version 3 with OCaml linking exception *)

exception Rc of Sqlite3.Rc.t;;

type transaction = Default | Immediate | Deferred | Exclusive;;

let busy_num_tries = ref 10;;
let busy_wait_s = ref 0.2;;

let next ?(busy_num_tries = !busy_num_tries) ?(busy_wait_s = !busy_wait_s)
         stmt =
  let rec try_repeat stmt n =
    if n < busy_num_tries then begin
      let%lwt () = Lwt.pause () in
      let%lwt () = Lwt_unix.sleep busy_wait_s in
      try_repeat stmt (n + 1)
    end else
      Lwt.return Sqlite3.Rc.BUSY
  and loop stmt _ =
    match Sqlite3.step stmt with
    | BUSY -> try_repeat stmt 0
    | res -> Lwt.return res
  in loop stmt 0;;

let single ?(busy_num_tries = !busy_num_tries)
           ?(busy_wait_s = !busy_wait_s) 
           stmt proc =
  match%lwt next ~busy_num_tries ~busy_wait_s stmt with
  | ROW -> 
      let%lwt res = proc stmt in
      Lwt.return_some res
  | DONE -> Lwt.return_none
  | res -> Lwt.fail (Rc res);;

let fold_left ?(busy_num_tries = !busy_num_tries)
              ?(busy_wait_s = !busy_wait_s)
              stmt init proc =
  let rec loop value =
    match%lwt next ~busy_num_tries ~busy_wait_s stmt with
    | ROW ->
        let%lwt value = (proc stmt value) in
        loop value
    | DONE -> Lwt.return value
    | res -> Lwt.fail (Rc res) in
  loop init;;

let ignore_result _ = Lwt.return_unit;;

let single_column stmt =
  if Sqlite3.data_count stmt > 0 then
    Lwt.return (Sqlite3.column stmt 0)
  else
    Lwt.return Sqlite3.Data.NONE;;

let to_array stmt =
  let n = Sqlite3.data_count stmt in
  let arr = Array.make n Sqlite3.Data.NONE in
  let rec loop i = 
    if i < n then begin
      Array.set arr i (Sqlite3.column stmt i);
      loop (i + 1)
    end else Lwt.return arr in
  loop 0;;

let batch_sql db sql ?(busy_num_tries = !busy_num_tries)
              ?(busy_wait_s = !busy_wait_s) () =
  let exec stmt = single ~busy_num_tries ~busy_wait_s stmt ignore_result in
  let next stmt =
    Lwt.finalize (fun () ->
      let%lwt _ = exec stmt in
      match Sqlite3.prepare_tail stmt with
      | Some stmt -> Lwt.return (Some stmt)
      | None -> Lwt.return_none)
    (fun () ->
      let _ = Sqlite3.finalize stmt in
      Lwt.return_unit) in
  let rec loop stmt =
    match%lwt next stmt with
    | Some stmt -> loop stmt
    | None -> Lwt.return_unit in
  Sqlite3.prepare db sql |> loop;;

let bind_parameter stmt name value = 
  let prefixes = ["?"; ":"; "$"; "@"] in
  let bind_if_exists name data =
    try
      let idx = Sqlite3.bind_parameter_index stmt name in
      let _ = Sqlite3.bind stmt idx data in
      true
    with | Not_found -> false in
  let rec bind name data = function
    | prefix :: prefixes when not (bind_if_exists (prefix ^ name) data) ->
        bind name data prefixes
    | _ -> () in
  bind name value prefixes;;

let bind_parameters stmt parameters =
  List.iter (fun (name, value) -> bind_parameter stmt name value) parameters;;

let statement_of_sql_with_parameters db sql parameters =
  let stmt = Sqlite3.prepare db sql in
  let () = bind_parameters stmt parameters in
  stmt;;

let with_statement db sql ?(parameters = []) proc =
  let stmt = statement_of_sql_with_parameters db sql parameters in
  Lwt.finalize (fun () -> proc stmt)
               (fun () ->
                 let _ = Sqlite3.finalize stmt in
                 Lwt.return_unit);;

let single_sql db sql ?(parameters = []) ?(busy_num_tries = !busy_num_tries)
               ?(busy_wait_s = !busy_wait_s) proc =
  with_statement db sql ~parameters @@ fun stmt ->
    single stmt ~busy_num_tries ~busy_wait_s proc;;

let fold_left_sql db sql ?(parameters = [])
                  ?(busy_num_tries = !busy_num_tries)
                  ?(busy_wait_s = !busy_wait_s) init proc =
  with_statement db sql ~parameters @@ fun stmt ->
    fold_left stmt ~busy_num_tries ~busy_wait_s init proc;;

let with_transaction db mutex behaviour proc =
  let start_sql = function
    | Default -> "BEGIN TRANSACTION"
    | Immediate -> "BEGIN IMMEDIATE TRANSACTION"
    | Deferred -> "BEGIN DEFERRED TRANSACTION"
    | Exclusive -> "BEGIN EXCLUSIVE TRANSACTION" in
  Lwt_mutex.with_lock mutex @@ fun () ->
    let _ = Sqlite3.exec db (start_sql behaviour) in
    try%lwt
      let%lwt res = proc () in
      let _ = Sqlite3.exec db "COMMIT TRANSACTION" in
      Lwt.return res
    with exn ->
      let _ = Sqlite3.exec db "ROLLBACK TRANSACTION" in
      Lwt.fail exn;;
