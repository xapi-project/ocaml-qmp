(*
 * Copyright (C) 2013 Citrix Systems Inc.
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published
 * by the Free Software Foundation; version 2.1 only. with the special
 * exception on linking described in file LICENSE.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 *)

type greeting = {
  major: int;
  minor: int;
  micro: int;
  package: string;
}

type event = {
  secs: int;
  usecs: int;
  event: string;
}

type command =
  | Qmp_capabilities
  | Query_commands
  | Query_kvm
  | Query_status
  | Stop
  | Eject of string

type result =
  | Name_list of string list
  | Status of string
  | Unit

type id = string

type message =
  | Greeting of greeting
  | Command of (id option * command)
  | Error of (id option * string)
  | Success of (id option * result)
  | Event of event

let message_of_string x =
  let int = function
  | `Int x -> x
  | _ -> failwith "int" in
  let string = function
  | `String x -> x
  | _ -> failwith "string" in
  let assoc = function
  | `Assoc x -> x
  | _ -> failwith "assoc" in
  match Yojson.Safe.from_string x with
  | `Assoc 
     [ ("QMP", `Assoc [ ("version", `Assoc [ "qemu", `Assoc version; "package", `String package ]); ("capabilities", _)] )] ->
    Greeting {
      minor = int (List.assoc "minor" version);
      major = int (List.assoc "major" version);
      micro = int (List.assoc "micro" version);
      package = package;  
    }
  | `Assoc list when List.mem_assoc "event" list ->
    let event = string (List.assoc "event" list) in
    let timestamp = assoc (List.assoc "timestamp" list) in
    let secs = int (List.assoc "seconds" timestamp) in
    let usecs = int (List.assoc "microseconds" timestamp) in
    Event { secs; usecs; event }
  | `Assoc list when List.mem_assoc "execute" list ->
    let id = if List.mem_assoc "id" list then Some (string (List.assoc "id" list)) else None in
    Command (id, (match string (List.assoc "execute" list) with
      | "qmp_capabilities" -> Qmp_capabilities
      | "stop" -> Stop
      | "query-commands" -> Query_commands
      | "query-status" -> Query_status
      | "query-kvm" -> Query_kvm
      | "eject" -> Eject (string (List.assoc "device" (assoc (List.assoc "arguments" list))))
      | x -> failwith (Printf.sprintf "unknown command %s" x)
    ))
  | `Assoc list when List.mem_assoc "return" list ->
    let id = if List.mem_assoc "id" list then Some (string (List.assoc "id" list)) else None in
    Success (id, (match List.assoc "return" list with
      | `Assoc [] -> Unit
      | `Assoc list when List.mem_assoc "status" list ->
        Status (string (List.assoc "status" list))
      | `List ((`Assoc pair :: _) as list) when List.mem_assoc "name" pair ->
        Name_list (List.map (function
                             | `Assoc [ "name", `String x ] -> x
                             | _ -> failwith "assoc") list)
      | x -> failwith (Printf.sprintf "unknown result %s" (Yojson.Safe.to_string x))
    ))
  | x ->
    failwith (Printf.sprintf "message_of_string %s" (Yojson.Safe.to_string x))

let json_of_message = function
  | Greeting { major; minor; micro; package } ->
    let version = [ "major", `Int major; "minor", `Int minor; "micro", `Int micro ] in
    `Assoc [ ("QMP", `Assoc [ ("version", `Assoc [ "qemu", `Assoc version; "package", `String package ]); ("capabilities", `List []) ])]
  | Command(id, cmd) ->
    let id = match id with None -> [] | Some x -> [ "id", `String x ] in
    let cmd, args = match cmd with
      | Qmp_capabilities -> "qmp_capabilities", []
      | Stop -> "stop", []
      | Query_commands -> "query-commands", []
      | Query_status -> "query-status", []
      | Query_kvm -> "query-kvm", []
      | Eject device -> "eject", [ "device", `String device ] in
    let args = match args with [] -> [] | args -> [ "arguments", `Assoc args ] in
    `Assoc (("execute", `String cmd) :: id @ args)
  | Event {secs; usecs; event} ->
    `Assoc [("event", `String event); ("timestamp", `Assoc [ "seconds", `Int secs; "microseconds", `Int usecs ])]
  | Success(id, result) ->
    let id = match id with None -> [] | Some x -> [ "id", `String x ] in
    let result = match result with
      | Unit -> `Assoc []
      | Status s -> `Assoc [ "status", `String s ]
      | Name_list xs -> `List (List.map (fun x -> `Assoc [ "name", `String x ]) xs) in
    `Assoc (("return", result) :: id)
  | Error(id, e) ->
    failwith "json_of_message Error"

let string_of_message m = Yojson.Safe.to_string (json_of_message m)



