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
  timestamp: float;
  event: string;
}

type enabled = {
  enabled: bool;
  present: bool;
}

type command =
  | Qmp_capabilities
  | Query_commands
  | Query_kvm
  | Query_status
  | Stop
  | Cont
  | Eject of string
  | System_powerdown

type result =
  | Name_list of string list
  | Enabled of enabled
  | Status of string
  | Unit

type error = {
  cls: string;
  descr: string;
}

type id = string

type message =
  | Greeting of greeting
  | Command of (id option * command)
  | Error of (id option * error)
  | Success of (id option * result)
  | Event of event

let message_of_string x =
  let int = function
  | `Int x -> x
  | _ -> failwith "int" in
  let float = function
  | `Int x -> float_of_int x
  | _ -> failwith "float" in
  let string = function
  | `String x -> x
  | _ -> failwith "string" in
  let assoc = function
  | `Assoc x -> x
  | _ -> failwith "assoc" in
  let bool = function
  | `Bool x -> x
  | _ -> failwith "bool" in
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
    let secs = float (List.assoc "seconds" timestamp) in
    let usecs = float (List.assoc "microseconds" timestamp) in
    let timestamp = secs +. usecs /. 1e6 in
    Event { timestamp; event }
  | `Assoc list when List.mem_assoc "execute" list ->
    let id = if List.mem_assoc "id" list then Some (string (List.assoc "id" list)) else None in
    Command (id, (match string (List.assoc "execute" list) with
      | "qmp_capabilities" -> Qmp_capabilities
      | "stop" -> Stop
      | "cont" -> Cont
      | "system_powerdown" -> System_powerdown
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
      | `Assoc list when List.mem_assoc "enabled" list ->
        let enabled = bool (List.assoc "enabled" list) in
        let present = bool (List.assoc "present" list) in
        Enabled {enabled; present}
      | `List ((`Assoc pair :: _) as list) when List.mem_assoc "name" pair ->
        Name_list (List.map (function
                             | `Assoc [ "name", `String x ] -> x
                             | _ -> failwith "assoc") list)
      | x -> failwith (Printf.sprintf "unknown result %s" (Yojson.Safe.to_string x))
    ))
  | `Assoc list when List.mem_assoc "error" list ->
    let id = if List.mem_assoc "id" list then Some (string (List.assoc "id" list)) else None in
    let error = assoc (List.assoc "error" list) in
    let cls = string (List.assoc "class" error) in
    let descr = string (List.assoc "desc" error) in
    Error (id, {cls; descr})
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
      | Cont -> "cont", []
      | System_powerdown -> "system_powerdown", []
      | Query_commands -> "query-commands", []
      | Query_status -> "query-status", []
      | Query_kvm -> "query-kvm", []
      | Eject device -> "eject", [ "device", `String device ] in
    let args = match args with [] -> [] | args -> [ "arguments", `Assoc args ] in
    `Assoc (("execute", `String cmd) :: id @ args)
  | Event {timestamp; event} ->
    let usecs, secs = modf timestamp in
    `Assoc [("event", `String event); ("timestamp", `Assoc [ "seconds", `Int (int_of_float secs); "microseconds", `Int (int_of_float (usecs *. 1e6)) ])]
  | Success(id, result) ->
    let id = match id with None -> [] | Some x -> [ "id", `String x ] in
    let result = match result with
      | Unit -> `Assoc []
      | Status s -> `Assoc [ "status", `String s ]
      | Enabled {enabled; present} -> `Assoc [ "enabled", `Bool enabled; "present", `Bool present ]
      | Name_list xs -> `List (List.map (fun x -> `Assoc [ "name", `String x ]) xs) in
    `Assoc (("return", result) :: id)
  | Error(id, e) ->
    let id = match id with None -> [] | Some x -> [ "id", `String x ] in
    let e = `Assoc [ "class", `String e.cls; "desc", `String e.descr; "data", `Assoc [] ] in
    `Assoc (("error", e) :: id)

let string_of_message m = Yojson.Safe.to_string (json_of_message m)

