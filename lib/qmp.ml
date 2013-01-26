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
  | Unit

type message =
  | Greeting of greeting
  | Command of command
  | Error of string
  | Success of result
  | Event of event

let message_of_string x =
  let int = function
  | `Int x -> x
  | _ -> failwith "int" in
  let string = function
  | `String x -> x
  | _ -> failwith "string" in
  match Yojson.Safe.from_string x with
  | `Assoc 
     [ ("QMP", `Assoc [ ("version", `Assoc [ "qemu", `Assoc version; "package", `String package ]); ("capabilities", _)] )] ->
    Greeting {
      minor = int (List.assoc "minor" version);
      major = int (List.assoc "major" version);
      micro = int (List.assoc "micro" version);
      package = package;  
    }
  | `Assoc [ ("execute", `String "qmp_capabilities") ] -> Command Qmp_capabilities
  | `Assoc [ ("execute", `String "stop") ] -> Command Stop
  | `Assoc [ ("execute", `String "query-commands") ] -> Command Query_commands
  | `Assoc [ ("execute", `String "query-status") ] -> Command Query_status
  | `Assoc [("timestamp", `Assoc [("seconds", `Int secs); ("microseconds", `Int usecs)]); ("event", `String event)] ->
    Event { secs; usecs; event }
  | `Assoc [("return", `Assoc [])] -> Success Unit
  | `Assoc [("return", `List ((`Assoc [ "name", `String _ ] :: _) as list) )] ->
    Success (Name_list (List.map (function
                                  | `Assoc [ "name", `String x ] -> x
                                  | _ -> failwith "assoc") list))
(*
  | `Assoc [("execute", `String "query-kvm"); ("id", `String "example")]
*)
  | `Assoc [("execute", `String "eject"); ("arguments", `Assoc [("device", `String device)])] -> Command (Eject device)
  | _ ->
    Error "unimplemented"

let string_of_message = function
  | Greeting g -> Printf.sprintf "Greeting { major = %d; minor = %d; micro = %d; package = %s }" g.major g.minor g.micro g.package
  | Command Qmp_capabilities -> "Command Qmp_capabilities"
  | Command Stop -> "Command Stop"
  | Command Query_commands -> "Command Query_commands"
  | Command Query_status -> "Command Query_status"
  | Command (Eject device) -> Printf.sprintf "Command Eject %s" device
  | Event e -> Printf.sprintf "Event { secs = %d; usecs = %d; event = %s }" e.secs e.usecs e.event
  | Success (Name_list xs) -> Printf.sprintf "Success [ %s ]" (String.concat ", " xs)
  | Success Unit -> "Success"
  | _ -> "unimplemented"


