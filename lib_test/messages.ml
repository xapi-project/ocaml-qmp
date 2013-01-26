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

open OUnit
open Qmp

let files = [
  "capabilities.json",          Command Qmp_capabilities;
  "error.json",                 Error "{\"class\": \"JSONParsing\", \"desc\": \"Invalid JSON syntax\", \"data\": {}}";
  "greeting.json",              Greeting { major = 1; minor = 1; micro = 0; package = "(Debian 1.1.0+dfsg-1)" };
  "powerdown.json",             Event { secs = 1258551470; usecs = 802384; event = "POWERDOWN" };
  "query-commands.json",        Command Query_commands;
  "query-commands-return.json", Success "";
  "query_kvm.json",             Command Query_kvm;
  "query_jvm-return.json",      Success "";
  "stop.json",                  Command Stop;
  "success.json",               Success "";
]


let _ =
  let verbose = ref false in
  Arg.parse [
    "-verbose", Arg.Unit (fun _ -> verbose := true), "Run in verbose mode";
  ] (fun x -> Printf.fprintf stderr "Ignoring argument: %s" x)
    "Test message parsing/printing code";

  let suite = "qmp" >:::
    [
    ] in
  run_test_tt ~verbose:!verbose suite



