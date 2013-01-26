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
  | Stop

type message =
  | Greeting of greeting
  | Command of command
  | Error of string
  | Success of string
  | Event of event

let message_of_string _ = Error "unimplemented"

let string_of_message _ = "unimplemented"

