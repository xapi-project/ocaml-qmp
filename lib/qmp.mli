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

type enabled = {
  enabled : bool; (** feature is present and turned on *)
  present : bool; (** feature is present (but may not be turned on) *)
}

type result =
    Name_list of string list
  | Enabled of enabled
  | Status of string
  | Unit
(** A successful RPC result *)

type greeting = {
  major : int;      (** qemu major version *)
  minor : int;      (** qemu minor version *)
  micro : int;      (** qemu micro version *)
  package : string; (** some information about the (binary?) package *)
}

type event = {
  secs : int;       (** time the event occurred in secs ... *)
  usecs : int;      (** ... and usecs *)
  event : string;   (** type of event *)
}

type error = { cls : string; descr : string; }

type id = string
(** identifier used to match responses with original requests *)

type command =
    Qmp_capabilities
  | Query_commands
  | Query_kvm
  | Query_status
  | Stop
  | Cont
  | Eject of string
  | System_powerdown
(** commands that may be sent to qemu *)

type message =
    Greeting of greeting
  | Command of (id option * command)
  | Error of (id option * error)
  | Success of (id option * result)
  | Event of event
(** an individual message sent or received to or from qemu *)

val message_of_string : string -> message
val string_of_message : message -> string

val json_of_message : message -> Yojson.Safe.json
