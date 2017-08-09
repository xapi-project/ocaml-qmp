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

type vnc = {
  enabled : bool;
  auth    : string;
  family  : string;
  service : int;
  host    : string;
}

type xen_platform_pv_driver_info = {
  product_num : int;
  build_num   : int;
}

type fd_info = {
  fd       : int;
  fdset_id : int;
}

type result =
    Name_list of string list
  | Enabled of enabled
  | Status of string
  | Vnc of vnc
  | Xen_platform_pv_driver_info of xen_platform_pv_driver_info
  | Fd_info of fd_info
  | Unit
(** A successful RPC result *)

type greeting = {
  major : int;      (** qemu major version *)
  minor : int;      (** qemu minor version *)
  micro : int;      (** qemu micro version *)
  package : string; (** some information about the (binary?) package *)
}

type event = {
  timestamp : int * int; (** time the event occurred in (seconds, microseconds) *)
  event : string;    (** type of event *)
}

type error = { cls : string; descr : string; }

type id = string
(** identifier used to match responses with original requests *)

type command =
    Qmp_capabilities
  | Query_commands
  | Query_kvm
  | Query_status
  | Query_vnc
  | Query_xen_platform_pv_driver_info
  | Stop
  | Cont
  | Eject of string * bool option
  | Change of string * string * string option
  | System_powerdown
  | Xen_save_devices_state of string
  | Xen_load_devices_state of string
  | Xen_set_global_dirty_log of bool
  | Add_fd of int
  | Blockdev_change_medium of string * string
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
