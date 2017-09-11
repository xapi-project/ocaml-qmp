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

type enabled = {
  enabled: bool;
  present: bool;
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

type qom = {
  name : string;
  ty   : string;
}

type params = {
  bus     : string;
  hostbus : string;
  hostport: string;
}

type device = {
  driver : string;
  id     : string;
  params : params option;
}

type command =
  | Qmp_capabilities
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
  | Add_fd of int option
  | Remove_fd of int
  | Blockdev_change_medium of string * string
  | Device_add of string * string * (string * string * string) option
  | Device_del of string
  | Qom_list of string

type result =
  | Name_list of string list
  | Enabled of enabled
  | Status of string
  | Vnc of vnc
  | Xen_platform_pv_driver_info of xen_platform_pv_driver_info
  | Fd_info of fd_info
  | Unit
  | Qom of qom list

type event_data =
    RTC_CHANGE of int
    (* extend this to support other qmp events data*)

type event = {
  timestamp: (int * int);
  event: string;
  data: event_data option
}

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

module Event = struct

  (* Emitted when XEN PV driver write build number to io-port 0x10,
     marking the end of preamble:
   # <- { "event": "XEN_PLATFORM_PV_DRIVER_INFO",
   #      "data": { "product-num": 3, "build-num": 1},
   #      "timestamp": { "seconds": 1500394278, "microseconds": 878290 } }
  *)
  let _XEN_PLATFORM_PV_DRIVER_INFO = "XEN_PLATFORM_PV_DRIVER_INFO"
end

(* class associated with a error message from an Invalid JSON syntax *)
let _JSONParsing = "JSONParsing"

let message_of_string str =
  let module Y = Yojson.Safe in
  let module U = Yojson.Safe.Util in

  let subset_of xs args = List.for_all (fun x->List.mem x args) xs in

  let id json = json |> U.member "id" |> U.to_string_option in

  let greeting json =
    let qmp = json |> U.member "QMP" in
    let version x = qmp |> U.member "version" |> U.member "qemu" |> U.member x |> U.to_int in
    { minor   = version "minor"
    ; major   = version "major"
    ; micro   = version "micro"
    ; package = qmp |> U.member "version" |> U.member "package" |> U.to_string
    }
  in

  let event json =
    let event_data event data =
      let rtc_offset data = data |> U.member "offset" |> U.to_int in
      match event with
        | "RTC_CHANGE"      -> data |> rtc_offset |> fun x -> Some (RTC_CHANGE x)
        (* ignore data for other events *)
        | _ -> None
    in
    let event = json |> U.member "event" |> U.to_string in
    let ts x = json |> U.member "timestamp" |> U.member x |> U.to_int in
    { timestamp = (ts "seconds", ts "microseconds")
    ; event
    ; data = event_data event ( json |> U.member "data")
    }
  in

  let execute json =
    let arguments = U.member "arguments" in
    let flatten_option = function Some x -> x | None -> None in
    let eject json =
      let device = json |> arguments |> U.member "device" |> U.to_string in
      let force  = json |> arguments |> U.member "force"  |> U.to_bool_option in
      (device, force)
    in
    let change json =
      let device = json |> arguments |> U.member "device" |> U.to_string in
      let target = json |> arguments |> U.member "target" |> U.to_string in
      let arg    = json |> arguments |> U.member "arg"    |> U.to_string_option in
      (device, target, arg)
    in
    let xen_save_devices_state json =
      json |> arguments |> U.member "filename" |> U.to_string
    in
    let xen_load_devices_state json =
      json |> arguments |> U.member "filename" |> U.to_string
    in
    let xen_set_global_dirty_log json =
      json |> arguments |> U.member "enable" |> U.to_bool
    in
    let add_fd json =
      json |> arguments |> U.to_option (fun x -> x |> U.member "fdset-id" |> U.to_int_option) |> flatten_option
    in
    let remove_fd json =
      json |> arguments |> U.member "fdset-id" |> U.to_int
    in
    let blockdev_change_medium json =
      let device   = json |> arguments |> U.member "device"   |> U.to_string in
      let filename = json |> arguments |> U.member "filename" |> U.to_string in
      (device, filename)
    in
    let device_add json =
      let driver = json |> arguments |> U.member "driver" |> U.to_string in
      let id     = json |> arguments |> U.member "id"     |> U.to_string in
      let maybe_mem_of k x = x |> U.member k |> U.to_option U.to_string in
      let params = json |> arguments |> fun x ->
      match maybe_mem_of "bus" x, maybe_mem_of "hostbus" x, maybe_mem_of "hostport" x with
        | Some bus, Some hostbus, Some hostport -> Some (bus, hostbus, hostport)
        | None, None, None -> None
        | _ -> failwith (Printf.sprintf "All of bus, hostbus, hostport fields are needed but only some passed in %s" (Y.to_string json))
      in
      (driver, id, params)
    in
    let device_del json =
      json |> arguments |> U.member "id" |> U.to_string
    in
    let qom_list json =
      json |> arguments |> U.member "path" |> U.to_string
    in
    let cmd = match json |> U.member "execute" |> U.to_string with
    | "qmp_capabilities"         -> Qmp_capabilities
    | "stop"                     -> Stop
    | "cont"                     -> Cont
    | "system_powerdown"         -> System_powerdown
    | "query-commands"           -> Query_commands
    | "query-status"             -> Query_status
    | "query-vnc"                -> Query_vnc
    | "query-kvm"                -> Query_kvm
    | "query-xen-platform-pv-driver-info" -> Query_xen_platform_pv_driver_info
    | "eject"                    -> json |> eject  |> fun (x, y) -> Eject (x, y)
    | "change"                   -> json |> change |> fun (x, y, z) -> Change (x, y, z)
    | "add-fd"                   -> json |> add_fd |> fun x -> Add_fd x
    | "remove-fd"                -> json |> remove_fd                |> fun x -> Remove_fd x
    | "xen-save-devices-state"   -> json |> xen_save_devices_state   |> fun x -> Xen_save_devices_state x
    | "xen-load-devices-state"   -> json |> xen_load_devices_state   |> fun x -> Xen_load_devices_state x
    | "xen-set-global-dirty-log" -> json |> xen_set_global_dirty_log |> fun x -> Xen_set_global_dirty_log x
    | "blockdev-change-medium"   -> json |> blockdev_change_medium   |> fun (x, y) -> Blockdev_change_medium (x, y)
    | "device_add"               -> json |> device_add               |> fun (x, y, z) -> Device_add (x, y, z)
    | "device_del"               -> json |> device_del               |> fun x -> Device_del x
    | "qom-list"                 -> json |> qom_list                 |> fun x -> Qom_list x
    | x -> Printf.sprintf "unknown command %s" x |> failwith
    in
    (json |> id, cmd)
  in

  let return json =
    let status json =
      json |> U.member "status" |> U.to_string
    in
    let query_vnc json =
      let enabled = json |> U.member "enabled" |> U.to_bool in
      let auth    = json |> U.member "auth"    |> U.to_string in
      let family  = json |> U.member "family"  |> U.to_string in
      let service = json |> U.member "service" |> U.to_string in
      let host    = json |> U.member "host"    |> U.to_string in
      { enabled; auth; family; service = service |> int_of_string; host}
    in
    let xen_platform_pv_driver_info json =
      let product_num = json |> U.member "product-num" |> U.to_int in
      let build_num   = json |> U.member "build-num"   |> U.to_int in
      {product_num; build_num}
    in
    let query_kvm json =
      let enabled = json |> U.member "enabled" |> U.to_bool in
      let present = json |> U.member "present" |> U.to_bool in
      {enabled; present}
    in
    let name_list json =
      json |> U.convert_each (fun x -> x |> U.member "name" |> U.to_string)
    in
    let add_fd json =
      let fd       = json |> U.member "fd"       |> U.to_int in
      let fdset_id = json |> U.member "fdset-id" |> U.to_int in
      {fd; fdset_id}
    in
    let qom json =
      let mem_of k x = x |> U.member k |> U.to_string in
      json |> U.convert_each (fun x -> {name = (mem_of "name" x); ty = (mem_of "type" x)})
    in
    let result =
      let return = json |> U.member "return" in
      return |> function
      | `List  _ -> (return |> U.convert_each U.keys |> List.flatten |> function
        | x when x |> subset_of ["name"; "type"]  -> return |> qom |> fun x -> Qom x
        | x when x |> subset_of ["name"]  -> return |> name_list |> fun x -> Name_list x
        | _ -> failwith (Printf.sprintf "unknown result %s" (Y.to_string return))
      )
      | `Assoc _ -> (return |> U.keys |> function
        | []         -> Unit
        | x when x |> subset_of ["status"]                   -> return |> status    |> fun x -> Status x
        | x when x |> subset_of ["enabled"; "auth"; "family"; "service"; "host"] -> return |> query_vnc |> fun x -> Vnc x
        | x when x |> subset_of ["product-num"; "build-num"] -> return |> xen_platform_pv_driver_info   |> fun x -> Xen_platform_pv_driver_info x
        | x when x |> subset_of ["enabled"; "present"]       -> return |> query_kvm |> fun x -> Enabled x
        | x when x |> subset_of ["fd"; "fdset-id"]           -> return |> add_fd    |> fun x -> Fd_info x
        | _ -> failwith (Printf.sprintf "unknown result %s" (Y.to_string return))
        )
      | _ -> failwith (Printf.sprintf "unknown result type %s" (Y.to_string json))
    in
    (json |> id, result)
  in

  let error json =
    let cls   = json |> U.member "error" |> U.member "class" |> U.to_string in
    let descr = json |> U.member "error" |> U.member "desc"  |> U.to_string in
    ( json |> id, {cls; descr})
  in

  let parse_err descr = Error (None, { cls = _JSONParsing; descr }) in

  try
    let json = Y.from_string str in
    match json |> U.keys with
    | x when x |> subset_of ["QMP"]     -> json |> greeting |> fun x -> Greeting x
    | x when x |> subset_of ["event"]   -> json |> event    |> fun x -> Event x
    | x when x |> subset_of ["execute"] -> json |> execute  |> fun x -> Command x
    | x when x |> subset_of ["return"]  -> json |> return   |> fun x -> Success x
    | x when x |> subset_of ["error"]   -> json |> error    |> fun x -> Error x
    | _ -> Printf.sprintf "can't decode %s" str |> fun x -> failwith x
  with
  | U.Type_error (msg, js) ->
    parse_err (Printf.sprintf "%s %s in %s" msg (Y.to_string js) str)
  | e ->
    parse_err (Printf.sprintf "%s in %s" (Printexc.to_string e)  str)

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
      | Query_vnc -> "query-vnc", []
      | Query_kvm -> "query-kvm", []
      | Query_xen_platform_pv_driver_info -> "query-xen-platform-pv-driver-info", []
      | Eject (device, None) -> "eject", [ "device", `String device ]
      | Eject (device, Some force) -> "eject", [ "device", `String device; "force", `Bool force ]
      | Change (device, target, None) -> "change", [ "device", `String device; "target", `String target ]
      | Change (device, target, Some arg) -> "change", [ "device", `String device; "target", `String target; "arg", `String arg ]
      | Xen_save_devices_state filename -> "xen-save-devices-state", [ "filename", `String filename]
      | Xen_load_devices_state filename -> "xen-load-devices-state", [ "filename", `String filename]
      | Xen_set_global_dirty_log enable -> "xen-set-global-dirty-log", [ "enable", `Bool enable ]
      | Add_fd id -> "add-fd", (match id with None -> [] | Some x -> [ "fdset-id", `Int x ])
      | Remove_fd id -> "remove-fd", ["fdset-id", `Int id]
      | Blockdev_change_medium (device, filename) -> "blockdev-change-medium", ["device", `String device; "filename", `String filename ]
      | Device_add (driver, id, params) -> "device_add", (
        match params with
        | None -> [ "driver", `String driver; "id", `String id ]
        | Some (x, y, z) -> [ "driver", `String driver; "id", `String id; "bus", `String x; "hostbus", `String y; "hostport", `String z ]
      )
      | Device_del id -> "device_del", [ "id", `String id ]
      | Qom_list path -> "qom-list", ["path", `String path ]
    in
    let args = match args with [] -> [] | args -> [ "arguments", `Assoc args ] in
    `Assoc (("execute", `String cmd) :: id @ args)
  | Event {timestamp; event; data} ->
    let secs, usecs = timestamp in
    let event_data = match data with
      | None -> []
      | Some x -> begin
          let data = match x with
            | RTC_CHANGE r -> `Assoc [ "offset", `Int r ]
          in
          [("data", data)]
        end
    in
    `Assoc (
      ("event", `String event)
      :: ("timestamp", `Assoc [ "seconds", `Int secs; "microseconds", `Int usecs])
      :: event_data
    )
  | Success(id, result) ->
    let id = match id with None -> [] | Some x -> [ "id", `String x ] in
    let result = match result with
      | Unit -> `Assoc []
      | Status s -> `Assoc [ "status", `String s ]
      | Enabled {enabled; present} -> `Assoc [ "enabled", `Bool enabled; "present", `Bool present ]
      | Name_list xs -> `List (List.map (fun x -> `Assoc [ "name", `String x ]) xs)
      | Vnc {enabled; auth; family; service; host} -> `Assoc [ "enabled", `Bool enabled; "auth", `String auth; "family", `String family; "service", `String (string_of_int service); "host", `String host ]
      | Xen_platform_pv_driver_info { product_num; build_num } -> `Assoc [ "product-num", `Int product_num; "build-num", `Int build_num; ]
      | Fd_info {fd; fdset_id} -> `Assoc [ "fd", `Int fd; "fdset-id", `Int fdset_id ]
      | Qom xs -> `List (List.map (fun {name; ty} -> `Assoc [ "name", `String name; "type",`String ty ]) xs)
     in
    `Assoc (("return", result) :: id)
  | Error(id, e) ->
    let id = match id with None -> [] | Some x -> [ "id", `String x ] in
    let e = `Assoc [ "class", `String e.cls; "desc", `String e.descr; "data", `Assoc [] ] in
    `Assoc (("error", e) :: id)

let string_of_message m = Yojson.Safe.to_string (json_of_message m)

