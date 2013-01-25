(*
 * Copyright (C) 2012 Citrix Systems Inc.
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

type 'a result =
  | Success of 'a
  | Error of string

module Request = struct
  type t =
    | Qmp_capabilities
    | Query_commands
end

module Response = struct
  type t =
    | Qmp_capabilities
    | Query_commands of string list
end

type event = {
  secs: int;
  usecs: int;
  event: string;
}
