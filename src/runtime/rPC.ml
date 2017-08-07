module Registry : sig
  (** Handy central registry of all known interfaces, for logging. *)

  (** Used in the generated code to register the interfaces. *)
  val register : interface_id:Uint64.t -> name:string -> (int -> string option) -> unit

  (** [pp_method] is a formatter for [(interface_id, method_id)] pairs.
      It prints out qualified names, suitable for logging
      (e.g. "Foo.bar") *)
  val pp_method : Format.formatter -> Uint64.t * int -> unit
end = struct
  type interface = {
    name : string;
    method_lookup : int -> string option;
  }

  let interfaces = Hashtbl.create 7

  let register ~interface_id ~name method_lookup =
    Hashtbl.add interfaces interface_id {name; method_lookup}

  let pp_method f (interface_id, method_id) =
    match Hashtbl.find interfaces interface_id with
    | exception Not_found ->
      Format.fprintf f "<interface %a>.<method-%d>"
        Uint64.printer interface_id
        method_id
    | interface ->
      match interface.method_lookup method_id with
      | Some method_name ->
        Format.fprintf f "%s.%s" interface.name method_name
      | None ->
        Format.fprintf f "%s.<method-%d>" interface.name method_id
end

module MethodID : sig
  (** A globally unique method ID, for a method on the interface ['interface],
      which takes parameters of type ['request] and produces results of type
      ['response]. *)
  type ('interface, 'request, 'response) t

  val v : interface_id:Uint64.t -> method_id:int -> ('interface, 'req, 'resp) t

  val interface_id : (_, _, _) t -> Uint64.t

  val method_id : (_, _, _) t -> int

  val pp : Format.formatter -> (_, _, _) t -> unit
end = struct
  type ('interface, 'request, 'response) t = Uint64.t * int

  let v ~interface_id ~method_id = (interface_id, method_id)

  let interface_id : (_, _, _) t -> Uint64.t = fst
  let method_id : (_, _, _) t -> int = snd

  let pp t = Registry.pp_method t
end
