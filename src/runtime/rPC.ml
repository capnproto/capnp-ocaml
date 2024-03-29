module Uint32 = Stdint.Uint32
module Uint64 = Stdint.Uint64

module Registry : sig
  (** Handy central registry of all known interfaces, for logging. *)

  (** Used in the generated code to register the interfaces. *)
  val register : interface_id:Uint64.t -> name:string -> (int -> string option) -> unit

  (** [pp_method] is a formatter for [(interface_id, method_id)] pairs.
      It prints out qualified names, suitable for logging
      (e.g. "Foo.bar") *)
  val pp_method : Format.formatter -> Uint64.t * int -> unit

  val pp_interface : Format.formatter -> Uint64.t -> unit
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

  let pp_interface f interface_id =
    match Hashtbl.find interfaces interface_id with
    | exception Not_found -> Format.fprintf f "<interface %a>" Uint64.printer interface_id
    | interface           -> Format.fprintf f "%s" interface.name
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

module type S = sig
  (** Extends [MessageSig.S] with types for RPC. *) 

  include MessageSig.S

  module Service : sig
    (** The type of a method provided by the server application code.
        This is used in the generated code for the service class type. *)
    type ('a, 'b) method_t
  end

  module StructRef : sig
    (** A reference to a struct, which may not have arrived yet. *)
    type 'a t
  end

  module Capability : sig
    (** A reference to an interface, which may be remote. *)
    type 'a t
  end

  module Untyped : sig
    (** This module is only for use by the code generated by the capnp-ocaml
        schema compiler. The generated code provides type-safe wrappers for
        everything here. *)

    (** An untyped method. This will typically be something like
        ['a reader_t -> 'b StructRef.t]. i.e. the result of calling an
        interface's method is a promise for the future result. *)
    type abstract_method_t

    (** Cast a method to [abstract_method_t]. Typically this will be the identity function.
        This is used in the generated code to ensure that all methods have the
        same type for the dispatch function. *)
    val abstract_method : ('a StructStorage.reader_t, 'b) Service.method_t -> abstract_method_t

    (** [struct_field t i] is a reference to the struct found at pointer index [i]
        within the struct [t]. Used to implement the "_pipelined" accessors. *)
    val struct_field : 'a StructRef.t -> int -> 'b StructRef.t

    (** [capability_field t i] is a reference to the capability found at pointer index [i]
        within the struct [t]. Used to implement the "_pipelined" accessors. *)
    val capability_field : 'a StructRef.t -> int -> 'b Capability.t

    class type generic_service = object
      method dispatch : interface_id:Uint64.t -> method_id:int -> abstract_method_t
      (** Look up a method by ID. The schema compiler generates an implementation of this
          that dispatches to the typed methods of the interface. *)

      method release : unit
      (** Called when the service's ref-count drops to zero.
          Implementations that hold other capabilities should override this to release them in turn. *)

      method pp : Format.formatter -> unit
      (** Used to identify the service in log messages.
          The schema compiler generates a default that displays the service's name. *)
    end

    (** [local service] is a capability reference to a local service implemented by [service#dispatch].
        Used by the generated functions with the same name (but a fixed type). *)
    val local : #generic_service -> 'a Capability.t

    (** Used in the generated code to get a capability from the attachments by index. *)
    val get_cap : MessageSig.attachments -> Uint32.t -> 'a Capability.t

    (** Used in the generated code to store a capability in the attachments. Returns the new index. *)
    val add_cap : MessageSig.attachments -> 'a Capability.t -> Uint32.t

    (** Remove a capability from the attachments. Used if the interface is changed. *)
    val clear_cap : MessageSig.attachments -> Uint32.t -> unit

    (** Used to handle calls when the interface ID isn't known. *)
    val unknown_interface : interface_id:Uint64.t -> abstract_method_t

    (** Used to handle calls when the method ID isn't known. *)
    val unknown_method : interface_id:Uint64.t -> method_id:int -> abstract_method_t
  end
end

module None (M : MessageSig.S) = struct
  (** A dummy RPC provider, for when the RPC features (interfaces) aren't needed. *)

  include M

  module Untyped = struct
    type untyped_struct = [`No_RPC_struct]

    type abstract_method_t = [`No_RPC_payload] -> untyped_struct

    let define_method ~interface_id ~method_id = (interface_id, method_id)

    let abstract_method x = x

    let struct_field `No_RPC_struct _ = `No_RPC_struct
    let capability_field `No_RPC_struct _ = failwith "Can't pipeline with RPC.None!"
    let local _ = failwith "Can't use local with RPC.None!"
    let get_cap _ i = i
    let add_cap _ i = i
    let clear_cap _ _ = ()
    let unknown_interface ~interface_id:_ _req = failwith "Unknown interface"
    let unknown_method ~interface_id:_ ~method_id:_ _req = failwith "Unknown method"

    class type generic_service = object
      method dispatch : interface_id:Uint64.t -> method_id:int -> abstract_method_t
      method release : unit
      method pp : Format.formatter -> unit
    end
  end

  module StructRef = struct
    type 'a t = Untyped.untyped_struct
  end

  module Capability = struct
    type 'a t = Uint32.t           (* Just the raw CapDescriptor table index. *)
  end

  module Service = struct
    type ('a, 'b) method_t = Untyped.abstract_method_t
  end
end
