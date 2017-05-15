(******************************************************************************
 * capnp-ocaml
 *
 * Copyright (c) 2013-2014, Paul Pelzl
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions are met:
 *
 *  1. Redistributions of source code must retain the above copyright notice,
 *     this list of conditions and the following disclaimer.
 *
 *  2. Redistributions in binary form must reproduce the above copyright
 *     notice, this list of conditions and the following disclaimer in the
 *     documentation and/or other materials provided with the distribution.
 *
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
 * AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
 * IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
 * ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE
 * LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
 * CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
 * SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
 * INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN
 * CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
 * ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
 * POSSIBILITY OF SUCH DAMAGE.
 ******************************************************************************)

module MessageSig   = CapnpRuntime.MessageSig
module Message      = CapnpRuntime.Message
module Array        = CapnpRuntime.CArray
module BytesStorage = CapnpRuntime.BytesStorage
module BytesMessage = CapnpRuntime.Message.BytesMessage
module Codecs       = CapnpRuntime.Codecs
module IO           = CapnpRuntime.IO
module Runtime      = CapnpRuntime

module RPC = struct
  module type S = sig
    type pointer_r
    type pointer_w

    module Client : sig
      (** A client proxy object, which can be used to send messages to a remote object. *)
      type 'a t

      (** A method on some instance, as seen by the client application code.
          This is typically an [('a t, interface_id, method_id)] tuple. *)
      type ('a, 'b) method_t

      module Request : sig
        type 'a t
      end
      module Response : sig
        type 'a t
        val content : 'a t -> pointer_r
      end

      (* These are used by the generated code to make type-safe equivalents. *)
      val bind_method : _ t -> interface_id:Uint64.t -> method_id:int -> ('a, 'b) method_t
    end

    module Server : sig
      type 'a t

      (** The type of a method provided by the server application code. *)
      type ('a, 'b) method_t

      module Request : sig
        type 'a t
        val content : 'a t -> pointer_r
      end
      module Response : sig
        type 'a t
      end

      (* These are used by the generated code to make type-safe equivalents. *)
      type generic_method_t
      val generic : ('a, 'b) method_t -> generic_method_t
      val server : (interface_id:Uint64.t -> method_id:int -> generic_method_t) -> 'a t
    end
  end

  module None(M : CapnpRuntime.MessageSig.S) = struct
    (** A dummy RPC provider, for when the RPC features (interfaces) aren't needed. *)

    type pointer_r = Message.ro M.Slice.t option
    type pointer_w = Message.rw M.Slice.t

    module Client = struct
      type 'a t = [`No_RPC_provider]
      type ('a, 'b) method_t = Uint64.t * int
      module Request = struct
        type 'a t
        let content _ = assert false
      end
      module Response = Request

      let bind_method `No_RPC_provider ~interface_id ~method_id = (interface_id, method_id)
      let content_of_response `No_RPC_provider = assert false
    end

    module Server = struct
      type generic_method_t = [`No_RPC_provider]
      type 'a t = interface_id:Uint64.t -> method_id:int -> generic_method_t
      type ('a, 'b) method_t = [`No_RPC_provider]

      module Request = Client.Request
      module Response = Client.Request

      let generic `No_RPC_provider = `No_RPC_provider
      let server x = x
      let content_of_request `No_RPC_provider = assert false
    end
  end
end
