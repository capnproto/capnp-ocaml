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
    (** A client proxy object, which can be used to send messages to a remote object. *)
    type client

    (** The type of a method, as seen by the client application code.
        Generally, an RPC system will define this to take a function for building the parameters,
        and to return a reader of the results. Typically it will also need extending with a way
        to attach and get capabilities, and the result will be some kind of promise. *)
    type ('a, 'b) proxy_method_t

    (** The type of a method provided by the server application code. *)
    type ('a, 'b) method_impl_t

    type generic_method_t

    val call : client -> interface_id:Uint64.t -> method_id:int -> ('a, 'b) proxy_method_t

    val generic : ('a, 'b) method_impl_t -> generic_method_t
  end

  module None : S = struct
    (** A dummy RPC provider, for when the RPC features (interfaces) aren't needed. *)

    type client = [`No_RPC_provider]
    type ('a, 'b) proxy_method_t = [`No_RPC_provider]
    type ('a, 'b) method_impl_t = [`No_RPC_provider]
    type generic_method_t = [`No_RPC_provider]
    let call `No_RPC_provider ~interface_id:_ ~method_id:_ = `No_RPC_provider
    let generic `No_RPC_provider = `No_RPC_provider
  end
end
