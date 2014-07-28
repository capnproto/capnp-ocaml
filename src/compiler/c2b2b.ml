type ro = Capnp.Message.ro
type rw = Capnp.Message.rw

module type S = sig
  type 'cap message_t


  module Reader : sig
    type array_t
    type builder_array_t
    type pointer_t
  end

  module Builder : sig
    type array_t = Reader.builder_array_t
    type reader_array_t = Reader.array_t
    type pointer_t
  end
end

module DefaultsMessage_ = Capnp.BytesMessage
module DefaultsCommon_  = Capnp.Runtime.Common.Make(DefaultsMessage_)

let _builder_defaults_message =
  let message_segments = [
    Bytes.unsafe_of_string "\
    \x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\
    \x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\
    \x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\
    \x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00";
  ] in
  DefaultsMessage_.Message.readonly
    (DefaultsMessage_.Message.of_storage message_segments)

module Make (MessageWrapper : Capnp.MessageSig.S) = struct
  let invalid_msg = Capnp.Message.invalid_msg

  module RA_ = struct
    open Capnp.Runtime
    INCLUDE "reader-inc.ml"
  end
  module BA_ = struct
    open Capnp.Runtime
    module NM = MessageWrapper
    INCLUDE "builder-inc.ml"
  end

  type 'cap message_t = 'cap MessageWrapper.Message.t


  module DefaultsCopier_ =
    Capnp.Runtime.BuilderOps.Make(Capnp.BytesMessage)(MessageWrapper)

  let _reader_defaults_message =
    MessageWrapper.Message.create
      (DefaultsMessage_.Message.total_size _builder_defaults_message)


  module Reader = struct
    type array_t = ro MessageWrapper.ListStorage.t
    type builder_array_t = rw MessageWrapper.ListStorage.t
    type pointer_t = ro MessageWrapper.Slice.t option

  end

  module Builder = struct
    type array_t = Reader.builder_array_t
    type reader_array_t = Reader.array_t
    type pointer_t = rw MessageWrapper.Slice.t

  end
end

