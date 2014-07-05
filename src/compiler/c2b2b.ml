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

module DefaultsMessage_ = Capnp.Runtime.Builder.DefaultsMessage
module DefaultsCommon_  = Capnp.Runtime.Builder.DC

let _builder_defaults_message =
  let message_segments = [
    "\
    \x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\
    \x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\
    \x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\
    \x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00";
  ] in
  DefaultsMessage_.Message.readonly
    (DefaultsMessage_.Message.of_storage message_segments)

module MakeUnsafe (MessageWrapper : Capnp.MessageSig.S) = struct
  let invalid_msg = Capnp.Message.invalid_msg

  module RA_ = Capnp.Runtime.Reader.Make(MessageWrapper)
  module BA_ = Capnp.Runtime.Builder.Make(MessageWrapper)

  type 'cap message_t = 'cap MessageWrapper.Message.t


  module DefaultsCopier_ =
    Capnp.Runtime.BuilderOps.Make(Capnp.Runtime.Builder.DefaultsMessage)(MessageWrapper)

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

module Make (MessageWrapper : Capnp.MessageSig.S) = MakeUnsafe(MessageWrapper)

