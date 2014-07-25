
module type TEST_CASE = sig
  type request_reader_t
  type request_builder_t
  type response_reader_t
  type response_builder_t
  type expectation_t

  val setup_request : unit -> request_builder_t * expectation_t
  val handle_request : request_reader_t -> response_builder_t
  val check_response : response_reader_t -> expectation_t -> bool
end

module type READER = sig
  type t
  type builder_t
  val of_message : 'cap Capnp.BytesMessage.Message.t -> t
  val of_builder : builder_t -> t
end

module type BUILDER = sig
  type t
  val to_message : t -> Capnp.Message.rw Capnp.BytesMessage.Message.t
end



