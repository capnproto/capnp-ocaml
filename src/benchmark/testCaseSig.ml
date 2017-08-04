open Capnp.BytesMessage.StructStorage

module type TEST_CASE = sig
  type request_t
  type response_t
  type expectation_t

  val setup_request : unit -> request_t builder_t * expectation_t
  val handle_request : request_t reader_t -> response_t builder_t
  val check_response : response_t reader_t -> expectation_t -> bool
end

module type READER = sig
  type struct_t
  val of_message : 'cap Capnp.BytesMessage.Message.t -> struct_t reader_t
end
