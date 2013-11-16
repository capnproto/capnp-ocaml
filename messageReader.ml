open Core.Std

module Make (Storage : MessageStorage.S) = struct
  module M = Message.Make(Storage)
  let invalid_msg = Message.invalid_msg
  open M


  let sizeof_uint32 = 4
  let sizeof_uint64 = 8


  let uint64_le_of_slice (slice : 'cap Slice.t) : Uint64.t =
    let open Slice in
    let () = assert (slice.len = sizeof_uint64) in
    let rec decode acc i =
      if i = slice.len then
        acc
      else
        let byte = Uint64.of_int (Slice.get slice i) in
        let shifted_byte = Uint64.shift_left byte (8 * i) in
        decode (Uint64.logor acc shifted_byte) (i + 1)
    in
    decode Uint64.zero 0


  let uint32_le_of_slice (slice : 'cap Slice.t) : Uint32.t =
    let open Slice in
    let () = assert (slice.len = sizeof_uint32) in
    let rec decode acc i =
      if i = slice.len then
        acc
      else
        let byte = Uint32.of_int (Slice.get slice i) in
        let shifted_byte = Uint32.shift_left byte (8 * i) in
        decode (Uint32.logor acc shifted_byte) (i + 1)
    in
    decode Uint32.zero 0


  let bounds_check_slice_exn ?err (slice : 'cap Slice.t) : unit =
    let open Slice in
    if slice.segment_id < 0 ||
      slice.segment_id >= Message.num_segments slice.msg ||
      slice.start < 0 ||
      slice.start + slice.len > Segment.length (Slice.get_segment slice)
    then
      let error_msg =
        match err with
        | None -> "pointer referenced a memory region outside the message"
        | Some msg -> msg
      in
      invalid_msg error_msg
    else
      ()


  module StructStorage = struct
    type 'cap t = {
      data     : 'cap Slice.t;
      pointers : 'cap Slice.t;
    }
  end


  module ListStorage = struct
    type storage_type_t =
      (** list(void), no storage required *)
      | Empty

      (** list(bool), tightly packed bits *)
      | Bits

      (** either primitive values or a data-only struct; argument is the byte count *)
      | Bytes of int

      (** either a pointer to an external object, or a pointer-only struct *)
      | Pointer

      (** typical struct; parameters are per-element word size for data section and pointers
       *  section, respectively *)
      | Composite of int * int

    type 'cap t = {
      storage      : 'cap Slice.t;
      storage_type : storage_type_t;
      num_elements : int
    }
  end


  let decode_pointer (pointer_bytes : 'cap Slice.t) : Pointer.t =
    let pointer64 = uint64_le_of_slice pointer_bytes in
    if Uint64.compare pointer64 Uint64.zero = 0 then
      Pointer.Null
    else
      let module B = Pointer.Bitfield in
      let tag = Uint64.logand pointer64 B.tag_mask in
      if Uint64.compare tag B.tag_val_list = 0 then
        Pointer.List (ListPointer.decode pointer64)
      else if Uint64.compare tag B.tag_val_struct = 0 then
        Pointer.Struct (StructPointer.decode pointer64)
      else if Uint64.compare tag B.tag_val_far = 0 then
        Pointer.Far (FarPointer.decode pointer64)
      else
        invalid_msg "pointer has undefined type tag"


  let deref_far_pointer
      (far_pointer : FarPointer.t)
      (message : 'cap Message.t)
      (deref_normal_pointer     : 'cap Slice.t -> 'a option)
      (deref_tagged_far_pointer : 'cap Slice.t -> 'a)
    : 'a option =
    let open FarPointer in
    begin match far_pointer.landing_pad with
      | NormalPointer ->
        let next_pointer_bytes = {
          Slice.msg        = message;
          Slice.segment_id = far_pointer.segment_id;
          Slice.start      = far_pointer.offset * sizeof_uint64;
          Slice.len        = sizeof_uint64;
        } in
        let () = bounds_check_slice_exn
          ~err:"far pointer describes invalid landing pad" next_pointer_bytes
        in
        deref_normal_pointer next_pointer_bytes
      | TaggedFarPointer ->
        let landing_pad_bytes = {
          Slice.msg        = message;
          Slice.segment_id = far_pointer.segment_id;
          Slice.start      = far_pointer.offset * sizeof_uint64;
          Slice.len        = 2 * sizeof_uint64;
        } in
        let () = bounds_check_slice_exn
          ~err:"far pointer describes invalid tagged landing pad" landing_pad_bytes
        in
        Some (deref_tagged_far_pointer landing_pad_bytes)
    end


  let make_list_storage ~message ~segment_id ~segment_offset ~list_pointer =
    let make_list_storage_aux ~offset ~num_words ~num_elements ~storage_type =
      let storage = {
        Slice.msg        = message;
        Slice.segment_id = segment_id;
        Slice.start      = segment_offset + offset;
        Slice.len        = num_words * sizeof_uint64;
      } in 
      let () = bounds_check_slice_exn
        ~err:"list pointer describes invalid storage region" storage
      in {
        ListStorage.storage      = storage;
        ListStorage.storage_type = storage_type;
        ListStorage.num_elements = num_elements;
      }
    in
    let open ListPointer in
    begin match list_pointer.element_type with
      | Void ->
        make_list_storage_aux ~offset:0 ~num_words:0 ~num_elements:list_pointer.num_elements 
          ~storage_type:ListStorage.Empty
      | OneBitValue ->
        make_list_storage_aux ~offset:0 ~num_words:(Util.ceil_int list_pointer.num_elements 64)
          ~num_elements:list_pointer.num_elements ~storage_type:ListStorage.Bits
      | OneByteValue ->
        make_list_storage_aux ~offset:0 ~num_words:(Util.ceil_int list_pointer.num_elements 8)
          ~num_elements:list_pointer.num_elements ~storage_type:(ListStorage.Bytes 1)
      | TwoByteValue ->
        make_list_storage_aux ~offset:0 ~num_words:(Util.ceil_int list_pointer.num_elements 4)
          ~num_elements:list_pointer.num_elements ~storage_type:(ListStorage.Bytes 2)
      | FourByteValue ->
        make_list_storage_aux ~offset:0 ~num_words:(Util.ceil_int list_pointer.num_elements 2)
          ~num_elements:list_pointer.num_elements ~storage_type:(ListStorage.Bytes 4)
      | EightByteValue ->
        make_list_storage_aux ~offset:0 ~num_words:list_pointer.num_elements
          ~num_elements:list_pointer.num_elements ~storage_type:(ListStorage.Bytes 8)
      | EightBytePointer ->
        make_list_storage_aux ~offset:0 ~num_words:list_pointer.num_elements
          ~num_elements:list_pointer.num_elements ~storage_type:ListStorage.Pointer
      | Composite ->
        let struct_tag_bytes = {
          Slice.msg        = message;
          Slice.segment_id = segment_id;
          Slice.start      = segment_offset;
          Slice.len        = sizeof_uint64;
        } in
        let () = bounds_check_slice_exn
          ~err:"composite list pointer describes invalid storage region" struct_tag_bytes
        in
        begin match decode_pointer struct_tag_bytes with
          | Pointer.Struct struct_pointer ->
            let module SP = StructPointer in
            let num_words = list_pointer.num_elements in
            let num_elements = struct_pointer.SP.offset in
            let words_per_element =
              struct_pointer.SP.data_size + struct_pointer.SP.pointers_size
            in
            if num_elements * words_per_element > num_words then
              invalid_msg "composite list pointer describes invalid word count";
            make_list_storage_aux ~offset:sizeof_uint64 ~num_words:list_pointer.num_elements
              ~num_elements:struct_pointer.SP.offset
              ~storage_type:(ListStorage.Composite
                               (struct_pointer.SP.data_size, struct_pointer.SP.pointers_size))
          | _ ->
            invalid_msg "composite list pointer has malformed element type tag"
        end
    end


  let deref_list_tagged_far_pointer (landing_pad_bytes : 'cap Slice.t) : 'cap ListStorage.t =
    let far_pointer_bytes = { landing_pad_bytes with Slice.len = sizeof_uint64 } in
    let list_tag_bytes = {
      landing_pad_bytes with
      Slice.start = Slice.get_end far_pointer_bytes;
      Slice.len   = sizeof_uint64;
    } in
    match (decode_pointer far_pointer_bytes, decode_pointer list_tag_bytes) with
    | (Pointer.Far far_pointer, Pointer.List list_pointer) ->
      make_list_storage
        ~message:landing_pad_bytes.Slice.msg
        ~segment_id:far_pointer.FarPointer.segment_id
        ~segment_offset:(far_pointer.FarPointer.offset * sizeof_uint64)
        ~list_pointer
    | _ ->
      invalid_msg "invalid type tags for list-tagged far pointer"


  let rec deref_list_pointer (pointer_bytes : 'cap Slice.t) : 'cap ListStorage.t option =
    match decode_pointer pointer_bytes with
    | Pointer.Null ->
      None
    | Pointer.List list_pointer ->
      Some (make_list_storage
        ~message:pointer_bytes.Slice.msg
        ~segment_id:pointer_bytes.Slice.segment_id
        ~segment_offset:(pointer_bytes.Slice.start + pointer_bytes.Slice.len +
                           (list_pointer.ListPointer.offset * sizeof_uint64))
        ~list_pointer)
    | Pointer.Far far_pointer ->
      deref_far_pointer far_pointer pointer_bytes.Slice.msg
        deref_list_pointer deref_list_tagged_far_pointer
    | Pointer.Struct _ ->
      invalid_msg "decoded struct pointer where list pointer was expected"


  let deref_struct_tagged_far_pointer (landing_pad_bytes : 'cap Slice.t) : 'cap StructStorage.t =
    let far_pointer_bytes = {
      landing_pad_bytes with
      Slice.len = sizeof_uint64
    } in
    let struct_tag_bytes = {
      landing_pad_bytes with
      Slice.start = Slice.get_end far_pointer_bytes;
      Slice.len   = sizeof_uint64;
    } in
    match (decode_pointer far_pointer_bytes, decode_pointer struct_tag_bytes) with
    | (Pointer.Far far_pointer, Pointer.Struct struct_pointer) ->
      let data = {
        Slice.msg        = landing_pad_bytes.Slice.msg;
        Slice.segment_id = far_pointer.FarPointer.segment_id;
        Slice.start      = far_pointer.FarPointer.offset * sizeof_uint64;
        Slice.len        = struct_pointer.StructPointer.data_size * sizeof_uint64;
      } in
      let pointers = {
        data with
        Slice.start = Slice.get_end data;
        Slice.len   = struct_pointer.StructPointer.pointers_size * sizeof_uint64;
      } in
      let () = bounds_check_slice_exn 
        ~err:"struct-tagged far pointer describes invalid data region" data
      in
      let () = bounds_check_slice_exn
        ~err:"struct-tagged far pointer describes invalid pointers region" pointers
      in
      { StructStorage.data; StructStorage.pointers; }

    | _ ->
      invalid_msg "invalid type tags for struct-tagged far pointer"


  let rec deref_struct_pointer (pointer_bytes : 'cap Slice.t) : 'cap StructStorage.t option =
    match decode_pointer pointer_bytes with
    | Pointer.Null ->
      None
    | Pointer.Struct struct_pointer ->
      let open StructPointer in
      let data = {
        pointer_bytes with
        Slice.start = (Slice.get_end pointer_bytes) + (struct_pointer.offset * sizeof_uint64);
        Slice.len   = struct_pointer.data_size * sizeof_uint64;
      } in
      let pointers = {
        data with
        Slice.start = Slice.get_end data;
        Slice.len   = struct_pointer.pointers_size * sizeof_uint64;
      } in
      let () = bounds_check_slice_exn
        ~err:"struct pointer describes invalid data region" data
      in
      let () = bounds_check_slice_exn
        ~err:"struct pointer describes invalid pointers region" data
      in
      Some { StructStorage.data; StructStorage.pointers; }
    | Pointer.Far far_pointer ->
      deref_far_pointer far_pointer pointer_bytes.Slice.msg
        deref_struct_pointer deref_struct_tagged_far_pointer
    | Pointer.List _ ->
      invalid_msg "decoded list pointer where struct pointer was expected"


  let string_of_uint8_list (list_storage : 'cap ListStorage.t) : string =
    let open ListStorage in
    match list_storage.storage_type with
    | Bytes 1 ->
      let buf = String.create list_storage.num_elements in
      for i = 0 to list_storage.num_elements - 1 do
        buf.[i] <- Char.of_int_exn (Slice.get list_storage.storage i)
      done;
      buf
    | _ ->
      invalid_msg "decoded non-UInt8 list where string data was expected"

end

