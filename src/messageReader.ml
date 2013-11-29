open Core.Std

module Make (Storage : MessageStorage.S) = struct
  let invalid_msg = Message.invalid_msg
  include Message.Make(Storage)


  let sizeof_uint32 = 4
  let sizeof_uint64 = 8


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


    let get_data_slice (struct_storage : 'cap t) (start : int) (len : int)
    : 'cap Slice.t option =
      let data = struct_storage.data in
      if start + len <= data.Slice.len then
        Some {
          data with
          Slice.start = data.Slice.start + start;
          Slice.len   = len
        }
      else
        None


    let get_pointer (struct_storage : 'cap t) (word : int) : 'cap Slice.t option =
      let pointers = struct_storage.pointers in
      let start = word * sizeof_uint64 in
      let len   = sizeof_uint64 in
      if start + len <= pointers.Slice.len then
        Some {
          pointers with
          Slice.start = pointers.Slice.start + start;
          Slice.len   = len
        }
      else
        None

  end


  module ListStorage = struct
    type storage_type_t =
      (** list(void), no storage required *)
      | Empty

      (** list(bool), tightly packed bits *)
      | Bit

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


  module CapnpArray = struct
    type ('cap, 'a) list_t = {
      storage  : 'cap ListStorage.t;
      get_item : 'cap ListStorage.t -> int -> 'a;
    }

    type ('cap, 'a) t = ('cap, 'a) list_t option

    let length (x : ('cap, 'a)  t) : int =
      match x with
      | Some {storage; _} ->
          storage.ListStorage.num_elements
      | None ->
          0

    let get (x : ('cap, 'a) t) (i : int) : 'a =
      match x with
      | Some {storage; get_item;} ->
          get_item storage i
      | None ->
          invalid_arg "index out of bounds"
  end


  let decode_pointer (pointer_bytes : 'cap Slice.t) : Pointer.t =
    let pointer64 = Slice.get_int64 pointer_bytes 0 in
    if Int64.compare pointer64 Int64.zero = 0 then
      Pointer.Null
    else
      let module B = Pointer.Bitfield in
      let tag = Int64.bit_and pointer64 B.tag_mask in
      if Int64.compare tag B.tag_val_list = 0 then
        Pointer.List (ListPointer.decode pointer64)
      else if Int64.compare tag B.tag_val_struct = 0 then
        Pointer.Struct (StructPointer.decode pointer64)
      else if Int64.compare tag B.tag_val_far = 0 then
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
          ~num_elements:list_pointer.num_elements ~storage_type:ListStorage.Bit
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
        ~segment_offset:((Slice.get_end pointer_bytes) +
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


  let string_of_uint8_list ~(null_terminated : bool) (list_storage : 'cap ListStorage.t) : string =
    let open ListStorage in
    match list_storage.storage_type with
    | Bytes 1 ->
        let result_byte_count =
          if null_terminated then
            let () =
              if list_storage.num_elements < 1 then
                invalid_msg "empty string list has no space for null terminator"
            in
            let terminator = Slice.get_uint8 list_storage.storage (list_storage.num_elements - 1) in
            let () = if terminator <> 0 then
              invalid_msg "string list is not null terminated"
            in
            list_storage.num_elements - 1
          else
            list_storage.num_elements
        in
        let buf = String.create result_byte_count in
        let () =
          for i = 0 to result_byte_count - 1 do
            buf.[i] <- Char.of_int_exn (Slice.get_uint8 list_storage.storage i)
          done
        in
        buf
    | _ ->
      invalid_msg "decoded non-UInt8 list where string data was expected"


  let list_fold_left_generic
    (list_storage : 'cap ListStorage.t)
    ~(get : 'cap ListStorage.t -> int -> 'a)
    ~(f : 'b -> 'a -> 'b)
    ~(acc : 'b)
  : 'b =
    let rec loop acc' i =
      if i = list_storage.ListStorage.num_elements then
        acc'
      else
        let v = get list_storage i in
        loop (f acc' v) (i + 1)
    in
    loop acc 0


  let list_fold_right_generic
    (list_storage : 'cap ListStorage.t)
    ~(get : 'cap ListStorage.t -> int -> 'a)
    ~(f : 'b -> 'a -> 'b)
    ~(acc : 'b)
  : 'b =
    let rec loop acc' i =
      if i < 0 then
        acc'
      else
        let v = get list_storage i in
        loop (f acc' v) (i - 1)
    in
    loop acc (list_storage.ListStorage.num_elements - 1)



  module BitList = struct
    let get (list_storage : 'cap ListStorage.t) (i : int) : bool =
      match list_storage.ListStorage.storage_type with
      | ListStorage.Bit ->
        let byte_ofs = i / 8 in
        let bit_ofs  = i mod 8 in
        let byte_val = Slice.get_uint8 list_storage.ListStorage.storage byte_ofs in
        (byte_val land (1 lsl bit_ofs)) <> 0
      | _ ->
        invalid_msg "decoded non-bool list where bool list was expected"

    let fold_left (list_storage : 'cap ListStorage.t) ~(f : 'a -> bool -> 'a) ~(acc : 'a) : 'a =
      list_fold_left_generic list_storage ~get ~f ~acc

    let fold_right (list_storage : 'cap ListStorage.t) ~(f : 'a -> bool -> 'a) ~(acc : 'a) : 'a =
      list_fold_right_generic list_storage ~get ~f ~acc
  end


  module BytesList = struct
    let get (list_storage : 'cap ListStorage.t) (i : int) : 'cap Slice.t =
      let byte_count =
        match list_storage.ListStorage.storage_type with
        | ListStorage.Bytes byte_count -> byte_count
        | ListStorage.Pointer          -> sizeof_uint64
        | _ -> invalid_msg "decoded non-bytes list where bytes list was expected"
      in {
        list_storage.ListStorage.storage with
        Slice.start = list_storage.ListStorage.storage.Slice.start + (i * byte_count);
        Slice.len   = byte_count
      }

    let fold_left (list_storage : 'cap ListStorage.t) ~(f : 'a -> 'cap Slice.t -> 'a) ~(acc : 'a) : 'a =
      list_fold_left_generic list_storage ~get ~f ~acc

    let fold_right (list_storage : 'cap ListStorage.t) ~(f : 'a -> 'cap Slice.t -> 'a) ~(acc : 'a) : 'a =
      list_fold_right_generic list_storage ~get ~f ~acc
  end


  module StructList = struct
    let get (list_storage : 'cap ListStorage.t) (i : int) : 'cap StructStorage.t =
      match list_storage.ListStorage.storage_type with
      | ListStorage.Bytes byte_count ->
        let data = {
          list_storage.ListStorage.storage with
          Slice.start = list_storage.ListStorage.storage.Slice.start + (i * byte_count);
          Slice.len   = byte_count;
        } in
        let pointers = {
          list_storage.ListStorage.storage with
          Slice.start = 0;
          Slice.len   = 0;
        }
        in
        { StructStorage.data; StructStorage.pointers; }
      | ListStorage.Pointer ->
        let data = {
          list_storage.ListStorage.storage with
          Slice.start = 0;
          Slice.len   = 0;
        } in
        let pointers = {
          list_storage.ListStorage.storage with
          Slice.start = list_storage.ListStorage.storage.Slice.start + (i * sizeof_uint64);
          Slice.len   = sizeof_uint64;
        }
        in
        { StructStorage.data; StructStorage.pointers; }
      | ListStorage.Composite (data_words, pointers_words) ->
        let data_size     = data_words * sizeof_uint64 in
        let pointers_size = pointers_words * sizeof_uint64 in
        let total_size    = data_size + pointers_size in
        let data = {
          list_storage.ListStorage.storage with
          Slice.start = list_storage.ListStorage.storage.Slice.start + (i * total_size);
          Slice.len   = data_size;
        } in
        let pointers = {
          data with
          Slice.start = Slice.get_end data;
          Slice.len   = pointers_size;
        } in
        { StructStorage.data; StructStorage.pointers; }
      | ListStorage.Empty | ListStorage.Bit ->
        invalid_msg "decoded non-struct list where struct list was expected"


    let fold_left (list_storage : 'cap ListStorage.t)
      ~(f : 'a -> 'cap StructStorage.t -> 'a)
      ~(acc : 'a)
    : 'a =
      list_fold_left_generic list_storage ~get ~f ~acc


    let fold_right (list_storage : 'cap ListStorage.t)
      ~(f : 'a -> 'cap StructStorage.t -> 'a)
      ~(acc : 'a)
    : 'a =
      list_fold_right_generic list_storage ~get ~f ~acc
  end


  let get_struct_pointer
      (struct_storage : 'cap StructStorage.t option)
      (pointer_word : int)
  : 'cap Slice.t option =
    match struct_storage with
    | Some storage ->
        StructStorage.get_pointer storage pointer_word
    | None ->
        None


  let string_of_pointer pointer_bytes =
    let rec loop acc i : string list =
      if i = 8 then
        List.rev acc
      else
        let byte_rep = Printf.sprintf "%02x" (Slice.get_uint8 pointer_bytes i) in
        loop (byte_rep :: acc) (i + 1)
    in
    String.concat ~sep:"" (loop [] 0)


  let get_struct_field_blob
      (struct_storage : 'cap StructStorage.t option)
      (pointer_word : int)
  : string =
    match struct_storage with
    | Some storage ->
        begin match StructStorage.get_pointer storage pointer_word with
        | Some pointer_bytes ->
          begin match deref_list_pointer pointer_bytes with
          | Some list_storage ->
              string_of_uint8_list ~null_terminated:false list_storage
          | None ->
              ""
          end
        | None ->
            ""
        end
    | None ->
        ""


  let get_struct_field_text
      (struct_storage : 'cap StructStorage.t option)
      (pointer_word : int)
  : string =
    match struct_storage with
    | Some storage ->
        begin match StructStorage.get_pointer storage pointer_word with
        | Some pointer_bytes ->
          begin match deref_list_pointer pointer_bytes with
          | Some list_storage ->
              string_of_uint8_list ~null_terminated:true list_storage
          | None ->
              ""
          end
        | None ->
            ""
        end
    | None ->
        ""


  let get_struct_field_bit_list
      (struct_storage : 'cap StructStorage.t option)
      (pointer_word : int)
  : ('cap, 'a) CapnpArray.t =
    match get_struct_pointer struct_storage pointer_word with
    | Some slice ->
        begin match deref_list_pointer slice with
        | Some list_storage ->
            Some {
              CapnpArray.storage  = list_storage;
              CapnpArray.get_item = fun storage i -> Some (BitList.get storage i)
            }
        | None ->
            None
        end
    | None ->
        None


  let get_struct_field_bytes_list
      (struct_storage : 'cap StructStorage.t option)
      (pointer_word : int)
      (convert : 'cap Slice.t -> 'a)
  : ('cap, 'a) CapnpArray.t =
    match get_struct_pointer struct_storage pointer_word with
    | Some slice ->
        begin match deref_list_pointer slice with
        | Some list_storage ->
            Some {
              CapnpArray.storage  = list_storage;
              CapnpArray.get_item = fun storage i -> convert (BytesList.get storage i);
            }
        | None ->
            None
        end
    | None ->
        None


  let get_struct_field_int8_list
    (struct_storage : 'cap StructStorage.t option)
    (pointer_word : int)
  : ('cap, int) CapnpArray.t =
    get_struct_field_bytes_list struct_storage pointer_word (fun slice -> Slice.get_int8 slice 0)


  let get_struct_field_int16_list
    (struct_storage : 'cap StructStorage.t option)
    (pointer_word : int)
  : ('cap, int) CapnpArray.t =
    get_struct_field_bytes_list struct_storage pointer_word (fun slice -> Slice.get_int16 slice 0)


  let get_struct_field_int32_list
    (struct_storage : 'cap StructStorage.t option)
    (pointer_word : int)
  : ('cap, int32) CapnpArray.t =
    get_struct_field_bytes_list struct_storage pointer_word (fun slice -> Slice.get_int32 slice 0)


  let get_struct_field_int64_list
    (struct_storage : 'cap StructStorage.t option)
    (pointer_word : int)
  : ('cap, int64) CapnpArray.t =
    get_struct_field_bytes_list struct_storage pointer_word (fun slice -> Slice.get_int64 slice 0)


  let get_struct_field_uint8_list
    (struct_storage : 'cap StructStorage.t option)
    (pointer_word : int)
  : ('cap, int) CapnpArray.t =
    get_struct_field_bytes_list struct_storage pointer_word (fun slice -> Slice.get_uint8 slice 0)


  let get_struct_field_uint16_list
    (struct_storage : 'cap StructStorage.t option)
    (pointer_word : int)
  : ('cap, int) CapnpArray.t =
    get_struct_field_bytes_list struct_storage pointer_word (fun slice -> Slice.get_uint16 slice 0)


  let get_struct_field_uint32_list
    (struct_storage : 'cap StructStorage.t option)
    (pointer_word : int)
  : ('cap, Uint32.t) CapnpArray.t =
    get_struct_field_bytes_list struct_storage pointer_word (fun slice -> Slice.get_uint32 slice 0)


  let get_struct_field_uint64_list
    (struct_storage : 'cap StructStorage.t option)
    (pointer_word : int)
  : ('cap, Uint64.t) CapnpArray.t =
    get_struct_field_bytes_list struct_storage pointer_word (fun slice -> Slice.get_uint64 slice 0)


  let get_struct_field_text_list
      (struct_storage : 'cap StructStorage.t option)
      (pointer_word : int)
  : ('cap, string) CapnpArray.t =
    get_struct_field_bytes_list struct_storage pointer_word (fun slice ->
      match deref_list_pointer slice with
      | Some list_storage ->
          string_of_uint8_list ~null_terminated:true list_storage
      | None ->
          "")


  let get_struct_field_blob_list
      (struct_storage : 'cap StructStorage.t option)
      (pointer_word : int)
  : ('cap, string) CapnpArray.t =
    get_struct_field_bytes_list struct_storage pointer_word (fun slice ->
      match deref_list_pointer slice with
      | Some list_storage ->
          string_of_uint8_list ~null_terminated:false list_storage
      | None ->
          "")


  let get_struct_field_struct_list
      (struct_storage : 'cap StructStorage.t option)
      (pointer_word : int)
  : ('cap, 'a) CapnpArray.t =
    match get_struct_pointer struct_storage pointer_word with
    | Some slice ->
        begin match deref_list_pointer slice with
        | Some list_storage ->
            Some {
              CapnpArray.storage  = list_storage;
              CapnpArray.get_item = fun storage i -> Some (StructList.get storage i)
            }
        | None ->
            None
        end
    | None ->
        None


  let get_struct_field_list_list
    (struct_storage : 'cap StructStorage.t option)
    (pointer_word : int)
  : ('cap, 'cap ListStorage.t option) CapnpArray.t =
    get_struct_field_bytes_list struct_storage pointer_word (fun slice -> deref_list_pointer slice)


  let get_struct_field_struct
      (struct_storage : 'cap StructStorage.t option)
      (pointer_word : int)
  : 'cap StructStorage.t option =
    match get_struct_pointer struct_storage pointer_word with
    | Some pointer_bytes -> deref_struct_pointer pointer_bytes
    | None -> None


  let get_struct_field_bit
      (struct_storage : 'cap StructStorage.t option)
      ?(default_bit = false)
      (byte_ofs : int) (bit_ofs : int)
  : bool =
    let byte_val =
      match struct_storage with
      | Some { StructStorage.data = data; _ } when byte_ofs < data.Slice.len ->
          Slice.get_uint8 data byte_ofs
      | _ ->
          0
    in
    let bit_val = (byte_val land (1 lsl bit_ofs)) <> 0 in
    if default_bit then not bit_val else bit_val


  let get_struct_field_uint8
      (struct_storage : 'cap StructStorage.t option)
      ?(default = 0) (i : int)
  : int =
    let numeric =
      match struct_storage with
      | Some { StructStorage.data = data; _ } when i < data.Slice.len ->
          Slice.get_uint8 data i
      | _ ->
          0
    in
    numeric lxor default


  let get_struct_field_uint16
      (struct_storage : 'cap StructStorage.t option)
      ?(default = 0) (i : int)
  : int =
    let numeric =
      match struct_storage with
      | Some { StructStorage.data = data; _ } when i < data.Slice.len - 1 ->
          Slice.get_uint16 data i
      | _ ->
          0
    in
    numeric lxor default


  let get_struct_field_uint32
      (struct_storage : 'cap StructStorage.t option)
      ?(default = Uint32.zero) (i : int)
  : Uint32.t =
    let numeric =
      match struct_storage with
      | Some { StructStorage.data = data; _ } when i < data.Slice.len - 3 ->
          Slice.get_uint32 data i
      | _ ->
          Uint32.zero
    in
    Uint32.logxor numeric default


  let get_struct_field_uint64
      (struct_storage : 'cap StructStorage.t option)
      ?(default = Uint64.zero) (i : int)
  : Uint64.t =
    let numeric =
      match struct_storage with
      | Some { StructStorage.data = data; _ } when i < data.Slice.len - 7 ->
          Slice.get_uint64 data i
      | _ ->
          Uint64.zero
    in
    Uint64.logxor numeric default


  let get_struct_field_int8
      (struct_storage : 'cap StructStorage.t option)
      ?(default = 0) (i : int)
  : int =
    let numeric =
      match struct_storage with
      | Some { StructStorage.data = data; _ } when i < data.Slice.len ->
          Slice.get_int8 data i
      | _ ->
          0
    in
    numeric lxor default


  let get_struct_field_int16
      (struct_storage : 'cap StructStorage.t option)
      ?(default = 0) (i : int)
  : int =
    let numeric =
      match struct_storage with
      | Some { StructStorage.data = data; _ } when i < data.Slice.len - 1 ->
          Slice.get_int16 data i
      | _ ->
          0
    in
    numeric lxor default


  let get_struct_field_int32
      (struct_storage : 'cap StructStorage.t option)
      ?(default = Int32.zero) (i : int)
  : Int32.t =
    let numeric =
      match struct_storage with
      | Some { StructStorage.data = data; _ } when i < data.Slice.len - 3 ->
          Slice.get_int32 data i
      | _ ->
          Int32.zero
    in
    Int32.bit_xor numeric default


  let get_struct_field_int64
      (struct_storage : 'cap StructStorage.t option)
      ?(default = Int64.zero) (i : int)
  : Int64.t =
    let numeric =
      match struct_storage with
      | Some { StructStorage.data = data; _ } when i < data.Slice.len - 7 ->
          Slice.get_int64 data i
      | _ ->
          Int64.zero
    in
    Int64.bit_xor numeric default


  let get_root_struct (m : 'cap Message.t) : 'cap StructStorage.t option =
    let first_segment = Message.get_segment m 0 in
    if Segment.length first_segment < sizeof_uint64 then
      None
    else
      let pointer_bytes = {
        Slice.msg        = m;
        Slice.segment_id = 0;
        Slice.start      = 0;
        Slice.len        = sizeof_uint64
      } in
      deref_struct_pointer pointer_bytes

end

