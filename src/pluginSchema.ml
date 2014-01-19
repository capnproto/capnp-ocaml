module type S = sig
  type message_t

  module AnyPointer : sig
    type t
  end

  module ElementSize : sig
    type t =
      | Empty
      | Bit
      | Byte
      | TwoBytes
      | FourBytes
      | EightBytes
      | Pointer
      | InlineComposite
      | Undefined_ of int
  end

  module Type : sig
    type t
    type t_Type_15020482145304562784 = t
    type array_t

    module Enum : sig
      type t
      type t_Enum_11389172934837766057 = t
      type array_t

      val typeId_get : t -> Uint64.t
      val typeId_get_int_exn : t -> int
      val of_message : message_t -> t
    end

    module Interface : sig
      type t
      type t_Interface_17116997365232503999 = t
      type array_t

      val typeId_get : t -> Uint64.t
      val typeId_get_int_exn : t -> int
      val of_message : message_t -> t
    end

    module List : sig
      type t
      type t_List_9792858745991129751 = t
      type array_t

      val elementType_get : t -> t_Type_15020482145304562784
      val of_message : message_t -> t
    end

    module Struct : sig
      type t
      type t_Struct_12410354185295152851 = t
      type array_t

      val typeId_get : t -> Uint64.t
      val typeId_get_int_exn : t -> int
      val of_message : message_t -> t
    end

    type unnamed_union_t =
      | Void
      | Bool
      | Int8
      | Int16
      | Int32
      | Int64
      | Uint8
      | Uint16
      | Uint32
      | Uint64
      | Float32
      | Float64
      | Text
      | Data
      | List of List.t
      | Enum of Enum.t
      | Struct of Struct.t
      | Interface of Interface.t
      | Object
      | Undefined_ of int

    val unnamed_union_get : t -> unnamed_union_t
    val of_message : message_t -> t
  end

  module Value : sig
    type t
    type t_Value_14853958794117909659 = t
    type array_t

    type unnamed_union_t =
      | Void
      | Bool of bool
      | Int8 of int
      | Int16 of int
      | Int32 of int32
      | Int64 of int64
      | Uint8 of int
      | Uint16 of int
      | Uint32 of Uint32.t
      | Uint64 of Uint64.t
      | Float32 of float
      | Float64 of float
      | Text of string
      | Data of string
      | List of AnyPointer.t
      | Enum of int
      | Struct of AnyPointer.t
      | Interface
      | Object of AnyPointer.t
      | Undefined_ of int

    val unnamed_union_get : t -> unnamed_union_t
    val of_message : message_t -> t
  end

  module Annotation : sig
    type t
    type t_Annotation_17422339044421236034 = t
    type array_t

    val id_get : t -> Uint64.t
    val id_get_int_exn : t -> int
    val value_get : t -> Value.t
    val of_message : message_t -> t
  end

  module Method : sig
    type t
    type t_Method_10736806783679155584 = t
    type array_t

    val name_get : t -> string
    val codeOrder_get : t -> int
    val paramStructType_get : t -> Uint64.t
    val paramStructType_get_int_exn : t -> int
    val resultStructType_get : t -> Uint64.t
    val resultStructType_get_int_exn : t -> int
    val annotations_get : t -> (Annotation.t, array_t) Runtime.Array.t
    val of_message : message_t -> t
  end

  module Enumerant : sig
    type t
    type t_Enumerant_10919677598968879693 = t
    type array_t

    val name_get : t -> string
    val codeOrder_get : t -> int
    val annotations_get : t -> (Annotation.t, array_t) Runtime.Array.t
    val of_message : message_t -> t
  end

  module Field : sig
    type t
    type t_Field_11145653318641710175 = t
    type array_t

    module Ordinal : sig
      type t
      type t_Ordinal_13515537513213004774 = t
      type array_t

      type unnamed_union_t =
        | Implicit
        | Explicit of int
        | Undefined_ of int

      val unnamed_union_get : t -> unnamed_union_t
      val of_message : message_t -> t
    end

    module Group : sig
      type t
      type t_Group_14626792032033250577 = t
      type array_t

      val typeId_get : t -> Uint64.t
      val typeId_get_int_exn : t -> int
      val of_message : message_t -> t
    end

    module Slot : sig
      type t
      type t_Slot_14133145859926553711 = t
      type array_t

      val offset_get : t -> Uint32.t
      val offset_get_int_exn : t -> int
      val type_get : t -> Type.t
      val defaultValue_get : t -> Value.t
      val hadExplicitDefault_get : t -> bool
      val of_message : message_t -> t
    end

    type unnamed_union_t =
      | Slot of Slot.t
      | Group of Group.t
      | Undefined_ of int

    val unnamed_union_get : t -> unnamed_union_t
    val name_get : t -> string
    val codeOrder_get : t -> int
    val annotations_get : t -> (Annotation.t, array_t) Runtime.Array.t
    val discriminantValue_get : t -> int
    val ordinal_get : t -> Ordinal.t
    val of_message : message_t -> t
  end

  module Node : sig
    type t
    type t_Node_16610026722781537303 = t
    type array_t

    module Struct : sig
      type t
      type t_Struct_11430331134483579957 = t
      type array_t

      val dataWordCount_get : t -> int
      val pointerCount_get : t -> int
      val preferredListEncoding_get : t -> ElementSize.t
      val isGroup_get : t -> bool
      val discriminantCount_get : t -> int
      val discriminantOffset_get : t -> Uint32.t
      val discriminantOffset_get_int_exn : t -> int
      val fields_get : t -> (Field.t, array_t) Runtime.Array.t
      val of_message : message_t -> t
    end

    module Enum : sig
      type t
      type t_Enum_13063450714778629528 = t
      type array_t

      val enumerants_get : t -> (Enumerant.t, array_t) Runtime.Array.t
      val of_message : message_t -> t
    end

    module Annotation : sig
      type t
      type t_Annotation_17011813041836786320 = t
      type array_t

      val type_get : t -> Type.t
      val targetsFile_get : t -> bool
      val targetsConst_get : t -> bool
      val targetsEnum_get : t -> bool
      val targetsEnumerant_get : t -> bool
      val targetsStruct_get : t -> bool
      val targetsField_get : t -> bool
      val targetsUnion_get : t -> bool
      val targetsGroup_get : t -> bool
      val targetsInterface_get : t -> bool
      val targetsMethod_get : t -> bool
      val targetsParam_get : t -> bool
      val targetsAnnotation_get : t -> bool
      val of_message : message_t -> t
    end

    module Const : sig
      type t
      type t_Const_12793219851699983392 = t
      type array_t

      val type_get : t -> Type.t
      val value_get : t -> Value.t
      val of_message : message_t -> t
    end

    module Interface : sig
      type t
      type t_Interface_16728431493453586831 = t
      type array_t

      val methods_get : t -> (Method.t, array_t) Runtime.Array.t
      val extends_get : t -> (Uint64.t, array_t) Runtime.Array.t
      val of_message : message_t -> t
    end

    module NestedNode : sig
      type t
      type t_NestedNode_16050641862814319170 = t
      type array_t

      val name_get : t -> string
      val id_get : t -> Uint64.t
      val id_get_int_exn : t -> int
      val of_message : message_t -> t
    end

    type unnamed_union_t =
      | File
      | Struct of Struct.t
      | Enum of Enum.t
      | Interface of Interface.t
      | Const of Const.t
      | Annotation of Annotation.t
      | Undefined_ of int

    val unnamed_union_get : t -> unnamed_union_t
    val id_get : t -> Uint64.t
    val id_get_int_exn : t -> int
    val displayName_get : t -> string
    val displayNamePrefixLength_get : t -> Uint32.t
    val displayNamePrefixLength_get_int_exn : t -> int
    val scopeId_get : t -> Uint64.t
    val scopeId_get_int_exn : t -> int
    val nestedNodes_get : t -> (NestedNode.t, array_t) Runtime.Array.t
    val annotations_get : t -> (Annotation.t, array_t) Runtime.Array.t
    val of_message : message_t -> t
  end

  module CodeGeneratorRequest : sig
    type t
    type t_CodeGeneratorRequest_13818529054586492878 = t
    type array_t

    module RequestedFile : sig
      type t
      type t_RequestedFile_14981803260258615394 = t
      type array_t

      module Import : sig
        type t
        type t_Import_12560611460656617445 = t
        type array_t

        val id_get : t -> Uint64.t
        val id_get_int_exn : t -> int
        val name_get : t -> string
        val of_message : message_t -> t
      end

      val id_get : t -> Uint64.t
      val id_get_int_exn : t -> int
      val filename_get : t -> string
      val imports_get : t -> (Import.t, array_t) Runtime.Array.t
      val of_message : message_t -> t
    end

    val nodes_get : t -> (Node.t, array_t) Runtime.Array.t
    val requestedFiles_get : t -> (RequestedFile.t, array_t) Runtime.Array.t
    val of_message : message_t -> t
  end

end

module Make (MessageWrapper : Message.S) = struct
  let invalid_msg = Message.invalid_msg

  module Reader_ = MessageReader.Make(MessageWrapper)
  open Reader_

  type message_t = ro Reader_.Message.t

  module AnyPointer = struct
    type t = ro Slice.t option
  end

  module ElementSize = struct
    type t =
      | Empty
      | Bit
      | Byte
      | TwoBytes
      | FourBytes
      | EightBytes
      | Pointer
      | InlineComposite
      | Undefined_ of int
  end

  module Type = struct
    type t = ro StructStorage.t option
    type t_Type_15020482145304562784 = t
    type array_t = ro ListStorage.t

    module Enum = struct
      type t = ro StructStorage.t option
      type t_Enum_11389172934837766057 = t
      type array_t = ro ListStorage.t

      let typeId_get x = get_struct_field_uint64 ~default:Uint64.zero x 8
      let typeId_get_int_exn x = Uint64.to_int (typeId_get x)
      let of_message x = get_root_struct x
    end

    module Interface = struct
      type t = ro StructStorage.t option
      type t_Interface_17116997365232503999 = t
      type array_t = ro ListStorage.t

      let typeId_get x = get_struct_field_uint64 ~default:Uint64.zero x 8
      let typeId_get_int_exn x = Uint64.to_int (typeId_get x)
      let of_message x = get_root_struct x
    end

    module List = struct
      type t = ro StructStorage.t option
      type t_List_9792858745991129751 = t
      type array_t = ro ListStorage.t

      let elementType_get x = get_struct_field_struct x 0
      let of_message x = get_root_struct x
    end

    module Struct = struct
      type t = ro StructStorage.t option
      type t_Struct_12410354185295152851 = t
      type array_t = ro ListStorage.t

      let typeId_get x = get_struct_field_uint64 ~default:Uint64.zero x 8
      let typeId_get_int_exn x = Uint64.to_int (typeId_get x)
      let of_message x = get_root_struct x
    end

    let void_get x = ()
    let bool_get x = ()
    let int8_get x = ()
    let int16_get x = ()
    let int32_get x = ()
    let int64_get x = ()
    let uint8_get x = ()
    let uint16_get x = ()
    let uint32_get x = ()
    let uint64_get x = ()
    let float32_get x = ()
    let float64_get x = ()
    let text_get x = ()
    let data_get x = ()
    let list_get x = x
    let enum_get x = x
    let struct_get x = x
    let interface_get x = x
    let object_get x = ()
    type unnamed_union_t =
      | Void
      | Bool
      | Int8
      | Int16
      | Int32
      | Int64
      | Uint8
      | Uint16
      | Uint32
      | Uint64
      | Float32
      | Float64
      | Text
      | Data
      | List of List.t
      | Enum of Enum.t
      | Struct of Struct.t
      | Interface of Interface.t
      | Object
      | Undefined_ of int

    let unnamed_union_get x =
      match get_struct_field_uint16 ~default:0 x 0 with
      | 0 -> Void
      | 1 -> Bool
      | 2 -> Int8
      | 3 -> Int16
      | 4 -> Int32
      | 5 -> Int64
      | 6 -> Uint8
      | 7 -> Uint16
      | 8 -> Uint32
      | 9 -> Uint64
      | 10 -> Float32
      | 11 -> Float64
      | 12 -> Text
      | 13 -> Data
      | 14 -> List (list_get x)
      | 15 -> Enum (enum_get x)
      | 16 -> Struct (struct_get x)
      | 17 -> Interface (interface_get x)
      | 18 -> Object
      | v -> Undefined_ v
    let of_message x = get_root_struct x
  end

  module Value = struct
    type t = ro StructStorage.t option
    type t_Value_14853958794117909659 = t
    type array_t = ro ListStorage.t

    let void_get x = ()
    let bool_get x = get_struct_field_bit ~default_bit:false x 2 0
    let int8_get x = get_struct_field_int8 ~default:0 x 2
    let int16_get x = get_struct_field_int16 ~default:0 x 2
    let int32_get x = get_struct_field_int32 ~default:0l x 4
    let int32_get_int_exn x = Int32.to_int (int32_get x)
    let int64_get x = get_struct_field_int64 ~default:0L x 8
    let int64_get_int_exn x = Int64.to_int (int64_get x)
    let uint8_get x = get_struct_field_uint8 ~default:0 x 2
    let uint16_get x = get_struct_field_uint16 ~default:0 x 2
    let uint32_get x = get_struct_field_uint32 ~default:Uint32.zero x 4
    let uint32_get_int_exn x = Uint32.to_int (uint32_get x)
    let uint64_get x = get_struct_field_uint64 ~default:Uint64.zero x 8
    let uint64_get_int_exn x = Uint64.to_int (uint64_get x)
    let float32_get x = Int32.float_of_bits (get_struct_field_int32 ~default:0l x 4)
    let float64_get x = Int64.float_of_bits (get_struct_field_int64 ~default:0L x 8)
    let text_get x = get_struct_field_text ~default:"" x 0
    let data_get x = get_struct_field_blob ~default:"" x 0
    let list_get x = get_struct_pointer x 0
    let enum_get x = get_struct_field_uint16 ~default:0 x 2
    let struct_get x = get_struct_pointer x 0
    let interface_get x = ()
    let object_get x = get_struct_pointer x 0
    type unnamed_union_t =
      | Void
      | Bool of bool
      | Int8 of int
      | Int16 of int
      | Int32 of int32
      | Int64 of int64
      | Uint8 of int
      | Uint16 of int
      | Uint32 of Uint32.t
      | Uint64 of Uint64.t
      | Float32 of float
      | Float64 of float
      | Text of string
      | Data of string
      | List of AnyPointer.t
      | Enum of int
      | Struct of AnyPointer.t
      | Interface
      | Object of AnyPointer.t
      | Undefined_ of int

    let unnamed_union_get x =
      match get_struct_field_uint16 ~default:0 x 0 with
      | 0 -> Void
      | 1 -> Bool (bool_get x)
      | 2 -> Int8 (int8_get x)
      | 3 -> Int16 (int16_get x)
      | 4 -> Int32 (int32_get x)
      | 5 -> Int64 (int64_get x)
      | 6 -> Uint8 (uint8_get x)
      | 7 -> Uint16 (uint16_get x)
      | 8 -> Uint32 (uint32_get x)
      | 9 -> Uint64 (uint64_get x)
      | 10 -> Float32 (float32_get x)
      | 11 -> Float64 (float64_get x)
      | 12 -> Text (text_get x)
      | 13 -> Data (data_get x)
      | 14 -> List (list_get x)
      | 15 -> Enum (enum_get x)
      | 16 -> Struct (struct_get x)
      | 17 -> Interface
      | 18 -> Object (object_get x)
      | v -> Undefined_ v
    let of_message x = get_root_struct x
  end

  module Annotation = struct
    type t = ro StructStorage.t option
    type t_Annotation_17422339044421236034 = t
    type array_t = ro ListStorage.t

    let id_get x = get_struct_field_uint64 ~default:Uint64.zero x 0
    let id_get_int_exn x = Uint64.to_int (id_get x)
    let value_get x = get_struct_field_struct x 0
    let of_message x = get_root_struct x
  end

  module Method = struct
    type t = ro StructStorage.t option
    type t_Method_10736806783679155584 = t
    type array_t = ro ListStorage.t

    let name_get x = get_struct_field_text ~default:"" x 0
    let codeOrder_get x = get_struct_field_uint16 ~default:0 x 0
    let paramStructType_get x = get_struct_field_uint64 ~default:Uint64.zero x 8
    let paramStructType_get_int_exn x = Uint64.to_int (paramStructType_get x)
    let resultStructType_get x = get_struct_field_uint64 ~default:Uint64.zero x 16
    let resultStructType_get_int_exn x = Uint64.to_int (resultStructType_get x)
    let annotations_get x = get_struct_field_struct_list x 1
    let of_message x = get_root_struct x
  end

  module Enumerant = struct
    type t = ro StructStorage.t option
    type t_Enumerant_10919677598968879693 = t
    type array_t = ro ListStorage.t

    let name_get x = get_struct_field_text ~default:"" x 0
    let codeOrder_get x = get_struct_field_uint16 ~default:0 x 0
    let annotations_get x = get_struct_field_struct_list x 1
    let of_message x = get_root_struct x
  end

  module Field = struct
    type t = ro StructStorage.t option
    type t_Field_11145653318641710175 = t
    type array_t = ro ListStorage.t

    module Ordinal = struct
      type t = ro StructStorage.t option
      type t_Ordinal_13515537513213004774 = t
      type array_t = ro ListStorage.t

      let implicit_get x = ()
      let explicit_get x = get_struct_field_uint16 ~default:0 x 12
      type unnamed_union_t =
        | Implicit
        | Explicit of int
        | Undefined_ of int

      let unnamed_union_get x =
        match get_struct_field_uint16 ~default:0 x 10 with
        | 0 -> Implicit
        | 1 -> Explicit (explicit_get x)
        | v -> Undefined_ v
      let of_message x = get_root_struct x
    end

    module Group = struct
      type t = ro StructStorage.t option
      type t_Group_14626792032033250577 = t
      type array_t = ro ListStorage.t

      let typeId_get x = get_struct_field_uint64 ~default:Uint64.zero x 16
      let typeId_get_int_exn x = Uint64.to_int (typeId_get x)
      let of_message x = get_root_struct x
    end

    module Slot = struct
      type t = ro StructStorage.t option
      type t_Slot_14133145859926553711 = t
      type array_t = ro ListStorage.t

      let offset_get x = get_struct_field_uint32 ~default:Uint32.zero x 4
      let offset_get_int_exn x = Uint32.to_int (offset_get x)
      let type_get x = get_struct_field_struct x 2
      let defaultValue_get x = get_struct_field_struct x 3
      let hadExplicitDefault_get x = get_struct_field_bit ~default_bit:false x 16 0
      let of_message x = get_root_struct x
    end

    let name_get x = get_struct_field_text ~default:"" x 0
    let codeOrder_get x = get_struct_field_uint16 ~default:0 x 0
    let annotations_get x = get_struct_field_struct_list x 1
    let discriminantValue_get x = get_struct_field_uint16 ~default:65535 x 2
    let slot_get x = x
    let group_get x = x
    let ordinal_get x = x
    type unnamed_union_t =
      | Slot of Slot.t
      | Group of Group.t
      | Undefined_ of int

    let unnamed_union_get x =
      match get_struct_field_uint16 ~default:0 x 8 with
      | 0 -> Slot (slot_get x)
      | 1 -> Group (group_get x)
      | v -> Undefined_ v
    let of_message x = get_root_struct x
  end

  module Node = struct
    type t = ro StructStorage.t option
    type t_Node_16610026722781537303 = t
    type array_t = ro ListStorage.t

    module Struct = struct
      type t = ro StructStorage.t option
      type t_Struct_11430331134483579957 = t
      type array_t = ro ListStorage.t

      let dataWordCount_get x = get_struct_field_uint16 ~default:0 x 14
      let pointerCount_get x = get_struct_field_uint16 ~default:0 x 24
      let preferredListEncoding_get x =
        let decode =
          (fun u16 -> match u16 with
            | 0 -> ElementSize.Empty
            | 1 -> ElementSize.Bit
            | 2 -> ElementSize.Byte
            | 3 -> ElementSize.TwoBytes
            | 4 -> ElementSize.FourBytes
            | 5 -> ElementSize.EightBytes
            | 6 -> ElementSize.Pointer
            | 7 -> ElementSize.InlineComposite
            | v -> ElementSize.Undefined_ v)
        in
      decode (get_struct_field_uint16 ~default:0 x 26)
      let isGroup_get x = get_struct_field_bit ~default_bit:false x 28 0
      let discriminantCount_get x = get_struct_field_uint16 ~default:0 x 30
      let discriminantOffset_get x = get_struct_field_uint32 ~default:Uint32.zero x 32
      let discriminantOffset_get_int_exn x = Uint32.to_int (discriminantOffset_get x)
      let fields_get x = get_struct_field_struct_list x 3
      let of_message x = get_root_struct x
    end

    module Enum = struct
      type t = ro StructStorage.t option
      type t_Enum_13063450714778629528 = t
      type array_t = ro ListStorage.t

      let enumerants_get x = get_struct_field_struct_list x 3
      let of_message x = get_root_struct x
    end

    module Annotation = struct
      type t = ro StructStorage.t option
      type t_Annotation_17011813041836786320 = t
      type array_t = ro ListStorage.t

      let type_get x = get_struct_field_struct x 3
      let targetsFile_get x = get_struct_field_bit ~default_bit:false x 14 0
      let targetsConst_get x = get_struct_field_bit ~default_bit:false x 14 1
      let targetsEnum_get x = get_struct_field_bit ~default_bit:false x 14 2
      let targetsEnumerant_get x = get_struct_field_bit ~default_bit:false x 14 3
      let targetsStruct_get x = get_struct_field_bit ~default_bit:false x 14 4
      let targetsField_get x = get_struct_field_bit ~default_bit:false x 14 5
      let targetsUnion_get x = get_struct_field_bit ~default_bit:false x 14 6
      let targetsGroup_get x = get_struct_field_bit ~default_bit:false x 14 7
      let targetsInterface_get x = get_struct_field_bit ~default_bit:false x 15 0
      let targetsMethod_get x = get_struct_field_bit ~default_bit:false x 15 1
      let targetsParam_get x = get_struct_field_bit ~default_bit:false x 15 2
      let targetsAnnotation_get x = get_struct_field_bit ~default_bit:false x 15 3
      let of_message x = get_root_struct x
    end

    module Const = struct
      type t = ro StructStorage.t option
      type t_Const_12793219851699983392 = t
      type array_t = ro ListStorage.t

      let type_get x = get_struct_field_struct x 3
      let value_get x = get_struct_field_struct x 4
      let of_message x = get_root_struct x
    end

    module Interface = struct
      type t = ro StructStorage.t option
      type t_Interface_16728431493453586831 = t
      type array_t = ro ListStorage.t

      let methods_get x = get_struct_field_struct_list x 3
      let extends_get x = get_struct_field_uint64_list x 4
      let of_message x = get_root_struct x
    end

    module NestedNode = struct
      type t = ro StructStorage.t option
      type t_NestedNode_16050641862814319170 = t
      type array_t = ro ListStorage.t

      let name_get x = get_struct_field_text ~default:"" x 0
      let id_get x = get_struct_field_uint64 ~default:Uint64.zero x 0
      let id_get_int_exn x = Uint64.to_int (id_get x)
      let of_message x = get_root_struct x
    end

    let id_get x = get_struct_field_uint64 ~default:Uint64.zero x 0
    let id_get_int_exn x = Uint64.to_int (id_get x)
    let displayName_get x = get_struct_field_text ~default:"" x 0
    let displayNamePrefixLength_get x = get_struct_field_uint32 ~default:Uint32.zero x 8
    let displayNamePrefixLength_get_int_exn x = Uint32.to_int (displayNamePrefixLength_get x)
    let scopeId_get x = get_struct_field_uint64 ~default:Uint64.zero x 16
    let scopeId_get_int_exn x = Uint64.to_int (scopeId_get x)
    let nestedNodes_get x = get_struct_field_struct_list x 1
    let annotations_get x = get_struct_field_struct_list x 2
    let file_get x = ()
    let struct_get x = x
    let enum_get x = x
    let interface_get x = x
    let const_get x = x
    let annotation_get x = x
    type unnamed_union_t =
      | File
      | Struct of Struct.t
      | Enum of Enum.t
      | Interface of Interface.t
      | Const of Const.t
      | Annotation of Annotation.t
      | Undefined_ of int

    let unnamed_union_get x =
      match get_struct_field_uint16 ~default:0 x 12 with
      | 0 -> File
      | 1 -> Struct (struct_get x)
      | 2 -> Enum (enum_get x)
      | 3 -> Interface (interface_get x)
      | 4 -> Const (const_get x)
      | 5 -> Annotation (annotation_get x)
      | v -> Undefined_ v
    let of_message x = get_root_struct x
  end

  module CodeGeneratorRequest = struct
    type t = ro StructStorage.t option
    type t_CodeGeneratorRequest_13818529054586492878 = t
    type array_t = ro ListStorage.t

    module RequestedFile = struct
      type t = ro StructStorage.t option
      type t_RequestedFile_14981803260258615394 = t
      type array_t = ro ListStorage.t

      module Import = struct
        type t = ro StructStorage.t option
        type t_Import_12560611460656617445 = t
        type array_t = ro ListStorage.t

        let id_get x = get_struct_field_uint64 ~default:Uint64.zero x 0
        let id_get_int_exn x = Uint64.to_int (id_get x)
        let name_get x = get_struct_field_text ~default:"" x 0
        let of_message x = get_root_struct x
      end

      let id_get x = get_struct_field_uint64 ~default:Uint64.zero x 0
      let id_get_int_exn x = Uint64.to_int (id_get x)
      let filename_get x = get_struct_field_text ~default:"" x 0
      let imports_get x = get_struct_field_struct_list x 1
      let of_message x = get_root_struct x
    end

    let nodes_get x = get_struct_field_struct_list x 0
    let requestedFiles_get x = get_struct_field_struct_list x 1
    let of_message x = get_root_struct x
  end

end

