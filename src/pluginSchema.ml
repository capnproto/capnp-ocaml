type ro = Message.ro
type rw = Message.rw

module type S = sig
  type 'cap message_t
  type reader_array_t
  type builder_array_t

  module AnyPointer : sig
    type reader_t
    type builder_t
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
      | Undefined of int
  end
  module Type : sig
    type reader_t
    type builder_t
    type reader_t_Type_15020482145304562784 = reader_t
    type builder_t_Type_15020482145304562784 = builder_t
    module Enum : sig
      type reader_t
      type builder_t
      type reader_t_Enum_11389172934837766057 = reader_t
      type builder_t_Enum_11389172934837766057 = builder_t
      module R : sig
        type t = reader_t
        val typeId_get : t -> Uint64.t
        val typeId_get_int_exn : t -> int
        val of_message : 'cap message_t -> t
      end
      module B : sig
        type t = builder_t
        val typeId_get : t -> Uint64.t
        val typeId_get_int_exn : t -> int
        val typeId_set : t -> Uint64.t -> unit
        val typeId_set_int_exn : t -> int -> unit
        val of_message : rw message_t -> t
      end
    end
    module Interface : sig
      type reader_t
      type builder_t
      type reader_t_Interface_17116997365232503999 = reader_t
      type builder_t_Interface_17116997365232503999 = builder_t
      module R : sig
        type t = reader_t
        val typeId_get : t -> Uint64.t
        val typeId_get_int_exn : t -> int
        val of_message : 'cap message_t -> t
      end
      module B : sig
        type t = builder_t
        val typeId_get : t -> Uint64.t
        val typeId_get_int_exn : t -> int
        val typeId_set : t -> Uint64.t -> unit
        val typeId_set_int_exn : t -> int -> unit
        val of_message : rw message_t -> t
      end
    end
    module List : sig
      type reader_t
      type builder_t
      type reader_t_List_9792858745991129751 = reader_t
      type builder_t_List_9792858745991129751 = builder_t
      module R : sig
        type t = reader_t
        val elementType_get : t -> reader_t_Type_15020482145304562784
        val of_message : 'cap message_t -> t
      end
      module B : sig
        type t = builder_t
        val elementType_get : t -> builder_t_Type_15020482145304562784
        val elementType_set_reader : t -> reader_t_Type_15020482145304562784 -> builder_t_Type_15020482145304562784
        val elementType_set_builder : t -> builder_t_Type_15020482145304562784 -> builder_t_Type_15020482145304562784
        val elementType_init : t -> builder_t_Type_15020482145304562784
        val of_message : rw message_t -> t
      end
    end
    module Struct : sig
      type reader_t
      type builder_t
      type reader_t_Struct_12410354185295152851 = reader_t
      type builder_t_Struct_12410354185295152851 = builder_t
      module R : sig
        type t = reader_t
        val typeId_get : t -> Uint64.t
        val typeId_get_int_exn : t -> int
        val of_message : 'cap message_t -> t
      end
      module B : sig
        type t = builder_t
        val typeId_get : t -> Uint64.t
        val typeId_get_int_exn : t -> int
        val typeId_set : t -> Uint64.t -> unit
        val typeId_set_int_exn : t -> int -> unit
        val of_message : rw message_t -> t
      end
    end
    module R : sig
      type t = reader_t
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
        | List of List.reader_t
        | Enum of Enum.reader_t
        | Struct of Struct.reader_t
        | Interface of Interface.reader_t
        | AnyPointer
        | Undefined of int
      val get : t -> unnamed_union_t
      val of_message : 'cap message_t -> t
    end
    module B : sig
      type t = builder_t
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
        | List of List.builder_t
        | Enum of Enum.builder_t
        | Struct of Struct.builder_t
        | Interface of Interface.builder_t
        | AnyPointer
        | Undefined of int
      val get : t -> unnamed_union_t
      val void_set : t -> unit
      val bool_set : t -> unit
      val int8_set : t -> unit
      val int16_set : t -> unit
      val int32_set : t -> unit
      val int64_set : t -> unit
      val uint8_set : t -> unit
      val uint16_set : t -> unit
      val uint32_set : t -> unit
      val uint64_set : t -> unit
      val float32_set : t -> unit
      val float64_set : t -> unit
      val text_set : t -> unit
      val data_set : t -> unit
      val anyPointer_set : t -> unit
      val of_message : rw message_t -> t
    end
  end
  module Value : sig
    type reader_t
    type builder_t
    type reader_t_Value_14853958794117909659 = reader_t
    type builder_t_Value_14853958794117909659 = builder_t
    module R : sig
      type t = reader_t
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
        | List of AnyPointer.reader_t
        | Enum of int
        | Struct of AnyPointer.reader_t
        | Interface
        | AnyPointer of AnyPointer.reader_t
        | Undefined of int
      val get : t -> unnamed_union_t
      val of_message : 'cap message_t -> t
    end
    module B : sig
      type t = builder_t
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
        | List of AnyPointer.builder_t
        | Enum of int
        | Struct of AnyPointer.builder_t
        | Interface
        | AnyPointer of AnyPointer.builder_t
        | Undefined of int
      val get : t -> unnamed_union_t
      val void_set : t -> unit
      val bool_set : t -> bool -> unit
      val int8_set_exn : t -> int -> unit
      val int16_set_exn : t -> int -> unit
      val int32_set : t -> int32 -> unit
      val int32_set_int_exn : t -> int -> unit
      val int64_set : t -> int64 -> unit
      val int64_set_int_exn : t -> int -> unit
      val uint8_set_exn : t -> int -> unit
      val uint16_set_exn : t -> int -> unit
      val uint32_set : t -> Uint32.t -> unit
      val uint32_set_int_exn : t -> int -> unit
      val uint64_set : t -> Uint64.t -> unit
      val uint64_set_int_exn : t -> int -> unit
      val float32_set : t -> float -> unit
      val float64_set : t -> float -> unit
      val text_set : t -> string -> unit
      val data_set : t -> string -> unit
      val list_set : t -> AnyPointer.builder_t -> unit
      val enum_set_exn : t -> int -> unit
      val struct_set : t -> AnyPointer.builder_t -> unit
      val interface_set : t -> unit
      val anyPointer_set : t -> AnyPointer.builder_t -> unit
      val of_message : rw message_t -> t
    end
  end
  module Annotation : sig
    type reader_t
    type builder_t
    type reader_t_Annotation_17422339044421236034 = reader_t
    type builder_t_Annotation_17422339044421236034 = builder_t
    module R : sig
      type t = reader_t
      val id_get : t -> Uint64.t
      val id_get_int_exn : t -> int
      val value_get : t -> Value.reader_t
      val of_message : 'cap message_t -> t
    end
    module B : sig
      type t = builder_t
      val id_get : t -> Uint64.t
      val id_get_int_exn : t -> int
      val value_get : t -> Value.builder_t
      val id_set : t -> Uint64.t -> unit
      val id_set_int_exn : t -> int -> unit
      val value_set_reader : t -> Value.reader_t -> Value.builder_t
      val value_set_builder : t -> Value.builder_t -> Value.builder_t
      val value_init : t -> Value.builder_t
      val of_message : rw message_t -> t
    end
  end
  module Method : sig
    type reader_t
    type builder_t
    type reader_t_Method_10736806783679155584 = reader_t
    type builder_t_Method_10736806783679155584 = builder_t
    module R : sig
      type t = reader_t
      val name_get : t -> string
      val codeOrder_get : t -> int
      val paramStructType_get : t -> Uint64.t
      val paramStructType_get_int_exn : t -> int
      val resultStructType_get : t -> Uint64.t
      val resultStructType_get_int_exn : t -> int
      val annotations_get : t -> (ro, Annotation.reader_t, reader_array_t) Runtime.Array.t
      val of_message : 'cap message_t -> t
    end
    module B : sig
      type t = builder_t
      val name_get : t -> string
      val codeOrder_get : t -> int
      val paramStructType_get : t -> Uint64.t
      val paramStructType_get_int_exn : t -> int
      val resultStructType_get : t -> Uint64.t
      val resultStructType_get_int_exn : t -> int
      val annotations_get : t -> (rw, Annotation.builder_t, builder_array_t) Runtime.Array.t
      val name_set : t -> string -> unit
      val codeOrder_set_exn : t -> int -> unit
      val paramStructType_set : t -> Uint64.t -> unit
      val paramStructType_set_int_exn : t -> int -> unit
      val resultStructType_set : t -> Uint64.t -> unit
      val resultStructType_set_int_exn : t -> int -> unit
      val annotations_set : t -> (rw, Annotation.builder_t, builder_array_t) Runtime.Array.t -> (rw, Annotation.builder_t, builder_array_t) Runtime.Array.t
      val annotations_init : t -> int -> (rw, Annotation.builder_t, builder_array_t) Runtime.Array.t
      val of_message : rw message_t -> t
    end
  end
  module Enumerant : sig
    type reader_t
    type builder_t
    type reader_t_Enumerant_10919677598968879693 = reader_t
    type builder_t_Enumerant_10919677598968879693 = builder_t
    module R : sig
      type t = reader_t
      val name_get : t -> string
      val codeOrder_get : t -> int
      val annotations_get : t -> (ro, Annotation.reader_t, reader_array_t) Runtime.Array.t
      val of_message : 'cap message_t -> t
    end
    module B : sig
      type t = builder_t
      val name_get : t -> string
      val codeOrder_get : t -> int
      val annotations_get : t -> (rw, Annotation.builder_t, builder_array_t) Runtime.Array.t
      val name_set : t -> string -> unit
      val codeOrder_set_exn : t -> int -> unit
      val annotations_set : t -> (rw, Annotation.builder_t, builder_array_t) Runtime.Array.t -> (rw, Annotation.builder_t, builder_array_t) Runtime.Array.t
      val annotations_init : t -> int -> (rw, Annotation.builder_t, builder_array_t) Runtime.Array.t
      val of_message : rw message_t -> t
    end
  end
  module Field : sig
    type reader_t
    type builder_t
    type reader_t_Field_11145653318641710175 = reader_t
    type builder_t_Field_11145653318641710175 = builder_t
    val noDiscriminant : int
    module Ordinal : sig
      type reader_t
      type builder_t
      type reader_t_Ordinal_13515537513213004774 = reader_t
      type builder_t_Ordinal_13515537513213004774 = builder_t
      module R : sig
        type t = reader_t
        type unnamed_union_t =
          | Implicit
          | Explicit of int
          | Undefined of int
        val get : t -> unnamed_union_t
        val of_message : 'cap message_t -> t
      end
      module B : sig
        type t = builder_t
        type unnamed_union_t =
          | Implicit
          | Explicit of int
          | Undefined of int
        val get : t -> unnamed_union_t
        val implicit_set : t -> unit
        val explicit_set_exn : t -> int -> unit
        val of_message : rw message_t -> t
      end
    end
    module Group : sig
      type reader_t
      type builder_t
      type reader_t_Group_14626792032033250577 = reader_t
      type builder_t_Group_14626792032033250577 = builder_t
      module R : sig
        type t = reader_t
        val typeId_get : t -> Uint64.t
        val typeId_get_int_exn : t -> int
        val of_message : 'cap message_t -> t
      end
      module B : sig
        type t = builder_t
        val typeId_get : t -> Uint64.t
        val typeId_get_int_exn : t -> int
        val typeId_set : t -> Uint64.t -> unit
        val typeId_set_int_exn : t -> int -> unit
        val of_message : rw message_t -> t
      end
    end
    module Slot : sig
      type reader_t
      type builder_t
      type reader_t_Slot_14133145859926553711 = reader_t
      type builder_t_Slot_14133145859926553711 = builder_t
      module R : sig
        type t = reader_t
        val offset_get : t -> Uint32.t
        val offset_get_int_exn : t -> int
        val type_get : t -> Type.reader_t
        val defaultValue_get : t -> Value.reader_t
        val hadExplicitDefault_get : t -> bool
        val of_message : 'cap message_t -> t
      end
      module B : sig
        type t = builder_t
        val offset_get : t -> Uint32.t
        val offset_get_int_exn : t -> int
        val type_get : t -> Type.builder_t
        val defaultValue_get : t -> Value.builder_t
        val hadExplicitDefault_get : t -> bool
        val offset_set : t -> Uint32.t -> unit
        val offset_set_int_exn : t -> int -> unit
        val type_set_reader : t -> Type.reader_t -> Type.builder_t
        val type_set_builder : t -> Type.builder_t -> Type.builder_t
        val type_init : t -> Type.builder_t
        val defaultValue_set_reader : t -> Value.reader_t -> Value.builder_t
        val defaultValue_set_builder : t -> Value.builder_t -> Value.builder_t
        val defaultValue_init : t -> Value.builder_t
        val hadExplicitDefault_set : t -> bool -> unit
        val of_message : rw message_t -> t
      end
    end
    module R : sig
      type t = reader_t
      type unnamed_union_t =
        | Slot of Slot.reader_t
        | Group of Group.reader_t
        | Undefined of int
      val get : t -> unnamed_union_t
      val name_get : t -> string
      val codeOrder_get : t -> int
      val annotations_get : t -> (ro, Annotation.reader_t, reader_array_t) Runtime.Array.t
      val discriminantValue_get : t -> int
      val ordinal_get : t -> Ordinal.reader_t
      val of_message : 'cap message_t -> t
    end
    module B : sig
      type t = builder_t
      type unnamed_union_t =
        | Slot of Slot.builder_t
        | Group of Group.builder_t
        | Undefined of int
      val get : t -> unnamed_union_t
      val name_get : t -> string
      val codeOrder_get : t -> int
      val annotations_get : t -> (rw, Annotation.builder_t, builder_array_t) Runtime.Array.t
      val discriminantValue_get : t -> int
      val ordinal_get : t -> Ordinal.builder_t
      val name_set : t -> string -> unit
      val codeOrder_set_exn : t -> int -> unit
      val annotations_set : t -> (rw, Annotation.builder_t, builder_array_t) Runtime.Array.t -> (rw, Annotation.builder_t, builder_array_t) Runtime.Array.t
      val annotations_init : t -> int -> (rw, Annotation.builder_t, builder_array_t) Runtime.Array.t
      val discriminantValue_set_exn : t -> int -> unit
      val of_message : rw message_t -> t
    end
  end
  module Node : sig
    type reader_t
    type builder_t
    type reader_t_Node_16610026722781537303 = reader_t
    type builder_t_Node_16610026722781537303 = builder_t
    module Struct : sig
      type reader_t
      type builder_t
      type reader_t_Struct_11430331134483579957 = reader_t
      type builder_t_Struct_11430331134483579957 = builder_t
      module R : sig
        type t = reader_t
        val dataWordCount_get : t -> int
        val pointerCount_get : t -> int
        val preferredListEncoding_get : t -> ElementSize.t
        val isGroup_get : t -> bool
        val discriminantCount_get : t -> int
        val discriminantOffset_get : t -> Uint32.t
        val discriminantOffset_get_int_exn : t -> int
        val fields_get : t -> (ro, Field.reader_t, reader_array_t) Runtime.Array.t
        val of_message : 'cap message_t -> t
      end
      module B : sig
        type t = builder_t
        val dataWordCount_get : t -> int
        val pointerCount_get : t -> int
        val preferredListEncoding_get : t -> ElementSize.t
        val isGroup_get : t -> bool
        val discriminantCount_get : t -> int
        val discriminantOffset_get : t -> Uint32.t
        val discriminantOffset_get_int_exn : t -> int
        val fields_get : t -> (rw, Field.builder_t, builder_array_t) Runtime.Array.t
        val dataWordCount_set_exn : t -> int -> unit
        val pointerCount_set_exn : t -> int -> unit
        val preferredListEncoding_set : t -> ElementSize.t -> unit
        val preferredListEncoding_set_unsafe : t -> ElementSize.t -> unit
        val isGroup_set : t -> bool -> unit
        val discriminantCount_set_exn : t -> int -> unit
        val discriminantOffset_set : t -> Uint32.t -> unit
        val discriminantOffset_set_int_exn : t -> int -> unit
        val fields_set : t -> (rw, Field.builder_t, builder_array_t) Runtime.Array.t -> (rw, Field.builder_t, builder_array_t) Runtime.Array.t
        val fields_init : t -> int -> (rw, Field.builder_t, builder_array_t) Runtime.Array.t
        val of_message : rw message_t -> t
      end
    end
    module Enum : sig
      type reader_t
      type builder_t
      type reader_t_Enum_13063450714778629528 = reader_t
      type builder_t_Enum_13063450714778629528 = builder_t
      module R : sig
        type t = reader_t
        val enumerants_get : t -> (ro, Enumerant.reader_t, reader_array_t) Runtime.Array.t
        val of_message : 'cap message_t -> t
      end
      module B : sig
        type t = builder_t
        val enumerants_get : t -> (rw, Enumerant.builder_t, builder_array_t) Runtime.Array.t
        val enumerants_set : t -> (rw, Enumerant.builder_t, builder_array_t) Runtime.Array.t -> (rw, Enumerant.builder_t, builder_array_t) Runtime.Array.t
        val enumerants_init : t -> int -> (rw, Enumerant.builder_t, builder_array_t) Runtime.Array.t
        val of_message : rw message_t -> t
      end
    end
    module Annotation : sig
      type reader_t
      type builder_t
      type reader_t_Annotation_17011813041836786320 = reader_t
      type builder_t_Annotation_17011813041836786320 = builder_t
      module R : sig
        type t = reader_t
        val type_get : t -> Type.reader_t
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
        val of_message : 'cap message_t -> t
      end
      module B : sig
        type t = builder_t
        val type_get : t -> Type.builder_t
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
        val type_set_reader : t -> Type.reader_t -> Type.builder_t
        val type_set_builder : t -> Type.builder_t -> Type.builder_t
        val type_init : t -> Type.builder_t
        val targetsFile_set : t -> bool -> unit
        val targetsConst_set : t -> bool -> unit
        val targetsEnum_set : t -> bool -> unit
        val targetsEnumerant_set : t -> bool -> unit
        val targetsStruct_set : t -> bool -> unit
        val targetsField_set : t -> bool -> unit
        val targetsUnion_set : t -> bool -> unit
        val targetsGroup_set : t -> bool -> unit
        val targetsInterface_set : t -> bool -> unit
        val targetsMethod_set : t -> bool -> unit
        val targetsParam_set : t -> bool -> unit
        val targetsAnnotation_set : t -> bool -> unit
        val of_message : rw message_t -> t
      end
    end
    module Const : sig
      type reader_t
      type builder_t
      type reader_t_Const_12793219851699983392 = reader_t
      type builder_t_Const_12793219851699983392 = builder_t
      module R : sig
        type t = reader_t
        val type_get : t -> Type.reader_t
        val value_get : t -> Value.reader_t
        val of_message : 'cap message_t -> t
      end
      module B : sig
        type t = builder_t
        val type_get : t -> Type.builder_t
        val value_get : t -> Value.builder_t
        val type_set_reader : t -> Type.reader_t -> Type.builder_t
        val type_set_builder : t -> Type.builder_t -> Type.builder_t
        val type_init : t -> Type.builder_t
        val value_set_reader : t -> Value.reader_t -> Value.builder_t
        val value_set_builder : t -> Value.builder_t -> Value.builder_t
        val value_init : t -> Value.builder_t
        val of_message : rw message_t -> t
      end
    end
    module Interface : sig
      type reader_t
      type builder_t
      type reader_t_Interface_16728431493453586831 = reader_t
      type builder_t_Interface_16728431493453586831 = builder_t
      module R : sig
        type t = reader_t
        val methods_get : t -> (ro, Method.reader_t, reader_array_t) Runtime.Array.t
        val extends_get : t -> (ro, Uint64.t, reader_array_t) Runtime.Array.t
        val of_message : 'cap message_t -> t
      end
      module B : sig
        type t = builder_t
        val methods_get : t -> (rw, Method.builder_t, builder_array_t) Runtime.Array.t
        val extends_get : t -> (rw, Uint64.t, builder_array_t) Runtime.Array.t
        val methods_set : t -> (rw, Method.builder_t, builder_array_t) Runtime.Array.t -> (rw, Method.builder_t, builder_array_t) Runtime.Array.t
        val methods_init : t -> int -> (rw, Method.builder_t, builder_array_t) Runtime.Array.t
        val extends_set : t -> (rw, Uint64.t, builder_array_t) Runtime.Array.t -> (rw, Uint64.t, builder_array_t) Runtime.Array.t
        val extends_init : t -> int -> (rw, Uint64.t, builder_array_t) Runtime.Array.t
        val of_message : rw message_t -> t
      end
    end
    module NestedNode : sig
      type reader_t
      type builder_t
      type reader_t_NestedNode_16050641862814319170 = reader_t
      type builder_t_NestedNode_16050641862814319170 = builder_t
      module R : sig
        type t = reader_t
        val name_get : t -> string
        val id_get : t -> Uint64.t
        val id_get_int_exn : t -> int
        val of_message : 'cap message_t -> t
      end
      module B : sig
        type t = builder_t
        val name_get : t -> string
        val id_get : t -> Uint64.t
        val id_get_int_exn : t -> int
        val name_set : t -> string -> unit
        val id_set : t -> Uint64.t -> unit
        val id_set_int_exn : t -> int -> unit
        val of_message : rw message_t -> t
      end
    end
    module R : sig
      type t = reader_t
      type unnamed_union_t =
        | File
        | Struct of Struct.reader_t
        | Enum of Enum.reader_t
        | Interface of Interface.reader_t
        | Const of Const.reader_t
        | Annotation of Annotation.reader_t
        | Undefined of int
      val get : t -> unnamed_union_t
      val id_get : t -> Uint64.t
      val id_get_int_exn : t -> int
      val displayName_get : t -> string
      val displayNamePrefixLength_get : t -> Uint32.t
      val displayNamePrefixLength_get_int_exn : t -> int
      val scopeId_get : t -> Uint64.t
      val scopeId_get_int_exn : t -> int
      val nestedNodes_get : t -> (ro, NestedNode.reader_t, reader_array_t) Runtime.Array.t
      val annotations_get : t -> (ro, Annotation.reader_t, reader_array_t) Runtime.Array.t
      val of_message : 'cap message_t -> t
    end
    module B : sig
      type t = builder_t
      type unnamed_union_t =
        | File
        | Struct of Struct.builder_t
        | Enum of Enum.builder_t
        | Interface of Interface.builder_t
        | Const of Const.builder_t
        | Annotation of Annotation.builder_t
        | Undefined of int
      val get : t -> unnamed_union_t
      val file_set : t -> unit
      val id_get : t -> Uint64.t
      val id_get_int_exn : t -> int
      val displayName_get : t -> string
      val displayNamePrefixLength_get : t -> Uint32.t
      val displayNamePrefixLength_get_int_exn : t -> int
      val scopeId_get : t -> Uint64.t
      val scopeId_get_int_exn : t -> int
      val nestedNodes_get : t -> (rw, NestedNode.builder_t, builder_array_t) Runtime.Array.t
      val annotations_get : t -> (rw, Annotation.builder_t, builder_array_t) Runtime.Array.t
      val id_set : t -> Uint64.t -> unit
      val id_set_int_exn : t -> int -> unit
      val displayName_set : t -> string -> unit
      val displayNamePrefixLength_set : t -> Uint32.t -> unit
      val displayNamePrefixLength_set_int_exn : t -> int -> unit
      val scopeId_set : t -> Uint64.t -> unit
      val scopeId_set_int_exn : t -> int -> unit
      val nestedNodes_set : t -> (rw, NestedNode.builder_t, builder_array_t) Runtime.Array.t -> (rw, NestedNode.builder_t, builder_array_t) Runtime.Array.t
      val nestedNodes_init : t -> int -> (rw, NestedNode.builder_t, builder_array_t) Runtime.Array.t
      val annotations_set : t -> (rw, Annotation.builder_t, builder_array_t) Runtime.Array.t -> (rw, Annotation.builder_t, builder_array_t) Runtime.Array.t
      val annotations_init : t -> int -> (rw, Annotation.builder_t, builder_array_t) Runtime.Array.t
      val of_message : rw message_t -> t
    end
  end
  module CodeGeneratorRequest : sig
    type reader_t
    type builder_t
    type reader_t_CodeGeneratorRequest_13818529054586492878 = reader_t
    type builder_t_CodeGeneratorRequest_13818529054586492878 = builder_t
    module RequestedFile : sig
      type reader_t
      type builder_t
      type reader_t_RequestedFile_14981803260258615394 = reader_t
      type builder_t_RequestedFile_14981803260258615394 = builder_t
      module Import : sig
        type reader_t
        type builder_t
        type reader_t_Import_12560611460656617445 = reader_t
        type builder_t_Import_12560611460656617445 = builder_t
        module R : sig
          type t = reader_t
          val id_get : t -> Uint64.t
          val id_get_int_exn : t -> int
          val name_get : t -> string
          val of_message : 'cap message_t -> t
        end
        module B : sig
          type t = builder_t
          val id_get : t -> Uint64.t
          val id_get_int_exn : t -> int
          val name_get : t -> string
          val id_set : t -> Uint64.t -> unit
          val id_set_int_exn : t -> int -> unit
          val name_set : t -> string -> unit
          val of_message : rw message_t -> t
        end
      end
      module R : sig
        type t = reader_t
        val id_get : t -> Uint64.t
        val id_get_int_exn : t -> int
        val filename_get : t -> string
        val imports_get : t -> (ro, Import.reader_t, reader_array_t) Runtime.Array.t
        val of_message : 'cap message_t -> t
      end
      module B : sig
        type t = builder_t
        val id_get : t -> Uint64.t
        val id_get_int_exn : t -> int
        val filename_get : t -> string
        val imports_get : t -> (rw, Import.builder_t, builder_array_t) Runtime.Array.t
        val id_set : t -> Uint64.t -> unit
        val id_set_int_exn : t -> int -> unit
        val filename_set : t -> string -> unit
        val imports_set : t -> (rw, Import.builder_t, builder_array_t) Runtime.Array.t -> (rw, Import.builder_t, builder_array_t) Runtime.Array.t
        val imports_init : t -> int -> (rw, Import.builder_t, builder_array_t) Runtime.Array.t
        val of_message : rw message_t -> t
      end
    end
    module R : sig
      type t = reader_t
      val nodes_get : t -> (ro, Node.reader_t, reader_array_t) Runtime.Array.t
      val requestedFiles_get : t -> (ro, RequestedFile.reader_t, reader_array_t) Runtime.Array.t
      val of_message : 'cap message_t -> t
    end
    module B : sig
      type t = builder_t
      val nodes_get : t -> (rw, Node.builder_t, builder_array_t) Runtime.Array.t
      val requestedFiles_get : t -> (rw, RequestedFile.builder_t, builder_array_t) Runtime.Array.t
      val nodes_set : t -> (rw, Node.builder_t, builder_array_t) Runtime.Array.t -> (rw, Node.builder_t, builder_array_t) Runtime.Array.t
      val nodes_init : t -> int -> (rw, Node.builder_t, builder_array_t) Runtime.Array.t
      val requestedFiles_set : t -> (rw, RequestedFile.builder_t, builder_array_t) Runtime.Array.t -> (rw, RequestedFile.builder_t, builder_array_t) Runtime.Array.t
      val requestedFiles_init : t -> int -> (rw, RequestedFile.builder_t, builder_array_t) Runtime.Array.t
      val of_message : rw message_t -> t
    end
  end
end

module Make (MessageWrapper : Message.S) = struct
  let invalid_msg = Message.invalid_msg

  module RA_ = MessageReader.Make(MessageWrapper)
  module BA_ = MessageBuilder.Make(MessageWrapper)

  type 'cap message_t = 'cap MessageWrapper.Message.t

  type reader_array_t = ro RA_.ListStorage.t
  type builder_array_t = rw RA_.ListStorage.t

  module AnyPointer = struct
    type reader_t = ro MessageWrapper.Slice.t option
    type builder_t = rw MessageWrapper.Slice.t
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
      | Undefined of int
  end
  module Type = struct
    type reader_t = ro RA_.StructStorage.t option
    type builder_t = rw RA_.StructStorage.t
    type reader_t_Type_15020482145304562784 = reader_t
    type builder_t_Type_15020482145304562784 = builder_t
    module Enum = struct
      type reader_t = ro RA_.StructStorage.t option
      type builder_t = rw RA_.StructStorage.t
      type reader_t_Enum_11389172934837766057 = reader_t
      type builder_t_Enum_11389172934837766057 = builder_t
      module R = struct
        type t = reader_t
        let typeId_get x =
          RA_.get_data_field x ~f:(RA_.get_uint64 ~default:Uint64.zero ~byte_ofs:8)
        let typeId_get_int_exn x =
          Uint64.to_int (typeId_get x)
        let of_message x = RA_.get_root_struct (RA_.Message.readonly x)
      end
      module B = struct
        type t = builder_t
        let typeId_get x =
          BA_.get_data_field x ~f:(BA_.get_uint64 ~default:Uint64.zero ~byte_ofs:8)
        let typeId_get_int_exn x =
          Uint64.to_int (typeId_get x)
        let typeId_set x v =
          BA_.get_data_field x ~f:(BA_.set_uint64 ~default:Uint64.zero ~byte_ofs:8 v)
        let typeId_set_int_exn x v = typeId_set x (Uint64.of_int v)
        let of_message x = BA_.get_root_struct ~data_words:2 ~pointer_words:1 x
      end
    end
    module Interface = struct
      type reader_t = ro RA_.StructStorage.t option
      type builder_t = rw RA_.StructStorage.t
      type reader_t_Interface_17116997365232503999 = reader_t
      type builder_t_Interface_17116997365232503999 = builder_t
      module R = struct
        type t = reader_t
        let typeId_get x =
          RA_.get_data_field x ~f:(RA_.get_uint64 ~default:Uint64.zero ~byte_ofs:8)
        let typeId_get_int_exn x =
          Uint64.to_int (typeId_get x)
        let of_message x = RA_.get_root_struct (RA_.Message.readonly x)
      end
      module B = struct
        type t = builder_t
        let typeId_get x =
          BA_.get_data_field x ~f:(BA_.get_uint64 ~default:Uint64.zero ~byte_ofs:8)
        let typeId_get_int_exn x =
          Uint64.to_int (typeId_get x)
        let typeId_set x v =
          BA_.get_data_field x ~f:(BA_.set_uint64 ~default:Uint64.zero ~byte_ofs:8 v)
        let typeId_set_int_exn x v = typeId_set x (Uint64.of_int v)
        let of_message x = BA_.get_root_struct ~data_words:2 ~pointer_words:1 x
      end
    end
    module List = struct
      type reader_t = ro RA_.StructStorage.t option
      type builder_t = rw RA_.StructStorage.t
      type reader_t_List_9792858745991129751 = reader_t
      type builder_t_List_9792858745991129751 = builder_t
      module R = struct
        type t = reader_t
        let elementType_get x =
          RA_.get_pointer_field x 0 ~f:RA_.get_struct
        let of_message x = RA_.get_root_struct (RA_.Message.readonly x)
      end
      module B = struct
        type t = builder_t
        let elementType_get x =
          BA_.get_pointer_field x 0 ~f:(BA_.get_struct ~data_words:2 ~pointer_words:1)
        let elementType_set_reader x v =
          BA_.get_pointer_field x 0 ~f:(BA_.set_struct ~data_words:2 ~pointer_words:1 v)
        let elementType_set_builder x v =
          BA_.get_pointer_field x 0 ~f:(BA_.set_struct ~data_words:2 ~pointer_words:1 (Some v))
        let elementType_init x =
          BA_.get_pointer_field x 0 ~f:(BA_.init_struct ~data_words:2 ~pointer_words:1)
        let of_message x = BA_.get_root_struct ~data_words:2 ~pointer_words:1 x
      end
    end
    module Struct = struct
      type reader_t = ro RA_.StructStorage.t option
      type builder_t = rw RA_.StructStorage.t
      type reader_t_Struct_12410354185295152851 = reader_t
      type builder_t_Struct_12410354185295152851 = builder_t
      module R = struct
        type t = reader_t
        let typeId_get x =
          RA_.get_data_field x ~f:(RA_.get_uint64 ~default:Uint64.zero ~byte_ofs:8)
        let typeId_get_int_exn x =
          Uint64.to_int (typeId_get x)
        let of_message x = RA_.get_root_struct (RA_.Message.readonly x)
      end
      module B = struct
        type t = builder_t
        let typeId_get x =
          BA_.get_data_field x ~f:(BA_.get_uint64 ~default:Uint64.zero ~byte_ofs:8)
        let typeId_get_int_exn x =
          Uint64.to_int (typeId_get x)
        let typeId_set x v =
          BA_.get_data_field x ~f:(BA_.set_uint64 ~default:Uint64.zero ~byte_ofs:8 v)
        let typeId_set_int_exn x v = typeId_set x (Uint64.of_int v)
        let of_message x = BA_.get_root_struct ~data_words:2 ~pointer_words:1 x
      end
    end
    module R = struct
      type t = reader_t
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
      let anyPointer_get x = ()
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
        | List of List.reader_t
        | Enum of Enum.reader_t
        | Struct of Struct.reader_t
        | Interface of Interface.reader_t
        | AnyPointer
        | Undefined of int
      let get x =
        match RA_.get_data_field x ~f:(RA_.get_uint16 ~default:0 ~byte_ofs:0) with
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
        | 18 -> AnyPointer
        | v -> Undefined v
      let of_message x = RA_.get_root_struct (RA_.Message.readonly x)
    end
    module B = struct
      type t = builder_t
      let void_get x = ()
      let void_set x =
        BA_.get_data_field ~discr:{BA_.Discr.value=0; BA_.Discr.byte_ofs=0} x ~f:BA_.set_void
      let bool_get x = ()
      let bool_set x =
        BA_.get_data_field ~discr:{BA_.Discr.value=1; BA_.Discr.byte_ofs=0} x ~f:BA_.set_void
      let int8_get x = ()
      let int8_set x =
        BA_.get_data_field ~discr:{BA_.Discr.value=2; BA_.Discr.byte_ofs=0} x ~f:BA_.set_void
      let int16_get x = ()
      let int16_set x =
        BA_.get_data_field ~discr:{BA_.Discr.value=3; BA_.Discr.byte_ofs=0} x ~f:BA_.set_void
      let int32_get x = ()
      let int32_set x =
        BA_.get_data_field ~discr:{BA_.Discr.value=4; BA_.Discr.byte_ofs=0} x ~f:BA_.set_void
      let int64_get x = ()
      let int64_set x =
        BA_.get_data_field ~discr:{BA_.Discr.value=5; BA_.Discr.byte_ofs=0} x ~f:BA_.set_void
      let uint8_get x = ()
      let uint8_set x =
        BA_.get_data_field ~discr:{BA_.Discr.value=6; BA_.Discr.byte_ofs=0} x ~f:BA_.set_void
      let uint16_get x = ()
      let uint16_set x =
        BA_.get_data_field ~discr:{BA_.Discr.value=7; BA_.Discr.byte_ofs=0} x ~f:BA_.set_void
      let uint32_get x = ()
      let uint32_set x =
        BA_.get_data_field ~discr:{BA_.Discr.value=8; BA_.Discr.byte_ofs=0} x ~f:BA_.set_void
      let uint64_get x = ()
      let uint64_set x =
        BA_.get_data_field ~discr:{BA_.Discr.value=9; BA_.Discr.byte_ofs=0} x ~f:BA_.set_void
      let float32_get x = ()
      let float32_set x =
        BA_.get_data_field ~discr:{BA_.Discr.value=10; BA_.Discr.byte_ofs=0} x ~f:BA_.set_void
      let float64_get x = ()
      let float64_set x =
        BA_.get_data_field ~discr:{BA_.Discr.value=11; BA_.Discr.byte_ofs=0} x ~f:BA_.set_void
      let text_get x = ()
      let text_set x =
        BA_.get_data_field ~discr:{BA_.Discr.value=12; BA_.Discr.byte_ofs=0} x ~f:BA_.set_void
      let data_get x = ()
      let data_set x =
        BA_.get_data_field ~discr:{BA_.Discr.value=13; BA_.Discr.byte_ofs=0} x ~f:BA_.set_void
      let list_get x = x
      let enum_get x = x
      let struct_get x = x
      let interface_get x = x
      let anyPointer_get x = ()
      let anyPointer_set x =
        BA_.get_data_field ~discr:{BA_.Discr.value=18; BA_.Discr.byte_ofs=0} x ~f:BA_.set_void
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
        | List of List.builder_t
        | Enum of Enum.builder_t
        | Struct of Struct.builder_t
        | Interface of Interface.builder_t
        | AnyPointer
        | Undefined of int
      let get x =
        match BA_.get_data_field x ~f:(BA_.get_uint16 ~default:0 ~byte_ofs:0) with
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
        | 18 -> AnyPointer
        | v -> Undefined v
      let of_message x = BA_.get_root_struct ~data_words:2 ~pointer_words:1 x
    end
  end
  module Value = struct
    type reader_t = ro RA_.StructStorage.t option
    type builder_t = rw RA_.StructStorage.t
    type reader_t_Value_14853958794117909659 = reader_t
    type builder_t_Value_14853958794117909659 = builder_t
    module R = struct
      type t = reader_t
      let void_get x = ()
      let bool_get x =
        RA_.get_data_field x ~f:(RA_.get_bit ~default:false ~byte_ofs:2 ~bit_ofs:0)
      let int8_get x =
        RA_.get_data_field x ~f:(RA_.get_int8 ~default:0 ~byte_ofs:2)
      let int16_get x =
        RA_.get_data_field x ~f:(RA_.get_int16 ~default:0 ~byte_ofs:2)
      let int32_get x =
        RA_.get_data_field x ~f:(RA_.get_int32 ~default:0l ~byte_ofs:4)
      let int32_get_int_exn x =
        Int32.to_int (int32_get x)
      let int64_get x =
        RA_.get_data_field x ~f:(RA_.get_int64 ~default:0L ~byte_ofs:8)
      let int64_get_int_exn x =
        Int64.to_int (int64_get x)
      let uint8_get x =
        RA_.get_data_field x ~f:(RA_.get_uint8 ~default:0 ~byte_ofs:2)
      let uint16_get x =
        RA_.get_data_field x ~f:(RA_.get_uint16 ~default:0 ~byte_ofs:2)
      let uint32_get x =
        RA_.get_data_field x ~f:(RA_.get_uint32 ~default:Uint32.zero ~byte_ofs:4)
      let uint32_get_int_exn x =
        Uint32.to_int (uint32_get x)
      let uint64_get x =
        RA_.get_data_field x ~f:(RA_.get_uint64 ~default:Uint64.zero ~byte_ofs:8)
      let uint64_get_int_exn x =
        Uint64.to_int (uint64_get x)
      let float32_get x =
        RA_.get_data_field x ~f:(RA_.get_float32 ~default_bits:0l ~byte_ofs:4)
      let float64_get x =
        RA_.get_data_field x ~f:(RA_.get_float64 ~default_bits:0L ~byte_ofs:8)
      let text_get x =
        RA_.get_pointer_field x 0 ~f:(RA_.get_text ~default:"")
      let data_get x =
        RA_.get_pointer_field x 0 ~f:(RA_.get_blob ~default:"")
      let list_get x =
        RA_.get_pointer_field x 0 ~f:(fun x -> x)
      let enum_get x =
        RA_.get_data_field x ~f:(RA_.get_uint16 ~default:0 ~byte_ofs:2)
      let struct_get x =
        RA_.get_pointer_field x 0 ~f:(fun x -> x)
      let interface_get x = ()
      let anyPointer_get x =
        RA_.get_pointer_field x 0 ~f:(fun x -> x)
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
        | List of AnyPointer.reader_t
        | Enum of int
        | Struct of AnyPointer.reader_t
        | Interface
        | AnyPointer of AnyPointer.reader_t
        | Undefined of int
      let get x =
        match RA_.get_data_field x ~f:(RA_.get_uint16 ~default:0 ~byte_ofs:0) with
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
        | 18 -> AnyPointer (anyPointer_get x)
        | v -> Undefined v
      let of_message x = RA_.get_root_struct (RA_.Message.readonly x)
    end
    module B = struct
      type t = builder_t
      let void_get x = ()
      let void_set x =
        BA_.get_data_field ~discr:{BA_.Discr.value=0; BA_.Discr.byte_ofs=0} x ~f:BA_.set_void
      let bool_get x =
        BA_.get_data_field x ~f:(BA_.get_bit ~default:false ~byte_ofs:2 ~bit_ofs:0)
      let bool_set x v =
        BA_.get_data_field ~discr:{BA_.Discr.value=1; BA_.Discr.byte_ofs=0} x ~f:(BA_.set_bit ~default:false ~byte_ofs:2 ~bit_ofs:0 v)
      let int8_get x =
        BA_.get_data_field x ~f:(BA_.get_int8 ~default:0 ~byte_ofs:2)
      let int8_set_exn x v =
        BA_.get_data_field ~discr:{BA_.Discr.value=2; BA_.Discr.byte_ofs=0} x ~f:(BA_.set_int8 ~default:0 ~byte_ofs:2 v)
      let int16_get x =
        BA_.get_data_field x ~f:(BA_.get_int16 ~default:0 ~byte_ofs:2)
      let int16_set_exn x v =
        BA_.get_data_field ~discr:{BA_.Discr.value=3; BA_.Discr.byte_ofs=0} x ~f:(BA_.set_int16 ~default:0 ~byte_ofs:2 v)
      let int32_get x =
        BA_.get_data_field x ~f:(BA_.get_int32 ~default:0l ~byte_ofs:4)
      let int32_get_int_exn x =
        Int32.to_int (int32_get x)
      let int32_set x v =
        BA_.get_data_field ~discr:{BA_.Discr.value=4; BA_.Discr.byte_ofs=0} x ~f:(BA_.set_int32 ~default:0l ~byte_ofs:4 v)
      let int32_set_int_exn x v = int32_set x (Int32.of_int v)
      let int64_get x =
        BA_.get_data_field x ~f:(BA_.get_int64 ~default:0L ~byte_ofs:8)
      let int64_get_int_exn x =
        Int64.to_int (int64_get x)
      let int64_set x v =
        BA_.get_data_field ~discr:{BA_.Discr.value=5; BA_.Discr.byte_ofs=0} x ~f:(BA_.set_int64 ~default:0L ~byte_ofs:8 v)
      let int64_set_int_exn x v = int64_set x (Int64.of_int v)
      let uint8_get x =
        BA_.get_data_field x ~f:(BA_.get_uint8 ~default:0 ~byte_ofs:2)
      let uint8_set_exn x v =
        BA_.get_data_field ~discr:{BA_.Discr.value=6; BA_.Discr.byte_ofs=0} x ~f:(BA_.set_uint8 ~default:0 ~byte_ofs:2 v)
      let uint16_get x =
        BA_.get_data_field x ~f:(BA_.get_uint16 ~default:0 ~byte_ofs:2)
      let uint16_set_exn x v =
        BA_.get_data_field ~discr:{BA_.Discr.value=7; BA_.Discr.byte_ofs=0} x ~f:(BA_.set_uint16 ~default:0 ~byte_ofs:2 v)
      let uint32_get x =
        BA_.get_data_field x ~f:(BA_.get_uint32 ~default:Uint32.zero ~byte_ofs:4)
      let uint32_get_int_exn x =
        Uint32.to_int (uint32_get x)
      let uint32_set x v =
        BA_.get_data_field ~discr:{BA_.Discr.value=8; BA_.Discr.byte_ofs=0} x ~f:(BA_.set_uint32 ~default:Uint32.zero ~byte_ofs:4 v)
      let uint32_set_int_exn x v = uint32_set x (Uint32.of_int v)
      let uint64_get x =
        BA_.get_data_field x ~f:(BA_.get_uint64 ~default:Uint64.zero ~byte_ofs:8)
      let uint64_get_int_exn x =
        Uint64.to_int (uint64_get x)
      let uint64_set x v =
        BA_.get_data_field ~discr:{BA_.Discr.value=9; BA_.Discr.byte_ofs=0} x ~f:(BA_.set_uint64 ~default:Uint64.zero ~byte_ofs:8 v)
      let uint64_set_int_exn x v = uint64_set x (Uint64.of_int v)
      let float32_get x =
        BA_.get_data_field x ~f:(BA_.get_float32 ~default_bits:0l ~byte_ofs:4)
      let float32_set x v =
        BA_.get_data_field ~discr:{BA_.Discr.value=10; BA_.Discr.byte_ofs=0} x ~f:(BA_.set_float32 ~default_bits:0l ~byte_ofs:4 v)
      let float64_get x =
        BA_.get_data_field x ~f:(BA_.get_float64 ~default_bits:0L ~byte_ofs:8)
      let float64_set x v =
      BA_.get_data_field ~discr:{BA_.Discr.value=11; BA_.Discr.byte_ofs=0} x ~f:(BA_.set_float64 ~default_bits:0L ~byte_ofs:8 v)
      let text_get x =
        BA_.get_pointer_field x 0 ~f:(BA_.get_text ~default:"")
      let text_set x v =
        BA_.get_pointer_field ~discr:{BA_.Discr.value=12; BA_.Discr.byte_ofs=0} x 0 ~f:(BA_.set_text v)
      let data_get x =
        BA_.get_pointer_field x 0 ~f:(BA_.get_blob ~default:"")
      let data_set x v =
        BA_.get_pointer_field ~discr:{BA_.Discr.value=13; BA_.Discr.byte_ofs=0} x 0 ~f:(BA_.set_blob v)
      let list_get x =
        BA_.get_pointer_field x 0 ~f:(fun x -> x)
      let list_set x v = failwith "not implemented"
      let enum_get x =
        BA_.get_data_field x ~f:(BA_.get_uint16 ~default:0 ~byte_ofs:2)
      let enum_set_exn x v =
        BA_.get_data_field ~discr:{BA_.Discr.value=15; BA_.Discr.byte_ofs=0} x ~f:(BA_.set_uint16 ~default:0 ~byte_ofs:2 v)
      let struct_get x =
        BA_.get_pointer_field x 0 ~f:(fun x -> x)
      let struct_set x v = failwith "not implemented"
      let interface_get x = ()
      let interface_set x =
        BA_.get_data_field ~discr:{BA_.Discr.value=17; BA_.Discr.byte_ofs=0} x ~f:BA_.set_void
      let anyPointer_get x =
        BA_.get_pointer_field x 0 ~f:(fun x -> x)
      let anyPointer_set x v = failwith "not implemented"
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
        | List of AnyPointer.builder_t
        | Enum of int
        | Struct of AnyPointer.builder_t
        | Interface
        | AnyPointer of AnyPointer.builder_t
        | Undefined of int
      let get x =
        match BA_.get_data_field x ~f:(BA_.get_uint16 ~default:0 ~byte_ofs:0) with
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
        | 18 -> AnyPointer (anyPointer_get x)
        | v -> Undefined v
      let of_message x = BA_.get_root_struct ~data_words:2 ~pointer_words:1 x
    end
  end
  module Annotation = struct
    type reader_t = ro RA_.StructStorage.t option
    type builder_t = rw RA_.StructStorage.t
    type reader_t_Annotation_17422339044421236034 = reader_t
    type builder_t_Annotation_17422339044421236034 = builder_t
    module R = struct
      type t = reader_t
      let id_get x =
        RA_.get_data_field x ~f:(RA_.get_uint64 ~default:Uint64.zero ~byte_ofs:0)
      let id_get_int_exn x =
        Uint64.to_int (id_get x)
      let value_get x =
        RA_.get_pointer_field x 0 ~f:RA_.get_struct
      let of_message x = RA_.get_root_struct (RA_.Message.readonly x)
    end
    module B = struct
      type t = builder_t
      let id_get x =
        BA_.get_data_field x ~f:(BA_.get_uint64 ~default:Uint64.zero ~byte_ofs:0)
      let id_get_int_exn x =
        Uint64.to_int (id_get x)
      let id_set x v =
        BA_.get_data_field x ~f:(BA_.set_uint64 ~default:Uint64.zero ~byte_ofs:0 v)
      let id_set_int_exn x v = id_set x (Uint64.of_int v)
      let value_get x =
        BA_.get_pointer_field x 0 ~f:(BA_.get_struct ~data_words:2 ~pointer_words:1)
      let value_set_reader x v =
        BA_.get_pointer_field x 0 ~f:(BA_.set_struct ~data_words:2 ~pointer_words:1 v)
      let value_set_builder x v =
        BA_.get_pointer_field x 0 ~f:(BA_.set_struct ~data_words:2 ~pointer_words:1 (Some v))
      let value_init x =
        BA_.get_pointer_field x 0 ~f:(BA_.init_struct ~data_words:2 ~pointer_words:1)
      let of_message x = BA_.get_root_struct ~data_words:1 ~pointer_words:1 x
    end
  end
  module Method = struct
    type reader_t = ro RA_.StructStorage.t option
    type builder_t = rw RA_.StructStorage.t
    type reader_t_Method_10736806783679155584 = reader_t
    type builder_t_Method_10736806783679155584 = builder_t
    module R = struct
      type t = reader_t
      let name_get x =
        RA_.get_pointer_field x 0 ~f:(RA_.get_text ~default:"")
      let codeOrder_get x =
        RA_.get_data_field x ~f:(RA_.get_uint16 ~default:0 ~byte_ofs:0)
      let paramStructType_get x =
        RA_.get_data_field x ~f:(RA_.get_uint64 ~default:Uint64.zero ~byte_ofs:8)
      let paramStructType_get_int_exn x =
        Uint64.to_int (paramStructType_get x)
      let resultStructType_get x =
        RA_.get_data_field x ~f:(RA_.get_uint64 ~default:Uint64.zero ~byte_ofs:16)
      let resultStructType_get_int_exn x =
        Uint64.to_int (resultStructType_get x)
      let annotations_get x =
        RA_.get_pointer_field x 1 ~f:RA_.get_struct_list
      let of_message x = RA_.get_root_struct (RA_.Message.readonly x)
    end
    module B = struct
      type t = builder_t
      let name_get x =
        BA_.get_pointer_field x 0 ~f:(BA_.get_text ~default:"")
      let name_set x v =
        BA_.get_pointer_field x 0 ~f:(BA_.set_text v)
      let codeOrder_get x =
        BA_.get_data_field x ~f:(BA_.get_uint16 ~default:0 ~byte_ofs:0)
      let codeOrder_set_exn x v =
        BA_.get_data_field x ~f:(BA_.set_uint16 ~default:0 ~byte_ofs:0 v)
      let paramStructType_get x =
        BA_.get_data_field x ~f:(BA_.get_uint64 ~default:Uint64.zero ~byte_ofs:8)
      let paramStructType_get_int_exn x =
        Uint64.to_int (paramStructType_get x)
      let paramStructType_set x v =
        BA_.get_data_field x ~f:(BA_.set_uint64 ~default:Uint64.zero ~byte_ofs:8 v)
      let paramStructType_set_int_exn x v = paramStructType_set x (Uint64.of_int v)
      let resultStructType_get x =
        BA_.get_data_field x ~f:(BA_.get_uint64 ~default:Uint64.zero ~byte_ofs:16)
      let resultStructType_get_int_exn x =
        Uint64.to_int (resultStructType_get x)
      let resultStructType_set x v =
        BA_.get_data_field x ~f:(BA_.set_uint64 ~default:Uint64.zero ~byte_ofs:16 v)
      let resultStructType_set_int_exn x v = resultStructType_set x (Uint64.of_int v)
      let annotations_get x =
        BA_.get_pointer_field x 1 ~f:(BA_.get_struct_list ~data_words:1 ~pointer_words:1)
      let annotations_set x v =
        BA_.get_pointer_field x 1 ~f:(BA_.set_struct_list ~data_words:1 ~pointer_words:1 v)
      let annotations_init x n =
        BA_.get_pointer_field x 1 ~f:(BA_.init_struct_list ~data_words:1 ~pointer_words:1 n)
      let of_message x = BA_.get_root_struct ~data_words:3 ~pointer_words:2 x
    end
  end
  module Enumerant = struct
    type reader_t = ro RA_.StructStorage.t option
    type builder_t = rw RA_.StructStorage.t
    type reader_t_Enumerant_10919677598968879693 = reader_t
    type builder_t_Enumerant_10919677598968879693 = builder_t
    module R = struct
      type t = reader_t
      let name_get x =
        RA_.get_pointer_field x 0 ~f:(RA_.get_text ~default:"")
      let codeOrder_get x =
        RA_.get_data_field x ~f:(RA_.get_uint16 ~default:0 ~byte_ofs:0)
      let annotations_get x =
        RA_.get_pointer_field x 1 ~f:RA_.get_struct_list
      let of_message x = RA_.get_root_struct (RA_.Message.readonly x)
    end
    module B = struct
      type t = builder_t
      let name_get x =
        BA_.get_pointer_field x 0 ~f:(BA_.get_text ~default:"")
      let name_set x v =
        BA_.get_pointer_field x 0 ~f:(BA_.set_text v)
      let codeOrder_get x =
        BA_.get_data_field x ~f:(BA_.get_uint16 ~default:0 ~byte_ofs:0)
      let codeOrder_set_exn x v =
        BA_.get_data_field x ~f:(BA_.set_uint16 ~default:0 ~byte_ofs:0 v)
      let annotations_get x =
        BA_.get_pointer_field x 1 ~f:(BA_.get_struct_list ~data_words:1 ~pointer_words:1)
      let annotations_set x v =
        BA_.get_pointer_field x 1 ~f:(BA_.set_struct_list ~data_words:1 ~pointer_words:1 v)
      let annotations_init x n =
        BA_.get_pointer_field x 1 ~f:(BA_.init_struct_list ~data_words:1 ~pointer_words:1 n)
      let of_message x = BA_.get_root_struct ~data_words:1 ~pointer_words:2 x
    end
  end
  module Field = struct
    type reader_t = ro RA_.StructStorage.t option
    type builder_t = rw RA_.StructStorage.t
    type reader_t_Field_11145653318641710175 = reader_t
    type builder_t_Field_11145653318641710175 = builder_t
    let noDiscriminant = 65535
    module Ordinal = struct
      type reader_t = ro RA_.StructStorage.t option
      type builder_t = rw RA_.StructStorage.t
      type reader_t_Ordinal_13515537513213004774 = reader_t
      type builder_t_Ordinal_13515537513213004774 = builder_t
      module R = struct
        type t = reader_t
        let implicit_get x = ()
        let explicit_get x =
          RA_.get_data_field x ~f:(RA_.get_uint16 ~default:0 ~byte_ofs:12)
        type unnamed_union_t =
          | Implicit
          | Explicit of int
          | Undefined of int
        let get x =
          match RA_.get_data_field x ~f:(RA_.get_uint16 ~default:0 ~byte_ofs:10) with
          | 0 -> Implicit
          | 1 -> Explicit (explicit_get x)
          | v -> Undefined v
        let of_message x = RA_.get_root_struct (RA_.Message.readonly x)
      end
      module B = struct
        type t = builder_t
        let implicit_get x = ()
        let implicit_set x =
          BA_.get_data_field ~discr:{BA_.Discr.value=0; BA_.Discr.byte_ofs=10} x ~f:BA_.set_void
        let explicit_get x =
          BA_.get_data_field x ~f:(BA_.get_uint16 ~default:0 ~byte_ofs:12)
        let explicit_set_exn x v =
          BA_.get_data_field ~discr:{BA_.Discr.value=1; BA_.Discr.byte_ofs=10} x ~f:(BA_.set_uint16 ~default:0 ~byte_ofs:12 v)
        type unnamed_union_t =
          | Implicit
          | Explicit of int
          | Undefined of int
        let get x =
          match BA_.get_data_field x ~f:(BA_.get_uint16 ~default:0 ~byte_ofs:10) with
          | 0 -> Implicit
          | 1 -> Explicit (explicit_get x)
          | v -> Undefined v
        let of_message x = BA_.get_root_struct ~data_words:3 ~pointer_words:4 x
      end
    end
    module Group = struct
      type reader_t = ro RA_.StructStorage.t option
      type builder_t = rw RA_.StructStorage.t
      type reader_t_Group_14626792032033250577 = reader_t
      type builder_t_Group_14626792032033250577 = builder_t
      module R = struct
        type t = reader_t
        let typeId_get x =
          RA_.get_data_field x ~f:(RA_.get_uint64 ~default:Uint64.zero ~byte_ofs:16)
        let typeId_get_int_exn x =
          Uint64.to_int (typeId_get x)
        let of_message x = RA_.get_root_struct (RA_.Message.readonly x)
      end
      module B = struct
        type t = builder_t
        let typeId_get x =
          BA_.get_data_field x ~f:(BA_.get_uint64 ~default:Uint64.zero ~byte_ofs:16)
        let typeId_get_int_exn x =
          Uint64.to_int (typeId_get x)
        let typeId_set x v =
          BA_.get_data_field x ~f:(BA_.set_uint64 ~default:Uint64.zero ~byte_ofs:16 v)
        let typeId_set_int_exn x v = typeId_set x (Uint64.of_int v)
        let of_message x = BA_.get_root_struct ~data_words:3 ~pointer_words:4 x
      end
    end
    module Slot = struct
      type reader_t = ro RA_.StructStorage.t option
      type builder_t = rw RA_.StructStorage.t
      type reader_t_Slot_14133145859926553711 = reader_t
      type builder_t_Slot_14133145859926553711 = builder_t
      module R = struct
        type t = reader_t
        let offset_get x =
          RA_.get_data_field x ~f:(RA_.get_uint32 ~default:Uint32.zero ~byte_ofs:4)
        let offset_get_int_exn x =
          Uint32.to_int (offset_get x)
        let type_get x =
          RA_.get_pointer_field x 2 ~f:RA_.get_struct
        let defaultValue_get x =
          RA_.get_pointer_field x 3 ~f:RA_.get_struct
        let hadExplicitDefault_get x =
          RA_.get_data_field x ~f:(RA_.get_bit ~default:false ~byte_ofs:16 ~bit_ofs:0)
        let of_message x = RA_.get_root_struct (RA_.Message.readonly x)
      end
      module B = struct
        type t = builder_t
        let offset_get x =
          BA_.get_data_field x ~f:(BA_.get_uint32 ~default:Uint32.zero ~byte_ofs:4)
        let offset_get_int_exn x =
          Uint32.to_int (offset_get x)
        let offset_set x v =
          BA_.get_data_field x ~f:(BA_.set_uint32 ~default:Uint32.zero ~byte_ofs:4 v)
        let offset_set_int_exn x v = offset_set x (Uint32.of_int v)
        let type_get x =
          BA_.get_pointer_field x 2 ~f:(BA_.get_struct ~data_words:2 ~pointer_words:1)
        let type_set_reader x v =
          BA_.get_pointer_field x 2 ~f:(BA_.set_struct ~data_words:2 ~pointer_words:1 v)
        let type_set_builder x v =
          BA_.get_pointer_field x 2 ~f:(BA_.set_struct ~data_words:2 ~pointer_words:1 (Some v))
        let type_init x =
          BA_.get_pointer_field x 2 ~f:(BA_.init_struct ~data_words:2 ~pointer_words:1)
        let defaultValue_get x =
          BA_.get_pointer_field x 3 ~f:(BA_.get_struct ~data_words:2 ~pointer_words:1)
        let defaultValue_set_reader x v =
          BA_.get_pointer_field x 3 ~f:(BA_.set_struct ~data_words:2 ~pointer_words:1 v)
        let defaultValue_set_builder x v =
          BA_.get_pointer_field x 3 ~f:(BA_.set_struct ~data_words:2 ~pointer_words:1 (Some v))
        let defaultValue_init x =
          BA_.get_pointer_field x 3 ~f:(BA_.init_struct ~data_words:2 ~pointer_words:1)
        let hadExplicitDefault_get x =
          BA_.get_data_field x ~f:(BA_.get_bit ~default:false ~byte_ofs:16 ~bit_ofs:0)
        let hadExplicitDefault_set x v =
          BA_.get_data_field x ~f:(BA_.set_bit ~default:false ~byte_ofs:16 ~bit_ofs:0 v)
        let of_message x = BA_.get_root_struct ~data_words:3 ~pointer_words:4 x
      end
    end
    module R = struct
      type t = reader_t
      let name_get x =
        RA_.get_pointer_field x 0 ~f:(RA_.get_text ~default:"")
      let codeOrder_get x =
        RA_.get_data_field x ~f:(RA_.get_uint16 ~default:0 ~byte_ofs:0)
      let annotations_get x =
        RA_.get_pointer_field x 1 ~f:RA_.get_struct_list
      let discriminantValue_get x =
        RA_.get_data_field x ~f:(RA_.get_uint16 ~default:65535 ~byte_ofs:2)
      let ordinal_get x = x
      let slot_get x = x
      let group_get x = x
      type unnamed_union_t =
        | Slot of Slot.reader_t
        | Group of Group.reader_t
        | Undefined of int
      let get x =
        match RA_.get_data_field x ~f:(RA_.get_uint16 ~default:0 ~byte_ofs:8) with
        | 0 -> Slot (slot_get x)
        | 1 -> Group (group_get x)
        | v -> Undefined v
      let of_message x = RA_.get_root_struct (RA_.Message.readonly x)
    end
    module B = struct
      type t = builder_t
      let name_get x =
        BA_.get_pointer_field x 0 ~f:(BA_.get_text ~default:"")
      let name_set x v =
        BA_.get_pointer_field x 0 ~f:(BA_.set_text v)
      let codeOrder_get x =
        BA_.get_data_field x ~f:(BA_.get_uint16 ~default:0 ~byte_ofs:0)
      let codeOrder_set_exn x v =
        BA_.get_data_field x ~f:(BA_.set_uint16 ~default:0 ~byte_ofs:0 v)
      let annotations_get x =
        BA_.get_pointer_field x 1 ~f:(BA_.get_struct_list ~data_words:1 ~pointer_words:1)
      let annotations_set x v =
        BA_.get_pointer_field x 1 ~f:(BA_.set_struct_list ~data_words:1 ~pointer_words:1 v)
      let annotations_init x n =
        BA_.get_pointer_field x 1 ~f:(BA_.init_struct_list ~data_words:1 ~pointer_words:1 n)
      let discriminantValue_get x =
        BA_.get_data_field x ~f:(BA_.get_uint16 ~default:65535 ~byte_ofs:2)
      let discriminantValue_set_exn x v =
        BA_.get_data_field x ~f:(BA_.set_uint16 ~default:65535 ~byte_ofs:2 v)
      let ordinal_get x = x
      let slot_get x = x
      let group_get x = x
      type unnamed_union_t =
        | Slot of Slot.builder_t
        | Group of Group.builder_t
        | Undefined of int
      let get x =
        match BA_.get_data_field x ~f:(BA_.get_uint16 ~default:0 ~byte_ofs:8) with
        | 0 -> Slot (slot_get x)
        | 1 -> Group (group_get x)
        | v -> Undefined v
      let of_message x = BA_.get_root_struct ~data_words:3 ~pointer_words:4 x
    end
  end
  module Node = struct
    type reader_t = ro RA_.StructStorage.t option
    type builder_t = rw RA_.StructStorage.t
    type reader_t_Node_16610026722781537303 = reader_t
    type builder_t_Node_16610026722781537303 = builder_t
    module Struct = struct
      type reader_t = ro RA_.StructStorage.t option
      type builder_t = rw RA_.StructStorage.t
      type reader_t_Struct_11430331134483579957 = reader_t
      type builder_t_Struct_11430331134483579957 = builder_t
      module R = struct
        type t = reader_t
        let dataWordCount_get x =
          RA_.get_data_field x ~f:(RA_.get_uint16 ~default:0 ~byte_ofs:14)
        let pointerCount_get x =
          RA_.get_data_field x ~f:(RA_.get_uint16 ~default:0 ~byte_ofs:24)
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
              | v -> ElementSize.Undefined v)
          in
          let discr = RA_.get_data_field x ~f:(RA_.get_uint16 ~default:0 ~byte_ofs:26) in
          decode discr
        let isGroup_get x =
          RA_.get_data_field x ~f:(RA_.get_bit ~default:false ~byte_ofs:28 ~bit_ofs:0)
        let discriminantCount_get x =
          RA_.get_data_field x ~f:(RA_.get_uint16 ~default:0 ~byte_ofs:30)
        let discriminantOffset_get x =
          RA_.get_data_field x ~f:(RA_.get_uint32 ~default:Uint32.zero ~byte_ofs:32)
        let discriminantOffset_get_int_exn x =
          Uint32.to_int (discriminantOffset_get x)
        let fields_get x =
          RA_.get_pointer_field x 3 ~f:RA_.get_struct_list
        let of_message x = RA_.get_root_struct (RA_.Message.readonly x)
      end
      module B = struct
        type t = builder_t
        let dataWordCount_get x =
          BA_.get_data_field x ~f:(BA_.get_uint16 ~default:0 ~byte_ofs:14)
        let dataWordCount_set_exn x v =
          BA_.get_data_field x ~f:(BA_.set_uint16 ~default:0 ~byte_ofs:14 v)
        let pointerCount_get x =
          BA_.get_data_field x ~f:(BA_.get_uint16 ~default:0 ~byte_ofs:24)
        let pointerCount_set_exn x v =
          BA_.get_data_field x ~f:(BA_.set_uint16 ~default:0 ~byte_ofs:24 v)
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
              | v -> ElementSize.Undefined v)
          in
          let discr = BA_.get_data_field x ~f:(BA_.get_uint16 ~default:0 ~byte_ofs:26) in
          decode discr
        let preferredListEncoding_set x e =
          let encode =
            (fun enum -> match enum with
              | ElementSize.Empty -> 0
              | ElementSize.Bit -> 1
              | ElementSize.Byte -> 2
              | ElementSize.TwoBytes -> 3
              | ElementSize.FourBytes -> 4
              | ElementSize.EightBytes -> 5
              | ElementSize.Pointer -> 6
              | ElementSize.InlineComposite -> 7
              | ElementSize.Undefined _ ->
                  invalid_msg "Cannot encode undefined enum value.")
          in
          BA_.get_data_field x ~f:(BA_.set_uint16 ~default:0 ~byte_ofs:26 (encode e))
        let preferredListEncoding_set_unsafe x e =
          let encode =
            (fun enum -> match enum with
              | ElementSize.Empty -> 0
              | ElementSize.Bit -> 1
              | ElementSize.Byte -> 2
              | ElementSize.TwoBytes -> 3
              | ElementSize.FourBytes -> 4
              | ElementSize.EightBytes -> 5
              | ElementSize.Pointer -> 6
              | ElementSize.InlineComposite -> 7
             | ElementSize.Undefined x -> x)
          in
          BA_.get_data_field x ~f:(BA_.set_uint16 ~default:0 ~byte_ofs:26 (encode e))
        let isGroup_get x =
          BA_.get_data_field x ~f:(BA_.get_bit ~default:false ~byte_ofs:28 ~bit_ofs:0)
        let isGroup_set x v =
          BA_.get_data_field x ~f:(BA_.set_bit ~default:false ~byte_ofs:28 ~bit_ofs:0 v)
        let discriminantCount_get x =
          BA_.get_data_field x ~f:(BA_.get_uint16 ~default:0 ~byte_ofs:30)
        let discriminantCount_set_exn x v =
          BA_.get_data_field x ~f:(BA_.set_uint16 ~default:0 ~byte_ofs:30 v)
        let discriminantOffset_get x =
          BA_.get_data_field x ~f:(BA_.get_uint32 ~default:Uint32.zero ~byte_ofs:32)
        let discriminantOffset_get_int_exn x =
          Uint32.to_int (discriminantOffset_get x)
        let discriminantOffset_set x v =
          BA_.get_data_field x ~f:(BA_.set_uint32 ~default:Uint32.zero ~byte_ofs:32 v)
        let discriminantOffset_set_int_exn x v = discriminantOffset_set x (Uint32.of_int v)
        let fields_get x =
          BA_.get_pointer_field x 3 ~f:(BA_.get_struct_list ~data_words:3 ~pointer_words:4)
        let fields_set x v =
          BA_.get_pointer_field x 3 ~f:(BA_.set_struct_list ~data_words:3 ~pointer_words:4 v)
        let fields_init x n =
          BA_.get_pointer_field x 3 ~f:(BA_.init_struct_list ~data_words:3 ~pointer_words:4 n)
        let of_message x = BA_.get_root_struct ~data_words:5 ~pointer_words:5 x
      end
    end
    module Enum = struct
      type reader_t = ro RA_.StructStorage.t option
      type builder_t = rw RA_.StructStorage.t
      type reader_t_Enum_13063450714778629528 = reader_t
      type builder_t_Enum_13063450714778629528 = builder_t
      module R = struct
        type t = reader_t
        let enumerants_get x =
          RA_.get_pointer_field x 3 ~f:RA_.get_struct_list
        let of_message x = RA_.get_root_struct (RA_.Message.readonly x)
      end
      module B = struct
        type t = builder_t
        let enumerants_get x =
          BA_.get_pointer_field x 3 ~f:(BA_.get_struct_list ~data_words:1 ~pointer_words:2)
        let enumerants_set x v =
          BA_.get_pointer_field x 3 ~f:(BA_.set_struct_list ~data_words:1 ~pointer_words:2 v)
        let enumerants_init x n =
          BA_.get_pointer_field x 3 ~f:(BA_.init_struct_list ~data_words:1 ~pointer_words:2 n)
        let of_message x = BA_.get_root_struct ~data_words:5 ~pointer_words:5 x
      end
    end
    module Annotation = struct
      type reader_t = ro RA_.StructStorage.t option
      type builder_t = rw RA_.StructStorage.t
      type reader_t_Annotation_17011813041836786320 = reader_t
      type builder_t_Annotation_17011813041836786320 = builder_t
      module R = struct
        type t = reader_t
        let type_get x =
          RA_.get_pointer_field x 3 ~f:RA_.get_struct
        let targetsFile_get x =
          RA_.get_data_field x ~f:(RA_.get_bit ~default:false ~byte_ofs:14 ~bit_ofs:0)
        let targetsConst_get x =
          RA_.get_data_field x ~f:(RA_.get_bit ~default:false ~byte_ofs:14 ~bit_ofs:1)
        let targetsEnum_get x =
          RA_.get_data_field x ~f:(RA_.get_bit ~default:false ~byte_ofs:14 ~bit_ofs:2)
        let targetsEnumerant_get x =
          RA_.get_data_field x ~f:(RA_.get_bit ~default:false ~byte_ofs:14 ~bit_ofs:3)
        let targetsStruct_get x =
          RA_.get_data_field x ~f:(RA_.get_bit ~default:false ~byte_ofs:14 ~bit_ofs:4)
        let targetsField_get x =
          RA_.get_data_field x ~f:(RA_.get_bit ~default:false ~byte_ofs:14 ~bit_ofs:5)
        let targetsUnion_get x =
          RA_.get_data_field x ~f:(RA_.get_bit ~default:false ~byte_ofs:14 ~bit_ofs:6)
        let targetsGroup_get x =
          RA_.get_data_field x ~f:(RA_.get_bit ~default:false ~byte_ofs:14 ~bit_ofs:7)
        let targetsInterface_get x =
          RA_.get_data_field x ~f:(RA_.get_bit ~default:false ~byte_ofs:15 ~bit_ofs:0)
        let targetsMethod_get x =
          RA_.get_data_field x ~f:(RA_.get_bit ~default:false ~byte_ofs:15 ~bit_ofs:1)
        let targetsParam_get x =
          RA_.get_data_field x ~f:(RA_.get_bit ~default:false ~byte_ofs:15 ~bit_ofs:2)
        let targetsAnnotation_get x =
          RA_.get_data_field x ~f:(RA_.get_bit ~default:false ~byte_ofs:15 ~bit_ofs:3)
        let of_message x = RA_.get_root_struct (RA_.Message.readonly x)
      end
      module B = struct
        type t = builder_t
        let type_get x =
          BA_.get_pointer_field x 3 ~f:(BA_.get_struct ~data_words:2 ~pointer_words:1)
        let type_set_reader x v =
          BA_.get_pointer_field x 3 ~f:(BA_.set_struct ~data_words:2 ~pointer_words:1 v)
        let type_set_builder x v =
          BA_.get_pointer_field x 3 ~f:(BA_.set_struct ~data_words:2 ~pointer_words:1 (Some v))
        let type_init x =
          BA_.get_pointer_field x 3 ~f:(BA_.init_struct ~data_words:2 ~pointer_words:1)
        let targetsFile_get x =
          BA_.get_data_field x ~f:(BA_.get_bit ~default:false ~byte_ofs:14 ~bit_ofs:0)
        let targetsFile_set x v =
          BA_.get_data_field x ~f:(BA_.set_bit ~default:false ~byte_ofs:14 ~bit_ofs:0 v)
        let targetsConst_get x =
          BA_.get_data_field x ~f:(BA_.get_bit ~default:false ~byte_ofs:14 ~bit_ofs:1)
        let targetsConst_set x v =
          BA_.get_data_field x ~f:(BA_.set_bit ~default:false ~byte_ofs:14 ~bit_ofs:1 v)
        let targetsEnum_get x =
          BA_.get_data_field x ~f:(BA_.get_bit ~default:false ~byte_ofs:14 ~bit_ofs:2)
        let targetsEnum_set x v =
          BA_.get_data_field x ~f:(BA_.set_bit ~default:false ~byte_ofs:14 ~bit_ofs:2 v)
        let targetsEnumerant_get x =
          BA_.get_data_field x ~f:(BA_.get_bit ~default:false ~byte_ofs:14 ~bit_ofs:3)
        let targetsEnumerant_set x v =
          BA_.get_data_field x ~f:(BA_.set_bit ~default:false ~byte_ofs:14 ~bit_ofs:3 v)
        let targetsStruct_get x =
          BA_.get_data_field x ~f:(BA_.get_bit ~default:false ~byte_ofs:14 ~bit_ofs:4)
        let targetsStruct_set x v =
          BA_.get_data_field x ~f:(BA_.set_bit ~default:false ~byte_ofs:14 ~bit_ofs:4 v)
        let targetsField_get x =
          BA_.get_data_field x ~f:(BA_.get_bit ~default:false ~byte_ofs:14 ~bit_ofs:5)
        let targetsField_set x v =
          BA_.get_data_field x ~f:(BA_.set_bit ~default:false ~byte_ofs:14 ~bit_ofs:5 v)
        let targetsUnion_get x =
          BA_.get_data_field x ~f:(BA_.get_bit ~default:false ~byte_ofs:14 ~bit_ofs:6)
        let targetsUnion_set x v =
          BA_.get_data_field x ~f:(BA_.set_bit ~default:false ~byte_ofs:14 ~bit_ofs:6 v)
        let targetsGroup_get x =
          BA_.get_data_field x ~f:(BA_.get_bit ~default:false ~byte_ofs:14 ~bit_ofs:7)
        let targetsGroup_set x v =
          BA_.get_data_field x ~f:(BA_.set_bit ~default:false ~byte_ofs:14 ~bit_ofs:7 v)
        let targetsInterface_get x =
          BA_.get_data_field x ~f:(BA_.get_bit ~default:false ~byte_ofs:15 ~bit_ofs:0)
        let targetsInterface_set x v =
          BA_.get_data_field x ~f:(BA_.set_bit ~default:false ~byte_ofs:15 ~bit_ofs:0 v)
        let targetsMethod_get x =
          BA_.get_data_field x ~f:(BA_.get_bit ~default:false ~byte_ofs:15 ~bit_ofs:1)
        let targetsMethod_set x v =
          BA_.get_data_field x ~f:(BA_.set_bit ~default:false ~byte_ofs:15 ~bit_ofs:1 v)
        let targetsParam_get x =
          BA_.get_data_field x ~f:(BA_.get_bit ~default:false ~byte_ofs:15 ~bit_ofs:2)
        let targetsParam_set x v =
          BA_.get_data_field x ~f:(BA_.set_bit ~default:false ~byte_ofs:15 ~bit_ofs:2 v)
        let targetsAnnotation_get x =
          BA_.get_data_field x ~f:(BA_.get_bit ~default:false ~byte_ofs:15 ~bit_ofs:3)
        let targetsAnnotation_set x v =
          BA_.get_data_field x ~f:(BA_.set_bit ~default:false ~byte_ofs:15 ~bit_ofs:3 v)
        let of_message x = BA_.get_root_struct ~data_words:5 ~pointer_words:5 x
      end
    end
    module Const = struct
      type reader_t = ro RA_.StructStorage.t option
      type builder_t = rw RA_.StructStorage.t
      type reader_t_Const_12793219851699983392 = reader_t
      type builder_t_Const_12793219851699983392 = builder_t
      module R = struct
        type t = reader_t
        let type_get x =
          RA_.get_pointer_field x 3 ~f:RA_.get_struct
        let value_get x =
          RA_.get_pointer_field x 4 ~f:RA_.get_struct
        let of_message x = RA_.get_root_struct (RA_.Message.readonly x)
      end
      module B = struct
        type t = builder_t
        let type_get x =
          BA_.get_pointer_field x 3 ~f:(BA_.get_struct ~data_words:2 ~pointer_words:1)
        let type_set_reader x v =
          BA_.get_pointer_field x 3 ~f:(BA_.set_struct ~data_words:2 ~pointer_words:1 v)
        let type_set_builder x v =
          BA_.get_pointer_field x 3 ~f:(BA_.set_struct ~data_words:2 ~pointer_words:1 (Some v))
        let type_init x =
          BA_.get_pointer_field x 3 ~f:(BA_.init_struct ~data_words:2 ~pointer_words:1)
        let value_get x =
          BA_.get_pointer_field x 4 ~f:(BA_.get_struct ~data_words:2 ~pointer_words:1)
        let value_set_reader x v =
          BA_.get_pointer_field x 4 ~f:(BA_.set_struct ~data_words:2 ~pointer_words:1 v)
        let value_set_builder x v =
          BA_.get_pointer_field x 4 ~f:(BA_.set_struct ~data_words:2 ~pointer_words:1 (Some v))
        let value_init x =
          BA_.get_pointer_field x 4 ~f:(BA_.init_struct ~data_words:2 ~pointer_words:1)
        let of_message x = BA_.get_root_struct ~data_words:5 ~pointer_words:5 x
      end
    end
    module Interface = struct
      type reader_t = ro RA_.StructStorage.t option
      type builder_t = rw RA_.StructStorage.t
      type reader_t_Interface_16728431493453586831 = reader_t
      type builder_t_Interface_16728431493453586831 = builder_t
      module R = struct
        type t = reader_t
        let methods_get x =
          RA_.get_pointer_field x 3 ~f:RA_.get_struct_list
        let extends_get x =
          RA_.get_pointer_field x 4 ~f:RA_.get_uint64_list
        let of_message x = RA_.get_root_struct (RA_.Message.readonly x)
      end
      module B = struct
        type t = builder_t
        let methods_get x =
          BA_.get_pointer_field x 3 ~f:(BA_.get_struct_list ~data_words:3 ~pointer_words:2)
        let methods_set x v =
          BA_.get_pointer_field x 3 ~f:(BA_.set_struct_list ~data_words:3 ~pointer_words:2 v)
        let methods_init x n =
          BA_.get_pointer_field x 3 ~f:(BA_.init_struct_list ~data_words:3 ~pointer_words:2 n)
        let extends_get x =
          BA_.get_pointer_field x 4 ~f:BA_.get_uint64_list
        let extends_set x v =
          BA_.get_pointer_field x 4 ~f:(BA_.set_uint64_list v)
        let extends_init x n =
          BA_.get_pointer_field x 4 ~f:(BA_.init_uint64_list n)
        let of_message x = BA_.get_root_struct ~data_words:5 ~pointer_words:5 x
      end
    end
    module NestedNode = struct
      type reader_t = ro RA_.StructStorage.t option
      type builder_t = rw RA_.StructStorage.t
      type reader_t_NestedNode_16050641862814319170 = reader_t
      type builder_t_NestedNode_16050641862814319170 = builder_t
      module R = struct
        type t = reader_t
        let name_get x =
          RA_.get_pointer_field x 0 ~f:(RA_.get_text ~default:"")
        let id_get x =
          RA_.get_data_field x ~f:(RA_.get_uint64 ~default:Uint64.zero ~byte_ofs:0)
        let id_get_int_exn x =
          Uint64.to_int (id_get x)
        let of_message x = RA_.get_root_struct (RA_.Message.readonly x)
      end
      module B = struct
        type t = builder_t
        let name_get x =
          BA_.get_pointer_field x 0 ~f:(BA_.get_text ~default:"")
        let name_set x v =
          BA_.get_pointer_field x 0 ~f:(BA_.set_text v)
        let id_get x =
          BA_.get_data_field x ~f:(BA_.get_uint64 ~default:Uint64.zero ~byte_ofs:0)
        let id_get_int_exn x =
          Uint64.to_int (id_get x)
        let id_set x v =
          BA_.get_data_field x ~f:(BA_.set_uint64 ~default:Uint64.zero ~byte_ofs:0 v)
        let id_set_int_exn x v = id_set x (Uint64.of_int v)
        let of_message x = BA_.get_root_struct ~data_words:1 ~pointer_words:1 x
      end
    end
    module R = struct
      type t = reader_t
      let id_get x =
        RA_.get_data_field x ~f:(RA_.get_uint64 ~default:Uint64.zero ~byte_ofs:0)
      let id_get_int_exn x =
        Uint64.to_int (id_get x)
      let displayName_get x =
        RA_.get_pointer_field x 0 ~f:(RA_.get_text ~default:"")
      let displayNamePrefixLength_get x =
        RA_.get_data_field x ~f:(RA_.get_uint32 ~default:Uint32.zero ~byte_ofs:8)
      let displayNamePrefixLength_get_int_exn x =
        Uint32.to_int (displayNamePrefixLength_get x)
      let scopeId_get x =
        RA_.get_data_field x ~f:(RA_.get_uint64 ~default:Uint64.zero ~byte_ofs:16)
      let scopeId_get_int_exn x =
        Uint64.to_int (scopeId_get x)
      let nestedNodes_get x =
        RA_.get_pointer_field x 1 ~f:RA_.get_struct_list
      let annotations_get x =
        RA_.get_pointer_field x 2 ~f:RA_.get_struct_list
      let file_get x = ()
      let struct_get x = x
      let enum_get x = x
      let interface_get x = x
      let const_get x = x
      let annotation_get x = x
      type unnamed_union_t =
        | File
        | Struct of Struct.reader_t
        | Enum of Enum.reader_t
        | Interface of Interface.reader_t
        | Const of Const.reader_t
        | Annotation of Annotation.reader_t
        | Undefined of int
      let get x =
        match RA_.get_data_field x ~f:(RA_.get_uint16 ~default:0 ~byte_ofs:12) with
        | 0 -> File
        | 1 -> Struct (struct_get x)
        | 2 -> Enum (enum_get x)
        | 3 -> Interface (interface_get x)
        | 4 -> Const (const_get x)
        | 5 -> Annotation (annotation_get x)
        | v -> Undefined v
      let of_message x = RA_.get_root_struct (RA_.Message.readonly x)
    end
    module B = struct
      type t = builder_t
      let id_get x =
        BA_.get_data_field x ~f:(BA_.get_uint64 ~default:Uint64.zero ~byte_ofs:0)
      let id_get_int_exn x =
        Uint64.to_int (id_get x)
      let id_set x v =
        BA_.get_data_field x ~f:(BA_.set_uint64 ~default:Uint64.zero ~byte_ofs:0 v)
      let id_set_int_exn x v = id_set x (Uint64.of_int v)
      let displayName_get x =
        BA_.get_pointer_field x 0 ~f:(BA_.get_text ~default:"")
      let displayName_set x v =
        BA_.get_pointer_field x 0 ~f:(BA_.set_text v)
      let displayNamePrefixLength_get x =
        BA_.get_data_field x ~f:(BA_.get_uint32 ~default:Uint32.zero ~byte_ofs:8)
      let displayNamePrefixLength_get_int_exn x =
        Uint32.to_int (displayNamePrefixLength_get x)
      let displayNamePrefixLength_set x v =
        BA_.get_data_field x ~f:(BA_.set_uint32 ~default:Uint32.zero ~byte_ofs:8 v)
      let displayNamePrefixLength_set_int_exn x v = displayNamePrefixLength_set x (Uint32.of_int v)
      let scopeId_get x =
        BA_.get_data_field x ~f:(BA_.get_uint64 ~default:Uint64.zero ~byte_ofs:16)
      let scopeId_get_int_exn x =
        Uint64.to_int (scopeId_get x)
      let scopeId_set x v =
        BA_.get_data_field x ~f:(BA_.set_uint64 ~default:Uint64.zero ~byte_ofs:16 v)
      let scopeId_set_int_exn x v = scopeId_set x (Uint64.of_int v)
      let nestedNodes_get x =
        BA_.get_pointer_field x 1 ~f:(BA_.get_struct_list ~data_words:1 ~pointer_words:1)
      let nestedNodes_set x v =
        BA_.get_pointer_field x 1 ~f:(BA_.set_struct_list ~data_words:1 ~pointer_words:1 v)
      let nestedNodes_init x n =
        BA_.get_pointer_field x 1 ~f:(BA_.init_struct_list ~data_words:1 ~pointer_words:1 n)
      let annotations_get x =
        BA_.get_pointer_field x 2 ~f:(BA_.get_struct_list ~data_words:1 ~pointer_words:1)
      let annotations_set x v =
        BA_.get_pointer_field x 2 ~f:(BA_.set_struct_list ~data_words:1 ~pointer_words:1 v)
      let annotations_init x n =
        BA_.get_pointer_field x 2 ~f:(BA_.init_struct_list ~data_words:1 ~pointer_words:1 n)
      let file_get x = ()
      let file_set x =
        BA_.get_data_field ~discr:{BA_.Discr.value=0; BA_.Discr.byte_ofs=12} x ~f:BA_.set_void
      let struct_get x = x
      let enum_get x = x
      let interface_get x = x
      let const_get x = x
      let annotation_get x = x
      type unnamed_union_t =
        | File
        | Struct of Struct.builder_t
        | Enum of Enum.builder_t
        | Interface of Interface.builder_t
        | Const of Const.builder_t
        | Annotation of Annotation.builder_t
        | Undefined of int
      let get x =
        match BA_.get_data_field x ~f:(BA_.get_uint16 ~default:0 ~byte_ofs:12) with
        | 0 -> File
        | 1 -> Struct (struct_get x)
        | 2 -> Enum (enum_get x)
        | 3 -> Interface (interface_get x)
        | 4 -> Const (const_get x)
        | 5 -> Annotation (annotation_get x)
        | v -> Undefined v
      let of_message x = BA_.get_root_struct ~data_words:5 ~pointer_words:5 x
    end
  end
  module CodeGeneratorRequest = struct
    type reader_t = ro RA_.StructStorage.t option
    type builder_t = rw RA_.StructStorage.t
    type reader_t_CodeGeneratorRequest_13818529054586492878 = reader_t
    type builder_t_CodeGeneratorRequest_13818529054586492878 = builder_t
    module RequestedFile = struct
      type reader_t = ro RA_.StructStorage.t option
      type builder_t = rw RA_.StructStorage.t
      type reader_t_RequestedFile_14981803260258615394 = reader_t
      type builder_t_RequestedFile_14981803260258615394 = builder_t
      module Import = struct
        type reader_t = ro RA_.StructStorage.t option
        type builder_t = rw RA_.StructStorage.t
        type reader_t_Import_12560611460656617445 = reader_t
        type builder_t_Import_12560611460656617445 = builder_t
        module R = struct
          type t = reader_t
          let id_get x =
            RA_.get_data_field x ~f:(RA_.get_uint64 ~default:Uint64.zero ~byte_ofs:0)
          let id_get_int_exn x =
            Uint64.to_int (id_get x)
          let name_get x =
            RA_.get_pointer_field x 0 ~f:(RA_.get_text ~default:"")
          let of_message x = RA_.get_root_struct (RA_.Message.readonly x)
        end
        module B = struct
          type t = builder_t
          let id_get x =
            BA_.get_data_field x ~f:(BA_.get_uint64 ~default:Uint64.zero ~byte_ofs:0)
          let id_get_int_exn x =
            Uint64.to_int (id_get x)
          let id_set x v =
            BA_.get_data_field x ~f:(BA_.set_uint64 ~default:Uint64.zero ~byte_ofs:0 v)
          let id_set_int_exn x v = id_set x (Uint64.of_int v)
          let name_get x =
            BA_.get_pointer_field x 0 ~f:(BA_.get_text ~default:"")
          let name_set x v =
            BA_.get_pointer_field x 0 ~f:(BA_.set_text v)
          let of_message x = BA_.get_root_struct ~data_words:1 ~pointer_words:1 x
        end
      end
      module R = struct
        type t = reader_t
        let id_get x =
          RA_.get_data_field x ~f:(RA_.get_uint64 ~default:Uint64.zero ~byte_ofs:0)
        let id_get_int_exn x =
          Uint64.to_int (id_get x)
        let filename_get x =
          RA_.get_pointer_field x 0 ~f:(RA_.get_text ~default:"")
        let imports_get x =
          RA_.get_pointer_field x 1 ~f:RA_.get_struct_list
        let of_message x = RA_.get_root_struct (RA_.Message.readonly x)
      end
      module B = struct
        type t = builder_t
        let id_get x =
          BA_.get_data_field x ~f:(BA_.get_uint64 ~default:Uint64.zero ~byte_ofs:0)
        let id_get_int_exn x =
          Uint64.to_int (id_get x)
        let id_set x v =
          BA_.get_data_field x ~f:(BA_.set_uint64 ~default:Uint64.zero ~byte_ofs:0 v)
        let id_set_int_exn x v = id_set x (Uint64.of_int v)
        let filename_get x =
          BA_.get_pointer_field x 0 ~f:(BA_.get_text ~default:"")
        let filename_set x v =
          BA_.get_pointer_field x 0 ~f:(BA_.set_text v)
        let imports_get x =
          BA_.get_pointer_field x 1 ~f:(BA_.get_struct_list ~data_words:1 ~pointer_words:1)
        let imports_set x v =
          BA_.get_pointer_field x 1 ~f:(BA_.set_struct_list ~data_words:1 ~pointer_words:1 v)
        let imports_init x n =
          BA_.get_pointer_field x 1 ~f:(BA_.init_struct_list ~data_words:1 ~pointer_words:1 n)
        let of_message x = BA_.get_root_struct ~data_words:1 ~pointer_words:2 x
      end
    end
    module R = struct
      type t = reader_t
      let nodes_get x =
        RA_.get_pointer_field x 0 ~f:RA_.get_struct_list
      let requestedFiles_get x =
        RA_.get_pointer_field x 1 ~f:RA_.get_struct_list
      let of_message x = RA_.get_root_struct (RA_.Message.readonly x)
    end
    module B = struct
      type t = builder_t
      let nodes_get x =
        BA_.get_pointer_field x 0 ~f:(BA_.get_struct_list ~data_words:5 ~pointer_words:5)
      let nodes_set x v =
        BA_.get_pointer_field x 0 ~f:(BA_.set_struct_list ~data_words:5 ~pointer_words:5 v)
      let nodes_init x n =
        BA_.get_pointer_field x 0 ~f:(BA_.init_struct_list ~data_words:5 ~pointer_words:5 n)
      let requestedFiles_get x =
        BA_.get_pointer_field x 1 ~f:(BA_.get_struct_list ~data_words:1 ~pointer_words:2)
      let requestedFiles_set x v =
        BA_.get_pointer_field x 1 ~f:(BA_.set_struct_list ~data_words:1 ~pointer_words:2 v)
      let requestedFiles_init x n =
        BA_.get_pointer_field x 1 ~f:(BA_.init_struct_list ~data_words:1 ~pointer_words:2 n)
      let of_message x = BA_.get_root_struct ~data_words:0 ~pointer_words:2 x
    end
  end
end

