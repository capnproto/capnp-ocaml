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
        val to_message : t -> rw message_t
        val init_root : ?message_size:int -> unit -> t
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
        val to_message : t -> rw message_t
        val init_root : ?message_size:int -> unit -> t
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
        val to_message : t -> rw message_t
        val init_root : ?message_size:int -> unit -> t
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
        val to_message : t -> rw message_t
        val init_root : ?message_size:int -> unit -> t
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
      val to_message : t -> rw message_t
      val init_root : ?message_size:int -> unit -> t
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
      val to_message : t -> rw message_t
      val init_root : ?message_size:int -> unit -> t
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
      val to_message : t -> rw message_t
      val init_root : ?message_size:int -> unit -> t
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
      val to_message : t -> rw message_t
      val init_root : ?message_size:int -> unit -> t
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
      val to_message : t -> rw message_t
      val init_root : ?message_size:int -> unit -> t
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
        val to_message : t -> rw message_t
        val init_root : ?message_size:int -> unit -> t
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
        val to_message : t -> rw message_t
        val init_root : ?message_size:int -> unit -> t
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
        val to_message : t -> rw message_t
        val init_root : ?message_size:int -> unit -> t
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
      val to_message : t -> rw message_t
      val init_root : ?message_size:int -> unit -> t
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
        val to_message : t -> rw message_t
        val init_root : ?message_size:int -> unit -> t
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
        val to_message : t -> rw message_t
        val init_root : ?message_size:int -> unit -> t
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
        val to_message : t -> rw message_t
        val init_root : ?message_size:int -> unit -> t
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
        val to_message : t -> rw message_t
        val init_root : ?message_size:int -> unit -> t
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
        val to_message : t -> rw message_t
        val init_root : ?message_size:int -> unit -> t
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
        val to_message : t -> rw message_t
        val init_root : ?message_size:int -> unit -> t
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
      val to_message : t -> rw message_t
      val init_root : ?message_size:int -> unit -> t
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
          val to_message : t -> rw message_t
          val init_root : ?message_size:int -> unit -> t
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
        val to_message : t -> rw message_t
        val init_root : ?message_size:int -> unit -> t
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
      val to_message : t -> rw message_t
      val init_root : ?message_size:int -> unit -> t
    end
  end
end

module Make (MessageWrapper : Message.S) :
  (S with type 'cap message_t = 'cap MessageWrapper.Message.t
    and type AnyPointer.reader_t = Message.ro MessageWrapper.Slice.t option
    and type AnyPointer.builder_t = Message.rw MessageWrapper.Slice.t)

