type ro = Message.ro
type rw = Message.rw

module type S = sig
  module Reader : sig
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
      type builder_t
      type builder_t_Type_15020482145304562784 = builder_t
      type array_t
      module Enum : sig
        type t
        type t_Enum_11389172934837766057 = t
        type builder_t
        type builder_t_Enum_11389172934837766057 = builder_t
        type array_t
        val typeId_get : t -> Uint64.t
        val typeId_get_int_exn : t -> int
        val of_message : message_t -> t
      end

      module Interface : sig
        type t
        type t_Interface_17116997365232503999 = t
        type builder_t
        type builder_t_Interface_17116997365232503999 = builder_t
        type array_t
        val typeId_get : t -> Uint64.t
        val typeId_get_int_exn : t -> int
        val of_message : message_t -> t
      end

      module List : sig
        type t
        type t_List_9792858745991129751 = t
        type builder_t
        type builder_t_List_9792858745991129751 = builder_t
        type array_t
        val elementType_get : t -> t_Type_15020482145304562784
        val of_message : message_t -> t
      end

      module Struct : sig
        type t
        type t_Struct_12410354185295152851 = t
        type builder_t
        type builder_t_Struct_12410354185295152851 = builder_t
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
        | AnyPointer
        | Undefined_ of int

      val unnamed_union_get : t -> unnamed_union_t
      val of_message : message_t -> t
    end

    module Value : sig
      type t
      type t_Value_14853958794117909659 = t
      type builder_t
      type builder_t_Value_14853958794117909659 = builder_t
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
        | AnyPointer of AnyPointer.t
        | Undefined_ of int

      val unnamed_union_get : t -> unnamed_union_t
      val of_message : message_t -> t
    end

    module Annotation : sig
      type t
      type t_Annotation_17422339044421236034 = t
      type builder_t
      type builder_t_Annotation_17422339044421236034 = builder_t
      type array_t
      val id_get : t -> Uint64.t
      val id_get_int_exn : t -> int
      val value_get : t -> Value.t
      val of_message : message_t -> t
    end

    module Method : sig
      type t
      type t_Method_10736806783679155584 = t
      type builder_t
      type builder_t_Method_10736806783679155584 = builder_t
      type array_t
      val name_get : t -> string
      val codeOrder_get : t -> int
      val paramStructType_get : t -> Uint64.t
      val paramStructType_get_int_exn : t -> int
      val resultStructType_get : t -> Uint64.t
      val resultStructType_get_int_exn : t -> int
      val annotations_get : t -> (ro, Annotation.t, array_t) Runtime.Array.t
      val of_message : message_t -> t
    end

    module Enumerant : sig
      type t
      type t_Enumerant_10919677598968879693 = t
      type builder_t
      type builder_t_Enumerant_10919677598968879693 = builder_t
      type array_t
      val name_get : t -> string
      val codeOrder_get : t -> int
      val annotations_get : t -> (ro, Annotation.t, array_t) Runtime.Array.t
      val of_message : message_t -> t
    end

    module Field : sig
      type t
      type t_Field_11145653318641710175 = t
      type builder_t
      type builder_t_Field_11145653318641710175 = builder_t
      type array_t
      val noDiscriminant : int

      module Ordinal : sig
        type t
        type t_Ordinal_13515537513213004774 = t
        type builder_t
        type builder_t_Ordinal_13515537513213004774 = builder_t
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
        type builder_t
        type builder_t_Group_14626792032033250577 = builder_t
        type array_t
        val typeId_get : t -> Uint64.t
        val typeId_get_int_exn : t -> int
        val of_message : message_t -> t
      end

      module Slot : sig
        type t
        type t_Slot_14133145859926553711 = t
        type builder_t
        type builder_t_Slot_14133145859926553711 = builder_t
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
      val annotations_get : t -> (ro, Annotation.t, array_t) Runtime.Array.t
      val discriminantValue_get : t -> int
      val ordinal_get : t -> Ordinal.t
      val of_message : message_t -> t
    end

    module Node : sig
      type t
      type t_Node_16610026722781537303 = t
      type builder_t
      type builder_t_Node_16610026722781537303 = builder_t
      type array_t
      module Struct : sig
        type t
        type t_Struct_11430331134483579957 = t
        type builder_t
        type builder_t_Struct_11430331134483579957 = builder_t
        type array_t
        val dataWordCount_get : t -> int
        val pointerCount_get : t -> int
        val preferredListEncoding_get : t -> ElementSize.t
        val isGroup_get : t -> bool
        val discriminantCount_get : t -> int
        val discriminantOffset_get : t -> Uint32.t
        val discriminantOffset_get_int_exn : t -> int
        val fields_get : t -> (ro, Field.t, array_t) Runtime.Array.t
        val of_message : message_t -> t
      end

      module Enum : sig
        type t
        type t_Enum_13063450714778629528 = t
        type builder_t
        type builder_t_Enum_13063450714778629528 = builder_t
        type array_t
        val enumerants_get : t -> (ro, Enumerant.t, array_t) Runtime.Array.t
        val of_message : message_t -> t
      end

      module Annotation : sig
        type t
        type t_Annotation_17011813041836786320 = t
        type builder_t
        type builder_t_Annotation_17011813041836786320 = builder_t
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
        type builder_t
        type builder_t_Const_12793219851699983392 = builder_t
        type array_t
        val type_get : t -> Type.t
        val value_get : t -> Value.t
        val of_message : message_t -> t
      end

      module Interface : sig
        type t
        type t_Interface_16728431493453586831 = t
        type builder_t
        type builder_t_Interface_16728431493453586831 = builder_t
        type array_t
        val methods_get : t -> (ro, Method.t, array_t) Runtime.Array.t
        val extends_get : t -> (ro, Uint64.t, array_t) Runtime.Array.t
        val of_message : message_t -> t
      end

      module NestedNode : sig
        type t
        type t_NestedNode_16050641862814319170 = t
        type builder_t
        type builder_t_NestedNode_16050641862814319170 = builder_t
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
      val nestedNodes_get : t -> (ro, NestedNode.t, array_t) Runtime.Array.t
      val annotations_get : t -> (ro, Annotation.t, array_t) Runtime.Array.t
      val of_message : message_t -> t
    end

    module CodeGeneratorRequest : sig
      type t
      type t_CodeGeneratorRequest_13818529054586492878 = t
      type builder_t
      type builder_t_CodeGeneratorRequest_13818529054586492878 = builder_t
      type array_t
      module RequestedFile : sig
        type t
        type t_RequestedFile_14981803260258615394 = t
        type builder_t
        type builder_t_RequestedFile_14981803260258615394 = builder_t
        type array_t
        module Import : sig
          type t
          type t_Import_12560611460656617445 = t
          type builder_t
          type builder_t_Import_12560611460656617445 = builder_t
          type array_t
          val id_get : t -> Uint64.t
          val id_get_int_exn : t -> int
          val name_get : t -> string
          val of_message : message_t -> t
        end

        val id_get : t -> Uint64.t
        val id_get_int_exn : t -> int
        val filename_get : t -> string
        val imports_get : t -> (ro, Import.t, array_t) Runtime.Array.t
        val of_message : message_t -> t
      end

      val nodes_get : t -> (ro, Node.t, array_t) Runtime.Array.t
      val requestedFiles_get : t -> (ro, RequestedFile.t, array_t) Runtime.Array.t
      val of_message : message_t -> t
    end

  end

  module Builder : sig
    type message_t

    module AnyPointer : sig
      type t
    end

    module ElementSize : sig
      type t = Reader.ElementSize.t =
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
      type t = Reader.Type.builder_t
      type t_Type_15020482145304562784 = t
      type reader_t = Reader.Type.t
      type reader_t_Type_15020482145304562784 = reader_t
      type array_t
      type reader_array_t = Reader.Type.array_t

      module Enum : sig
        type t = Reader.Type.Enum.builder_t
        type t_Enum_11389172934837766057 = t
        type reader_t = Reader.Type.Enum.t
        type reader_t_Enum_11389172934837766057 = reader_t
        type array_t
        type reader_array_t = Reader.Type.Enum.array_t

        val typeId_get : t -> Uint64.t
        val typeId_get_int_exn : t -> int
        val typeId_set : t -> Uint64.t -> unit
        val typeId_set_int_exn : t -> int -> unit
        val of_message : message_t -> t
      end

      module Interface : sig
        type t = Reader.Type.Interface.builder_t
        type t_Interface_17116997365232503999 = t
        type reader_t = Reader.Type.Interface.t
        type reader_t_Interface_17116997365232503999 = reader_t
        type array_t
        type reader_array_t = Reader.Type.Interface.array_t

        val typeId_get : t -> Uint64.t
        val typeId_get_int_exn : t -> int
        val typeId_set : t -> Uint64.t -> unit
        val typeId_set_int_exn : t -> int -> unit
        val of_message : message_t -> t
      end

      module List : sig
        type t = Reader.Type.List.builder_t
        type t_List_9792858745991129751 = t
        type reader_t = Reader.Type.List.t
        type reader_t_List_9792858745991129751 = reader_t
        type array_t
        type reader_array_t = Reader.Type.List.array_t

        val elementType_get : t -> t_Type_15020482145304562784
        val elementType_set : t -> t_Type_15020482145304562784 -> t_Type_15020482145304562784
        val elementType_init : t -> t_Type_15020482145304562784
        val of_message : message_t -> t
      end

      module Struct : sig
        type t = Reader.Type.Struct.builder_t
        type t_Struct_12410354185295152851 = t
        type reader_t = Reader.Type.Struct.t
        type reader_t_Struct_12410354185295152851 = reader_t
        type array_t
        type reader_array_t = Reader.Type.Struct.array_t

        val typeId_get : t -> Uint64.t
        val typeId_get_int_exn : t -> int
        val typeId_set : t -> Uint64.t -> unit
        val typeId_set_int_exn : t -> int -> unit
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
        | AnyPointer
        | Undefined_ of int

      val unnamed_union_get : t -> unnamed_union_t
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
      val of_message : message_t -> t
    end

    module Value : sig
      type t = Reader.Value.builder_t
      type t_Value_14853958794117909659 = t
      type reader_t = Reader.Value.t
      type reader_t_Value_14853958794117909659 = reader_t
      type array_t
      type reader_array_t = Reader.Value.array_t

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
        | AnyPointer of AnyPointer.t
        | Undefined_ of int

      val unnamed_union_get : t -> unnamed_union_t
      val void_set : t -> unit
      val bool_set : t -> bool -> unit
      val int8_set : t -> int -> unit
      val int16_set : t -> int -> unit
      val int32_set : t -> int32 -> unit
      val int32_set_int_exn : t -> int -> unit
      val int64_set : t -> int64 -> unit
      val int64_set_int_exn : t -> int -> unit
      val uint8_set : t -> int -> unit
      val uint16_set : t -> int -> unit
      val uint32_set : t -> Uint32.t -> unit
      val uint32_set_int_exn : t -> int -> unit
      val uint64_set : t -> Uint64.t -> unit
      val uint64_set_int_exn : t -> int -> unit
      val float32_set : t -> float -> unit
      val float64_set : t -> float -> unit
      val text_set : t -> string -> unit
      val data_set : t -> string -> unit
      val list_set : t -> AnyPointer.t -> unit
      val enum_set : t -> int -> unit
      val struct_set : t -> AnyPointer.t -> unit
      val interface_set : t -> unit
      val anyPointer_set : t -> AnyPointer.t -> unit
      val of_message : message_t -> t
    end

    module Annotation : sig
      type t = Reader.Annotation.builder_t
      type t_Annotation_17422339044421236034 = t
      type reader_t = Reader.Annotation.t
      type reader_t_Annotation_17422339044421236034 = reader_t
      type array_t
      type reader_array_t = Reader.Annotation.array_t

      val id_get : t -> Uint64.t
      val id_get_int_exn : t -> int
      val value_get : t -> Value.t
      val id_set : t -> Uint64.t -> unit
      val id_set_int_exn : t -> int -> unit
      val value_set : t -> Value.t -> Value.t
      val value_init : t -> Value.t
      val of_message : message_t -> t
    end

    module Method : sig
      type t = Reader.Method.builder_t
      type t_Method_10736806783679155584 = t
      type reader_t = Reader.Method.t
      type reader_t_Method_10736806783679155584 = reader_t
      type array_t
      type reader_array_t = Reader.Method.array_t

      val name_get : t -> string
      val codeOrder_get : t -> int
      val paramStructType_get : t -> Uint64.t
      val paramStructType_get_int_exn : t -> int
      val resultStructType_get : t -> Uint64.t
      val resultStructType_get_int_exn : t -> int
      val annotations_get : t -> (rw, Annotation.t, array_t) Runtime.Array.t
      val name_set : t -> string -> unit
      val codeOrder_set : t -> int -> unit
      val paramStructType_set : t -> Uint64.t -> unit
      val paramStructType_set_int_exn : t -> int -> unit
      val resultStructType_set : t -> Uint64.t -> unit
      val resultStructType_set_int_exn : t -> int -> unit
      val annotations_set : t -> (rw, Annotation.t, array_t) Runtime.Array.t -> (rw, Annotation.t, array_t) Runtime.Array.t
      val annotations_init : t -> int -> (rw, Annotation.t, array_t) Runtime.Array.t
      val of_message : message_t -> t
    end

    module Enumerant : sig
      type t = Reader.Enumerant.builder_t
      type t_Enumerant_10919677598968879693 = t
      type reader_t = Reader.Enumerant.t
      type reader_t_Enumerant_10919677598968879693 = reader_t
      type array_t
      type reader_array_t = Reader.Enumerant.array_t

      val name_get : t -> string
      val codeOrder_get : t -> int
      val annotations_get : t -> (rw, Annotation.t, array_t) Runtime.Array.t
      val name_set : t -> string -> unit
      val codeOrder_set : t -> int -> unit
      val annotations_set : t -> (rw, Annotation.t, array_t) Runtime.Array.t -> (rw, Annotation.t, array_t) Runtime.Array.t
      val annotations_init : t -> int -> (rw, Annotation.t, array_t) Runtime.Array.t
      val of_message : message_t -> t
    end

    module Field : sig
      type t = Reader.Field.builder_t
      type t_Field_11145653318641710175 = t
      type reader_t = Reader.Field.t
      type reader_t_Field_11145653318641710175 = reader_t
      type array_t
      type reader_array_t = Reader.Field.array_t

      val noDiscriminant : int

      module Ordinal : sig
        type t = Reader.Field.Ordinal.builder_t
        type t_Ordinal_13515537513213004774 = t
        type reader_t = Reader.Field.Ordinal.t
        type reader_t_Ordinal_13515537513213004774 = reader_t
        type array_t
        type reader_array_t = Reader.Field.Ordinal.array_t

        type unnamed_union_t =
          | Implicit
          | Explicit of int
          | Undefined_ of int

        val unnamed_union_get : t -> unnamed_union_t
        val implicit_set : t -> unit
        val explicit_set : t -> int -> unit
        val of_message : message_t -> t
      end

      module Group : sig
        type t = Reader.Field.Group.builder_t
        type t_Group_14626792032033250577 = t
        type reader_t = Reader.Field.Group.t
        type reader_t_Group_14626792032033250577 = reader_t
        type array_t
        type reader_array_t = Reader.Field.Group.array_t

        val typeId_get : t -> Uint64.t
        val typeId_get_int_exn : t -> int
        val typeId_set : t -> Uint64.t -> unit
        val typeId_set_int_exn : t -> int -> unit
        val of_message : message_t -> t
      end

      module Slot : sig
        type t = Reader.Field.Slot.builder_t
        type t_Slot_14133145859926553711 = t
        type reader_t = Reader.Field.Slot.t
        type reader_t_Slot_14133145859926553711 = reader_t
        type array_t
        type reader_array_t = Reader.Field.Slot.array_t

        val offset_get : t -> Uint32.t
        val offset_get_int_exn : t -> int
        val type_get : t -> Type.t
        val defaultValue_get : t -> Value.t
        val hadExplicitDefault_get : t -> bool
        val offset_set : t -> Uint32.t -> unit
        val offset_set_int_exn : t -> int -> unit
        val type_set : t -> Type.t -> Type.t
        val type_init : t -> Type.t
        val defaultValue_set : t -> Value.t -> Value.t
        val defaultValue_init : t -> Value.t
        val hadExplicitDefault_set : t -> bool -> unit
        val of_message : message_t -> t
      end

      type unnamed_union_t =
        | Slot of Slot.t
        | Group of Group.t
        | Undefined_ of int

      val unnamed_union_get : t -> unnamed_union_t
      val name_get : t -> string
      val codeOrder_get : t -> int
      val annotations_get : t -> (rw, Annotation.t, array_t) Runtime.Array.t
      val discriminantValue_get : t -> int
      val ordinal_get : t -> Ordinal.t
      val name_set : t -> string -> unit
      val codeOrder_set : t -> int -> unit
      val annotations_set : t -> (rw, Annotation.t, array_t) Runtime.Array.t -> (rw, Annotation.t, array_t) Runtime.Array.t
      val annotations_init : t -> int -> (rw, Annotation.t, array_t) Runtime.Array.t
      val discriminantValue_set : t -> int -> unit
      val of_message : message_t -> t
    end

    module Node : sig
      type t = Reader.Node.builder_t
      type t_Node_16610026722781537303 = t
      type reader_t = Reader.Node.t
      type reader_t_Node_16610026722781537303 = reader_t
      type array_t
      type reader_array_t = Reader.Node.array_t

      module Struct : sig
        type t = Reader.Node.Struct.builder_t
        type t_Struct_11430331134483579957 = t
        type reader_t = Reader.Node.Struct.t
        type reader_t_Struct_11430331134483579957 = reader_t
        type array_t
        type reader_array_t = Reader.Node.Struct.array_t

        val dataWordCount_get : t -> int
        val pointerCount_get : t -> int
        val preferredListEncoding_get : t -> ElementSize.t
        val isGroup_get : t -> bool
        val discriminantCount_get : t -> int
        val discriminantOffset_get : t -> Uint32.t
        val discriminantOffset_get_int_exn : t -> int
        val fields_get : t -> (rw, Field.t, array_t) Runtime.Array.t
        val dataWordCount_set : t -> int -> unit
        val pointerCount_set : t -> int -> unit
        val preferredListEncoding_set : t -> ElementSize.t -> unit
        val isGroup_set : t -> bool -> unit
        val discriminantCount_set : t -> int -> unit
        val discriminantOffset_set : t -> Uint32.t -> unit
        val discriminantOffset_set_int_exn : t -> int -> unit
        val fields_set : t -> (rw, Field.t, array_t) Runtime.Array.t -> (rw, Field.t, array_t) Runtime.Array.t
        val fields_init : t -> int -> (rw, Field.t, array_t) Runtime.Array.t
        val of_message : message_t -> t
      end

      module Enum : sig
        type t = Reader.Node.Enum.builder_t
        type t_Enum_13063450714778629528 = t
        type reader_t = Reader.Node.Enum.t
        type reader_t_Enum_13063450714778629528 = reader_t
        type array_t
        type reader_array_t = Reader.Node.Enum.array_t

        val enumerants_get : t -> (rw, Enumerant.t, array_t) Runtime.Array.t
        val enumerants_set : t -> (rw, Enumerant.t, array_t) Runtime.Array.t -> (rw, Enumerant.t, array_t) Runtime.Array.t
        val enumerants_init : t -> int -> (rw, Enumerant.t, array_t) Runtime.Array.t
        val of_message : message_t -> t
      end

      module Annotation : sig
        type t = Reader.Node.Annotation.builder_t
        type t_Annotation_17011813041836786320 = t
        type reader_t = Reader.Node.Annotation.t
        type reader_t_Annotation_17011813041836786320 = reader_t
        type array_t
        type reader_array_t = Reader.Node.Annotation.array_t

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
        val type_set : t -> Type.t -> Type.t
        val type_init : t -> Type.t
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
        val of_message : message_t -> t
      end

      module Const : sig
        type t = Reader.Node.Const.builder_t
        type t_Const_12793219851699983392 = t
        type reader_t = Reader.Node.Const.t
        type reader_t_Const_12793219851699983392 = reader_t
        type array_t
        type reader_array_t = Reader.Node.Const.array_t

        val type_get : t -> Type.t
        val value_get : t -> Value.t
        val type_set : t -> Type.t -> Type.t
        val type_init : t -> Type.t
        val value_set : t -> Value.t -> Value.t
        val value_init : t -> Value.t
        val of_message : message_t -> t
      end

      module Interface : sig
        type t = Reader.Node.Interface.builder_t
        type t_Interface_16728431493453586831 = t
        type reader_t = Reader.Node.Interface.t
        type reader_t_Interface_16728431493453586831 = reader_t
        type array_t
        type reader_array_t = Reader.Node.Interface.array_t

        val methods_get : t -> (rw, Method.t, array_t) Runtime.Array.t
        val extends_get : t -> (rw, Uint64.t, array_t) Runtime.Array.t
        val methods_set : t -> (rw, Method.t, array_t) Runtime.Array.t -> (rw, Method.t, array_t) Runtime.Array.t
        val methods_init : t -> int -> (rw, Method.t, array_t) Runtime.Array.t
        val extends_set : t -> (rw, Uint64.t, array_t) Runtime.Array.t -> (rw, Uint64.t, array_t) Runtime.Array.t
        val extends_init : t -> int -> (rw, Uint64.t, array_t) Runtime.Array.t
        val of_message : message_t -> t
      end

      module NestedNode : sig
        type t = Reader.Node.NestedNode.builder_t
        type t_NestedNode_16050641862814319170 = t
        type reader_t = Reader.Node.NestedNode.t
        type reader_t_NestedNode_16050641862814319170 = reader_t
        type array_t
        type reader_array_t = Reader.Node.NestedNode.array_t

        val name_get : t -> string
        val id_get : t -> Uint64.t
        val id_get_int_exn : t -> int
        val name_set : t -> string -> unit
        val id_set : t -> Uint64.t -> unit
        val id_set_int_exn : t -> int -> unit
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
      val file_set : t -> unit
      val id_get : t -> Uint64.t
      val id_get_int_exn : t -> int
      val displayName_get : t -> string
      val displayNamePrefixLength_get : t -> Uint32.t
      val displayNamePrefixLength_get_int_exn : t -> int
      val scopeId_get : t -> Uint64.t
      val scopeId_get_int_exn : t -> int
      val nestedNodes_get : t -> (rw, NestedNode.t, array_t) Runtime.Array.t
      val annotations_get : t -> (rw, Annotation.t, array_t) Runtime.Array.t
      val id_set : t -> Uint64.t -> unit
      val id_set_int_exn : t -> int -> unit
      val displayName_set : t -> string -> unit
      val displayNamePrefixLength_set : t -> Uint32.t -> unit
      val displayNamePrefixLength_set_int_exn : t -> int -> unit
      val scopeId_set : t -> Uint64.t -> unit
      val scopeId_set_int_exn : t -> int -> unit
      val nestedNodes_set : t -> (rw, NestedNode.t, array_t) Runtime.Array.t -> (rw, NestedNode.t, array_t) Runtime.Array.t
      val nestedNodes_init : t -> int -> (rw, NestedNode.t, array_t) Runtime.Array.t
      val annotations_set : t -> (rw, Annotation.t, array_t) Runtime.Array.t -> (rw, Annotation.t, array_t) Runtime.Array.t
      val annotations_init : t -> int -> (rw, Annotation.t, array_t) Runtime.Array.t
      val of_message : message_t -> t
    end

    module CodeGeneratorRequest : sig
      type t = Reader.CodeGeneratorRequest.builder_t
      type t_CodeGeneratorRequest_13818529054586492878 = t
      type reader_t = Reader.CodeGeneratorRequest.t
      type reader_t_CodeGeneratorRequest_13818529054586492878 = reader_t
      type array_t
      type reader_array_t = Reader.CodeGeneratorRequest.array_t

      module RequestedFile : sig
        type t = Reader.CodeGeneratorRequest.RequestedFile.builder_t
        type t_RequestedFile_14981803260258615394 = t
        type reader_t = Reader.CodeGeneratorRequest.RequestedFile.t
        type reader_t_RequestedFile_14981803260258615394 = reader_t
        type array_t
        type reader_array_t = Reader.CodeGeneratorRequest.RequestedFile.array_t

        module Import : sig
          type t = Reader.CodeGeneratorRequest.RequestedFile.Import.builder_t
          type t_Import_12560611460656617445 = t
          type reader_t = Reader.CodeGeneratorRequest.RequestedFile.Import.t
          type reader_t_Import_12560611460656617445 = reader_t
          type array_t
          type reader_array_t = Reader.CodeGeneratorRequest.RequestedFile.Import.array_t

          val id_get : t -> Uint64.t
          val id_get_int_exn : t -> int
          val name_get : t -> string
          val id_set : t -> Uint64.t -> unit
          val id_set_int_exn : t -> int -> unit
          val name_set : t -> string -> unit
          val of_message : message_t -> t
        end

        val id_get : t -> Uint64.t
        val id_get_int_exn : t -> int
        val filename_get : t -> string
        val imports_get : t -> (rw, Import.t, array_t) Runtime.Array.t
        val id_set : t -> Uint64.t -> unit
        val id_set_int_exn : t -> int -> unit
        val filename_set : t -> string -> unit
        val imports_set : t -> (rw, Import.t, array_t) Runtime.Array.t -> (rw, Import.t, array_t) Runtime.Array.t
        val imports_init : t -> int -> (rw, Import.t, array_t) Runtime.Array.t
        val of_message : message_t -> t
      end

      val nodes_get : t -> (rw, Node.t, array_t) Runtime.Array.t
      val requestedFiles_get : t -> (rw, RequestedFile.t, array_t) Runtime.Array.t
      val nodes_set : t -> (rw, Node.t, array_t) Runtime.Array.t -> (rw, Node.t, array_t) Runtime.Array.t
      val nodes_init : t -> int -> (rw, Node.t, array_t) Runtime.Array.t
      val requestedFiles_set : t -> (rw, RequestedFile.t, array_t) Runtime.Array.t -> (rw, RequestedFile.t, array_t) Runtime.Array.t
      val requestedFiles_init : t -> int -> (rw, RequestedFile.t, array_t) Runtime.Array.t
      val of_message : message_t -> t
    end

  end
end

module Make (MessageWrapper : Message.S) = struct
  let invalid_msg = Message.invalid_msg

  module Reader = struct
    module RuntimeReader_ = MessageReader.Make(MessageWrapper)
    open RuntimeReader_

    type message_t = ro RuntimeReader_.Message.t

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
      type builder_t = rw StructStorage.t
      type builder_t_Type_15020482145304562784 = builder_t
      type array_t = ro ListStorage.t
      module Enum = struct
        type t = ro StructStorage.t option
        type t_Enum_11389172934837766057 = t
        type builder_t = rw StructStorage.t
        type builder_t_Enum_11389172934837766057 = builder_t
        type array_t = ro ListStorage.t
        let typeId_get x =
          get_data_field x ~f:(get_uint64 ~default:Uint64.zero ~byte_ofs:8)
        let typeId_get_int_exn x =
          Uint64.to_int (typeId_get x)
        let of_message x = get_root_struct x
      end
      module Interface = struct
        type t = ro StructStorage.t option
        type t_Interface_17116997365232503999 = t
        type builder_t = rw StructStorage.t
        type builder_t_Interface_17116997365232503999 = builder_t
        type array_t = ro ListStorage.t
        let typeId_get x =
          get_data_field x ~f:(get_uint64 ~default:Uint64.zero ~byte_ofs:8)
        let typeId_get_int_exn x =
          Uint64.to_int (typeId_get x)
        let of_message x = get_root_struct x
      end
      module List = struct
        type t = ro StructStorage.t option
        type t_List_9792858745991129751 = t
        type builder_t = rw StructStorage.t
        type builder_t_List_9792858745991129751 = builder_t
        type array_t = ro ListStorage.t
        let elementType_get x =
          get_pointer_field x 0 ~f:get_struct
        let of_message x = get_root_struct x
      end
      module Struct = struct
        type t = ro StructStorage.t option
        type t_Struct_12410354185295152851 = t
        type builder_t = rw StructStorage.t
        type builder_t_Struct_12410354185295152851 = builder_t
        type array_t = ro ListStorage.t
        let typeId_get x =
          get_data_field x ~f:(get_uint64 ~default:Uint64.zero ~byte_ofs:8)
        let typeId_get_int_exn x =
          Uint64.to_int (typeId_get x)
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
        | List of List.t
        | Enum of Enum.t
        | Struct of Struct.t
        | Interface of Interface.t
        | AnyPointer
        | Undefined_ of int
      let unnamed_union_get x =
        match get_data_field x ~f:(get_uint16 ~default:0 ~byte_ofs:0) with
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
        | v -> Undefined_ v
      let of_message x = get_root_struct x
    end
    module Value = struct
      type t = ro StructStorage.t option
      type t_Value_14853958794117909659 = t
      type builder_t = rw StructStorage.t
      type builder_t_Value_14853958794117909659 = builder_t
      type array_t = ro ListStorage.t
      let void_get x = ()
      let bool_get x =
        get_data_field x ~f:(get_bit ~default:false ~byte_ofs:2 ~bit_ofs:0)
      let int8_get x =
        get_data_field x ~f:(get_int8 ~default:0 ~byte_ofs:2)
      let int16_get x =
        get_data_field x ~f:(get_int16 ~default:0 ~byte_ofs:2)
      let int32_get x =
        get_data_field x ~f:(get_int32 ~default:0l ~byte_ofs:4)
      let int32_get_int_exn x =
        Int32.to_int (int32_get x)
      let int64_get x =
        get_data_field x ~f:(get_int64 ~default:0L ~byte_ofs:8)
      let int64_get_int_exn x =
        Int64.to_int (int64_get x)
      let uint8_get x =
        get_data_field x ~f:(get_uint8 ~default:0 ~byte_ofs:2)
      let uint16_get x =
        get_data_field x ~f:(get_uint16 ~default:0 ~byte_ofs:2)
      let uint32_get x =
        get_data_field x ~f:(get_uint32 ~default:Uint32.zero ~byte_ofs:4)
      let uint32_get_int_exn x =
        Uint32.to_int (uint32_get x)
      let uint64_get x =
        get_data_field x ~f:(get_uint64 ~default:Uint64.zero ~byte_ofs:8)
      let uint64_get_int_exn x =
        Uint64.to_int (uint64_get x)
      let float32_get x =
        get_data_field x ~f:(get_float32 ~default_bits:0l ~byte_ofs:4)
      let float64_get x =
        get_data_field x ~f:(get_float64 ~default_bits:0L ~byte_ofs:8)
      let text_get x =
        get_pointer_field x 0 ~f:(get_text ~default:"")
      let data_get x =
        get_pointer_field x 0 ~f:(get_blob ~default:"")
      let list_get x =
        get_pointer_field x 0 ~f:(fun x -> x)
      let enum_get x =
        get_data_field x ~f:(get_uint16 ~default:0 ~byte_ofs:2)
      let struct_get x =
        get_pointer_field x 0 ~f:(fun x -> x)
      let interface_get x = ()
      let anyPointer_get x =
        get_pointer_field x 0 ~f:(fun x -> x)
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
        | AnyPointer of AnyPointer.t
        | Undefined_ of int
      let unnamed_union_get x =
        match get_data_field x ~f:(get_uint16 ~default:0 ~byte_ofs:0) with
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
        | v -> Undefined_ v
      let of_message x = get_root_struct x
    end
    module Annotation = struct
      type t = ro StructStorage.t option
      type t_Annotation_17422339044421236034 = t
      type builder_t = rw StructStorage.t
      type builder_t_Annotation_17422339044421236034 = builder_t
      type array_t = ro ListStorage.t
      let id_get x =
        get_data_field x ~f:(get_uint64 ~default:Uint64.zero ~byte_ofs:0)
      let id_get_int_exn x =
        Uint64.to_int (id_get x)
      let value_get x =
        get_pointer_field x 0 ~f:get_struct
      let of_message x = get_root_struct x
    end
    module Method = struct
      type t = ro StructStorage.t option
      type t_Method_10736806783679155584 = t
      type builder_t = rw StructStorage.t
      type builder_t_Method_10736806783679155584 = builder_t
      type array_t = ro ListStorage.t
      let name_get x =
        get_pointer_field x 0 ~f:(get_text ~default:"")
      let codeOrder_get x =
        get_data_field x ~f:(get_uint16 ~default:0 ~byte_ofs:0)
      let paramStructType_get x =
        get_data_field x ~f:(get_uint64 ~default:Uint64.zero ~byte_ofs:8)
      let paramStructType_get_int_exn x =
        Uint64.to_int (paramStructType_get x)
      let resultStructType_get x =
        get_data_field x ~f:(get_uint64 ~default:Uint64.zero ~byte_ofs:16)
      let resultStructType_get_int_exn x =
        Uint64.to_int (resultStructType_get x)
      let annotations_get x =
        get_pointer_field x 1 ~f:get_struct_list
      let of_message x = get_root_struct x
    end
    module Enumerant = struct
      type t = ro StructStorage.t option
      type t_Enumerant_10919677598968879693 = t
      type builder_t = rw StructStorage.t
      type builder_t_Enumerant_10919677598968879693 = builder_t
      type array_t = ro ListStorage.t
      let name_get x =
        get_pointer_field x 0 ~f:(get_text ~default:"")
      let codeOrder_get x =
        get_data_field x ~f:(get_uint16 ~default:0 ~byte_ofs:0)
      let annotations_get x =
        get_pointer_field x 1 ~f:get_struct_list
      let of_message x = get_root_struct x
    end
    module Field = struct
      type t = ro StructStorage.t option
      type t_Field_11145653318641710175 = t
      type builder_t = rw StructStorage.t
      type builder_t_Field_11145653318641710175 = builder_t
      type array_t = ro ListStorage.t
      let noDiscriminant = 65535
      module Ordinal = struct
        type t = ro StructStorage.t option
        type t_Ordinal_13515537513213004774 = t
        type builder_t = rw StructStorage.t
        type builder_t_Ordinal_13515537513213004774 = builder_t
        type array_t = ro ListStorage.t
        let implicit_get x = ()
        let explicit_get x =
          get_data_field x ~f:(get_uint16 ~default:0 ~byte_ofs:12)
        type unnamed_union_t =
          | Implicit
          | Explicit of int
          | Undefined_ of int
        let unnamed_union_get x =
          match get_data_field x ~f:(get_uint16 ~default:0 ~byte_ofs:10) with
          | 0 -> Implicit
          | 1 -> Explicit (explicit_get x)
          | v -> Undefined_ v
        let of_message x = get_root_struct x
      end
      module Group = struct
        type t = ro StructStorage.t option
        type t_Group_14626792032033250577 = t
        type builder_t = rw StructStorage.t
        type builder_t_Group_14626792032033250577 = builder_t
        type array_t = ro ListStorage.t
        let typeId_get x =
          get_data_field x ~f:(get_uint64 ~default:Uint64.zero ~byte_ofs:16)
        let typeId_get_int_exn x =
          Uint64.to_int (typeId_get x)
        let of_message x = get_root_struct x
      end
      module Slot = struct
        type t = ro StructStorage.t option
        type t_Slot_14133145859926553711 = t
        type builder_t = rw StructStorage.t
        type builder_t_Slot_14133145859926553711 = builder_t
        type array_t = ro ListStorage.t
        let offset_get x =
          get_data_field x ~f:(get_uint32 ~default:Uint32.zero ~byte_ofs:4)
        let offset_get_int_exn x =
          Uint32.to_int (offset_get x)
        let type_get x =
          get_pointer_field x 2 ~f:get_struct
        let defaultValue_get x =
          get_pointer_field x 3 ~f:get_struct
        let hadExplicitDefault_get x =
          get_data_field x ~f:(get_bit ~default:false ~byte_ofs:16 ~bit_ofs:0)
        let of_message x = get_root_struct x
      end
      let name_get x =
        get_pointer_field x 0 ~f:(get_text ~default:"")
      let codeOrder_get x =
        get_data_field x ~f:(get_uint16 ~default:0 ~byte_ofs:0)
      let annotations_get x =
        get_pointer_field x 1 ~f:get_struct_list
      let discriminantValue_get x =
        get_data_field x ~f:(get_uint16 ~default:65535 ~byte_ofs:2)
      let slot_get x = x
      let group_get x = x
      let ordinal_get x = x
      type unnamed_union_t =
        | Slot of Slot.t
        | Group of Group.t
        | Undefined_ of int
      let unnamed_union_get x =
        match get_data_field x ~f:(get_uint16 ~default:0 ~byte_ofs:8) with
        | 0 -> Slot (slot_get x)
        | 1 -> Group (group_get x)
        | v -> Undefined_ v
      let of_message x = get_root_struct x
    end
    module Node = struct
      type t = ro StructStorage.t option
      type t_Node_16610026722781537303 = t
      type builder_t = rw StructStorage.t
      type builder_t_Node_16610026722781537303 = builder_t
      type array_t = ro ListStorage.t
      module Struct = struct
        type t = ro StructStorage.t option
        type t_Struct_11430331134483579957 = t
        type builder_t = rw StructStorage.t
        type builder_t_Struct_11430331134483579957 = builder_t
        type array_t = ro ListStorage.t
        let dataWordCount_get x =
          get_data_field x ~f:(get_uint16 ~default:0 ~byte_ofs:14)
        let pointerCount_get x =
          get_data_field x ~f:(get_uint16 ~default:0 ~byte_ofs:24)
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
          let discr = get_data_field x ~f:(get_uint16 ~default:0 ~byte_ofs:26) in
          decode discr
        let isGroup_get x =
          get_data_field x ~f:(get_bit ~default:false ~byte_ofs:28 ~bit_ofs:0)
        let discriminantCount_get x =
          get_data_field x ~f:(get_uint16 ~default:0 ~byte_ofs:30)
        let discriminantOffset_get x =
          get_data_field x ~f:(get_uint32 ~default:Uint32.zero ~byte_ofs:32)
        let discriminantOffset_get_int_exn x =
          Uint32.to_int (discriminantOffset_get x)
        let fields_get x =
          get_pointer_field x 3 ~f:get_struct_list
        let of_message x = get_root_struct x
      end
      module Enum = struct
        type t = ro StructStorage.t option
        type t_Enum_13063450714778629528 = t
        type builder_t = rw StructStorage.t
        type builder_t_Enum_13063450714778629528 = builder_t
        type array_t = ro ListStorage.t
        let enumerants_get x =
          get_pointer_field x 3 ~f:get_struct_list
        let of_message x = get_root_struct x
      end
      module Annotation = struct
        type t = ro StructStorage.t option
        type t_Annotation_17011813041836786320 = t
        type builder_t = rw StructStorage.t
        type builder_t_Annotation_17011813041836786320 = builder_t
        type array_t = ro ListStorage.t
        let type_get x =
          get_pointer_field x 3 ~f:get_struct
        let targetsFile_get x =
          get_data_field x ~f:(get_bit ~default:false ~byte_ofs:14 ~bit_ofs:0)
        let targetsConst_get x =
          get_data_field x ~f:(get_bit ~default:false ~byte_ofs:14 ~bit_ofs:1)
        let targetsEnum_get x =
          get_data_field x ~f:(get_bit ~default:false ~byte_ofs:14 ~bit_ofs:2)
        let targetsEnumerant_get x =
          get_data_field x ~f:(get_bit ~default:false ~byte_ofs:14 ~bit_ofs:3)
        let targetsStruct_get x =
          get_data_field x ~f:(get_bit ~default:false ~byte_ofs:14 ~bit_ofs:4)
        let targetsField_get x =
          get_data_field x ~f:(get_bit ~default:false ~byte_ofs:14 ~bit_ofs:5)
        let targetsUnion_get x =
          get_data_field x ~f:(get_bit ~default:false ~byte_ofs:14 ~bit_ofs:6)
        let targetsGroup_get x =
          get_data_field x ~f:(get_bit ~default:false ~byte_ofs:14 ~bit_ofs:7)
        let targetsInterface_get x =
          get_data_field x ~f:(get_bit ~default:false ~byte_ofs:15 ~bit_ofs:0)
        let targetsMethod_get x =
          get_data_field x ~f:(get_bit ~default:false ~byte_ofs:15 ~bit_ofs:1)
        let targetsParam_get x =
          get_data_field x ~f:(get_bit ~default:false ~byte_ofs:15 ~bit_ofs:2)
        let targetsAnnotation_get x =
          get_data_field x ~f:(get_bit ~default:false ~byte_ofs:15 ~bit_ofs:3)
        let of_message x = get_root_struct x
      end
      module Const = struct
        type t = ro StructStorage.t option
        type t_Const_12793219851699983392 = t
        type builder_t = rw StructStorage.t
        type builder_t_Const_12793219851699983392 = builder_t
        type array_t = ro ListStorage.t
        let type_get x =
          get_pointer_field x 3 ~f:get_struct
        let value_get x =
          get_pointer_field x 4 ~f:get_struct
        let of_message x = get_root_struct x
      end
      module Interface = struct
        type t = ro StructStorage.t option
        type t_Interface_16728431493453586831 = t
        type builder_t = rw StructStorage.t
        type builder_t_Interface_16728431493453586831 = builder_t
        type array_t = ro ListStorage.t
        let methods_get x =
          get_pointer_field x 3 ~f:get_struct_list
        let extends_get x =
          get_pointer_field x 4 ~f:get_uint64_list
        let of_message x = get_root_struct x
      end
      module NestedNode = struct
        type t = ro StructStorage.t option
        type t_NestedNode_16050641862814319170 = t
        type builder_t = rw StructStorage.t
        type builder_t_NestedNode_16050641862814319170 = builder_t
        type array_t = ro ListStorage.t
        let name_get x =
          get_pointer_field x 0 ~f:(get_text ~default:"")
        let id_get x =
          get_data_field x ~f:(get_uint64 ~default:Uint64.zero ~byte_ofs:0)
        let id_get_int_exn x =
          Uint64.to_int (id_get x)
        let of_message x = get_root_struct x
      end
      let id_get x =
        get_data_field x ~f:(get_uint64 ~default:Uint64.zero ~byte_ofs:0)
      let id_get_int_exn x =
        Uint64.to_int (id_get x)
      let displayName_get x =
        get_pointer_field x 0 ~f:(get_text ~default:"")
      let displayNamePrefixLength_get x =
        get_data_field x ~f:(get_uint32 ~default:Uint32.zero ~byte_ofs:8)
      let displayNamePrefixLength_get_int_exn x =
        Uint32.to_int (displayNamePrefixLength_get x)
      let scopeId_get x =
        get_data_field x ~f:(get_uint64 ~default:Uint64.zero ~byte_ofs:16)
      let scopeId_get_int_exn x =
        Uint64.to_int (scopeId_get x)
      let nestedNodes_get x =
        get_pointer_field x 1 ~f:get_struct_list
      let annotations_get x =
        get_pointer_field x 2 ~f:get_struct_list
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
        match get_data_field x ~f:(get_uint16 ~default:0 ~byte_ofs:12) with
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
      type builder_t = rw StructStorage.t
      type builder_t_CodeGeneratorRequest_13818529054586492878 = builder_t
      type array_t = ro ListStorage.t
      module RequestedFile = struct
        type t = ro StructStorage.t option
        type t_RequestedFile_14981803260258615394 = t
        type builder_t = rw StructStorage.t
        type builder_t_RequestedFile_14981803260258615394 = builder_t
        type array_t = ro ListStorage.t
        module Import = struct
          type t = ro StructStorage.t option
          type t_Import_12560611460656617445 = t
          type builder_t = rw StructStorage.t
          type builder_t_Import_12560611460656617445 = builder_t
          type array_t = ro ListStorage.t
          let id_get x =
            get_data_field x ~f:(get_uint64 ~default:Uint64.zero ~byte_ofs:0)
          let id_get_int_exn x =
            Uint64.to_int (id_get x)
          let name_get x =
            get_pointer_field x 0 ~f:(get_text ~default:"")
          let of_message x = get_root_struct x
        end
        let id_get x =
          get_data_field x ~f:(get_uint64 ~default:Uint64.zero ~byte_ofs:0)
        let id_get_int_exn x =
          Uint64.to_int (id_get x)
        let filename_get x =
          get_pointer_field x 0 ~f:(get_text ~default:"")
        let imports_get x =
          get_pointer_field x 1 ~f:get_struct_list
        let of_message x = get_root_struct x
      end
      let nodes_get x =
        get_pointer_field x 0 ~f:get_struct_list
      let requestedFiles_get x =
        get_pointer_field x 1 ~f:get_struct_list
      let of_message x = get_root_struct x
    end  end

  module Builder = struct
    module RuntimeBuilder_ = MessageBuilder.Make(MessageWrapper)
    open RuntimeBuilder_

    type message_t = rw RuntimeBuilder_.Message.t

    module AnyPointer = struct
      type t = rw Slice.t
    end

    module ElementSize = struct
      type t = Reader.ElementSize.t =
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
      type t = rw StructStorage.t
      type t_Type_15020482145304562784 = t
      type reader_t = Reader.Type.t
      type reader_t_Type_15020482145304562784 = reader_t
      type array_t = rw ListStorage.t
      type reader_array_t = ro ListStorage.t
      module Enum = struct
        type t = rw StructStorage.t
        type t_Enum_11389172934837766057 = t
        type reader_t = Reader.Type.Enum.t
        type reader_t_Enum_11389172934837766057 = reader_t
        type array_t = rw ListStorage.t
        type reader_array_t = ro ListStorage.t
        let typeId_get x = get_data_field x ~f:(get_uint64 ~default:Uint64.zero ~byte_ofs:8)
        let typeId_get_int_exn x = Uint64.to_int (typeId_get x)
        let typeId_set x v = get_data_field x ~f:(set_uint64 ~default:Uint64.zero ~byte_ofs:8 v)
        let typeId_set_int_exn x v = typeId_set x (Uint64.of_int v)
        let of_message x = get_root_struct ~data_words:2 ~pointer_words:1 x
      end
      module Interface = struct
        type t = rw StructStorage.t
        type t_Interface_17116997365232503999 = t
        type reader_t = Reader.Type.Interface.t
        type reader_t_Interface_17116997365232503999 = reader_t
        type array_t = rw ListStorage.t
        type reader_array_t = ro ListStorage.t
        let typeId_get x = get_data_field x ~f:(get_uint64 ~default:Uint64.zero ~byte_ofs:8)
        let typeId_get_int_exn x = Uint64.to_int (typeId_get x)
        let typeId_set x v = get_data_field x ~f:(set_uint64 ~default:Uint64.zero ~byte_ofs:8 v)
        let typeId_set_int_exn x v = typeId_set x (Uint64.of_int v)
        let of_message x = get_root_struct ~data_words:2 ~pointer_words:1 x
      end
      module List = struct
        type t = rw StructStorage.t
        type t_List_9792858745991129751 = t
        type reader_t = Reader.Type.List.t
        type reader_t_List_9792858745991129751 = reader_t
        type array_t = rw ListStorage.t
        type reader_array_t = ro ListStorage.t
        let elementType_get x = get_pointer_field x 0 ~f:(get_struct ~data_words:2 ~pointer_words:1)
        let elementType_set x v = get_pointer_field x 0 ~f:(set_struct ~data_words:2 ~pointer_words:1 v)
        let elementType_init x = get_pointer_field x 0 ~f:(init_struct ~data_words:2 ~pointer_words:1)
        let of_message x = get_root_struct ~data_words:2 ~pointer_words:1 x
      end
      module Struct = struct
        type t = rw StructStorage.t
        type t_Struct_12410354185295152851 = t
        type reader_t = Reader.Type.Struct.t
        type reader_t_Struct_12410354185295152851 = reader_t
        type array_t = rw ListStorage.t
        type reader_array_t = ro ListStorage.t
        let typeId_get x = get_data_field x ~f:(get_uint64 ~default:Uint64.zero ~byte_ofs:8)
        let typeId_get_int_exn x = Uint64.to_int (typeId_get x)
        let typeId_set x v = get_data_field x ~f:(set_uint64 ~default:Uint64.zero ~byte_ofs:8 v)
        let typeId_set_int_exn x v = typeId_set x (Uint64.of_int v)
        let of_message x = get_root_struct ~data_words:2 ~pointer_words:1 x
      end
      let void_get x = ()
      let void_set x = get_data_field ~discr:{Discr.value=0; Discr.byte_ofs=0} x ~f:set_void
      let bool_get x = ()
      let bool_set x = get_data_field ~discr:{Discr.value=1; Discr.byte_ofs=0} x ~f:set_void
      let int8_get x = ()
      let int8_set x = get_data_field ~discr:{Discr.value=2; Discr.byte_ofs=0} x ~f:set_void
      let int16_get x = ()
      let int16_set x = get_data_field ~discr:{Discr.value=3; Discr.byte_ofs=0} x ~f:set_void
      let int32_get x = ()
      let int32_set x = get_data_field ~discr:{Discr.value=4; Discr.byte_ofs=0} x ~f:set_void
      let int64_get x = ()
      let int64_set x = get_data_field ~discr:{Discr.value=5; Discr.byte_ofs=0} x ~f:set_void
      let uint8_get x = ()
      let uint8_set x = get_data_field ~discr:{Discr.value=6; Discr.byte_ofs=0} x ~f:set_void
      let uint16_get x = ()
      let uint16_set x = get_data_field ~discr:{Discr.value=7; Discr.byte_ofs=0} x ~f:set_void
      let uint32_get x = ()
      let uint32_set x = get_data_field ~discr:{Discr.value=8; Discr.byte_ofs=0} x ~f:set_void
      let uint64_get x = ()
      let uint64_set x = get_data_field ~discr:{Discr.value=9; Discr.byte_ofs=0} x ~f:set_void
      let float32_get x = ()
      let float32_set x = get_data_field ~discr:{Discr.value=10; Discr.byte_ofs=0} x ~f:set_void
      let float64_get x = ()
      let float64_set x = get_data_field ~discr:{Discr.value=11; Discr.byte_ofs=0} x ~f:set_void
      let text_get x = ()
      let text_set x = get_data_field ~discr:{Discr.value=12; Discr.byte_ofs=0} x ~f:set_void
      let data_get x = ()
      let data_set x = get_data_field ~discr:{Discr.value=13; Discr.byte_ofs=0} x ~f:set_void
      let list_get x = x
      let list_set x v = failwith "not implemented"
      let list_init x n = failwith "not implemented"
      let enum_get x = x
      let enum_set x v = failwith "not implemented"
      let enum_init x n = failwith "not implemented"
      let struct_get x = x
      let struct_set x v = failwith "not implemented"
      let struct_init x n = failwith "not implemented"
      let interface_get x = x
      let interface_set x v = failwith "not implemented"
      let interface_init x n = failwith "not implemented"
      let anyPointer_get x = ()
      let anyPointer_set x = get_data_field ~discr:{Discr.value=18; Discr.byte_ofs=0} x ~f:set_void
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
        | AnyPointer
        | Undefined_ of int
      let unnamed_union_get x =
        match get_data_field x ~f:(get_uint16 ~default:0 ~byte_ofs:0) with
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
        | v -> Undefined_ v
      let of_message x = get_root_struct ~data_words:2 ~pointer_words:1 x
    end
    module Value = struct
      type t = rw StructStorage.t
      type t_Value_14853958794117909659 = t
      type reader_t = Reader.Value.t
      type reader_t_Value_14853958794117909659 = reader_t
      type array_t = rw ListStorage.t
      type reader_array_t = ro ListStorage.t
      let void_get x = ()
      let void_set x = get_data_field ~discr:{Discr.value=0; Discr.byte_ofs=0} x ~f:set_void
      let bool_get x = get_data_field x ~f:(get_bit ~default:false ~byte_ofs:2 ~bit_ofs:0)
      let bool_set x v = get_data_field ~discr:{Discr.value=1; Discr.byte_ofs=0} x ~f:(set_bit ~default:false ~byte_ofs:2 ~bit_ofs:0 v)
      let int8_get x = get_data_field x ~f:(get_int8 ~default:0 ~byte_ofs:2)
      let int8_set x v = get_data_field ~discr:{Discr.value=2; Discr.byte_ofs=0} x ~f:(set_int8 ~default:0 ~byte_ofs:2 v)
      let int16_get x = get_data_field x ~f:(get_int16 ~default:0 ~byte_ofs:2)
      let int16_set x v = get_data_field ~discr:{Discr.value=3; Discr.byte_ofs=0} x ~f:(set_int16 ~default:0 ~byte_ofs:2 v)
      let int32_get x = get_data_field x ~f:(get_int32 ~default:0l ~byte_ofs:4)
      let int32_get_int_exn x = Int32.to_int (int32_get x)
      let int32_set x v = get_data_field ~discr:{Discr.value=4; Discr.byte_ofs=0} x ~f:(set_int32 ~default:0l ~byte_ofs:4 v)
      let int32_set_int_exn x v = int32_set x (Int32.of_int v)
      let int64_get x = get_data_field x ~f:(get_int64 ~default:0L ~byte_ofs:8)
      let int64_get_int_exn x = Int64.to_int (int64_get x)
      let int64_set x v = get_data_field ~discr:{Discr.value=5; Discr.byte_ofs=0} x ~f:(set_int64 ~default:0L ~byte_ofs:8 v)
      let int64_set_int_exn x v = int64_set x (Int64.of_int v)
      let uint8_get x = get_data_field x ~f:(get_uint8 ~default:0 ~byte_ofs:2)
      let uint8_set x v = get_data_field ~discr:{Discr.value=6; Discr.byte_ofs=0} x ~f:(set_uint8 ~default:0 ~byte_ofs:2 v)
      let uint16_get x = get_data_field x ~f:(get_uint16 ~default:0 ~byte_ofs:1)
      let uint16_set x v = get_data_field ~discr:{Discr.value=7; Discr.byte_ofs=0} x ~f:(set_uint16 ~default:0 ~byte_ofs:1 v)
      let uint32_get x = get_data_field x ~f:(get_uint32 ~default:Uint32.zero ~byte_ofs:4)
      let uint32_get_int_exn x = Uint32.to_int (uint32_get x)
      let uint32_set x v = get_data_field ~discr:{Discr.value=8; Discr.byte_ofs=0} x ~f:(set_uint32 ~default:Uint32.zero ~byte_ofs:4 v)
      let uint32_set_int_exn x v = uint32_set x (Uint32.of_int v)
      let uint64_get x = get_data_field x ~f:(get_uint64 ~default:Uint64.zero ~byte_ofs:8)
      let uint64_get_int_exn x = Uint64.to_int (uint64_get x)
      let uint64_set x v = get_data_field ~discr:{Discr.value=9; Discr.byte_ofs=0} x ~f:(set_uint64 ~default:Uint64.zero ~byte_ofs:8 v)
      let uint64_set_int_exn x v = uint64_set x (Uint64.of_int v)
      let float32_get x = get_data_field x ~f:(get_float32 ~default_bits:0l ~byte_ofs:4)
      let float32_set x v = get_data_field ~discr:{Discr.value=10; Discr.byte_ofs=0} x ~f:(set_float32 ~default_bits:0l ~byte_ofs:4 v)
      let float64_get x = get_data_field x ~f:(get_float64 ~default_bits:0L ~byte_ofs:8)
      let float64_set x v = get_data_field ~discr:{Discr.value=11; Discr.byte_ofs=0} x ~f:(set_float64 ~default_bits:0L ~byte_ofs:8 v)
      let text_get x = get_pointer_field x 0 ~f:(get_text ~default:"")
      let text_set x v = get_pointer_field ~discr:{Discr.value=12; Discr.byte_ofs=0} x 0 ~f:(set_text v)
      let data_get x = get_pointer_field x 0 ~f:(get_blob ~default:"")
      let data_set x v = get_pointer_field ~discr:{Discr.value=13; Discr.byte_ofs=0} x 0 ~f:(set_blob v)
      let list_get x = get_pointer_field x 0 ~f:(fun s -> s)
      let list_set x v = failwith "not implemented"
      let enum_get x = get_data_field x ~f:(get_uint16 ~default:0 ~byte_ofs:1)
      let enum_set x v = get_data_field ~discr:{Discr.value=15; Discr.byte_ofs=0} x ~f:(set_uint16 ~default:0 ~byte_ofs:1 v)
      let struct_get x = get_pointer_field x 0 ~f:(fun s -> s)
      let struct_set x v = failwith "not implemented"
      let interface_get x = ()
      let interface_set x = get_data_field ~discr:{Discr.value=17; Discr.byte_ofs=0} x ~f:set_void
      let anyPointer_get x = get_pointer_field x 0 ~f:(fun s -> s)
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
        | List of AnyPointer.t
        | Enum of int
        | Struct of AnyPointer.t
        | Interface
        | AnyPointer of AnyPointer.t
        | Undefined_ of int
      let unnamed_union_get x =
        match get_data_field x ~f:(get_uint16 ~default:0 ~byte_ofs:0) with
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
        | v -> Undefined_ v
      let of_message x = get_root_struct ~data_words:2 ~pointer_words:1 x
    end
    module Annotation = struct
      type t = rw StructStorage.t
      type t_Annotation_17422339044421236034 = t
      type reader_t = Reader.Annotation.t
      type reader_t_Annotation_17422339044421236034 = reader_t
      type array_t = rw ListStorage.t
      type reader_array_t = ro ListStorage.t
      let id_get x = get_data_field x ~f:(get_uint64 ~default:Uint64.zero ~byte_ofs:0)
      let id_get_int_exn x = Uint64.to_int (id_get x)
      let id_set x v = get_data_field x ~f:(set_uint64 ~default:Uint64.zero ~byte_ofs:0 v)
      let id_set_int_exn x v = id_set x (Uint64.of_int v)
      let value_get x = get_pointer_field x 0 ~f:(get_struct ~data_words:2 ~pointer_words:1)
      let value_set x v = get_pointer_field x 0 ~f:(set_struct ~data_words:2 ~pointer_words:1 v)
      let value_init x = get_pointer_field x 0 ~f:(init_struct ~data_words:2 ~pointer_words:1)
      let of_message x = get_root_struct ~data_words:1 ~pointer_words:1 x
    end
    module Method = struct
      type t = rw StructStorage.t
      type t_Method_10736806783679155584 = t
      type reader_t = Reader.Method.t
      type reader_t_Method_10736806783679155584 = reader_t
      type array_t = rw ListStorage.t
      type reader_array_t = ro ListStorage.t
      let name_get x = get_pointer_field x 0 ~f:(get_text ~default:"")
      let name_set x v = get_pointer_field x 0 ~f:(set_text v)
      let codeOrder_get x = get_data_field x ~f:(get_uint16 ~default:0 ~byte_ofs:0)
      let codeOrder_set x v = get_data_field x ~f:(set_uint16 ~default:0 ~byte_ofs:0 v)
      let paramStructType_get x = get_data_field x ~f:(get_uint64 ~default:Uint64.zero ~byte_ofs:8)
      let paramStructType_get_int_exn x = Uint64.to_int (paramStructType_get x)
      let paramStructType_set x v = get_data_field x ~f:(set_uint64 ~default:Uint64.zero ~byte_ofs:8 v)
      let paramStructType_set_int_exn x v = paramStructType_set x (Uint64.of_int v)
      let resultStructType_get x = get_data_field x ~f:(get_uint64 ~default:Uint64.zero ~byte_ofs:16)
      let resultStructType_get_int_exn x = Uint64.to_int (resultStructType_get x)
      let resultStructType_set x v = get_data_field x ~f:(set_uint64 ~default:Uint64.zero ~byte_ofs:16 v)
      let resultStructType_set_int_exn x v = resultStructType_set x (Uint64.of_int v)
      let annotations_get x = get_pointer_field x 1 ~f:(get_struct_list ~data_words:1 ~pointer_words:1)
      let annotations_set x v = get_pointer_field x 1 ~f:(set_struct_list ~data_words:1 ~pointer_words:1 v)
      let annotations_init x n = get_pointer_field x 1 ~f:(init_struct_list ~data_words:1 ~pointer_words:1 n)
      let of_message x = get_root_struct ~data_words:3 ~pointer_words:2 x
    end
    module Enumerant = struct
      type t = rw StructStorage.t
      type t_Enumerant_10919677598968879693 = t
      type reader_t = Reader.Enumerant.t
      type reader_t_Enumerant_10919677598968879693 = reader_t
      type array_t = rw ListStorage.t
      type reader_array_t = ro ListStorage.t
      let name_get x = get_pointer_field x 0 ~f:(get_text ~default:"")
      let name_set x v = get_pointer_field x 0 ~f:(set_text v)
      let codeOrder_get x = get_data_field x ~f:(get_uint16 ~default:0 ~byte_ofs:0)
      let codeOrder_set x v = get_data_field x ~f:(set_uint16 ~default:0 ~byte_ofs:0 v)
      let annotations_get x = get_pointer_field x 1 ~f:(get_struct_list ~data_words:1 ~pointer_words:1)
      let annotations_set x v = get_pointer_field x 1 ~f:(set_struct_list ~data_words:1 ~pointer_words:1 v)
      let annotations_init x n = get_pointer_field x 1 ~f:(init_struct_list ~data_words:1 ~pointer_words:1 n)
      let of_message x = get_root_struct ~data_words:1 ~pointer_words:2 x
    end
    module Field = struct
      type t = rw StructStorage.t
      type t_Field_11145653318641710175 = t
      type reader_t = Reader.Field.t
      type reader_t_Field_11145653318641710175 = reader_t
      type array_t = rw ListStorage.t
      type reader_array_t = ro ListStorage.t
      let noDiscriminant = 65535
      module Ordinal = struct
        type t = rw StructStorage.t
        type t_Ordinal_13515537513213004774 = t
        type reader_t = Reader.Field.Ordinal.t
        type reader_t_Ordinal_13515537513213004774 = reader_t
        type array_t = rw ListStorage.t
        type reader_array_t = ro ListStorage.t
        let implicit_get x = ()
        let implicit_set x = get_data_field ~discr:{Discr.value=0; Discr.byte_ofs=10} x ~f:set_void
        let explicit_get x = get_data_field x ~f:(get_uint16 ~default:0 ~byte_ofs:6)
        let explicit_set x v = get_data_field ~discr:{Discr.value=1; Discr.byte_ofs=10} x ~f:(set_uint16 ~default:0 ~byte_ofs:6 v)
        type unnamed_union_t =
          | Implicit
          | Explicit of int
          | Undefined_ of int
        let unnamed_union_get x =
          match get_data_field x ~f:(get_uint16 ~default:0 ~byte_ofs:10) with
          | 0 -> Implicit
          | 1 -> Explicit (explicit_get x)
          | v -> Undefined_ v
        let of_message x = get_root_struct ~data_words:3 ~pointer_words:4 x
      end
      module Group = struct
        type t = rw StructStorage.t
        type t_Group_14626792032033250577 = t
        type reader_t = Reader.Field.Group.t
        type reader_t_Group_14626792032033250577 = reader_t
        type array_t = rw ListStorage.t
        type reader_array_t = ro ListStorage.t
        let typeId_get x = get_data_field x ~f:(get_uint64 ~default:Uint64.zero ~byte_ofs:16)
        let typeId_get_int_exn x = Uint64.to_int (typeId_get x)
        let typeId_set x v = get_data_field x ~f:(set_uint64 ~default:Uint64.zero ~byte_ofs:16 v)
        let typeId_set_int_exn x v = typeId_set x (Uint64.of_int v)
        let of_message x = get_root_struct ~data_words:3 ~pointer_words:4 x
      end
      module Slot = struct
        type t = rw StructStorage.t
        type t_Slot_14133145859926553711 = t
        type reader_t = Reader.Field.Slot.t
        type reader_t_Slot_14133145859926553711 = reader_t
        type array_t = rw ListStorage.t
        type reader_array_t = ro ListStorage.t
        let offset_get x = get_data_field x ~f:(get_uint32 ~default:Uint32.zero ~byte_ofs:4)
        let offset_get_int_exn x = Uint32.to_int (offset_get x)
        let offset_set x v = get_data_field x ~f:(set_uint32 ~default:Uint32.zero ~byte_ofs:4 v)
        let offset_set_int_exn x v = offset_set x (Uint32.of_int v)
        let type_get x = get_pointer_field x 2 ~f:(get_struct ~data_words:2 ~pointer_words:1)
        let type_set x v = get_pointer_field x 2 ~f:(set_struct ~data_words:2 ~pointer_words:1 v)
        let type_init x = get_pointer_field x 2 ~f:(init_struct ~data_words:2 ~pointer_words:1)
        let defaultValue_get x = get_pointer_field x 3 ~f:(get_struct ~data_words:2 ~pointer_words:1)
        let defaultValue_set x v = get_pointer_field x 3 ~f:(set_struct ~data_words:2 ~pointer_words:1 v)
        let defaultValue_init x = get_pointer_field x 3 ~f:(init_struct ~data_words:2 ~pointer_words:1)
        let hadExplicitDefault_get x = get_data_field x ~f:(get_bit ~default:false ~byte_ofs:16 ~bit_ofs:0)
        let hadExplicitDefault_set x v = get_data_field x ~f:(set_bit ~default:false ~byte_ofs:16 ~bit_ofs:0 v)
        let of_message x = get_root_struct ~data_words:3 ~pointer_words:4 x
      end
      let name_get x = get_pointer_field x 0 ~f:(get_text ~default:"")
      let name_set x v = get_pointer_field x 0 ~f:(set_text v)
      let codeOrder_get x = get_data_field x ~f:(get_uint16 ~default:0 ~byte_ofs:0)
      let codeOrder_set x v = get_data_field x ~f:(set_uint16 ~default:0 ~byte_ofs:0 v)
      let annotations_get x = get_pointer_field x 1 ~f:(get_struct_list ~data_words:1 ~pointer_words:1)
      let annotations_set x v = get_pointer_field x 1 ~f:(set_struct_list ~data_words:1 ~pointer_words:1 v)
      let annotations_init x n = get_pointer_field x 1 ~f:(init_struct_list ~data_words:1 ~pointer_words:1 n)
      let discriminantValue_get x = get_data_field x ~f:(get_uint16 ~default:65535 ~byte_ofs:1)
      let discriminantValue_set x v = get_data_field x ~f:(set_uint16 ~default:65535 ~byte_ofs:1 v)
      let slot_get x = x
      let slot_set x v = failwith "not implemented"
      let slot_init x n = failwith "not implemented"
      let group_get x = x
      let group_set x v = failwith "not implemented"
      let group_init x n = failwith "not implemented"
      let ordinal_get x = x
      let ordinal_set x v = failwith "not implemented"
      let ordinal_init x n = failwith "not implemented"
      type unnamed_union_t =
        | Slot of Slot.t
        | Group of Group.t
        | Undefined_ of int
      let unnamed_union_get x =
        match get_data_field x ~f:(get_uint16 ~default:0 ~byte_ofs:8) with
        | 0 -> Slot (slot_get x)
        | 1 -> Group (group_get x)
        | v -> Undefined_ v
      let of_message x = get_root_struct ~data_words:3 ~pointer_words:4 x
    end
    module Node = struct
      type t = rw StructStorage.t
      type t_Node_16610026722781537303 = t
      type reader_t = Reader.Node.t
      type reader_t_Node_16610026722781537303 = reader_t
      type array_t = rw ListStorage.t
      type reader_array_t = ro ListStorage.t
      module Struct = struct
        type t = rw StructStorage.t
        type t_Struct_11430331134483579957 = t
        type reader_t = Reader.Node.Struct.t
        type reader_t_Struct_11430331134483579957 = reader_t
        type array_t = rw ListStorage.t
        type reader_array_t = ro ListStorage.t
        let dataWordCount_get x = get_data_field x ~f:(get_uint16 ~default:0 ~byte_ofs:7)
        let dataWordCount_set x v = get_data_field x ~f:(set_uint16 ~default:0 ~byte_ofs:7 v)
        let pointerCount_get x = get_data_field x ~f:(get_uint16 ~default:0 ~byte_ofs:12)
        let pointerCount_set x v = get_data_field x ~f:(set_uint16 ~default:0 ~byte_ofs:12 v)
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
          decode (get_data_field x ~f:(get_uint16 ~default:0 ~byte_ofs:26))
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
                      | ElementSize.Undefined_ _ ->
                          invalid_msg "Cannot encode undefined enum value.")
          in
          get_data_field x ~f:(set_uint16 ~default:0 ~byte_ofs:26 (encode e))
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
                     | ElementSize.Undefined_ x -> x)
          in
          get_data_field x ~f:(set_uint16 ~default:0 ~byte_ofs:26 (encode e))
        let isGroup_get x = get_data_field x ~f:(get_bit ~default:false ~byte_ofs:28 ~bit_ofs:0)
        let isGroup_set x v = get_data_field x ~f:(set_bit ~default:false ~byte_ofs:28 ~bit_ofs:0 v)
        let discriminantCount_get x = get_data_field x ~f:(get_uint16 ~default:0 ~byte_ofs:15)
        let discriminantCount_set x v = get_data_field x ~f:(set_uint16 ~default:0 ~byte_ofs:15 v)
        let discriminantOffset_get x = get_data_field x ~f:(get_uint32 ~default:Uint32.zero ~byte_ofs:32)
        let discriminantOffset_get_int_exn x = Uint32.to_int (discriminantOffset_get x)
        let discriminantOffset_set x v = get_data_field x ~f:(set_uint32 ~default:Uint32.zero ~byte_ofs:32 v)
        let discriminantOffset_set_int_exn x v = discriminantOffset_set x (Uint32.of_int v)
        let fields_get x = get_pointer_field x 3 ~f:(get_struct_list ~data_words:3 ~pointer_words:4)
        let fields_set x v = get_pointer_field x 3 ~f:(set_struct_list ~data_words:3 ~pointer_words:4 v)
        let fields_init x n = get_pointer_field x 3 ~f:(init_struct_list ~data_words:3 ~pointer_words:4 n)
        let of_message x = get_root_struct ~data_words:5 ~pointer_words:5 x
      end
      module Enum = struct
        type t = rw StructStorage.t
        type t_Enum_13063450714778629528 = t
        type reader_t = Reader.Node.Enum.t
        type reader_t_Enum_13063450714778629528 = reader_t
        type array_t = rw ListStorage.t
        type reader_array_t = ro ListStorage.t
        let enumerants_get x = get_pointer_field x 3 ~f:(get_struct_list ~data_words:1 ~pointer_words:2)
        let enumerants_set x v = get_pointer_field x 3 ~f:(set_struct_list ~data_words:1 ~pointer_words:2 v)
        let enumerants_init x n = get_pointer_field x 3 ~f:(init_struct_list ~data_words:1 ~pointer_words:2 n)
        let of_message x = get_root_struct ~data_words:5 ~pointer_words:5 x
      end
      module Annotation = struct
        type t = rw StructStorage.t
        type t_Annotation_17011813041836786320 = t
        type reader_t = Reader.Node.Annotation.t
        type reader_t_Annotation_17011813041836786320 = reader_t
        type array_t = rw ListStorage.t
        type reader_array_t = ro ListStorage.t
        let type_get x = get_pointer_field x 3 ~f:(get_struct ~data_words:2 ~pointer_words:1)
        let type_set x v = get_pointer_field x 3 ~f:(set_struct ~data_words:2 ~pointer_words:1 v)
        let type_init x = get_pointer_field x 3 ~f:(init_struct ~data_words:2 ~pointer_words:1)
        let targetsFile_get x = get_data_field x ~f:(get_bit ~default:false ~byte_ofs:14 ~bit_ofs:0)
        let targetsFile_set x v = get_data_field x ~f:(set_bit ~default:false ~byte_ofs:14 ~bit_ofs:0 v)
        let targetsConst_get x = get_data_field x ~f:(get_bit ~default:false ~byte_ofs:14 ~bit_ofs:1)
        let targetsConst_set x v = get_data_field x ~f:(set_bit ~default:false ~byte_ofs:14 ~bit_ofs:1 v)
        let targetsEnum_get x = get_data_field x ~f:(get_bit ~default:false ~byte_ofs:14 ~bit_ofs:2)
        let targetsEnum_set x v = get_data_field x ~f:(set_bit ~default:false ~byte_ofs:14 ~bit_ofs:2 v)
        let targetsEnumerant_get x = get_data_field x ~f:(get_bit ~default:false ~byte_ofs:14 ~bit_ofs:3)
        let targetsEnumerant_set x v = get_data_field x ~f:(set_bit ~default:false ~byte_ofs:14 ~bit_ofs:3 v)
        let targetsStruct_get x = get_data_field x ~f:(get_bit ~default:false ~byte_ofs:14 ~bit_ofs:4)
        let targetsStruct_set x v = get_data_field x ~f:(set_bit ~default:false ~byte_ofs:14 ~bit_ofs:4 v)
        let targetsField_get x = get_data_field x ~f:(get_bit ~default:false ~byte_ofs:14 ~bit_ofs:5)
        let targetsField_set x v = get_data_field x ~f:(set_bit ~default:false ~byte_ofs:14 ~bit_ofs:5 v)
        let targetsUnion_get x = get_data_field x ~f:(get_bit ~default:false ~byte_ofs:14 ~bit_ofs:6)
        let targetsUnion_set x v = get_data_field x ~f:(set_bit ~default:false ~byte_ofs:14 ~bit_ofs:6 v)
        let targetsGroup_get x = get_data_field x ~f:(get_bit ~default:false ~byte_ofs:14 ~bit_ofs:7)
        let targetsGroup_set x v = get_data_field x ~f:(set_bit ~default:false ~byte_ofs:14 ~bit_ofs:7 v)
        let targetsInterface_get x = get_data_field x ~f:(get_bit ~default:false ~byte_ofs:15 ~bit_ofs:0)
        let targetsInterface_set x v = get_data_field x ~f:(set_bit ~default:false ~byte_ofs:15 ~bit_ofs:0 v)
        let targetsMethod_get x = get_data_field x ~f:(get_bit ~default:false ~byte_ofs:15 ~bit_ofs:1)
        let targetsMethod_set x v = get_data_field x ~f:(set_bit ~default:false ~byte_ofs:15 ~bit_ofs:1 v)
        let targetsParam_get x = get_data_field x ~f:(get_bit ~default:false ~byte_ofs:15 ~bit_ofs:2)
        let targetsParam_set x v = get_data_field x ~f:(set_bit ~default:false ~byte_ofs:15 ~bit_ofs:2 v)
        let targetsAnnotation_get x = get_data_field x ~f:(get_bit ~default:false ~byte_ofs:15 ~bit_ofs:3)
        let targetsAnnotation_set x v = get_data_field x ~f:(set_bit ~default:false ~byte_ofs:15 ~bit_ofs:3 v)
        let of_message x = get_root_struct ~data_words:5 ~pointer_words:5 x
      end
      module Const = struct
        type t = rw StructStorage.t
        type t_Const_12793219851699983392 = t
        type reader_t = Reader.Node.Const.t
        type reader_t_Const_12793219851699983392 = reader_t
        type array_t = rw ListStorage.t
        type reader_array_t = ro ListStorage.t
        let type_get x = get_pointer_field x 3 ~f:(get_struct ~data_words:2 ~pointer_words:1)
        let type_set x v = get_pointer_field x 3 ~f:(set_struct ~data_words:2 ~pointer_words:1 v)
        let type_init x = get_pointer_field x 3 ~f:(init_struct ~data_words:2 ~pointer_words:1)
        let value_get x = get_pointer_field x 4 ~f:(get_struct ~data_words:2 ~pointer_words:1)
        let value_set x v = get_pointer_field x 4 ~f:(set_struct ~data_words:2 ~pointer_words:1 v)
        let value_init x = get_pointer_field x 4 ~f:(init_struct ~data_words:2 ~pointer_words:1)
        let of_message x = get_root_struct ~data_words:5 ~pointer_words:5 x
      end
      module Interface = struct
        type t = rw StructStorage.t
        type t_Interface_16728431493453586831 = t
        type reader_t = Reader.Node.Interface.t
        type reader_t_Interface_16728431493453586831 = reader_t
        type array_t = rw ListStorage.t
        type reader_array_t = ro ListStorage.t
        let methods_get x = get_pointer_field x 3 ~f:(get_struct_list ~data_words:3 ~pointer_words:2)
        let methods_set x v = get_pointer_field x 3 ~f:(set_struct_list ~data_words:3 ~pointer_words:2 v)
        let methods_init x n = get_pointer_field x 3 ~f:(init_struct_list ~data_words:3 ~pointer_words:2 n)
        let extends_get x = get_pointer_field x 4 ~f:get_uint64_list
        let extends_set x v = get_pointer_field x 4 ~f:(set_uint64_list v)
        let extends_init x n = get_pointer_field x 4 ~f:(init_uint64_list n)
        let of_message x = get_root_struct ~data_words:5 ~pointer_words:5 x
      end
      module NestedNode = struct
        type t = rw StructStorage.t
        type t_NestedNode_16050641862814319170 = t
        type reader_t = Reader.Node.NestedNode.t
        type reader_t_NestedNode_16050641862814319170 = reader_t
        type array_t = rw ListStorage.t
        type reader_array_t = ro ListStorage.t
        let name_get x = get_pointer_field x 0 ~f:(get_text ~default:"")
        let name_set x v = get_pointer_field x 0 ~f:(set_text v)
        let id_get x = get_data_field x ~f:(get_uint64 ~default:Uint64.zero ~byte_ofs:0)
        let id_get_int_exn x = Uint64.to_int (id_get x)
        let id_set x v = get_data_field x ~f:(set_uint64 ~default:Uint64.zero ~byte_ofs:0 v)
        let id_set_int_exn x v = id_set x (Uint64.of_int v)
        let of_message x = get_root_struct ~data_words:1 ~pointer_words:1 x
      end
      let id_get x = get_data_field x ~f:(get_uint64 ~default:Uint64.zero ~byte_ofs:0)
      let id_get_int_exn x = Uint64.to_int (id_get x)
      let id_set x v = get_data_field x ~f:(set_uint64 ~default:Uint64.zero ~byte_ofs:0 v)
      let id_set_int_exn x v = id_set x (Uint64.of_int v)
      let displayName_get x = get_pointer_field x 0 ~f:(get_text ~default:"")
      let displayName_set x v = get_pointer_field x 0 ~f:(set_text v)
      let displayNamePrefixLength_get x = get_data_field x ~f:(get_uint32 ~default:Uint32.zero ~byte_ofs:8)
      let displayNamePrefixLength_get_int_exn x = Uint32.to_int (displayNamePrefixLength_get x)
      let displayNamePrefixLength_set x v = get_data_field x ~f:(set_uint32 ~default:Uint32.zero ~byte_ofs:8 v)
      let displayNamePrefixLength_set_int_exn x v = displayNamePrefixLength_set x (Uint32.of_int v)
      let scopeId_get x = get_data_field x ~f:(get_uint64 ~default:Uint64.zero ~byte_ofs:16)
      let scopeId_get_int_exn x = Uint64.to_int (scopeId_get x)
      let scopeId_set x v = get_data_field x ~f:(set_uint64 ~default:Uint64.zero ~byte_ofs:16 v)
      let scopeId_set_int_exn x v = scopeId_set x (Uint64.of_int v)
      let nestedNodes_get x = get_pointer_field x 1 ~f:(get_struct_list ~data_words:1 ~pointer_words:1)
      let nestedNodes_set x v = get_pointer_field x 1 ~f:(set_struct_list ~data_words:1 ~pointer_words:1 v)
      let nestedNodes_init x n = get_pointer_field x 1 ~f:(init_struct_list ~data_words:1 ~pointer_words:1 n)
      let annotations_get x = get_pointer_field x 2 ~f:(get_struct_list ~data_words:1 ~pointer_words:1)
      let annotations_set x v = get_pointer_field x 2 ~f:(set_struct_list ~data_words:1 ~pointer_words:1 v)
      let annotations_init x n = get_pointer_field x 2 ~f:(init_struct_list ~data_words:1 ~pointer_words:1 n)
      let file_get x = ()
      let file_set x = get_data_field ~discr:{Discr.value=0; Discr.byte_ofs=12} x ~f:set_void
      let struct_get x = x
      let struct_set x v = failwith "not implemented"
      let struct_init x n = failwith "not implemented"
      let enum_get x = x
      let enum_set x v = failwith "not implemented"
      let enum_init x n = failwith "not implemented"
      let interface_get x = x
      let interface_set x v = failwith "not implemented"
      let interface_init x n = failwith "not implemented"
      let const_get x = x
      let const_set x v = failwith "not implemented"
      let const_init x n = failwith "not implemented"
      let annotation_get x = x
      let annotation_set x v = failwith "not implemented"
      let annotation_init x n = failwith "not implemented"
      type unnamed_union_t =
        | File
        | Struct of Struct.t
        | Enum of Enum.t
        | Interface of Interface.t
        | Const of Const.t
        | Annotation of Annotation.t
        | Undefined_ of int
      let unnamed_union_get x =
        match get_data_field x ~f:(get_uint16 ~default:0 ~byte_ofs:12) with
        | 0 -> File
        | 1 -> Struct (struct_get x)
        | 2 -> Enum (enum_get x)
        | 3 -> Interface (interface_get x)
        | 4 -> Const (const_get x)
        | 5 -> Annotation (annotation_get x)
        | v -> Undefined_ v
      let of_message x = get_root_struct ~data_words:5 ~pointer_words:5 x
    end
    module CodeGeneratorRequest = struct
      type t = rw StructStorage.t
      type t_CodeGeneratorRequest_13818529054586492878 = t
      type reader_t = Reader.CodeGeneratorRequest.t
      type reader_t_CodeGeneratorRequest_13818529054586492878 = reader_t
      type array_t = rw ListStorage.t
      type reader_array_t = ro ListStorage.t
      module RequestedFile = struct
        type t = rw StructStorage.t
        type t_RequestedFile_14981803260258615394 = t
        type reader_t = Reader.CodeGeneratorRequest.RequestedFile.t
        type reader_t_RequestedFile_14981803260258615394 = reader_t
        type array_t = rw ListStorage.t
        type reader_array_t = ro ListStorage.t
        module Import = struct
          type t = rw StructStorage.t
          type t_Import_12560611460656617445 = t
          type reader_t = Reader.CodeGeneratorRequest.RequestedFile.Import.t
          type reader_t_Import_12560611460656617445 = reader_t
          type array_t = rw ListStorage.t
          type reader_array_t = ro ListStorage.t
          let id_get x = get_data_field x ~f:(get_uint64 ~default:Uint64.zero ~byte_ofs:0)
          let id_get_int_exn x = Uint64.to_int (id_get x)
          let id_set x v = get_data_field x ~f:(set_uint64 ~default:Uint64.zero ~byte_ofs:0 v)
          let id_set_int_exn x v = id_set x (Uint64.of_int v)
          let name_get x = get_pointer_field x 0 ~f:(get_text ~default:"")
          let name_set x v = get_pointer_field x 0 ~f:(set_text v)
          let of_message x = get_root_struct ~data_words:1 ~pointer_words:1 x
        end
        let id_get x = get_data_field x ~f:(get_uint64 ~default:Uint64.zero ~byte_ofs:0)
        let id_get_int_exn x = Uint64.to_int (id_get x)
        let id_set x v = get_data_field x ~f:(set_uint64 ~default:Uint64.zero ~byte_ofs:0 v)
        let id_set_int_exn x v = id_set x (Uint64.of_int v)
        let filename_get x = get_pointer_field x 0 ~f:(get_text ~default:"")
        let filename_set x v = get_pointer_field x 0 ~f:(set_text v)
        let imports_get x = get_pointer_field x 1 ~f:(get_struct_list ~data_words:1 ~pointer_words:1)
        let imports_set x v = get_pointer_field x 1 ~f:(set_struct_list ~data_words:1 ~pointer_words:1 v)
        let imports_init x n = get_pointer_field x 1 ~f:(init_struct_list ~data_words:1 ~pointer_words:1 n)
        let of_message x = get_root_struct ~data_words:1 ~pointer_words:2 x
      end
      let nodes_get x = get_pointer_field x 0 ~f:(get_struct_list ~data_words:5 ~pointer_words:5)
      let nodes_set x v = get_pointer_field x 0 ~f:(set_struct_list ~data_words:5 ~pointer_words:5 v)
      let nodes_init x n = get_pointer_field x 0 ~f:(init_struct_list ~data_words:5 ~pointer_words:5 n)
      let requestedFiles_get x = get_pointer_field x 1 ~f:(get_struct_list ~data_words:1 ~pointer_words:2)
      let requestedFiles_set x v = get_pointer_field x 1 ~f:(set_struct_list ~data_words:1 ~pointer_words:2 v)
      let requestedFiles_init x n = get_pointer_field x 1 ~f:(init_struct_list ~data_words:1 ~pointer_words:2 n)
      let of_message x = get_root_struct ~data_words:0 ~pointer_words:2 x
    end  end
end

