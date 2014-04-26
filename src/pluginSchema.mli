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

module Make (MessageWrapper : Message.S) :
  (S with type Reader.message_t = Message.ro MessageWrapper.Message.t
    and type Builder.message_t = Message.rw MessageWrapper.Message.t
    and type Reader.AnyPointer.t = Message.ro MessageWrapper.Slice.t option)

