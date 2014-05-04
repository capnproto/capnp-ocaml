type ro = Capnp.Message.ro
type rw = Capnp.Message.rw

module type S = sig
  type 'cap message_t

  module Reader : sig
    type array_t
    type builder_array_t
    type pointer_t
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
      type t
      type builder_t
      type t_Type_15020482145304562784 = t
      type builder_t_Type_15020482145304562784 = builder_t
      module Enum : sig
        type t
        type builder_t
        type t_Enum_11389172934837766057 = t
        type builder_t_Enum_11389172934837766057 = builder_t
        val type_id_get : t -> Uint64.t
        val type_id_get_int_exn : t -> int
        val of_message : 'cap message_t -> t
      end
      module Interface : sig
        type t
        type builder_t
        type t_Interface_17116997365232503999 = t
        type builder_t_Interface_17116997365232503999 = builder_t
        val type_id_get : t -> Uint64.t
        val type_id_get_int_exn : t -> int
        val of_message : 'cap message_t -> t
      end
      module List : sig
        type t
        type builder_t
        type t_List_9792858745991129751 = t
        type builder_t_List_9792858745991129751 = builder_t
        val element_type_get : t -> t_Type_15020482145304562784
        val of_message : 'cap message_t -> t
      end
      module Struct : sig
        type t
        type builder_t
        type t_Struct_12410354185295152851 = t
        type builder_t_Struct_12410354185295152851 = builder_t
        val type_id_get : t -> Uint64.t
        val type_id_get_int_exn : t -> int
        val of_message : 'cap message_t -> t
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
        | Undefined of int
      val get : t -> unnamed_union_t
      val of_message : 'cap message_t -> t
    end
    module Value : sig
      type t
      type builder_t
      type t_Value_14853958794117909659 = t
      type builder_t_Value_14853958794117909659 = builder_t
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
        | List of pointer_t
        | Enum of int
        | Struct of pointer_t
        | Interface
        | AnyPointer of pointer_t
        | Undefined of int
      val get : t -> unnamed_union_t
      val of_message : 'cap message_t -> t
    end
    module Annotation : sig
      type t
      type builder_t
      type t_Annotation_17422339044421236034 = t
      type builder_t_Annotation_17422339044421236034 = builder_t
      val id_get : t -> Uint64.t
      val id_get_int_exn : t -> int
      val value_get : t -> Value.t
      val of_message : 'cap message_t -> t
    end
    module Method : sig
      type t
      type builder_t
      type t_Method_10736806783679155584 = t
      type builder_t_Method_10736806783679155584 = builder_t
      val name_get : t -> string
      val code_order_get : t -> int
      val param_struct_type_get : t -> Uint64.t
      val param_struct_type_get_int_exn : t -> int
      val result_struct_type_get : t -> Uint64.t
      val result_struct_type_get_int_exn : t -> int
      val annotations_get : t -> (ro, Annotation.t, array_t) Capnp.Runtime.Array.t
      val of_message : 'cap message_t -> t
    end
    module Enumerant : sig
      type t
      type builder_t
      type t_Enumerant_10919677598968879693 = t
      type builder_t_Enumerant_10919677598968879693 = builder_t
      val name_get : t -> string
      val code_order_get : t -> int
      val annotations_get : t -> (ro, Annotation.t, array_t) Capnp.Runtime.Array.t
      val of_message : 'cap message_t -> t
    end
    module Field : sig
      type t
      type builder_t
      type t_Field_11145653318641710175 = t
      type builder_t_Field_11145653318641710175 = builder_t
      val no_discriminant : int
      module Ordinal : sig
        type t
        type builder_t
        type t_Ordinal_13515537513213004774 = t
        type builder_t_Ordinal_13515537513213004774 = builder_t
        type unnamed_union_t =
          | Implicit
          | Explicit of int
          | Undefined of int
        val get : t -> unnamed_union_t
        val of_message : 'cap message_t -> t
      end
      module Group : sig
        type t
        type builder_t
        type t_Group_14626792032033250577 = t
        type builder_t_Group_14626792032033250577 = builder_t
        val type_id_get : t -> Uint64.t
        val type_id_get_int_exn : t -> int
        val of_message : 'cap message_t -> t
      end
      module Slot : sig
        type t
        type builder_t
        type t_Slot_14133145859926553711 = t
        type builder_t_Slot_14133145859926553711 = builder_t
        val offset_get : t -> Uint32.t
        val offset_get_int_exn : t -> int
        val type_get : t -> Type.t
        val default_value_get : t -> Value.t
        val had_explicit_default_get : t -> bool
        val of_message : 'cap message_t -> t
      end
      type unnamed_union_t =
        | Slot of Slot.t
        | Group of Group.t
        | Undefined of int
      val get : t -> unnamed_union_t
      val name_get : t -> string
      val code_order_get : t -> int
      val annotations_get : t -> (ro, Annotation.t, array_t) Capnp.Runtime.Array.t
      val discriminant_value_get : t -> int
      val ordinal_get : t -> Ordinal.t
      val of_message : 'cap message_t -> t
    end
    module Node : sig
      type t
      type builder_t
      type t_Node_16610026722781537303 = t
      type builder_t_Node_16610026722781537303 = builder_t
      module Struct : sig
        type t
        type builder_t
        type t_Struct_11430331134483579957 = t
        type builder_t_Struct_11430331134483579957 = builder_t
        val data_word_count_get : t -> int
        val pointer_count_get : t -> int
        val preferred_list_encoding_get : t -> ElementSize.t
        val is_group_get : t -> bool
        val discriminant_count_get : t -> int
        val discriminant_offset_get : t -> Uint32.t
        val discriminant_offset_get_int_exn : t -> int
        val fields_get : t -> (ro, Field.t, array_t) Capnp.Runtime.Array.t
        val of_message : 'cap message_t -> t
      end
      module Enum : sig
        type t
        type builder_t
        type t_Enum_13063450714778629528 = t
        type builder_t_Enum_13063450714778629528 = builder_t
        val enumerants_get : t -> (ro, Enumerant.t, array_t) Capnp.Runtime.Array.t
        val of_message : 'cap message_t -> t
      end
      module Annotation : sig
        type t
        type builder_t
        type t_Annotation_17011813041836786320 = t
        type builder_t_Annotation_17011813041836786320 = builder_t
        val type_get : t -> Type.t
        val targets_file_get : t -> bool
        val targets_const_get : t -> bool
        val targets_enum_get : t -> bool
        val targets_enumerant_get : t -> bool
        val targets_struct_get : t -> bool
        val targets_field_get : t -> bool
        val targets_union_get : t -> bool
        val targets_group_get : t -> bool
        val targets_interface_get : t -> bool
        val targets_method_get : t -> bool
        val targets_param_get : t -> bool
        val targets_annotation_get : t -> bool
        val of_message : 'cap message_t -> t
      end
      module Const : sig
        type t
        type builder_t
        type t_Const_12793219851699983392 = t
        type builder_t_Const_12793219851699983392 = builder_t
        val type_get : t -> Type.t
        val value_get : t -> Value.t
        val of_message : 'cap message_t -> t
      end
      module Interface : sig
        type t
        type builder_t
        type t_Interface_16728431493453586831 = t
        type builder_t_Interface_16728431493453586831 = builder_t
        val methods_get : t -> (ro, Method.t, array_t) Capnp.Runtime.Array.t
        val extends_get : t -> (ro, Uint64.t, array_t) Capnp.Runtime.Array.t
        val of_message : 'cap message_t -> t
      end
      module NestedNode : sig
        type t
        type builder_t
        type t_NestedNode_16050641862814319170 = t
        type builder_t_NestedNode_16050641862814319170 = builder_t
        val name_get : t -> string
        val id_get : t -> Uint64.t
        val id_get_int_exn : t -> int
        val of_message : 'cap message_t -> t
      end
      type unnamed_union_t =
        | File
        | Struct of Struct.t
        | Enum of Enum.t
        | Interface of Interface.t
        | Const of Const.t
        | Annotation of Annotation.t
        | Undefined of int
      val get : t -> unnamed_union_t
      val id_get : t -> Uint64.t
      val id_get_int_exn : t -> int
      val display_name_get : t -> string
      val display_name_prefix_length_get : t -> Uint32.t
      val display_name_prefix_length_get_int_exn : t -> int
      val scope_id_get : t -> Uint64.t
      val scope_id_get_int_exn : t -> int
      val nested_nodes_get : t -> (ro, NestedNode.t, array_t) Capnp.Runtime.Array.t
      val annotations_get : t -> (ro, Annotation.t, array_t) Capnp.Runtime.Array.t
      val of_message : 'cap message_t -> t
    end
    module CodeGeneratorRequest : sig
      type t
      type builder_t
      type t_CodeGeneratorRequest_13818529054586492878 = t
      type builder_t_CodeGeneratorRequest_13818529054586492878 = builder_t
      module RequestedFile : sig
        type t
        type builder_t
        type t_RequestedFile_14981803260258615394 = t
        type builder_t_RequestedFile_14981803260258615394 = builder_t
        module Import : sig
          type t
          type builder_t
          type t_Import_12560611460656617445 = t
          type builder_t_Import_12560611460656617445 = builder_t
          val id_get : t -> Uint64.t
          val id_get_int_exn : t -> int
          val name_get : t -> string
          val of_message : 'cap message_t -> t
        end
        val id_get : t -> Uint64.t
        val id_get_int_exn : t -> int
        val filename_get : t -> string
        val imports_get : t -> (ro, Import.t, array_t) Capnp.Runtime.Array.t
        val of_message : 'cap message_t -> t
      end
      val nodes_get : t -> (ro, Node.t, array_t) Capnp.Runtime.Array.t
      val requested_files_get : t -> (ro, RequestedFile.t, array_t) Capnp.Runtime.Array.t
      val of_message : 'cap message_t -> t
    end
  end

  module Builder : sig
    type array_t = Reader.builder_array_t
    type reader_array_t = Reader.array_t
    type pointer_t
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
        | Undefined of int
    end
    module Type : sig
      type t = Reader.Type.builder_t
      type reader_t = Reader.Type.t
      type t_Type_15020482145304562784 = t
      type reader_t_Type_15020482145304562784 = reader_t
      module Enum : sig
        type t = Reader.Type.Enum.builder_t
        type reader_t = Reader.Type.Enum.t
        type t_Enum_11389172934837766057 = t
        type reader_t_Enum_11389172934837766057 = reader_t
        val type_id_get : t -> Uint64.t
        val type_id_get_int_exn : t -> int
        val type_id_set : t -> Uint64.t -> unit
        val type_id_set_int_exn : t -> int -> unit
        val of_message : rw message_t -> t
        val to_message : t -> rw message_t
        val init_root : ?message_size:int -> unit -> t
      end
      module Interface : sig
        type t = Reader.Type.Interface.builder_t
        type reader_t = Reader.Type.Interface.t
        type t_Interface_17116997365232503999 = t
        type reader_t_Interface_17116997365232503999 = reader_t
        val type_id_get : t -> Uint64.t
        val type_id_get_int_exn : t -> int
        val type_id_set : t -> Uint64.t -> unit
        val type_id_set_int_exn : t -> int -> unit
        val of_message : rw message_t -> t
        val to_message : t -> rw message_t
        val init_root : ?message_size:int -> unit -> t
      end
      module List : sig
        type t = Reader.Type.List.builder_t
        type reader_t = Reader.Type.List.t
        type t_List_9792858745991129751 = t
        type reader_t_List_9792858745991129751 = reader_t
        val element_type_get : t -> t_Type_15020482145304562784
        val element_type_set_reader : t -> reader_t_Type_15020482145304562784 -> t_Type_15020482145304562784
        val element_type_set_builder : t -> t_Type_15020482145304562784 -> t_Type_15020482145304562784
        val element_type_init : t -> t_Type_15020482145304562784
        val of_message : rw message_t -> t
        val to_message : t -> rw message_t
        val init_root : ?message_size:int -> unit -> t
      end
      module Struct : sig
        type t = Reader.Type.Struct.builder_t
        type reader_t = Reader.Type.Struct.t
        type t_Struct_12410354185295152851 = t
        type reader_t_Struct_12410354185295152851 = reader_t
        val type_id_get : t -> Uint64.t
        val type_id_get_int_exn : t -> int
        val type_id_set : t -> Uint64.t -> unit
        val type_id_set_int_exn : t -> int -> unit
        val of_message : rw message_t -> t
        val to_message : t -> rw message_t
        val init_root : ?message_size:int -> unit -> t
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
      val any_pointer_set : t -> unit
      val of_message : rw message_t -> t
      val to_message : t -> rw message_t
      val init_root : ?message_size:int -> unit -> t
    end
    module Value : sig
      type t = Reader.Value.builder_t
      type reader_t = Reader.Value.t
      type t_Value_14853958794117909659 = t
      type reader_t_Value_14853958794117909659 = reader_t
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
        | List of pointer_t
        | Enum of int
        | Struct of pointer_t
        | Interface
        | AnyPointer of pointer_t
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
      val list_set : t -> pointer_t -> unit
      val enum_set_exn : t -> int -> unit
      val struct_set : t -> pointer_t -> unit
      val interface_set : t -> unit
      val any_pointer_set : t -> pointer_t -> unit
      val of_message : rw message_t -> t
      val to_message : t -> rw message_t
      val init_root : ?message_size:int -> unit -> t
    end
    module Annotation : sig
      type t = Reader.Annotation.builder_t
      type reader_t = Reader.Annotation.t
      type t_Annotation_17422339044421236034 = t
      type reader_t_Annotation_17422339044421236034 = reader_t
      val id_get : t -> Uint64.t
      val id_get_int_exn : t -> int
      val id_set : t -> Uint64.t -> unit
      val id_set_int_exn : t -> int -> unit
      val value_get : t -> Value.t
      val value_set_reader : t -> Value.reader_t -> Value.t
      val value_set_builder : t -> Value.t -> Value.t
      val value_init : t -> Value.t
      val of_message : rw message_t -> t
      val to_message : t -> rw message_t
      val init_root : ?message_size:int -> unit -> t
    end
    module Method : sig
      type t = Reader.Method.builder_t
      type reader_t = Reader.Method.t
      type t_Method_10736806783679155584 = t
      type reader_t_Method_10736806783679155584 = reader_t
      val name_get : t -> string
      val name_set : t -> string -> unit
      val code_order_get : t -> int
      val code_order_set_exn : t -> int -> unit
      val param_struct_type_get : t -> Uint64.t
      val param_struct_type_get_int_exn : t -> int
      val param_struct_type_set : t -> Uint64.t -> unit
      val param_struct_type_set_int_exn : t -> int -> unit
      val result_struct_type_get : t -> Uint64.t
      val result_struct_type_get_int_exn : t -> int
      val result_struct_type_set : t -> Uint64.t -> unit
      val result_struct_type_set_int_exn : t -> int -> unit
      val annotations_get : t -> (rw, Annotation.t, array_t) Capnp.Runtime.Array.t
      val annotations_set : t -> (rw, Annotation.t, array_t) Capnp.Runtime.Array.t -> (rw, Annotation.t, array_t) Capnp.Runtime.Array.t
      val annotations_init : t -> int -> (rw, Annotation.t, array_t) Capnp.Runtime.Array.t
      val of_message : rw message_t -> t
      val to_message : t -> rw message_t
      val init_root : ?message_size:int -> unit -> t
    end
    module Enumerant : sig
      type t = Reader.Enumerant.builder_t
      type reader_t = Reader.Enumerant.t
      type t_Enumerant_10919677598968879693 = t
      type reader_t_Enumerant_10919677598968879693 = reader_t
      val name_get : t -> string
      val name_set : t -> string -> unit
      val code_order_get : t -> int
      val code_order_set_exn : t -> int -> unit
      val annotations_get : t -> (rw, Annotation.t, array_t) Capnp.Runtime.Array.t
      val annotations_set : t -> (rw, Annotation.t, array_t) Capnp.Runtime.Array.t -> (rw, Annotation.t, array_t) Capnp.Runtime.Array.t
      val annotations_init : t -> int -> (rw, Annotation.t, array_t) Capnp.Runtime.Array.t
      val of_message : rw message_t -> t
      val to_message : t -> rw message_t
      val init_root : ?message_size:int -> unit -> t
    end
    module Field : sig
      type t = Reader.Field.builder_t
      type reader_t = Reader.Field.t
      type t_Field_11145653318641710175 = t
      type reader_t_Field_11145653318641710175 = reader_t
      val no_discriminant : int
      module Ordinal : sig
        type t = Reader.Field.Ordinal.builder_t
        type reader_t = Reader.Field.Ordinal.t
        type t_Ordinal_13515537513213004774 = t
        type reader_t_Ordinal_13515537513213004774 = reader_t
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
      module Group : sig
        type t = Reader.Field.Group.builder_t
        type reader_t = Reader.Field.Group.t
        type t_Group_14626792032033250577 = t
        type reader_t_Group_14626792032033250577 = reader_t
        val type_id_get : t -> Uint64.t
        val type_id_get_int_exn : t -> int
        val type_id_set : t -> Uint64.t -> unit
        val type_id_set_int_exn : t -> int -> unit
        val of_message : rw message_t -> t
        val to_message : t -> rw message_t
        val init_root : ?message_size:int -> unit -> t
      end
      module Slot : sig
        type t = Reader.Field.Slot.builder_t
        type reader_t = Reader.Field.Slot.t
        type t_Slot_14133145859926553711 = t
        type reader_t_Slot_14133145859926553711 = reader_t
        val offset_get : t -> Uint32.t
        val offset_get_int_exn : t -> int
        val offset_set : t -> Uint32.t -> unit
        val offset_set_int_exn : t -> int -> unit
        val type_get : t -> Type.t
        val type_set_reader : t -> Type.reader_t -> Type.t
        val type_set_builder : t -> Type.t -> Type.t
        val type_init : t -> Type.t
        val default_value_get : t -> Value.t
        val default_value_set_reader : t -> Value.reader_t -> Value.t
        val default_value_set_builder : t -> Value.t -> Value.t
        val default_value_init : t -> Value.t
        val had_explicit_default_get : t -> bool
        val had_explicit_default_set : t -> bool -> unit
        val of_message : rw message_t -> t
        val to_message : t -> rw message_t
        val init_root : ?message_size:int -> unit -> t
      end
      type unnamed_union_t =
        | Slot of Slot.t
        | Group of Group.t
        | Undefined of int
      val get : t -> unnamed_union_t
      val name_get : t -> string
      val name_set : t -> string -> unit
      val code_order_get : t -> int
      val code_order_set_exn : t -> int -> unit
      val annotations_get : t -> (rw, Annotation.t, array_t) Capnp.Runtime.Array.t
      val annotations_set : t -> (rw, Annotation.t, array_t) Capnp.Runtime.Array.t -> (rw, Annotation.t, array_t) Capnp.Runtime.Array.t
      val annotations_init : t -> int -> (rw, Annotation.t, array_t) Capnp.Runtime.Array.t
      val discriminant_value_get : t -> int
      val discriminant_value_set_exn : t -> int -> unit
      val ordinal_get : t -> Ordinal.t
      val of_message : rw message_t -> t
      val to_message : t -> rw message_t
      val init_root : ?message_size:int -> unit -> t
    end
    module Node : sig
      type t = Reader.Node.builder_t
      type reader_t = Reader.Node.t
      type t_Node_16610026722781537303 = t
      type reader_t_Node_16610026722781537303 = reader_t
      module Struct : sig
        type t = Reader.Node.Struct.builder_t
        type reader_t = Reader.Node.Struct.t
        type t_Struct_11430331134483579957 = t
        type reader_t_Struct_11430331134483579957 = reader_t
        val data_word_count_get : t -> int
        val data_word_count_set_exn : t -> int -> unit
        val pointer_count_get : t -> int
        val pointer_count_set_exn : t -> int -> unit
        val preferred_list_encoding_get : t -> ElementSize.t
        val preferred_list_encoding_set : t -> ElementSize.t -> unit
        val preferred_list_encoding_set_unsafe : t -> ElementSize.t -> unit
        val is_group_get : t -> bool
        val is_group_set : t -> bool -> unit
        val discriminant_count_get : t -> int
        val discriminant_count_set_exn : t -> int -> unit
        val discriminant_offset_get : t -> Uint32.t
        val discriminant_offset_get_int_exn : t -> int
        val discriminant_offset_set : t -> Uint32.t -> unit
        val discriminant_offset_set_int_exn : t -> int -> unit
        val fields_get : t -> (rw, Field.t, array_t) Capnp.Runtime.Array.t
        val fields_set : t -> (rw, Field.t, array_t) Capnp.Runtime.Array.t -> (rw, Field.t, array_t) Capnp.Runtime.Array.t
        val fields_init : t -> int -> (rw, Field.t, array_t) Capnp.Runtime.Array.t
        val of_message : rw message_t -> t
        val to_message : t -> rw message_t
        val init_root : ?message_size:int -> unit -> t
      end
      module Enum : sig
        type t = Reader.Node.Enum.builder_t
        type reader_t = Reader.Node.Enum.t
        type t_Enum_13063450714778629528 = t
        type reader_t_Enum_13063450714778629528 = reader_t
        val enumerants_get : t -> (rw, Enumerant.t, array_t) Capnp.Runtime.Array.t
        val enumerants_set : t -> (rw, Enumerant.t, array_t) Capnp.Runtime.Array.t -> (rw, Enumerant.t, array_t) Capnp.Runtime.Array.t
        val enumerants_init : t -> int -> (rw, Enumerant.t, array_t) Capnp.Runtime.Array.t
        val of_message : rw message_t -> t
        val to_message : t -> rw message_t
        val init_root : ?message_size:int -> unit -> t
      end
      module Annotation : sig
        type t = Reader.Node.Annotation.builder_t
        type reader_t = Reader.Node.Annotation.t
        type t_Annotation_17011813041836786320 = t
        type reader_t_Annotation_17011813041836786320 = reader_t
        val type_get : t -> Type.t
        val type_set_reader : t -> Type.reader_t -> Type.t
        val type_set_builder : t -> Type.t -> Type.t
        val type_init : t -> Type.t
        val targets_file_get : t -> bool
        val targets_file_set : t -> bool -> unit
        val targets_const_get : t -> bool
        val targets_const_set : t -> bool -> unit
        val targets_enum_get : t -> bool
        val targets_enum_set : t -> bool -> unit
        val targets_enumerant_get : t -> bool
        val targets_enumerant_set : t -> bool -> unit
        val targets_struct_get : t -> bool
        val targets_struct_set : t -> bool -> unit
        val targets_field_get : t -> bool
        val targets_field_set : t -> bool -> unit
        val targets_union_get : t -> bool
        val targets_union_set : t -> bool -> unit
        val targets_group_get : t -> bool
        val targets_group_set : t -> bool -> unit
        val targets_interface_get : t -> bool
        val targets_interface_set : t -> bool -> unit
        val targets_method_get : t -> bool
        val targets_method_set : t -> bool -> unit
        val targets_param_get : t -> bool
        val targets_param_set : t -> bool -> unit
        val targets_annotation_get : t -> bool
        val targets_annotation_set : t -> bool -> unit
        val of_message : rw message_t -> t
        val to_message : t -> rw message_t
        val init_root : ?message_size:int -> unit -> t
      end
      module Const : sig
        type t = Reader.Node.Const.builder_t
        type reader_t = Reader.Node.Const.t
        type t_Const_12793219851699983392 = t
        type reader_t_Const_12793219851699983392 = reader_t
        val type_get : t -> Type.t
        val type_set_reader : t -> Type.reader_t -> Type.t
        val type_set_builder : t -> Type.t -> Type.t
        val type_init : t -> Type.t
        val value_get : t -> Value.t
        val value_set_reader : t -> Value.reader_t -> Value.t
        val value_set_builder : t -> Value.t -> Value.t
        val value_init : t -> Value.t
        val of_message : rw message_t -> t
        val to_message : t -> rw message_t
        val init_root : ?message_size:int -> unit -> t
      end
      module Interface : sig
        type t = Reader.Node.Interface.builder_t
        type reader_t = Reader.Node.Interface.t
        type t_Interface_16728431493453586831 = t
        type reader_t_Interface_16728431493453586831 = reader_t
        val methods_get : t -> (rw, Method.t, array_t) Capnp.Runtime.Array.t
        val methods_set : t -> (rw, Method.t, array_t) Capnp.Runtime.Array.t -> (rw, Method.t, array_t) Capnp.Runtime.Array.t
        val methods_init : t -> int -> (rw, Method.t, array_t) Capnp.Runtime.Array.t
        val extends_get : t -> (rw, Uint64.t, array_t) Capnp.Runtime.Array.t
        val extends_set : t -> (rw, Uint64.t, array_t) Capnp.Runtime.Array.t -> (rw, Uint64.t, array_t) Capnp.Runtime.Array.t
        val extends_init : t -> int -> (rw, Uint64.t, array_t) Capnp.Runtime.Array.t
        val of_message : rw message_t -> t
        val to_message : t -> rw message_t
        val init_root : ?message_size:int -> unit -> t
      end
      module NestedNode : sig
        type t = Reader.Node.NestedNode.builder_t
        type reader_t = Reader.Node.NestedNode.t
        type t_NestedNode_16050641862814319170 = t
        type reader_t_NestedNode_16050641862814319170 = reader_t
        val name_get : t -> string
        val name_set : t -> string -> unit
        val id_get : t -> Uint64.t
        val id_get_int_exn : t -> int
        val id_set : t -> Uint64.t -> unit
        val id_set_int_exn : t -> int -> unit
        val of_message : rw message_t -> t
        val to_message : t -> rw message_t
        val init_root : ?message_size:int -> unit -> t
      end
      type unnamed_union_t =
        | File
        | Struct of Struct.t
        | Enum of Enum.t
        | Interface of Interface.t
        | Const of Const.t
        | Annotation of Annotation.t
        | Undefined of int
      val get : t -> unnamed_union_t
      val file_set : t -> unit
      val id_get : t -> Uint64.t
      val id_get_int_exn : t -> int
      val id_set : t -> Uint64.t -> unit
      val id_set_int_exn : t -> int -> unit
      val display_name_get : t -> string
      val display_name_set : t -> string -> unit
      val display_name_prefix_length_get : t -> Uint32.t
      val display_name_prefix_length_get_int_exn : t -> int
      val display_name_prefix_length_set : t -> Uint32.t -> unit
      val display_name_prefix_length_set_int_exn : t -> int -> unit
      val scope_id_get : t -> Uint64.t
      val scope_id_get_int_exn : t -> int
      val scope_id_set : t -> Uint64.t -> unit
      val scope_id_set_int_exn : t -> int -> unit
      val nested_nodes_get : t -> (rw, NestedNode.t, array_t) Capnp.Runtime.Array.t
      val nested_nodes_set : t -> (rw, NestedNode.t, array_t) Capnp.Runtime.Array.t -> (rw, NestedNode.t, array_t) Capnp.Runtime.Array.t
      val nested_nodes_init : t -> int -> (rw, NestedNode.t, array_t) Capnp.Runtime.Array.t
      val annotations_get : t -> (rw, Annotation.t, array_t) Capnp.Runtime.Array.t
      val annotations_set : t -> (rw, Annotation.t, array_t) Capnp.Runtime.Array.t -> (rw, Annotation.t, array_t) Capnp.Runtime.Array.t
      val annotations_init : t -> int -> (rw, Annotation.t, array_t) Capnp.Runtime.Array.t
      val of_message : rw message_t -> t
      val to_message : t -> rw message_t
      val init_root : ?message_size:int -> unit -> t
    end
    module CodeGeneratorRequest : sig
      type t = Reader.CodeGeneratorRequest.builder_t
      type reader_t = Reader.CodeGeneratorRequest.t
      type t_CodeGeneratorRequest_13818529054586492878 = t
      type reader_t_CodeGeneratorRequest_13818529054586492878 = reader_t
      module RequestedFile : sig
        type t = Reader.CodeGeneratorRequest.RequestedFile.builder_t
        type reader_t = Reader.CodeGeneratorRequest.RequestedFile.t
        type t_RequestedFile_14981803260258615394 = t
        type reader_t_RequestedFile_14981803260258615394 = reader_t
        module Import : sig
          type t = Reader.CodeGeneratorRequest.RequestedFile.Import.builder_t
          type reader_t = Reader.CodeGeneratorRequest.RequestedFile.Import.t
          type t_Import_12560611460656617445 = t
          type reader_t_Import_12560611460656617445 = reader_t
          val id_get : t -> Uint64.t
          val id_get_int_exn : t -> int
          val id_set : t -> Uint64.t -> unit
          val id_set_int_exn : t -> int -> unit
          val name_get : t -> string
          val name_set : t -> string -> unit
          val of_message : rw message_t -> t
          val to_message : t -> rw message_t
          val init_root : ?message_size:int -> unit -> t
        end
        val id_get : t -> Uint64.t
        val id_get_int_exn : t -> int
        val id_set : t -> Uint64.t -> unit
        val id_set_int_exn : t -> int -> unit
        val filename_get : t -> string
        val filename_set : t -> string -> unit
        val imports_get : t -> (rw, Import.t, array_t) Capnp.Runtime.Array.t
        val imports_set : t -> (rw, Import.t, array_t) Capnp.Runtime.Array.t -> (rw, Import.t, array_t) Capnp.Runtime.Array.t
        val imports_init : t -> int -> (rw, Import.t, array_t) Capnp.Runtime.Array.t
        val of_message : rw message_t -> t
        val to_message : t -> rw message_t
        val init_root : ?message_size:int -> unit -> t
      end
      val nodes_get : t -> (rw, Node.t, array_t) Capnp.Runtime.Array.t
      val nodes_set : t -> (rw, Node.t, array_t) Capnp.Runtime.Array.t -> (rw, Node.t, array_t) Capnp.Runtime.Array.t
      val nodes_init : t -> int -> (rw, Node.t, array_t) Capnp.Runtime.Array.t
      val requested_files_get : t -> (rw, RequestedFile.t, array_t) Capnp.Runtime.Array.t
      val requested_files_set : t -> (rw, RequestedFile.t, array_t) Capnp.Runtime.Array.t -> (rw, RequestedFile.t, array_t) Capnp.Runtime.Array.t
      val requested_files_init : t -> int -> (rw, RequestedFile.t, array_t) Capnp.Runtime.Array.t
      val of_message : rw message_t -> t
      val to_message : t -> rw message_t
      val init_root : ?message_size:int -> unit -> t
    end
  end
end
module Make (MessageWrapper : Capnp.Message.S) :
  (S with type 'cap message_t = 'cap MessageWrapper.Message.t
    and type Reader.pointer_t = ro MessageWrapper.Slice.t option
    and type Builder.pointer_t = rw MessageWrapper.Slice.t)

