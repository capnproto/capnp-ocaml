type ro = Capnp.Message.ro
type rw = Capnp.Message.rw

module type S = sig
  type 'cap message_t

  module C2b2b_13688829037717245569 : C2b2b.S with
    type 'cap message_t = 'cap message_t

  type reader_t_Slot_14133145859926553711
  type builder_t_Slot_14133145859926553711
  type reader_t_Group_14626792032033250577
  type builder_t_Group_14626792032033250577
  type reader_t_Ordinal_13515537513213004774
  type builder_t_Ordinal_13515537513213004774
  type reader_t_Field_11145653318641710175
  type builder_t_Field_11145653318641710175
  type reader_t_Import_12560611460656617445
  type builder_t_Import_12560611460656617445
  type reader_t_RequestedFile_14981803260258615394
  type builder_t_RequestedFile_14981803260258615394
  type reader_t_CodeGeneratorRequest_13818529054586492878
  type builder_t_CodeGeneratorRequest_13818529054586492878
  type reader_t_NestedNode_16050641862814319170
  type builder_t_NestedNode_16050641862814319170
  type reader_t_Interface_16728431493453586831
  type builder_t_Interface_16728431493453586831
  type reader_t_Const_12793219851699983392
  type builder_t_Const_12793219851699983392
  type reader_t_Annotation_17011813041836786320
  type builder_t_Annotation_17011813041836786320
  type reader_t_Enum_13063450714778629528
  type builder_t_Enum_13063450714778629528
  type reader_t_Struct_11430331134483579957
  type builder_t_Struct_11430331134483579957
  type reader_t_Node_16610026722781537303
  type builder_t_Node_16610026722781537303
  type reader_t_Value_14853958794117909659
  type builder_t_Value_14853958794117909659
  type reader_t_Enumerant_10919677598968879693
  type builder_t_Enumerant_10919677598968879693
  type reader_t_Annotation_17422339044421236034
  type builder_t_Annotation_17422339044421236034
  type reader_t_Method_10736806783679155584
  type builder_t_Method_10736806783679155584
  type reader_t_Struct_12410354185295152851
  type builder_t_Struct_12410354185295152851
  type reader_t_List_9792858745991129751
  type builder_t_List_9792858745991129751
  type reader_t_Interface_17116997365232503999
  type builder_t_Interface_17116997365232503999
  type reader_t_Enum_11389172934837766057
  type builder_t_Enum_11389172934837766057
  type reader_t_Type_15020482145304562784
  type builder_t_Type_15020482145304562784
  module ElementSize_15102134695616452902 : sig
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

  module Reader : sig
    type array_t
    type builder_array_t
    type pointer_t
    module ElementSize : sig
      type t = ElementSize_15102134695616452902.t =
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
      type t = reader_t_Type_15020482145304562784
      type builder_t = builder_t_Type_15020482145304562784
      module Enum : sig
        type t = reader_t_Enum_11389172934837766057
        type builder_t = builder_t_Enum_11389172934837766057
        val type_id_get : t -> Uint64.t
        val type_id_get_int_exn : t -> int
        val of_message : 'cap message_t -> t
        val of_builder : builder_t -> t
      end
      module Interface : sig
        type t = reader_t_Interface_17116997365232503999
        type builder_t = builder_t_Interface_17116997365232503999
        val type_id_get : t -> Uint64.t
        val type_id_get_int_exn : t -> int
        val of_message : 'cap message_t -> t
        val of_builder : builder_t -> t
      end
      module List : sig
        type t = reader_t_List_9792858745991129751
        type builder_t = builder_t_List_9792858745991129751
        val has_element_type : t -> bool
        val element_type_get : t -> reader_t_Type_15020482145304562784
        val of_message : 'cap message_t -> t
        val of_builder : builder_t -> t
      end
      module Struct : sig
        type t = reader_t_Struct_12410354185295152851
        type builder_t = builder_t_Struct_12410354185295152851
        val type_id_get : t -> Uint64.t
        val type_id_get_int_exn : t -> int
        val of_message : 'cap message_t -> t
        val of_builder : builder_t -> t
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
      val of_builder : builder_t -> t
    end
    module Value : sig
      type t = reader_t_Value_14853958794117909659
      type builder_t = builder_t_Value_14853958794117909659
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
      val of_builder : builder_t -> t
    end
    module Annotation : sig
      type t = reader_t_Annotation_17422339044421236034
      type builder_t = builder_t_Annotation_17422339044421236034
      val id_get : t -> Uint64.t
      val id_get_int_exn : t -> int
      val has_value : t -> bool
      val value_get : t -> Value.t
      val of_message : 'cap message_t -> t
      val of_builder : builder_t -> t
    end
    module Method : sig
      type t = reader_t_Method_10736806783679155584
      type builder_t = builder_t_Method_10736806783679155584
      val has_name : t -> bool
      val name_get : t -> string
      val code_order_get : t -> int
      val param_struct_type_get : t -> Uint64.t
      val param_struct_type_get_int_exn : t -> int
      val result_struct_type_get : t -> Uint64.t
      val result_struct_type_get_int_exn : t -> int
      val has_annotations : t -> bool
      val annotations_get : t -> (ro, Annotation.t, array_t) Capnp.Array.t
      val annotations_get_list : t -> Annotation.t list
      val annotations_get_array : t -> Annotation.t array
      val of_message : 'cap message_t -> t
      val of_builder : builder_t -> t
    end
    module Enumerant : sig
      type t = reader_t_Enumerant_10919677598968879693
      type builder_t = builder_t_Enumerant_10919677598968879693
      val has_name : t -> bool
      val name_get : t -> string
      val code_order_get : t -> int
      val has_annotations : t -> bool
      val annotations_get : t -> (ro, Annotation.t, array_t) Capnp.Array.t
      val annotations_get_list : t -> Annotation.t list
      val annotations_get_array : t -> Annotation.t array
      val of_message : 'cap message_t -> t
      val of_builder : builder_t -> t
    end
    module Field : sig
      type t = reader_t_Field_11145653318641710175
      type builder_t = builder_t_Field_11145653318641710175
      val no_discriminant : int
      module Ordinal : sig
        type t = reader_t_Ordinal_13515537513213004774
        type builder_t = builder_t_Ordinal_13515537513213004774
        type unnamed_union_t =
          | Implicit
          | Explicit of int
          | Undefined of int
        val get : t -> unnamed_union_t
        val of_message : 'cap message_t -> t
        val of_builder : builder_t -> t
      end
      module Group : sig
        type t = reader_t_Group_14626792032033250577
        type builder_t = builder_t_Group_14626792032033250577
        val type_id_get : t -> Uint64.t
        val type_id_get_int_exn : t -> int
        val of_message : 'cap message_t -> t
        val of_builder : builder_t -> t
      end
      module Slot : sig
        type t = reader_t_Slot_14133145859926553711
        type builder_t = builder_t_Slot_14133145859926553711
        val offset_get : t -> Uint32.t
        val offset_get_int_exn : t -> int
        val has_type : t -> bool
        val type_get : t -> Type.t
        val has_default_value : t -> bool
        val default_value_get : t -> Value.t
        val had_explicit_default_get : t -> bool
        val of_message : 'cap message_t -> t
        val of_builder : builder_t -> t
      end
      type unnamed_union_t =
        | Slot of Slot.t
        | Group of Group.t
        | Undefined of int
      val get : t -> unnamed_union_t
      val has_name : t -> bool
      val name_get : t -> string
      val code_order_get : t -> int
      val has_annotations : t -> bool
      val annotations_get : t -> (ro, Annotation.t, array_t) Capnp.Array.t
      val annotations_get_list : t -> Annotation.t list
      val annotations_get_array : t -> Annotation.t array
      val discriminant_value_get : t -> int
      val ordinal_get : t -> Ordinal.t
      val of_message : 'cap message_t -> t
      val of_builder : builder_t -> t
    end
    module Node : sig
      type t = reader_t_Node_16610026722781537303
      type builder_t = builder_t_Node_16610026722781537303
      module Struct : sig
        type t = reader_t_Struct_11430331134483579957
        type builder_t = builder_t_Struct_11430331134483579957
        val data_word_count_get : t -> int
        val pointer_count_get : t -> int
        val preferred_list_encoding_get : t -> ElementSize.t
        val is_group_get : t -> bool
        val discriminant_count_get : t -> int
        val discriminant_offset_get : t -> Uint32.t
        val discriminant_offset_get_int_exn : t -> int
        val has_fields : t -> bool
        val fields_get : t -> (ro, Field.t, array_t) Capnp.Array.t
        val fields_get_list : t -> Field.t list
        val fields_get_array : t -> Field.t array
        val of_message : 'cap message_t -> t
        val of_builder : builder_t -> t
      end
      module Enum : sig
        type t = reader_t_Enum_13063450714778629528
        type builder_t = builder_t_Enum_13063450714778629528
        val has_enumerants : t -> bool
        val enumerants_get : t -> (ro, Enumerant.t, array_t) Capnp.Array.t
        val enumerants_get_list : t -> Enumerant.t list
        val enumerants_get_array : t -> Enumerant.t array
        val of_message : 'cap message_t -> t
        val of_builder : builder_t -> t
      end
      module Annotation : sig
        type t = reader_t_Annotation_17011813041836786320
        type builder_t = builder_t_Annotation_17011813041836786320
        val has_type : t -> bool
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
        val of_builder : builder_t -> t
      end
      module Const : sig
        type t = reader_t_Const_12793219851699983392
        type builder_t = builder_t_Const_12793219851699983392
        val has_type : t -> bool
        val type_get : t -> Type.t
        val has_value : t -> bool
        val value_get : t -> Value.t
        val of_message : 'cap message_t -> t
        val of_builder : builder_t -> t
      end
      module Interface : sig
        type t = reader_t_Interface_16728431493453586831
        type builder_t = builder_t_Interface_16728431493453586831
        val has_methods : t -> bool
        val methods_get : t -> (ro, Method.t, array_t) Capnp.Array.t
        val methods_get_list : t -> Method.t list
        val methods_get_array : t -> Method.t array
        val has_extends : t -> bool
        val extends_get : t -> (ro, Uint64.t, array_t) Capnp.Array.t
        val extends_get_list : t -> Uint64.t list
        val extends_get_array : t -> Uint64.t array
        val of_message : 'cap message_t -> t
        val of_builder : builder_t -> t
      end
      module NestedNode : sig
        type t = reader_t_NestedNode_16050641862814319170
        type builder_t = builder_t_NestedNode_16050641862814319170
        val has_name : t -> bool
        val name_get : t -> string
        val id_get : t -> Uint64.t
        val id_get_int_exn : t -> int
        val of_message : 'cap message_t -> t
        val of_builder : builder_t -> t
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
      val has_display_name : t -> bool
      val display_name_get : t -> string
      val display_name_prefix_length_get : t -> Uint32.t
      val display_name_prefix_length_get_int_exn : t -> int
      val scope_id_get : t -> Uint64.t
      val scope_id_get_int_exn : t -> int
      val has_nested_nodes : t -> bool
      val nested_nodes_get : t -> (ro, NestedNode.t, array_t) Capnp.Array.t
      val nested_nodes_get_list : t -> NestedNode.t list
      val nested_nodes_get_array : t -> NestedNode.t array
      val has_annotations : t -> bool
      val annotations_get : t -> (ro, reader_t_Annotation_17422339044421236034, array_t) Capnp.Array.t
      val annotations_get_list : t -> reader_t_Annotation_17422339044421236034 list
      val annotations_get_array : t -> reader_t_Annotation_17422339044421236034 array
      val of_message : 'cap message_t -> t
      val of_builder : builder_t -> t
    end
    module CodeGeneratorRequest : sig
      type t = reader_t_CodeGeneratorRequest_13818529054586492878
      type builder_t = builder_t_CodeGeneratorRequest_13818529054586492878
      module RequestedFile : sig
        type t = reader_t_RequestedFile_14981803260258615394
        type builder_t = builder_t_RequestedFile_14981803260258615394
        module Import : sig
          type t = reader_t_Import_12560611460656617445
          type builder_t = builder_t_Import_12560611460656617445
          val id_get : t -> Uint64.t
          val id_get_int_exn : t -> int
          val has_name : t -> bool
          val name_get : t -> string
          val of_message : 'cap message_t -> t
          val of_builder : builder_t -> t
        end
        val id_get : t -> Uint64.t
        val id_get_int_exn : t -> int
        val has_filename : t -> bool
        val filename_get : t -> string
        val has_imports : t -> bool
        val imports_get : t -> (ro, Import.t, array_t) Capnp.Array.t
        val imports_get_list : t -> Import.t list
        val imports_get_array : t -> Import.t array
        val of_message : 'cap message_t -> t
        val of_builder : builder_t -> t
      end
      val has_nodes : t -> bool
      val nodes_get : t -> (ro, Node.t, array_t) Capnp.Array.t
      val nodes_get_list : t -> Node.t list
      val nodes_get_array : t -> Node.t array
      val has_requested_files : t -> bool
      val requested_files_get : t -> (ro, RequestedFile.t, array_t) Capnp.Array.t
      val requested_files_get_list : t -> RequestedFile.t list
      val requested_files_get_array : t -> RequestedFile.t array
      val of_message : 'cap message_t -> t
      val of_builder : builder_t -> t
    end
  end

  module Builder : sig
    type array_t = Reader.builder_array_t
    type reader_array_t = Reader.array_t
    type pointer_t
    module ElementSize : sig
      type t = ElementSize_15102134695616452902.t =
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
      type t = builder_t_Type_15020482145304562784
      type reader_t = reader_t_Type_15020482145304562784
      module Enum : sig
        type t = builder_t_Enum_11389172934837766057
        type reader_t = reader_t_Enum_11389172934837766057
        val type_id_get : t -> Uint64.t
        val type_id_get_int_exn : t -> int
        val type_id_set : t -> Uint64.t -> unit
        val type_id_set_int_exn : t -> int -> unit
        val of_message : rw message_t -> t
        val to_message : t -> rw message_t
        val to_reader : t -> reader_t
        val init_root : ?message_size:int -> unit -> t
      end
      module Interface : sig
        type t = builder_t_Interface_17116997365232503999
        type reader_t = reader_t_Interface_17116997365232503999
        val type_id_get : t -> Uint64.t
        val type_id_get_int_exn : t -> int
        val type_id_set : t -> Uint64.t -> unit
        val type_id_set_int_exn : t -> int -> unit
        val of_message : rw message_t -> t
        val to_message : t -> rw message_t
        val to_reader : t -> reader_t
        val init_root : ?message_size:int -> unit -> t
      end
      module List : sig
        type t = builder_t_List_9792858745991129751
        type reader_t = reader_t_List_9792858745991129751
        val has_element_type : t -> bool
        val element_type_get : t -> builder_t_Type_15020482145304562784
        val element_type_set_reader : t -> reader_t_Type_15020482145304562784 -> builder_t_Type_15020482145304562784
        val element_type_set_builder : t -> builder_t_Type_15020482145304562784 -> builder_t_Type_15020482145304562784
        val element_type_init : t -> builder_t_Type_15020482145304562784
        val of_message : rw message_t -> t
        val to_message : t -> rw message_t
        val to_reader : t -> reader_t
        val init_root : ?message_size:int -> unit -> t
      end
      module Struct : sig
        type t = builder_t_Struct_12410354185295152851
        type reader_t = reader_t_Struct_12410354185295152851
        val type_id_get : t -> Uint64.t
        val type_id_get_int_exn : t -> int
        val type_id_set : t -> Uint64.t -> unit
        val type_id_set_int_exn : t -> int -> unit
        val of_message : rw message_t -> t
        val to_message : t -> rw message_t
        val to_reader : t -> reader_t
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
      val list_init : t -> List.t
      val enum_init : t -> Enum.t
      val struct_init : t -> Struct.t
      val interface_init : t -> Interface.t
      val any_pointer_set : t -> unit
      val of_message : rw message_t -> t
      val to_message : t -> rw message_t
      val to_reader : t -> reader_t
      val init_root : ?message_size:int -> unit -> t
    end
    module Value : sig
      type t = builder_t_Value_14853958794117909659
      type reader_t = reader_t_Value_14853958794117909659
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
      val int64_set_int : t -> int -> unit
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
      val list_set : t -> pointer_t -> pointer_t
      val enum_set_exn : t -> int -> unit
      val struct_set : t -> pointer_t -> pointer_t
      val interface_set : t -> unit
      val any_pointer_set : t -> pointer_t -> pointer_t
      val of_message : rw message_t -> t
      val to_message : t -> rw message_t
      val to_reader : t -> reader_t
      val init_root : ?message_size:int -> unit -> t
    end
    module Annotation : sig
      type t = builder_t_Annotation_17422339044421236034
      type reader_t = reader_t_Annotation_17422339044421236034
      val id_get : t -> Uint64.t
      val id_get_int_exn : t -> int
      val id_set : t -> Uint64.t -> unit
      val id_set_int_exn : t -> int -> unit
      val has_value : t -> bool
      val value_get : t -> Value.t
      val value_set_reader : t -> Value.reader_t -> Value.t
      val value_set_builder : t -> Value.t -> Value.t
      val value_init : t -> Value.t
      val of_message : rw message_t -> t
      val to_message : t -> rw message_t
      val to_reader : t -> reader_t
      val init_root : ?message_size:int -> unit -> t
    end
    module Method : sig
      type t = builder_t_Method_10736806783679155584
      type reader_t = reader_t_Method_10736806783679155584
      val has_name : t -> bool
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
      val has_annotations : t -> bool
      val annotations_get : t -> (rw, Annotation.t, array_t) Capnp.Array.t
      val annotations_get_list : t -> Annotation.t list
      val annotations_get_array : t -> Annotation.t array
      val annotations_set : t -> (rw, Annotation.t, array_t) Capnp.Array.t -> (rw, Annotation.t, array_t) Capnp.Array.t
      val annotations_set_list : t -> Annotation.t list -> (rw, Annotation.t, array_t) Capnp.Array.t
      val annotations_set_array : t -> Annotation.t array -> (rw, Annotation.t, array_t) Capnp.Array.t
      val annotations_init : t -> int -> (rw, Annotation.t, array_t) Capnp.Array.t
      val of_message : rw message_t -> t
      val to_message : t -> rw message_t
      val to_reader : t -> reader_t
      val init_root : ?message_size:int -> unit -> t
    end
    module Enumerant : sig
      type t = builder_t_Enumerant_10919677598968879693
      type reader_t = reader_t_Enumerant_10919677598968879693
      val has_name : t -> bool
      val name_get : t -> string
      val name_set : t -> string -> unit
      val code_order_get : t -> int
      val code_order_set_exn : t -> int -> unit
      val has_annotations : t -> bool
      val annotations_get : t -> (rw, Annotation.t, array_t) Capnp.Array.t
      val annotations_get_list : t -> Annotation.t list
      val annotations_get_array : t -> Annotation.t array
      val annotations_set : t -> (rw, Annotation.t, array_t) Capnp.Array.t -> (rw, Annotation.t, array_t) Capnp.Array.t
      val annotations_set_list : t -> Annotation.t list -> (rw, Annotation.t, array_t) Capnp.Array.t
      val annotations_set_array : t -> Annotation.t array -> (rw, Annotation.t, array_t) Capnp.Array.t
      val annotations_init : t -> int -> (rw, Annotation.t, array_t) Capnp.Array.t
      val of_message : rw message_t -> t
      val to_message : t -> rw message_t
      val to_reader : t -> reader_t
      val init_root : ?message_size:int -> unit -> t
    end
    module Field : sig
      type t = builder_t_Field_11145653318641710175
      type reader_t = reader_t_Field_11145653318641710175
      val no_discriminant : int
      module Ordinal : sig
        type t = builder_t_Ordinal_13515537513213004774
        type reader_t = reader_t_Ordinal_13515537513213004774
        type unnamed_union_t =
          | Implicit
          | Explicit of int
          | Undefined of int
        val get : t -> unnamed_union_t
        val implicit_set : t -> unit
        val explicit_set_exn : t -> int -> unit
        val of_message : rw message_t -> t
        val to_message : t -> rw message_t
        val to_reader : t -> reader_t
        val init_root : ?message_size:int -> unit -> t
      end
      module Group : sig
        type t = builder_t_Group_14626792032033250577
        type reader_t = reader_t_Group_14626792032033250577
        val type_id_get : t -> Uint64.t
        val type_id_get_int_exn : t -> int
        val type_id_set : t -> Uint64.t -> unit
        val type_id_set_int_exn : t -> int -> unit
        val of_message : rw message_t -> t
        val to_message : t -> rw message_t
        val to_reader : t -> reader_t
        val init_root : ?message_size:int -> unit -> t
      end
      module Slot : sig
        type t = builder_t_Slot_14133145859926553711
        type reader_t = reader_t_Slot_14133145859926553711
        val offset_get : t -> Uint32.t
        val offset_get_int_exn : t -> int
        val offset_set : t -> Uint32.t -> unit
        val offset_set_int_exn : t -> int -> unit
        val has_type : t -> bool
        val type_get : t -> Type.t
        val type_set_reader : t -> Type.reader_t -> Type.t
        val type_set_builder : t -> Type.t -> Type.t
        val type_init : t -> Type.t
        val has_default_value : t -> bool
        val default_value_get : t -> Value.t
        val default_value_set_reader : t -> Value.reader_t -> Value.t
        val default_value_set_builder : t -> Value.t -> Value.t
        val default_value_init : t -> Value.t
        val had_explicit_default_get : t -> bool
        val had_explicit_default_set : t -> bool -> unit
        val of_message : rw message_t -> t
        val to_message : t -> rw message_t
        val to_reader : t -> reader_t
        val init_root : ?message_size:int -> unit -> t
      end
      type unnamed_union_t =
        | Slot of Slot.t
        | Group of Group.t
        | Undefined of int
      val get : t -> unnamed_union_t
      val slot_init : t -> Slot.t
      val group_init : t -> Group.t
      val has_name : t -> bool
      val name_get : t -> string
      val name_set : t -> string -> unit
      val code_order_get : t -> int
      val code_order_set_exn : t -> int -> unit
      val has_annotations : t -> bool
      val annotations_get : t -> (rw, Annotation.t, array_t) Capnp.Array.t
      val annotations_get_list : t -> Annotation.t list
      val annotations_get_array : t -> Annotation.t array
      val annotations_set : t -> (rw, Annotation.t, array_t) Capnp.Array.t -> (rw, Annotation.t, array_t) Capnp.Array.t
      val annotations_set_list : t -> Annotation.t list -> (rw, Annotation.t, array_t) Capnp.Array.t
      val annotations_set_array : t -> Annotation.t array -> (rw, Annotation.t, array_t) Capnp.Array.t
      val annotations_init : t -> int -> (rw, Annotation.t, array_t) Capnp.Array.t
      val discriminant_value_get : t -> int
      val discriminant_value_set_exn : t -> int -> unit
      val ordinal_get : t -> Ordinal.t
      val ordinal_init : t -> Ordinal.t
      val of_message : rw message_t -> t
      val to_message : t -> rw message_t
      val to_reader : t -> reader_t
      val init_root : ?message_size:int -> unit -> t
    end
    module Node : sig
      type t = builder_t_Node_16610026722781537303
      type reader_t = reader_t_Node_16610026722781537303
      module Struct : sig
        type t = builder_t_Struct_11430331134483579957
        type reader_t = reader_t_Struct_11430331134483579957
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
        val has_fields : t -> bool
        val fields_get : t -> (rw, Field.t, array_t) Capnp.Array.t
        val fields_get_list : t -> Field.t list
        val fields_get_array : t -> Field.t array
        val fields_set : t -> (rw, Field.t, array_t) Capnp.Array.t -> (rw, Field.t, array_t) Capnp.Array.t
        val fields_set_list : t -> Field.t list -> (rw, Field.t, array_t) Capnp.Array.t
        val fields_set_array : t -> Field.t array -> (rw, Field.t, array_t) Capnp.Array.t
        val fields_init : t -> int -> (rw, Field.t, array_t) Capnp.Array.t
        val of_message : rw message_t -> t
        val to_message : t -> rw message_t
        val to_reader : t -> reader_t
        val init_root : ?message_size:int -> unit -> t
      end
      module Enum : sig
        type t = builder_t_Enum_13063450714778629528
        type reader_t = reader_t_Enum_13063450714778629528
        val has_enumerants : t -> bool
        val enumerants_get : t -> (rw, Enumerant.t, array_t) Capnp.Array.t
        val enumerants_get_list : t -> Enumerant.t list
        val enumerants_get_array : t -> Enumerant.t array
        val enumerants_set : t -> (rw, Enumerant.t, array_t) Capnp.Array.t -> (rw, Enumerant.t, array_t) Capnp.Array.t
        val enumerants_set_list : t -> Enumerant.t list -> (rw, Enumerant.t, array_t) Capnp.Array.t
        val enumerants_set_array : t -> Enumerant.t array -> (rw, Enumerant.t, array_t) Capnp.Array.t
        val enumerants_init : t -> int -> (rw, Enumerant.t, array_t) Capnp.Array.t
        val of_message : rw message_t -> t
        val to_message : t -> rw message_t
        val to_reader : t -> reader_t
        val init_root : ?message_size:int -> unit -> t
      end
      module Annotation : sig
        type t = builder_t_Annotation_17011813041836786320
        type reader_t = reader_t_Annotation_17011813041836786320
        val has_type : t -> bool
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
        val to_reader : t -> reader_t
        val init_root : ?message_size:int -> unit -> t
      end
      module Const : sig
        type t = builder_t_Const_12793219851699983392
        type reader_t = reader_t_Const_12793219851699983392
        val has_type : t -> bool
        val type_get : t -> Type.t
        val type_set_reader : t -> Type.reader_t -> Type.t
        val type_set_builder : t -> Type.t -> Type.t
        val type_init : t -> Type.t
        val has_value : t -> bool
        val value_get : t -> Value.t
        val value_set_reader : t -> Value.reader_t -> Value.t
        val value_set_builder : t -> Value.t -> Value.t
        val value_init : t -> Value.t
        val of_message : rw message_t -> t
        val to_message : t -> rw message_t
        val to_reader : t -> reader_t
        val init_root : ?message_size:int -> unit -> t
      end
      module Interface : sig
        type t = builder_t_Interface_16728431493453586831
        type reader_t = reader_t_Interface_16728431493453586831
        val has_methods : t -> bool
        val methods_get : t -> (rw, Method.t, array_t) Capnp.Array.t
        val methods_get_list : t -> Method.t list
        val methods_get_array : t -> Method.t array
        val methods_set : t -> (rw, Method.t, array_t) Capnp.Array.t -> (rw, Method.t, array_t) Capnp.Array.t
        val methods_set_list : t -> Method.t list -> (rw, Method.t, array_t) Capnp.Array.t
        val methods_set_array : t -> Method.t array -> (rw, Method.t, array_t) Capnp.Array.t
        val methods_init : t -> int -> (rw, Method.t, array_t) Capnp.Array.t
        val has_extends : t -> bool
        val extends_get : t -> (rw, Uint64.t, array_t) Capnp.Array.t
        val extends_get_list : t -> Uint64.t list
        val extends_get_array : t -> Uint64.t array
        val extends_set : t -> (rw, Uint64.t, array_t) Capnp.Array.t -> (rw, Uint64.t, array_t) Capnp.Array.t
        val extends_set_list : t -> Uint64.t list -> (rw, Uint64.t, array_t) Capnp.Array.t
        val extends_set_array : t -> Uint64.t array -> (rw, Uint64.t, array_t) Capnp.Array.t
        val extends_init : t -> int -> (rw, Uint64.t, array_t) Capnp.Array.t
        val of_message : rw message_t -> t
        val to_message : t -> rw message_t
        val to_reader : t -> reader_t
        val init_root : ?message_size:int -> unit -> t
      end
      module NestedNode : sig
        type t = builder_t_NestedNode_16050641862814319170
        type reader_t = reader_t_NestedNode_16050641862814319170
        val has_name : t -> bool
        val name_get : t -> string
        val name_set : t -> string -> unit
        val id_get : t -> Uint64.t
        val id_get_int_exn : t -> int
        val id_set : t -> Uint64.t -> unit
        val id_set_int_exn : t -> int -> unit
        val of_message : rw message_t -> t
        val to_message : t -> rw message_t
        val to_reader : t -> reader_t
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
      val struct_init : t -> Struct.t
      val enum_init : t -> Enum.t
      val interface_init : t -> Interface.t
      val const_init : t -> Const.t
      val annotation_init : t -> Annotation.t
      val id_get : t -> Uint64.t
      val id_get_int_exn : t -> int
      val id_set : t -> Uint64.t -> unit
      val id_set_int_exn : t -> int -> unit
      val has_display_name : t -> bool
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
      val has_nested_nodes : t -> bool
      val nested_nodes_get : t -> (rw, NestedNode.t, array_t) Capnp.Array.t
      val nested_nodes_get_list : t -> NestedNode.t list
      val nested_nodes_get_array : t -> NestedNode.t array
      val nested_nodes_set : t -> (rw, NestedNode.t, array_t) Capnp.Array.t -> (rw, NestedNode.t, array_t) Capnp.Array.t
      val nested_nodes_set_list : t -> NestedNode.t list -> (rw, NestedNode.t, array_t) Capnp.Array.t
      val nested_nodes_set_array : t -> NestedNode.t array -> (rw, NestedNode.t, array_t) Capnp.Array.t
      val nested_nodes_init : t -> int -> (rw, NestedNode.t, array_t) Capnp.Array.t
      val has_annotations : t -> bool
      val annotations_get : t -> (rw, builder_t_Annotation_17422339044421236034, array_t) Capnp.Array.t
      val annotations_get_list : t -> builder_t_Annotation_17422339044421236034 list
      val annotations_get_array : t -> builder_t_Annotation_17422339044421236034 array
      val annotations_set : t -> (rw, builder_t_Annotation_17422339044421236034, array_t) Capnp.Array.t -> (rw, builder_t_Annotation_17422339044421236034, array_t) Capnp.Array.t
      val annotations_set_list : t -> builder_t_Annotation_17422339044421236034 list -> (rw, builder_t_Annotation_17422339044421236034, array_t) Capnp.Array.t
      val annotations_set_array : t -> builder_t_Annotation_17422339044421236034 array -> (rw, builder_t_Annotation_17422339044421236034, array_t) Capnp.Array.t
      val annotations_init : t -> int -> (rw, builder_t_Annotation_17422339044421236034, array_t) Capnp.Array.t
      val of_message : rw message_t -> t
      val to_message : t -> rw message_t
      val to_reader : t -> reader_t
      val init_root : ?message_size:int -> unit -> t
    end
    module CodeGeneratorRequest : sig
      type t = builder_t_CodeGeneratorRequest_13818529054586492878
      type reader_t = reader_t_CodeGeneratorRequest_13818529054586492878
      module RequestedFile : sig
        type t = builder_t_RequestedFile_14981803260258615394
        type reader_t = reader_t_RequestedFile_14981803260258615394
        module Import : sig
          type t = builder_t_Import_12560611460656617445
          type reader_t = reader_t_Import_12560611460656617445
          val id_get : t -> Uint64.t
          val id_get_int_exn : t -> int
          val id_set : t -> Uint64.t -> unit
          val id_set_int_exn : t -> int -> unit
          val has_name : t -> bool
          val name_get : t -> string
          val name_set : t -> string -> unit
          val of_message : rw message_t -> t
          val to_message : t -> rw message_t
          val to_reader : t -> reader_t
          val init_root : ?message_size:int -> unit -> t
        end
        val id_get : t -> Uint64.t
        val id_get_int_exn : t -> int
        val id_set : t -> Uint64.t -> unit
        val id_set_int_exn : t -> int -> unit
        val has_filename : t -> bool
        val filename_get : t -> string
        val filename_set : t -> string -> unit
        val has_imports : t -> bool
        val imports_get : t -> (rw, Import.t, array_t) Capnp.Array.t
        val imports_get_list : t -> Import.t list
        val imports_get_array : t -> Import.t array
        val imports_set : t -> (rw, Import.t, array_t) Capnp.Array.t -> (rw, Import.t, array_t) Capnp.Array.t
        val imports_set_list : t -> Import.t list -> (rw, Import.t, array_t) Capnp.Array.t
        val imports_set_array : t -> Import.t array -> (rw, Import.t, array_t) Capnp.Array.t
        val imports_init : t -> int -> (rw, Import.t, array_t) Capnp.Array.t
        val of_message : rw message_t -> t
        val to_message : t -> rw message_t
        val to_reader : t -> reader_t
        val init_root : ?message_size:int -> unit -> t
      end
      val has_nodes : t -> bool
      val nodes_get : t -> (rw, Node.t, array_t) Capnp.Array.t
      val nodes_get_list : t -> Node.t list
      val nodes_get_array : t -> Node.t array
      val nodes_set : t -> (rw, Node.t, array_t) Capnp.Array.t -> (rw, Node.t, array_t) Capnp.Array.t
      val nodes_set_list : t -> Node.t list -> (rw, Node.t, array_t) Capnp.Array.t
      val nodes_set_array : t -> Node.t array -> (rw, Node.t, array_t) Capnp.Array.t
      val nodes_init : t -> int -> (rw, Node.t, array_t) Capnp.Array.t
      val has_requested_files : t -> bool
      val requested_files_get : t -> (rw, RequestedFile.t, array_t) Capnp.Array.t
      val requested_files_get_list : t -> RequestedFile.t list
      val requested_files_get_array : t -> RequestedFile.t array
      val requested_files_set : t -> (rw, RequestedFile.t, array_t) Capnp.Array.t -> (rw, RequestedFile.t, array_t) Capnp.Array.t
      val requested_files_set_list : t -> RequestedFile.t list -> (rw, RequestedFile.t, array_t) Capnp.Array.t
      val requested_files_set_array : t -> RequestedFile.t array -> (rw, RequestedFile.t, array_t) Capnp.Array.t
      val requested_files_init : t -> int -> (rw, RequestedFile.t, array_t) Capnp.Array.t
      val of_message : rw message_t -> t
      val to_message : t -> rw message_t
      val to_reader : t -> reader_t
      val init_root : ?message_size:int -> unit -> t
    end
  end
end

module DefaultsMessage_ = Capnp.BytesMessage
module DefaultsCommon_  = Capnp.Runtime.Common.Make(DefaultsMessage_)

let _builder_defaults_message =
  let message_segments = [
    Bytes.unsafe_of_string "\
    \x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\
    \x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\
    \x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\
    \x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00";
  ] in
  DefaultsMessage_.Message.readonly
    (DefaultsMessage_.Message.of_storage message_segments)

module Make (MessageWrapper : Capnp.MessageSig.S) = struct
  let invalid_msg = Capnp.Message.invalid_msg

  module RA_ = struct
    open Capnp.Runtime
    INCLUDE "reader-inc.ml"
  end
  module BA_ = struct
    open Capnp.Runtime
    module NM = MessageWrapper
    INCLUDE "builder-inc.ml"
  end

  type 'cap message_t = 'cap MessageWrapper.Message.t

  module C2b2b_13688829037717245569 = C2b2b.Make(MessageWrapper)

  type reader_t_Slot_14133145859926553711 = ro MessageWrapper.StructStorage.t option
  type builder_t_Slot_14133145859926553711 = rw MessageWrapper.StructStorage.t
  type reader_t_Group_14626792032033250577 = ro MessageWrapper.StructStorage.t option
  type builder_t_Group_14626792032033250577 = rw MessageWrapper.StructStorage.t
  type reader_t_Ordinal_13515537513213004774 = ro MessageWrapper.StructStorage.t option
  type builder_t_Ordinal_13515537513213004774 = rw MessageWrapper.StructStorage.t
  type reader_t_Field_11145653318641710175 = ro MessageWrapper.StructStorage.t option
  type builder_t_Field_11145653318641710175 = rw MessageWrapper.StructStorage.t
  type reader_t_Import_12560611460656617445 = ro MessageWrapper.StructStorage.t option
  type builder_t_Import_12560611460656617445 = rw MessageWrapper.StructStorage.t
  type reader_t_RequestedFile_14981803260258615394 = ro MessageWrapper.StructStorage.t option
  type builder_t_RequestedFile_14981803260258615394 = rw MessageWrapper.StructStorage.t
  type reader_t_CodeGeneratorRequest_13818529054586492878 = ro MessageWrapper.StructStorage.t option
  type builder_t_CodeGeneratorRequest_13818529054586492878 = rw MessageWrapper.StructStorage.t
  type reader_t_NestedNode_16050641862814319170 = ro MessageWrapper.StructStorage.t option
  type builder_t_NestedNode_16050641862814319170 = rw MessageWrapper.StructStorage.t
  type reader_t_Interface_16728431493453586831 = ro MessageWrapper.StructStorage.t option
  type builder_t_Interface_16728431493453586831 = rw MessageWrapper.StructStorage.t
  type reader_t_Const_12793219851699983392 = ro MessageWrapper.StructStorage.t option
  type builder_t_Const_12793219851699983392 = rw MessageWrapper.StructStorage.t
  type reader_t_Annotation_17011813041836786320 = ro MessageWrapper.StructStorage.t option
  type builder_t_Annotation_17011813041836786320 = rw MessageWrapper.StructStorage.t
  type reader_t_Enum_13063450714778629528 = ro MessageWrapper.StructStorage.t option
  type builder_t_Enum_13063450714778629528 = rw MessageWrapper.StructStorage.t
  type reader_t_Struct_11430331134483579957 = ro MessageWrapper.StructStorage.t option
  type builder_t_Struct_11430331134483579957 = rw MessageWrapper.StructStorage.t
  type reader_t_Node_16610026722781537303 = ro MessageWrapper.StructStorage.t option
  type builder_t_Node_16610026722781537303 = rw MessageWrapper.StructStorage.t
  type reader_t_Value_14853958794117909659 = ro MessageWrapper.StructStorage.t option
  type builder_t_Value_14853958794117909659 = rw MessageWrapper.StructStorage.t
  type reader_t_Enumerant_10919677598968879693 = ro MessageWrapper.StructStorage.t option
  type builder_t_Enumerant_10919677598968879693 = rw MessageWrapper.StructStorage.t
  type reader_t_Annotation_17422339044421236034 = ro MessageWrapper.StructStorage.t option
  type builder_t_Annotation_17422339044421236034 = rw MessageWrapper.StructStorage.t
  type reader_t_Method_10736806783679155584 = ro MessageWrapper.StructStorage.t option
  type builder_t_Method_10736806783679155584 = rw MessageWrapper.StructStorage.t
  type reader_t_Struct_12410354185295152851 = ro MessageWrapper.StructStorage.t option
  type builder_t_Struct_12410354185295152851 = rw MessageWrapper.StructStorage.t
  type reader_t_List_9792858745991129751 = ro MessageWrapper.StructStorage.t option
  type builder_t_List_9792858745991129751 = rw MessageWrapper.StructStorage.t
  type reader_t_Interface_17116997365232503999 = ro MessageWrapper.StructStorage.t option
  type builder_t_Interface_17116997365232503999 = rw MessageWrapper.StructStorage.t
  type reader_t_Enum_11389172934837766057 = ro MessageWrapper.StructStorage.t option
  type builder_t_Enum_11389172934837766057 = rw MessageWrapper.StructStorage.t
  type reader_t_Type_15020482145304562784 = ro MessageWrapper.StructStorage.t option
  type builder_t_Type_15020482145304562784 = rw MessageWrapper.StructStorage.t

  module ElementSize_15102134695616452902 = struct
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
    let decode u16 = match u16 with
      | 0 -> Empty
      | 1 -> Bit
      | 2 -> Byte
      | 3 -> TwoBytes
      | 4 -> FourBytes
      | 5 -> EightBytes
      | 6 -> Pointer
      | 7 -> InlineComposite
      | v -> Undefined v
    let encode_safe enum = match enum with
      | Empty -> 0
      | Bit -> 1
      | Byte -> 2
      | TwoBytes -> 3
      | FourBytes -> 4
      | EightBytes -> 5
      | Pointer -> 6
      | InlineComposite -> 7
      | Undefined x -> invalid_msg "Cannot encode undefined enum value."
    let encode_unsafe enum = match enum with
      | Empty -> 0
      | Bit -> 1
      | Byte -> 2
      | TwoBytes -> 3
      | FourBytes -> 4
      | EightBytes -> 5
      | Pointer -> 6
      | InlineComposite -> 7
      | Undefined x -> x
  end
  module DefaultsCopier_ =
    Capnp.Runtime.BuilderOps.Make(Capnp.BytesMessage)(MessageWrapper)

  let _reader_defaults_message =
    MessageWrapper.Message.create
      (DefaultsMessage_.Message.total_size _builder_defaults_message)


  module Reader = struct
    type array_t = ro MessageWrapper.ListStorage.t
    type builder_array_t = rw MessageWrapper.ListStorage.t
    type pointer_t = ro MessageWrapper.Slice.t option

    module ElementSize = struct
      type t = ElementSize_15102134695616452902.t =
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
      type t = reader_t_Type_15020482145304562784
      type builder_t = builder_t_Type_15020482145304562784
      module Enum = struct
        type t = reader_t_Enum_11389172934837766057
        type builder_t = builder_t_Enum_11389172934837766057
        let type_id_get x =
          let data = RA_.get_data_region x in
          RA_.get_uint64 ~default:Uint64.zero ~byte_ofs:8 data
        let type_id_get_int_exn x =
          Capnp.Runtime.Util.int_of_uint64_exn (type_id_get x)
        let of_message x = RA_.get_root_struct (RA_.Message.readonly x)
        let of_builder x = Some (RA_.StructStorage.readonly x)
      end
      module Interface = struct
        type t = reader_t_Interface_17116997365232503999
        type builder_t = builder_t_Interface_17116997365232503999
        let type_id_get x =
          let data = RA_.get_data_region x in
          RA_.get_uint64 ~default:Uint64.zero ~byte_ofs:8 data
        let type_id_get_int_exn x =
          Capnp.Runtime.Util.int_of_uint64_exn (type_id_get x)
        let of_message x = RA_.get_root_struct (RA_.Message.readonly x)
        let of_builder x = Some (RA_.StructStorage.readonly x)
      end
      module List = struct
        type t = reader_t_List_9792858745991129751
        type builder_t = builder_t_List_9792858745991129751
        let has_element_type x = 
          (let ptr = RA_.get_pointer_bytes x 0 in
          RA_.has_field ptr)
        let element_type_get x = 
          (let ptr = RA_.get_pointer_bytes x 0 in
          RA_.get_struct ptr)
        let of_message x = RA_.get_root_struct (RA_.Message.readonly x)
        let of_builder x = Some (RA_.StructStorage.readonly x)
      end
      module Struct = struct
        type t = reader_t_Struct_12410354185295152851
        type builder_t = builder_t_Struct_12410354185295152851
        let type_id_get x =
          let data = RA_.get_data_region x in
          RA_.get_uint64 ~default:Uint64.zero ~byte_ofs:8 data
        let type_id_get_int_exn x =
          Capnp.Runtime.Util.int_of_uint64_exn (type_id_get x)
        let of_message x = RA_.get_root_struct (RA_.Message.readonly x)
        let of_builder x = Some (RA_.StructStorage.readonly x)
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
      let any_pointer_get x = ()
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
      let get x =
        let data = RA_.get_data_region x in
        match RA_.get_uint16 ~default:0 ~byte_ofs:0 data with
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
      let of_builder x = Some (RA_.StructStorage.readonly x)
    end
    module Value = struct
      type t = reader_t_Value_14853958794117909659
      type builder_t = builder_t_Value_14853958794117909659
      let void_get x = ()
      let bool_get x =
        let data = RA_.get_data_region x in
        RA_.get_bit ~default:false ~byte_ofs:2 ~bit_ofs:0 data
      let int8_get x =
        let data = RA_.get_data_region x in
        RA_.get_int8 ~default:(0) ~byte_ofs:2 data
      let int16_get x =
        let data = RA_.get_data_region x in
        RA_.get_int16 ~default:(0) ~byte_ofs:2 data
      let int32_get x =
        let data = RA_.get_data_region x in
        RA_.get_int32 ~default:(0l) ~byte_ofs:4 data
      let int32_get_int_exn x =
        Capnp.Runtime.Util.int_of_int32_exn (int32_get x)
      let int64_get x =
        let data = RA_.get_data_region x in
        RA_.get_int64 ~default:(0L) ~byte_ofs:8 data
      let int64_get_int_exn x =
        Capnp.Runtime.Util.int_of_int64_exn (int64_get x)
      let uint8_get x =
        let data = RA_.get_data_region x in
        RA_.get_uint8 ~default:0 ~byte_ofs:2 data
      let uint16_get x =
        let data = RA_.get_data_region x in
        RA_.get_uint16 ~default:0 ~byte_ofs:2 data
      let uint32_get x =
        let data = RA_.get_data_region x in
        RA_.get_uint32 ~default:Uint32.zero ~byte_ofs:4 data
      let uint32_get_int_exn x =
        Capnp.Runtime.Util.int_of_uint32_exn (uint32_get x)
      let uint64_get x =
        let data = RA_.get_data_region x in
        RA_.get_uint64 ~default:Uint64.zero ~byte_ofs:8 data
      let uint64_get_int_exn x =
        Capnp.Runtime.Util.int_of_uint64_exn (uint64_get x)
      let float32_get x =
        let data = RA_.get_data_region x in
        RA_.get_float32 ~default_bits:(0l) ~byte_ofs:4 data
      let float64_get x =
        let data = RA_.get_data_region x in
        RA_.get_float64 ~default_bits:(0L) ~byte_ofs:8 data
      let has_text x =
        let ptr = RA_.get_pointer_bytes x 0 in
        RA_.has_field ptr
      let text_get x =
        let ptr = RA_.get_pointer_bytes x 0 in
        RA_.get_text ~default:"" ptr
      let has_data x =
        let ptr = RA_.get_pointer_bytes x 0 in
        RA_.has_field ptr
      let data_get x =
        let ptr = RA_.get_pointer_bytes x 0 in
        RA_.get_blob ~default:"" ptr
      let list_get x =
        let ptr = RA_.get_pointer_bytes x 0 in
        RA_.get_pointer ptr
      let enum_get x =
        let data = RA_.get_data_region x in
        RA_.get_uint16 ~default:0 ~byte_ofs:2 data
      let struct_get x =
        let ptr = RA_.get_pointer_bytes x 0 in
        RA_.get_pointer ptr
      let interface_get x = ()
      let any_pointer_get x =
        let ptr = RA_.get_pointer_bytes x 0 in
        RA_.get_pointer ptr
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
      let get x =
        let data = RA_.get_data_region x in
        match RA_.get_uint16 ~default:0 ~byte_ofs:0 data with
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
        | 18 -> AnyPointer (any_pointer_get x)
        | v -> Undefined v
      let of_message x = RA_.get_root_struct (RA_.Message.readonly x)
      let of_builder x = Some (RA_.StructStorage.readonly x)
    end
    module Annotation = struct
      type t = reader_t_Annotation_17422339044421236034
      type builder_t = builder_t_Annotation_17422339044421236034
      let id_get x =
        let data = RA_.get_data_region x in
        RA_.get_uint64 ~default:Uint64.zero ~byte_ofs:0 data
      let id_get_int_exn x =
        Capnp.Runtime.Util.int_of_uint64_exn (id_get x)
      let has_value x = 
        (let ptr = RA_.get_pointer_bytes x 0 in
        RA_.has_field ptr)
      let value_get x = 
        (let ptr = RA_.get_pointer_bytes x 0 in
        RA_.get_struct ptr)
      let of_message x = RA_.get_root_struct (RA_.Message.readonly x)
      let of_builder x = Some (RA_.StructStorage.readonly x)
    end
    module Method = struct
      type t = reader_t_Method_10736806783679155584
      type builder_t = builder_t_Method_10736806783679155584
      let has_name x =
        let ptr = RA_.get_pointer_bytes x 0 in
        RA_.has_field ptr
      let name_get x =
        let ptr = RA_.get_pointer_bytes x 0 in
        RA_.get_text ~default:"" ptr
      let code_order_get x =
        let data = RA_.get_data_region x in
        RA_.get_uint16 ~default:0 ~byte_ofs:0 data
      let param_struct_type_get x =
        let data = RA_.get_data_region x in
        RA_.get_uint64 ~default:Uint64.zero ~byte_ofs:8 data
      let param_struct_type_get_int_exn x =
        Capnp.Runtime.Util.int_of_uint64_exn (param_struct_type_get x)
      let result_struct_type_get x =
        let data = RA_.get_data_region x in
        RA_.get_uint64 ~default:Uint64.zero ~byte_ofs:16 data
      let result_struct_type_get_int_exn x =
        Capnp.Runtime.Util.int_of_uint64_exn (result_struct_type_get x)
      let has_annotations x = 
        (let ptr = RA_.get_pointer_bytes x 1 in
        RA_.has_field ptr)
      let annotations_get x = 
        (let ptr = RA_.get_pointer_bytes x 1 in
        RA_.get_struct_list ptr)
      let annotations_get_list x =
        Capnp.Array.to_list (annotations_get x)
      let annotations_get_array x =
        Capnp.Array.to_array (annotations_get x)
      let of_message x = RA_.get_root_struct (RA_.Message.readonly x)
      let of_builder x = Some (RA_.StructStorage.readonly x)
    end
    module Enumerant = struct
      type t = reader_t_Enumerant_10919677598968879693
      type builder_t = builder_t_Enumerant_10919677598968879693
      let has_name x =
        let ptr = RA_.get_pointer_bytes x 0 in
        RA_.has_field ptr
      let name_get x =
        let ptr = RA_.get_pointer_bytes x 0 in
        RA_.get_text ~default:"" ptr
      let code_order_get x =
        let data = RA_.get_data_region x in
        RA_.get_uint16 ~default:0 ~byte_ofs:0 data
      let has_annotations x = 
        (let ptr = RA_.get_pointer_bytes x 1 in
        RA_.has_field ptr)
      let annotations_get x = 
        (let ptr = RA_.get_pointer_bytes x 1 in
        RA_.get_struct_list ptr)
      let annotations_get_list x =
        Capnp.Array.to_list (annotations_get x)
      let annotations_get_array x =
        Capnp.Array.to_array (annotations_get x)
      let of_message x = RA_.get_root_struct (RA_.Message.readonly x)
      let of_builder x = Some (RA_.StructStorage.readonly x)
    end
    module Field = struct
      type t = reader_t_Field_11145653318641710175
      type builder_t = builder_t_Field_11145653318641710175
      let no_discriminant =
        65535
      module Ordinal = struct
        type t = reader_t_Ordinal_13515537513213004774
        type builder_t = builder_t_Ordinal_13515537513213004774
        let implicit_get x = ()
        let explicit_get x =
          let data = RA_.get_data_region x in
          RA_.get_uint16 ~default:0 ~byte_ofs:12 data
        type unnamed_union_t =
          | Implicit
          | Explicit of int
          | Undefined of int
        let get x =
          let data = RA_.get_data_region x in
          match RA_.get_uint16 ~default:0 ~byte_ofs:10 data with
          | 0 -> Implicit
          | 1 -> Explicit (explicit_get x)
          | v -> Undefined v
        let of_message x = RA_.get_root_struct (RA_.Message.readonly x)
        let of_builder x = Some (RA_.StructStorage.readonly x)
      end
      module Group = struct
        type t = reader_t_Group_14626792032033250577
        type builder_t = builder_t_Group_14626792032033250577
        let type_id_get x =
          let data = RA_.get_data_region x in
          RA_.get_uint64 ~default:Uint64.zero ~byte_ofs:16 data
        let type_id_get_int_exn x =
          Capnp.Runtime.Util.int_of_uint64_exn (type_id_get x)
        let of_message x = RA_.get_root_struct (RA_.Message.readonly x)
        let of_builder x = Some (RA_.StructStorage.readonly x)
      end
      module Slot = struct
        type t = reader_t_Slot_14133145859926553711
        type builder_t = builder_t_Slot_14133145859926553711
        let offset_get x =
          let data = RA_.get_data_region x in
          RA_.get_uint32 ~default:Uint32.zero ~byte_ofs:4 data
        let offset_get_int_exn x =
          Capnp.Runtime.Util.int_of_uint32_exn (offset_get x)
        let has_type x = 
          (let ptr = RA_.get_pointer_bytes x 2 in
          RA_.has_field ptr)
        let type_get x = 
          (let ptr = RA_.get_pointer_bytes x 2 in
          RA_.get_struct ptr)
        let has_default_value x = 
          (let ptr = RA_.get_pointer_bytes x 3 in
          RA_.has_field ptr)
        let default_value_get x = 
          (let ptr = RA_.get_pointer_bytes x 3 in
          RA_.get_struct ptr)
        let had_explicit_default_get x =
          let data = RA_.get_data_region x in
          RA_.get_bit ~default:false ~byte_ofs:16 ~bit_ofs:0 data
        let of_message x = RA_.get_root_struct (RA_.Message.readonly x)
        let of_builder x = Some (RA_.StructStorage.readonly x)
      end
      let slot_get x = x
      let group_get x = x
      type unnamed_union_t =
        | Slot of Slot.t
        | Group of Group.t
        | Undefined of int
      let get x =
        let data = RA_.get_data_region x in
        match RA_.get_uint16 ~default:0 ~byte_ofs:8 data with
        | 0 -> Slot (slot_get x)
        | 1 -> Group (group_get x)
        | v -> Undefined v
      let has_name x =
        let ptr = RA_.get_pointer_bytes x 0 in
        RA_.has_field ptr
      let name_get x =
        let ptr = RA_.get_pointer_bytes x 0 in
        RA_.get_text ~default:"" ptr
      let code_order_get x =
        let data = RA_.get_data_region x in
        RA_.get_uint16 ~default:0 ~byte_ofs:0 data
      let has_annotations x = 
        (let ptr = RA_.get_pointer_bytes x 1 in
        RA_.has_field ptr)
      let annotations_get x = 
        (let ptr = RA_.get_pointer_bytes x 1 in
        RA_.get_struct_list ptr)
      let annotations_get_list x =
        Capnp.Array.to_list (annotations_get x)
      let annotations_get_array x =
        Capnp.Array.to_array (annotations_get x)
      let discriminant_value_get x =
        let data = RA_.get_data_region x in
        RA_.get_uint16 ~default:65535 ~byte_ofs:2 data
      let ordinal_get x = x
      let of_message x = RA_.get_root_struct (RA_.Message.readonly x)
      let of_builder x = Some (RA_.StructStorage.readonly x)
    end
    module Node = struct
      type t = reader_t_Node_16610026722781537303
      type builder_t = builder_t_Node_16610026722781537303
      module Struct = struct
        type t = reader_t_Struct_11430331134483579957
        type builder_t = builder_t_Struct_11430331134483579957
        let data_word_count_get x =
          let data = RA_.get_data_region x in
          RA_.get_uint16 ~default:0 ~byte_ofs:14 data
        let pointer_count_get x =
          let data = RA_.get_data_region x in
          RA_.get_uint16 ~default:0 ~byte_ofs:24 data
        let preferred_list_encoding_get x =
          let data = RA_.get_data_region x in
          let discr = RA_.get_uint16 ~default:0 ~byte_ofs:26 data in
          ElementSize_15102134695616452902.decode discr
        let is_group_get x =
          let data = RA_.get_data_region x in
          RA_.get_bit ~default:false ~byte_ofs:28 ~bit_ofs:0 data
        let discriminant_count_get x =
          let data = RA_.get_data_region x in
          RA_.get_uint16 ~default:0 ~byte_ofs:30 data
        let discriminant_offset_get x =
          let data = RA_.get_data_region x in
          RA_.get_uint32 ~default:Uint32.zero ~byte_ofs:32 data
        let discriminant_offset_get_int_exn x =
          Capnp.Runtime.Util.int_of_uint32_exn (discriminant_offset_get x)
        let has_fields x = 
          (let ptr = RA_.get_pointer_bytes x 3 in
          RA_.has_field ptr)
        let fields_get x = 
          (let ptr = RA_.get_pointer_bytes x 3 in
          RA_.get_struct_list ptr)
        let fields_get_list x =
          Capnp.Array.to_list (fields_get x)
        let fields_get_array x =
          Capnp.Array.to_array (fields_get x)
        let of_message x = RA_.get_root_struct (RA_.Message.readonly x)
        let of_builder x = Some (RA_.StructStorage.readonly x)
      end
      module Enum = struct
        type t = reader_t_Enum_13063450714778629528
        type builder_t = builder_t_Enum_13063450714778629528
        let has_enumerants x = 
          (let ptr = RA_.get_pointer_bytes x 3 in
          RA_.has_field ptr)
        let enumerants_get x = 
          (let ptr = RA_.get_pointer_bytes x 3 in
          RA_.get_struct_list ptr)
        let enumerants_get_list x =
          Capnp.Array.to_list (enumerants_get x)
        let enumerants_get_array x =
          Capnp.Array.to_array (enumerants_get x)
        let of_message x = RA_.get_root_struct (RA_.Message.readonly x)
        let of_builder x = Some (RA_.StructStorage.readonly x)
      end
      module Annotation = struct
        type t = reader_t_Annotation_17011813041836786320
        type builder_t = builder_t_Annotation_17011813041836786320
        let has_type x = 
          (let ptr = RA_.get_pointer_bytes x 3 in
          RA_.has_field ptr)
        let type_get x = 
          (let ptr = RA_.get_pointer_bytes x 3 in
          RA_.get_struct ptr)
        let targets_file_get x =
          let data = RA_.get_data_region x in
          RA_.get_bit ~default:false ~byte_ofs:14 ~bit_ofs:0 data
        let targets_const_get x =
          let data = RA_.get_data_region x in
          RA_.get_bit ~default:false ~byte_ofs:14 ~bit_ofs:1 data
        let targets_enum_get x =
          let data = RA_.get_data_region x in
          RA_.get_bit ~default:false ~byte_ofs:14 ~bit_ofs:2 data
        let targets_enumerant_get x =
          let data = RA_.get_data_region x in
          RA_.get_bit ~default:false ~byte_ofs:14 ~bit_ofs:3 data
        let targets_struct_get x =
          let data = RA_.get_data_region x in
          RA_.get_bit ~default:false ~byte_ofs:14 ~bit_ofs:4 data
        let targets_field_get x =
          let data = RA_.get_data_region x in
          RA_.get_bit ~default:false ~byte_ofs:14 ~bit_ofs:5 data
        let targets_union_get x =
          let data = RA_.get_data_region x in
          RA_.get_bit ~default:false ~byte_ofs:14 ~bit_ofs:6 data
        let targets_group_get x =
          let data = RA_.get_data_region x in
          RA_.get_bit ~default:false ~byte_ofs:14 ~bit_ofs:7 data
        let targets_interface_get x =
          let data = RA_.get_data_region x in
          RA_.get_bit ~default:false ~byte_ofs:15 ~bit_ofs:0 data
        let targets_method_get x =
          let data = RA_.get_data_region x in
          RA_.get_bit ~default:false ~byte_ofs:15 ~bit_ofs:1 data
        let targets_param_get x =
          let data = RA_.get_data_region x in
          RA_.get_bit ~default:false ~byte_ofs:15 ~bit_ofs:2 data
        let targets_annotation_get x =
          let data = RA_.get_data_region x in
          RA_.get_bit ~default:false ~byte_ofs:15 ~bit_ofs:3 data
        let of_message x = RA_.get_root_struct (RA_.Message.readonly x)
        let of_builder x = Some (RA_.StructStorage.readonly x)
      end
      module Const = struct
        type t = reader_t_Const_12793219851699983392
        type builder_t = builder_t_Const_12793219851699983392
        let has_type x = 
          (let ptr = RA_.get_pointer_bytes x 3 in
          RA_.has_field ptr)
        let type_get x = 
          (let ptr = RA_.get_pointer_bytes x 3 in
          RA_.get_struct ptr)
        let has_value x = 
          (let ptr = RA_.get_pointer_bytes x 4 in
          RA_.has_field ptr)
        let value_get x = 
          (let ptr = RA_.get_pointer_bytes x 4 in
          RA_.get_struct ptr)
        let of_message x = RA_.get_root_struct (RA_.Message.readonly x)
        let of_builder x = Some (RA_.StructStorage.readonly x)
      end
      module Interface = struct
        type t = reader_t_Interface_16728431493453586831
        type builder_t = builder_t_Interface_16728431493453586831
        let has_methods x = 
          (let ptr = RA_.get_pointer_bytes x 3 in
          RA_.has_field ptr)
        let methods_get x = 
          (let ptr = RA_.get_pointer_bytes x 3 in
          RA_.get_struct_list ptr)
        let methods_get_list x =
          Capnp.Array.to_list (methods_get x)
        let methods_get_array x =
          Capnp.Array.to_array (methods_get x)
        let has_extends x = 
          (let ptr = RA_.get_pointer_bytes x 4 in
          RA_.has_field ptr)
        let extends_get x =
          let ptr = RA_.get_pointer_bytes x 4 in
          RA_.get_uint64_list ptr
        let extends_get_list x =
          Capnp.Array.to_list (extends_get x)
        let extends_get_array x =
          Capnp.Array.to_array (extends_get x)
        let of_message x = RA_.get_root_struct (RA_.Message.readonly x)
        let of_builder x = Some (RA_.StructStorage.readonly x)
      end
      module NestedNode = struct
        type t = reader_t_NestedNode_16050641862814319170
        type builder_t = builder_t_NestedNode_16050641862814319170
        let has_name x =
          let ptr = RA_.get_pointer_bytes x 0 in
          RA_.has_field ptr
        let name_get x =
          let ptr = RA_.get_pointer_bytes x 0 in
          RA_.get_text ~default:"" ptr
        let id_get x =
          let data = RA_.get_data_region x in
          RA_.get_uint64 ~default:Uint64.zero ~byte_ofs:0 data
        let id_get_int_exn x =
          Capnp.Runtime.Util.int_of_uint64_exn (id_get x)
        let of_message x = RA_.get_root_struct (RA_.Message.readonly x)
        let of_builder x = Some (RA_.StructStorage.readonly x)
      end
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
        | Undefined of int
      let get x =
        let data = RA_.get_data_region x in
        match RA_.get_uint16 ~default:0 ~byte_ofs:12 data with
        | 0 -> File
        | 1 -> Struct (struct_get x)
        | 2 -> Enum (enum_get x)
        | 3 -> Interface (interface_get x)
        | 4 -> Const (const_get x)
        | 5 -> Annotation (annotation_get x)
        | v -> Undefined v
      let id_get x =
        let data = RA_.get_data_region x in
        RA_.get_uint64 ~default:Uint64.zero ~byte_ofs:0 data
      let id_get_int_exn x =
        Capnp.Runtime.Util.int_of_uint64_exn (id_get x)
      let has_display_name x =
        let ptr = RA_.get_pointer_bytes x 0 in
        RA_.has_field ptr
      let display_name_get x =
        let ptr = RA_.get_pointer_bytes x 0 in
        RA_.get_text ~default:"" ptr
      let display_name_prefix_length_get x =
        let data = RA_.get_data_region x in
        RA_.get_uint32 ~default:Uint32.zero ~byte_ofs:8 data
      let display_name_prefix_length_get_int_exn x =
        Capnp.Runtime.Util.int_of_uint32_exn (display_name_prefix_length_get x)
      let scope_id_get x =
        let data = RA_.get_data_region x in
        RA_.get_uint64 ~default:Uint64.zero ~byte_ofs:16 data
      let scope_id_get_int_exn x =
        Capnp.Runtime.Util.int_of_uint64_exn (scope_id_get x)
      let has_nested_nodes x = 
        (let ptr = RA_.get_pointer_bytes x 1 in
        RA_.has_field ptr)
      let nested_nodes_get x = 
        (let ptr = RA_.get_pointer_bytes x 1 in
        RA_.get_struct_list ptr)
      let nested_nodes_get_list x =
        Capnp.Array.to_list (nested_nodes_get x)
      let nested_nodes_get_array x =
        Capnp.Array.to_array (nested_nodes_get x)
      let has_annotations x = 
        (let ptr = RA_.get_pointer_bytes x 2 in
        RA_.has_field ptr)
      let annotations_get x = 
        (let ptr = RA_.get_pointer_bytes x 2 in
        RA_.get_struct_list ptr)
      let annotations_get_list x =
        Capnp.Array.to_list (annotations_get x)
      let annotations_get_array x =
        Capnp.Array.to_array (annotations_get x)
      let of_message x = RA_.get_root_struct (RA_.Message.readonly x)
      let of_builder x = Some (RA_.StructStorage.readonly x)
    end
    module CodeGeneratorRequest = struct
      type t = reader_t_CodeGeneratorRequest_13818529054586492878
      type builder_t = builder_t_CodeGeneratorRequest_13818529054586492878
      module RequestedFile = struct
        type t = reader_t_RequestedFile_14981803260258615394
        type builder_t = builder_t_RequestedFile_14981803260258615394
        module Import = struct
          type t = reader_t_Import_12560611460656617445
          type builder_t = builder_t_Import_12560611460656617445
          let id_get x =
            let data = RA_.get_data_region x in
            RA_.get_uint64 ~default:Uint64.zero ~byte_ofs:0 data
          let id_get_int_exn x =
            Capnp.Runtime.Util.int_of_uint64_exn (id_get x)
          let has_name x =
            let ptr = RA_.get_pointer_bytes x 0 in
            RA_.has_field ptr
          let name_get x =
            let ptr = RA_.get_pointer_bytes x 0 in
            RA_.get_text ~default:"" ptr
          let of_message x = RA_.get_root_struct (RA_.Message.readonly x)
          let of_builder x = Some (RA_.StructStorage.readonly x)
        end
        let id_get x =
          let data = RA_.get_data_region x in
          RA_.get_uint64 ~default:Uint64.zero ~byte_ofs:0 data
        let id_get_int_exn x =
          Capnp.Runtime.Util.int_of_uint64_exn (id_get x)
        let has_filename x =
          let ptr = RA_.get_pointer_bytes x 0 in
          RA_.has_field ptr
        let filename_get x =
          let ptr = RA_.get_pointer_bytes x 0 in
          RA_.get_text ~default:"" ptr
        let has_imports x = 
          (let ptr = RA_.get_pointer_bytes x 1 in
          RA_.has_field ptr)
        let imports_get x = 
          (let ptr = RA_.get_pointer_bytes x 1 in
          RA_.get_struct_list ptr)
        let imports_get_list x =
          Capnp.Array.to_list (imports_get x)
        let imports_get_array x =
          Capnp.Array.to_array (imports_get x)
        let of_message x = RA_.get_root_struct (RA_.Message.readonly x)
        let of_builder x = Some (RA_.StructStorage.readonly x)
      end
      let has_nodes x = 
        (let ptr = RA_.get_pointer_bytes x 0 in
        RA_.has_field ptr)
      let nodes_get x = 
        (let ptr = RA_.get_pointer_bytes x 0 in
        RA_.get_struct_list ptr)
      let nodes_get_list x =
        Capnp.Array.to_list (nodes_get x)
      let nodes_get_array x =
        Capnp.Array.to_array (nodes_get x)
      let has_requested_files x = 
        (let ptr = RA_.get_pointer_bytes x 1 in
        RA_.has_field ptr)
      let requested_files_get x = 
        (let ptr = RA_.get_pointer_bytes x 1 in
        RA_.get_struct_list ptr)
      let requested_files_get_list x =
        Capnp.Array.to_list (requested_files_get x)
      let requested_files_get_array x =
        Capnp.Array.to_array (requested_files_get x)
      let of_message x = RA_.get_root_struct (RA_.Message.readonly x)
      let of_builder x = Some (RA_.StructStorage.readonly x)
    end
  end

  module Builder = struct
    type array_t = Reader.builder_array_t
    type reader_array_t = Reader.array_t
    type pointer_t = rw MessageWrapper.Slice.t

    module ElementSize = struct
      type t = ElementSize_15102134695616452902.t =
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
      type t = builder_t_Type_15020482145304562784
      type reader_t = reader_t_Type_15020482145304562784
      module Enum = struct
        type t = builder_t_Enum_11389172934837766057
        type reader_t = reader_t_Enum_11389172934837766057
        let type_id_get x =
          let data = BA_.get_data_region x in
          BA_.get_uint64 ~default:Uint64.zero ~byte_ofs:8 data
        let type_id_get_int_exn x =
          Capnp.Runtime.Util.int_of_uint64_exn (type_id_get x)
        let type_id_set x v =
          let data = BA_.get_data_region x in
          BA_.set_uint64 ~default:Uint64.zero ~byte_ofs:8 v data
        let type_id_set_int_exn x v = type_id_set x (Capnp.Runtime.Util.uint64_of_int_exn v)
        let of_message x = BA_.get_root_struct ~data_words:2 ~pointer_words:1 x
        let to_message x = x.BA_.NC.StructStorage.data.MessageWrapper.Slice.msg
        let to_reader x = Some (RA_.StructStorage.readonly x)
        let init_root ?message_size () =
          BA_.alloc_root_struct ?message_size ~data_words:2 ~pointer_words:1 ()
      end
      module Interface = struct
        type t = builder_t_Interface_17116997365232503999
        type reader_t = reader_t_Interface_17116997365232503999
        let type_id_get x =
          let data = BA_.get_data_region x in
          BA_.get_uint64 ~default:Uint64.zero ~byte_ofs:8 data
        let type_id_get_int_exn x =
          Capnp.Runtime.Util.int_of_uint64_exn (type_id_get x)
        let type_id_set x v =
          let data = BA_.get_data_region x in
          BA_.set_uint64 ~default:Uint64.zero ~byte_ofs:8 v data
        let type_id_set_int_exn x v = type_id_set x (Capnp.Runtime.Util.uint64_of_int_exn v)
        let of_message x = BA_.get_root_struct ~data_words:2 ~pointer_words:1 x
        let to_message x = x.BA_.NC.StructStorage.data.MessageWrapper.Slice.msg
        let to_reader x = Some (RA_.StructStorage.readonly x)
        let init_root ?message_size () =
          BA_.alloc_root_struct ?message_size ~data_words:2 ~pointer_words:1 ()
      end
      module List = struct
        type t = builder_t_List_9792858745991129751
        type reader_t = reader_t_List_9792858745991129751
        let has_element_type x = 
          (let ptr = BA_.get_pointer_bytes x 0 in
          BA_.has_field ptr)
        let element_type_get x = 
          (let ptr = BA_.get_pointer_bytes x 0 in
          BA_.get_struct ~data_words:2 ~pointer_words:1 ptr)
        let element_type_set_reader x v = 
          (let ptr = BA_.get_pointer_bytes x 0 in
          BA_.set_struct ~data_words:2 ~pointer_words:1 (v) ptr)
        let element_type_set_builder x v = 
          (let ptr = BA_.get_pointer_bytes x 0 in
          BA_.set_struct ~data_words:2 ~pointer_words:1 (Some (v)) ptr)
        let element_type_init x = 
          (let ptr = BA_.get_pointer_bytes x 0 in
          BA_.init_struct ~data_words:2 ~pointer_words:1 ptr)
        let of_message x = BA_.get_root_struct ~data_words:2 ~pointer_words:1 x
        let to_message x = x.BA_.NC.StructStorage.data.MessageWrapper.Slice.msg
        let to_reader x = Some (RA_.StructStorage.readonly x)
        let init_root ?message_size () =
          BA_.alloc_root_struct ?message_size ~data_words:2 ~pointer_words:1 ()
      end
      module Struct = struct
        type t = builder_t_Struct_12410354185295152851
        type reader_t = reader_t_Struct_12410354185295152851
        let type_id_get x =
          let data = BA_.get_data_region x in
          BA_.get_uint64 ~default:Uint64.zero ~byte_ofs:8 data
        let type_id_get_int_exn x =
          Capnp.Runtime.Util.int_of_uint64_exn (type_id_get x)
        let type_id_set x v =
          let data = BA_.get_data_region x in
          BA_.set_uint64 ~default:Uint64.zero ~byte_ofs:8 v data
        let type_id_set_int_exn x v = type_id_set x (Capnp.Runtime.Util.uint64_of_int_exn v)
        let of_message x = BA_.get_root_struct ~data_words:2 ~pointer_words:1 x
        let to_message x = x.BA_.NC.StructStorage.data.MessageWrapper.Slice.msg
        let to_reader x = Some (RA_.StructStorage.readonly x)
        let init_root ?message_size () =
          BA_.alloc_root_struct ?message_size ~data_words:2 ~pointer_words:1 ()
      end
      let void_get x = ()
      let void_set x =
        let data = BA_.get_data_region ~discr:{BA_.Discr.value=0; BA_.Discr.byte_ofs=0} x in
        BA_.set_void data
      let bool_get x = ()
      let bool_set x =
        let data = BA_.get_data_region ~discr:{BA_.Discr.value=1; BA_.Discr.byte_ofs=0} x in
        BA_.set_void data
      let int8_get x = ()
      let int8_set x =
        let data = BA_.get_data_region ~discr:{BA_.Discr.value=2; BA_.Discr.byte_ofs=0} x in
        BA_.set_void data
      let int16_get x = ()
      let int16_set x =
        let data = BA_.get_data_region ~discr:{BA_.Discr.value=3; BA_.Discr.byte_ofs=0} x in
        BA_.set_void data
      let int32_get x = ()
      let int32_set x =
        let data = BA_.get_data_region ~discr:{BA_.Discr.value=4; BA_.Discr.byte_ofs=0} x in
        BA_.set_void data
      let int64_get x = ()
      let int64_set x =
        let data = BA_.get_data_region ~discr:{BA_.Discr.value=5; BA_.Discr.byte_ofs=0} x in
        BA_.set_void data
      let uint8_get x = ()
      let uint8_set x =
        let data = BA_.get_data_region ~discr:{BA_.Discr.value=6; BA_.Discr.byte_ofs=0} x in
        BA_.set_void data
      let uint16_get x = ()
      let uint16_set x =
        let data = BA_.get_data_region ~discr:{BA_.Discr.value=7; BA_.Discr.byte_ofs=0} x in
        BA_.set_void data
      let uint32_get x = ()
      let uint32_set x =
        let data = BA_.get_data_region ~discr:{BA_.Discr.value=8; BA_.Discr.byte_ofs=0} x in
        BA_.set_void data
      let uint64_get x = ()
      let uint64_set x =
        let data = BA_.get_data_region ~discr:{BA_.Discr.value=9; BA_.Discr.byte_ofs=0} x in
        BA_.set_void data
      let float32_get x = ()
      let float32_set x =
        let data = BA_.get_data_region ~discr:{BA_.Discr.value=10; BA_.Discr.byte_ofs=0} x in
        BA_.set_void data
      let float64_get x = ()
      let float64_set x =
        let data = BA_.get_data_region ~discr:{BA_.Discr.value=11; BA_.Discr.byte_ofs=0} x in
        BA_.set_void data
      let text_get x = ()
      let text_set x =
        let data = BA_.get_data_region ~discr:{BA_.Discr.value=12; BA_.Discr.byte_ofs=0} x in
        BA_.set_void data
      let data_get x = ()
      let data_set x =
        let data = BA_.get_data_region ~discr:{BA_.Discr.value=13; BA_.Discr.byte_ofs=0} x in
        BA_.set_void data
      let list_get x = x
      let list_init x =
        let data = x.BA_.NC.StructStorage.data in
        let pointers = x.BA_.NC.StructStorage.pointers in
        let () = ignore data in
        let () = ignore pointers in
        let () = BA_.set_opt_discriminant data
          (Some {BA_.Discr.value=14; BA_.Discr.byte_ofs=0})
        in
        let () =
          let ptr = {
            pointers with
            MessageWrapper.Slice.start = pointers.MessageWrapper.Slice.start + 0;
            MessageWrapper.Slice.len = 8;
          } in
          let () = BA_.BOps.deep_zero_pointer ptr in
          MessageWrapper.Slice.set_int64 ptr 0 0L
        in
        x
      let enum_get x = x
      let enum_init x =
        let data = x.BA_.NC.StructStorage.data in
        let pointers = x.BA_.NC.StructStorage.pointers in
        let () = ignore data in
        let () = ignore pointers in
        let () = BA_.set_opt_discriminant data
          (Some {BA_.Discr.value=15; BA_.Discr.byte_ofs=0})
        in
        let () = BA_.set_int64 ~default:0L ~byte_ofs:8 0L data in
        x
      let struct_get x = x
      let struct_init x =
        let data = x.BA_.NC.StructStorage.data in
        let pointers = x.BA_.NC.StructStorage.pointers in
        let () = ignore data in
        let () = ignore pointers in
        let () = BA_.set_opt_discriminant data
          (Some {BA_.Discr.value=16; BA_.Discr.byte_ofs=0})
        in
        let () = BA_.set_int64 ~default:0L ~byte_ofs:8 0L data in
        x
      let interface_get x = x
      let interface_init x =
        let data = x.BA_.NC.StructStorage.data in
        let pointers = x.BA_.NC.StructStorage.pointers in
        let () = ignore data in
        let () = ignore pointers in
        let () = BA_.set_opt_discriminant data
          (Some {BA_.Discr.value=17; BA_.Discr.byte_ofs=0})
        in
        let () = BA_.set_int64 ~default:0L ~byte_ofs:8 0L data in
        x
      let any_pointer_get x = ()
      let any_pointer_set x =
        let data = BA_.get_data_region ~discr:{BA_.Discr.value=18; BA_.Discr.byte_ofs=0} x in
        BA_.set_void data
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
      let get x =
        let data = BA_.get_data_region x in
        match BA_.get_uint16 ~default:0 ~byte_ofs:0 data with
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
      let to_message x = x.BA_.NC.StructStorage.data.MessageWrapper.Slice.msg
      let to_reader x = Some (RA_.StructStorage.readonly x)
      let init_root ?message_size () =
        BA_.alloc_root_struct ?message_size ~data_words:2 ~pointer_words:1 ()
    end
    module Value = struct
      type t = builder_t_Value_14853958794117909659
      type reader_t = reader_t_Value_14853958794117909659
      let void_get x = ()
      let void_set x =
        let data = BA_.get_data_region ~discr:{BA_.Discr.value=0; BA_.Discr.byte_ofs=0} x in
        BA_.set_void data
      let bool_get x =
        let data = BA_.get_data_region x in
        BA_.get_bit ~default:false ~byte_ofs:2 ~bit_ofs:0 data
      let bool_set x v =
        let data = BA_.get_data_region ~discr:{BA_.Discr.value=1; BA_.Discr.byte_ofs=0} x in
        BA_.set_bit ~default:false ~byte_ofs:2 ~bit_ofs:0 v data
      let int8_get x =
        let data = BA_.get_data_region x in
        BA_.get_int8 ~default:(0) ~byte_ofs:2 data
      let int8_set_exn x v =
        let data = BA_.get_data_region ~discr:{BA_.Discr.value=2; BA_.Discr.byte_ofs=0} x in
        BA_.set_int8 ~default:(0) ~byte_ofs:2 v data
      let int16_get x =
        let data = BA_.get_data_region x in
        BA_.get_int16 ~default:(0) ~byte_ofs:2 data
      let int16_set_exn x v =
        let data = BA_.get_data_region ~discr:{BA_.Discr.value=3; BA_.Discr.byte_ofs=0} x in
        BA_.set_int16 ~default:(0) ~byte_ofs:2 v data
      let int32_get x =
        let data = BA_.get_data_region x in
        BA_.get_int32 ~default:(0l) ~byte_ofs:4 data
      let int32_get_int_exn x =
        Capnp.Runtime.Util.int_of_int32_exn (int32_get x)
      let int32_set x v =
        let data = BA_.get_data_region ~discr:{BA_.Discr.value=4; BA_.Discr.byte_ofs=0} x in
        BA_.set_int32 ~default:(0l) ~byte_ofs:4 v data
      let int32_set_int_exn x v = int32_set x (Capnp.Runtime.Util.int32_of_int_exn v)
      let int64_get x =
        let data = BA_.get_data_region x in
        BA_.get_int64 ~default:(0L) ~byte_ofs:8 data
      let int64_get_int_exn x =
        Capnp.Runtime.Util.int_of_int64_exn (int64_get x)
      let int64_set x v =
        let data = BA_.get_data_region ~discr:{BA_.Discr.value=5; BA_.Discr.byte_ofs=0} x in
        BA_.set_int64 ~default:(0L) ~byte_ofs:8 v data
      let int64_set_int x v = int64_set x (Int64.of_int v)
      let uint8_get x =
        let data = BA_.get_data_region x in
        BA_.get_uint8 ~default:0 ~byte_ofs:2 data
      let uint8_set_exn x v =
        let data = BA_.get_data_region ~discr:{BA_.Discr.value=6; BA_.Discr.byte_ofs=0} x in
        BA_.set_uint8 ~default:0 ~byte_ofs:2 v data
      let uint16_get x =
        let data = BA_.get_data_region x in
        BA_.get_uint16 ~default:0 ~byte_ofs:2 data
      let uint16_set_exn x v =
        let data = BA_.get_data_region ~discr:{BA_.Discr.value=7; BA_.Discr.byte_ofs=0} x in
        BA_.set_uint16 ~default:0 ~byte_ofs:2 v data
      let uint32_get x =
        let data = BA_.get_data_region x in
        BA_.get_uint32 ~default:Uint32.zero ~byte_ofs:4 data
      let uint32_get_int_exn x =
        Capnp.Runtime.Util.int_of_uint32_exn (uint32_get x)
      let uint32_set x v =
        let data = BA_.get_data_region ~discr:{BA_.Discr.value=8; BA_.Discr.byte_ofs=0} x in
        BA_.set_uint32 ~default:Uint32.zero ~byte_ofs:4 v data
      let uint32_set_int_exn x v = uint32_set x (Capnp.Runtime.Util.uint32_of_int_exn v)
      let uint64_get x =
        let data = BA_.get_data_region x in
        BA_.get_uint64 ~default:Uint64.zero ~byte_ofs:8 data
      let uint64_get_int_exn x =
        Capnp.Runtime.Util.int_of_uint64_exn (uint64_get x)
      let uint64_set x v =
        let data = BA_.get_data_region ~discr:{BA_.Discr.value=9; BA_.Discr.byte_ofs=0} x in
        BA_.set_uint64 ~default:Uint64.zero ~byte_ofs:8 v data
      let uint64_set_int_exn x v = uint64_set x (Capnp.Runtime.Util.uint64_of_int_exn v)
      let float32_get x =
        let data = BA_.get_data_region x in
        BA_.get_float32 ~default_bits:(0l) ~byte_ofs:4 data
      let float32_set x v =
        let data = BA_.get_data_region ~discr:{BA_.Discr.value=10; BA_.Discr.byte_ofs=0} x in
        BA_.set_float32 ~default_bits:(0l) ~byte_ofs:4 v data
      let float64_get x =
        let data = BA_.get_data_region x in
        BA_.get_float64 ~default_bits:(0L) ~byte_ofs:8 data
      let float64_set x v =
        let data = BA_.get_data_region ~discr:{BA_.Discr.value=11; BA_.Discr.byte_ofs=0} x in
        BA_.set_float64 ~default_bits:(0L) ~byte_ofs:8 v data
      let has_text x =
        let ptr = BA_.get_pointer_bytes x 0 in
        BA_.has_field ptr
      let text_get x =
        let ptr = BA_.get_pointer_bytes x 0 in
        BA_.get_text ~default:"" ptr
      let text_set x v =
        let ptr = BA_.get_pointer_bytes ~discr:{BA_.Discr.value=12; BA_.Discr.byte_ofs=0} x 0 in
        BA_.set_text v ptr
      let has_data x =
        let ptr = BA_.get_pointer_bytes x 0 in
        BA_.has_field ptr
      let data_get x =
        let ptr = BA_.get_pointer_bytes x 0 in
        BA_.get_blob ~default:"" ptr
      let data_set x v =
        let ptr = BA_.get_pointer_bytes ~discr:{BA_.Discr.value=13; BA_.Discr.byte_ofs=0} x 0 in
        BA_.set_blob v ptr
      let list_get x =
        let ptr = BA_.get_pointer_bytes x 0 in
        BA_.get_pointer ptr
      let list_set x v =
        let ptr = BA_.get_pointer_bytes ~discr:{BA_.Discr.value=14; BA_.Discr.byte_ofs=0} x 0 in
        BA_.set_pointer v ptr
      let enum_get x =
        let data = BA_.get_data_region x in
        BA_.get_uint16 ~default:0 ~byte_ofs:2 data
      let enum_set_exn x v =
        let data = BA_.get_data_region ~discr:{BA_.Discr.value=15; BA_.Discr.byte_ofs=0} x in
        BA_.set_uint16 ~default:0 ~byte_ofs:2 v data
      let struct_get x =
        let ptr = BA_.get_pointer_bytes x 0 in
        BA_.get_pointer ptr
      let struct_set x v =
        let ptr = BA_.get_pointer_bytes ~discr:{BA_.Discr.value=16; BA_.Discr.byte_ofs=0} x 0 in
        BA_.set_pointer v ptr
      let interface_get x = ()
      let interface_set x =
        let data = BA_.get_data_region ~discr:{BA_.Discr.value=17; BA_.Discr.byte_ofs=0} x in
        BA_.set_void data
      let any_pointer_get x =
        let ptr = BA_.get_pointer_bytes x 0 in
        BA_.get_pointer ptr
      let any_pointer_set x v =
        let ptr = BA_.get_pointer_bytes ~discr:{BA_.Discr.value=18; BA_.Discr.byte_ofs=0} x 0 in
        BA_.set_pointer v ptr
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
      let get x =
        let data = BA_.get_data_region x in
        match BA_.get_uint16 ~default:0 ~byte_ofs:0 data with
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
        | 18 -> AnyPointer (any_pointer_get x)
        | v -> Undefined v
      let of_message x = BA_.get_root_struct ~data_words:2 ~pointer_words:1 x
      let to_message x = x.BA_.NC.StructStorage.data.MessageWrapper.Slice.msg
      let to_reader x = Some (RA_.StructStorage.readonly x)
      let init_root ?message_size () =
        BA_.alloc_root_struct ?message_size ~data_words:2 ~pointer_words:1 ()
    end
    module Annotation = struct
      type t = builder_t_Annotation_17422339044421236034
      type reader_t = reader_t_Annotation_17422339044421236034
      let id_get x =
        let data = BA_.get_data_region x in
        BA_.get_uint64 ~default:Uint64.zero ~byte_ofs:0 data
      let id_get_int_exn x =
        Capnp.Runtime.Util.int_of_uint64_exn (id_get x)
      let id_set x v =
        let data = BA_.get_data_region x in
        BA_.set_uint64 ~default:Uint64.zero ~byte_ofs:0 v data
      let id_set_int_exn x v = id_set x (Capnp.Runtime.Util.uint64_of_int_exn v)
      let has_value x = 
        (let ptr = BA_.get_pointer_bytes x 0 in
        BA_.has_field ptr)
      let value_get x = 
        (let ptr = BA_.get_pointer_bytes x 0 in
        BA_.get_struct ~data_words:2 ~pointer_words:1 ptr)
      let value_set_reader x v = 
        (let ptr = BA_.get_pointer_bytes x 0 in
        BA_.set_struct ~data_words:2 ~pointer_words:1 (v) ptr)
      let value_set_builder x v = 
        (let ptr = BA_.get_pointer_bytes x 0 in
        BA_.set_struct ~data_words:2 ~pointer_words:1 (Some (v)) ptr)
      let value_init x = 
        (let ptr = BA_.get_pointer_bytes x 0 in
        BA_.init_struct ~data_words:2 ~pointer_words:1 ptr)
      let of_message x = BA_.get_root_struct ~data_words:1 ~pointer_words:1 x
      let to_message x = x.BA_.NC.StructStorage.data.MessageWrapper.Slice.msg
      let to_reader x = Some (RA_.StructStorage.readonly x)
      let init_root ?message_size () =
        BA_.alloc_root_struct ?message_size ~data_words:1 ~pointer_words:1 ()
    end
    module Method = struct
      type t = builder_t_Method_10736806783679155584
      type reader_t = reader_t_Method_10736806783679155584
      let has_name x =
        let ptr = BA_.get_pointer_bytes x 0 in
        BA_.has_field ptr
      let name_get x =
        let ptr = BA_.get_pointer_bytes x 0 in
        BA_.get_text ~default:"" ptr
      let name_set x v =
        let ptr = BA_.get_pointer_bytes x 0 in
        BA_.set_text v ptr
      let code_order_get x =
        let data = BA_.get_data_region x in
        BA_.get_uint16 ~default:0 ~byte_ofs:0 data
      let code_order_set_exn x v =
        let data = BA_.get_data_region x in
        BA_.set_uint16 ~default:0 ~byte_ofs:0 v data
      let param_struct_type_get x =
        let data = BA_.get_data_region x in
        BA_.get_uint64 ~default:Uint64.zero ~byte_ofs:8 data
      let param_struct_type_get_int_exn x =
        Capnp.Runtime.Util.int_of_uint64_exn (param_struct_type_get x)
      let param_struct_type_set x v =
        let data = BA_.get_data_region x in
        BA_.set_uint64 ~default:Uint64.zero ~byte_ofs:8 v data
      let param_struct_type_set_int_exn x v = param_struct_type_set x (Capnp.Runtime.Util.uint64_of_int_exn v)
      let result_struct_type_get x =
        let data = BA_.get_data_region x in
        BA_.get_uint64 ~default:Uint64.zero ~byte_ofs:16 data
      let result_struct_type_get_int_exn x =
        Capnp.Runtime.Util.int_of_uint64_exn (result_struct_type_get x)
      let result_struct_type_set x v =
        let data = BA_.get_data_region x in
        BA_.set_uint64 ~default:Uint64.zero ~byte_ofs:16 v data
      let result_struct_type_set_int_exn x v = result_struct_type_set x (Capnp.Runtime.Util.uint64_of_int_exn v)
      let has_annotations x = 
        (let ptr = BA_.get_pointer_bytes x 1 in
        BA_.has_field ptr)
      let annotations_get x = 
        (let ptr = BA_.get_pointer_bytes x 1 in
        BA_.get_struct_list ~data_words:1 ~pointer_words:1 ptr)
      let annotations_get_list x =
        Capnp.Array.to_list (annotations_get x)
      let annotations_get_array x =
        Capnp.Array.to_array (annotations_get x)
      let annotations_set x v = 
        (let ptr = BA_.get_pointer_bytes x 1 in
        BA_.set_struct_list ~data_words:1 ~pointer_words:1 (v) ptr)
      let annotations_init x n = 
        (let ptr = BA_.get_pointer_bytes x 1 in
        BA_.init_struct_list ~data_words:1 ~pointer_words:1 n ptr)
      let annotations_set_list x v =
        let builder = annotations_init x (List.length v) in
        let () = List.iteri (fun i a -> Capnp.Array.set builder i a) v in
        builder
      let annotations_set_array x v =
        let builder = annotations_init x (Array.length v) in
        let () = Array.iteri (fun i a -> Capnp.Array.set builder i a) v in
        builder
      let of_message x = BA_.get_root_struct ~data_words:3 ~pointer_words:2 x
      let to_message x = x.BA_.NC.StructStorage.data.MessageWrapper.Slice.msg
      let to_reader x = Some (RA_.StructStorage.readonly x)
      let init_root ?message_size () =
        BA_.alloc_root_struct ?message_size ~data_words:3 ~pointer_words:2 ()
    end
    module Enumerant = struct
      type t = builder_t_Enumerant_10919677598968879693
      type reader_t = reader_t_Enumerant_10919677598968879693
      let has_name x =
        let ptr = BA_.get_pointer_bytes x 0 in
        BA_.has_field ptr
      let name_get x =
        let ptr = BA_.get_pointer_bytes x 0 in
        BA_.get_text ~default:"" ptr
      let name_set x v =
        let ptr = BA_.get_pointer_bytes x 0 in
        BA_.set_text v ptr
      let code_order_get x =
        let data = BA_.get_data_region x in
        BA_.get_uint16 ~default:0 ~byte_ofs:0 data
      let code_order_set_exn x v =
        let data = BA_.get_data_region x in
        BA_.set_uint16 ~default:0 ~byte_ofs:0 v data
      let has_annotations x = 
        (let ptr = BA_.get_pointer_bytes x 1 in
        BA_.has_field ptr)
      let annotations_get x = 
        (let ptr = BA_.get_pointer_bytes x 1 in
        BA_.get_struct_list ~data_words:1 ~pointer_words:1 ptr)
      let annotations_get_list x =
        Capnp.Array.to_list (annotations_get x)
      let annotations_get_array x =
        Capnp.Array.to_array (annotations_get x)
      let annotations_set x v = 
        (let ptr = BA_.get_pointer_bytes x 1 in
        BA_.set_struct_list ~data_words:1 ~pointer_words:1 (v) ptr)
      let annotations_init x n = 
        (let ptr = BA_.get_pointer_bytes x 1 in
        BA_.init_struct_list ~data_words:1 ~pointer_words:1 n ptr)
      let annotations_set_list x v =
        let builder = annotations_init x (List.length v) in
        let () = List.iteri (fun i a -> Capnp.Array.set builder i a) v in
        builder
      let annotations_set_array x v =
        let builder = annotations_init x (Array.length v) in
        let () = Array.iteri (fun i a -> Capnp.Array.set builder i a) v in
        builder
      let of_message x = BA_.get_root_struct ~data_words:1 ~pointer_words:2 x
      let to_message x = x.BA_.NC.StructStorage.data.MessageWrapper.Slice.msg
      let to_reader x = Some (RA_.StructStorage.readonly x)
      let init_root ?message_size () =
        BA_.alloc_root_struct ?message_size ~data_words:1 ~pointer_words:2 ()
    end
    module Field = struct
      type t = builder_t_Field_11145653318641710175
      type reader_t = reader_t_Field_11145653318641710175
      let no_discriminant =
        65535
      module Ordinal = struct
        type t = builder_t_Ordinal_13515537513213004774
        type reader_t = reader_t_Ordinal_13515537513213004774
        let implicit_get x = ()
        let implicit_set x =
          let data = BA_.get_data_region ~discr:{BA_.Discr.value=0; BA_.Discr.byte_ofs=10} x in
          BA_.set_void data
        let explicit_get x =
          let data = BA_.get_data_region x in
          BA_.get_uint16 ~default:0 ~byte_ofs:12 data
        let explicit_set_exn x v =
          let data = BA_.get_data_region ~discr:{BA_.Discr.value=1; BA_.Discr.byte_ofs=10} x in
          BA_.set_uint16 ~default:0 ~byte_ofs:12 v data
        type unnamed_union_t =
          | Implicit
          | Explicit of int
          | Undefined of int
        let get x =
          let data = BA_.get_data_region x in
          match BA_.get_uint16 ~default:0 ~byte_ofs:10 data with
          | 0 -> Implicit
          | 1 -> Explicit (explicit_get x)
          | v -> Undefined v
        let of_message x = BA_.get_root_struct ~data_words:3 ~pointer_words:4 x
        let to_message x = x.BA_.NC.StructStorage.data.MessageWrapper.Slice.msg
        let to_reader x = Some (RA_.StructStorage.readonly x)
        let init_root ?message_size () =
          BA_.alloc_root_struct ?message_size ~data_words:3 ~pointer_words:4 ()
      end
      module Group = struct
        type t = builder_t_Group_14626792032033250577
        type reader_t = reader_t_Group_14626792032033250577
        let type_id_get x =
          let data = BA_.get_data_region x in
          BA_.get_uint64 ~default:Uint64.zero ~byte_ofs:16 data
        let type_id_get_int_exn x =
          Capnp.Runtime.Util.int_of_uint64_exn (type_id_get x)
        let type_id_set x v =
          let data = BA_.get_data_region x in
          BA_.set_uint64 ~default:Uint64.zero ~byte_ofs:16 v data
        let type_id_set_int_exn x v = type_id_set x (Capnp.Runtime.Util.uint64_of_int_exn v)
        let of_message x = BA_.get_root_struct ~data_words:3 ~pointer_words:4 x
        let to_message x = x.BA_.NC.StructStorage.data.MessageWrapper.Slice.msg
        let to_reader x = Some (RA_.StructStorage.readonly x)
        let init_root ?message_size () =
          BA_.alloc_root_struct ?message_size ~data_words:3 ~pointer_words:4 ()
      end
      module Slot = struct
        type t = builder_t_Slot_14133145859926553711
        type reader_t = reader_t_Slot_14133145859926553711
        let offset_get x =
          let data = BA_.get_data_region x in
          BA_.get_uint32 ~default:Uint32.zero ~byte_ofs:4 data
        let offset_get_int_exn x =
          Capnp.Runtime.Util.int_of_uint32_exn (offset_get x)
        let offset_set x v =
          let data = BA_.get_data_region x in
          BA_.set_uint32 ~default:Uint32.zero ~byte_ofs:4 v data
        let offset_set_int_exn x v = offset_set x (Capnp.Runtime.Util.uint32_of_int_exn v)
        let has_type x = 
          (let ptr = BA_.get_pointer_bytes x 2 in
          BA_.has_field ptr)
        let type_get x = 
          (let ptr = BA_.get_pointer_bytes x 2 in
          BA_.get_struct ~data_words:2 ~pointer_words:1 ptr)
        let type_set_reader x v = 
          (let ptr = BA_.get_pointer_bytes x 2 in
          BA_.set_struct ~data_words:2 ~pointer_words:1 (v) ptr)
        let type_set_builder x v = 
          (let ptr = BA_.get_pointer_bytes x 2 in
          BA_.set_struct ~data_words:2 ~pointer_words:1 (Some (v)) ptr)
        let type_init x = 
          (let ptr = BA_.get_pointer_bytes x 2 in
          BA_.init_struct ~data_words:2 ~pointer_words:1 ptr)
        let has_default_value x = 
          (let ptr = BA_.get_pointer_bytes x 3 in
          BA_.has_field ptr)
        let default_value_get x = 
          (let ptr = BA_.get_pointer_bytes x 3 in
          BA_.get_struct ~data_words:2 ~pointer_words:1 ptr)
        let default_value_set_reader x v = 
          (let ptr = BA_.get_pointer_bytes x 3 in
          BA_.set_struct ~data_words:2 ~pointer_words:1 (v) ptr)
        let default_value_set_builder x v = 
          (let ptr = BA_.get_pointer_bytes x 3 in
          BA_.set_struct ~data_words:2 ~pointer_words:1 (Some (v)) ptr)
        let default_value_init x = 
          (let ptr = BA_.get_pointer_bytes x 3 in
          BA_.init_struct ~data_words:2 ~pointer_words:1 ptr)
        let had_explicit_default_get x =
          let data = BA_.get_data_region x in
          BA_.get_bit ~default:false ~byte_ofs:16 ~bit_ofs:0 data
        let had_explicit_default_set x v =
          let data = BA_.get_data_region x in
          BA_.set_bit ~default:false ~byte_ofs:16 ~bit_ofs:0 v data
        let of_message x = BA_.get_root_struct ~data_words:3 ~pointer_words:4 x
        let to_message x = x.BA_.NC.StructStorage.data.MessageWrapper.Slice.msg
        let to_reader x = Some (RA_.StructStorage.readonly x)
        let init_root ?message_size () =
          BA_.alloc_root_struct ?message_size ~data_words:3 ~pointer_words:4 ()
      end
      let slot_get x = x
      let slot_init x =
        let data = x.BA_.NC.StructStorage.data in
        let pointers = x.BA_.NC.StructStorage.pointers in
        let () = ignore data in
        let () = ignore pointers in
        let () = BA_.set_opt_discriminant data
          (Some {BA_.Discr.value=0; BA_.Discr.byte_ofs=8})
        in
        let () = BA_.set_int32 ~default:0l ~byte_ofs:4 0l data in
        let () =
          let ptr = {
            pointers with
            MessageWrapper.Slice.start = pointers.MessageWrapper.Slice.start + 16;
            MessageWrapper.Slice.len = 8;
          } in
          let () = BA_.BOps.deep_zero_pointer ptr in
          MessageWrapper.Slice.set_int64 ptr 0 0L
        in
        let () =
          let ptr = {
            pointers with
            MessageWrapper.Slice.start = pointers.MessageWrapper.Slice.start + 24;
            MessageWrapper.Slice.len = 8;
          } in
          let () = BA_.BOps.deep_zero_pointer ptr in
          MessageWrapper.Slice.set_int64 ptr 0 0L
        in
        let () = BA_.set_bit ~default:false ~byte_ofs:16 ~bit_ofs:0 false data in
        x
      let group_get x = x
      let group_init x =
        let data = x.BA_.NC.StructStorage.data in
        let pointers = x.BA_.NC.StructStorage.pointers in
        let () = ignore data in
        let () = ignore pointers in
        let () = BA_.set_opt_discriminant data
          (Some {BA_.Discr.value=1; BA_.Discr.byte_ofs=8})
        in
        let () = BA_.set_int64 ~default:0L ~byte_ofs:16 0L data in
        x
      type unnamed_union_t =
        | Slot of Slot.t
        | Group of Group.t
        | Undefined of int
      let get x =
        let data = BA_.get_data_region x in
        match BA_.get_uint16 ~default:0 ~byte_ofs:8 data with
        | 0 -> Slot (slot_get x)
        | 1 -> Group (group_get x)
        | v -> Undefined v
      let has_name x =
        let ptr = BA_.get_pointer_bytes x 0 in
        BA_.has_field ptr
      let name_get x =
        let ptr = BA_.get_pointer_bytes x 0 in
        BA_.get_text ~default:"" ptr
      let name_set x v =
        let ptr = BA_.get_pointer_bytes x 0 in
        BA_.set_text v ptr
      let code_order_get x =
        let data = BA_.get_data_region x in
        BA_.get_uint16 ~default:0 ~byte_ofs:0 data
      let code_order_set_exn x v =
        let data = BA_.get_data_region x in
        BA_.set_uint16 ~default:0 ~byte_ofs:0 v data
      let has_annotations x = 
        (let ptr = BA_.get_pointer_bytes x 1 in
        BA_.has_field ptr)
      let annotations_get x = 
        (let ptr = BA_.get_pointer_bytes x 1 in
        BA_.get_struct_list ~data_words:1 ~pointer_words:1 ptr)
      let annotations_get_list x =
        Capnp.Array.to_list (annotations_get x)
      let annotations_get_array x =
        Capnp.Array.to_array (annotations_get x)
      let annotations_set x v = 
        (let ptr = BA_.get_pointer_bytes x 1 in
        BA_.set_struct_list ~data_words:1 ~pointer_words:1 (v) ptr)
      let annotations_init x n = 
        (let ptr = BA_.get_pointer_bytes x 1 in
        BA_.init_struct_list ~data_words:1 ~pointer_words:1 n ptr)
      let annotations_set_list x v =
        let builder = annotations_init x (List.length v) in
        let () = List.iteri (fun i a -> Capnp.Array.set builder i a) v in
        builder
      let annotations_set_array x v =
        let builder = annotations_init x (Array.length v) in
        let () = Array.iteri (fun i a -> Capnp.Array.set builder i a) v in
        builder
      let discriminant_value_get x =
        let data = BA_.get_data_region x in
        BA_.get_uint16 ~default:65535 ~byte_ofs:2 data
      let discriminant_value_set_exn x v =
        let data = BA_.get_data_region x in
        BA_.set_uint16 ~default:65535 ~byte_ofs:2 v data
      let ordinal_get x = x
      let ordinal_init x =
        let data = x.BA_.NC.StructStorage.data in
        let pointers = x.BA_.NC.StructStorage.pointers in
        let () = ignore data in
        let () = ignore pointers in
        let () = BA_.set_int16 ~default:0 ~byte_ofs:10 0 data in
        let () = BA_.set_int16 ~default:0 ~byte_ofs:12 0 data in
        x
      let of_message x = BA_.get_root_struct ~data_words:3 ~pointer_words:4 x
      let to_message x = x.BA_.NC.StructStorage.data.MessageWrapper.Slice.msg
      let to_reader x = Some (RA_.StructStorage.readonly x)
      let init_root ?message_size () =
        BA_.alloc_root_struct ?message_size ~data_words:3 ~pointer_words:4 ()
    end
    module Node = struct
      type t = builder_t_Node_16610026722781537303
      type reader_t = reader_t_Node_16610026722781537303
      module Struct = struct
        type t = builder_t_Struct_11430331134483579957
        type reader_t = reader_t_Struct_11430331134483579957
        let data_word_count_get x =
          let data = BA_.get_data_region x in
          BA_.get_uint16 ~default:0 ~byte_ofs:14 data
        let data_word_count_set_exn x v =
          let data = BA_.get_data_region x in
          BA_.set_uint16 ~default:0 ~byte_ofs:14 v data
        let pointer_count_get x =
          let data = BA_.get_data_region x in
          BA_.get_uint16 ~default:0 ~byte_ofs:24 data
        let pointer_count_set_exn x v =
          let data = BA_.get_data_region x in
          BA_.set_uint16 ~default:0 ~byte_ofs:24 v data
        let preferred_list_encoding_get x =
          let data = BA_.get_data_region x in
          let discr = BA_.get_uint16 ~default:0 ~byte_ofs:26 data in
          ElementSize_15102134695616452902.decode discr
        let preferred_list_encoding_set x e =
          let data = BA_.get_data_region x in
          BA_.set_uint16 ~default:0 ~byte_ofs:26 (ElementSize_15102134695616452902.encode_safe e) data
        let preferred_list_encoding_set_unsafe x e =
          let data = BA_.get_data_region x in
          BA_.set_uint16 ~default:0 ~byte_ofs:26 (ElementSize_15102134695616452902.encode_unsafe e) data
        let is_group_get x =
          let data = BA_.get_data_region x in
          BA_.get_bit ~default:false ~byte_ofs:28 ~bit_ofs:0 data
        let is_group_set x v =
          let data = BA_.get_data_region x in
          BA_.set_bit ~default:false ~byte_ofs:28 ~bit_ofs:0 v data
        let discriminant_count_get x =
          let data = BA_.get_data_region x in
          BA_.get_uint16 ~default:0 ~byte_ofs:30 data
        let discriminant_count_set_exn x v =
          let data = BA_.get_data_region x in
          BA_.set_uint16 ~default:0 ~byte_ofs:30 v data
        let discriminant_offset_get x =
          let data = BA_.get_data_region x in
          BA_.get_uint32 ~default:Uint32.zero ~byte_ofs:32 data
        let discriminant_offset_get_int_exn x =
          Capnp.Runtime.Util.int_of_uint32_exn (discriminant_offset_get x)
        let discriminant_offset_set x v =
          let data = BA_.get_data_region x in
          BA_.set_uint32 ~default:Uint32.zero ~byte_ofs:32 v data
        let discriminant_offset_set_int_exn x v = discriminant_offset_set x (Capnp.Runtime.Util.uint32_of_int_exn v)
        let has_fields x = 
          (let ptr = BA_.get_pointer_bytes x 3 in
          BA_.has_field ptr)
        let fields_get x = 
          (let ptr = BA_.get_pointer_bytes x 3 in
          BA_.get_struct_list ~data_words:3 ~pointer_words:4 ptr)
        let fields_get_list x =
          Capnp.Array.to_list (fields_get x)
        let fields_get_array x =
          Capnp.Array.to_array (fields_get x)
        let fields_set x v = 
          (let ptr = BA_.get_pointer_bytes x 3 in
          BA_.set_struct_list ~data_words:3 ~pointer_words:4 (v) ptr)
        let fields_init x n = 
          (let ptr = BA_.get_pointer_bytes x 3 in
          BA_.init_struct_list ~data_words:3 ~pointer_words:4 n ptr)
        let fields_set_list x v =
          let builder = fields_init x (List.length v) in
          let () = List.iteri (fun i a -> Capnp.Array.set builder i a) v in
          builder
        let fields_set_array x v =
          let builder = fields_init x (Array.length v) in
          let () = Array.iteri (fun i a -> Capnp.Array.set builder i a) v in
          builder
        let of_message x = BA_.get_root_struct ~data_words:5 ~pointer_words:5 x
        let to_message x = x.BA_.NC.StructStorage.data.MessageWrapper.Slice.msg
        let to_reader x = Some (RA_.StructStorage.readonly x)
        let init_root ?message_size () =
          BA_.alloc_root_struct ?message_size ~data_words:5 ~pointer_words:5 ()
      end
      module Enum = struct
        type t = builder_t_Enum_13063450714778629528
        type reader_t = reader_t_Enum_13063450714778629528
        let has_enumerants x = 
          (let ptr = BA_.get_pointer_bytes x 3 in
          BA_.has_field ptr)
        let enumerants_get x = 
          (let ptr = BA_.get_pointer_bytes x 3 in
          BA_.get_struct_list ~data_words:1 ~pointer_words:2 ptr)
        let enumerants_get_list x =
          Capnp.Array.to_list (enumerants_get x)
        let enumerants_get_array x =
          Capnp.Array.to_array (enumerants_get x)
        let enumerants_set x v = 
          (let ptr = BA_.get_pointer_bytes x 3 in
          BA_.set_struct_list ~data_words:1 ~pointer_words:2 (v) ptr)
        let enumerants_init x n = 
          (let ptr = BA_.get_pointer_bytes x 3 in
          BA_.init_struct_list ~data_words:1 ~pointer_words:2 n ptr)
        let enumerants_set_list x v =
          let builder = enumerants_init x (List.length v) in
          let () = List.iteri (fun i a -> Capnp.Array.set builder i a) v in
          builder
        let enumerants_set_array x v =
          let builder = enumerants_init x (Array.length v) in
          let () = Array.iteri (fun i a -> Capnp.Array.set builder i a) v in
          builder
        let of_message x = BA_.get_root_struct ~data_words:5 ~pointer_words:5 x
        let to_message x = x.BA_.NC.StructStorage.data.MessageWrapper.Slice.msg
        let to_reader x = Some (RA_.StructStorage.readonly x)
        let init_root ?message_size () =
          BA_.alloc_root_struct ?message_size ~data_words:5 ~pointer_words:5 ()
      end
      module Annotation = struct
        type t = builder_t_Annotation_17011813041836786320
        type reader_t = reader_t_Annotation_17011813041836786320
        let has_type x = 
          (let ptr = BA_.get_pointer_bytes x 3 in
          BA_.has_field ptr)
        let type_get x = 
          (let ptr = BA_.get_pointer_bytes x 3 in
          BA_.get_struct ~data_words:2 ~pointer_words:1 ptr)
        let type_set_reader x v = 
          (let ptr = BA_.get_pointer_bytes x 3 in
          BA_.set_struct ~data_words:2 ~pointer_words:1 (v) ptr)
        let type_set_builder x v = 
          (let ptr = BA_.get_pointer_bytes x 3 in
          BA_.set_struct ~data_words:2 ~pointer_words:1 (Some (v)) ptr)
        let type_init x = 
          (let ptr = BA_.get_pointer_bytes x 3 in
          BA_.init_struct ~data_words:2 ~pointer_words:1 ptr)
        let targets_file_get x =
          let data = BA_.get_data_region x in
          BA_.get_bit ~default:false ~byte_ofs:14 ~bit_ofs:0 data
        let targets_file_set x v =
          let data = BA_.get_data_region x in
          BA_.set_bit ~default:false ~byte_ofs:14 ~bit_ofs:0 v data
        let targets_const_get x =
          let data = BA_.get_data_region x in
          BA_.get_bit ~default:false ~byte_ofs:14 ~bit_ofs:1 data
        let targets_const_set x v =
          let data = BA_.get_data_region x in
          BA_.set_bit ~default:false ~byte_ofs:14 ~bit_ofs:1 v data
        let targets_enum_get x =
          let data = BA_.get_data_region x in
          BA_.get_bit ~default:false ~byte_ofs:14 ~bit_ofs:2 data
        let targets_enum_set x v =
          let data = BA_.get_data_region x in
          BA_.set_bit ~default:false ~byte_ofs:14 ~bit_ofs:2 v data
        let targets_enumerant_get x =
          let data = BA_.get_data_region x in
          BA_.get_bit ~default:false ~byte_ofs:14 ~bit_ofs:3 data
        let targets_enumerant_set x v =
          let data = BA_.get_data_region x in
          BA_.set_bit ~default:false ~byte_ofs:14 ~bit_ofs:3 v data
        let targets_struct_get x =
          let data = BA_.get_data_region x in
          BA_.get_bit ~default:false ~byte_ofs:14 ~bit_ofs:4 data
        let targets_struct_set x v =
          let data = BA_.get_data_region x in
          BA_.set_bit ~default:false ~byte_ofs:14 ~bit_ofs:4 v data
        let targets_field_get x =
          let data = BA_.get_data_region x in
          BA_.get_bit ~default:false ~byte_ofs:14 ~bit_ofs:5 data
        let targets_field_set x v =
          let data = BA_.get_data_region x in
          BA_.set_bit ~default:false ~byte_ofs:14 ~bit_ofs:5 v data
        let targets_union_get x =
          let data = BA_.get_data_region x in
          BA_.get_bit ~default:false ~byte_ofs:14 ~bit_ofs:6 data
        let targets_union_set x v =
          let data = BA_.get_data_region x in
          BA_.set_bit ~default:false ~byte_ofs:14 ~bit_ofs:6 v data
        let targets_group_get x =
          let data = BA_.get_data_region x in
          BA_.get_bit ~default:false ~byte_ofs:14 ~bit_ofs:7 data
        let targets_group_set x v =
          let data = BA_.get_data_region x in
          BA_.set_bit ~default:false ~byte_ofs:14 ~bit_ofs:7 v data
        let targets_interface_get x =
          let data = BA_.get_data_region x in
          BA_.get_bit ~default:false ~byte_ofs:15 ~bit_ofs:0 data
        let targets_interface_set x v =
          let data = BA_.get_data_region x in
          BA_.set_bit ~default:false ~byte_ofs:15 ~bit_ofs:0 v data
        let targets_method_get x =
          let data = BA_.get_data_region x in
          BA_.get_bit ~default:false ~byte_ofs:15 ~bit_ofs:1 data
        let targets_method_set x v =
          let data = BA_.get_data_region x in
          BA_.set_bit ~default:false ~byte_ofs:15 ~bit_ofs:1 v data
        let targets_param_get x =
          let data = BA_.get_data_region x in
          BA_.get_bit ~default:false ~byte_ofs:15 ~bit_ofs:2 data
        let targets_param_set x v =
          let data = BA_.get_data_region x in
          BA_.set_bit ~default:false ~byte_ofs:15 ~bit_ofs:2 v data
        let targets_annotation_get x =
          let data = BA_.get_data_region x in
          BA_.get_bit ~default:false ~byte_ofs:15 ~bit_ofs:3 data
        let targets_annotation_set x v =
          let data = BA_.get_data_region x in
          BA_.set_bit ~default:false ~byte_ofs:15 ~bit_ofs:3 v data
        let of_message x = BA_.get_root_struct ~data_words:5 ~pointer_words:5 x
        let to_message x = x.BA_.NC.StructStorage.data.MessageWrapper.Slice.msg
        let to_reader x = Some (RA_.StructStorage.readonly x)
        let init_root ?message_size () =
          BA_.alloc_root_struct ?message_size ~data_words:5 ~pointer_words:5 ()
      end
      module Const = struct
        type t = builder_t_Const_12793219851699983392
        type reader_t = reader_t_Const_12793219851699983392
        let has_type x = 
          (let ptr = BA_.get_pointer_bytes x 3 in
          BA_.has_field ptr)
        let type_get x = 
          (let ptr = BA_.get_pointer_bytes x 3 in
          BA_.get_struct ~data_words:2 ~pointer_words:1 ptr)
        let type_set_reader x v = 
          (let ptr = BA_.get_pointer_bytes x 3 in
          BA_.set_struct ~data_words:2 ~pointer_words:1 (v) ptr)
        let type_set_builder x v = 
          (let ptr = BA_.get_pointer_bytes x 3 in
          BA_.set_struct ~data_words:2 ~pointer_words:1 (Some (v)) ptr)
        let type_init x = 
          (let ptr = BA_.get_pointer_bytes x 3 in
          BA_.init_struct ~data_words:2 ~pointer_words:1 ptr)
        let has_value x = 
          (let ptr = BA_.get_pointer_bytes x 4 in
          BA_.has_field ptr)
        let value_get x = 
          (let ptr = BA_.get_pointer_bytes x 4 in
          BA_.get_struct ~data_words:2 ~pointer_words:1 ptr)
        let value_set_reader x v = 
          (let ptr = BA_.get_pointer_bytes x 4 in
          BA_.set_struct ~data_words:2 ~pointer_words:1 (v) ptr)
        let value_set_builder x v = 
          (let ptr = BA_.get_pointer_bytes x 4 in
          BA_.set_struct ~data_words:2 ~pointer_words:1 (Some (v)) ptr)
        let value_init x = 
          (let ptr = BA_.get_pointer_bytes x 4 in
          BA_.init_struct ~data_words:2 ~pointer_words:1 ptr)
        let of_message x = BA_.get_root_struct ~data_words:5 ~pointer_words:5 x
        let to_message x = x.BA_.NC.StructStorage.data.MessageWrapper.Slice.msg
        let to_reader x = Some (RA_.StructStorage.readonly x)
        let init_root ?message_size () =
          BA_.alloc_root_struct ?message_size ~data_words:5 ~pointer_words:5 ()
      end
      module Interface = struct
        type t = builder_t_Interface_16728431493453586831
        type reader_t = reader_t_Interface_16728431493453586831
        let has_methods x = 
          (let ptr = BA_.get_pointer_bytes x 3 in
          BA_.has_field ptr)
        let methods_get x = 
          (let ptr = BA_.get_pointer_bytes x 3 in
          BA_.get_struct_list ~data_words:3 ~pointer_words:2 ptr)
        let methods_get_list x =
          Capnp.Array.to_list (methods_get x)
        let methods_get_array x =
          Capnp.Array.to_array (methods_get x)
        let methods_set x v = 
          (let ptr = BA_.get_pointer_bytes x 3 in
          BA_.set_struct_list ~data_words:3 ~pointer_words:2 (v) ptr)
        let methods_init x n = 
          (let ptr = BA_.get_pointer_bytes x 3 in
          BA_.init_struct_list ~data_words:3 ~pointer_words:2 n ptr)
        let methods_set_list x v =
          let builder = methods_init x (List.length v) in
          let () = List.iteri (fun i a -> Capnp.Array.set builder i a) v in
          builder
        let methods_set_array x v =
          let builder = methods_init x (Array.length v) in
          let () = Array.iteri (fun i a -> Capnp.Array.set builder i a) v in
          builder
        let has_extends x = 
          (let ptr = BA_.get_pointer_bytes x 4 in
          BA_.has_field ptr)
        let extends_get x =
          let ptr = BA_.get_pointer_bytes x 4 in
          BA_.get_uint64_list ptr
        let extends_get_list x =
          Capnp.Array.to_list (extends_get x)
        let extends_get_array x =
          Capnp.Array.to_array (extends_get x)
        let extends_set x v =
          let ptr = BA_.get_pointer_bytes x 4 in
          BA_.set_uint64_list v ptr
        let extends_init x n =
          let ptr = BA_.get_pointer_bytes x 4 in
          BA_.init_uint64_list n ptr
        let extends_set_list x v =
          let builder = extends_init x (List.length v) in
          let () = List.iteri (fun i a -> Capnp.Array.set builder i a) v in
          builder
        let extends_set_array x v =
          let builder = extends_init x (Array.length v) in
          let () = Array.iteri (fun i a -> Capnp.Array.set builder i a) v in
          builder
        let of_message x = BA_.get_root_struct ~data_words:5 ~pointer_words:5 x
        let to_message x = x.BA_.NC.StructStorage.data.MessageWrapper.Slice.msg
        let to_reader x = Some (RA_.StructStorage.readonly x)
        let init_root ?message_size () =
          BA_.alloc_root_struct ?message_size ~data_words:5 ~pointer_words:5 ()
      end
      module NestedNode = struct
        type t = builder_t_NestedNode_16050641862814319170
        type reader_t = reader_t_NestedNode_16050641862814319170
        let has_name x =
          let ptr = BA_.get_pointer_bytes x 0 in
          BA_.has_field ptr
        let name_get x =
          let ptr = BA_.get_pointer_bytes x 0 in
          BA_.get_text ~default:"" ptr
        let name_set x v =
          let ptr = BA_.get_pointer_bytes x 0 in
          BA_.set_text v ptr
        let id_get x =
          let data = BA_.get_data_region x in
          BA_.get_uint64 ~default:Uint64.zero ~byte_ofs:0 data
        let id_get_int_exn x =
          Capnp.Runtime.Util.int_of_uint64_exn (id_get x)
        let id_set x v =
          let data = BA_.get_data_region x in
          BA_.set_uint64 ~default:Uint64.zero ~byte_ofs:0 v data
        let id_set_int_exn x v = id_set x (Capnp.Runtime.Util.uint64_of_int_exn v)
        let of_message x = BA_.get_root_struct ~data_words:1 ~pointer_words:1 x
        let to_message x = x.BA_.NC.StructStorage.data.MessageWrapper.Slice.msg
        let to_reader x = Some (RA_.StructStorage.readonly x)
        let init_root ?message_size () =
          BA_.alloc_root_struct ?message_size ~data_words:1 ~pointer_words:1 ()
      end
      let file_get x = ()
      let file_set x =
        let data = BA_.get_data_region ~discr:{BA_.Discr.value=0; BA_.Discr.byte_ofs=12} x in
        BA_.set_void data
      let struct_get x = x
      let struct_init x =
        let data = x.BA_.NC.StructStorage.data in
        let pointers = x.BA_.NC.StructStorage.pointers in
        let () = ignore data in
        let () = ignore pointers in
        let () = BA_.set_opt_discriminant data
          (Some {BA_.Discr.value=1; BA_.Discr.byte_ofs=12})
        in
        let () = BA_.set_int16 ~default:0 ~byte_ofs:14 0 data in
        let () = BA_.set_int16 ~default:0 ~byte_ofs:24 0 data in
        let () = BA_.set_int16 ~default:0 ~byte_ofs:26 0 data in
        let () = BA_.set_bit ~default:false ~byte_ofs:28 ~bit_ofs:0 false data in
        let () = BA_.set_int16 ~default:0 ~byte_ofs:30 0 data in
        let () = BA_.set_int32 ~default:0l ~byte_ofs:32 0l data in
        let () =
          let ptr = {
            pointers with
            MessageWrapper.Slice.start = pointers.MessageWrapper.Slice.start + 24;
            MessageWrapper.Slice.len = 8;
          } in
          let () = BA_.BOps.deep_zero_pointer ptr in
          MessageWrapper.Slice.set_int64 ptr 0 0L
        in
        x
      let enum_get x = x
      let enum_init x =
        let data = x.BA_.NC.StructStorage.data in
        let pointers = x.BA_.NC.StructStorage.pointers in
        let () = ignore data in
        let () = ignore pointers in
        let () = BA_.set_opt_discriminant data
          (Some {BA_.Discr.value=2; BA_.Discr.byte_ofs=12})
        in
        let () =
          let ptr = {
            pointers with
            MessageWrapper.Slice.start = pointers.MessageWrapper.Slice.start + 24;
            MessageWrapper.Slice.len = 8;
          } in
          let () = BA_.BOps.deep_zero_pointer ptr in
          MessageWrapper.Slice.set_int64 ptr 0 0L
        in
        x
      let interface_get x = x
      let interface_init x =
        let data = x.BA_.NC.StructStorage.data in
        let pointers = x.BA_.NC.StructStorage.pointers in
        let () = ignore data in
        let () = ignore pointers in
        let () = BA_.set_opt_discriminant data
          (Some {BA_.Discr.value=3; BA_.Discr.byte_ofs=12})
        in
        let () =
          let ptr = {
            pointers with
            MessageWrapper.Slice.start = pointers.MessageWrapper.Slice.start + 24;
            MessageWrapper.Slice.len = 8;
          } in
          let () = BA_.BOps.deep_zero_pointer ptr in
          MessageWrapper.Slice.set_int64 ptr 0 0L
        in
        let () =
          let ptr = {
            pointers with
            MessageWrapper.Slice.start = pointers.MessageWrapper.Slice.start + 32;
            MessageWrapper.Slice.len = 8;
          } in
          let () = BA_.BOps.deep_zero_pointer ptr in
          MessageWrapper.Slice.set_int64 ptr 0 0L
        in
        x
      let const_get x = x
      let const_init x =
        let data = x.BA_.NC.StructStorage.data in
        let pointers = x.BA_.NC.StructStorage.pointers in
        let () = ignore data in
        let () = ignore pointers in
        let () = BA_.set_opt_discriminant data
          (Some {BA_.Discr.value=4; BA_.Discr.byte_ofs=12})
        in
        let () =
          let ptr = {
            pointers with
            MessageWrapper.Slice.start = pointers.MessageWrapper.Slice.start + 24;
            MessageWrapper.Slice.len = 8;
          } in
          let () = BA_.BOps.deep_zero_pointer ptr in
          MessageWrapper.Slice.set_int64 ptr 0 0L
        in
        let () =
          let ptr = {
            pointers with
            MessageWrapper.Slice.start = pointers.MessageWrapper.Slice.start + 32;
            MessageWrapper.Slice.len = 8;
          } in
          let () = BA_.BOps.deep_zero_pointer ptr in
          MessageWrapper.Slice.set_int64 ptr 0 0L
        in
        x
      let annotation_get x = x
      let annotation_init x =
        let data = x.BA_.NC.StructStorage.data in
        let pointers = x.BA_.NC.StructStorage.pointers in
        let () = ignore data in
        let () = ignore pointers in
        let () = BA_.set_opt_discriminant data
          (Some {BA_.Discr.value=5; BA_.Discr.byte_ofs=12})
        in
        let () =
          let ptr = {
            pointers with
            MessageWrapper.Slice.start = pointers.MessageWrapper.Slice.start + 24;
            MessageWrapper.Slice.len = 8;
          } in
          let () = BA_.BOps.deep_zero_pointer ptr in
          MessageWrapper.Slice.set_int64 ptr 0 0L
        in
        let () = BA_.set_bit ~default:false ~byte_ofs:14 ~bit_ofs:0 false data in
        let () = BA_.set_bit ~default:false ~byte_ofs:14 ~bit_ofs:1 false data in
        let () = BA_.set_bit ~default:false ~byte_ofs:14 ~bit_ofs:2 false data in
        let () = BA_.set_bit ~default:false ~byte_ofs:14 ~bit_ofs:3 false data in
        let () = BA_.set_bit ~default:false ~byte_ofs:14 ~bit_ofs:4 false data in
        let () = BA_.set_bit ~default:false ~byte_ofs:14 ~bit_ofs:5 false data in
        let () = BA_.set_bit ~default:false ~byte_ofs:14 ~bit_ofs:6 false data in
        let () = BA_.set_bit ~default:false ~byte_ofs:14 ~bit_ofs:7 false data in
        let () = BA_.set_bit ~default:false ~byte_ofs:15 ~bit_ofs:0 false data in
        let () = BA_.set_bit ~default:false ~byte_ofs:15 ~bit_ofs:1 false data in
        let () = BA_.set_bit ~default:false ~byte_ofs:15 ~bit_ofs:2 false data in
        let () = BA_.set_bit ~default:false ~byte_ofs:15 ~bit_ofs:3 false data in
        x
      type unnamed_union_t =
        | File
        | Struct of Struct.t
        | Enum of Enum.t
        | Interface of Interface.t
        | Const of Const.t
        | Annotation of Annotation.t
        | Undefined of int
      let get x =
        let data = BA_.get_data_region x in
        match BA_.get_uint16 ~default:0 ~byte_ofs:12 data with
        | 0 -> File
        | 1 -> Struct (struct_get x)
        | 2 -> Enum (enum_get x)
        | 3 -> Interface (interface_get x)
        | 4 -> Const (const_get x)
        | 5 -> Annotation (annotation_get x)
        | v -> Undefined v
      let id_get x =
        let data = BA_.get_data_region x in
        BA_.get_uint64 ~default:Uint64.zero ~byte_ofs:0 data
      let id_get_int_exn x =
        Capnp.Runtime.Util.int_of_uint64_exn (id_get x)
      let id_set x v =
        let data = BA_.get_data_region x in
        BA_.set_uint64 ~default:Uint64.zero ~byte_ofs:0 v data
      let id_set_int_exn x v = id_set x (Capnp.Runtime.Util.uint64_of_int_exn v)
      let has_display_name x =
        let ptr = BA_.get_pointer_bytes x 0 in
        BA_.has_field ptr
      let display_name_get x =
        let ptr = BA_.get_pointer_bytes x 0 in
        BA_.get_text ~default:"" ptr
      let display_name_set x v =
        let ptr = BA_.get_pointer_bytes x 0 in
        BA_.set_text v ptr
      let display_name_prefix_length_get x =
        let data = BA_.get_data_region x in
        BA_.get_uint32 ~default:Uint32.zero ~byte_ofs:8 data
      let display_name_prefix_length_get_int_exn x =
        Capnp.Runtime.Util.int_of_uint32_exn (display_name_prefix_length_get x)
      let display_name_prefix_length_set x v =
        let data = BA_.get_data_region x in
        BA_.set_uint32 ~default:Uint32.zero ~byte_ofs:8 v data
      let display_name_prefix_length_set_int_exn x v = display_name_prefix_length_set x (Capnp.Runtime.Util.uint32_of_int_exn v)
      let scope_id_get x =
        let data = BA_.get_data_region x in
        BA_.get_uint64 ~default:Uint64.zero ~byte_ofs:16 data
      let scope_id_get_int_exn x =
        Capnp.Runtime.Util.int_of_uint64_exn (scope_id_get x)
      let scope_id_set x v =
        let data = BA_.get_data_region x in
        BA_.set_uint64 ~default:Uint64.zero ~byte_ofs:16 v data
      let scope_id_set_int_exn x v = scope_id_set x (Capnp.Runtime.Util.uint64_of_int_exn v)
      let has_nested_nodes x = 
        (let ptr = BA_.get_pointer_bytes x 1 in
        BA_.has_field ptr)
      let nested_nodes_get x = 
        (let ptr = BA_.get_pointer_bytes x 1 in
        BA_.get_struct_list ~data_words:1 ~pointer_words:1 ptr)
      let nested_nodes_get_list x =
        Capnp.Array.to_list (nested_nodes_get x)
      let nested_nodes_get_array x =
        Capnp.Array.to_array (nested_nodes_get x)
      let nested_nodes_set x v = 
        (let ptr = BA_.get_pointer_bytes x 1 in
        BA_.set_struct_list ~data_words:1 ~pointer_words:1 (v) ptr)
      let nested_nodes_init x n = 
        (let ptr = BA_.get_pointer_bytes x 1 in
        BA_.init_struct_list ~data_words:1 ~pointer_words:1 n ptr)
      let nested_nodes_set_list x v =
        let builder = nested_nodes_init x (List.length v) in
        let () = List.iteri (fun i a -> Capnp.Array.set builder i a) v in
        builder
      let nested_nodes_set_array x v =
        let builder = nested_nodes_init x (Array.length v) in
        let () = Array.iteri (fun i a -> Capnp.Array.set builder i a) v in
        builder
      let has_annotations x = 
        (let ptr = BA_.get_pointer_bytes x 2 in
        BA_.has_field ptr)
      let annotations_get x = 
        (let ptr = BA_.get_pointer_bytes x 2 in
        BA_.get_struct_list ~data_words:1 ~pointer_words:1 ptr)
      let annotations_get_list x =
        Capnp.Array.to_list (annotations_get x)
      let annotations_get_array x =
        Capnp.Array.to_array (annotations_get x)
      let annotations_set x v = 
        (let ptr = BA_.get_pointer_bytes x 2 in
        BA_.set_struct_list ~data_words:1 ~pointer_words:1 (v) ptr)
      let annotations_init x n = 
        (let ptr = BA_.get_pointer_bytes x 2 in
        BA_.init_struct_list ~data_words:1 ~pointer_words:1 n ptr)
      let annotations_set_list x v =
        let builder = annotations_init x (List.length v) in
        let () = List.iteri (fun i a -> Capnp.Array.set builder i a) v in
        builder
      let annotations_set_array x v =
        let builder = annotations_init x (Array.length v) in
        let () = Array.iteri (fun i a -> Capnp.Array.set builder i a) v in
        builder
      let of_message x = BA_.get_root_struct ~data_words:5 ~pointer_words:5 x
      let to_message x = x.BA_.NC.StructStorage.data.MessageWrapper.Slice.msg
      let to_reader x = Some (RA_.StructStorage.readonly x)
      let init_root ?message_size () =
        BA_.alloc_root_struct ?message_size ~data_words:5 ~pointer_words:5 ()
    end
    module CodeGeneratorRequest = struct
      type t = builder_t_CodeGeneratorRequest_13818529054586492878
      type reader_t = reader_t_CodeGeneratorRequest_13818529054586492878
      module RequestedFile = struct
        type t = builder_t_RequestedFile_14981803260258615394
        type reader_t = reader_t_RequestedFile_14981803260258615394
        module Import = struct
          type t = builder_t_Import_12560611460656617445
          type reader_t = reader_t_Import_12560611460656617445
          let id_get x =
            let data = BA_.get_data_region x in
            BA_.get_uint64 ~default:Uint64.zero ~byte_ofs:0 data
          let id_get_int_exn x =
            Capnp.Runtime.Util.int_of_uint64_exn (id_get x)
          let id_set x v =
            let data = BA_.get_data_region x in
            BA_.set_uint64 ~default:Uint64.zero ~byte_ofs:0 v data
          let id_set_int_exn x v = id_set x (Capnp.Runtime.Util.uint64_of_int_exn v)
          let has_name x =
            let ptr = BA_.get_pointer_bytes x 0 in
            BA_.has_field ptr
          let name_get x =
            let ptr = BA_.get_pointer_bytes x 0 in
            BA_.get_text ~default:"" ptr
          let name_set x v =
            let ptr = BA_.get_pointer_bytes x 0 in
            BA_.set_text v ptr
          let of_message x = BA_.get_root_struct ~data_words:1 ~pointer_words:1 x
          let to_message x = x.BA_.NC.StructStorage.data.MessageWrapper.Slice.msg
          let to_reader x = Some (RA_.StructStorage.readonly x)
          let init_root ?message_size () =
            BA_.alloc_root_struct ?message_size ~data_words:1 ~pointer_words:1 ()
        end
        let id_get x =
          let data = BA_.get_data_region x in
          BA_.get_uint64 ~default:Uint64.zero ~byte_ofs:0 data
        let id_get_int_exn x =
          Capnp.Runtime.Util.int_of_uint64_exn (id_get x)
        let id_set x v =
          let data = BA_.get_data_region x in
          BA_.set_uint64 ~default:Uint64.zero ~byte_ofs:0 v data
        let id_set_int_exn x v = id_set x (Capnp.Runtime.Util.uint64_of_int_exn v)
        let has_filename x =
          let ptr = BA_.get_pointer_bytes x 0 in
          BA_.has_field ptr
        let filename_get x =
          let ptr = BA_.get_pointer_bytes x 0 in
          BA_.get_text ~default:"" ptr
        let filename_set x v =
          let ptr = BA_.get_pointer_bytes x 0 in
          BA_.set_text v ptr
        let has_imports x = 
          (let ptr = BA_.get_pointer_bytes x 1 in
          BA_.has_field ptr)
        let imports_get x = 
          (let ptr = BA_.get_pointer_bytes x 1 in
          BA_.get_struct_list ~data_words:1 ~pointer_words:1 ptr)
        let imports_get_list x =
          Capnp.Array.to_list (imports_get x)
        let imports_get_array x =
          Capnp.Array.to_array (imports_get x)
        let imports_set x v = 
          (let ptr = BA_.get_pointer_bytes x 1 in
          BA_.set_struct_list ~data_words:1 ~pointer_words:1 (v) ptr)
        let imports_init x n = 
          (let ptr = BA_.get_pointer_bytes x 1 in
          BA_.init_struct_list ~data_words:1 ~pointer_words:1 n ptr)
        let imports_set_list x v =
          let builder = imports_init x (List.length v) in
          let () = List.iteri (fun i a -> Capnp.Array.set builder i a) v in
          builder
        let imports_set_array x v =
          let builder = imports_init x (Array.length v) in
          let () = Array.iteri (fun i a -> Capnp.Array.set builder i a) v in
          builder
        let of_message x = BA_.get_root_struct ~data_words:1 ~pointer_words:2 x
        let to_message x = x.BA_.NC.StructStorage.data.MessageWrapper.Slice.msg
        let to_reader x = Some (RA_.StructStorage.readonly x)
        let init_root ?message_size () =
          BA_.alloc_root_struct ?message_size ~data_words:1 ~pointer_words:2 ()
      end
      let has_nodes x = 
        (let ptr = BA_.get_pointer_bytes x 0 in
        BA_.has_field ptr)
      let nodes_get x = 
        (let ptr = BA_.get_pointer_bytes x 0 in
        BA_.get_struct_list ~data_words:5 ~pointer_words:5 ptr)
      let nodes_get_list x =
        Capnp.Array.to_list (nodes_get x)
      let nodes_get_array x =
        Capnp.Array.to_array (nodes_get x)
      let nodes_set x v = 
        (let ptr = BA_.get_pointer_bytes x 0 in
        BA_.set_struct_list ~data_words:5 ~pointer_words:5 (v) ptr)
      let nodes_init x n = 
        (let ptr = BA_.get_pointer_bytes x 0 in
        BA_.init_struct_list ~data_words:5 ~pointer_words:5 n ptr)
      let nodes_set_list x v =
        let builder = nodes_init x (List.length v) in
        let () = List.iteri (fun i a -> Capnp.Array.set builder i a) v in
        builder
      let nodes_set_array x v =
        let builder = nodes_init x (Array.length v) in
        let () = Array.iteri (fun i a -> Capnp.Array.set builder i a) v in
        builder
      let has_requested_files x = 
        (let ptr = BA_.get_pointer_bytes x 1 in
        BA_.has_field ptr)
      let requested_files_get x = 
        (let ptr = BA_.get_pointer_bytes x 1 in
        BA_.get_struct_list ~data_words:1 ~pointer_words:2 ptr)
      let requested_files_get_list x =
        Capnp.Array.to_list (requested_files_get x)
      let requested_files_get_array x =
        Capnp.Array.to_array (requested_files_get x)
      let requested_files_set x v = 
        (let ptr = BA_.get_pointer_bytes x 1 in
        BA_.set_struct_list ~data_words:1 ~pointer_words:2 (v) ptr)
      let requested_files_init x n = 
        (let ptr = BA_.get_pointer_bytes x 1 in
        BA_.init_struct_list ~data_words:1 ~pointer_words:2 n ptr)
      let requested_files_set_list x v =
        let builder = requested_files_init x (List.length v) in
        let () = List.iteri (fun i a -> Capnp.Array.set builder i a) v in
        builder
      let requested_files_set_array x v =
        let builder = requested_files_init x (Array.length v) in
        let () = Array.iteri (fun i a -> Capnp.Array.set builder i a) v in
        builder
      let of_message x = BA_.get_root_struct ~data_words:0 ~pointer_words:2 x
      let to_message x = x.BA_.NC.StructStorage.data.MessageWrapper.Slice.msg
      let to_reader x = Some (RA_.StructStorage.readonly x)
      let init_root ?message_size () =
        BA_.alloc_root_struct ?message_size ~data_words:0 ~pointer_words:2 ()
    end
  end
end

