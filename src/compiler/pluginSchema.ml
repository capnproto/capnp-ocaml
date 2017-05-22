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

module Make (MessageWrapper : Capnp.MessageSig.S) = struct
  module CamlBytes = Bytes
  module DefaultsMessage_ = Capnp.BytesMessage

  let _builder_defaults_message =
    let message_segments = [
      Bytes.unsafe_of_string "\
      ";
    ] in
    DefaultsMessage_.Message.readonly
      (DefaultsMessage_.Message.of_storage message_segments)

  let invalid_msg = Capnp.Message.invalid_msg

  module RA_ = struct
    open Capnp
    open Capnp.Runtime
    (******************************************************************************
     * capnp-ocaml
     *
     * Copyright (c) 2013-2014, Paul Pelzl
     * All rights reserved.
     *
     * Redistribution and use in source and binary forms, with or without
     * modification, are permitted provided that the following conditions are met:
     *
     *  1. Redistributions of source code must retain the above copyright notice,
     *     this list of conditions and the following disclaimer.
     *
     *  2. Redistributions in binary form must reproduce the above copyright
     *     notice, this list of conditions and the following disclaimer in the
     *     documentation and/or other materials provided with the distribution.
     *
     * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
     * AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
     * IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
     * ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE
     * LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
     * CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
     * SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
     * INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN
     * CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
     * ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
     * POSSIBILITY OF SUCH DAMAGE.
     ******************************************************************************)

    (* Runtime support for Reader interfaces.  None of the functions provided
       here will modify the underlying message; derefencing null pointers and
       reading from truncated structs both lead to default data being returned. *)


    open Core_kernel.Std

    let sizeof_uint64 = 8

    module RC = struct
      (******************************************************************************
       * capnp-ocaml
       *
       * Copyright (c) 2013-2014, Paul Pelzl
       * All rights reserved.
       *
       * Redistribution and use in source and binary forms, with or without
       * modification, are permitted provided that the following conditions are met:
       *
       *  1. Redistributions of source code must retain the above copyright notice,
       *     this list of conditions and the following disclaimer.
       *
       *  2. Redistributions in binary form must reproduce the above copyright
       *     notice, this list of conditions and the following disclaimer in the
       *     documentation and/or other materials provided with the distribution.
       *
       * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
       * AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
       * IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
       * ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE
       * LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
       * CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
       * SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
       * INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN
       * CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
       * ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
       * POSSIBILITY OF SUCH DAMAGE.
       ******************************************************************************)
      
      (* Runtime support which is common to both Reader and Builder interfaces. *)
      
      open Core_kernel.Std
      
      
      let sizeof_uint32 = 4
      let sizeof_uint64 = 8
      
      let invalid_msg      = Message.invalid_msg
      let out_of_int_range = Message.out_of_int_range
      type ro = Message.ro
      type rw = Message.rw
      
      include MessageWrapper
      
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
      
      
      (** Get the range of bytes associated with a pointer stored in a struct. *)
      let ss_get_pointer
          (struct_storage : 'cap StructStorage.t)
          (word : int)           (* Struct-relative pointer index *)
        : 'cap Slice.t option =  (* Returns None if storage is too small for this word *)
        let pointers = struct_storage.StructStorage.pointers in
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
      
      
      let decode_pointer64 (pointer64 : int64) : Pointer.t =
        if Util.is_int64_zero pointer64 then
          Pointer.Null
        else
          let pointer_int = Caml.Int64.to_int pointer64 in
          let tag = pointer_int land Pointer.Bitfield.tag_mask in
          (* OCaml won't match an int against let-bound variables,
             only against constants. *)
          match tag with
          | 0x0 ->  (* Pointer.Bitfield.tag_val_struct *)
              Pointer.Struct (StructPointer.decode pointer64)
          | 0x1 ->  (* Pointer.Bitfield.tag_val_list *)
              Pointer.List (ListPointer.decode pointer64)
          | 0x2 ->  (* Pointer.Bitfield.tag_val_far *)
              Pointer.Far (FarPointer.decode pointer64)
          | 0x3 ->  (* Pointer.Bitfield.tag_val_other *)
              Pointer.Other (OtherPointer.decode pointer64)
          | _ ->
              assert false
      
      
      (* Given a range of eight bytes corresponding to a cap'n proto pointer,
         decode the information stored in the pointer. *)
      let decode_pointer (pointer_bytes : 'cap Slice.t) : Pointer.t =
        let pointer64 = Slice.get_int64 pointer_bytes 0 in
        decode_pointer64 pointer64
      
      
      let make_list_storage_aux ~message ~num_words ~num_elements ~storage_type
          ~segment_id ~segment_offset =
        let storage = {
          Slice.msg        = message;
          Slice.segment    = Message.get_segment message segment_id;
          Slice.segment_id = segment_id;
          Slice.start      = segment_offset;
          Slice.len        = num_words * sizeof_uint64;
        } in
        let () = bounds_check_slice_exn
          ~err:"list pointer describes invalid storage region" storage
        in {
          ListStorage.storage      = storage;
          ListStorage.storage_type = storage_type;
          ListStorage.num_elements = num_elements;
        }
      
      
      (* Given a list pointer descriptor, construct the corresponding list storage
         descriptor. *)
      let make_list_storage
          ~(message : 'cap Message.t)     (* Message of interest *)
          ~(segment_id : int)             (* Segment ID where list storage is found *)
          ~(segment_offset : int)         (* Segment offset where list storage is found *)
          ~(list_pointer : ListPointer.t)
        : 'cap ListStorage.t =
        let open ListPointer in
        match list_pointer.element_type with
        | Void ->
            make_list_storage_aux ~message ~num_words:0
              ~num_elements:list_pointer.num_elements
              ~storage_type:ListStorageType.Empty ~segment_id ~segment_offset
        | OneBitValue ->
            make_list_storage_aux ~message
              ~num_words:(Util.ceil_ratio list_pointer.num_elements 64)
              ~num_elements:list_pointer.num_elements ~storage_type:ListStorageType.Bit
              ~segment_id ~segment_offset
        | OneByteValue ->
            make_list_storage_aux ~message
              ~num_words:(Util.ceil_ratio list_pointer.num_elements 8)
              ~num_elements:list_pointer.num_elements
              ~storage_type:ListStorageType.Bytes1
              ~segment_id ~segment_offset
        | TwoByteValue ->
            make_list_storage_aux ~message
              ~num_words:(Util.ceil_ratio list_pointer.num_elements 4)
              ~num_elements:list_pointer.num_elements
              ~storage_type:ListStorageType.Bytes2
              ~segment_id ~segment_offset
        | FourByteValue ->
            make_list_storage_aux ~message
              ~num_words:(Util.ceil_ratio list_pointer.num_elements 2)
              ~num_elements:list_pointer.num_elements
              ~storage_type:ListStorageType.Bytes4
              ~segment_id ~segment_offset
        | EightByteValue ->
            make_list_storage_aux ~message ~num_words:list_pointer.num_elements
              ~num_elements:list_pointer.num_elements
              ~storage_type:ListStorageType.Bytes8
              ~segment_id ~segment_offset
        | EightBytePointer ->
            make_list_storage_aux ~message ~num_words:list_pointer.num_elements
              ~num_elements:list_pointer.num_elements
              ~storage_type:ListStorageType.Pointer
              ~segment_id ~segment_offset
        | Composite ->
            if segment_id < 0 || segment_id >= Message.num_segments message then
              invalid_msg "composite list pointer describes invalid tag region"
            else
              let segment = Message.get_segment message segment_id in
              if segment_offset + sizeof_uint64 > Segment.length segment then
                invalid_msg "composite list pointer describes invalid tag region"
              else
                let pointer64 = Segment.get_int64 segment segment_offset in
                let pointer_int = Caml.Int64.to_int pointer64 in
                let tag = pointer_int land Pointer.Bitfield.tag_mask in
                if tag = Pointer.Bitfield.tag_val_struct then
                  let struct_pointer = StructPointer.decode pointer64 in
                  let num_words = list_pointer.num_elements in
                  let num_elements = struct_pointer.StructPointer.offset in
                  let words_per_element = struct_pointer.StructPointer.data_words +
                      struct_pointer.StructPointer.pointer_words
                  in
                  if num_elements * words_per_element > num_words then
                    invalid_msg "composite list pointer describes invalid word count"
                  else
                    make_list_storage_aux ~message ~num_words ~num_elements
                      ~storage_type:(ListStorageType.Composite
                          (struct_pointer.StructPointer.data_words,
                           struct_pointer.StructPointer.pointer_words))
                      ~segment_id ~segment_offset
                else
                  invalid_msg "composite list pointer has malformed element type tag"
      
      
      (* Given a description of a cap'n proto far pointer, get the object which
         the pointer points to. *)
      let rec deref_far_pointer
          (far_pointer : FarPointer.t)
          (message : 'cap Message.t)
        : 'cap Object.t =
        let open FarPointer in
        match far_pointer.landing_pad with
        | NormalPointer ->
            let next_pointer_bytes = {
              Slice.msg        = message;
              Slice.segment    = Message.get_segment message far_pointer.segment_id;
              Slice.segment_id = far_pointer.segment_id;
              Slice.start      = far_pointer.offset * sizeof_uint64;
              Slice.len        = sizeof_uint64;
            } in
            let () = bounds_check_slice_exn
              ~err:"far pointer describes invalid landing pad" next_pointer_bytes
            in
            deref_pointer next_pointer_bytes
        | TaggedFarPointer ->
            let content_pointer_bytes = {
              Slice.msg        = message;
              Slice.segment    = Message.get_segment message far_pointer.segment_id;
              Slice.segment_id = far_pointer.segment_id;
              Slice.start      = far_pointer.offset * sizeof_uint64;
              Slice.len        = sizeof_uint64;
            } in
            let tag_bytes = {
              content_pointer_bytes with
              Slice.start = Slice.get_end content_pointer_bytes;
            } in
            match (decode_pointer content_pointer_bytes, decode_pointer tag_bytes) with
            | (Pointer.Far content_pointer, Pointer.List list_pointer) ->
                Object.List (make_list_storage
                  ~message
                  ~segment_id:content_pointer.FarPointer.segment_id
                  ~segment_offset:(content_pointer.FarPointer.offset * sizeof_uint64)
                  ~list_pointer)
            | (Pointer.Far content_pointer, Pointer.Struct struct_pointer) ->
                let segment_id = content_pointer.FarPointer.segment_id in
                let data = {
                  Slice.msg = message;
                  Slice.segment = Message.get_segment message segment_id;
                  Slice.segment_id;
                  Slice.start = content_pointer.FarPointer.offset * sizeof_uint64;
                  Slice.len = struct_pointer.StructPointer.data_words * sizeof_uint64;
                } in
                let pointers = {
                  data with
                  Slice.start = Slice.get_end data;
                  Slice.len =
                    struct_pointer.StructPointer.pointer_words * sizeof_uint64;
                } in
                let () = bounds_check_slice_exn
                    ~err:"struct-tagged far pointer describes invalid data region"
                    data
                in
                let () = bounds_check_slice_exn
                    ~err:"struct-tagged far pointer describes invalid pointers region"
                    pointers
                in
                Object.Struct { StructStorage.data; StructStorage.pointers; }
            | _ ->
                invalid_msg "tagged far pointer points to invalid landing pad"
      
      
      (* Given a range of eight bytes which represent a pointer, get the object which
         the pointer points to. *)
      and deref_pointer (pointer_bytes : 'cap Slice.t) : 'cap Object.t =
        let pointer64 = Slice.get_int64 pointer_bytes 0 in
        if Util.is_int64_zero pointer64 then
          Object.None
        else
          let pointer64 = Slice.get_int64 pointer_bytes 0 in
          let tag_bits = Caml.Int64.to_int pointer64 in
          let tag = tag_bits land Pointer.Bitfield.tag_mask in
          (* OCaml won't match an int against let-bound variables,
             only against constants. *)
          match tag with
          | 0x0 ->  (* Pointer.Bitfield.tag_val_struct *)
              let struct_pointer = StructPointer.decode pointer64 in
              let open StructPointer in
              let data = {
                pointer_bytes with
                Slice.start =
                  (Slice.get_end pointer_bytes) + (struct_pointer.offset * sizeof_uint64);
                Slice.len = struct_pointer.data_words * sizeof_uint64;
              } in
              let pointers = {
                data with
                Slice.start = Slice.get_end data;
                Slice.len   = struct_pointer.pointer_words * sizeof_uint64;
              } in
              let () = bounds_check_slice_exn
                ~err:"struct pointer describes invalid data region" data
              in
              let () = bounds_check_slice_exn
                ~err:"struct pointer describes invalid pointers region" pointers
              in
              Object.Struct { StructStorage.data; StructStorage.pointers; }
          | 0x1 ->  (* Pointer.Bitfield.tag_val_list *)
              let list_pointer = ListPointer.decode pointer64 in
              Object.List (make_list_storage
                ~message:pointer_bytes.Slice.msg
                ~segment_id:pointer_bytes.Slice.segment_id
                ~segment_offset:((Slice.get_end pointer_bytes) +
                                   (list_pointer.ListPointer.offset * sizeof_uint64))
                ~list_pointer)
          | 0x2 ->  (* Pointer.Bitfield.tag_val_far *)
              let far_pointer = FarPointer.decode pointer64 in
              deref_far_pointer far_pointer pointer_bytes.Slice.msg
          | 0x3 ->  (* Pointer.Bitfield.tag_val_other *)
              let other_pointer = OtherPointer.decode pointer64 in
              let (OtherPointer.Capability index) = other_pointer in
              Object.Capability index
          | _ ->
              assert false
      
      
      module ListDecoders = struct
        type ('cap, 'a) struct_decoders_t = {
          bytes     : 'cap Slice.t -> 'a;
          pointer   : 'cap Slice.t -> 'a;
          composite : 'cap StructStorage.t -> 'a;
        }
      
        type ('cap, 'a) t =
          | Empty of (unit -> 'a)
          | Bit of (bool -> 'a)
          | Bytes1 of ('cap Slice.t -> 'a)
          | Bytes2 of ('cap Slice.t -> 'a)
          | Bytes4 of ('cap Slice.t -> 'a)
          | Bytes8 of ('cap Slice.t -> 'a)
          | Pointer of ('cap Slice.t -> 'a)
          | Struct of ('cap, 'a) struct_decoders_t
      end
      
      
      module ListCodecs = struct
        type 'a struct_codecs_t = {
          bytes     : (rw Slice.t -> 'a) * ('a -> rw Slice.t -> unit);
          pointer   : (rw Slice.t -> 'a) * ('a -> rw Slice.t -> unit);
          composite : (rw StructStorage.t -> 'a) * ('a -> rw StructStorage.t -> unit);
        }
      
        type 'a t =
          | Empty of (unit -> 'a) * ('a -> unit)
          | Bit of (bool -> 'a) * ('a -> bool)
          | Bytes1 of (rw Slice.t -> 'a) * ('a -> rw Slice.t -> unit)
          | Bytes2 of (rw Slice.t -> 'a) * ('a -> rw Slice.t -> unit)
          | Bytes4 of (rw Slice.t -> 'a) * ('a -> rw Slice.t -> unit)
          | Bytes8 of (rw Slice.t -> 'a) * ('a -> rw Slice.t -> unit)
          | Pointer of (rw Slice.t -> 'a) * ('a -> rw Slice.t -> unit)
          | Struct of 'a struct_codecs_t
      end
      
      let _dummy = ref true
      
      let make_array_readonly
          (list_storage : 'cap ListStorage.t)
          (decoders : ('cap, 'a) ListDecoders.t)
        : (ro, 'a, 'cap ListStorage.t) InnerArray.t =
        let make_element_slice ls i byte_count = {
          ls.ListStorage.storage with
          Slice.start = ls.ListStorage.storage.Slice.start + (i * byte_count);
          Slice.len = byte_count;
        } in
        let length = list_storage.ListStorage.num_elements in
        (* Note: the following is attempting to strike a balance between
         * (1) building InnerArray.get_unsafe closures that do as little work as
         *     possible and
         * (2) making the closure calling convention as efficient as possible.
         *
         * A naive implementation of this getter can result in quite slow code. *)
        match list_storage.ListStorage.storage_type with
        | ListStorageType.Empty ->
            begin match decoders with
            | ListDecoders.Empty decode ->
                let ro_get_unsafe_void ls i = decode () in {
                  InnerArray.length;
                  InnerArray.init = InnerArray.invalid_init;
                  InnerArray.get_unsafe = ro_get_unsafe_void;
                  InnerArray.set_unsafe = InnerArray.invalid_set_unsafe;
                  InnerArray.storage = Some list_storage;
                }
            | _ ->
                invalid_msg
                  "decoded List<Void> where a different list type was expected"
            end
        | ListStorageType.Bit ->
            begin match decoders with
            | ListDecoders.Bit decode ->
                let ro_get_unsafe_bool ls i =
                  let byte_ofs = i / 8 in
                  let bit_ofs  = i mod 8 in
                  let byte_val =
                    Slice.get_uint8 ls.ListStorage.storage byte_ofs
                  in
                  decode ((byte_val land (1 lsl bit_ofs)) <> 0)
                in {
                  InnerArray.length;
                  InnerArray.init = InnerArray.invalid_init;
                  InnerArray.get_unsafe = ro_get_unsafe_bool;
                  InnerArray.set_unsafe = InnerArray.invalid_set_unsafe;
                  InnerArray.storage = Some list_storage;
                }
            | _ ->
                invalid_msg
                  "decoded List<Bool> where a different list type was expected"
            end
        | ListStorageType.Bytes1 ->
            begin match decoders with
            | ListDecoders.Bytes1 decode
            | ListDecoders.Struct { ListDecoders.bytes = decode; _ } ->
                let ro_get_unsafe_bytes1 ls i = decode (make_element_slice ls i 1) in {
                  InnerArray.length;
                  InnerArray.init = InnerArray.invalid_init;
                  InnerArray.get_unsafe = ro_get_unsafe_bytes1;
                  InnerArray.set_unsafe = InnerArray.invalid_set_unsafe;
                  InnerArray.storage = Some list_storage;
                }
            | _ ->
                invalid_msg
                  "decoded List<1 byte> where a different list type was expected"
            end
        | ListStorageType.Bytes2 ->
            begin match decoders with
            | ListDecoders.Bytes2 decode
            | ListDecoders.Struct { ListDecoders.bytes = decode; _ } ->
                let ro_get_unsafe_bytes2 ls i = decode (make_element_slice ls i 2) in {
                  InnerArray.length;
                  InnerArray.init = InnerArray.invalid_init;
                  InnerArray.get_unsafe = ro_get_unsafe_bytes2;
                  InnerArray.set_unsafe = InnerArray.invalid_set_unsafe;
                  InnerArray.storage = Some list_storage;
                }
            | _ ->
                invalid_msg
                  "decoded List<2 byte> where a different list type was expected"
            end
        | ListStorageType.Bytes4 ->
            begin match decoders with
            | ListDecoders.Bytes4 decode
            | ListDecoders.Struct { ListDecoders.bytes = decode; _ } ->
                let ro_get_unsafe_bytes4 ls i = decode (make_element_slice ls i 4) in {
                  InnerArray.length;
                  InnerArray.init = InnerArray.invalid_init;
                  InnerArray.get_unsafe = ro_get_unsafe_bytes4;
                  InnerArray.set_unsafe = InnerArray.invalid_set_unsafe;
                  InnerArray.storage = Some list_storage;
                }
            | _ ->
                invalid_msg
                  "decoded List<4 byte> where a different list type was expected"
            end
        | ListStorageType.Bytes8 ->
            begin match decoders with
            | ListDecoders.Bytes8 decode
            | ListDecoders.Struct { ListDecoders.bytes = decode; _ } ->
                let ro_get_unsafe_bytes8 ls i = decode (make_element_slice ls i 8) in {
                  InnerArray.length;
                  InnerArray.init = InnerArray.invalid_init;
                  InnerArray.get_unsafe = ro_get_unsafe_bytes8;
                  InnerArray.set_unsafe = InnerArray.invalid_set_unsafe;
                  InnerArray.storage = Some list_storage;
                }
            | _ ->
                invalid_msg
                  "decoded List<8 byte> where a different list type was expected"
            end
        | ListStorageType.Pointer ->
            begin match decoders with
            | ListDecoders.Pointer decode
            | ListDecoders.Struct { ListDecoders.pointer = decode; _ } ->
                let ro_get_unsafe_pointer ls i = decode (make_element_slice ls i sizeof_uint64) in {
                  InnerArray.length;
                  InnerArray.init = InnerArray.invalid_init;
                  InnerArray.get_unsafe = ro_get_unsafe_pointer;
                  InnerArray.set_unsafe = InnerArray.invalid_set_unsafe;
                  InnerArray.storage = Some list_storage;
                }
            | _ ->
                invalid_msg
                  "decoded List<pointer> a different list type was expected"
            end
        | ListStorageType.Composite (data_words, pointer_words) ->
            let data_size     = data_words * sizeof_uint64 in
            let pointers_size = pointer_words * sizeof_uint64 in
            let make_storage ls i ~data_size ~pointers_size =
              let total_size = data_size + pointers_size in
              (* Skip over the composite tag word *)
              let content_offset =
                ls.ListStorage.storage.Slice.start + sizeof_uint64
              in
              let data = {
                ls.ListStorage.storage with
                Slice.start = content_offset + (i * total_size);
                Slice.len = data_size;
              } in
              let pointers = {
                data with
                Slice.start = Slice.get_end data;
                Slice.len   = pointers_size;
              } in
              { StructStorage.data; StructStorage.pointers; }
            in
            let make_bytes_handler ~size ~decode =
              if data_words = 0 then
                invalid_msg
                  "decoded List<composite> with empty data region where data was expected"
              else
                let ro_get_unsafe_composite_bytes ls i =
                  let struct_storage = make_storage ls i ~data_size ~pointers_size in
                  let slice = {
                    struct_storage.StructStorage.data with
                    Slice.len = size
                  } in
                  decode slice
                in {
                  InnerArray.length;
                  InnerArray.init = InnerArray.invalid_init;
                  InnerArray.get_unsafe = ro_get_unsafe_composite_bytes;
                  InnerArray.set_unsafe = InnerArray.invalid_set_unsafe;
                  InnerArray.storage = Some list_storage;
                }
            in
            begin match decoders with
            | ListDecoders.Empty decode ->
                let ro_get_unsafe_composite_void ls i = decode () in {
                  InnerArray.length;
                  InnerArray.init = InnerArray.invalid_init;
                  InnerArray.get_unsafe = ro_get_unsafe_composite_void;
                  InnerArray.set_unsafe = InnerArray.invalid_set_unsafe;
                  InnerArray.storage = Some list_storage;
                }
            | ListDecoders.Bit decode ->
                if data_words = 0 then
                  invalid_msg
                    "decoded List<composite> with empty data region where data was expected"
                else
                  let ro_get_unsafe_composite_bool ls i =
                    let struct_storage = make_storage ls i ~data_size ~pointers_size in
                    let first_byte = Slice.get_uint8 struct_storage.StructStorage.data 0 in
                    let is_set = (first_byte land 0x1) <> 0 in
                    decode is_set
                  in {
                    InnerArray.length;
                    InnerArray.init = InnerArray.invalid_init;
                    InnerArray.get_unsafe = ro_get_unsafe_composite_bool;
                    InnerArray.set_unsafe = InnerArray.invalid_set_unsafe;
                    InnerArray.storage = Some list_storage;
                  }
            | ListDecoders.Bytes1 decode ->
                make_bytes_handler ~size:1 ~decode
            | ListDecoders.Bytes2 decode ->
                make_bytes_handler ~size:2 ~decode
            | ListDecoders.Bytes4 decode ->
                make_bytes_handler ~size:4 ~decode
            | ListDecoders.Bytes8 decode ->
                make_bytes_handler ~size:8 ~decode
            | ListDecoders.Pointer decode ->
                if pointer_words = 0 then
                  invalid_msg
                    "decoded List<composite> with empty pointers region where \
                     pointers were expected"
                else
                  let ro_get_unsafe_composite_pointer ls i =
                    let struct_storage = make_storage ls i ~data_size ~pointers_size in
                    let slice = {
                      struct_storage.StructStorage.pointers with
                      Slice.len = sizeof_uint64
                    } in
                    decode slice
                  in {
                    InnerArray.length;
                    InnerArray.init = InnerArray.invalid_init;
                    InnerArray.get_unsafe = ro_get_unsafe_composite_pointer;
                    InnerArray.set_unsafe = InnerArray.invalid_set_unsafe;
                    InnerArray.storage = Some list_storage;
                  }
            | ListDecoders.Struct struct_decoders ->
                let ro_get_unsafe_composite_struct ls i =
                  let struct_storage = make_storage ls i ~data_size ~pointers_size in
                  struct_decoders.ListDecoders.composite struct_storage
                in {
                  InnerArray.length;
                  InnerArray.init = InnerArray.invalid_init;
                  InnerArray.get_unsafe = ro_get_unsafe_composite_struct;
                  InnerArray.set_unsafe = InnerArray.invalid_set_unsafe;
                  InnerArray.storage = Some list_storage;
                }
            end
      
      
      let make_array_readwrite
          ~(list_storage : rw ListStorage.t)
          ~(init : int -> rw ListStorage.t)
          ~(codecs : 'a ListCodecs.t)
        : (rw, 'a, rw ListStorage.t) InnerArray.t =
        let make_element_slice ls i byte_count = {
          ls.ListStorage.storage with
          Slice.start = ls.ListStorage.storage.Slice.start + (i * byte_count);
          Slice.len = byte_count;
        } in
        let length = list_storage.ListStorage.num_elements in
        (* Note: the following is attempting to strike a balance between
         * (1) building InnerArray.get_unsafe/set_unsafe closures that do as little
         *     work as possible and
         * (2) making the closure calling convention as efficient as possible.
         *
         * A naive implementation of these accessors can result in quite slow code. *)
        match list_storage.ListStorage.storage_type with
        | ListStorageType.Empty ->
            begin match codecs with
            | ListCodecs.Empty (decode, encode) ->
                let rw_get_unsafe_void ls i = decode () in
                let rw_set_unsafe_void ls i v = encode v in {
                  InnerArray.length;
                  InnerArray.init;
                  InnerArray.get_unsafe = rw_get_unsafe_void;
                  InnerArray.set_unsafe = rw_set_unsafe_void;
                  InnerArray.storage = Some list_storage;
                }
            | _ ->
                invalid_msg
                  "decoded List<Void> where a different list type was expected"
            end
        | ListStorageType.Bit ->
            begin match codecs with
            | ListCodecs.Bit (decode, encode) ->
                let rw_get_unsafe_bool ls i =
                  let byte_ofs = i / 8 in
                  let bit_ofs  = i mod 8 in
                  let byte_val =
                    Slice.get_uint8 ls.ListStorage.storage byte_ofs
                  in
                  decode ((byte_val land (1 lsl bit_ofs)) <> 0)
                in
                let rw_set_unsafe_bool ls i v =
                  let byte_ofs = i / 8 in
                  let bit_ofs  = i mod 8 in
                  let bitmask  = 1 lsl bit_ofs in
                  let old_byte_val =
                    Slice.get_uint8 ls.ListStorage.storage byte_ofs
                  in
                  let new_byte_val =
                    if encode v then
                      old_byte_val lor bitmask
                    else
                      old_byte_val land (lnot bitmask)
                  in
                  Slice.set_uint8 ls.ListStorage.storage byte_ofs new_byte_val
                in {
                  InnerArray.length;
                  InnerArray.init;
                  InnerArray.get_unsafe = rw_get_unsafe_bool;
                  InnerArray.set_unsafe = rw_set_unsafe_bool;
                  InnerArray.storage = Some list_storage;
                }
            | _ ->
                invalid_msg
                  "decoded List<Bool> where a different list type was expected"
            end
        | ListStorageType.Bytes1 ->
            begin match codecs with
            | ListCodecs.Bytes1 (decode, encode)
            | ListCodecs.Struct { ListCodecs.bytes = (decode, encode); _ } ->
                let rw_get_unsafe_bytes1 ls i = decode (make_element_slice ls i 1) in
                let rw_set_unsafe_bytes1 ls i v = encode v (make_element_slice ls i 1) in {
                  InnerArray.length;
                  InnerArray.init;
                  InnerArray.get_unsafe = rw_get_unsafe_bytes1;
                  InnerArray.set_unsafe = rw_set_unsafe_bytes1;
                  InnerArray.storage = Some list_storage;
                }
            | _ ->
                invalid_msg
                  "decoded List<1 byte> where a different list type was expected"
            end
        | ListStorageType.Bytes2 ->
            begin match codecs with
            | ListCodecs.Bytes2 (decode, encode)
            | ListCodecs.Struct { ListCodecs.bytes = (decode, encode); _ } ->
                let rw_get_unsafe_bytes2 ls i = decode (make_element_slice ls i 2) in
                let rw_set_unsafe_bytes2 ls i v = encode v (make_element_slice ls i 2) in {
                  InnerArray.length;
                  InnerArray.init;
                  InnerArray.get_unsafe = rw_get_unsafe_bytes2;
                  InnerArray.set_unsafe = rw_set_unsafe_bytes2;
                  InnerArray.storage = Some list_storage;
                }
            | _ ->
                invalid_msg
                  "decoded List<2 byte> where a different list type was expected"
            end
        | ListStorageType.Bytes4 ->
            begin match codecs with
            | ListCodecs.Bytes4 (decode, encode)
            | ListCodecs.Struct { ListCodecs.bytes = (decode, encode); _ } ->
                let rw_get_unsafe_bytes4 ls i = decode (make_element_slice ls i 4) in
                let rw_set_unsafe_bytes4 ls i v = encode v (make_element_slice ls i 4) in {
                  InnerArray.length;
                  InnerArray.init;
                  InnerArray.get_unsafe = rw_get_unsafe_bytes4;
                  InnerArray.set_unsafe = rw_set_unsafe_bytes4;
                  InnerArray.storage = Some list_storage;
                }
            | _ ->
                invalid_msg
                  "decoded List<4 byte> where a different list type was expected"
            end
        | ListStorageType.Bytes8 ->
            begin match codecs with
            | ListCodecs.Bytes8 (decode, encode)
            | ListCodecs.Struct { ListCodecs.bytes = (decode, encode); _ } ->
                let rw_get_unsafe_bytes8 ls i = decode (make_element_slice ls i 8) in
                let rw_set_unsafe_bytes8 ls i v = encode v (make_element_slice ls i 8) in {
                  InnerArray.length;
                  InnerArray.init;
                  InnerArray.get_unsafe = rw_get_unsafe_bytes8;
                  InnerArray.set_unsafe = rw_set_unsafe_bytes8;
                  InnerArray.storage = Some list_storage;
                }
            | _ ->
                invalid_msg
                  "decoded List<8 byte> where a different list type was expected"
            end
        | ListStorageType.Pointer ->
            begin match codecs with
            | ListCodecs.Pointer (decode, encode)
            | ListCodecs.Struct { ListCodecs.pointer = (decode, encode); _ } ->
                let rw_get_unsafe_ptr ls i =
                  decode (make_element_slice ls i sizeof_uint64)
                in
                let rw_set_unsafe_ptr ls i v =
                  encode v (make_element_slice ls i sizeof_uint64)
                in {
                  InnerArray.length;
                  InnerArray.init;
                  InnerArray.get_unsafe = rw_get_unsafe_ptr;
                  InnerArray.set_unsafe = rw_set_unsafe_ptr;
                  InnerArray.storage = Some list_storage;
                }
            | _ ->
                invalid_msg
                  "decoded List<pointer> where a different list type was expected"
            end
        | ListStorageType.Composite (data_words, pointer_words) ->
            let data_size     = data_words * sizeof_uint64 in
            let pointers_size = pointer_words * sizeof_uint64 in
            let make_storage ls i ~data_size ~pointers_size =
              let total_size    = data_size + pointers_size in
              (* Skip over the composite tag word *)
              let content_offset =
                ls.ListStorage.storage.Slice.start + sizeof_uint64
              in
              let data = {
                ls.ListStorage.storage with
                Slice.start = content_offset + (i * total_size);
                Slice.len = data_size;
              } in
              let pointers = {
                data with
                Slice.start = Slice.get_end data;
                Slice.len   = pointers_size;
              } in
              { StructStorage.data; StructStorage.pointers; }
            in
            let make_bytes_handlers ~size ~decode ~encode =
              if data_words = 0 then
                invalid_msg
                  "decoded List<composite> with empty data region where data was expected"
              else
                let rw_get_unsafe_composite_bytes ls i =
                  let struct_storage = make_storage ls i ~data_size ~pointers_size in
                  let slice = {
                    struct_storage.StructStorage.data with
                    Slice.len = size
                  } in
                  decode slice
                in
                let rw_set_unsafe_composite_bytes ls i v =
                  let struct_storage = make_storage ls i ~data_size ~pointers_size in
                  let slice = {
                    struct_storage.StructStorage.data with
                    Slice.len = size
                  } in
                  encode v slice
                in {
                  InnerArray.length;
                  InnerArray.init;
                  InnerArray.get_unsafe = rw_get_unsafe_composite_bytes;
                  InnerArray.set_unsafe = rw_set_unsafe_composite_bytes;
                  InnerArray.storage = Some list_storage;
                }
            in
            begin match codecs with
            | ListCodecs.Empty (decode, encode) ->
                let rw_get_unsafe_composite_void ls i = decode () in
                let rw_set_unsafe_composite_void ls i v = encode v in {
                  InnerArray.length;
                  InnerArray.init;
                  InnerArray.get_unsafe = rw_get_unsafe_composite_void;
                  InnerArray.set_unsafe = rw_set_unsafe_composite_void;
                  InnerArray.storage = Some list_storage;
                }
            | ListCodecs.Bit (decode, encode) ->
                if data_words = 0 then
                  invalid_msg
                    "decoded List<composite> with empty data region where data was expected"
                else
                  let rw_get_unsafe_composite_bool ls i =
                    let struct_storage = make_storage ls i ~data_size ~pointers_size in
                    let first_byte = Slice.get_uint8 struct_storage.StructStorage.data 0 in
                    let is_set = (first_byte land 0x1) <> 0 in
                    decode is_set
                  in
                  let rw_set_unsafe_composite_bool ls i v =
                    let struct_storage = make_storage ls i ~data_size ~pointers_size in
                    let first_byte =
                      Slice.get_uint8 struct_storage.StructStorage.data 0
                    in
                    let first_byte =
                      if encode v then first_byte lor 0x1 else first_byte land 0xfe
                    in
                    Slice.set_uint8 struct_storage.StructStorage.data 0 first_byte
                  in {
                    InnerArray.length;
                    InnerArray.init;
                    InnerArray.get_unsafe = rw_get_unsafe_composite_bool;
                    InnerArray.set_unsafe = rw_set_unsafe_composite_bool;
                    InnerArray.storage = Some list_storage;
                  }
            | ListCodecs.Bytes1 (decode, encode) ->
                make_bytes_handlers ~size:1 ~decode ~encode
            | ListCodecs.Bytes2 (decode, encode) ->
                make_bytes_handlers ~size:2 ~decode ~encode
            | ListCodecs.Bytes4 (decode, encode) ->
                make_bytes_handlers ~size:4 ~decode ~encode
            | ListCodecs.Bytes8 (decode, encode) ->
                make_bytes_handlers ~size:8 ~decode ~encode
            | ListCodecs.Pointer (decode, encode) ->
                if pointer_words = 0 then
                  invalid_msg
                    "decoded List<composite> with empty pointers region where \
                     pointers were expected"
                else
                  let rw_get_unsafe_composite_ptr ls i =
                    let struct_storage = make_storage ls i ~data_size ~pointers_size in
                    let slice = {
                      struct_storage.StructStorage.pointers with
                      Slice.len = sizeof_uint64
                    } in
                    decode slice
                  in
                  let rw_set_unsafe_composite_ptr ls i v =
                    let struct_storage = make_storage ls i ~data_size ~pointers_size in
                    let slice = {
                      struct_storage.StructStorage.pointers with
                      Slice.len = sizeof_uint64
                    } in
                    encode v slice
                  in {
                    InnerArray.length;
                    InnerArray.init;
                    InnerArray.get_unsafe = rw_get_unsafe_composite_ptr;
                    InnerArray.set_unsafe = rw_set_unsafe_composite_ptr;
                    InnerArray.storage = Some list_storage;
                  }
            | ListCodecs.Struct { ListCodecs.composite = (decode, encode); _ } ->
                let rw_get_unsafe_composite_struct ls i =
                  decode (make_storage ls i ~data_size ~pointers_size)
                in
                let rw_set_unsafe_composite_struct ls i v =
                  encode v (make_storage ls i ~data_size ~pointers_size)
                in {
                  InnerArray.length;
                  InnerArray.init;
                  InnerArray.get_unsafe = rw_get_unsafe_composite_struct;
                  InnerArray.set_unsafe = rw_set_unsafe_composite_struct;
                  InnerArray.storage = Some list_storage;
                }
            end
      
      
      (* Given list storage which is expected to contain UInt8 data, decode the data as
         an OCaml string. *)
      let string_of_uint8_list
          ~(null_terminated : bool)   (* true if the data is expected to end in 0 *)
          (list_storage : 'cap ListStorage.t)
        : string =
        let open ListStorage in
        match list_storage.storage_type with
        | ListStorageType.Bytes1 ->
            let result_byte_count =
              if null_terminated then
                let () =
                  if list_storage.num_elements < 1 then
                    invalid_msg "empty string list has no space for null terminator"
                in
                let terminator =
                  Slice.get_uint8 list_storage.storage (list_storage.num_elements - 1)
                in
                let () = if terminator <> 0 then
                  invalid_msg "string list is not null terminated"
                in
                list_storage.num_elements - 1
              else
                list_storage.num_elements
            in
            let buf = CamlBytes.create result_byte_count in
            Slice.blit_to_bytes
              ~src:list_storage.storage ~src_pos:0
              ~dst:buf ~dst_pos:0
              ~len:result_byte_count;
            CamlBytes.unsafe_to_string buf
        | _ ->
            invalid_msg "decoded non-UInt8 list where string data was expected"
      
      
      let struct_of_bytes_slice slice =
        let data = slice in
        let pointers = {
          slice with
          Slice.start = Slice.get_end data;
          Slice.len   = 0;
        } in
        { StructStorage.data; StructStorage.pointers }
      
      let struct_of_pointer_slice slice =
        let () = assert (slice.Slice.len = sizeof_uint64) in
        let data = {
          slice with
          Slice.len = 0
        } in
        let pointers = {
          slice with
          Slice.len = sizeof_uint64;
        } in
        { StructStorage.data; StructStorage.pointers }
      
      
      (* Given some list storage corresponding to a struct list, construct
         a function for mapping an element index to the associated
         struct storage. *)
      let make_struct_of_list_index list_storage =
        let storage      = list_storage.ListStorage.storage in
        let storage_type = list_storage.ListStorage.storage_type in
        match list_storage.ListStorage.storage_type with
        | ListStorageType.Empty ->
            let make_struct_of_list_index_void i =
              let slice = {
                storage with
                Slice.start = storage.Slice.start;
                Slice.len   = 0;
              } in
              struct_of_bytes_slice slice
            in
            make_struct_of_list_index_void
        | ListStorageType.Bytes1
        | ListStorageType.Bytes2
        | ListStorageType.Bytes4
        | ListStorageType.Bytes8 ->
            (* Short data-only struct *)
            let byte_count = ListStorageType.get_byte_count storage_type in
            let make_struct_of_list_index_bytes i =
              let slice = {
                storage with
                Slice.start = storage.Slice.start + (i * byte_count);
                Slice.len   = byte_count;
              } in
              struct_of_bytes_slice slice
            in
            make_struct_of_list_index_bytes
        | ListStorageType.Pointer ->
            (* Single-pointer struct *)
            let make_struct_of_list_index_pointer i =
              let slice = {
                storage with
                Slice.start = (storage.Slice.start) + (i * sizeof_uint64);
                Slice.len   = sizeof_uint64;
              } in
              struct_of_pointer_slice slice
            in
            make_struct_of_list_index_pointer
        | ListStorageType.Composite (data_words, pointer_words) ->
            let data_size     = data_words * sizeof_uint64 in
            let pointers_size = pointer_words * sizeof_uint64 in
            let element_size  = data_size + pointers_size in
            (* Skip over the composite tag word *)
            let content_offset = storage.Slice.start + sizeof_uint64 in
            let make_struct_of_list_index_composite i =
              let data = {
                storage with
                Slice.start = content_offset + (i * element_size);
                Slice.len   = data_size;
              } in
              let pointers = {
                storage with
                Slice.start = Slice.get_end data;
                Slice.len   = pointers_size;
              } in
              { StructStorage.data; StructStorage.pointers }
            in
            make_struct_of_list_index_composite
        | ListStorageType.Bit ->
            invalid_msg "decoded List<Bool> where List<composite> was expected"
      
      
    end
    include RC

    (* Given a pointer which is expected to be a list pointer, compute the
       corresponding list storage descriptor.  Returns None if the pointer is
       null. *)
    let deref_list_pointer (pointer_bytes : 'cap Slice.t)
      : 'cap ListStorage.t option =
      match deref_pointer pointer_bytes with
      | Object.None ->
          None
      | Object.List list_descr ->
          Some list_descr
      | Object.Struct _ ->
          invalid_msg "decoded struct pointer where list pointer was expected"
      | Object.Capability _ ->
          invalid_msg "decoded capability pointer where list pointer was expected"


    (* Given a pointer which is expected to be a struct pointer, compute the
       corresponding struct storage descriptor.  Returns None if the pointer is
       null. *)
    let deref_struct_pointer (pointer_bytes : 'cap Slice.t)
      : 'cap StructStorage.t option =
      match deref_pointer pointer_bytes with
      | Object.None ->
          None
      | Object.Struct struct_descr ->
          Some struct_descr
      | Object.List _ ->
          invalid_msg "decoded list pointer where struct pointer was expected"
      | Object.Capability _ ->
          invalid_msg "decoded capability pointer where struct pointer was expected"


    let void_list_decoders =
      ListDecoders.Empty (fun (x : unit) -> x)

    let bit_list_decoders =
      ListDecoders.Bit (fun (x : bool) -> x)

    let int8_list_decoders =
      ListDecoders.Bytes1 (fun slice -> Slice.get_int8 slice 0)

    let int16_list_decoders =
      ListDecoders.Bytes2 (fun slice -> Slice.get_int16 slice 0)

    let int32_list_decoders =
      ListDecoders.Bytes4 (fun slice -> Slice.get_int32 slice 0)

    let int64_list_decoders =
      ListDecoders.Bytes8 (fun slice -> Slice.get_int64 slice 0)

    let uint8_list_decoders =
      ListDecoders.Bytes1 (fun slice -> Slice.get_uint8 slice 0)

    let uint16_list_decoders =
      ListDecoders.Bytes2 (fun slice -> Slice.get_uint16 slice 0)

    let uint32_list_decoders =
      ListDecoders.Bytes4 (fun slice -> Slice.get_uint32 slice 0)

    let uint64_list_decoders =
      ListDecoders.Bytes8 (fun slice -> Slice.get_uint64 slice 0)

    let float32_list_decoders = ListDecoders.Bytes4
        (fun slice -> Int32.float_of_bits (Slice.get_int32 slice 0))

    let float64_list_decoders = ListDecoders.Bytes8
        (fun slice -> Int64.float_of_bits (Slice.get_int64 slice 0))

    let text_list_decoders = ListDecoders.Pointer (fun slice ->
        match deref_list_pointer slice with
        | Some list_storage ->
            string_of_uint8_list ~null_terminated:true list_storage
        | None ->
            "")

    let blob_list_decoders = ListDecoders.Pointer (fun slice ->
        match deref_list_pointer slice with
        | Some list_storage ->
            string_of_uint8_list ~null_terminated:false list_storage
        | None ->
            "")

    let struct_list_decoders =
      let struct_decoders =
        let bytes slice = Some {
            StructStorage.data = slice;
            StructStorage.pointers = {
              slice with
              Slice.start = Slice.get_end slice;
              Slice.len   = 0;
            };
          }
        in
        let pointer slice = Some {
            StructStorage.data = {
              slice with
              Slice.len = 0;
            };
            StructStorage.pointers = slice;
          }
        in
        let composite x = Some x in {
          ListDecoders.bytes;
          ListDecoders.pointer;
          ListDecoders.composite;
        }
      in
      ListDecoders.Struct struct_decoders


    (* Locate the storage region corresponding to the root struct of a message. *)
    let get_root_struct (m : 'cap Message.t) : 'cap StructStorage.t option =
      let first_segment = Message.get_segment m 0 in
      if Segment.length first_segment < sizeof_uint64 then
        None
      else
        let pointer_bytes = {
          Slice.msg        = m;
          Slice.segment    = first_segment;
          Slice.segment_id = 0;
          Slice.start      = 0;
          Slice.len        = sizeof_uint64
        } in
        deref_struct_pointer pointer_bytes


    (*******************************************************************************
     * METHODS FOR GETTING OBJECTS STORED BY VALUE
     *******************************************************************************)

    let get_bit
        ~(default : bool)
        (struct_storage_opt : 'cap StructStorage.t option)
        ~(byte_ofs : int)
        ~(bit_ofs : int)
      : bool =
      match struct_storage_opt with
      | Some struct_storage ->
          let data = struct_storage.StructStorage.data in
          if byte_ofs < data.Slice.len then
            let byte_val = Slice.get_uint8 data byte_ofs in
            let is_set = Util.get_bit byte_val bit_ofs in
            if default then
              not is_set
            else
              is_set
          else
            default
      | None ->
          default

    let get_int8
        ~(default : int)
        (struct_storage_opt : 'cap StructStorage.t option)
        (byte_ofs : int)
      : int =
      match struct_storage_opt with
      | Some struct_storage ->
          let data = struct_storage.StructStorage.data in
          if byte_ofs < data.Slice.len then
            let numeric = Slice.get_int8 data byte_ofs in
            numeric lxor default
          else
            default
      | None ->
          default

    let get_int16
        ~(default : int)
        (struct_storage_opt : 'cap StructStorage.t option)
        (byte_ofs : int)
      : int =
      match struct_storage_opt with
      | Some struct_storage ->
          let data = struct_storage.StructStorage.data in
          if byte_ofs + 1 < data.Slice.len then
            let numeric = Slice.get_int16 data byte_ofs in
            numeric lxor default
          else
            default
      | None ->
          default

    let get_int32
        ~(default : int32)
        (struct_storage_opt : 'cap StructStorage.t option)
        (byte_ofs : int)
      : int32 =
      match struct_storage_opt with
      | Some struct_storage ->
          let data = struct_storage.StructStorage.data in
          if byte_ofs + 3 < data.Slice.len then
            let numeric = Slice.get_int32 data byte_ofs in
            Int32.bit_xor numeric default
          else
            default
      | None ->
          default

    let get_int64
        ~(default : int64)
        (struct_storage_opt : 'cap StructStorage.t option)
        (byte_ofs : int)
      : int64 =
      match struct_storage_opt with
      | Some struct_storage ->
          let data = struct_storage.StructStorage.data in
          if byte_ofs + 7 < data.Slice.len then
            let numeric = Slice.get_int64 data byte_ofs in
            Int64.bit_xor numeric default
          else
            default
      | None ->
          default

    let get_uint8
        ~(default : int)
        (struct_storage_opt : 'cap StructStorage.t option)
        (byte_ofs : int)
      : int =
      match struct_storage_opt with
      | Some struct_storage ->
          let data = struct_storage.StructStorage.data in
          if byte_ofs < data.Slice.len then
            let numeric = Slice.get_uint8 data byte_ofs in
            numeric lxor default
          else
            default
      | None ->
          default

    let get_uint16
        ~(default : int)
        (struct_storage_opt : 'cap StructStorage.t option)
        (byte_ofs : int)
      : int =
      match struct_storage_opt with
      | Some struct_storage ->
          let data = struct_storage.StructStorage.data in
          if byte_ofs + 1 < data.Slice.len then
            let numeric = Slice.get_uint16 data byte_ofs in
            numeric lxor default
          else
            default
      | None ->
          default

    let get_uint32
        ~(default : Uint32.t)
        (struct_storage_opt : 'cap StructStorage.t option)
        (byte_ofs : int)
      : Uint32.t =
      match struct_storage_opt with
      | Some struct_storage ->
          let data = struct_storage.StructStorage.data in
          if byte_ofs + 3 < data.Slice.len then
            let numeric = Slice.get_uint32 data byte_ofs in
            Uint32.logxor numeric default
          else
            default
      | None ->
          default

    let get_uint64
        ~(default : Uint64.t)
        (struct_storage_opt : 'cap StructStorage.t option)
        (byte_ofs : int)
      : Uint64.t =
      match struct_storage_opt with
      | Some struct_storage ->
          let data = struct_storage.StructStorage.data in
          if byte_ofs + 7 < data.Slice.len then
            let numeric = Slice.get_uint64 data byte_ofs in
            Uint64.logxor numeric default
          else
            default
      | None ->
          default

    let get_float32
        ~(default_bits : int32)
        (struct_storage_opt : 'cap StructStorage.t option)
        (byte_ofs : int)
      : float =
      let numeric =
        match struct_storage_opt with
        | Some struct_storage ->
            let data = struct_storage.StructStorage.data in
            if byte_ofs + 3 < data.Slice.len then
              Slice.get_int32 data byte_ofs
            else
              Int32.zero
        | None ->
            Int32.zero
      in
      let bits = Int32.bit_xor numeric default_bits in
      Int32.float_of_bits bits

    let get_float64
        ~(default_bits : int64)
        (struct_storage_opt : 'cap StructStorage.t option)
        (byte_ofs : int)
      : float =
      let numeric =
        match struct_storage_opt with
        | Some struct_storage ->
            let data = struct_storage.StructStorage.data in
            if byte_ofs + 7 < data.Slice.len then
              Slice.get_int64 data byte_ofs
            else
              Int64.zero
        | None ->
            Int64.zero
      in
      let bits = Int64.bit_xor numeric default_bits in
      Int64.float_of_bits bits


    (*******************************************************************************
     * METHODS FOR GETTING OBJECTS STORED BY POINTER
     *******************************************************************************)

    let has_field
        (struct_storage_opt : 'cap StructStorage.t option)
        (pointer_word : int)
      : bool =
      match struct_storage_opt with
      | Some struct_storage ->
          let pointers = struct_storage.StructStorage.pointers in
          let start = pointer_word * sizeof_uint64 in
          let len   = sizeof_uint64 in
          if start + len <= pointers.Slice.len then
            let pointer64 = Slice.get_int64 pointers start in
            not (Util.is_int64_zero pointer64)
          else
            false
      | None ->
          false

    let get_text
        ~(default : string)
        (struct_storage_opt : 'cap StructStorage.t option)
        (pointer_word : int)
      : string =
      match struct_storage_opt with
      | Some struct_storage ->
          let pointers = struct_storage.StructStorage.pointers in
          let start = pointer_word * sizeof_uint64 in
          let len   = sizeof_uint64 in
          if start + len <= pointers.Slice.len then
            let pointer_bytes = {
              pointers with
              Slice.start = pointers.Slice.start + start;
              Slice.len   = len;
            } in
            match deref_list_pointer pointer_bytes with
            | Some list_storage ->
                string_of_uint8_list ~null_terminated:true list_storage
            | None ->
                default
          else
            default
      | None ->
          default

    let get_blob
        ~(default : string)
        (struct_storage_opt : 'cap StructStorage.t option)
        (pointer_word : int)
      : string =
      match struct_storage_opt with
      | Some struct_storage ->
          let pointers = struct_storage.StructStorage.pointers in
          let start = pointer_word * sizeof_uint64 in
          let len   = sizeof_uint64 in
          if start + len <= pointers.Slice.len then
            let pointer_bytes = {
              pointers with
              Slice.start = pointers.Slice.start + start;
              Slice.len   = len;
            } in
            match deref_list_pointer pointer_bytes with
            | Some list_storage ->
                string_of_uint8_list ~null_terminated:false list_storage
            | None ->
                default
          else
            default
      | None ->
          default

    let get_list
        ?(default : ro ListStorage.t option)
        (decoders : ('cap, 'a) ListDecoders.t)
        (struct_storage_opt : 'cap StructStorage.t option)
        (pointer_word : int)
      : (ro, 'a, 'cap ListStorage.t) InnerArray.t =
      let make_default default' decoders' =
        begin match default' with
        | Some default_storage ->
            make_array_readonly default_storage decoders'
        | None ->
            (* Empty array *)
            { InnerArray.length     = 0;
              InnerArray.storage    = None;
              InnerArray.init       = InnerArray.invalid_init;
              InnerArray.get_unsafe = InnerArray.invalid_get_unsafe;
              InnerArray.set_unsafe = InnerArray.invalid_set_unsafe; }
        end
      in
      match struct_storage_opt with
      | Some struct_storage ->
          let pointers = struct_storage.StructStorage.pointers in
          let start = pointer_word * sizeof_uint64 in
          let len   = sizeof_uint64 in
          if start + len <= pointers.Slice.len then
            (* Fast path. *)
            let pointer64 = Slice.get_int64 pointers start in
            let pointer_int = Caml.Int64.to_int pointer64 in
            let tag = pointer_int land Pointer.Bitfield.tag_mask in
            if tag = Pointer.Bitfield.tag_val_list then
              let list_pointer = ListPointer.decode pointer64 in
              let list_storage = make_list_storage
                ~message:pointers.Slice.msg
                ~segment_id:pointers.Slice.segment_id
                ~segment_offset:((pointers.Slice.start + start + len) +
                                   (list_pointer.ListPointer.offset * sizeof_uint64))
                ~list_pointer
              in
              make_array_readonly list_storage decoders
            else
              (* Slow path... most likely a far pointer.*)
              let pointer_bytes = {
                pointers with
                Slice.start = pointers.Slice.start + start;
                Slice.len   = len;
              } in
              match deref_list_pointer pointer_bytes with
              | Some list_storage ->
                  make_array_readonly list_storage decoders
              | None ->
                  make_default default decoders
          else
            make_default default decoders
      | None ->
          make_default default decoders

    let get_void_list
        ?(default : ro ListStorage.t option)
        (struct_storage_opt : 'cap StructStorage.t option)
        (pointer_word : int)
      : (ro, unit, 'cap ListStorage.t) InnerArray.t =
      get_list ?default void_list_decoders struct_storage_opt pointer_word

    let get_bit_list
        ?(default : ro ListStorage.t option)
        (struct_storage_opt : 'cap StructStorage.t option)
        (pointer_word : int)
      : (ro, bool, 'cap ListStorage.t) InnerArray.t =
      get_list ?default bit_list_decoders struct_storage_opt pointer_word

    let get_int8_list
        ?(default : ro ListStorage.t option)
        (struct_storage_opt : 'cap StructStorage.t option)
        (pointer_word : int)
      : (ro, int, 'cap ListStorage.t) InnerArray.t =
      get_list ?default int8_list_decoders struct_storage_opt pointer_word

    let get_int16_list
        ?(default : ro ListStorage.t option)
        (struct_storage_opt : 'cap StructStorage.t option)
        (pointer_word : int)
      : (ro, int, 'cap ListStorage.t) InnerArray.t =
      get_list ?default int16_list_decoders struct_storage_opt pointer_word

    let get_int32_list
        ?(default : ro ListStorage.t option)
        (struct_storage_opt : 'cap StructStorage.t option)
        (pointer_word : int)
      : (ro, int32, 'cap ListStorage.t) InnerArray.t =
      get_list ?default int32_list_decoders struct_storage_opt pointer_word

    let get_int64_list
        ?(default : ro ListStorage.t option)
        (struct_storage_opt : 'cap StructStorage.t option)
        (pointer_word : int)
      : (ro, int64, 'cap ListStorage.t) InnerArray.t =
      get_list ?default int64_list_decoders struct_storage_opt pointer_word

    let get_uint8_list
        ?(default : ro ListStorage.t option)
        (struct_storage_opt : 'cap StructStorage.t option)
        (pointer_word : int)
      : (ro, int, 'cap ListStorage.t) InnerArray.t =
      get_list ?default uint8_list_decoders struct_storage_opt pointer_word

    let get_uint16_list
        ?(default : ro ListStorage.t option)
        (struct_storage_opt : 'cap StructStorage.t option)
        (pointer_word : int)
      : (ro, int, 'cap ListStorage.t) InnerArray.t =
      get_list ?default uint16_list_decoders struct_storage_opt pointer_word

    let get_uint32_list
        ?(default : ro ListStorage.t option)
        (struct_storage_opt : 'cap StructStorage.t option)
        (pointer_word : int)
      : (ro, Uint32.t, 'cap ListStorage.t) InnerArray.t =
      get_list ?default uint32_list_decoders struct_storage_opt pointer_word

    let get_uint64_list
        ?(default : ro ListStorage.t option)
        (struct_storage_opt : 'cap StructStorage.t option)
        (pointer_word : int)
      : (ro, Uint64.t, 'cap ListStorage.t) InnerArray.t =
      get_list ?default uint64_list_decoders struct_storage_opt pointer_word

    let get_float32_list
        ?(default : ro ListStorage.t option)
        (struct_storage_opt : 'cap StructStorage.t option)
        (pointer_word : int)
      : (ro, float, 'cap ListStorage.t) InnerArray.t =
      get_list ?default float32_list_decoders struct_storage_opt pointer_word

    let get_float64_list
        ?(default : ro ListStorage.t option)
        (struct_storage_opt : 'cap StructStorage.t option)
        (pointer_word : int)
      : (ro, float, 'cap ListStorage.t) InnerArray.t =
      get_list ?default float64_list_decoders struct_storage_opt pointer_word

    let get_text_list
        ?(default : ro ListStorage.t option)
        (struct_storage_opt : 'cap StructStorage.t option)
        (pointer_word : int)
      : (ro, string, 'cap ListStorage.t) InnerArray.t =
      get_list ?default text_list_decoders struct_storage_opt pointer_word

    let get_blob_list
        ?(default : ro ListStorage.t option)
        (struct_storage_opt : 'cap StructStorage.t option)
        (pointer_word : int)
      : (ro, string, 'cap ListStorage.t) InnerArray.t =
      get_list ?default blob_list_decoders struct_storage_opt pointer_word

    let get_struct_list
        ?(default : ro ListStorage.t option)
        (struct_storage_opt : 'cap StructStorage.t option)
        (pointer_word : int)
      : (ro, 'cap StructStorage.t option, 'cap ListStorage.t) InnerArray.t =
      get_list ?default struct_list_decoders struct_storage_opt pointer_word

    let get_struct
        ?(default : ro StructStorage.t option)
        (struct_storage_opt : 'cap StructStorage.t option)
        (pointer_word : int)
      : 'cap StructStorage.t option =
      match struct_storage_opt with
      | Some struct_storage ->
          let pointers = struct_storage.StructStorage.pointers in
          let start = pointer_word * sizeof_uint64 in
          let len   = sizeof_uint64 in
          if start + len <= pointers.Slice.len then
            let pointer_bytes = {
              pointers with
              Slice.start = pointers.Slice.start + start;
              Slice.len   = len;
            } in
            match deref_struct_pointer pointer_bytes with
            | Some storage ->
                Some storage
            | None ->
                default
          else
            default
      | None ->
          default

    let get_pointer
        ?(default: ro Slice.t option)
        (struct_storage_opt : 'cap StructStorage.t option)
        (pointer_word : int)
      : 'cap Slice.t option =
      match struct_storage_opt with
      | Some struct_storage ->
          let pointers = struct_storage.StructStorage.pointers in
          let start = pointer_word * sizeof_uint64 in
          let len   = sizeof_uint64 in
          if start + len <= pointers.Slice.len then
            let pointer64 = Slice.get_int64 pointers start in
            if Util.is_int64_zero pointer64 then
              default
            else
              let pointer_bytes = {
                pointers with
                Slice.start = pointers.Slice.start + start;
                Slice.len   = len;
              } in
              Some pointer_bytes
          else
            default
      | None ->
          default

    let get_interface
        (struct_storage_opt : 'cap StructStorage.t option)
        (pointer_word : int)
      : Uint32.t option =
      match struct_storage_opt with
      | Some struct_storage ->
          let pointers = struct_storage.StructStorage.pointers in
          let start = pointer_word * sizeof_uint64 in
          let len   = sizeof_uint64 in
          if start + len <= pointers.Slice.len then
            let pointer_bytes = {
              pointers with
              Slice.start = pointers.Slice.start + start;
              Slice.len   = len;
            } in
            match decode_pointer pointer_bytes with
            | Pointer.Null ->
                None
            | Pointer.Other (OtherPointer.Capability index) ->
                Some index
            | _ ->
                invalid_msg "decoded non-capability pointer where capability was expected"
          else
            None
      | None ->
          None

  end
  module BA_ = struct
    open Capnp
    open Capnp.Runtime
    module NM = MessageWrapper
    (******************************************************************************
     * capnp-ocaml
     *
     * Copyright (c) 2013-2014, Paul Pelzl
     * All rights reserved.
     *
     * Redistribution and use in source and binary forms, with or without
     * modification, are permitted provided that the following conditions are met:
     *
     *  1. Redistributions of source code must retain the above copyright notice,
     *     this list of conditions and the following disclaimer.
     *
     *  2. Redistributions in binary form must reproduce the above copyright
     *     notice, this list of conditions and the following disclaimer in the
     *     documentation and/or other materials provided with the distribution.
     *
     * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
     * AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
     * IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
     * ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE
     * LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
     * CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
     * SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
     * INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN
     * CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
     * ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
     * POSSIBILITY OF SUCH DAMAGE.
     ******************************************************************************)

    (* Runtime support for Builder interfaces.  In many ways this parallels the
       Reader support, to the point of using the same function names; however,
       the underlying message must be tagged as read/write, and many functions in
       this module may allocate message space (for example, dereferencing a struct
       pointer will cause struct storage to be immediately allocated if that pointer
       was null). *)

    open Core_kernel.Std

    type ro = Message.ro
    type rw = Message.rw
    let invalid_msg = Message.invalid_msg

    let sizeof_uint64 = 8

    (* Functor parameter: NM == "native message" *)

    (* DM == "defaults message", meaning "the type of messages that store default values" *)
    module DM = Message.BytesMessage

    module NC = struct
      module MessageWrapper = NM
      (******************************************************************************
       * capnp-ocaml
       *
       * Copyright (c) 2013-2014, Paul Pelzl
       * All rights reserved.
       *
       * Redistribution and use in source and binary forms, with or without
       * modification, are permitted provided that the following conditions are met:
       *
       *  1. Redistributions of source code must retain the above copyright notice,
       *     this list of conditions and the following disclaimer.
       *
       *  2. Redistributions in binary form must reproduce the above copyright
       *     notice, this list of conditions and the following disclaimer in the
       *     documentation and/or other materials provided with the distribution.
       *
       * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
       * AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
       * IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
       * ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE
       * LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
       * CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
       * SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
       * INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN
       * CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
       * ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
       * POSSIBILITY OF SUCH DAMAGE.
       ******************************************************************************)
      
      (* Runtime support which is common to both Reader and Builder interfaces. *)
      
      open Core_kernel.Std
      
      
      let sizeof_uint32 = 4
      let sizeof_uint64 = 8
      
      let invalid_msg      = Message.invalid_msg
      let out_of_int_range = Message.out_of_int_range
      type ro = Message.ro
      type rw = Message.rw
      
      include MessageWrapper
      
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
      
      
      (** Get the range of bytes associated with a pointer stored in a struct. *)
      let ss_get_pointer
          (struct_storage : 'cap StructStorage.t)
          (word : int)           (* Struct-relative pointer index *)
        : 'cap Slice.t option =  (* Returns None if storage is too small for this word *)
        let pointers = struct_storage.StructStorage.pointers in
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
      
      
      let decode_pointer64 (pointer64 : int64) : Pointer.t =
        if Util.is_int64_zero pointer64 then
          Pointer.Null
        else
          let pointer_int = Caml.Int64.to_int pointer64 in
          let tag = pointer_int land Pointer.Bitfield.tag_mask in
          (* OCaml won't match an int against let-bound variables,
             only against constants. *)
          match tag with
          | 0x0 ->  (* Pointer.Bitfield.tag_val_struct *)
              Pointer.Struct (StructPointer.decode pointer64)
          | 0x1 ->  (* Pointer.Bitfield.tag_val_list *)
              Pointer.List (ListPointer.decode pointer64)
          | 0x2 ->  (* Pointer.Bitfield.tag_val_far *)
              Pointer.Far (FarPointer.decode pointer64)
          | 0x3 ->  (* Pointer.Bitfield.tag_val_other *)
              Pointer.Other (OtherPointer.decode pointer64)
          | _ ->
              assert false
      
      
      (* Given a range of eight bytes corresponding to a cap'n proto pointer,
         decode the information stored in the pointer. *)
      let decode_pointer (pointer_bytes : 'cap Slice.t) : Pointer.t =
        let pointer64 = Slice.get_int64 pointer_bytes 0 in
        decode_pointer64 pointer64
      
      
      let make_list_storage_aux ~message ~num_words ~num_elements ~storage_type
          ~segment_id ~segment_offset =
        let storage = {
          Slice.msg        = message;
          Slice.segment    = Message.get_segment message segment_id;
          Slice.segment_id = segment_id;
          Slice.start      = segment_offset;
          Slice.len        = num_words * sizeof_uint64;
        } in
        let () = bounds_check_slice_exn
          ~err:"list pointer describes invalid storage region" storage
        in {
          ListStorage.storage      = storage;
          ListStorage.storage_type = storage_type;
          ListStorage.num_elements = num_elements;
        }
      
      
      (* Given a list pointer descriptor, construct the corresponding list storage
         descriptor. *)
      let make_list_storage
          ~(message : 'cap Message.t)     (* Message of interest *)
          ~(segment_id : int)             (* Segment ID where list storage is found *)
          ~(segment_offset : int)         (* Segment offset where list storage is found *)
          ~(list_pointer : ListPointer.t)
        : 'cap ListStorage.t =
        let open ListPointer in
        match list_pointer.element_type with
        | Void ->
            make_list_storage_aux ~message ~num_words:0
              ~num_elements:list_pointer.num_elements
              ~storage_type:ListStorageType.Empty ~segment_id ~segment_offset
        | OneBitValue ->
            make_list_storage_aux ~message
              ~num_words:(Util.ceil_ratio list_pointer.num_elements 64)
              ~num_elements:list_pointer.num_elements ~storage_type:ListStorageType.Bit
              ~segment_id ~segment_offset
        | OneByteValue ->
            make_list_storage_aux ~message
              ~num_words:(Util.ceil_ratio list_pointer.num_elements 8)
              ~num_elements:list_pointer.num_elements
              ~storage_type:ListStorageType.Bytes1
              ~segment_id ~segment_offset
        | TwoByteValue ->
            make_list_storage_aux ~message
              ~num_words:(Util.ceil_ratio list_pointer.num_elements 4)
              ~num_elements:list_pointer.num_elements
              ~storage_type:ListStorageType.Bytes2
              ~segment_id ~segment_offset
        | FourByteValue ->
            make_list_storage_aux ~message
              ~num_words:(Util.ceil_ratio list_pointer.num_elements 2)
              ~num_elements:list_pointer.num_elements
              ~storage_type:ListStorageType.Bytes4
              ~segment_id ~segment_offset
        | EightByteValue ->
            make_list_storage_aux ~message ~num_words:list_pointer.num_elements
              ~num_elements:list_pointer.num_elements
              ~storage_type:ListStorageType.Bytes8
              ~segment_id ~segment_offset
        | EightBytePointer ->
            make_list_storage_aux ~message ~num_words:list_pointer.num_elements
              ~num_elements:list_pointer.num_elements
              ~storage_type:ListStorageType.Pointer
              ~segment_id ~segment_offset
        | Composite ->
            if segment_id < 0 || segment_id >= Message.num_segments message then
              invalid_msg "composite list pointer describes invalid tag region"
            else
              let segment = Message.get_segment message segment_id in
              if segment_offset + sizeof_uint64 > Segment.length segment then
                invalid_msg "composite list pointer describes invalid tag region"
              else
                let pointer64 = Segment.get_int64 segment segment_offset in
                let pointer_int = Caml.Int64.to_int pointer64 in
                let tag = pointer_int land Pointer.Bitfield.tag_mask in
                if tag = Pointer.Bitfield.tag_val_struct then
                  let struct_pointer = StructPointer.decode pointer64 in
                  let num_words = list_pointer.num_elements in
                  let num_elements = struct_pointer.StructPointer.offset in
                  let words_per_element = struct_pointer.StructPointer.data_words +
                      struct_pointer.StructPointer.pointer_words
                  in
                  if num_elements * words_per_element > num_words then
                    invalid_msg "composite list pointer describes invalid word count"
                  else
                    make_list_storage_aux ~message ~num_words ~num_elements
                      ~storage_type:(ListStorageType.Composite
                          (struct_pointer.StructPointer.data_words,
                           struct_pointer.StructPointer.pointer_words))
                      ~segment_id ~segment_offset
                else
                  invalid_msg "composite list pointer has malformed element type tag"
      
      
      (* Given a description of a cap'n proto far pointer, get the object which
         the pointer points to. *)
      let rec deref_far_pointer
          (far_pointer : FarPointer.t)
          (message : 'cap Message.t)
        : 'cap Object.t =
        let open FarPointer in
        match far_pointer.landing_pad with
        | NormalPointer ->
            let next_pointer_bytes = {
              Slice.msg        = message;
              Slice.segment    = Message.get_segment message far_pointer.segment_id;
              Slice.segment_id = far_pointer.segment_id;
              Slice.start      = far_pointer.offset * sizeof_uint64;
              Slice.len        = sizeof_uint64;
            } in
            let () = bounds_check_slice_exn
              ~err:"far pointer describes invalid landing pad" next_pointer_bytes
            in
            deref_pointer next_pointer_bytes
        | TaggedFarPointer ->
            let content_pointer_bytes = {
              Slice.msg        = message;
              Slice.segment    = Message.get_segment message far_pointer.segment_id;
              Slice.segment_id = far_pointer.segment_id;
              Slice.start      = far_pointer.offset * sizeof_uint64;
              Slice.len        = sizeof_uint64;
            } in
            let tag_bytes = {
              content_pointer_bytes with
              Slice.start = Slice.get_end content_pointer_bytes;
            } in
            match (decode_pointer content_pointer_bytes, decode_pointer tag_bytes) with
            | (Pointer.Far content_pointer, Pointer.List list_pointer) ->
                Object.List (make_list_storage
                  ~message
                  ~segment_id:content_pointer.FarPointer.segment_id
                  ~segment_offset:(content_pointer.FarPointer.offset * sizeof_uint64)
                  ~list_pointer)
            | (Pointer.Far content_pointer, Pointer.Struct struct_pointer) ->
                let segment_id = content_pointer.FarPointer.segment_id in
                let data = {
                  Slice.msg = message;
                  Slice.segment = Message.get_segment message segment_id;
                  Slice.segment_id;
                  Slice.start = content_pointer.FarPointer.offset * sizeof_uint64;
                  Slice.len = struct_pointer.StructPointer.data_words * sizeof_uint64;
                } in
                let pointers = {
                  data with
                  Slice.start = Slice.get_end data;
                  Slice.len =
                    struct_pointer.StructPointer.pointer_words * sizeof_uint64;
                } in
                let () = bounds_check_slice_exn
                    ~err:"struct-tagged far pointer describes invalid data region"
                    data
                in
                let () = bounds_check_slice_exn
                    ~err:"struct-tagged far pointer describes invalid pointers region"
                    pointers
                in
                Object.Struct { StructStorage.data; StructStorage.pointers; }
            | _ ->
                invalid_msg "tagged far pointer points to invalid landing pad"
      
      
      (* Given a range of eight bytes which represent a pointer, get the object which
         the pointer points to. *)
      and deref_pointer (pointer_bytes : 'cap Slice.t) : 'cap Object.t =
        let pointer64 = Slice.get_int64 pointer_bytes 0 in
        if Util.is_int64_zero pointer64 then
          Object.None
        else
          let pointer64 = Slice.get_int64 pointer_bytes 0 in
          let tag_bits = Caml.Int64.to_int pointer64 in
          let tag = tag_bits land Pointer.Bitfield.tag_mask in
          (* OCaml won't match an int against let-bound variables,
             only against constants. *)
          match tag with
          | 0x0 ->  (* Pointer.Bitfield.tag_val_struct *)
              let struct_pointer = StructPointer.decode pointer64 in
              let open StructPointer in
              let data = {
                pointer_bytes with
                Slice.start =
                  (Slice.get_end pointer_bytes) + (struct_pointer.offset * sizeof_uint64);
                Slice.len = struct_pointer.data_words * sizeof_uint64;
              } in
              let pointers = {
                data with
                Slice.start = Slice.get_end data;
                Slice.len   = struct_pointer.pointer_words * sizeof_uint64;
              } in
              let () = bounds_check_slice_exn
                ~err:"struct pointer describes invalid data region" data
              in
              let () = bounds_check_slice_exn
                ~err:"struct pointer describes invalid pointers region" pointers
              in
              Object.Struct { StructStorage.data; StructStorage.pointers; }
          | 0x1 ->  (* Pointer.Bitfield.tag_val_list *)
              let list_pointer = ListPointer.decode pointer64 in
              Object.List (make_list_storage
                ~message:pointer_bytes.Slice.msg
                ~segment_id:pointer_bytes.Slice.segment_id
                ~segment_offset:((Slice.get_end pointer_bytes) +
                                   (list_pointer.ListPointer.offset * sizeof_uint64))
                ~list_pointer)
          | 0x2 ->  (* Pointer.Bitfield.tag_val_far *)
              let far_pointer = FarPointer.decode pointer64 in
              deref_far_pointer far_pointer pointer_bytes.Slice.msg
          | 0x3 ->  (* Pointer.Bitfield.tag_val_other *)
              let other_pointer = OtherPointer.decode pointer64 in
              let (OtherPointer.Capability index) = other_pointer in
              Object.Capability index
          | _ ->
              assert false
      
      
      module ListDecoders = struct
        type ('cap, 'a) struct_decoders_t = {
          bytes     : 'cap Slice.t -> 'a;
          pointer   : 'cap Slice.t -> 'a;
          composite : 'cap StructStorage.t -> 'a;
        }
      
        type ('cap, 'a) t =
          | Empty of (unit -> 'a)
          | Bit of (bool -> 'a)
          | Bytes1 of ('cap Slice.t -> 'a)
          | Bytes2 of ('cap Slice.t -> 'a)
          | Bytes4 of ('cap Slice.t -> 'a)
          | Bytes8 of ('cap Slice.t -> 'a)
          | Pointer of ('cap Slice.t -> 'a)
          | Struct of ('cap, 'a) struct_decoders_t
      end
      
      
      module ListCodecs = struct
        type 'a struct_codecs_t = {
          bytes     : (rw Slice.t -> 'a) * ('a -> rw Slice.t -> unit);
          pointer   : (rw Slice.t -> 'a) * ('a -> rw Slice.t -> unit);
          composite : (rw StructStorage.t -> 'a) * ('a -> rw StructStorage.t -> unit);
        }
      
        type 'a t =
          | Empty of (unit -> 'a) * ('a -> unit)
          | Bit of (bool -> 'a) * ('a -> bool)
          | Bytes1 of (rw Slice.t -> 'a) * ('a -> rw Slice.t -> unit)
          | Bytes2 of (rw Slice.t -> 'a) * ('a -> rw Slice.t -> unit)
          | Bytes4 of (rw Slice.t -> 'a) * ('a -> rw Slice.t -> unit)
          | Bytes8 of (rw Slice.t -> 'a) * ('a -> rw Slice.t -> unit)
          | Pointer of (rw Slice.t -> 'a) * ('a -> rw Slice.t -> unit)
          | Struct of 'a struct_codecs_t
      end
      
      let _dummy = ref true
      
      let make_array_readonly
          (list_storage : 'cap ListStorage.t)
          (decoders : ('cap, 'a) ListDecoders.t)
        : (ro, 'a, 'cap ListStorage.t) InnerArray.t =
        let make_element_slice ls i byte_count = {
          ls.ListStorage.storage with
          Slice.start = ls.ListStorage.storage.Slice.start + (i * byte_count);
          Slice.len = byte_count;
        } in
        let length = list_storage.ListStorage.num_elements in
        (* Note: the following is attempting to strike a balance between
         * (1) building InnerArray.get_unsafe closures that do as little work as
         *     possible and
         * (2) making the closure calling convention as efficient as possible.
         *
         * A naive implementation of this getter can result in quite slow code. *)
        match list_storage.ListStorage.storage_type with
        | ListStorageType.Empty ->
            begin match decoders with
            | ListDecoders.Empty decode ->
                let ro_get_unsafe_void ls i = decode () in {
                  InnerArray.length;
                  InnerArray.init = InnerArray.invalid_init;
                  InnerArray.get_unsafe = ro_get_unsafe_void;
                  InnerArray.set_unsafe = InnerArray.invalid_set_unsafe;
                  InnerArray.storage = Some list_storage;
                }
            | _ ->
                invalid_msg
                  "decoded List<Void> where a different list type was expected"
            end
        | ListStorageType.Bit ->
            begin match decoders with
            | ListDecoders.Bit decode ->
                let ro_get_unsafe_bool ls i =
                  let byte_ofs = i / 8 in
                  let bit_ofs  = i mod 8 in
                  let byte_val =
                    Slice.get_uint8 ls.ListStorage.storage byte_ofs
                  in
                  decode ((byte_val land (1 lsl bit_ofs)) <> 0)
                in {
                  InnerArray.length;
                  InnerArray.init = InnerArray.invalid_init;
                  InnerArray.get_unsafe = ro_get_unsafe_bool;
                  InnerArray.set_unsafe = InnerArray.invalid_set_unsafe;
                  InnerArray.storage = Some list_storage;
                }
            | _ ->
                invalid_msg
                  "decoded List<Bool> where a different list type was expected"
            end
        | ListStorageType.Bytes1 ->
            begin match decoders with
            | ListDecoders.Bytes1 decode
            | ListDecoders.Struct { ListDecoders.bytes = decode; _ } ->
                let ro_get_unsafe_bytes1 ls i = decode (make_element_slice ls i 1) in {
                  InnerArray.length;
                  InnerArray.init = InnerArray.invalid_init;
                  InnerArray.get_unsafe = ro_get_unsafe_bytes1;
                  InnerArray.set_unsafe = InnerArray.invalid_set_unsafe;
                  InnerArray.storage = Some list_storage;
                }
            | _ ->
                invalid_msg
                  "decoded List<1 byte> where a different list type was expected"
            end
        | ListStorageType.Bytes2 ->
            begin match decoders with
            | ListDecoders.Bytes2 decode
            | ListDecoders.Struct { ListDecoders.bytes = decode; _ } ->
                let ro_get_unsafe_bytes2 ls i = decode (make_element_slice ls i 2) in {
                  InnerArray.length;
                  InnerArray.init = InnerArray.invalid_init;
                  InnerArray.get_unsafe = ro_get_unsafe_bytes2;
                  InnerArray.set_unsafe = InnerArray.invalid_set_unsafe;
                  InnerArray.storage = Some list_storage;
                }
            | _ ->
                invalid_msg
                  "decoded List<2 byte> where a different list type was expected"
            end
        | ListStorageType.Bytes4 ->
            begin match decoders with
            | ListDecoders.Bytes4 decode
            | ListDecoders.Struct { ListDecoders.bytes = decode; _ } ->
                let ro_get_unsafe_bytes4 ls i = decode (make_element_slice ls i 4) in {
                  InnerArray.length;
                  InnerArray.init = InnerArray.invalid_init;
                  InnerArray.get_unsafe = ro_get_unsafe_bytes4;
                  InnerArray.set_unsafe = InnerArray.invalid_set_unsafe;
                  InnerArray.storage = Some list_storage;
                }
            | _ ->
                invalid_msg
                  "decoded List<4 byte> where a different list type was expected"
            end
        | ListStorageType.Bytes8 ->
            begin match decoders with
            | ListDecoders.Bytes8 decode
            | ListDecoders.Struct { ListDecoders.bytes = decode; _ } ->
                let ro_get_unsafe_bytes8 ls i = decode (make_element_slice ls i 8) in {
                  InnerArray.length;
                  InnerArray.init = InnerArray.invalid_init;
                  InnerArray.get_unsafe = ro_get_unsafe_bytes8;
                  InnerArray.set_unsafe = InnerArray.invalid_set_unsafe;
                  InnerArray.storage = Some list_storage;
                }
            | _ ->
                invalid_msg
                  "decoded List<8 byte> where a different list type was expected"
            end
        | ListStorageType.Pointer ->
            begin match decoders with
            | ListDecoders.Pointer decode
            | ListDecoders.Struct { ListDecoders.pointer = decode; _ } ->
                let ro_get_unsafe_pointer ls i = decode (make_element_slice ls i sizeof_uint64) in {
                  InnerArray.length;
                  InnerArray.init = InnerArray.invalid_init;
                  InnerArray.get_unsafe = ro_get_unsafe_pointer;
                  InnerArray.set_unsafe = InnerArray.invalid_set_unsafe;
                  InnerArray.storage = Some list_storage;
                }
            | _ ->
                invalid_msg
                  "decoded List<pointer> a different list type was expected"
            end
        | ListStorageType.Composite (data_words, pointer_words) ->
            let data_size     = data_words * sizeof_uint64 in
            let pointers_size = pointer_words * sizeof_uint64 in
            let make_storage ls i ~data_size ~pointers_size =
              let total_size = data_size + pointers_size in
              (* Skip over the composite tag word *)
              let content_offset =
                ls.ListStorage.storage.Slice.start + sizeof_uint64
              in
              let data = {
                ls.ListStorage.storage with
                Slice.start = content_offset + (i * total_size);
                Slice.len = data_size;
              } in
              let pointers = {
                data with
                Slice.start = Slice.get_end data;
                Slice.len   = pointers_size;
              } in
              { StructStorage.data; StructStorage.pointers; }
            in
            let make_bytes_handler ~size ~decode =
              if data_words = 0 then
                invalid_msg
                  "decoded List<composite> with empty data region where data was expected"
              else
                let ro_get_unsafe_composite_bytes ls i =
                  let struct_storage = make_storage ls i ~data_size ~pointers_size in
                  let slice = {
                    struct_storage.StructStorage.data with
                    Slice.len = size
                  } in
                  decode slice
                in {
                  InnerArray.length;
                  InnerArray.init = InnerArray.invalid_init;
                  InnerArray.get_unsafe = ro_get_unsafe_composite_bytes;
                  InnerArray.set_unsafe = InnerArray.invalid_set_unsafe;
                  InnerArray.storage = Some list_storage;
                }
            in
            begin match decoders with
            | ListDecoders.Empty decode ->
                let ro_get_unsafe_composite_void ls i = decode () in {
                  InnerArray.length;
                  InnerArray.init = InnerArray.invalid_init;
                  InnerArray.get_unsafe = ro_get_unsafe_composite_void;
                  InnerArray.set_unsafe = InnerArray.invalid_set_unsafe;
                  InnerArray.storage = Some list_storage;
                }
            | ListDecoders.Bit decode ->
                if data_words = 0 then
                  invalid_msg
                    "decoded List<composite> with empty data region where data was expected"
                else
                  let ro_get_unsafe_composite_bool ls i =
                    let struct_storage = make_storage ls i ~data_size ~pointers_size in
                    let first_byte = Slice.get_uint8 struct_storage.StructStorage.data 0 in
                    let is_set = (first_byte land 0x1) <> 0 in
                    decode is_set
                  in {
                    InnerArray.length;
                    InnerArray.init = InnerArray.invalid_init;
                    InnerArray.get_unsafe = ro_get_unsafe_composite_bool;
                    InnerArray.set_unsafe = InnerArray.invalid_set_unsafe;
                    InnerArray.storage = Some list_storage;
                  }
            | ListDecoders.Bytes1 decode ->
                make_bytes_handler ~size:1 ~decode
            | ListDecoders.Bytes2 decode ->
                make_bytes_handler ~size:2 ~decode
            | ListDecoders.Bytes4 decode ->
                make_bytes_handler ~size:4 ~decode
            | ListDecoders.Bytes8 decode ->
                make_bytes_handler ~size:8 ~decode
            | ListDecoders.Pointer decode ->
                if pointer_words = 0 then
                  invalid_msg
                    "decoded List<composite> with empty pointers region where \
                     pointers were expected"
                else
                  let ro_get_unsafe_composite_pointer ls i =
                    let struct_storage = make_storage ls i ~data_size ~pointers_size in
                    let slice = {
                      struct_storage.StructStorage.pointers with
                      Slice.len = sizeof_uint64
                    } in
                    decode slice
                  in {
                    InnerArray.length;
                    InnerArray.init = InnerArray.invalid_init;
                    InnerArray.get_unsafe = ro_get_unsafe_composite_pointer;
                    InnerArray.set_unsafe = InnerArray.invalid_set_unsafe;
                    InnerArray.storage = Some list_storage;
                  }
            | ListDecoders.Struct struct_decoders ->
                let ro_get_unsafe_composite_struct ls i =
                  let struct_storage = make_storage ls i ~data_size ~pointers_size in
                  struct_decoders.ListDecoders.composite struct_storage
                in {
                  InnerArray.length;
                  InnerArray.init = InnerArray.invalid_init;
                  InnerArray.get_unsafe = ro_get_unsafe_composite_struct;
                  InnerArray.set_unsafe = InnerArray.invalid_set_unsafe;
                  InnerArray.storage = Some list_storage;
                }
            end
      
      
      let make_array_readwrite
          ~(list_storage : rw ListStorage.t)
          ~(init : int -> rw ListStorage.t)
          ~(codecs : 'a ListCodecs.t)
        : (rw, 'a, rw ListStorage.t) InnerArray.t =
        let make_element_slice ls i byte_count = {
          ls.ListStorage.storage with
          Slice.start = ls.ListStorage.storage.Slice.start + (i * byte_count);
          Slice.len = byte_count;
        } in
        let length = list_storage.ListStorage.num_elements in
        (* Note: the following is attempting to strike a balance between
         * (1) building InnerArray.get_unsafe/set_unsafe closures that do as little
         *     work as possible and
         * (2) making the closure calling convention as efficient as possible.
         *
         * A naive implementation of these accessors can result in quite slow code. *)
        match list_storage.ListStorage.storage_type with
        | ListStorageType.Empty ->
            begin match codecs with
            | ListCodecs.Empty (decode, encode) ->
                let rw_get_unsafe_void ls i = decode () in
                let rw_set_unsafe_void ls i v = encode v in {
                  InnerArray.length;
                  InnerArray.init;
                  InnerArray.get_unsafe = rw_get_unsafe_void;
                  InnerArray.set_unsafe = rw_set_unsafe_void;
                  InnerArray.storage = Some list_storage;
                }
            | _ ->
                invalid_msg
                  "decoded List<Void> where a different list type was expected"
            end
        | ListStorageType.Bit ->
            begin match codecs with
            | ListCodecs.Bit (decode, encode) ->
                let rw_get_unsafe_bool ls i =
                  let byte_ofs = i / 8 in
                  let bit_ofs  = i mod 8 in
                  let byte_val =
                    Slice.get_uint8 ls.ListStorage.storage byte_ofs
                  in
                  decode ((byte_val land (1 lsl bit_ofs)) <> 0)
                in
                let rw_set_unsafe_bool ls i v =
                  let byte_ofs = i / 8 in
                  let bit_ofs  = i mod 8 in
                  let bitmask  = 1 lsl bit_ofs in
                  let old_byte_val =
                    Slice.get_uint8 ls.ListStorage.storage byte_ofs
                  in
                  let new_byte_val =
                    if encode v then
                      old_byte_val lor bitmask
                    else
                      old_byte_val land (lnot bitmask)
                  in
                  Slice.set_uint8 ls.ListStorage.storage byte_ofs new_byte_val
                in {
                  InnerArray.length;
                  InnerArray.init;
                  InnerArray.get_unsafe = rw_get_unsafe_bool;
                  InnerArray.set_unsafe = rw_set_unsafe_bool;
                  InnerArray.storage = Some list_storage;
                }
            | _ ->
                invalid_msg
                  "decoded List<Bool> where a different list type was expected"
            end
        | ListStorageType.Bytes1 ->
            begin match codecs with
            | ListCodecs.Bytes1 (decode, encode)
            | ListCodecs.Struct { ListCodecs.bytes = (decode, encode); _ } ->
                let rw_get_unsafe_bytes1 ls i = decode (make_element_slice ls i 1) in
                let rw_set_unsafe_bytes1 ls i v = encode v (make_element_slice ls i 1) in {
                  InnerArray.length;
                  InnerArray.init;
                  InnerArray.get_unsafe = rw_get_unsafe_bytes1;
                  InnerArray.set_unsafe = rw_set_unsafe_bytes1;
                  InnerArray.storage = Some list_storage;
                }
            | _ ->
                invalid_msg
                  "decoded List<1 byte> where a different list type was expected"
            end
        | ListStorageType.Bytes2 ->
            begin match codecs with
            | ListCodecs.Bytes2 (decode, encode)
            | ListCodecs.Struct { ListCodecs.bytes = (decode, encode); _ } ->
                let rw_get_unsafe_bytes2 ls i = decode (make_element_slice ls i 2) in
                let rw_set_unsafe_bytes2 ls i v = encode v (make_element_slice ls i 2) in {
                  InnerArray.length;
                  InnerArray.init;
                  InnerArray.get_unsafe = rw_get_unsafe_bytes2;
                  InnerArray.set_unsafe = rw_set_unsafe_bytes2;
                  InnerArray.storage = Some list_storage;
                }
            | _ ->
                invalid_msg
                  "decoded List<2 byte> where a different list type was expected"
            end
        | ListStorageType.Bytes4 ->
            begin match codecs with
            | ListCodecs.Bytes4 (decode, encode)
            | ListCodecs.Struct { ListCodecs.bytes = (decode, encode); _ } ->
                let rw_get_unsafe_bytes4 ls i = decode (make_element_slice ls i 4) in
                let rw_set_unsafe_bytes4 ls i v = encode v (make_element_slice ls i 4) in {
                  InnerArray.length;
                  InnerArray.init;
                  InnerArray.get_unsafe = rw_get_unsafe_bytes4;
                  InnerArray.set_unsafe = rw_set_unsafe_bytes4;
                  InnerArray.storage = Some list_storage;
                }
            | _ ->
                invalid_msg
                  "decoded List<4 byte> where a different list type was expected"
            end
        | ListStorageType.Bytes8 ->
            begin match codecs with
            | ListCodecs.Bytes8 (decode, encode)
            | ListCodecs.Struct { ListCodecs.bytes = (decode, encode); _ } ->
                let rw_get_unsafe_bytes8 ls i = decode (make_element_slice ls i 8) in
                let rw_set_unsafe_bytes8 ls i v = encode v (make_element_slice ls i 8) in {
                  InnerArray.length;
                  InnerArray.init;
                  InnerArray.get_unsafe = rw_get_unsafe_bytes8;
                  InnerArray.set_unsafe = rw_set_unsafe_bytes8;
                  InnerArray.storage = Some list_storage;
                }
            | _ ->
                invalid_msg
                  "decoded List<8 byte> where a different list type was expected"
            end
        | ListStorageType.Pointer ->
            begin match codecs with
            | ListCodecs.Pointer (decode, encode)
            | ListCodecs.Struct { ListCodecs.pointer = (decode, encode); _ } ->
                let rw_get_unsafe_ptr ls i =
                  decode (make_element_slice ls i sizeof_uint64)
                in
                let rw_set_unsafe_ptr ls i v =
                  encode v (make_element_slice ls i sizeof_uint64)
                in {
                  InnerArray.length;
                  InnerArray.init;
                  InnerArray.get_unsafe = rw_get_unsafe_ptr;
                  InnerArray.set_unsafe = rw_set_unsafe_ptr;
                  InnerArray.storage = Some list_storage;
                }
            | _ ->
                invalid_msg
                  "decoded List<pointer> where a different list type was expected"
            end
        | ListStorageType.Composite (data_words, pointer_words) ->
            let data_size     = data_words * sizeof_uint64 in
            let pointers_size = pointer_words * sizeof_uint64 in
            let make_storage ls i ~data_size ~pointers_size =
              let total_size    = data_size + pointers_size in
              (* Skip over the composite tag word *)
              let content_offset =
                ls.ListStorage.storage.Slice.start + sizeof_uint64
              in
              let data = {
                ls.ListStorage.storage with
                Slice.start = content_offset + (i * total_size);
                Slice.len = data_size;
              } in
              let pointers = {
                data with
                Slice.start = Slice.get_end data;
                Slice.len   = pointers_size;
              } in
              { StructStorage.data; StructStorage.pointers; }
            in
            let make_bytes_handlers ~size ~decode ~encode =
              if data_words = 0 then
                invalid_msg
                  "decoded List<composite> with empty data region where data was expected"
              else
                let rw_get_unsafe_composite_bytes ls i =
                  let struct_storage = make_storage ls i ~data_size ~pointers_size in
                  let slice = {
                    struct_storage.StructStorage.data with
                    Slice.len = size
                  } in
                  decode slice
                in
                let rw_set_unsafe_composite_bytes ls i v =
                  let struct_storage = make_storage ls i ~data_size ~pointers_size in
                  let slice = {
                    struct_storage.StructStorage.data with
                    Slice.len = size
                  } in
                  encode v slice
                in {
                  InnerArray.length;
                  InnerArray.init;
                  InnerArray.get_unsafe = rw_get_unsafe_composite_bytes;
                  InnerArray.set_unsafe = rw_set_unsafe_composite_bytes;
                  InnerArray.storage = Some list_storage;
                }
            in
            begin match codecs with
            | ListCodecs.Empty (decode, encode) ->
                let rw_get_unsafe_composite_void ls i = decode () in
                let rw_set_unsafe_composite_void ls i v = encode v in {
                  InnerArray.length;
                  InnerArray.init;
                  InnerArray.get_unsafe = rw_get_unsafe_composite_void;
                  InnerArray.set_unsafe = rw_set_unsafe_composite_void;
                  InnerArray.storage = Some list_storage;
                }
            | ListCodecs.Bit (decode, encode) ->
                if data_words = 0 then
                  invalid_msg
                    "decoded List<composite> with empty data region where data was expected"
                else
                  let rw_get_unsafe_composite_bool ls i =
                    let struct_storage = make_storage ls i ~data_size ~pointers_size in
                    let first_byte = Slice.get_uint8 struct_storage.StructStorage.data 0 in
                    let is_set = (first_byte land 0x1) <> 0 in
                    decode is_set
                  in
                  let rw_set_unsafe_composite_bool ls i v =
                    let struct_storage = make_storage ls i ~data_size ~pointers_size in
                    let first_byte =
                      Slice.get_uint8 struct_storage.StructStorage.data 0
                    in
                    let first_byte =
                      if encode v then first_byte lor 0x1 else first_byte land 0xfe
                    in
                    Slice.set_uint8 struct_storage.StructStorage.data 0 first_byte
                  in {
                    InnerArray.length;
                    InnerArray.init;
                    InnerArray.get_unsafe = rw_get_unsafe_composite_bool;
                    InnerArray.set_unsafe = rw_set_unsafe_composite_bool;
                    InnerArray.storage = Some list_storage;
                  }
            | ListCodecs.Bytes1 (decode, encode) ->
                make_bytes_handlers ~size:1 ~decode ~encode
            | ListCodecs.Bytes2 (decode, encode) ->
                make_bytes_handlers ~size:2 ~decode ~encode
            | ListCodecs.Bytes4 (decode, encode) ->
                make_bytes_handlers ~size:4 ~decode ~encode
            | ListCodecs.Bytes8 (decode, encode) ->
                make_bytes_handlers ~size:8 ~decode ~encode
            | ListCodecs.Pointer (decode, encode) ->
                if pointer_words = 0 then
                  invalid_msg
                    "decoded List<composite> with empty pointers region where \
                     pointers were expected"
                else
                  let rw_get_unsafe_composite_ptr ls i =
                    let struct_storage = make_storage ls i ~data_size ~pointers_size in
                    let slice = {
                      struct_storage.StructStorage.pointers with
                      Slice.len = sizeof_uint64
                    } in
                    decode slice
                  in
                  let rw_set_unsafe_composite_ptr ls i v =
                    let struct_storage = make_storage ls i ~data_size ~pointers_size in
                    let slice = {
                      struct_storage.StructStorage.pointers with
                      Slice.len = sizeof_uint64
                    } in
                    encode v slice
                  in {
                    InnerArray.length;
                    InnerArray.init;
                    InnerArray.get_unsafe = rw_get_unsafe_composite_ptr;
                    InnerArray.set_unsafe = rw_set_unsafe_composite_ptr;
                    InnerArray.storage = Some list_storage;
                  }
            | ListCodecs.Struct { ListCodecs.composite = (decode, encode); _ } ->
                let rw_get_unsafe_composite_struct ls i =
                  decode (make_storage ls i ~data_size ~pointers_size)
                in
                let rw_set_unsafe_composite_struct ls i v =
                  encode v (make_storage ls i ~data_size ~pointers_size)
                in {
                  InnerArray.length;
                  InnerArray.init;
                  InnerArray.get_unsafe = rw_get_unsafe_composite_struct;
                  InnerArray.set_unsafe = rw_set_unsafe_composite_struct;
                  InnerArray.storage = Some list_storage;
                }
            end
      
      
      (* Given list storage which is expected to contain UInt8 data, decode the data as
         an OCaml string. *)
      let string_of_uint8_list
          ~(null_terminated : bool)   (* true if the data is expected to end in 0 *)
          (list_storage : 'cap ListStorage.t)
        : string =
        let open ListStorage in
        match list_storage.storage_type with
        | ListStorageType.Bytes1 ->
            let result_byte_count =
              if null_terminated then
                let () =
                  if list_storage.num_elements < 1 then
                    invalid_msg "empty string list has no space for null terminator"
                in
                let terminator =
                  Slice.get_uint8 list_storage.storage (list_storage.num_elements - 1)
                in
                let () = if terminator <> 0 then
                  invalid_msg "string list is not null terminated"
                in
                list_storage.num_elements - 1
              else
                list_storage.num_elements
            in
            let buf = CamlBytes.create result_byte_count in
            Slice.blit_to_bytes
              ~src:list_storage.storage ~src_pos:0
              ~dst:buf ~dst_pos:0
              ~len:result_byte_count;
            CamlBytes.unsafe_to_string buf
        | _ ->
            invalid_msg "decoded non-UInt8 list where string data was expected"
      
      
      let struct_of_bytes_slice slice =
        let data = slice in
        let pointers = {
          slice with
          Slice.start = Slice.get_end data;
          Slice.len   = 0;
        } in
        { StructStorage.data; StructStorage.pointers }
      
      let struct_of_pointer_slice slice =
        let () = assert (slice.Slice.len = sizeof_uint64) in
        let data = {
          slice with
          Slice.len = 0
        } in
        let pointers = {
          slice with
          Slice.len = sizeof_uint64;
        } in
        { StructStorage.data; StructStorage.pointers }
      
      
      (* Given some list storage corresponding to a struct list, construct
         a function for mapping an element index to the associated
         struct storage. *)
      let make_struct_of_list_index list_storage =
        let storage      = list_storage.ListStorage.storage in
        let storage_type = list_storage.ListStorage.storage_type in
        match list_storage.ListStorage.storage_type with
        | ListStorageType.Empty ->
            let make_struct_of_list_index_void i =
              let slice = {
                storage with
                Slice.start = storage.Slice.start;
                Slice.len   = 0;
              } in
              struct_of_bytes_slice slice
            in
            make_struct_of_list_index_void
        | ListStorageType.Bytes1
        | ListStorageType.Bytes2
        | ListStorageType.Bytes4
        | ListStorageType.Bytes8 ->
            (* Short data-only struct *)
            let byte_count = ListStorageType.get_byte_count storage_type in
            let make_struct_of_list_index_bytes i =
              let slice = {
                storage with
                Slice.start = storage.Slice.start + (i * byte_count);
                Slice.len   = byte_count;
              } in
              struct_of_bytes_slice slice
            in
            make_struct_of_list_index_bytes
        | ListStorageType.Pointer ->
            (* Single-pointer struct *)
            let make_struct_of_list_index_pointer i =
              let slice = {
                storage with
                Slice.start = (storage.Slice.start) + (i * sizeof_uint64);
                Slice.len   = sizeof_uint64;
              } in
              struct_of_pointer_slice slice
            in
            make_struct_of_list_index_pointer
        | ListStorageType.Composite (data_words, pointer_words) ->
            let data_size     = data_words * sizeof_uint64 in
            let pointers_size = pointer_words * sizeof_uint64 in
            let element_size  = data_size + pointers_size in
            (* Skip over the composite tag word *)
            let content_offset = storage.Slice.start + sizeof_uint64 in
            let make_struct_of_list_index_composite i =
              let data = {
                storage with
                Slice.start = content_offset + (i * element_size);
                Slice.len   = data_size;
              } in
              let pointers = {
                storage with
                Slice.start = Slice.get_end data;
                Slice.len   = pointers_size;
              } in
              { StructStorage.data; StructStorage.pointers }
            in
            make_struct_of_list_index_composite
        | ListStorageType.Bit ->
            invalid_msg "decoded List<Bool> where List<composite> was expected"
      
      
    end

    (* DefaultsCopier will provide algorithms for making deep copies of default
       data from DM storage into native storage *)
    module DefaultsCopier = BuilderOps.Make(DM)(NM)

    (* Most of the Builder operations need to copy from native storage back into
       native storage *)
    module BOps = BuilderOps.Make(NM)(NM)

    (* Given a string, generate an orphaned cap'n proto List<Uint8> which contains
       the string content. *)
    let uint8_list_of_string
        ~(null_terminated : bool)   (* true if the data is expected to end in 0 *)
        ~(dest_message : rw NM.Message.t)
        (src : string)
      : rw NM.ListStorage.t =
      let list_storage = BOps.alloc_list_storage dest_message
          ListStorageType.Bytes1
          (String.length src + (if null_terminated then 1 else 0))
      in
      NM.Slice.blit_from_string
        ~src ~src_pos:0
        ~dst:list_storage.NM.ListStorage.storage ~dst_pos:0
        ~len:(String.length src);
      list_storage


    let void_list_codecs = NC.ListCodecs.Empty (
        (fun (x : unit) -> x), (fun (x : unit) -> x))

    let bit_list_codecs = NC.ListCodecs.Bit (
        (fun (x : bool) -> x), (fun (x : bool) -> x))

    let int8_list_codecs = NC.ListCodecs.Bytes1 (
        (fun slice -> NM.Slice.get_int8 slice 0),
          (fun v slice -> NM.Slice.set_int8 slice 0 v))

    let int16_list_codecs = NC.ListCodecs.Bytes2 (
        (fun slice -> NM.Slice.get_int16 slice 0),
          (fun v slice -> NM.Slice.set_int16 slice 0 v))

    let int32_list_codecs = NC.ListCodecs.Bytes4 (
        (fun slice -> NM.Slice.get_int32 slice 0),
          (fun v slice -> NM.Slice.set_int32 slice 0 v))

    let int64_list_codecs = NC.ListCodecs.Bytes8 (
        (fun slice -> NM.Slice.get_int64 slice 0),
          (fun v slice -> NM.Slice.set_int64 slice 0 v))

    let uint8_list_codecs = NC.ListCodecs.Bytes1 (
        (fun slice -> NM.Slice.get_uint8 slice 0),
          (fun v slice -> NM.Slice.set_uint8 slice 0 v))

    let uint16_list_codecs = NC.ListCodecs.Bytes2 (
        (fun slice -> NM.Slice.get_uint16 slice 0),
          (fun v slice -> NM.Slice.set_uint16 slice 0 v))

    let uint32_list_codecs = NC.ListCodecs.Bytes4 (
        (fun slice -> NM.Slice.get_uint32 slice 0),
          (fun v slice -> NM.Slice.set_uint32 slice 0 v))

    let uint64_list_codecs = NC.ListCodecs.Bytes8 (
        (fun slice -> NM.Slice.get_uint64 slice 0),
          (fun v slice -> NM.Slice.set_uint64 slice 0 v))

    let float32_list_codecs = NC.ListCodecs.Bytes4 (
        (fun slice -> Int32.float_of_bits (NM.Slice.get_int32 slice 0)),
          (fun v slice -> NM.Slice.set_int32 slice 0
            (Int32.bits_of_float v)))

    let float64_list_codecs = NC.ListCodecs.Bytes8 (
        (fun slice -> Int64.float_of_bits (NM.Slice.get_int64 slice 0)),
          (fun v slice -> NM.Slice.set_int64 slice 0
            (Int64.bits_of_float v)))

    let text_list_codecs =
      let decode slice =
        (* Text fields are always accessed by value, not by reference, since
           we always do an immediate decode to [string].  Therefore we can
           use the Reader logic to handle this case. *)
        match RA_.deref_list_pointer slice with
        | Some list_storage ->
            NC.string_of_uint8_list ~null_terminated:true list_storage
        | None ->
            ""
      in
      let encode s slice =
        let new_list_storage = uint8_list_of_string ~null_terminated:true
            ~dest_message:slice.NM.Slice.msg s
        in
        BOps.init_list_pointer slice new_list_storage
      in
      NC.ListCodecs.Pointer (decode, encode)

    let blob_list_codecs =
      let decode slice =
        (* Data fields are always accessed by value, not by reference, since
           we always do an immediate decode to [string].  Therefore we can
           use the Reader logic to handle this case. *)
        match RA_.deref_list_pointer slice with
        | Some list_storage ->
            NC.string_of_uint8_list ~null_terminated:false list_storage
        | None ->
            ""
      in
      let encode s slice =
        let new_list_storage = uint8_list_of_string ~null_terminated:false
            ~dest_message:slice.NM.Slice.msg s
        in
        BOps.init_list_pointer slice new_list_storage
      in
      NC.ListCodecs.Pointer (decode, encode)

    let struct_list_codecs =
      let bytes_decoder slice =
        NC.struct_of_bytes_slice slice
      in
      let bytes_encoder v slice =
        let dest = NC.struct_of_bytes_slice slice in
        BOps.deep_copy_struct_to_dest ~src:v ~dest
      in
      let pointer_decoder slice =
        NC.struct_of_pointer_slice slice
      in
      let pointer_encoder v slice =
        let dest = NC.struct_of_pointer_slice slice in
        BOps.deep_copy_struct_to_dest ~src:v ~dest
      in
      let composite_decoder x = x in
      let composite_encoder v dest = BOps.deep_copy_struct_to_dest ~src:v ~dest in
      NC.ListCodecs.Struct {
        NC.ListCodecs.bytes     = (bytes_decoder, bytes_encoder);
        NC.ListCodecs.pointer   = (pointer_decoder, pointer_encoder);
        NC.ListCodecs.composite = (composite_decoder, composite_encoder);
      }


    (*******************************************************************************
     * METHODS FOR GETTING OBJECTS STORED BY VALUE
     *******************************************************************************)

    module Discr = struct
      type t = {
        value    : int;
        byte_ofs : int;
      }
    end

    let rec set_opt_discriminant
        (data : rw NM.Slice.t)
        (discr : Discr.t option)
      : unit =
      match discr with
      | None ->
          ()
      | Some x ->
          set_uint16 data ~default:0 ~byte_ofs:x.Discr.byte_ofs x.Discr.value

    and set_uint16
        ?(discr : Discr.t option)
        (data : rw NM.Slice.t)
        ~(default : int)
        ~(byte_ofs : int)
        (value : int)
      : unit =
      let () = set_opt_discriminant data discr in
      NM.Slice.set_uint16 data byte_ofs (value lxor default)


    (* Given storage for a struct, get the bytes associated with the
       struct data section.  If the optional discriminant parameter is
       supplied, then the discriminant is also set as a side-effect. *)
    let get_data_region
        ?(discr : Discr.t option)
        (struct_storage : rw NM.StructStorage.t)
      : rw NM.Slice.t =
      let data = struct_storage.NM.StructStorage.data in
      let () = set_opt_discriminant data discr in
      data

    let get_bit
       ~(default : bool)
       (struct_storage : rw NM.StructStorage.t)
       ~(byte_ofs : int)
       ~(bit_ofs : int)
      : bool =
      let data = struct_storage.NM.StructStorage.data in
      let byte_val = NM.Slice.get_uint8 data byte_ofs in
      let is_set = Util.get_bit byte_val bit_ofs in
      if default then
        not is_set
      else
        is_set

    let get_int8
        ~(default : int)
        (struct_storage : rw NM.StructStorage.t)
        (byte_ofs : int)
      : int =
      let data = struct_storage.NM.StructStorage.data in
      let numeric = NM.Slice.get_int8 data byte_ofs in
      numeric lxor default

    let get_int16
        ~(default : int)
        (struct_storage : rw NM.StructStorage.t)
        (byte_ofs : int)
      : int =
      let data = struct_storage.NM.StructStorage.data in
      let numeric = NM.Slice.get_int16 data byte_ofs in
      numeric lxor default

    let get_int32
        ~(default : int32)
        (struct_storage : rw NM.StructStorage.t)
        (byte_ofs : int)
        : int32 =
      let data = struct_storage.NM.StructStorage.data in
      let numeric = NM.Slice.get_int32 data byte_ofs in
      Int32.bit_xor numeric default

    let get_int64
        ~(default : int64)
        (struct_storage : rw NM.StructStorage.t)
        (byte_ofs : int)
      : int64 =
      let data = struct_storage.NM.StructStorage.data in
      let numeric = NM.Slice.get_int64 data byte_ofs in
      Int64.bit_xor numeric default

    let get_uint8
        ~(default : int)
        (struct_storage : rw NM.StructStorage.t)
        (byte_ofs : int)
      : int =
      let data = struct_storage.NM.StructStorage.data in
      let numeric = NM.Slice.get_uint8 data byte_ofs in
      numeric lxor default

    let get_uint16
        ~(default : int)
        (struct_storage : rw NM.StructStorage.t)
        (byte_ofs : int)
      : int =
      let data = struct_storage.NM.StructStorage.data in
      let numeric = NM.Slice.get_uint16 data byte_ofs in
      numeric lxor default

    let get_uint32
        ~(default : Uint32.t)
        (struct_storage : rw NM.StructStorage.t)
        (byte_ofs : int)
      : Uint32.t =
      let data = struct_storage.NM.StructStorage.data in
      let numeric = NM.Slice.get_uint32 data byte_ofs in
      Uint32.logxor numeric default

    let get_uint64
        ~(default : Uint64.t)
        (struct_storage : rw NM.StructStorage.t)
        (byte_ofs : int)
      : Uint64.t =
      let data = struct_storage.NM.StructStorage.data in
      let numeric = NM.Slice.get_uint64 data byte_ofs in
      Uint64.logxor numeric default

    let get_float32
        ~(default_bits : int32)
        (struct_storage : rw NM.StructStorage.t)
        (byte_ofs : int)
      : float =
      let data = struct_storage.NM.StructStorage.data in
      let numeric = NM.Slice.get_int32 data byte_ofs in
      let bits = Int32.bit_xor numeric default_bits in
      Int32.float_of_bits bits

    let get_float64
        ~(default_bits : int64)
        (struct_storage : rw NM.StructStorage.t)
        (byte_ofs : int)
      : float =
      let data = struct_storage.NM.StructStorage.data in
      let numeric = NM.Slice.get_int64 data byte_ofs in
      let bits = Int64.bit_xor numeric default_bits in
      Int64.float_of_bits bits


    (*******************************************************************************
     * METHODS FOR SETTING OBJECTS STORED BY VALUE
     *******************************************************************************)

    let set_void
        ?(discr : Discr.t option)
        (struct_storage : rw NM.StructStorage.t)
      : unit =
      let data = struct_storage.NM.StructStorage.data in
      set_opt_discriminant data discr

    let set_bit
        ?(discr : Discr.t option)
        ~(default : bool)
        (struct_storage : rw NM.StructStorage.t)
        ~(byte_ofs : int)
        ~(bit_ofs : int)
        (value : bool)
      : unit =
      let data = struct_storage.NM.StructStorage.data in
      let () = set_opt_discriminant data discr in
      let default_bit = Util.int_of_bool default in
      let value_bit = Util.int_of_bool value in
      let stored_bit = default_bit lxor value_bit in
      let byte_val = NM.Slice.get_uint8 data byte_ofs in
      let byte_val = byte_val land (lnot (1 lsl bit_ofs)) in
      let byte_val = byte_val lor (stored_bit lsl bit_ofs) in
      NM.Slice.set_uint8 data byte_ofs byte_val

    let set_int8
        ?(discr : Discr.t option)
        ~(default : int)
        (struct_storage : rw NM.StructStorage.t)
        (byte_ofs : int)
        (value : int)
      : unit =
      let data = struct_storage.NM.StructStorage.data in
      let () = set_opt_discriminant data discr in
      NM.Slice.set_int8 data byte_ofs (value lxor default)

    let set_int16
        ?(discr : Discr.t option)
        ~(default : int)
        (struct_storage : rw NM.StructStorage.t)
        (byte_ofs : int)
        (value : int)
      : unit =
      let data = struct_storage.NM.StructStorage.data in
      let () = set_opt_discriminant data discr in
      NM.Slice.set_int16 data byte_ofs (value lxor default)

    let set_int32
        ?(discr : Discr.t option)
        ~(default : int32)
        (struct_storage : rw NM.StructStorage.t)
        (byte_ofs : int)
        (value : int32)
      : unit =
      let data = struct_storage.NM.StructStorage.data in
      let () = set_opt_discriminant data discr in
      NM.Slice.set_int32 data byte_ofs (Int32.bit_xor value default)

    let set_int64
        ?(discr : Discr.t option)
        ~(default : int64)
        (struct_storage : rw NM.StructStorage.t)
        (byte_ofs : int)
        (value : int64)
      : unit =
      let data = struct_storage.NM.StructStorage.data in
      let () = set_opt_discriminant data discr in
      NM.Slice.set_int64 data byte_ofs (Int64.bit_xor value default)

    let set_uint8
        ?(discr : Discr.t option)
        ~(default : int)
        (struct_storage : rw NM.StructStorage.t)
        (byte_ofs : int)
        (value : int)
      : unit =
      let data = struct_storage.NM.StructStorage.data in
      let () = set_opt_discriminant data discr in
      NM.Slice.set_uint8 data byte_ofs (value lxor default)

    let set_uint16
        ?(discr : Discr.t option)
        ~(default : int)
        (struct_storage : rw NM.StructStorage.t)
        (byte_ofs : int)
        (value : int)
      : unit =
      let data = struct_storage.NM.StructStorage.data in
      let () = set_opt_discriminant data discr in
      NM.Slice.set_uint16 data byte_ofs (value lxor default)

    let set_uint32
        ?(discr : Discr.t option)
        ~(default : Uint32.t)
        (struct_storage : rw NM.StructStorage.t)
        (byte_ofs : int)
        (value : Uint32.t)
      : unit =
      let data = struct_storage.NM.StructStorage.data in
      let () = set_opt_discriminant data discr in
      NM.Slice.set_uint32 data byte_ofs (Uint32.logxor value default)

    let set_uint64
        ?(discr : Discr.t option)
        ~(default : Uint64.t)
        (struct_storage : rw NM.StructStorage.t)
        (byte_ofs : int)
        (value : Uint64.t)
      : unit =
      let data = struct_storage.NM.StructStorage.data in
      let () = set_opt_discriminant data discr in
      NM.Slice.set_uint64 data byte_ofs (Uint64.logxor value default)

    let set_float32
        ?(discr : Discr.t option)
        ~(default_bits : int32)
        (struct_storage : rw NM.StructStorage.t)
        (byte_ofs : int)
        (value : float)
      : unit =
      let data = struct_storage.NM.StructStorage.data in
      let () = set_opt_discriminant data discr in
      NM.Slice.set_int32 data byte_ofs
        (Int32.bit_xor (Int32.bits_of_float value) default_bits)

    let set_float64
        ?(discr : Discr.t option)
        ~(default_bits : int64)
        (struct_storage : rw NM.StructStorage.t)
        (byte_ofs : int)
        (value : float)
      : unit =
      let data = struct_storage.NM.StructStorage.data in
      let () = set_opt_discriminant data discr in
      NM.Slice.set_int64 data byte_ofs
        (Int64.bit_xor (Int64.bits_of_float value) default_bits)


    (*******************************************************************************
     * METHODS FOR GETTING OBJECTS STORED BY POINTER
     *******************************************************************************)

    let has_field
        (struct_storage : rw NM.StructStorage.t)
        (pointer_word : int)
      : bool =
      let pointers = struct_storage.NM.StructStorage.pointers in
      let num_pointers = pointers.NM.Slice.len / sizeof_uint64 in
      (* Struct should have already been upgraded to at least the
         expected data region and pointer region sizes *)
      assert (pointer_word < num_pointers);
      let pointer64 = NM.Slice.get_int64 pointers (pointer_word * sizeof_uint64) in
      not (Util.is_int64_zero pointer64)

    let get_text
        ~(default : string)
        (struct_storage : rw NM.StructStorage.t)
        (pointer_word : int)
      : string =
      let pointers = struct_storage.NM.StructStorage.pointers in
      let num_pointers = pointers.NM.Slice.len / sizeof_uint64 in
      (* Struct should have already been upgraded to at least the
         expected data region and pointer region sizes *)
      assert (pointer_word < num_pointers);
      let pointer_bytes = {
        pointers with
        NM.Slice.start = pointers.NM.Slice.start + (pointer_word * sizeof_uint64);
        NM.Slice.len   = sizeof_uint64;
      } in
      (* Text fields are always accessed by value, not by reference, since
         we always do an immediate decode to [string].  Therefore we can
         use the Reader logic to handle this case. *)
      match RA_.deref_list_pointer pointer_bytes with
      | Some list_storage ->
          NC.string_of_uint8_list ~null_terminated:true list_storage
      | None ->
          default

    let get_blob
        ~(default : string)
        (struct_storage : rw NM.StructStorage.t)
        (pointer_word : int)
      : string =
      let pointers = struct_storage.NM.StructStorage.pointers in
      let num_pointers = pointers.NM.Slice.len / sizeof_uint64 in
      (* Struct should have already been upgraded to at least the
         expected data region and pointer region sizes *)
      assert (pointer_word < num_pointers);
      let pointer_bytes = {
        pointers with
        NM.Slice.start = pointers.NM.Slice.start + (pointer_word * sizeof_uint64);
        NM.Slice.len   = sizeof_uint64;
      } in
      (* Data fields are always accessed by value, not by reference, since
         we always do an immediate decode to [string].  Therefore we can
         use the Reader logic to handle this case. *)
      match RA_.deref_list_pointer pointer_bytes with
      | Some list_storage ->
          NC.string_of_uint8_list ~null_terminated:false list_storage
      | None ->
          default


    (* Zero-initialize list storage of the given length and storage type, 
       associating it with the specified list pointer. *)
    let init_list_storage
        ~(storage_type : ListStorageType.t)
        ~(num_elements : int)
        (pointer_bytes : rw NM.Slice.t)
      : rw NM.ListStorage.t =
      let () = BOps.deep_zero_pointer pointer_bytes in
      let message = pointer_bytes.NM.Slice.msg in
      let list_storage = BOps.alloc_list_storage message storage_type num_elements in
      let () = BOps.init_list_pointer pointer_bytes list_storage in
      list_storage


    let get_list
        ?(struct_sizes : BuilderOps.StructSizes.t option)
        ?(default : ro DM.ListStorage.t option)
        ~(storage_type : ListStorageType.t)
        ~(codecs : 'a NC.ListCodecs.t)
        (struct_storage : rw NM.StructStorage.t)
        (pointer_word : int)
      : (rw, 'a, rw NM.ListStorage.t) InnerArray.t =
      let create_default message =
        match default with
        | Some default_storage ->
            DefaultsCopier.deep_copy_list ?struct_sizes
              ~src:default_storage ~dest_message:message ()
        | None ->
            BOps.alloc_list_storage message storage_type 0
      in
      let pointers = struct_storage.NM.StructStorage.pointers in
      let num_pointers = pointers.NM.Slice.len / sizeof_uint64 in
      (* Struct should have already been upgraded to at least the
         expected data region and pointer region sizes *)
      assert (pointer_word < num_pointers);
      let pointer_bytes = {
        pointers with
        NM.Slice.start = pointers.NM.Slice.start + (pointer_word * sizeof_uint64);
        NM.Slice.len   = sizeof_uint64;
      } in
      let list_storage = BOps.deref_list_pointer ?struct_sizes ~create_default
          pointer_bytes
      in
      NC.make_array_readwrite ~list_storage ~codecs
        ~init:(fun n -> init_list_storage ~storage_type ~num_elements:n pointer_bytes)

    let get_void_list
        ?(default : ro DM.ListStorage.t option)
        (struct_storage : rw NM.StructStorage.t)
        (pointer_word : int)
      : (rw, unit, rw NM.ListStorage.t) InnerArray.t =
      get_list ?default ~storage_type:ListStorageType.Empty
        ~codecs:void_list_codecs struct_storage pointer_word

    let get_bit_list
        ?(default : ro DM.ListStorage.t option)
        (struct_storage : rw NM.StructStorage.t)
        (pointer_word : int)
      : (rw, bool, rw NM.ListStorage.t) InnerArray.t =
      get_list ?default ~storage_type:ListStorageType.Bit
        ~codecs:bit_list_codecs struct_storage pointer_word

    let get_int8_list
        ?(default : ro DM.ListStorage.t option)
        (struct_storage : rw NM.StructStorage.t)
        (pointer_word : int)
      : (rw, int, rw NM.ListStorage.t) InnerArray.t =
      get_list ?default ~storage_type:ListStorageType.Bytes1
        ~codecs:int8_list_codecs struct_storage pointer_word

    let get_int16_list
        ?(default : ro DM.ListStorage.t option)
        (struct_storage : rw NM.StructStorage.t)
        (pointer_word : int)
      : (rw, int, rw NM.ListStorage.t) InnerArray.t =
      get_list ?default ~storage_type:ListStorageType.Bytes2
        ~codecs:int16_list_codecs struct_storage pointer_word

    let get_int32_list
        ?(default : ro DM.ListStorage.t option)
        (struct_storage : rw NM.StructStorage.t)
        (pointer_word : int)
      : (rw, int32, rw NM.ListStorage.t) InnerArray.t =
      get_list ?default ~storage_type:ListStorageType.Bytes4
        ~codecs:int32_list_codecs struct_storage pointer_word

    let get_int64_list
        ?(default : ro DM.ListStorage.t option)
        (struct_storage : rw NM.StructStorage.t)
        (pointer_word : int)
      : (rw, int64, rw NM.ListStorage.t) InnerArray.t =
      get_list ?default ~storage_type:ListStorageType.Bytes8
        ~codecs:int64_list_codecs struct_storage pointer_word

    let get_uint8_list
        ?(default : ro DM.ListStorage.t option)
        (struct_storage : rw NM.StructStorage.t)
        (pointer_word : int)
      : (rw, int, rw NM.ListStorage.t) InnerArray.t =
      get_list ?default ~storage_type:ListStorageType.Bytes1
        ~codecs:uint8_list_codecs struct_storage pointer_word

    let get_uint16_list
        ?(default : ro DM.ListStorage.t option)
        (struct_storage : rw NM.StructStorage.t)
        (pointer_word : int)
      : (rw, int, rw NM.ListStorage.t) InnerArray.t =
      get_list ?default ~storage_type:ListStorageType.Bytes2
        ~codecs:uint16_list_codecs struct_storage pointer_word

    let get_uint32_list
        ?(default : ro DM.ListStorage.t option)
        (struct_storage : rw NM.StructStorage.t)
        (pointer_word : int)
      : (rw, Uint32.t, rw NM.ListStorage.t) InnerArray.t =
      get_list ?default ~storage_type:ListStorageType.Bytes4
        ~codecs:uint32_list_codecs struct_storage pointer_word

    let get_uint64_list
        ?(default : ro DM.ListStorage.t option)
        (struct_storage : rw NM.StructStorage.t)
        (pointer_word : int)
      : (rw, Uint64.t, rw NM.ListStorage.t) InnerArray.t =
      get_list ?default ~storage_type:ListStorageType.Bytes8
        ~codecs:uint64_list_codecs struct_storage pointer_word

    let get_float32_list
        ?(default : ro DM.ListStorage.t option)
        (struct_storage : rw NM.StructStorage.t)
        (pointer_word : int)
      : (rw, float, rw NM.ListStorage.t) InnerArray.t =
      get_list ?default ~storage_type:ListStorageType.Bytes4
        ~codecs:float32_list_codecs struct_storage pointer_word

    let get_float64_list
        ?(default : ro DM.ListStorage.t option)
        (struct_storage : rw NM.StructStorage.t)
        (pointer_word : int)
      : (rw, float, rw NM.ListStorage.t) InnerArray.t =
      get_list ?default ~storage_type:ListStorageType.Bytes8
        ~codecs:float64_list_codecs struct_storage pointer_word

    let get_text_list
        ?(default : ro DM.ListStorage.t option)
        (struct_storage : rw NM.StructStorage.t)
        (pointer_word : int)
      : (rw, string, rw NM.ListStorage.t) InnerArray.t =
      get_list ?default ~storage_type:ListStorageType.Pointer
        ~codecs:text_list_codecs struct_storage pointer_word

    let get_blob_list
        ?(default : ro DM.ListStorage.t option)
        (struct_storage : rw NM.StructStorage.t)
        (pointer_word : int)
      : (rw, string, rw NM.ListStorage.t) InnerArray.t =
      get_list ?default ~storage_type:ListStorageType.Pointer
        ~codecs:blob_list_codecs struct_storage pointer_word

    let get_struct_list
        ?(default : ro DM.ListStorage.t option)
        ~(data_words : int)
        ~(pointer_words : int)
        (struct_storage : rw NM.StructStorage.t)
        (pointer_word : int)
      : (rw, rw NM.StructStorage.t, rw NM.ListStorage.t) InnerArray.t =
      get_list ~struct_sizes:{
        BuilderOps.StructSizes.data_words;
        BuilderOps.StructSizes.pointer_words }
        ?default ~storage_type:(
          ListStorageType.Composite (data_words, pointer_words))
        ~codecs:struct_list_codecs struct_storage pointer_word

    let get_struct
        ?(default : ro DM.StructStorage.t option)
        ~(data_words : int)
        ~(pointer_words : int)
        (struct_storage : rw NM.StructStorage.t)
        (pointer_word : int)
      : rw NM.StructStorage.t =
      let create_default message =
        match default with
        | Some default_storage ->
            DefaultsCopier.deep_copy_struct ~src:default_storage ~dest_message:message
              ~data_words ~pointer_words
        | None ->
            BOps.alloc_struct_storage message ~data_words ~pointer_words
      in
      let pointers = struct_storage.NM.StructStorage.pointers in
      let num_pointers = pointers.NM.Slice.len / sizeof_uint64 in
      (* Struct should have already been upgraded to at least the
         expected data region and pointer region sizes *)
      assert (pointer_word < num_pointers);
      let pointer_bytes = {
        pointers with
        NM.Slice.start = pointers.NM.Slice.start + (pointer_word * sizeof_uint64);
        NM.Slice.len   = sizeof_uint64;
      } in
      BOps.deref_struct_pointer ~create_default ~data_words ~pointer_words pointer_bytes

    let get_pointer
        ?(default : ro DM.Slice.t option)
        (struct_storage : rw NM.StructStorage.t)
        (pointer_word : int)
      : rw NM.Slice.t =
      let pointers = struct_storage.NM.StructStorage.pointers in
      let num_pointers = pointers.NM.Slice.len / sizeof_uint64 in
      (* Struct should have already been upgraded to at least the
         expected data region and pointer region sizes *)
      assert (pointer_word < num_pointers);
      let pointer_bytes = {
        pointers with
        NM.Slice.start = pointers.NM.Slice.start + (pointer_word * sizeof_uint64);
        NM.Slice.len   = sizeof_uint64;
      } in
      let () =
        let pointer_val = NM.Slice.get_int64 pointer_bytes 0 in
        if Util.is_int64_zero pointer_val then
          match default with
          | Some default_pointer ->
              DefaultsCopier.deep_copy_pointer ~src:default_pointer
                ~dest:pointer_bytes
          | None ->
              ()
        else
          ()
      in
      pointer_bytes

    let get_interface
        (struct_storage : rw NM.StructStorage.t)
        (pointer_word : int)
      : Uint32.t option =
      let pointers = struct_storage.NM.StructStorage.pointers in
      let num_pointers = pointers.NM.Slice.len / sizeof_uint64 in
      (* Struct should have already been upgraded to at least the
         expected data region and pointer region sizes *)
      assert (pointer_word < num_pointers);
      let pointer_bytes = {
        pointers with
        NM.Slice.start = pointers.NM.Slice.start + (pointer_word * sizeof_uint64);
        NM.Slice.len   = sizeof_uint64;
      } in
      match NC.decode_pointer pointer_bytes with
      | Pointer.Null ->
          None
      | Pointer.Other (OtherPointer.Capability index) ->
          Some index
      | _ ->
          invalid_msg "decoded non-capability pointer where capability was expected"


    (*******************************************************************************
     * METHODS FOR SETTING OBJECTS STORED BY POINTER
     *******************************************************************************)

    let set_text
        ?(discr : Discr.t option)
        (struct_storage : rw NM.StructStorage.t)
        (pointer_word : int)
        (value : string)
      : unit =
      let pointers = struct_storage.NM.StructStorage.pointers in
      let num_pointers = pointers.NM.Slice.len / sizeof_uint64 in
      (* Struct should have already been upgraded to at least the
         expected data region and pointer region sizes *)
      assert (pointer_word < num_pointers);
      let pointer_bytes = {
        pointers with
        NM.Slice.start = pointers.NM.Slice.start + (pointer_word * sizeof_uint64);
        NM.Slice.len   = sizeof_uint64;
      } in
      let () = set_opt_discriminant struct_storage.NM.StructStorage.data discr in
      let new_string_storage = uint8_list_of_string
        ~null_terminated:true ~dest_message:pointer_bytes.NM.Slice.msg
        value
      in
      let () = BOps.deep_zero_pointer pointer_bytes in
      BOps.init_list_pointer pointer_bytes new_string_storage

    let set_blob
        ?(discr : Discr.t option)
        (struct_storage : rw NM.StructStorage.t)
        (pointer_word : int)
        (value : string)
      : unit =
      let pointers = struct_storage.NM.StructStorage.pointers in
      let num_pointers = pointers.NM.Slice.len / sizeof_uint64 in
      (* Struct should have already been upgraded to at least the
         expected data region and pointer region sizes *)
      assert (pointer_word < num_pointers);
      let pointer_bytes = {
        pointers with
        NM.Slice.start = pointers.NM.Slice.start + (pointer_word * sizeof_uint64);
        NM.Slice.len   = sizeof_uint64;
      } in
      let () = set_opt_discriminant struct_storage.NM.StructStorage.data discr in
      let new_string_storage = uint8_list_of_string
        ~null_terminated:false ~dest_message:pointer_bytes.NM.Slice.msg
        value
      in
      let () = BOps.deep_zero_pointer pointer_bytes in
      BOps.init_list_pointer pointer_bytes new_string_storage

    let set_list_from_storage
        ?(struct_sizes : BuilderOps.StructSizes.t option)
        ~(storage_type : ListStorageType.t)
        ~(codecs : 'a NC.ListCodecs.t)
        (struct_storage : rw NM.StructStorage.t)
        (pointer_word : int)
        (value : 'cap NM.ListStorage.t option)
      : (rw, 'a, rw NM.ListStorage.t) InnerArray.t =
      let pointers = struct_storage.NM.StructStorage.pointers in
      let num_pointers = pointers.NM.Slice.len / sizeof_uint64 in
      (* Struct should have already been upgraded to at least the
         expected data region and pointer region sizes *)
      assert (pointer_word < num_pointers);
      let pointer_bytes = {
        pointers with
        NM.Slice.start = pointers.NM.Slice.start + (pointer_word * sizeof_uint64);
        NM.Slice.len   = sizeof_uint64;
      } in
      let list_storage =
        match value with
        | Some src_storage ->
            BOps.deep_copy_list ?struct_sizes
              ~src:src_storage ~dest_message:pointer_bytes.NM.Slice.msg ()
        | None ->
            BOps.alloc_list_storage pointer_bytes.NM.Slice.msg storage_type 0
      in
      let () = BOps.deep_zero_pointer pointer_bytes in
      let () = BOps.init_list_pointer pointer_bytes list_storage in
      NC.make_array_readwrite ~list_storage ~codecs
        ~init:(fun n -> init_list_storage ~storage_type ~num_elements:n pointer_bytes)

    let set_list
        ?(discr : Discr.t option)
        ?(struct_sizes : BuilderOps.StructSizes.t option)
        ~(storage_type : ListStorageType.t)
        ~(codecs : 'a NC.ListCodecs.t)
        (struct_storage : rw NM.StructStorage.t)
        (pointer_word : int)
        (value : ('cap1, 'a, 'cap2 NM.ListStorage.t) InnerArray.t)
      : (rw, 'a, rw NM.ListStorage.t) InnerArray.t =
      let () = set_opt_discriminant struct_storage.NM.StructStorage.data discr in
      set_list_from_storage ?struct_sizes ~storage_type ~codecs
        struct_storage pointer_word (InnerArray.to_storage value)

    let set_void_list
        ?(discr : Discr.t option)
        (struct_storage : rw NM.StructStorage.t)
        (pointer_word : int)
        (value : ('cap1, unit, 'cap2 NM.ListStorage.t) InnerArray.t)
      : (rw, unit, rw NM.ListStorage.t) InnerArray.t =
      set_list ?discr ~storage_type:ListStorageType.Empty ~codecs:void_list_codecs
        struct_storage pointer_word value

    let set_bit_list
        ?(discr : Discr.t option)
        (struct_storage : rw NM.StructStorage.t)
        (pointer_word : int)
        (value : ('cap1, bool, 'cap2 NM.ListStorage.t) InnerArray.t)
      : (rw, bool, rw NM.ListStorage.t) InnerArray.t =
      set_list ?discr ~storage_type:ListStorageType.Bit ~codecs:bit_list_codecs
        struct_storage pointer_word value

    let set_int8_list
        ?(discr : Discr.t option)
        (struct_storage : rw NM.StructStorage.t)
        (pointer_word : int)
        (value : ('cap1, int, 'cap2 NM.ListStorage.t) InnerArray.t)
      : (rw, int, 'cap NM.ListStorage.t) InnerArray.t =
      set_list ?discr ~storage_type:ListStorageType.Bytes1 ~codecs:int8_list_codecs
        struct_storage pointer_word value

    let set_int16_list
        ?(discr : Discr.t option)
        (struct_storage : rw NM.StructStorage.t)
        (pointer_word : int)
        (value : ('cap1, int, 'cap2 NM.ListStorage.t) InnerArray.t)
      : (rw, int, 'cap NM.ListStorage.t) InnerArray.t =
      set_list ?discr ~storage_type:ListStorageType.Bytes2 ~codecs:int16_list_codecs
        struct_storage pointer_word value

    let set_int32_list
        ?(discr : Discr.t option)
        (struct_storage : rw NM.StructStorage.t)
        (pointer_word : int)
        (value : ('cap1, int32, 'cap NM.ListStorage.t) InnerArray.t)
      : (rw, int32, rw NM.ListStorage.t) InnerArray.t =
      set_list ?discr ~storage_type:ListStorageType.Bytes4 ~codecs:int32_list_codecs
        struct_storage pointer_word value

    let set_int64_list
        ?(discr : Discr.t option)
        (struct_storage : rw NM.StructStorage.t)
        (pointer_word : int)
        (value : ('cap1, int64, 'cap2 NM.ListStorage.t) InnerArray.t)
      : (rw, int64, rw NM.ListStorage.t) InnerArray.t =
      set_list ?discr ~storage_type:ListStorageType.Bytes8 ~codecs:int64_list_codecs
        struct_storage pointer_word value

    let set_uint8_list
        ?(discr : Discr.t option)
        (struct_storage : rw NM.StructStorage.t)
        (pointer_word : int)
        (value : ('cap1, int, 'cap2 NM.ListStorage.t) InnerArray.t)
      : (rw, int, rw NM.ListStorage.t) InnerArray.t =
      set_list ?discr ~storage_type:ListStorageType.Bytes1 ~codecs:uint8_list_codecs
        struct_storage pointer_word value

    let set_uint16_list
        ?(discr : Discr.t option)
        (struct_storage : rw NM.StructStorage.t)
        (pointer_word : int)
        (value : ('cap1, int, 'cap2 NM.ListStorage.t) InnerArray.t)
      : (rw, int, rw NM.ListStorage.t) InnerArray.t =
      set_list ?discr ~storage_type:ListStorageType.Bytes2 ~codecs:uint16_list_codecs
        struct_storage pointer_word value

    let set_uint32_list
        ?(discr : Discr.t option)
        (struct_storage : rw NM.StructStorage.t)
        (pointer_word : int)
        (value : ('cap1, Uint32.t, 'cap2 NM.ListStorage.t) InnerArray.t)
      : (rw, Uint32.t, rw NM.ListStorage.t) InnerArray.t =
      set_list ?discr ~storage_type:ListStorageType.Bytes4 ~codecs:uint32_list_codecs
        struct_storage pointer_word value

    let set_uint64_list
        ?(discr : Discr.t option)
        (struct_storage : rw NM.StructStorage.t)
        (pointer_word : int)
        (value : ('cap1, Uint64.t, 'cap2 NM.ListStorage.t) InnerArray.t)
      : (rw, Uint64.t, rw NM.ListStorage.t) InnerArray.t =
      set_list ?discr ~storage_type:ListStorageType.Bytes8 ~codecs:uint64_list_codecs
        struct_storage pointer_word value

    let set_float32_list
        ?(discr : Discr.t option)
        (struct_storage : rw NM.StructStorage.t)
        (pointer_word : int)
        (value : ('cap1, float, 'cap2 NM.ListStorage.t) InnerArray.t)
      : (rw, float, rw NM.ListStorage.t) InnerArray.t =
      set_list ?discr ~storage_type:ListStorageType.Bytes4 ~codecs:float32_list_codecs
        struct_storage pointer_word value

    let set_float64_list
        ?(discr : Discr.t option)
        (struct_storage : rw NM.StructStorage.t)
        (pointer_word : int)
        (value : ('cap1, float, 'cap2 NM.ListStorage.t) InnerArray.t)
      : (rw, float, rw NM.ListStorage.t) InnerArray.t =
      set_list ?discr ~storage_type:ListStorageType.Bytes8 ~codecs:float64_list_codecs
        struct_storage pointer_word value

    let set_text_list
        ?(discr : Discr.t option)
        (struct_storage : rw NM.StructStorage.t)
        (pointer_word : int)
        (value : ('cap1, string, 'cap2 NM.ListStorage.t) InnerArray.t)
      : (rw, string, rw NM.ListStorage.t) InnerArray.t =
      set_list ?discr ~storage_type:ListStorageType.Pointer ~codecs:text_list_codecs
        struct_storage pointer_word value

    let set_blob_list
        ?(discr : Discr.t option)
        (struct_storage : rw NM.StructStorage.t)
        (pointer_word : int)
        (value : ('cap1, string, 'cap2 NM.ListStorage.t) InnerArray.t)
      : (rw, string, rw NM.ListStorage.t) InnerArray.t =
      set_list ?discr ~storage_type:ListStorageType.Pointer ~codecs:blob_list_codecs
        struct_storage pointer_word value

    let set_struct_list
        ?(discr : Discr.t option)
        ~(data_words : int)
        ~(pointer_words : int)
        (* FIXME: this won't allow assignment from Reader struct lists *)
        (struct_storage : rw NM.StructStorage.t)
        (pointer_word : int)
        (value : ('cap1, 'cap2 NM.StructStorage.t, 'cap2 NM.ListStorage.t) InnerArray.t)
      : (rw, rw NM.StructStorage.t, rw NM.ListStorage.t) InnerArray.t =
      set_list ?discr ~struct_sizes:{
        BuilderOps.StructSizes.data_words;
        BuilderOps.StructSizes.pointer_words }
        ~storage_type:(ListStorageType.Composite (data_words, pointer_words))
        ~codecs:struct_list_codecs struct_storage pointer_word value

    let set_struct
        ?(discr : Discr.t option)
        ~(data_words : int)
        ~(pointer_words : int)
        (struct_storage : rw NM.StructStorage.t)
        (pointer_word : int)
        (value : 'cap NM.StructStorage.t option)
      : rw NM.StructStorage.t =
      let pointers = struct_storage.NM.StructStorage.pointers in
      let num_pointers = pointers.NM.Slice.len / sizeof_uint64 in
      (* Struct should have already been upgraded to at least the
         expected data region and pointer region sizes *)
      assert (pointer_word < num_pointers);
      let pointer_bytes = {
        pointers with
        NM.Slice.start = pointers.NM.Slice.start + (pointer_word * sizeof_uint64);
        NM.Slice.len   = sizeof_uint64;
      } in
      let () = set_opt_discriminant struct_storage.NM.StructStorage.data discr in
      let dest_storage =
        match value with
        | Some src_storage ->
            BOps.deep_copy_struct ~src:src_storage
              ~dest_message:pointer_bytes.NM.Slice.msg ~data_words ~pointer_words
        | None ->
            BOps.alloc_struct_storage pointer_bytes.NM.Slice.msg ~data_words ~pointer_words
      in
      let () = BOps.deep_zero_pointer pointer_bytes in
      let () = BOps.init_struct_pointer pointer_bytes dest_storage in
      dest_storage

    let set_pointer
        ?(discr : Discr.t option)
        (struct_storage : rw NM.StructStorage.t)
        (pointer_word : int)
        (value : 'cap NM.Slice.t)
      : rw NM.Slice.t =
      let pointers = struct_storage.NM.StructStorage.pointers in
      let num_pointers = pointers.NM.Slice.len / sizeof_uint64 in
      (* Struct should have already been upgraded to at least the
         expected data region and pointer region sizes *)
      assert (pointer_word < num_pointers);
      let pointer_bytes = {
        pointers with
        NM.Slice.start = pointers.NM.Slice.start + (pointer_word * sizeof_uint64);
        NM.Slice.len   = sizeof_uint64;
      } in
      let () = set_opt_discriminant struct_storage.NM.StructStorage.data discr in
      let () = BOps.deep_copy_pointer ~src:value ~dest:pointer_bytes in
      pointer_bytes

    let set_interface
        ?(discr : Discr.t option)
        (struct_storage : rw NM.StructStorage.t)
        (pointer_word : int)
        (value : Uint32.t option)
      : unit =
      let pointers = struct_storage.NM.StructStorage.pointers in
      let num_pointers = pointers.NM.Slice.len / sizeof_uint64 in
      (* Struct should have already been upgraded to at least the
         expected data region and pointer region sizes *)
      assert (pointer_word < num_pointers);
      let pointer_bytes = {
        pointers with
        NM.Slice.start = pointers.NM.Slice.start + (pointer_word * sizeof_uint64);
        NM.Slice.len   = sizeof_uint64;
      } in
      let () = set_opt_discriminant struct_storage.NM.StructStorage.data discr in
      match value with
      | Some index ->
          NM.Slice.set_int64 pointer_bytes 0
            (OtherPointer.encode (OtherPointer.Capability index))
      | None ->
          NM.Slice.set_int64 pointer_bytes 0 Int64.zero


    (*******************************************************************************
     * METHODS FOR INITIALIZING OBJECTS STORED BY POINTER
     *******************************************************************************)

    let init_blob
        ?(discr : Discr.t option)
        (struct_storage : rw NM.StructStorage.t)
        (pointer_word : int)
        (num_elements : int)
      : unit =
      let s = String.make num_elements '\x00' in
      set_blob ?discr struct_storage pointer_word s

    let init_list
        ?(discr : Discr.t option)
        ~(storage_type : ListStorageType.t)
        ~(codecs : 'a NC.ListCodecs.t)
        (struct_storage : 'cap NM.StructStorage.t)
        (pointer_word : int)
        (num_elements : int)
      : (rw, 'a, rw NM.ListStorage.t) InnerArray.t =
      let pointers = struct_storage.NM.StructStorage.pointers in
      let num_pointers = pointers.NM.Slice.len / sizeof_uint64 in
      (* Struct should have already been upgraded to at least the
         expected data region and pointer region sizes *)
      assert (pointer_word < num_pointers);
      let pointer_bytes = {
        pointers with
        NM.Slice.start = pointers.NM.Slice.start + (pointer_word * sizeof_uint64);
        NM.Slice.len   = sizeof_uint64;
      } in
      let () = set_opt_discriminant struct_storage.NM.StructStorage.data discr in
      let list_storage = init_list_storage ~storage_type ~num_elements pointer_bytes in
      NC.make_array_readwrite ~list_storage ~codecs
        ~init:(fun n -> init_list_storage ~storage_type ~num_elements:n pointer_bytes)

    let init_void_list
        ?(discr : Discr.t option)
        (struct_storage : rw NM.StructStorage.t)
        (pointer_word : int)
        (num_elements : int)
      : (rw, unit, rw NM.ListStorage.t) InnerArray.t =
      init_list ?discr ~storage_type:ListStorageType.Empty ~codecs:void_list_codecs
        struct_storage pointer_word num_elements

    let init_bit_list
        ?(discr : Discr.t option)
        (struct_storage : rw NM.StructStorage.t)
        (pointer_word : int)
        (num_elements : int)
      : (rw, bool, rw NM.ListStorage.t) InnerArray.t =
      init_list ?discr ~storage_type:ListStorageType.Bit ~codecs:bit_list_codecs
        struct_storage pointer_word num_elements

    let init_int8_list
        ?(discr : Discr.t option)
        (struct_storage : rw NM.StructStorage.t)
        (pointer_word : int)
        (num_elements : int)
      : (rw, int, rw NM.ListStorage.t) InnerArray.t =
      init_list ?discr ~storage_type:ListStorageType.Bytes1 ~codecs:int8_list_codecs
        struct_storage pointer_word num_elements

    let init_int16_list
        ?(discr : Discr.t option)
        (struct_storage : rw NM.StructStorage.t)
        (pointer_word : int)
        (num_elements : int)
      : (rw, int, rw NM.ListStorage.t) InnerArray.t =
      init_list ?discr ~storage_type:ListStorageType.Bytes2 ~codecs:int16_list_codecs
        struct_storage pointer_word num_elements

    let init_int32_list
        ?(discr : Discr.t option)
        (struct_storage : rw NM.StructStorage.t)
        (pointer_word : int)
        (num_elements : int)
      : (rw, int32, rw NM.ListStorage.t) InnerArray.t =
      init_list ?discr ~storage_type:ListStorageType.Bytes4 ~codecs:int32_list_codecs
        struct_storage pointer_word num_elements

    let init_int64_list
        ?(discr : Discr.t option)
        (struct_storage : rw NM.StructStorage.t)
        (pointer_word : int)
        (num_elements : int)
      : (rw, int64, rw NM.ListStorage.t) InnerArray.t =
      init_list ?discr ~storage_type:ListStorageType.Bytes8 ~codecs:int64_list_codecs
        struct_storage pointer_word num_elements

    let init_uint8_list
        ?(discr : Discr.t option)
        (struct_storage : rw NM.StructStorage.t)
        (pointer_word : int)
        (num_elements : int)
      : (rw, int, rw NM.ListStorage.t) InnerArray.t =
      init_list ?discr ~storage_type:ListStorageType.Bytes1 ~codecs:uint8_list_codecs
        struct_storage pointer_word num_elements

    let init_uint16_list
        ?(discr : Discr.t option)
        (struct_storage : rw NM.StructStorage.t)
        (pointer_word : int)
        (num_elements : int)
      : (rw, int, rw NM.ListStorage.t) InnerArray.t =
      init_list ?discr ~storage_type:ListStorageType.Bytes2 ~codecs:uint16_list_codecs
        struct_storage pointer_word num_elements

    let init_uint32_list
        ?(discr : Discr.t option)
        (struct_storage : rw NM.StructStorage.t)
        (pointer_word : int)
        (num_elements : int)
      : (rw, Uint32.t, rw NM.ListStorage.t) InnerArray.t =
      init_list ?discr ~storage_type:ListStorageType.Bytes4 ~codecs:uint32_list_codecs
        struct_storage pointer_word num_elements

    let init_uint64_list
        ?(discr : Discr.t option)
        (struct_storage : rw NM.StructStorage.t)
        (pointer_word : int)
        (num_elements : int)
      : (rw, Uint64.t, rw NM.ListStorage.t) InnerArray.t =
      init_list ?discr ~storage_type:ListStorageType.Bytes8 ~codecs:uint64_list_codecs
        struct_storage pointer_word num_elements

    let init_float32_list
        ?(discr : Discr.t option)
        (struct_storage : rw NM.StructStorage.t)
        (pointer_word : int)
        (num_elements : int)
      : (rw, float, rw NM.ListStorage.t) InnerArray.t =
      init_list ?discr ~storage_type:ListStorageType.Bytes4 ~codecs:float32_list_codecs
        struct_storage pointer_word num_elements

    let init_float64_list
        ?(discr : Discr.t option)
        (struct_storage : rw NM.StructStorage.t)
        (pointer_word : int)
        (num_elements : int)
      : (rw, float, rw NM.ListStorage.t) InnerArray.t =
      init_list ?discr ~storage_type:ListStorageType.Bytes8 ~codecs:float64_list_codecs
        struct_storage pointer_word num_elements

    let init_text_list
        ?(discr : Discr.t option)
        (struct_storage : rw NM.StructStorage.t)
        (pointer_word : int)
        (num_elements : int)
      : (rw, string, rw NM.ListStorage.t) InnerArray.t =
      init_list ?discr ~storage_type:ListStorageType.Pointer ~codecs:text_list_codecs
        struct_storage pointer_word num_elements

    let init_blob_list
        ?(discr : Discr.t option)
        (struct_storage : rw NM.StructStorage.t)
        (pointer_word : int)
        (num_elements : int)
      : (rw, string, rw NM.ListStorage.t) InnerArray.t =
      init_list ?discr ~storage_type:ListStorageType.Pointer ~codecs:blob_list_codecs
        struct_storage pointer_word num_elements

    let init_struct_list
        ?(discr : Discr.t option)
        ~(data_words : int)
        ~(pointer_words : int)
        (struct_storage : rw NM.StructStorage.t)
        (pointer_word : int)
        (num_elements : int)
      : (rw, rw NM.StructStorage.t, rw NM.ListStorage.t) InnerArray.t =
      init_list ?discr ~storage_type:(
        ListStorageType.Composite (data_words, pointer_words))
        struct_storage pointer_word ~codecs:struct_list_codecs num_elements

    let init_struct
        ?(discr : Discr.t option)
        ~(data_words : int)
        ~(pointer_words : int)
        (struct_storage : rw NM.StructStorage.t)
        (pointer_word : int)
      : rw NM.StructStorage.t =
      let pointers = struct_storage.NM.StructStorage.pointers in
      let num_pointers = pointers.NM.Slice.len / sizeof_uint64 in
      (* Struct should have already been upgraded to at least the
         expected data region and pointer region sizes *)
      assert (pointer_word < num_pointers);
      let pointer_bytes = {
        pointers with
        NM.Slice.start = pointers.NM.Slice.start + (pointer_word * sizeof_uint64);
        NM.Slice.len   = sizeof_uint64;
      } in
      let () = set_opt_discriminant struct_storage.NM.StructStorage.data discr in
      let () = BOps.deep_zero_pointer pointer_bytes in
      let storage =
        BOps.alloc_struct_storage pointer_bytes.NM.Slice.msg ~data_words ~pointer_words
      in
      let () = BOps.init_struct_pointer pointer_bytes storage in
      storage

    (* Locate the storage region corresponding to the root struct of a message.
       The [data_words] and [pointer_words] specify the expected struct layout. *)
    let get_root_struct
        (m : rw NM.Message.t)
        ~(data_words : int)
        ~(pointer_words : int)
      : rw NM.StructStorage.t =
      let first_segment = NM.Message.get_segment m 0 in
      if NM.Segment.length first_segment < sizeof_uint64 then
        invalid_msg "message is too small to contain root struct pointer"
      else
        let pointer_bytes = {
          NM.Slice.msg        = m;
          NM.Slice.segment    = first_segment;
          NM.Slice.segment_id = 0;
          NM.Slice.start      = 0;
          NM.Slice.len        = sizeof_uint64
        } in
        let create_default message =
          BOps.alloc_struct_storage message ~data_words ~pointer_words
        in
        BOps.deref_struct_pointer ~create_default ~data_words ~pointer_words
          pointer_bytes


    (* Allocate a new message of at least the given [message_size], creating a
       root struct with the specified struct layout.
       Returns: newly-allocated root struct storage *)
    let alloc_root_struct
        ?(message_size : int option)
        ~(data_words : int)
        ~(pointer_words : int)
        ()
      : rw NM.StructStorage.t =
      let act_message_size =
        let requested_size =
          match message_size with
          | Some x -> x
          | None   -> 8192
        in
        max requested_size ((data_words + pointer_words + 1) * sizeof_uint64)
      in
      let message = NM.Message.create act_message_size in
      (* Has the important side effect of reserving space in the message for
         the root struct pointer... *)
      let _ = NM.Slice.alloc message sizeof_uint64 in
      get_root_struct message ~data_words ~pointer_words

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
          RA_.get_uint64 ~default:Uint64.zero x 8
        let type_id_get_int_exn x =
          Capnp.Runtime.Util.int_of_uint64_exn (type_id_get x)
        let of_message x = RA_.get_root_struct (RA_.Message.readonly x)
        let of_builder x = Some (RA_.StructStorage.readonly x)
      end
      module Interface = struct
        type t = reader_t_Interface_17116997365232503999
        type builder_t = builder_t_Interface_17116997365232503999
        let type_id_get x =
          RA_.get_uint64 ~default:Uint64.zero x 8
        let type_id_get_int_exn x =
          Capnp.Runtime.Util.int_of_uint64_exn (type_id_get x)
        let of_message x = RA_.get_root_struct (RA_.Message.readonly x)
        let of_builder x = Some (RA_.StructStorage.readonly x)
      end
      module List = struct
        type t = reader_t_List_9792858745991129751
        type builder_t = builder_t_List_9792858745991129751
        let has_element_type x = 
          (RA_.has_field x 0)
        let element_type_get x = 
          (RA_.get_struct x 0)
        let of_message x = RA_.get_root_struct (RA_.Message.readonly x)
        let of_builder x = Some (RA_.StructStorage.readonly x)
      end
      module Struct = struct
        type t = reader_t_Struct_12410354185295152851
        type builder_t = builder_t_Struct_12410354185295152851
        let type_id_get x =
          RA_.get_uint64 ~default:Uint64.zero x 8
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
        match RA_.get_uint16 ~default:0 x 0 with
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
        RA_.get_bit ~default:false x ~byte_ofs:2 ~bit_ofs:0
      let int8_get x =
        RA_.get_int8 ~default:(0) x 2
      let int16_get x =
        RA_.get_int16 ~default:(0) x 2
      let int32_get x =
        RA_.get_int32 ~default:(0l) x 4
      let int32_get_int_exn x =
        Capnp.Runtime.Util.int_of_int32_exn (int32_get x)
      let int64_get x =
        RA_.get_int64 ~default:(0L) x 8
      let int64_get_int_exn x =
        Capnp.Runtime.Util.int_of_int64_exn (int64_get x)
      let uint8_get x =
        RA_.get_uint8 ~default:0 x 2
      let uint16_get x =
        RA_.get_uint16 ~default:0 x 2
      let uint32_get x =
        RA_.get_uint32 ~default:Uint32.zero x 4
      let uint32_get_int_exn x =
        Capnp.Runtime.Util.int_of_uint32_exn (uint32_get x)
      let uint64_get x =
        RA_.get_uint64 ~default:Uint64.zero x 8
      let uint64_get_int_exn x =
        Capnp.Runtime.Util.int_of_uint64_exn (uint64_get x)
      let float32_get x =
        RA_.get_float32 ~default_bits:(0l) x 4
      let float64_get x =
        RA_.get_float64 ~default_bits:(0L) x 8
      let has_text x =
        RA_.has_field x 0
      let text_get x =
        RA_.get_text ~default:"" x 0
      let has_data x =
        RA_.has_field x 0
      let data_get x =
        RA_.get_blob ~default:"" x 0
      let list_get x =
        RA_.get_pointer x 0
      let enum_get x =
        RA_.get_uint16 ~default:0 x 2
      let struct_get x =
        RA_.get_pointer x 0
      let interface_get x = ()
      let any_pointer_get x =
        RA_.get_pointer x 0
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
        match RA_.get_uint16 ~default:0 x 0 with
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
        RA_.get_uint64 ~default:Uint64.zero x 0
      let id_get_int_exn x =
        Capnp.Runtime.Util.int_of_uint64_exn (id_get x)
      let has_value x = 
        (RA_.has_field x 0)
      let value_get x = 
        (RA_.get_struct x 0)
      let of_message x = RA_.get_root_struct (RA_.Message.readonly x)
      let of_builder x = Some (RA_.StructStorage.readonly x)
    end
    module Method = struct
      type t = reader_t_Method_10736806783679155584
      type builder_t = builder_t_Method_10736806783679155584
      let has_name x =
        RA_.has_field x 0
      let name_get x =
        RA_.get_text ~default:"" x 0
      let code_order_get x =
        RA_.get_uint16 ~default:0 x 0
      let param_struct_type_get x =
        RA_.get_uint64 ~default:Uint64.zero x 8
      let param_struct_type_get_int_exn x =
        Capnp.Runtime.Util.int_of_uint64_exn (param_struct_type_get x)
      let result_struct_type_get x =
        RA_.get_uint64 ~default:Uint64.zero x 16
      let result_struct_type_get_int_exn x =
        Capnp.Runtime.Util.int_of_uint64_exn (result_struct_type_get x)
      let has_annotations x = 
        (RA_.has_field x 1)
      let annotations_get x = 
        (RA_.get_struct_list x 1)
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
        RA_.has_field x 0
      let name_get x =
        RA_.get_text ~default:"" x 0
      let code_order_get x =
        RA_.get_uint16 ~default:0 x 0
      let has_annotations x = 
        (RA_.has_field x 1)
      let annotations_get x = 
        (RA_.get_struct_list x 1)
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
          RA_.get_uint16 ~default:0 x 12
        type unnamed_union_t =
          | Implicit
          | Explicit of int
          | Undefined of int
        let get x =
          match RA_.get_uint16 ~default:0 x 10 with
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
          RA_.get_uint64 ~default:Uint64.zero x 16
        let type_id_get_int_exn x =
          Capnp.Runtime.Util.int_of_uint64_exn (type_id_get x)
        let of_message x = RA_.get_root_struct (RA_.Message.readonly x)
        let of_builder x = Some (RA_.StructStorage.readonly x)
      end
      module Slot = struct
        type t = reader_t_Slot_14133145859926553711
        type builder_t = builder_t_Slot_14133145859926553711
        let offset_get x =
          RA_.get_uint32 ~default:Uint32.zero x 4
        let offset_get_int_exn x =
          Capnp.Runtime.Util.int_of_uint32_exn (offset_get x)
        let has_type x = 
          (RA_.has_field x 2)
        let type_get x = 
          (RA_.get_struct x 2)
        let has_default_value x = 
          (RA_.has_field x 3)
        let default_value_get x = 
          (RA_.get_struct x 3)
        let had_explicit_default_get x =
          RA_.get_bit ~default:false x ~byte_ofs:16 ~bit_ofs:0
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
        match RA_.get_uint16 ~default:0 x 8 with
        | 0 -> Slot (slot_get x)
        | 1 -> Group (group_get x)
        | v -> Undefined v
      let has_name x =
        RA_.has_field x 0
      let name_get x =
        RA_.get_text ~default:"" x 0
      let code_order_get x =
        RA_.get_uint16 ~default:0 x 0
      let has_annotations x = 
        (RA_.has_field x 1)
      let annotations_get x = 
        (RA_.get_struct_list x 1)
      let annotations_get_list x =
        Capnp.Array.to_list (annotations_get x)
      let annotations_get_array x =
        Capnp.Array.to_array (annotations_get x)
      let discriminant_value_get x =
        RA_.get_uint16 ~default:65535 x 2
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
          RA_.get_uint16 ~default:0 x 14
        let pointer_count_get x =
          RA_.get_uint16 ~default:0 x 24
        let preferred_list_encoding_get x =
          let discr = RA_.get_uint16 ~default:0 x 26 in
          ElementSize_15102134695616452902.decode discr
        let is_group_get x =
          RA_.get_bit ~default:false x ~byte_ofs:28 ~bit_ofs:0
        let discriminant_count_get x =
          RA_.get_uint16 ~default:0 x 30
        let discriminant_offset_get x =
          RA_.get_uint32 ~default:Uint32.zero x 32
        let discriminant_offset_get_int_exn x =
          Capnp.Runtime.Util.int_of_uint32_exn (discriminant_offset_get x)
        let has_fields x = 
          (RA_.has_field x 3)
        let fields_get x = 
          (RA_.get_struct_list x 3)
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
          (RA_.has_field x 3)
        let enumerants_get x = 
          (RA_.get_struct_list x 3)
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
          (RA_.has_field x 3)
        let type_get x = 
          (RA_.get_struct x 3)
        let targets_file_get x =
          RA_.get_bit ~default:false x ~byte_ofs:14 ~bit_ofs:0
        let targets_const_get x =
          RA_.get_bit ~default:false x ~byte_ofs:14 ~bit_ofs:1
        let targets_enum_get x =
          RA_.get_bit ~default:false x ~byte_ofs:14 ~bit_ofs:2
        let targets_enumerant_get x =
          RA_.get_bit ~default:false x ~byte_ofs:14 ~bit_ofs:3
        let targets_struct_get x =
          RA_.get_bit ~default:false x ~byte_ofs:14 ~bit_ofs:4
        let targets_field_get x =
          RA_.get_bit ~default:false x ~byte_ofs:14 ~bit_ofs:5
        let targets_union_get x =
          RA_.get_bit ~default:false x ~byte_ofs:14 ~bit_ofs:6
        let targets_group_get x =
          RA_.get_bit ~default:false x ~byte_ofs:14 ~bit_ofs:7
        let targets_interface_get x =
          RA_.get_bit ~default:false x ~byte_ofs:15 ~bit_ofs:0
        let targets_method_get x =
          RA_.get_bit ~default:false x ~byte_ofs:15 ~bit_ofs:1
        let targets_param_get x =
          RA_.get_bit ~default:false x ~byte_ofs:15 ~bit_ofs:2
        let targets_annotation_get x =
          RA_.get_bit ~default:false x ~byte_ofs:15 ~bit_ofs:3
        let of_message x = RA_.get_root_struct (RA_.Message.readonly x)
        let of_builder x = Some (RA_.StructStorage.readonly x)
      end
      module Const = struct
        type t = reader_t_Const_12793219851699983392
        type builder_t = builder_t_Const_12793219851699983392
        let has_type x = 
          (RA_.has_field x 3)
        let type_get x = 
          (RA_.get_struct x 3)
        let has_value x = 
          (RA_.has_field x 4)
        let value_get x = 
          (RA_.get_struct x 4)
        let of_message x = RA_.get_root_struct (RA_.Message.readonly x)
        let of_builder x = Some (RA_.StructStorage.readonly x)
      end
      module Interface = struct
        type t = reader_t_Interface_16728431493453586831
        type builder_t = builder_t_Interface_16728431493453586831
        let has_methods x = 
          (RA_.has_field x 3)
        let methods_get x = 
          (RA_.get_struct_list x 3)
        let methods_get_list x =
          Capnp.Array.to_list (methods_get x)
        let methods_get_array x =
          Capnp.Array.to_array (methods_get x)
        let has_extends x = 
          (RA_.has_field x 4)
        let extends_get x =
          RA_.get_uint64_list x 4
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
          RA_.has_field x 0
        let name_get x =
          RA_.get_text ~default:"" x 0
        let id_get x =
          RA_.get_uint64 ~default:Uint64.zero x 0
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
        match RA_.get_uint16 ~default:0 x 12 with
        | 0 -> File
        | 1 -> Struct (struct_get x)
        | 2 -> Enum (enum_get x)
        | 3 -> Interface (interface_get x)
        | 4 -> Const (const_get x)
        | 5 -> Annotation (annotation_get x)
        | v -> Undefined v
      let id_get x =
        RA_.get_uint64 ~default:Uint64.zero x 0
      let id_get_int_exn x =
        Capnp.Runtime.Util.int_of_uint64_exn (id_get x)
      let has_display_name x =
        RA_.has_field x 0
      let display_name_get x =
        RA_.get_text ~default:"" x 0
      let display_name_prefix_length_get x =
        RA_.get_uint32 ~default:Uint32.zero x 8
      let display_name_prefix_length_get_int_exn x =
        Capnp.Runtime.Util.int_of_uint32_exn (display_name_prefix_length_get x)
      let scope_id_get x =
        RA_.get_uint64 ~default:Uint64.zero x 16
      let scope_id_get_int_exn x =
        Capnp.Runtime.Util.int_of_uint64_exn (scope_id_get x)
      let has_nested_nodes x = 
        (RA_.has_field x 1)
      let nested_nodes_get x = 
        (RA_.get_struct_list x 1)
      let nested_nodes_get_list x =
        Capnp.Array.to_list (nested_nodes_get x)
      let nested_nodes_get_array x =
        Capnp.Array.to_array (nested_nodes_get x)
      let has_annotations x = 
        (RA_.has_field x 2)
      let annotations_get x = 
        (RA_.get_struct_list x 2)
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
            RA_.get_uint64 ~default:Uint64.zero x 0
          let id_get_int_exn x =
            Capnp.Runtime.Util.int_of_uint64_exn (id_get x)
          let has_name x =
            RA_.has_field x 0
          let name_get x =
            RA_.get_text ~default:"" x 0
          let of_message x = RA_.get_root_struct (RA_.Message.readonly x)
          let of_builder x = Some (RA_.StructStorage.readonly x)
        end
        let id_get x =
          RA_.get_uint64 ~default:Uint64.zero x 0
        let id_get_int_exn x =
          Capnp.Runtime.Util.int_of_uint64_exn (id_get x)
        let has_filename x =
          RA_.has_field x 0
        let filename_get x =
          RA_.get_text ~default:"" x 0
        let has_imports x = 
          (RA_.has_field x 1)
        let imports_get x = 
          (RA_.get_struct_list x 1)
        let imports_get_list x =
          Capnp.Array.to_list (imports_get x)
        let imports_get_array x =
          Capnp.Array.to_array (imports_get x)
        let of_message x = RA_.get_root_struct (RA_.Message.readonly x)
        let of_builder x = Some (RA_.StructStorage.readonly x)
      end
      let has_nodes x = 
        (RA_.has_field x 0)
      let nodes_get x = 
        (RA_.get_struct_list x 0)
      let nodes_get_list x =
        Capnp.Array.to_list (nodes_get x)
      let nodes_get_array x =
        Capnp.Array.to_array (nodes_get x)
      let has_requested_files x = 
        (RA_.has_field x 1)
      let requested_files_get x = 
        (RA_.get_struct_list x 1)
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
          BA_.get_uint64 ~default:Uint64.zero x 8
        let type_id_get_int_exn x =
          Capnp.Runtime.Util.int_of_uint64_exn (type_id_get x)
        let type_id_set x v =
          BA_.set_uint64 ~default:Uint64.zero x 8 v
        let type_id_set_int_exn x v = type_id_set x (Capnp.Runtime.Util.uint64_of_int_exn v)
        let of_message x = BA_.get_root_struct ~data_words:2 ~pointer_words:1 x
        let to_message x = x.BA_.NM.StructStorage.data.MessageWrapper.Slice.msg
        let to_reader x = Some (RA_.StructStorage.readonly x)
        let init_root ?message_size () =
          BA_.alloc_root_struct ?message_size ~data_words:2 ~pointer_words:1 ()
      end
      module Interface = struct
        type t = builder_t_Interface_17116997365232503999
        type reader_t = reader_t_Interface_17116997365232503999
        let type_id_get x =
          BA_.get_uint64 ~default:Uint64.zero x 8
        let type_id_get_int_exn x =
          Capnp.Runtime.Util.int_of_uint64_exn (type_id_get x)
        let type_id_set x v =
          BA_.set_uint64 ~default:Uint64.zero x 8 v
        let type_id_set_int_exn x v = type_id_set x (Capnp.Runtime.Util.uint64_of_int_exn v)
        let of_message x = BA_.get_root_struct ~data_words:2 ~pointer_words:1 x
        let to_message x = x.BA_.NM.StructStorage.data.MessageWrapper.Slice.msg
        let to_reader x = Some (RA_.StructStorage.readonly x)
        let init_root ?message_size () =
          BA_.alloc_root_struct ?message_size ~data_words:2 ~pointer_words:1 ()
      end
      module List = struct
        type t = builder_t_List_9792858745991129751
        type reader_t = reader_t_List_9792858745991129751
        let has_element_type x = 
          (BA_.has_field x 0)
        let element_type_get x = 
          (BA_.get_struct ~data_words:2 ~pointer_words:1 x 0)
        let element_type_set_reader x v = 
          (BA_.set_struct ~data_words:2 ~pointer_words:1 x 0 (v))
        let element_type_set_builder x v = 
          (BA_.set_struct ~data_words:2 ~pointer_words:1 x 0 (Some (v)))
        let element_type_init x = 
          (BA_.init_struct ~data_words:2 ~pointer_words:1 x 0)
        let of_message x = BA_.get_root_struct ~data_words:2 ~pointer_words:1 x
        let to_message x = x.BA_.NM.StructStorage.data.MessageWrapper.Slice.msg
        let to_reader x = Some (RA_.StructStorage.readonly x)
        let init_root ?message_size () =
          BA_.alloc_root_struct ?message_size ~data_words:2 ~pointer_words:1 ()
      end
      module Struct = struct
        type t = builder_t_Struct_12410354185295152851
        type reader_t = reader_t_Struct_12410354185295152851
        let type_id_get x =
          BA_.get_uint64 ~default:Uint64.zero x 8
        let type_id_get_int_exn x =
          Capnp.Runtime.Util.int_of_uint64_exn (type_id_get x)
        let type_id_set x v =
          BA_.set_uint64 ~default:Uint64.zero x 8 v
        let type_id_set_int_exn x v = type_id_set x (Capnp.Runtime.Util.uint64_of_int_exn v)
        let of_message x = BA_.get_root_struct ~data_words:2 ~pointer_words:1 x
        let to_message x = x.BA_.NM.StructStorage.data.MessageWrapper.Slice.msg
        let to_reader x = Some (RA_.StructStorage.readonly x)
        let init_root ?message_size () =
          BA_.alloc_root_struct ?message_size ~data_words:2 ~pointer_words:1 ()
      end
      let void_get x = ()
      let void_set x =
        BA_.set_void ~discr:{BA_.Discr.value=0; BA_.Discr.byte_ofs=0} x
      let bool_get x = ()
      let bool_set x =
        BA_.set_void ~discr:{BA_.Discr.value=1; BA_.Discr.byte_ofs=0} x
      let int8_get x = ()
      let int8_set x =
        BA_.set_void ~discr:{BA_.Discr.value=2; BA_.Discr.byte_ofs=0} x
      let int16_get x = ()
      let int16_set x =
        BA_.set_void ~discr:{BA_.Discr.value=3; BA_.Discr.byte_ofs=0} x
      let int32_get x = ()
      let int32_set x =
        BA_.set_void ~discr:{BA_.Discr.value=4; BA_.Discr.byte_ofs=0} x
      let int64_get x = ()
      let int64_set x =
        BA_.set_void ~discr:{BA_.Discr.value=5; BA_.Discr.byte_ofs=0} x
      let uint8_get x = ()
      let uint8_set x =
        BA_.set_void ~discr:{BA_.Discr.value=6; BA_.Discr.byte_ofs=0} x
      let uint16_get x = ()
      let uint16_set x =
        BA_.set_void ~discr:{BA_.Discr.value=7; BA_.Discr.byte_ofs=0} x
      let uint32_get x = ()
      let uint32_set x =
        BA_.set_void ~discr:{BA_.Discr.value=8; BA_.Discr.byte_ofs=0} x
      let uint64_get x = ()
      let uint64_set x =
        BA_.set_void ~discr:{BA_.Discr.value=9; BA_.Discr.byte_ofs=0} x
      let float32_get x = ()
      let float32_set x =
        BA_.set_void ~discr:{BA_.Discr.value=10; BA_.Discr.byte_ofs=0} x
      let float64_get x = ()
      let float64_set x =
        BA_.set_void ~discr:{BA_.Discr.value=11; BA_.Discr.byte_ofs=0} x
      let text_get x = ()
      let text_set x =
        BA_.set_void ~discr:{BA_.Discr.value=12; BA_.Discr.byte_ofs=0} x
      let data_get x = ()
      let data_set x =
        BA_.set_void ~discr:{BA_.Discr.value=13; BA_.Discr.byte_ofs=0} x
      let list_get x = x
      let list_init x =
        let data = x.BA_.NM.StructStorage.data in
        let pointers = x.BA_.NM.StructStorage.pointers in
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
        let data = x.BA_.NM.StructStorage.data in
        let pointers = x.BA_.NM.StructStorage.pointers in
        let () = ignore data in
        let () = ignore pointers in
        let () = BA_.set_opt_discriminant data
          (Some {BA_.Discr.value=15; BA_.Discr.byte_ofs=0})
        in
        let () = BA_.set_int64 ~default:0L x 8 0L in
        x
      let struct_get x = x
      let struct_init x =
        let data = x.BA_.NM.StructStorage.data in
        let pointers = x.BA_.NM.StructStorage.pointers in
        let () = ignore data in
        let () = ignore pointers in
        let () = BA_.set_opt_discriminant data
          (Some {BA_.Discr.value=16; BA_.Discr.byte_ofs=0})
        in
        let () = BA_.set_int64 ~default:0L x 8 0L in
        x
      let interface_get x = x
      let interface_init x =
        let data = x.BA_.NM.StructStorage.data in
        let pointers = x.BA_.NM.StructStorage.pointers in
        let () = ignore data in
        let () = ignore pointers in
        let () = BA_.set_opt_discriminant data
          (Some {BA_.Discr.value=17; BA_.Discr.byte_ofs=0})
        in
        let () = BA_.set_int64 ~default:0L x 8 0L in
        x
      let any_pointer_get x = ()
      let any_pointer_set x =
        BA_.set_void ~discr:{BA_.Discr.value=18; BA_.Discr.byte_ofs=0} x
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
        match BA_.get_uint16 ~default:0 x 0 with
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
      let to_message x = x.BA_.NM.StructStorage.data.MessageWrapper.Slice.msg
      let to_reader x = Some (RA_.StructStorage.readonly x)
      let init_root ?message_size () =
        BA_.alloc_root_struct ?message_size ~data_words:2 ~pointer_words:1 ()
    end
    module Value = struct
      type t = builder_t_Value_14853958794117909659
      type reader_t = reader_t_Value_14853958794117909659
      let void_get x = ()
      let void_set x =
        BA_.set_void ~discr:{BA_.Discr.value=0; BA_.Discr.byte_ofs=0} x
      let bool_get x =
        BA_.get_bit ~default:false x ~byte_ofs:2 ~bit_ofs:0
      let bool_set x v =
        BA_.set_bit ~discr:{BA_.Discr.value=1; BA_.Discr.byte_ofs=0} ~default:false x ~byte_ofs:2 ~bit_ofs:0 v
      let int8_get x =
        BA_.get_int8 ~default:(0) x 2
      let int8_set_exn x v =
        BA_.set_int8 ~discr:{BA_.Discr.value=2; BA_.Discr.byte_ofs=0} ~default:(0) x 2 v
      let int16_get x =
        BA_.get_int16 ~default:(0) x 2
      let int16_set_exn x v =
        BA_.set_int16 ~discr:{BA_.Discr.value=3; BA_.Discr.byte_ofs=0} ~default:(0) x 2 v
      let int32_get x =
        BA_.get_int32 ~default:(0l) x 4
      let int32_get_int_exn x =
        Capnp.Runtime.Util.int_of_int32_exn (int32_get x)
      let int32_set x v =
        BA_.set_int32 ~discr:{BA_.Discr.value=4; BA_.Discr.byte_ofs=0} ~default:(0l) x 4 v
      let int32_set_int_exn x v = int32_set x (Capnp.Runtime.Util.int32_of_int_exn v)
      let int64_get x =
        BA_.get_int64 ~default:(0L) x 8
      let int64_get_int_exn x =
        Capnp.Runtime.Util.int_of_int64_exn (int64_get x)
      let int64_set x v =
        BA_.set_int64 ~discr:{BA_.Discr.value=5; BA_.Discr.byte_ofs=0} ~default:(0L) x 8 v
      let int64_set_int x v = int64_set x (Int64.of_int v)
      let uint8_get x =
        BA_.get_uint8 ~default:0 x 2
      let uint8_set_exn x v =
        BA_.set_uint8 ~discr:{BA_.Discr.value=6; BA_.Discr.byte_ofs=0} ~default:0 x 2 v
      let uint16_get x =
        BA_.get_uint16 ~default:0 x 2
      let uint16_set_exn x v =
        BA_.set_uint16 ~discr:{BA_.Discr.value=7; BA_.Discr.byte_ofs=0} ~default:0 x 2 v
      let uint32_get x =
        BA_.get_uint32 ~default:Uint32.zero x 4
      let uint32_get_int_exn x =
        Capnp.Runtime.Util.int_of_uint32_exn (uint32_get x)
      let uint32_set x v =
        BA_.set_uint32 ~discr:{BA_.Discr.value=8; BA_.Discr.byte_ofs=0} ~default:Uint32.zero x 4 v
      let uint32_set_int_exn x v = uint32_set x (Capnp.Runtime.Util.uint32_of_int_exn v)
      let uint64_get x =
        BA_.get_uint64 ~default:Uint64.zero x 8
      let uint64_get_int_exn x =
        Capnp.Runtime.Util.int_of_uint64_exn (uint64_get x)
      let uint64_set x v =
        BA_.set_uint64 ~discr:{BA_.Discr.value=9; BA_.Discr.byte_ofs=0} ~default:Uint64.zero x 8 v
      let uint64_set_int_exn x v = uint64_set x (Capnp.Runtime.Util.uint64_of_int_exn v)
      let float32_get x =
        BA_.get_float32 ~default_bits:(0l) x 4
      let float32_set x v =
        BA_.set_float32 ~discr:{BA_.Discr.value=10; BA_.Discr.byte_ofs=0} ~default_bits:(0l) x 4 v
      let float64_get x =
        BA_.get_float64 ~default_bits:(0L) x 8
      let float64_set x v =
        BA_.set_float64 ~discr:{BA_.Discr.value=11; BA_.Discr.byte_ofs=0} ~default_bits:(0L) x 8 v
      let has_text x =
        BA_.has_field x 0
      let text_get x =
        BA_.get_text ~default:"" x 0
      let text_set x v =
        BA_.set_text ~discr:{BA_.Discr.value=12; BA_.Discr.byte_ofs=0} x 0 v
      let has_data x =
        BA_.has_field x 0
      let data_get x =
        BA_.get_blob ~default:"" x 0
      let data_set x v =
        BA_.set_blob ~discr:{BA_.Discr.value=13; BA_.Discr.byte_ofs=0} x 0 v
      let list_get x =
        BA_.get_pointer x 0
      let list_set x v =
        BA_.set_pointer ~discr:{BA_.Discr.value=14; BA_.Discr.byte_ofs=0} x 0 v
      let enum_get x =
        BA_.get_uint16 ~default:0 x 2
      let enum_set_exn x v =
        BA_.set_uint16 ~discr:{BA_.Discr.value=15; BA_.Discr.byte_ofs=0} ~default:0 x 2 v
      let struct_get x =
        BA_.get_pointer x 0
      let struct_set x v =
        BA_.set_pointer ~discr:{BA_.Discr.value=16; BA_.Discr.byte_ofs=0} x 0 v
      let interface_get x = ()
      let interface_set x =
        BA_.set_void ~discr:{BA_.Discr.value=17; BA_.Discr.byte_ofs=0} x
      let any_pointer_get x =
        BA_.get_pointer x 0
      let any_pointer_set x v =
        BA_.set_pointer ~discr:{BA_.Discr.value=18; BA_.Discr.byte_ofs=0} x 0 v
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
        match BA_.get_uint16 ~default:0 x 0 with
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
      let to_message x = x.BA_.NM.StructStorage.data.MessageWrapper.Slice.msg
      let to_reader x = Some (RA_.StructStorage.readonly x)
      let init_root ?message_size () =
        BA_.alloc_root_struct ?message_size ~data_words:2 ~pointer_words:1 ()
    end
    module Annotation = struct
      type t = builder_t_Annotation_17422339044421236034
      type reader_t = reader_t_Annotation_17422339044421236034
      let id_get x =
        BA_.get_uint64 ~default:Uint64.zero x 0
      let id_get_int_exn x =
        Capnp.Runtime.Util.int_of_uint64_exn (id_get x)
      let id_set x v =
        BA_.set_uint64 ~default:Uint64.zero x 0 v
      let id_set_int_exn x v = id_set x (Capnp.Runtime.Util.uint64_of_int_exn v)
      let has_value x = 
        (BA_.has_field x 0)
      let value_get x = 
        (BA_.get_struct ~data_words:2 ~pointer_words:1 x 0)
      let value_set_reader x v = 
        (BA_.set_struct ~data_words:2 ~pointer_words:1 x 0 (v))
      let value_set_builder x v = 
        (BA_.set_struct ~data_words:2 ~pointer_words:1 x 0 (Some (v)))
      let value_init x = 
        (BA_.init_struct ~data_words:2 ~pointer_words:1 x 0)
      let of_message x = BA_.get_root_struct ~data_words:1 ~pointer_words:1 x
      let to_message x = x.BA_.NM.StructStorage.data.MessageWrapper.Slice.msg
      let to_reader x = Some (RA_.StructStorage.readonly x)
      let init_root ?message_size () =
        BA_.alloc_root_struct ?message_size ~data_words:1 ~pointer_words:1 ()
    end
    module Method = struct
      type t = builder_t_Method_10736806783679155584
      type reader_t = reader_t_Method_10736806783679155584
      let has_name x =
        BA_.has_field x 0
      let name_get x =
        BA_.get_text ~default:"" x 0
      let name_set x v =
        BA_.set_text x 0 v
      let code_order_get x =
        BA_.get_uint16 ~default:0 x 0
      let code_order_set_exn x v =
        BA_.set_uint16 ~default:0 x 0 v
      let param_struct_type_get x =
        BA_.get_uint64 ~default:Uint64.zero x 8
      let param_struct_type_get_int_exn x =
        Capnp.Runtime.Util.int_of_uint64_exn (param_struct_type_get x)
      let param_struct_type_set x v =
        BA_.set_uint64 ~default:Uint64.zero x 8 v
      let param_struct_type_set_int_exn x v = param_struct_type_set x (Capnp.Runtime.Util.uint64_of_int_exn v)
      let result_struct_type_get x =
        BA_.get_uint64 ~default:Uint64.zero x 16
      let result_struct_type_get_int_exn x =
        Capnp.Runtime.Util.int_of_uint64_exn (result_struct_type_get x)
      let result_struct_type_set x v =
        BA_.set_uint64 ~default:Uint64.zero x 16 v
      let result_struct_type_set_int_exn x v = result_struct_type_set x (Capnp.Runtime.Util.uint64_of_int_exn v)
      let has_annotations x = 
        (BA_.has_field x 1)
      let annotations_get x = 
        (BA_.get_struct_list ~data_words:1 ~pointer_words:1 x 1)
      let annotations_get_list x =
        Capnp.Array.to_list (annotations_get x)
      let annotations_get_array x =
        Capnp.Array.to_array (annotations_get x)
      let annotations_set x v = 
        (BA_.set_struct_list ~data_words:1 ~pointer_words:1 x 1 (v))
      let annotations_init x n = 
        (BA_.init_struct_list ~data_words:1 ~pointer_words:1 x 1 n)
      let annotations_set_list x v =
        let builder = annotations_init x (List.length v) in
        let () = List.iteri (fun i a -> Capnp.Array.set builder i a) v in
        builder
      let annotations_set_array x v =
        let builder = annotations_init x (Array.length v) in
        let () = Array.iteri (fun i a -> Capnp.Array.set builder i a) v in
        builder
      let of_message x = BA_.get_root_struct ~data_words:3 ~pointer_words:2 x
      let to_message x = x.BA_.NM.StructStorage.data.MessageWrapper.Slice.msg
      let to_reader x = Some (RA_.StructStorage.readonly x)
      let init_root ?message_size () =
        BA_.alloc_root_struct ?message_size ~data_words:3 ~pointer_words:2 ()
    end
    module Enumerant = struct
      type t = builder_t_Enumerant_10919677598968879693
      type reader_t = reader_t_Enumerant_10919677598968879693
      let has_name x =
        BA_.has_field x 0
      let name_get x =
        BA_.get_text ~default:"" x 0
      let name_set x v =
        BA_.set_text x 0 v
      let code_order_get x =
        BA_.get_uint16 ~default:0 x 0
      let code_order_set_exn x v =
        BA_.set_uint16 ~default:0 x 0 v
      let has_annotations x = 
        (BA_.has_field x 1)
      let annotations_get x = 
        (BA_.get_struct_list ~data_words:1 ~pointer_words:1 x 1)
      let annotations_get_list x =
        Capnp.Array.to_list (annotations_get x)
      let annotations_get_array x =
        Capnp.Array.to_array (annotations_get x)
      let annotations_set x v = 
        (BA_.set_struct_list ~data_words:1 ~pointer_words:1 x 1 (v))
      let annotations_init x n = 
        (BA_.init_struct_list ~data_words:1 ~pointer_words:1 x 1 n)
      let annotations_set_list x v =
        let builder = annotations_init x (List.length v) in
        let () = List.iteri (fun i a -> Capnp.Array.set builder i a) v in
        builder
      let annotations_set_array x v =
        let builder = annotations_init x (Array.length v) in
        let () = Array.iteri (fun i a -> Capnp.Array.set builder i a) v in
        builder
      let of_message x = BA_.get_root_struct ~data_words:1 ~pointer_words:2 x
      let to_message x = x.BA_.NM.StructStorage.data.MessageWrapper.Slice.msg
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
          BA_.set_void ~discr:{BA_.Discr.value=0; BA_.Discr.byte_ofs=10} x
        let explicit_get x =
          BA_.get_uint16 ~default:0 x 12
        let explicit_set_exn x v =
          BA_.set_uint16 ~discr:{BA_.Discr.value=1; BA_.Discr.byte_ofs=10} ~default:0 x 12 v
        type unnamed_union_t =
          | Implicit
          | Explicit of int
          | Undefined of int
        let get x =
          match BA_.get_uint16 ~default:0 x 10 with
          | 0 -> Implicit
          | 1 -> Explicit (explicit_get x)
          | v -> Undefined v
        let of_message x = BA_.get_root_struct ~data_words:3 ~pointer_words:4 x
        let to_message x = x.BA_.NM.StructStorage.data.MessageWrapper.Slice.msg
        let to_reader x = Some (RA_.StructStorage.readonly x)
        let init_root ?message_size () =
          BA_.alloc_root_struct ?message_size ~data_words:3 ~pointer_words:4 ()
      end
      module Group = struct
        type t = builder_t_Group_14626792032033250577
        type reader_t = reader_t_Group_14626792032033250577
        let type_id_get x =
          BA_.get_uint64 ~default:Uint64.zero x 16
        let type_id_get_int_exn x =
          Capnp.Runtime.Util.int_of_uint64_exn (type_id_get x)
        let type_id_set x v =
          BA_.set_uint64 ~default:Uint64.zero x 16 v
        let type_id_set_int_exn x v = type_id_set x (Capnp.Runtime.Util.uint64_of_int_exn v)
        let of_message x = BA_.get_root_struct ~data_words:3 ~pointer_words:4 x
        let to_message x = x.BA_.NM.StructStorage.data.MessageWrapper.Slice.msg
        let to_reader x = Some (RA_.StructStorage.readonly x)
        let init_root ?message_size () =
          BA_.alloc_root_struct ?message_size ~data_words:3 ~pointer_words:4 ()
      end
      module Slot = struct
        type t = builder_t_Slot_14133145859926553711
        type reader_t = reader_t_Slot_14133145859926553711
        let offset_get x =
          BA_.get_uint32 ~default:Uint32.zero x 4
        let offset_get_int_exn x =
          Capnp.Runtime.Util.int_of_uint32_exn (offset_get x)
        let offset_set x v =
          BA_.set_uint32 ~default:Uint32.zero x 4 v
        let offset_set_int_exn x v = offset_set x (Capnp.Runtime.Util.uint32_of_int_exn v)
        let has_type x = 
          (BA_.has_field x 2)
        let type_get x = 
          (BA_.get_struct ~data_words:2 ~pointer_words:1 x 2)
        let type_set_reader x v = 
          (BA_.set_struct ~data_words:2 ~pointer_words:1 x 2 (v))
        let type_set_builder x v = 
          (BA_.set_struct ~data_words:2 ~pointer_words:1 x 2 (Some (v)))
        let type_init x = 
          (BA_.init_struct ~data_words:2 ~pointer_words:1 x 2)
        let has_default_value x = 
          (BA_.has_field x 3)
        let default_value_get x = 
          (BA_.get_struct ~data_words:2 ~pointer_words:1 x 3)
        let default_value_set_reader x v = 
          (BA_.set_struct ~data_words:2 ~pointer_words:1 x 3 (v))
        let default_value_set_builder x v = 
          (BA_.set_struct ~data_words:2 ~pointer_words:1 x 3 (Some (v)))
        let default_value_init x = 
          (BA_.init_struct ~data_words:2 ~pointer_words:1 x 3)
        let had_explicit_default_get x =
          BA_.get_bit ~default:false x ~byte_ofs:16 ~bit_ofs:0
        let had_explicit_default_set x v =
          BA_.set_bit ~default:false x ~byte_ofs:16 ~bit_ofs:0 v
        let of_message x = BA_.get_root_struct ~data_words:3 ~pointer_words:4 x
        let to_message x = x.BA_.NM.StructStorage.data.MessageWrapper.Slice.msg
        let to_reader x = Some (RA_.StructStorage.readonly x)
        let init_root ?message_size () =
          BA_.alloc_root_struct ?message_size ~data_words:3 ~pointer_words:4 ()
      end
      let slot_get x = x
      let slot_init x =
        let data = x.BA_.NM.StructStorage.data in
        let pointers = x.BA_.NM.StructStorage.pointers in
        let () = ignore data in
        let () = ignore pointers in
        let () = BA_.set_opt_discriminant data
          (Some {BA_.Discr.value=0; BA_.Discr.byte_ofs=8})
        in
        let () = BA_.set_int32 ~default:0l x 4 0l in
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
        let () = BA_.set_bit ~default:false x ~byte_ofs:16 ~bit_ofs:0 false in
        x
      let group_get x = x
      let group_init x =
        let data = x.BA_.NM.StructStorage.data in
        let pointers = x.BA_.NM.StructStorage.pointers in
        let () = ignore data in
        let () = ignore pointers in
        let () = BA_.set_opt_discriminant data
          (Some {BA_.Discr.value=1; BA_.Discr.byte_ofs=8})
        in
        let () = BA_.set_int64 ~default:0L x 16 0L in
        x
      type unnamed_union_t =
        | Slot of Slot.t
        | Group of Group.t
        | Undefined of int
      let get x =
        match BA_.get_uint16 ~default:0 x 8 with
        | 0 -> Slot (slot_get x)
        | 1 -> Group (group_get x)
        | v -> Undefined v
      let has_name x =
        BA_.has_field x 0
      let name_get x =
        BA_.get_text ~default:"" x 0
      let name_set x v =
        BA_.set_text x 0 v
      let code_order_get x =
        BA_.get_uint16 ~default:0 x 0
      let code_order_set_exn x v =
        BA_.set_uint16 ~default:0 x 0 v
      let has_annotations x = 
        (BA_.has_field x 1)
      let annotations_get x = 
        (BA_.get_struct_list ~data_words:1 ~pointer_words:1 x 1)
      let annotations_get_list x =
        Capnp.Array.to_list (annotations_get x)
      let annotations_get_array x =
        Capnp.Array.to_array (annotations_get x)
      let annotations_set x v = 
        (BA_.set_struct_list ~data_words:1 ~pointer_words:1 x 1 (v))
      let annotations_init x n = 
        (BA_.init_struct_list ~data_words:1 ~pointer_words:1 x 1 n)
      let annotations_set_list x v =
        let builder = annotations_init x (List.length v) in
        let () = List.iteri (fun i a -> Capnp.Array.set builder i a) v in
        builder
      let annotations_set_array x v =
        let builder = annotations_init x (Array.length v) in
        let () = Array.iteri (fun i a -> Capnp.Array.set builder i a) v in
        builder
      let discriminant_value_get x =
        BA_.get_uint16 ~default:65535 x 2
      let discriminant_value_set_exn x v =
        BA_.set_uint16 ~default:65535 x 2 v
      let ordinal_get x = x
      let ordinal_init x =
        let data = x.BA_.NM.StructStorage.data in
        let pointers = x.BA_.NM.StructStorage.pointers in
        let () = ignore data in
        let () = ignore pointers in
        let () = BA_.set_int16 ~default:0 x 10 0 in
        let () = BA_.set_int16 ~default:0 x 12 0 in
        x
      let of_message x = BA_.get_root_struct ~data_words:3 ~pointer_words:4 x
      let to_message x = x.BA_.NM.StructStorage.data.MessageWrapper.Slice.msg
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
          BA_.get_uint16 ~default:0 x 14
        let data_word_count_set_exn x v =
          BA_.set_uint16 ~default:0 x 14 v
        let pointer_count_get x =
          BA_.get_uint16 ~default:0 x 24
        let pointer_count_set_exn x v =
          BA_.set_uint16 ~default:0 x 24 v
        let preferred_list_encoding_get x =
          let discr = BA_.get_uint16 ~default:0 x 26 in
          ElementSize_15102134695616452902.decode discr
        let preferred_list_encoding_set x e =
          BA_.set_uint16 ~default:0 x 26 (ElementSize_15102134695616452902.encode_safe e)
        let preferred_list_encoding_set_unsafe x e =
          BA_.set_uint16 ~default:0 x 26 (ElementSize_15102134695616452902.encode_unsafe e)
        let is_group_get x =
          BA_.get_bit ~default:false x ~byte_ofs:28 ~bit_ofs:0
        let is_group_set x v =
          BA_.set_bit ~default:false x ~byte_ofs:28 ~bit_ofs:0 v
        let discriminant_count_get x =
          BA_.get_uint16 ~default:0 x 30
        let discriminant_count_set_exn x v =
          BA_.set_uint16 ~default:0 x 30 v
        let discriminant_offset_get x =
          BA_.get_uint32 ~default:Uint32.zero x 32
        let discriminant_offset_get_int_exn x =
          Capnp.Runtime.Util.int_of_uint32_exn (discriminant_offset_get x)
        let discriminant_offset_set x v =
          BA_.set_uint32 ~default:Uint32.zero x 32 v
        let discriminant_offset_set_int_exn x v = discriminant_offset_set x (Capnp.Runtime.Util.uint32_of_int_exn v)
        let has_fields x = 
          (BA_.has_field x 3)
        let fields_get x = 
          (BA_.get_struct_list ~data_words:3 ~pointer_words:4 x 3)
        let fields_get_list x =
          Capnp.Array.to_list (fields_get x)
        let fields_get_array x =
          Capnp.Array.to_array (fields_get x)
        let fields_set x v = 
          (BA_.set_struct_list ~data_words:3 ~pointer_words:4 x 3 (v))
        let fields_init x n = 
          (BA_.init_struct_list ~data_words:3 ~pointer_words:4 x 3 n)
        let fields_set_list x v =
          let builder = fields_init x (List.length v) in
          let () = List.iteri (fun i a -> Capnp.Array.set builder i a) v in
          builder
        let fields_set_array x v =
          let builder = fields_init x (Array.length v) in
          let () = Array.iteri (fun i a -> Capnp.Array.set builder i a) v in
          builder
        let of_message x = BA_.get_root_struct ~data_words:5 ~pointer_words:5 x
        let to_message x = x.BA_.NM.StructStorage.data.MessageWrapper.Slice.msg
        let to_reader x = Some (RA_.StructStorage.readonly x)
        let init_root ?message_size () =
          BA_.alloc_root_struct ?message_size ~data_words:5 ~pointer_words:5 ()
      end
      module Enum = struct
        type t = builder_t_Enum_13063450714778629528
        type reader_t = reader_t_Enum_13063450714778629528
        let has_enumerants x = 
          (BA_.has_field x 3)
        let enumerants_get x = 
          (BA_.get_struct_list ~data_words:1 ~pointer_words:2 x 3)
        let enumerants_get_list x =
          Capnp.Array.to_list (enumerants_get x)
        let enumerants_get_array x =
          Capnp.Array.to_array (enumerants_get x)
        let enumerants_set x v = 
          (BA_.set_struct_list ~data_words:1 ~pointer_words:2 x 3 (v))
        let enumerants_init x n = 
          (BA_.init_struct_list ~data_words:1 ~pointer_words:2 x 3 n)
        let enumerants_set_list x v =
          let builder = enumerants_init x (List.length v) in
          let () = List.iteri (fun i a -> Capnp.Array.set builder i a) v in
          builder
        let enumerants_set_array x v =
          let builder = enumerants_init x (Array.length v) in
          let () = Array.iteri (fun i a -> Capnp.Array.set builder i a) v in
          builder
        let of_message x = BA_.get_root_struct ~data_words:5 ~pointer_words:5 x
        let to_message x = x.BA_.NM.StructStorage.data.MessageWrapper.Slice.msg
        let to_reader x = Some (RA_.StructStorage.readonly x)
        let init_root ?message_size () =
          BA_.alloc_root_struct ?message_size ~data_words:5 ~pointer_words:5 ()
      end
      module Annotation = struct
        type t = builder_t_Annotation_17011813041836786320
        type reader_t = reader_t_Annotation_17011813041836786320
        let has_type x = 
          (BA_.has_field x 3)
        let type_get x = 
          (BA_.get_struct ~data_words:2 ~pointer_words:1 x 3)
        let type_set_reader x v = 
          (BA_.set_struct ~data_words:2 ~pointer_words:1 x 3 (v))
        let type_set_builder x v = 
          (BA_.set_struct ~data_words:2 ~pointer_words:1 x 3 (Some (v)))
        let type_init x = 
          (BA_.init_struct ~data_words:2 ~pointer_words:1 x 3)
        let targets_file_get x =
          BA_.get_bit ~default:false x ~byte_ofs:14 ~bit_ofs:0
        let targets_file_set x v =
          BA_.set_bit ~default:false x ~byte_ofs:14 ~bit_ofs:0 v
        let targets_const_get x =
          BA_.get_bit ~default:false x ~byte_ofs:14 ~bit_ofs:1
        let targets_const_set x v =
          BA_.set_bit ~default:false x ~byte_ofs:14 ~bit_ofs:1 v
        let targets_enum_get x =
          BA_.get_bit ~default:false x ~byte_ofs:14 ~bit_ofs:2
        let targets_enum_set x v =
          BA_.set_bit ~default:false x ~byte_ofs:14 ~bit_ofs:2 v
        let targets_enumerant_get x =
          BA_.get_bit ~default:false x ~byte_ofs:14 ~bit_ofs:3
        let targets_enumerant_set x v =
          BA_.set_bit ~default:false x ~byte_ofs:14 ~bit_ofs:3 v
        let targets_struct_get x =
          BA_.get_bit ~default:false x ~byte_ofs:14 ~bit_ofs:4
        let targets_struct_set x v =
          BA_.set_bit ~default:false x ~byte_ofs:14 ~bit_ofs:4 v
        let targets_field_get x =
          BA_.get_bit ~default:false x ~byte_ofs:14 ~bit_ofs:5
        let targets_field_set x v =
          BA_.set_bit ~default:false x ~byte_ofs:14 ~bit_ofs:5 v
        let targets_union_get x =
          BA_.get_bit ~default:false x ~byte_ofs:14 ~bit_ofs:6
        let targets_union_set x v =
          BA_.set_bit ~default:false x ~byte_ofs:14 ~bit_ofs:6 v
        let targets_group_get x =
          BA_.get_bit ~default:false x ~byte_ofs:14 ~bit_ofs:7
        let targets_group_set x v =
          BA_.set_bit ~default:false x ~byte_ofs:14 ~bit_ofs:7 v
        let targets_interface_get x =
          BA_.get_bit ~default:false x ~byte_ofs:15 ~bit_ofs:0
        let targets_interface_set x v =
          BA_.set_bit ~default:false x ~byte_ofs:15 ~bit_ofs:0 v
        let targets_method_get x =
          BA_.get_bit ~default:false x ~byte_ofs:15 ~bit_ofs:1
        let targets_method_set x v =
          BA_.set_bit ~default:false x ~byte_ofs:15 ~bit_ofs:1 v
        let targets_param_get x =
          BA_.get_bit ~default:false x ~byte_ofs:15 ~bit_ofs:2
        let targets_param_set x v =
          BA_.set_bit ~default:false x ~byte_ofs:15 ~bit_ofs:2 v
        let targets_annotation_get x =
          BA_.get_bit ~default:false x ~byte_ofs:15 ~bit_ofs:3
        let targets_annotation_set x v =
          BA_.set_bit ~default:false x ~byte_ofs:15 ~bit_ofs:3 v
        let of_message x = BA_.get_root_struct ~data_words:5 ~pointer_words:5 x
        let to_message x = x.BA_.NM.StructStorage.data.MessageWrapper.Slice.msg
        let to_reader x = Some (RA_.StructStorage.readonly x)
        let init_root ?message_size () =
          BA_.alloc_root_struct ?message_size ~data_words:5 ~pointer_words:5 ()
      end
      module Const = struct
        type t = builder_t_Const_12793219851699983392
        type reader_t = reader_t_Const_12793219851699983392
        let has_type x = 
          (BA_.has_field x 3)
        let type_get x = 
          (BA_.get_struct ~data_words:2 ~pointer_words:1 x 3)
        let type_set_reader x v = 
          (BA_.set_struct ~data_words:2 ~pointer_words:1 x 3 (v))
        let type_set_builder x v = 
          (BA_.set_struct ~data_words:2 ~pointer_words:1 x 3 (Some (v)))
        let type_init x = 
          (BA_.init_struct ~data_words:2 ~pointer_words:1 x 3)
        let has_value x = 
          (BA_.has_field x 4)
        let value_get x = 
          (BA_.get_struct ~data_words:2 ~pointer_words:1 x 4)
        let value_set_reader x v = 
          (BA_.set_struct ~data_words:2 ~pointer_words:1 x 4 (v))
        let value_set_builder x v = 
          (BA_.set_struct ~data_words:2 ~pointer_words:1 x 4 (Some (v)))
        let value_init x = 
          (BA_.init_struct ~data_words:2 ~pointer_words:1 x 4)
        let of_message x = BA_.get_root_struct ~data_words:5 ~pointer_words:5 x
        let to_message x = x.BA_.NM.StructStorage.data.MessageWrapper.Slice.msg
        let to_reader x = Some (RA_.StructStorage.readonly x)
        let init_root ?message_size () =
          BA_.alloc_root_struct ?message_size ~data_words:5 ~pointer_words:5 ()
      end
      module Interface = struct
        type t = builder_t_Interface_16728431493453586831
        type reader_t = reader_t_Interface_16728431493453586831
        let has_methods x = 
          (BA_.has_field x 3)
        let methods_get x = 
          (BA_.get_struct_list ~data_words:3 ~pointer_words:2 x 3)
        let methods_get_list x =
          Capnp.Array.to_list (methods_get x)
        let methods_get_array x =
          Capnp.Array.to_array (methods_get x)
        let methods_set x v = 
          (BA_.set_struct_list ~data_words:3 ~pointer_words:2 x 3 (v))
        let methods_init x n = 
          (BA_.init_struct_list ~data_words:3 ~pointer_words:2 x 3 n)
        let methods_set_list x v =
          let builder = methods_init x (List.length v) in
          let () = List.iteri (fun i a -> Capnp.Array.set builder i a) v in
          builder
        let methods_set_array x v =
          let builder = methods_init x (Array.length v) in
          let () = Array.iteri (fun i a -> Capnp.Array.set builder i a) v in
          builder
        let has_extends x = 
          (BA_.has_field x 4)
        let extends_get x =
          BA_.get_uint64_list x 4
        let extends_get_list x =
          Capnp.Array.to_list (extends_get x)
        let extends_get_array x =
          Capnp.Array.to_array (extends_get x)
        let extends_set x v =
          BA_.set_uint64_list x 4 v
        let extends_init x n =
          BA_.init_uint64_list x 4 n
        let extends_set_list x v =
          let builder = extends_init x (List.length v) in
          let () = List.iteri (fun i a -> Capnp.Array.set builder i a) v in
          builder
        let extends_set_array x v =
          let builder = extends_init x (Array.length v) in
          let () = Array.iteri (fun i a -> Capnp.Array.set builder i a) v in
          builder
        let of_message x = BA_.get_root_struct ~data_words:5 ~pointer_words:5 x
        let to_message x = x.BA_.NM.StructStorage.data.MessageWrapper.Slice.msg
        let to_reader x = Some (RA_.StructStorage.readonly x)
        let init_root ?message_size () =
          BA_.alloc_root_struct ?message_size ~data_words:5 ~pointer_words:5 ()
      end
      module NestedNode = struct
        type t = builder_t_NestedNode_16050641862814319170
        type reader_t = reader_t_NestedNode_16050641862814319170
        let has_name x =
          BA_.has_field x 0
        let name_get x =
          BA_.get_text ~default:"" x 0
        let name_set x v =
          BA_.set_text x 0 v
        let id_get x =
          BA_.get_uint64 ~default:Uint64.zero x 0
        let id_get_int_exn x =
          Capnp.Runtime.Util.int_of_uint64_exn (id_get x)
        let id_set x v =
          BA_.set_uint64 ~default:Uint64.zero x 0 v
        let id_set_int_exn x v = id_set x (Capnp.Runtime.Util.uint64_of_int_exn v)
        let of_message x = BA_.get_root_struct ~data_words:1 ~pointer_words:1 x
        let to_message x = x.BA_.NM.StructStorage.data.MessageWrapper.Slice.msg
        let to_reader x = Some (RA_.StructStorage.readonly x)
        let init_root ?message_size () =
          BA_.alloc_root_struct ?message_size ~data_words:1 ~pointer_words:1 ()
      end
      let file_get x = ()
      let file_set x =
        BA_.set_void ~discr:{BA_.Discr.value=0; BA_.Discr.byte_ofs=12} x
      let struct_get x = x
      let struct_init x =
        let data = x.BA_.NM.StructStorage.data in
        let pointers = x.BA_.NM.StructStorage.pointers in
        let () = ignore data in
        let () = ignore pointers in
        let () = BA_.set_opt_discriminant data
          (Some {BA_.Discr.value=1; BA_.Discr.byte_ofs=12})
        in
        let () = BA_.set_int16 ~default:0 x 14 0 in
        let () = BA_.set_int16 ~default:0 x 24 0 in
        let () = BA_.set_int16 ~default:0 x 26 0 in
        let () = BA_.set_bit ~default:false x ~byte_ofs:28 ~bit_ofs:0 false in
        let () = BA_.set_int16 ~default:0 x 30 0 in
        let () = BA_.set_int32 ~default:0l x 32 0l in
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
        let data = x.BA_.NM.StructStorage.data in
        let pointers = x.BA_.NM.StructStorage.pointers in
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
        let data = x.BA_.NM.StructStorage.data in
        let pointers = x.BA_.NM.StructStorage.pointers in
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
        let data = x.BA_.NM.StructStorage.data in
        let pointers = x.BA_.NM.StructStorage.pointers in
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
        let data = x.BA_.NM.StructStorage.data in
        let pointers = x.BA_.NM.StructStorage.pointers in
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
        let () = BA_.set_bit ~default:false x ~byte_ofs:14 ~bit_ofs:0 false in
        let () = BA_.set_bit ~default:false x ~byte_ofs:14 ~bit_ofs:1 false in
        let () = BA_.set_bit ~default:false x ~byte_ofs:14 ~bit_ofs:2 false in
        let () = BA_.set_bit ~default:false x ~byte_ofs:14 ~bit_ofs:3 false in
        let () = BA_.set_bit ~default:false x ~byte_ofs:14 ~bit_ofs:4 false in
        let () = BA_.set_bit ~default:false x ~byte_ofs:14 ~bit_ofs:5 false in
        let () = BA_.set_bit ~default:false x ~byte_ofs:14 ~bit_ofs:6 false in
        let () = BA_.set_bit ~default:false x ~byte_ofs:14 ~bit_ofs:7 false in
        let () = BA_.set_bit ~default:false x ~byte_ofs:15 ~bit_ofs:0 false in
        let () = BA_.set_bit ~default:false x ~byte_ofs:15 ~bit_ofs:1 false in
        let () = BA_.set_bit ~default:false x ~byte_ofs:15 ~bit_ofs:2 false in
        let () = BA_.set_bit ~default:false x ~byte_ofs:15 ~bit_ofs:3 false in
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
        match BA_.get_uint16 ~default:0 x 12 with
        | 0 -> File
        | 1 -> Struct (struct_get x)
        | 2 -> Enum (enum_get x)
        | 3 -> Interface (interface_get x)
        | 4 -> Const (const_get x)
        | 5 -> Annotation (annotation_get x)
        | v -> Undefined v
      let id_get x =
        BA_.get_uint64 ~default:Uint64.zero x 0
      let id_get_int_exn x =
        Capnp.Runtime.Util.int_of_uint64_exn (id_get x)
      let id_set x v =
        BA_.set_uint64 ~default:Uint64.zero x 0 v
      let id_set_int_exn x v = id_set x (Capnp.Runtime.Util.uint64_of_int_exn v)
      let has_display_name x =
        BA_.has_field x 0
      let display_name_get x =
        BA_.get_text ~default:"" x 0
      let display_name_set x v =
        BA_.set_text x 0 v
      let display_name_prefix_length_get x =
        BA_.get_uint32 ~default:Uint32.zero x 8
      let display_name_prefix_length_get_int_exn x =
        Capnp.Runtime.Util.int_of_uint32_exn (display_name_prefix_length_get x)
      let display_name_prefix_length_set x v =
        BA_.set_uint32 ~default:Uint32.zero x 8 v
      let display_name_prefix_length_set_int_exn x v = display_name_prefix_length_set x (Capnp.Runtime.Util.uint32_of_int_exn v)
      let scope_id_get x =
        BA_.get_uint64 ~default:Uint64.zero x 16
      let scope_id_get_int_exn x =
        Capnp.Runtime.Util.int_of_uint64_exn (scope_id_get x)
      let scope_id_set x v =
        BA_.set_uint64 ~default:Uint64.zero x 16 v
      let scope_id_set_int_exn x v = scope_id_set x (Capnp.Runtime.Util.uint64_of_int_exn v)
      let has_nested_nodes x = 
        (BA_.has_field x 1)
      let nested_nodes_get x = 
        (BA_.get_struct_list ~data_words:1 ~pointer_words:1 x 1)
      let nested_nodes_get_list x =
        Capnp.Array.to_list (nested_nodes_get x)
      let nested_nodes_get_array x =
        Capnp.Array.to_array (nested_nodes_get x)
      let nested_nodes_set x v = 
        (BA_.set_struct_list ~data_words:1 ~pointer_words:1 x 1 (v))
      let nested_nodes_init x n = 
        (BA_.init_struct_list ~data_words:1 ~pointer_words:1 x 1 n)
      let nested_nodes_set_list x v =
        let builder = nested_nodes_init x (List.length v) in
        let () = List.iteri (fun i a -> Capnp.Array.set builder i a) v in
        builder
      let nested_nodes_set_array x v =
        let builder = nested_nodes_init x (Array.length v) in
        let () = Array.iteri (fun i a -> Capnp.Array.set builder i a) v in
        builder
      let has_annotations x = 
        (BA_.has_field x 2)
      let annotations_get x = 
        (BA_.get_struct_list ~data_words:1 ~pointer_words:1 x 2)
      let annotations_get_list x =
        Capnp.Array.to_list (annotations_get x)
      let annotations_get_array x =
        Capnp.Array.to_array (annotations_get x)
      let annotations_set x v = 
        (BA_.set_struct_list ~data_words:1 ~pointer_words:1 x 2 (v))
      let annotations_init x n = 
        (BA_.init_struct_list ~data_words:1 ~pointer_words:1 x 2 n)
      let annotations_set_list x v =
        let builder = annotations_init x (List.length v) in
        let () = List.iteri (fun i a -> Capnp.Array.set builder i a) v in
        builder
      let annotations_set_array x v =
        let builder = annotations_init x (Array.length v) in
        let () = Array.iteri (fun i a -> Capnp.Array.set builder i a) v in
        builder
      let of_message x = BA_.get_root_struct ~data_words:5 ~pointer_words:5 x
      let to_message x = x.BA_.NM.StructStorage.data.MessageWrapper.Slice.msg
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
            BA_.get_uint64 ~default:Uint64.zero x 0
          let id_get_int_exn x =
            Capnp.Runtime.Util.int_of_uint64_exn (id_get x)
          let id_set x v =
            BA_.set_uint64 ~default:Uint64.zero x 0 v
          let id_set_int_exn x v = id_set x (Capnp.Runtime.Util.uint64_of_int_exn v)
          let has_name x =
            BA_.has_field x 0
          let name_get x =
            BA_.get_text ~default:"" x 0
          let name_set x v =
            BA_.set_text x 0 v
          let of_message x = BA_.get_root_struct ~data_words:1 ~pointer_words:1 x
          let to_message x = x.BA_.NM.StructStorage.data.MessageWrapper.Slice.msg
          let to_reader x = Some (RA_.StructStorage.readonly x)
          let init_root ?message_size () =
            BA_.alloc_root_struct ?message_size ~data_words:1 ~pointer_words:1 ()
        end
        let id_get x =
          BA_.get_uint64 ~default:Uint64.zero x 0
        let id_get_int_exn x =
          Capnp.Runtime.Util.int_of_uint64_exn (id_get x)
        let id_set x v =
          BA_.set_uint64 ~default:Uint64.zero x 0 v
        let id_set_int_exn x v = id_set x (Capnp.Runtime.Util.uint64_of_int_exn v)
        let has_filename x =
          BA_.has_field x 0
        let filename_get x =
          BA_.get_text ~default:"" x 0
        let filename_set x v =
          BA_.set_text x 0 v
        let has_imports x = 
          (BA_.has_field x 1)
        let imports_get x = 
          (BA_.get_struct_list ~data_words:1 ~pointer_words:1 x 1)
        let imports_get_list x =
          Capnp.Array.to_list (imports_get x)
        let imports_get_array x =
          Capnp.Array.to_array (imports_get x)
        let imports_set x v = 
          (BA_.set_struct_list ~data_words:1 ~pointer_words:1 x 1 (v))
        let imports_init x n = 
          (BA_.init_struct_list ~data_words:1 ~pointer_words:1 x 1 n)
        let imports_set_list x v =
          let builder = imports_init x (List.length v) in
          let () = List.iteri (fun i a -> Capnp.Array.set builder i a) v in
          builder
        let imports_set_array x v =
          let builder = imports_init x (Array.length v) in
          let () = Array.iteri (fun i a -> Capnp.Array.set builder i a) v in
          builder
        let of_message x = BA_.get_root_struct ~data_words:1 ~pointer_words:2 x
        let to_message x = x.BA_.NM.StructStorage.data.MessageWrapper.Slice.msg
        let to_reader x = Some (RA_.StructStorage.readonly x)
        let init_root ?message_size () =
          BA_.alloc_root_struct ?message_size ~data_words:1 ~pointer_words:2 ()
      end
      let has_nodes x = 
        (BA_.has_field x 0)
      let nodes_get x = 
        (BA_.get_struct_list ~data_words:5 ~pointer_words:5 x 0)
      let nodes_get_list x =
        Capnp.Array.to_list (nodes_get x)
      let nodes_get_array x =
        Capnp.Array.to_array (nodes_get x)
      let nodes_set x v = 
        (BA_.set_struct_list ~data_words:5 ~pointer_words:5 x 0 (v))
      let nodes_init x n = 
        (BA_.init_struct_list ~data_words:5 ~pointer_words:5 x 0 n)
      let nodes_set_list x v =
        let builder = nodes_init x (List.length v) in
        let () = List.iteri (fun i a -> Capnp.Array.set builder i a) v in
        builder
      let nodes_set_array x v =
        let builder = nodes_init x (Array.length v) in
        let () = Array.iteri (fun i a -> Capnp.Array.set builder i a) v in
        builder
      let has_requested_files x = 
        (BA_.has_field x 1)
      let requested_files_get x = 
        (BA_.get_struct_list ~data_words:1 ~pointer_words:2 x 1)
      let requested_files_get_list x =
        Capnp.Array.to_list (requested_files_get x)
      let requested_files_get_array x =
        Capnp.Array.to_array (requested_files_get x)
      let requested_files_set x v = 
        (BA_.set_struct_list ~data_words:1 ~pointer_words:2 x 1 (v))
      let requested_files_init x n = 
        (BA_.init_struct_list ~data_words:1 ~pointer_words:2 x 1 n)
      let requested_files_set_list x v =
        let builder = requested_files_init x (List.length v) in
        let () = List.iteri (fun i a -> Capnp.Array.set builder i a) v in
        builder
      let requested_files_set_array x v =
        let builder = requested_files_init x (Array.length v) in
        let () = Array.iteri (fun i a -> Capnp.Array.set builder i a) v in
        builder
      let of_message x = BA_.get_root_struct ~data_words:0 ~pointer_words:2 x
      let to_message x = x.BA_.NM.StructStorage.data.MessageWrapper.Slice.msg
      let to_reader x = Some (RA_.StructStorage.readonly x)
      let init_root ?message_size () =
        BA_.alloc_root_struct ?message_size ~data_words:0 ~pointer_words:2 ()
    end
  end
end

