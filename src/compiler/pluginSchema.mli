[@@@ocaml.warning "-27-32-37-60"]

type ro = Capnp.Message.ro
type rw = Capnp.Message.rw

module type S = sig
  module MessageWrapper : Capnp.RPC.S
  type 'cap message_t = 'cap MessageWrapper.Message.t
  type 'a reader_t = 'a MessageWrapper.StructStorage.reader_t
  type 'a builder_t = 'a MessageWrapper.StructStorage.builder_t

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
    type pointer_t = ro MessageWrapper.Slice.t option
    val of_pointer : pointer_t -> 'a reader_t
    module Node : sig
      type struct_t = [`Node_e682ab4cf923a417]
      type t = struct_t reader_t
      module Struct : sig
        type struct_t = [`Struct_9ea0b19b37fb4435]
        type t = struct_t reader_t
        val data_word_count_get : t -> int
        val pointer_count_get : t -> int
        val preferred_list_encoding_get : t -> ElementSize_15102134695616452902.t
        val is_group_get : t -> bool
        val discriminant_count_get : t -> int
        val discriminant_offset_get : t -> Uint32.t
        val discriminant_offset_get_int_exn : t -> int
        val has_fields : t -> bool
        val fields_get : t -> (ro, [`Field_9aad50a41f4af45f] reader_t, array_t) Capnp.Array.t
        val fields_get_list : t -> [`Field_9aad50a41f4af45f] reader_t list
        val fields_get_array : t -> [`Field_9aad50a41f4af45f] reader_t array
        val of_message : 'cap message_t -> t
        val of_builder : struct_t builder_t -> t
      end
      module Enum : sig
        type struct_t = [`Enum_b54ab3364333f598]
        type t = struct_t reader_t
        val has_enumerants : t -> bool
        val enumerants_get : t -> (ro, [`Enumerant_978a7cebdc549a4d] reader_t, array_t) Capnp.Array.t
        val enumerants_get_list : t -> [`Enumerant_978a7cebdc549a4d] reader_t list
        val enumerants_get_array : t -> [`Enumerant_978a7cebdc549a4d] reader_t array
        val of_message : 'cap message_t -> t
        val of_builder : struct_t builder_t -> t
      end
      module Interface : sig
        type struct_t = [`Interface_e82753cff0c2218f]
        type t = struct_t reader_t
        val has_methods : t -> bool
        val methods_get : t -> (ro, [`Method_9500cce23b334d80] reader_t, array_t) Capnp.Array.t
        val methods_get_list : t -> [`Method_9500cce23b334d80] reader_t list
        val methods_get_array : t -> [`Method_9500cce23b334d80] reader_t array
        val has_superclasses : t -> bool
        val superclasses_get : t -> (ro, [`Superclass_a9962a9ed0a4d7f8] reader_t, array_t) Capnp.Array.t
        val superclasses_get_list : t -> [`Superclass_a9962a9ed0a4d7f8] reader_t list
        val superclasses_get_array : t -> [`Superclass_a9962a9ed0a4d7f8] reader_t array
        val of_message : 'cap message_t -> t
        val of_builder : struct_t builder_t -> t
      end
      module Const : sig
        type struct_t = [`Const_b18aa5ac7a0d9420]
        type t = struct_t reader_t
        val has_type : t -> bool
        val type_get : t -> [`Type_d07378ede1f9cc60] reader_t
        val type_get_pipelined : struct_t MessageWrapper.StructRef.t -> [`Type_d07378ede1f9cc60] MessageWrapper.StructRef.t
        val has_value : t -> bool
        val value_get : t -> [`Value_ce23dcd2d7b00c9b] reader_t
        val value_get_pipelined : struct_t MessageWrapper.StructRef.t -> [`Value_ce23dcd2d7b00c9b] MessageWrapper.StructRef.t
        val of_message : 'cap message_t -> t
        val of_builder : struct_t builder_t -> t
      end
      module Annotation : sig
        type struct_t = [`Annotation_ec1619d4400a0290]
        type t = struct_t reader_t
        val has_type : t -> bool
        val type_get : t -> [`Type_d07378ede1f9cc60] reader_t
        val type_get_pipelined : struct_t MessageWrapper.StructRef.t -> [`Type_d07378ede1f9cc60] MessageWrapper.StructRef.t
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
        val of_builder : struct_t builder_t -> t
      end
      module Parameter : sig
        type struct_t = [`Parameter_b9521bccf10fa3b1]
        type t = struct_t reader_t
        val has_name : t -> bool
        val name_get : t -> string
        val of_message : 'cap message_t -> t
        val of_builder : struct_t builder_t -> t
      end
      module NestedNode : sig
        type struct_t = [`NestedNode_debf55bbfa0fc242]
        type t = struct_t reader_t
        val has_name : t -> bool
        val name_get : t -> string
        val id_get : t -> Uint64.t
        val id_get_int_exn : t -> int
        val of_message : 'cap message_t -> t
        val of_builder : struct_t builder_t -> t
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
      val has_parameters : t -> bool
      val parameters_get : t -> (ro, [`Parameter_b9521bccf10fa3b1] reader_t, array_t) Capnp.Array.t
      val parameters_get_list : t -> [`Parameter_b9521bccf10fa3b1] reader_t list
      val parameters_get_array : t -> [`Parameter_b9521bccf10fa3b1] reader_t array
      val is_generic_get : t -> bool
      val has_nested_nodes : t -> bool
      val nested_nodes_get : t -> (ro, [`NestedNode_debf55bbfa0fc242] reader_t, array_t) Capnp.Array.t
      val nested_nodes_get_list : t -> [`NestedNode_debf55bbfa0fc242] reader_t list
      val nested_nodes_get_array : t -> [`NestedNode_debf55bbfa0fc242] reader_t array
      val has_annotations : t -> bool
      val annotations_get : t -> (ro, [`Annotation_f1c8950dab257542] reader_t, array_t) Capnp.Array.t
      val annotations_get_list : t -> [`Annotation_f1c8950dab257542] reader_t list
      val annotations_get_array : t -> [`Annotation_f1c8950dab257542] reader_t array
      val of_message : 'cap message_t -> t
      val of_builder : struct_t builder_t -> t
    end
    module Field : sig
      type struct_t = [`Field_9aad50a41f4af45f]
      type t = struct_t reader_t
      module Slot : sig
        type struct_t = [`Slot_c42305476bb4746f]
        type t = struct_t reader_t
        val offset_get : t -> Uint32.t
        val offset_get_int_exn : t -> int
        val has_type : t -> bool
        val type_get : t -> [`Type_d07378ede1f9cc60] reader_t
        val type_get_pipelined : struct_t MessageWrapper.StructRef.t -> [`Type_d07378ede1f9cc60] MessageWrapper.StructRef.t
        val has_default_value : t -> bool
        val default_value_get : t -> [`Value_ce23dcd2d7b00c9b] reader_t
        val default_value_get_pipelined : struct_t MessageWrapper.StructRef.t -> [`Value_ce23dcd2d7b00c9b] MessageWrapper.StructRef.t
        val had_explicit_default_get : t -> bool
        val of_message : 'cap message_t -> t
        val of_builder : struct_t builder_t -> t
      end
      module Group : sig
        type struct_t = [`Group_cafccddb68db1d11]
        type t = struct_t reader_t
        val type_id_get : t -> Uint64.t
        val type_id_get_int_exn : t -> int
        val of_message : 'cap message_t -> t
        val of_builder : struct_t builder_t -> t
      end
      module Ordinal : sig
        type struct_t = [`Ordinal_bb90d5c287870be6]
        type t = struct_t reader_t
        type unnamed_union_t =
          | Implicit
          | Explicit of int
          | Undefined of int
        val get : t -> unnamed_union_t
        val of_message : 'cap message_t -> t
        val of_builder : struct_t builder_t -> t
      end
      val no_discriminant : int
      type unnamed_union_t =
        | Slot of Slot.t
        | Group of Group.t
        | Undefined of int
      val get : t -> unnamed_union_t
      val has_name : t -> bool
      val name_get : t -> string
      val code_order_get : t -> int
      val has_annotations : t -> bool
      val annotations_get : t -> (ro, [`Annotation_f1c8950dab257542] reader_t, array_t) Capnp.Array.t
      val annotations_get_list : t -> [`Annotation_f1c8950dab257542] reader_t list
      val annotations_get_array : t -> [`Annotation_f1c8950dab257542] reader_t array
      val discriminant_value_get : t -> int
      val ordinal_get : t -> Ordinal.t
      val of_message : 'cap message_t -> t
      val of_builder : struct_t builder_t -> t
    end
    module Enumerant : sig
      type struct_t = [`Enumerant_978a7cebdc549a4d]
      type t = struct_t reader_t
      val has_name : t -> bool
      val name_get : t -> string
      val code_order_get : t -> int
      val has_annotations : t -> bool
      val annotations_get : t -> (ro, [`Annotation_f1c8950dab257542] reader_t, array_t) Capnp.Array.t
      val annotations_get_list : t -> [`Annotation_f1c8950dab257542] reader_t list
      val annotations_get_array : t -> [`Annotation_f1c8950dab257542] reader_t array
      val of_message : 'cap message_t -> t
      val of_builder : struct_t builder_t -> t
    end
    module Superclass : sig
      type struct_t = [`Superclass_a9962a9ed0a4d7f8]
      type t = struct_t reader_t
      val id_get : t -> Uint64.t
      val id_get_int_exn : t -> int
      val has_brand : t -> bool
      val brand_get : t -> [`Brand_903455f06065422b] reader_t
      val brand_get_pipelined : struct_t MessageWrapper.StructRef.t -> [`Brand_903455f06065422b] MessageWrapper.StructRef.t
      val of_message : 'cap message_t -> t
      val of_builder : struct_t builder_t -> t
    end
    module Method : sig
      type struct_t = [`Method_9500cce23b334d80]
      type t = struct_t reader_t
      val has_name : t -> bool
      val name_get : t -> string
      val code_order_get : t -> int
      val has_implicit_parameters : t -> bool
      val implicit_parameters_get : t -> (ro, Node.Parameter.t, array_t) Capnp.Array.t
      val implicit_parameters_get_list : t -> Node.Parameter.t list
      val implicit_parameters_get_array : t -> Node.Parameter.t array
      val param_struct_type_get : t -> Uint64.t
      val param_struct_type_get_int_exn : t -> int
      val has_param_brand : t -> bool
      val param_brand_get : t -> [`Brand_903455f06065422b] reader_t
      val param_brand_get_pipelined : struct_t MessageWrapper.StructRef.t -> [`Brand_903455f06065422b] MessageWrapper.StructRef.t
      val result_struct_type_get : t -> Uint64.t
      val result_struct_type_get_int_exn : t -> int
      val has_result_brand : t -> bool
      val result_brand_get : t -> [`Brand_903455f06065422b] reader_t
      val result_brand_get_pipelined : struct_t MessageWrapper.StructRef.t -> [`Brand_903455f06065422b] MessageWrapper.StructRef.t
      val has_annotations : t -> bool
      val annotations_get : t -> (ro, [`Annotation_f1c8950dab257542] reader_t, array_t) Capnp.Array.t
      val annotations_get_list : t -> [`Annotation_f1c8950dab257542] reader_t list
      val annotations_get_array : t -> [`Annotation_f1c8950dab257542] reader_t array
      val of_message : 'cap message_t -> t
      val of_builder : struct_t builder_t -> t
    end
    module Type : sig
      type struct_t = [`Type_d07378ede1f9cc60]
      type t = struct_t reader_t
      module List : sig
        type struct_t = [`List_87e739250a60ea97]
        type t = struct_t reader_t
        val has_element_type : t -> bool
        val element_type_get : t -> [`Type_d07378ede1f9cc60] reader_t
        val element_type_get_pipelined : struct_t MessageWrapper.StructRef.t -> [`Type_d07378ede1f9cc60] MessageWrapper.StructRef.t
        val of_message : 'cap message_t -> t
        val of_builder : struct_t builder_t -> t
      end
      module Enum : sig
        type struct_t = [`Enum_9e0e78711a7f87a9]
        type t = struct_t reader_t
        val type_id_get : t -> Uint64.t
        val type_id_get_int_exn : t -> int
        val has_brand : t -> bool
        val brand_get : t -> [`Brand_903455f06065422b] reader_t
        val brand_get_pipelined : struct_t MessageWrapper.StructRef.t -> [`Brand_903455f06065422b] MessageWrapper.StructRef.t
        val of_message : 'cap message_t -> t
        val of_builder : struct_t builder_t -> t
      end
      module Struct : sig
        type struct_t = [`Struct_ac3a6f60ef4cc6d3]
        type t = struct_t reader_t
        val type_id_get : t -> Uint64.t
        val type_id_get_int_exn : t -> int
        val has_brand : t -> bool
        val brand_get : t -> [`Brand_903455f06065422b] reader_t
        val brand_get_pipelined : struct_t MessageWrapper.StructRef.t -> [`Brand_903455f06065422b] MessageWrapper.StructRef.t
        val of_message : 'cap message_t -> t
        val of_builder : struct_t builder_t -> t
      end
      module Interface : sig
        type struct_t = [`Interface_ed8bca69f7fb0cbf]
        type t = struct_t reader_t
        val type_id_get : t -> Uint64.t
        val type_id_get_int_exn : t -> int
        val has_brand : t -> bool
        val brand_get : t -> [`Brand_903455f06065422b] reader_t
        val brand_get_pipelined : struct_t MessageWrapper.StructRef.t -> [`Brand_903455f06065422b] MessageWrapper.StructRef.t
        val of_message : 'cap message_t -> t
        val of_builder : struct_t builder_t -> t
      end
      module AnyPointer : sig
        type struct_t = [`AnyPointer_c2573fe8a23e49f1]
        type t = struct_t reader_t
        module Unconstrained : sig
          type struct_t = [`Unconstrained_8e3b5f79fe593656]
          type t = struct_t reader_t
          type unnamed_union_t =
            | AnyKind
            | Struct
            | List
            | Capability
            | Undefined of int
          val get : t -> unnamed_union_t
          val of_message : 'cap message_t -> t
          val of_builder : struct_t builder_t -> t
        end
        module Parameter : sig
          type struct_t = [`Parameter_9dd1f724f4614a85]
          type t = struct_t reader_t
          val scope_id_get : t -> Uint64.t
          val scope_id_get_int_exn : t -> int
          val parameter_index_get : t -> int
          val of_message : 'cap message_t -> t
          val of_builder : struct_t builder_t -> t
        end
        module ImplicitMethodParameter : sig
          type struct_t = [`ImplicitMethodParameter_baefc9120c56e274]
          type t = struct_t reader_t
          val parameter_index_get : t -> int
          val of_message : 'cap message_t -> t
          val of_builder : struct_t builder_t -> t
        end
        type unnamed_union_t =
          | Unconstrained of Unconstrained.t
          | Parameter of Parameter.t
          | ImplicitMethodParameter of ImplicitMethodParameter.t
          | Undefined of int
        val get : t -> unnamed_union_t
        val of_message : 'cap message_t -> t
        val of_builder : struct_t builder_t -> t
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
        | AnyPointer of AnyPointer.t
        | Undefined of int
      val get : t -> unnamed_union_t
      val of_message : 'cap message_t -> t
      val of_builder : struct_t builder_t -> t
    end
    module Brand : sig
      type struct_t = [`Brand_903455f06065422b]
      type t = struct_t reader_t
      module Scope : sig
        type struct_t = [`Scope_abd73485a9636bc9]
        type t = struct_t reader_t
        type unnamed_union_t =
          | Bind of (ro, [`Binding_c863cd16969ee7fc] reader_t, array_t) Capnp.Array.t
          | Inherit
          | Undefined of int
        val get : t -> unnamed_union_t
        val scope_id_get : t -> Uint64.t
        val scope_id_get_int_exn : t -> int
        val of_message : 'cap message_t -> t
        val of_builder : struct_t builder_t -> t
      end
      module Binding : sig
        type struct_t = [`Binding_c863cd16969ee7fc]
        type t = struct_t reader_t
        type unnamed_union_t =
          | Unbound
          | Type of Type.t
          | Undefined of int
        val get : t -> unnamed_union_t
        val of_message : 'cap message_t -> t
        val of_builder : struct_t builder_t -> t
      end
      val has_scopes : t -> bool
      val scopes_get : t -> (ro, [`Scope_abd73485a9636bc9] reader_t, array_t) Capnp.Array.t
      val scopes_get_list : t -> [`Scope_abd73485a9636bc9] reader_t list
      val scopes_get_array : t -> [`Scope_abd73485a9636bc9] reader_t array
      val of_message : 'cap message_t -> t
      val of_builder : struct_t builder_t -> t
    end
    module Value : sig
      type struct_t = [`Value_ce23dcd2d7b00c9b]
      type t = struct_t reader_t
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
      val of_builder : struct_t builder_t -> t
    end
    module Annotation : sig
      type struct_t = [`Annotation_f1c8950dab257542]
      type t = struct_t reader_t
      val id_get : t -> Uint64.t
      val id_get_int_exn : t -> int
      val has_brand : t -> bool
      val brand_get : t -> Brand.t
      val brand_get_pipelined : struct_t MessageWrapper.StructRef.t -> Brand.struct_t MessageWrapper.StructRef.t
      val has_value : t -> bool
      val value_get : t -> Value.t
      val value_get_pipelined : struct_t MessageWrapper.StructRef.t -> Value.struct_t MessageWrapper.StructRef.t
      val of_message : 'cap message_t -> t
      val of_builder : struct_t builder_t -> t
    end
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
    module CapnpVersion : sig
      type struct_t = [`CapnpVersion_d85d305b7d839963]
      type t = struct_t reader_t
      val major_get : t -> int
      val minor_get : t -> int
      val micro_get : t -> int
      val of_message : 'cap message_t -> t
      val of_builder : struct_t builder_t -> t
    end
    module CodeGeneratorRequest : sig
      type struct_t = [`CodeGeneratorRequest_bfc546f6210ad7ce]
      type t = struct_t reader_t
      module RequestedFile : sig
        type struct_t = [`RequestedFile_cfea0eb02e810062]
        type t = struct_t reader_t
        module Import : sig
          type struct_t = [`Import_ae504193122357e5]
          type t = struct_t reader_t
          val id_get : t -> Uint64.t
          val id_get_int_exn : t -> int
          val has_name : t -> bool
          val name_get : t -> string
          val of_message : 'cap message_t -> t
          val of_builder : struct_t builder_t -> t
        end
        val id_get : t -> Uint64.t
        val id_get_int_exn : t -> int
        val has_filename : t -> bool
        val filename_get : t -> string
        val has_imports : t -> bool
        val imports_get : t -> (ro, [`Import_ae504193122357e5] reader_t, array_t) Capnp.Array.t
        val imports_get_list : t -> [`Import_ae504193122357e5] reader_t list
        val imports_get_array : t -> [`Import_ae504193122357e5] reader_t array
        val of_message : 'cap message_t -> t
        val of_builder : struct_t builder_t -> t
      end
      val has_capnp_version : t -> bool
      val capnp_version_get : t -> CapnpVersion.t
      val capnp_version_get_pipelined : struct_t MessageWrapper.StructRef.t -> CapnpVersion.struct_t MessageWrapper.StructRef.t
      val has_nodes : t -> bool
      val nodes_get : t -> (ro, Node.t, array_t) Capnp.Array.t
      val nodes_get_list : t -> Node.t list
      val nodes_get_array : t -> Node.t array
      val has_requested_files : t -> bool
      val requested_files_get : t -> (ro, [`RequestedFile_cfea0eb02e810062] reader_t, array_t) Capnp.Array.t
      val requested_files_get_list : t -> [`RequestedFile_cfea0eb02e810062] reader_t list
      val requested_files_get_array : t -> [`RequestedFile_cfea0eb02e810062] reader_t array
      val of_message : 'cap message_t -> t
      val of_builder : struct_t builder_t -> t
    end
  end

  module Builder : sig
    type array_t = Reader.builder_array_t
    type reader_array_t = Reader.array_t
    type pointer_t = rw MessageWrapper.Slice.t
    module Node : sig
      type struct_t = [`Node_e682ab4cf923a417]
      type t = struct_t builder_t
      module Struct : sig
        type struct_t = [`Struct_9ea0b19b37fb4435]
        type t = struct_t builder_t
        val data_word_count_get : t -> int
        val data_word_count_set_exn : t -> int -> unit
        val pointer_count_get : t -> int
        val pointer_count_set_exn : t -> int -> unit
        val preferred_list_encoding_get : t -> ElementSize_15102134695616452902.t
        val preferred_list_encoding_set : t -> ElementSize_15102134695616452902.t -> unit
        val preferred_list_encoding_set_unsafe : t -> ElementSize_15102134695616452902.t -> unit
        val is_group_get : t -> bool
        val is_group_set : t -> bool -> unit
        val discriminant_count_get : t -> int
        val discriminant_count_set_exn : t -> int -> unit
        val discriminant_offset_get : t -> Uint32.t
        val discriminant_offset_get_int_exn : t -> int
        val discriminant_offset_set : t -> Uint32.t -> unit
        val discriminant_offset_set_int_exn : t -> int -> unit
        val has_fields : t -> bool
        val fields_get : t -> (rw, [`Field_9aad50a41f4af45f] builder_t, array_t) Capnp.Array.t
        val fields_get_list : t -> [`Field_9aad50a41f4af45f] builder_t list
        val fields_get_array : t -> [`Field_9aad50a41f4af45f] builder_t array
        val fields_set : t -> (rw, [`Field_9aad50a41f4af45f] builder_t, array_t) Capnp.Array.t -> (rw, [`Field_9aad50a41f4af45f] builder_t, array_t) Capnp.Array.t
        val fields_set_list : t -> [`Field_9aad50a41f4af45f] builder_t list -> (rw, [`Field_9aad50a41f4af45f] builder_t, array_t) Capnp.Array.t
        val fields_set_array : t -> [`Field_9aad50a41f4af45f] builder_t array -> (rw, [`Field_9aad50a41f4af45f] builder_t, array_t) Capnp.Array.t
        val fields_init : t -> int -> (rw, [`Field_9aad50a41f4af45f] builder_t, array_t) Capnp.Array.t
        val of_message : rw message_t -> t
        val to_message : t -> rw message_t
        val to_reader : t -> struct_t reader_t
        val init_root : ?message_size:int -> unit -> t
        val init_pointer : pointer_t -> t
      end
      module Enum : sig
        type struct_t = [`Enum_b54ab3364333f598]
        type t = struct_t builder_t
        val has_enumerants : t -> bool
        val enumerants_get : t -> (rw, [`Enumerant_978a7cebdc549a4d] builder_t, array_t) Capnp.Array.t
        val enumerants_get_list : t -> [`Enumerant_978a7cebdc549a4d] builder_t list
        val enumerants_get_array : t -> [`Enumerant_978a7cebdc549a4d] builder_t array
        val enumerants_set : t -> (rw, [`Enumerant_978a7cebdc549a4d] builder_t, array_t) Capnp.Array.t -> (rw, [`Enumerant_978a7cebdc549a4d] builder_t, array_t) Capnp.Array.t
        val enumerants_set_list : t -> [`Enumerant_978a7cebdc549a4d] builder_t list -> (rw, [`Enumerant_978a7cebdc549a4d] builder_t, array_t) Capnp.Array.t
        val enumerants_set_array : t -> [`Enumerant_978a7cebdc549a4d] builder_t array -> (rw, [`Enumerant_978a7cebdc549a4d] builder_t, array_t) Capnp.Array.t
        val enumerants_init : t -> int -> (rw, [`Enumerant_978a7cebdc549a4d] builder_t, array_t) Capnp.Array.t
        val of_message : rw message_t -> t
        val to_message : t -> rw message_t
        val to_reader : t -> struct_t reader_t
        val init_root : ?message_size:int -> unit -> t
        val init_pointer : pointer_t -> t
      end
      module Interface : sig
        type struct_t = [`Interface_e82753cff0c2218f]
        type t = struct_t builder_t
        val has_methods : t -> bool
        val methods_get : t -> (rw, [`Method_9500cce23b334d80] builder_t, array_t) Capnp.Array.t
        val methods_get_list : t -> [`Method_9500cce23b334d80] builder_t list
        val methods_get_array : t -> [`Method_9500cce23b334d80] builder_t array
        val methods_set : t -> (rw, [`Method_9500cce23b334d80] builder_t, array_t) Capnp.Array.t -> (rw, [`Method_9500cce23b334d80] builder_t, array_t) Capnp.Array.t
        val methods_set_list : t -> [`Method_9500cce23b334d80] builder_t list -> (rw, [`Method_9500cce23b334d80] builder_t, array_t) Capnp.Array.t
        val methods_set_array : t -> [`Method_9500cce23b334d80] builder_t array -> (rw, [`Method_9500cce23b334d80] builder_t, array_t) Capnp.Array.t
        val methods_init : t -> int -> (rw, [`Method_9500cce23b334d80] builder_t, array_t) Capnp.Array.t
        val has_superclasses : t -> bool
        val superclasses_get : t -> (rw, [`Superclass_a9962a9ed0a4d7f8] builder_t, array_t) Capnp.Array.t
        val superclasses_get_list : t -> [`Superclass_a9962a9ed0a4d7f8] builder_t list
        val superclasses_get_array : t -> [`Superclass_a9962a9ed0a4d7f8] builder_t array
        val superclasses_set : t -> (rw, [`Superclass_a9962a9ed0a4d7f8] builder_t, array_t) Capnp.Array.t -> (rw, [`Superclass_a9962a9ed0a4d7f8] builder_t, array_t) Capnp.Array.t
        val superclasses_set_list : t -> [`Superclass_a9962a9ed0a4d7f8] builder_t list -> (rw, [`Superclass_a9962a9ed0a4d7f8] builder_t, array_t) Capnp.Array.t
        val superclasses_set_array : t -> [`Superclass_a9962a9ed0a4d7f8] builder_t array -> (rw, [`Superclass_a9962a9ed0a4d7f8] builder_t, array_t) Capnp.Array.t
        val superclasses_init : t -> int -> (rw, [`Superclass_a9962a9ed0a4d7f8] builder_t, array_t) Capnp.Array.t
        val of_message : rw message_t -> t
        val to_message : t -> rw message_t
        val to_reader : t -> struct_t reader_t
        val init_root : ?message_size:int -> unit -> t
        val init_pointer : pointer_t -> t
      end
      module Const : sig
        type struct_t = [`Const_b18aa5ac7a0d9420]
        type t = struct_t builder_t
        val has_type : t -> bool
        val type_get : t -> [`Type_d07378ede1f9cc60] builder_t
        val type_set_reader : t -> [`Type_d07378ede1f9cc60] reader_t -> [`Type_d07378ede1f9cc60] builder_t
        val type_set_builder : t -> [`Type_d07378ede1f9cc60] builder_t -> [`Type_d07378ede1f9cc60] builder_t
        val type_init : t -> [`Type_d07378ede1f9cc60] builder_t
        val has_value : t -> bool
        val value_get : t -> [`Value_ce23dcd2d7b00c9b] builder_t
        val value_set_reader : t -> [`Value_ce23dcd2d7b00c9b] reader_t -> [`Value_ce23dcd2d7b00c9b] builder_t
        val value_set_builder : t -> [`Value_ce23dcd2d7b00c9b] builder_t -> [`Value_ce23dcd2d7b00c9b] builder_t
        val value_init : t -> [`Value_ce23dcd2d7b00c9b] builder_t
        val of_message : rw message_t -> t
        val to_message : t -> rw message_t
        val to_reader : t -> struct_t reader_t
        val init_root : ?message_size:int -> unit -> t
        val init_pointer : pointer_t -> t
      end
      module Annotation : sig
        type struct_t = [`Annotation_ec1619d4400a0290]
        type t = struct_t builder_t
        val has_type : t -> bool
        val type_get : t -> [`Type_d07378ede1f9cc60] builder_t
        val type_set_reader : t -> [`Type_d07378ede1f9cc60] reader_t -> [`Type_d07378ede1f9cc60] builder_t
        val type_set_builder : t -> [`Type_d07378ede1f9cc60] builder_t -> [`Type_d07378ede1f9cc60] builder_t
        val type_init : t -> [`Type_d07378ede1f9cc60] builder_t
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
        val to_reader : t -> struct_t reader_t
        val init_root : ?message_size:int -> unit -> t
        val init_pointer : pointer_t -> t
      end
      module Parameter : sig
        type struct_t = [`Parameter_b9521bccf10fa3b1]
        type t = struct_t builder_t
        val has_name : t -> bool
        val name_get : t -> string
        val name_set : t -> string -> unit
        val of_message : rw message_t -> t
        val to_message : t -> rw message_t
        val to_reader : t -> struct_t reader_t
        val init_root : ?message_size:int -> unit -> t
        val init_pointer : pointer_t -> t
      end
      module NestedNode : sig
        type struct_t = [`NestedNode_debf55bbfa0fc242]
        type t = struct_t builder_t
        val has_name : t -> bool
        val name_get : t -> string
        val name_set : t -> string -> unit
        val id_get : t -> Uint64.t
        val id_get_int_exn : t -> int
        val id_set : t -> Uint64.t -> unit
        val id_set_int_exn : t -> int -> unit
        val of_message : rw message_t -> t
        val to_message : t -> rw message_t
        val to_reader : t -> struct_t reader_t
        val init_root : ?message_size:int -> unit -> t
        val init_pointer : pointer_t -> t
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
      val has_parameters : t -> bool
      val parameters_get : t -> (rw, [`Parameter_b9521bccf10fa3b1] builder_t, array_t) Capnp.Array.t
      val parameters_get_list : t -> [`Parameter_b9521bccf10fa3b1] builder_t list
      val parameters_get_array : t -> [`Parameter_b9521bccf10fa3b1] builder_t array
      val parameters_set : t -> (rw, [`Parameter_b9521bccf10fa3b1] builder_t, array_t) Capnp.Array.t -> (rw, [`Parameter_b9521bccf10fa3b1] builder_t, array_t) Capnp.Array.t
      val parameters_set_list : t -> [`Parameter_b9521bccf10fa3b1] builder_t list -> (rw, [`Parameter_b9521bccf10fa3b1] builder_t, array_t) Capnp.Array.t
      val parameters_set_array : t -> [`Parameter_b9521bccf10fa3b1] builder_t array -> (rw, [`Parameter_b9521bccf10fa3b1] builder_t, array_t) Capnp.Array.t
      val parameters_init : t -> int -> (rw, [`Parameter_b9521bccf10fa3b1] builder_t, array_t) Capnp.Array.t
      val is_generic_get : t -> bool
      val is_generic_set : t -> bool -> unit
      val has_nested_nodes : t -> bool
      val nested_nodes_get : t -> (rw, [`NestedNode_debf55bbfa0fc242] builder_t, array_t) Capnp.Array.t
      val nested_nodes_get_list : t -> [`NestedNode_debf55bbfa0fc242] builder_t list
      val nested_nodes_get_array : t -> [`NestedNode_debf55bbfa0fc242] builder_t array
      val nested_nodes_set : t -> (rw, [`NestedNode_debf55bbfa0fc242] builder_t, array_t) Capnp.Array.t -> (rw, [`NestedNode_debf55bbfa0fc242] builder_t, array_t) Capnp.Array.t
      val nested_nodes_set_list : t -> [`NestedNode_debf55bbfa0fc242] builder_t list -> (rw, [`NestedNode_debf55bbfa0fc242] builder_t, array_t) Capnp.Array.t
      val nested_nodes_set_array : t -> [`NestedNode_debf55bbfa0fc242] builder_t array -> (rw, [`NestedNode_debf55bbfa0fc242] builder_t, array_t) Capnp.Array.t
      val nested_nodes_init : t -> int -> (rw, [`NestedNode_debf55bbfa0fc242] builder_t, array_t) Capnp.Array.t
      val has_annotations : t -> bool
      val annotations_get : t -> (rw, [`Annotation_f1c8950dab257542] builder_t, array_t) Capnp.Array.t
      val annotations_get_list : t -> [`Annotation_f1c8950dab257542] builder_t list
      val annotations_get_array : t -> [`Annotation_f1c8950dab257542] builder_t array
      val annotations_set : t -> (rw, [`Annotation_f1c8950dab257542] builder_t, array_t) Capnp.Array.t -> (rw, [`Annotation_f1c8950dab257542] builder_t, array_t) Capnp.Array.t
      val annotations_set_list : t -> [`Annotation_f1c8950dab257542] builder_t list -> (rw, [`Annotation_f1c8950dab257542] builder_t, array_t) Capnp.Array.t
      val annotations_set_array : t -> [`Annotation_f1c8950dab257542] builder_t array -> (rw, [`Annotation_f1c8950dab257542] builder_t, array_t) Capnp.Array.t
      val annotations_init : t -> int -> (rw, [`Annotation_f1c8950dab257542] builder_t, array_t) Capnp.Array.t
      val of_message : rw message_t -> t
      val to_message : t -> rw message_t
      val to_reader : t -> struct_t reader_t
      val init_root : ?message_size:int -> unit -> t
      val init_pointer : pointer_t -> t
    end
    module Field : sig
      type struct_t = [`Field_9aad50a41f4af45f]
      type t = struct_t builder_t
      module Slot : sig
        type struct_t = [`Slot_c42305476bb4746f]
        type t = struct_t builder_t
        val offset_get : t -> Uint32.t
        val offset_get_int_exn : t -> int
        val offset_set : t -> Uint32.t -> unit
        val offset_set_int_exn : t -> int -> unit
        val has_type : t -> bool
        val type_get : t -> [`Type_d07378ede1f9cc60] builder_t
        val type_set_reader : t -> [`Type_d07378ede1f9cc60] reader_t -> [`Type_d07378ede1f9cc60] builder_t
        val type_set_builder : t -> [`Type_d07378ede1f9cc60] builder_t -> [`Type_d07378ede1f9cc60] builder_t
        val type_init : t -> [`Type_d07378ede1f9cc60] builder_t
        val has_default_value : t -> bool
        val default_value_get : t -> [`Value_ce23dcd2d7b00c9b] builder_t
        val default_value_set_reader : t -> [`Value_ce23dcd2d7b00c9b] reader_t -> [`Value_ce23dcd2d7b00c9b] builder_t
        val default_value_set_builder : t -> [`Value_ce23dcd2d7b00c9b] builder_t -> [`Value_ce23dcd2d7b00c9b] builder_t
        val default_value_init : t -> [`Value_ce23dcd2d7b00c9b] builder_t
        val had_explicit_default_get : t -> bool
        val had_explicit_default_set : t -> bool -> unit
        val of_message : rw message_t -> t
        val to_message : t -> rw message_t
        val to_reader : t -> struct_t reader_t
        val init_root : ?message_size:int -> unit -> t
        val init_pointer : pointer_t -> t
      end
      module Group : sig
        type struct_t = [`Group_cafccddb68db1d11]
        type t = struct_t builder_t
        val type_id_get : t -> Uint64.t
        val type_id_get_int_exn : t -> int
        val type_id_set : t -> Uint64.t -> unit
        val type_id_set_int_exn : t -> int -> unit
        val of_message : rw message_t -> t
        val to_message : t -> rw message_t
        val to_reader : t -> struct_t reader_t
        val init_root : ?message_size:int -> unit -> t
        val init_pointer : pointer_t -> t
      end
      module Ordinal : sig
        type struct_t = [`Ordinal_bb90d5c287870be6]
        type t = struct_t builder_t
        type unnamed_union_t =
          | Implicit
          | Explicit of int
          | Undefined of int
        val get : t -> unnamed_union_t
        val implicit_set : t -> unit
        val explicit_set_exn : t -> int -> unit
        val of_message : rw message_t -> t
        val to_message : t -> rw message_t
        val to_reader : t -> struct_t reader_t
        val init_root : ?message_size:int -> unit -> t
        val init_pointer : pointer_t -> t
      end
      val no_discriminant : int
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
      val annotations_get : t -> (rw, [`Annotation_f1c8950dab257542] builder_t, array_t) Capnp.Array.t
      val annotations_get_list : t -> [`Annotation_f1c8950dab257542] builder_t list
      val annotations_get_array : t -> [`Annotation_f1c8950dab257542] builder_t array
      val annotations_set : t -> (rw, [`Annotation_f1c8950dab257542] builder_t, array_t) Capnp.Array.t -> (rw, [`Annotation_f1c8950dab257542] builder_t, array_t) Capnp.Array.t
      val annotations_set_list : t -> [`Annotation_f1c8950dab257542] builder_t list -> (rw, [`Annotation_f1c8950dab257542] builder_t, array_t) Capnp.Array.t
      val annotations_set_array : t -> [`Annotation_f1c8950dab257542] builder_t array -> (rw, [`Annotation_f1c8950dab257542] builder_t, array_t) Capnp.Array.t
      val annotations_init : t -> int -> (rw, [`Annotation_f1c8950dab257542] builder_t, array_t) Capnp.Array.t
      val discriminant_value_get : t -> int
      val discriminant_value_set_exn : t -> int -> unit
      val ordinal_get : t -> Ordinal.t
      val ordinal_init : t -> Ordinal.t
      val of_message : rw message_t -> t
      val to_message : t -> rw message_t
      val to_reader : t -> struct_t reader_t
      val init_root : ?message_size:int -> unit -> t
      val init_pointer : pointer_t -> t
    end
    module Enumerant : sig
      type struct_t = [`Enumerant_978a7cebdc549a4d]
      type t = struct_t builder_t
      val has_name : t -> bool
      val name_get : t -> string
      val name_set : t -> string -> unit
      val code_order_get : t -> int
      val code_order_set_exn : t -> int -> unit
      val has_annotations : t -> bool
      val annotations_get : t -> (rw, [`Annotation_f1c8950dab257542] builder_t, array_t) Capnp.Array.t
      val annotations_get_list : t -> [`Annotation_f1c8950dab257542] builder_t list
      val annotations_get_array : t -> [`Annotation_f1c8950dab257542] builder_t array
      val annotations_set : t -> (rw, [`Annotation_f1c8950dab257542] builder_t, array_t) Capnp.Array.t -> (rw, [`Annotation_f1c8950dab257542] builder_t, array_t) Capnp.Array.t
      val annotations_set_list : t -> [`Annotation_f1c8950dab257542] builder_t list -> (rw, [`Annotation_f1c8950dab257542] builder_t, array_t) Capnp.Array.t
      val annotations_set_array : t -> [`Annotation_f1c8950dab257542] builder_t array -> (rw, [`Annotation_f1c8950dab257542] builder_t, array_t) Capnp.Array.t
      val annotations_init : t -> int -> (rw, [`Annotation_f1c8950dab257542] builder_t, array_t) Capnp.Array.t
      val of_message : rw message_t -> t
      val to_message : t -> rw message_t
      val to_reader : t -> struct_t reader_t
      val init_root : ?message_size:int -> unit -> t
      val init_pointer : pointer_t -> t
    end
    module Superclass : sig
      type struct_t = [`Superclass_a9962a9ed0a4d7f8]
      type t = struct_t builder_t
      val id_get : t -> Uint64.t
      val id_get_int_exn : t -> int
      val id_set : t -> Uint64.t -> unit
      val id_set_int_exn : t -> int -> unit
      val has_brand : t -> bool
      val brand_get : t -> [`Brand_903455f06065422b] builder_t
      val brand_set_reader : t -> [`Brand_903455f06065422b] reader_t -> [`Brand_903455f06065422b] builder_t
      val brand_set_builder : t -> [`Brand_903455f06065422b] builder_t -> [`Brand_903455f06065422b] builder_t
      val brand_init : t -> [`Brand_903455f06065422b] builder_t
      val of_message : rw message_t -> t
      val to_message : t -> rw message_t
      val to_reader : t -> struct_t reader_t
      val init_root : ?message_size:int -> unit -> t
      val init_pointer : pointer_t -> t
    end
    module Method : sig
      type struct_t = [`Method_9500cce23b334d80]
      type t = struct_t builder_t
      val has_name : t -> bool
      val name_get : t -> string
      val name_set : t -> string -> unit
      val code_order_get : t -> int
      val code_order_set_exn : t -> int -> unit
      val has_implicit_parameters : t -> bool
      val implicit_parameters_get : t -> (rw, Node.Parameter.t, array_t) Capnp.Array.t
      val implicit_parameters_get_list : t -> Node.Parameter.t list
      val implicit_parameters_get_array : t -> Node.Parameter.t array
      val implicit_parameters_set : t -> (rw, Node.Parameter.t, array_t) Capnp.Array.t -> (rw, Node.Parameter.t, array_t) Capnp.Array.t
      val implicit_parameters_set_list : t -> Node.Parameter.t list -> (rw, Node.Parameter.t, array_t) Capnp.Array.t
      val implicit_parameters_set_array : t -> Node.Parameter.t array -> (rw, Node.Parameter.t, array_t) Capnp.Array.t
      val implicit_parameters_init : t -> int -> (rw, Node.Parameter.t, array_t) Capnp.Array.t
      val param_struct_type_get : t -> Uint64.t
      val param_struct_type_get_int_exn : t -> int
      val param_struct_type_set : t -> Uint64.t -> unit
      val param_struct_type_set_int_exn : t -> int -> unit
      val has_param_brand : t -> bool
      val param_brand_get : t -> [`Brand_903455f06065422b] builder_t
      val param_brand_set_reader : t -> [`Brand_903455f06065422b] reader_t -> [`Brand_903455f06065422b] builder_t
      val param_brand_set_builder : t -> [`Brand_903455f06065422b] builder_t -> [`Brand_903455f06065422b] builder_t
      val param_brand_init : t -> [`Brand_903455f06065422b] builder_t
      val result_struct_type_get : t -> Uint64.t
      val result_struct_type_get_int_exn : t -> int
      val result_struct_type_set : t -> Uint64.t -> unit
      val result_struct_type_set_int_exn : t -> int -> unit
      val has_result_brand : t -> bool
      val result_brand_get : t -> [`Brand_903455f06065422b] builder_t
      val result_brand_set_reader : t -> [`Brand_903455f06065422b] reader_t -> [`Brand_903455f06065422b] builder_t
      val result_brand_set_builder : t -> [`Brand_903455f06065422b] builder_t -> [`Brand_903455f06065422b] builder_t
      val result_brand_init : t -> [`Brand_903455f06065422b] builder_t
      val has_annotations : t -> bool
      val annotations_get : t -> (rw, [`Annotation_f1c8950dab257542] builder_t, array_t) Capnp.Array.t
      val annotations_get_list : t -> [`Annotation_f1c8950dab257542] builder_t list
      val annotations_get_array : t -> [`Annotation_f1c8950dab257542] builder_t array
      val annotations_set : t -> (rw, [`Annotation_f1c8950dab257542] builder_t, array_t) Capnp.Array.t -> (rw, [`Annotation_f1c8950dab257542] builder_t, array_t) Capnp.Array.t
      val annotations_set_list : t -> [`Annotation_f1c8950dab257542] builder_t list -> (rw, [`Annotation_f1c8950dab257542] builder_t, array_t) Capnp.Array.t
      val annotations_set_array : t -> [`Annotation_f1c8950dab257542] builder_t array -> (rw, [`Annotation_f1c8950dab257542] builder_t, array_t) Capnp.Array.t
      val annotations_init : t -> int -> (rw, [`Annotation_f1c8950dab257542] builder_t, array_t) Capnp.Array.t
      val of_message : rw message_t -> t
      val to_message : t -> rw message_t
      val to_reader : t -> struct_t reader_t
      val init_root : ?message_size:int -> unit -> t
      val init_pointer : pointer_t -> t
    end
    module Type : sig
      type struct_t = [`Type_d07378ede1f9cc60]
      type t = struct_t builder_t
      module List : sig
        type struct_t = [`List_87e739250a60ea97]
        type t = struct_t builder_t
        val has_element_type : t -> bool
        val element_type_get : t -> [`Type_d07378ede1f9cc60] builder_t
        val element_type_set_reader : t -> [`Type_d07378ede1f9cc60] reader_t -> [`Type_d07378ede1f9cc60] builder_t
        val element_type_set_builder : t -> [`Type_d07378ede1f9cc60] builder_t -> [`Type_d07378ede1f9cc60] builder_t
        val element_type_init : t -> [`Type_d07378ede1f9cc60] builder_t
        val of_message : rw message_t -> t
        val to_message : t -> rw message_t
        val to_reader : t -> struct_t reader_t
        val init_root : ?message_size:int -> unit -> t
        val init_pointer : pointer_t -> t
      end
      module Enum : sig
        type struct_t = [`Enum_9e0e78711a7f87a9]
        type t = struct_t builder_t
        val type_id_get : t -> Uint64.t
        val type_id_get_int_exn : t -> int
        val type_id_set : t -> Uint64.t -> unit
        val type_id_set_int_exn : t -> int -> unit
        val has_brand : t -> bool
        val brand_get : t -> [`Brand_903455f06065422b] builder_t
        val brand_set_reader : t -> [`Brand_903455f06065422b] reader_t -> [`Brand_903455f06065422b] builder_t
        val brand_set_builder : t -> [`Brand_903455f06065422b] builder_t -> [`Brand_903455f06065422b] builder_t
        val brand_init : t -> [`Brand_903455f06065422b] builder_t
        val of_message : rw message_t -> t
        val to_message : t -> rw message_t
        val to_reader : t -> struct_t reader_t
        val init_root : ?message_size:int -> unit -> t
        val init_pointer : pointer_t -> t
      end
      module Struct : sig
        type struct_t = [`Struct_ac3a6f60ef4cc6d3]
        type t = struct_t builder_t
        val type_id_get : t -> Uint64.t
        val type_id_get_int_exn : t -> int
        val type_id_set : t -> Uint64.t -> unit
        val type_id_set_int_exn : t -> int -> unit
        val has_brand : t -> bool
        val brand_get : t -> [`Brand_903455f06065422b] builder_t
        val brand_set_reader : t -> [`Brand_903455f06065422b] reader_t -> [`Brand_903455f06065422b] builder_t
        val brand_set_builder : t -> [`Brand_903455f06065422b] builder_t -> [`Brand_903455f06065422b] builder_t
        val brand_init : t -> [`Brand_903455f06065422b] builder_t
        val of_message : rw message_t -> t
        val to_message : t -> rw message_t
        val to_reader : t -> struct_t reader_t
        val init_root : ?message_size:int -> unit -> t
        val init_pointer : pointer_t -> t
      end
      module Interface : sig
        type struct_t = [`Interface_ed8bca69f7fb0cbf]
        type t = struct_t builder_t
        val type_id_get : t -> Uint64.t
        val type_id_get_int_exn : t -> int
        val type_id_set : t -> Uint64.t -> unit
        val type_id_set_int_exn : t -> int -> unit
        val has_brand : t -> bool
        val brand_get : t -> [`Brand_903455f06065422b] builder_t
        val brand_set_reader : t -> [`Brand_903455f06065422b] reader_t -> [`Brand_903455f06065422b] builder_t
        val brand_set_builder : t -> [`Brand_903455f06065422b] builder_t -> [`Brand_903455f06065422b] builder_t
        val brand_init : t -> [`Brand_903455f06065422b] builder_t
        val of_message : rw message_t -> t
        val to_message : t -> rw message_t
        val to_reader : t -> struct_t reader_t
        val init_root : ?message_size:int -> unit -> t
        val init_pointer : pointer_t -> t
      end
      module AnyPointer : sig
        type struct_t = [`AnyPointer_c2573fe8a23e49f1]
        type t = struct_t builder_t
        module Unconstrained : sig
          type struct_t = [`Unconstrained_8e3b5f79fe593656]
          type t = struct_t builder_t
          type unnamed_union_t =
            | AnyKind
            | Struct
            | List
            | Capability
            | Undefined of int
          val get : t -> unnamed_union_t
          val any_kind_set : t -> unit
          val struct_set : t -> unit
          val list_set : t -> unit
          val capability_set : t -> unit
          val of_message : rw message_t -> t
          val to_message : t -> rw message_t
          val to_reader : t -> struct_t reader_t
          val init_root : ?message_size:int -> unit -> t
          val init_pointer : pointer_t -> t
        end
        module Parameter : sig
          type struct_t = [`Parameter_9dd1f724f4614a85]
          type t = struct_t builder_t
          val scope_id_get : t -> Uint64.t
          val scope_id_get_int_exn : t -> int
          val scope_id_set : t -> Uint64.t -> unit
          val scope_id_set_int_exn : t -> int -> unit
          val parameter_index_get : t -> int
          val parameter_index_set_exn : t -> int -> unit
          val of_message : rw message_t -> t
          val to_message : t -> rw message_t
          val to_reader : t -> struct_t reader_t
          val init_root : ?message_size:int -> unit -> t
          val init_pointer : pointer_t -> t
        end
        module ImplicitMethodParameter : sig
          type struct_t = [`ImplicitMethodParameter_baefc9120c56e274]
          type t = struct_t builder_t
          val parameter_index_get : t -> int
          val parameter_index_set_exn : t -> int -> unit
          val of_message : rw message_t -> t
          val to_message : t -> rw message_t
          val to_reader : t -> struct_t reader_t
          val init_root : ?message_size:int -> unit -> t
          val init_pointer : pointer_t -> t
        end
        type unnamed_union_t =
          | Unconstrained of Unconstrained.t
          | Parameter of Parameter.t
          | ImplicitMethodParameter of ImplicitMethodParameter.t
          | Undefined of int
        val get : t -> unnamed_union_t
        val unconstrained_init : t -> Unconstrained.t
        val parameter_init : t -> Parameter.t
        val implicit_method_parameter_init : t -> ImplicitMethodParameter.t
        val of_message : rw message_t -> t
        val to_message : t -> rw message_t
        val to_reader : t -> struct_t reader_t
        val init_root : ?message_size:int -> unit -> t
        val init_pointer : pointer_t -> t
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
        | AnyPointer of AnyPointer.t
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
      val any_pointer_init : t -> AnyPointer.t
      val of_message : rw message_t -> t
      val to_message : t -> rw message_t
      val to_reader : t -> struct_t reader_t
      val init_root : ?message_size:int -> unit -> t
      val init_pointer : pointer_t -> t
    end
    module Brand : sig
      type struct_t = [`Brand_903455f06065422b]
      type t = struct_t builder_t
      module Scope : sig
        type struct_t = [`Scope_abd73485a9636bc9]
        type t = struct_t builder_t
        type unnamed_union_t =
          | Bind of (rw, [`Binding_c863cd16969ee7fc] builder_t, array_t) Capnp.Array.t
          | Inherit
          | Undefined of int
        val get : t -> unnamed_union_t
        val bind_set : t -> (rw, [`Binding_c863cd16969ee7fc] builder_t, array_t) Capnp.Array.t -> (rw, [`Binding_c863cd16969ee7fc] builder_t, array_t) Capnp.Array.t
        val bind_set_list : t -> [`Binding_c863cd16969ee7fc] builder_t list -> (rw, [`Binding_c863cd16969ee7fc] builder_t, array_t) Capnp.Array.t
        val bind_set_array : t -> [`Binding_c863cd16969ee7fc] builder_t array -> (rw, [`Binding_c863cd16969ee7fc] builder_t, array_t) Capnp.Array.t
        val bind_init : t -> int -> (rw, [`Binding_c863cd16969ee7fc] builder_t, array_t) Capnp.Array.t
        val inherit_set : t -> unit
        val scope_id_get : t -> Uint64.t
        val scope_id_get_int_exn : t -> int
        val scope_id_set : t -> Uint64.t -> unit
        val scope_id_set_int_exn : t -> int -> unit
        val of_message : rw message_t -> t
        val to_message : t -> rw message_t
        val to_reader : t -> struct_t reader_t
        val init_root : ?message_size:int -> unit -> t
        val init_pointer : pointer_t -> t
      end
      module Binding : sig
        type struct_t = [`Binding_c863cd16969ee7fc]
        type t = struct_t builder_t
        type unnamed_union_t =
          | Unbound
          | Type of Type.t
          | Undefined of int
        val get : t -> unnamed_union_t
        val unbound_set : t -> unit
        val type_set_reader : t -> Type.struct_t reader_t -> Type.t
        val type_set_builder : t -> Type.t -> Type.t
        val type_init : t -> Type.t
        val of_message : rw message_t -> t
        val to_message : t -> rw message_t
        val to_reader : t -> struct_t reader_t
        val init_root : ?message_size:int -> unit -> t
        val init_pointer : pointer_t -> t
      end
      val has_scopes : t -> bool
      val scopes_get : t -> (rw, [`Scope_abd73485a9636bc9] builder_t, array_t) Capnp.Array.t
      val scopes_get_list : t -> [`Scope_abd73485a9636bc9] builder_t list
      val scopes_get_array : t -> [`Scope_abd73485a9636bc9] builder_t array
      val scopes_set : t -> (rw, [`Scope_abd73485a9636bc9] builder_t, array_t) Capnp.Array.t -> (rw, [`Scope_abd73485a9636bc9] builder_t, array_t) Capnp.Array.t
      val scopes_set_list : t -> [`Scope_abd73485a9636bc9] builder_t list -> (rw, [`Scope_abd73485a9636bc9] builder_t, array_t) Capnp.Array.t
      val scopes_set_array : t -> [`Scope_abd73485a9636bc9] builder_t array -> (rw, [`Scope_abd73485a9636bc9] builder_t, array_t) Capnp.Array.t
      val scopes_init : t -> int -> (rw, [`Scope_abd73485a9636bc9] builder_t, array_t) Capnp.Array.t
      val of_message : rw message_t -> t
      val to_message : t -> rw message_t
      val to_reader : t -> struct_t reader_t
      val init_root : ?message_size:int -> unit -> t
      val init_pointer : pointer_t -> t
    end
    module Value : sig
      type struct_t = [`Value_ce23dcd2d7b00c9b]
      type t = struct_t builder_t
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
      val list_set_reader : t -> Reader.pointer_t -> pointer_t
      val list_set_interface : t -> 'a MessageWrapper.Capability.t option -> unit
      val enum_set_exn : t -> int -> unit
      val struct_set : t -> pointer_t -> pointer_t
      val struct_set_reader : t -> Reader.pointer_t -> pointer_t
      val struct_set_interface : t -> 'a MessageWrapper.Capability.t option -> unit
      val interface_set : t -> unit
      val any_pointer_set : t -> pointer_t -> pointer_t
      val any_pointer_set_reader : t -> Reader.pointer_t -> pointer_t
      val any_pointer_set_interface : t -> 'a MessageWrapper.Capability.t option -> unit
      val of_message : rw message_t -> t
      val to_message : t -> rw message_t
      val to_reader : t -> struct_t reader_t
      val init_root : ?message_size:int -> unit -> t
      val init_pointer : pointer_t -> t
    end
    module Annotation : sig
      type struct_t = [`Annotation_f1c8950dab257542]
      type t = struct_t builder_t
      val id_get : t -> Uint64.t
      val id_get_int_exn : t -> int
      val id_set : t -> Uint64.t -> unit
      val id_set_int_exn : t -> int -> unit
      val has_brand : t -> bool
      val brand_get : t -> Brand.t
      val brand_set_reader : t -> Brand.struct_t reader_t -> Brand.t
      val brand_set_builder : t -> Brand.t -> Brand.t
      val brand_init : t -> Brand.t
      val has_value : t -> bool
      val value_get : t -> Value.t
      val value_set_reader : t -> Value.struct_t reader_t -> Value.t
      val value_set_builder : t -> Value.t -> Value.t
      val value_init : t -> Value.t
      val of_message : rw message_t -> t
      val to_message : t -> rw message_t
      val to_reader : t -> struct_t reader_t
      val init_root : ?message_size:int -> unit -> t
      val init_pointer : pointer_t -> t
    end
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
    module CapnpVersion : sig
      type struct_t = [`CapnpVersion_d85d305b7d839963]
      type t = struct_t builder_t
      val major_get : t -> int
      val major_set_exn : t -> int -> unit
      val minor_get : t -> int
      val minor_set_exn : t -> int -> unit
      val micro_get : t -> int
      val micro_set_exn : t -> int -> unit
      val of_message : rw message_t -> t
      val to_message : t -> rw message_t
      val to_reader : t -> struct_t reader_t
      val init_root : ?message_size:int -> unit -> t
      val init_pointer : pointer_t -> t
    end
    module CodeGeneratorRequest : sig
      type struct_t = [`CodeGeneratorRequest_bfc546f6210ad7ce]
      type t = struct_t builder_t
      module RequestedFile : sig
        type struct_t = [`RequestedFile_cfea0eb02e810062]
        type t = struct_t builder_t
        module Import : sig
          type struct_t = [`Import_ae504193122357e5]
          type t = struct_t builder_t
          val id_get : t -> Uint64.t
          val id_get_int_exn : t -> int
          val id_set : t -> Uint64.t -> unit
          val id_set_int_exn : t -> int -> unit
          val has_name : t -> bool
          val name_get : t -> string
          val name_set : t -> string -> unit
          val of_message : rw message_t -> t
          val to_message : t -> rw message_t
          val to_reader : t -> struct_t reader_t
          val init_root : ?message_size:int -> unit -> t
          val init_pointer : pointer_t -> t
        end
        val id_get : t -> Uint64.t
        val id_get_int_exn : t -> int
        val id_set : t -> Uint64.t -> unit
        val id_set_int_exn : t -> int -> unit
        val has_filename : t -> bool
        val filename_get : t -> string
        val filename_set : t -> string -> unit
        val has_imports : t -> bool
        val imports_get : t -> (rw, [`Import_ae504193122357e5] builder_t, array_t) Capnp.Array.t
        val imports_get_list : t -> [`Import_ae504193122357e5] builder_t list
        val imports_get_array : t -> [`Import_ae504193122357e5] builder_t array
        val imports_set : t -> (rw, [`Import_ae504193122357e5] builder_t, array_t) Capnp.Array.t -> (rw, [`Import_ae504193122357e5] builder_t, array_t) Capnp.Array.t
        val imports_set_list : t -> [`Import_ae504193122357e5] builder_t list -> (rw, [`Import_ae504193122357e5] builder_t, array_t) Capnp.Array.t
        val imports_set_array : t -> [`Import_ae504193122357e5] builder_t array -> (rw, [`Import_ae504193122357e5] builder_t, array_t) Capnp.Array.t
        val imports_init : t -> int -> (rw, [`Import_ae504193122357e5] builder_t, array_t) Capnp.Array.t
        val of_message : rw message_t -> t
        val to_message : t -> rw message_t
        val to_reader : t -> struct_t reader_t
        val init_root : ?message_size:int -> unit -> t
        val init_pointer : pointer_t -> t
      end
      val has_capnp_version : t -> bool
      val capnp_version_get : t -> CapnpVersion.t
      val capnp_version_set_reader : t -> CapnpVersion.struct_t reader_t -> CapnpVersion.t
      val capnp_version_set_builder : t -> CapnpVersion.t -> CapnpVersion.t
      val capnp_version_init : t -> CapnpVersion.t
      val has_nodes : t -> bool
      val nodes_get : t -> (rw, Node.t, array_t) Capnp.Array.t
      val nodes_get_list : t -> Node.t list
      val nodes_get_array : t -> Node.t array
      val nodes_set : t -> (rw, Node.t, array_t) Capnp.Array.t -> (rw, Node.t, array_t) Capnp.Array.t
      val nodes_set_list : t -> Node.t list -> (rw, Node.t, array_t) Capnp.Array.t
      val nodes_set_array : t -> Node.t array -> (rw, Node.t, array_t) Capnp.Array.t
      val nodes_init : t -> int -> (rw, Node.t, array_t) Capnp.Array.t
      val has_requested_files : t -> bool
      val requested_files_get : t -> (rw, [`RequestedFile_cfea0eb02e810062] builder_t, array_t) Capnp.Array.t
      val requested_files_get_list : t -> [`RequestedFile_cfea0eb02e810062] builder_t list
      val requested_files_get_array : t -> [`RequestedFile_cfea0eb02e810062] builder_t array
      val requested_files_set : t -> (rw, [`RequestedFile_cfea0eb02e810062] builder_t, array_t) Capnp.Array.t -> (rw, [`RequestedFile_cfea0eb02e810062] builder_t, array_t) Capnp.Array.t
      val requested_files_set_list : t -> [`RequestedFile_cfea0eb02e810062] builder_t list -> (rw, [`RequestedFile_cfea0eb02e810062] builder_t, array_t) Capnp.Array.t
      val requested_files_set_array : t -> [`RequestedFile_cfea0eb02e810062] builder_t array -> (rw, [`RequestedFile_cfea0eb02e810062] builder_t, array_t) Capnp.Array.t
      val requested_files_init : t -> int -> (rw, [`RequestedFile_cfea0eb02e810062] builder_t, array_t) Capnp.Array.t
      val of_message : rw message_t -> t
      val to_message : t -> rw message_t
      val to_reader : t -> struct_t reader_t
      val init_root : ?message_size:int -> unit -> t
      val init_pointer : pointer_t -> t
    end
  end
end

module MakeRPC(MessageWrapper : Capnp.RPC.S) : sig
  include S with module MessageWrapper = MessageWrapper

  module Client : sig
  end

  module Service : sig
  end
end

module Make(M : Capnp.MessageSig.S) : module type of MakeRPC(Capnp.RPC.None(M))
