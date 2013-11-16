
module Make (Storage : MessageStorage.S) = struct
  module Reader = MessageReader.Make(Storage)
  open Reader

  module NestedNode = struct
    type 'cap t = 'cap StructStorage.t option

    let name_get (node : 'cap t) : string =
      get_struct_text_field node 0

    let id_get (node : 'cap t) : Uint64.t =
      Util.uint64_le_of_array (get_struct_byte_field node 0 8)


  end

end

