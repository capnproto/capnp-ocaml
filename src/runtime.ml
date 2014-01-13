
module Array = struct
  type ('a, 'arr) storage_t = {
    storage : 'arr;
    length  : 'arr -> int;
    get     : 'arr -> int -> 'a;
  }

  type ('a, 'arr) t = ('a, 'arr) storage_t option

  let length x =
    match x with
    | Some storage ->
        storage.length storage.storage
    | None ->
        0

  let get x i =
    match x with
    | Some storage ->
        storage.get storage.storage i
    | None ->
        invalid_arg "index out of bounds"

  let make ~length ~get storage = Some { storage; length; get }

  let make_default () = None
end

