
module E :
  Eval.S with type 'cap message_t = 'cap Capnp.BytesMessage.Message.t
= struct
  module MessageWrapper = Capnp.BytesMessage
  INCLUDE "eval_defun.ml"
end

open Core_kernel.Std

module TestCase = struct
  type request_reader_t   = E.Reader.Expression.t
  type request_builder_t  = E.Builder.Expression.t
  type response_reader_t  = E.Reader.EvaluationResult.t
  type response_builder_t = E.Builder.EvaluationResult.t
  type expectation_t      = int

  let operation_of_int v =
    match v with
    | 0 -> E.Reader.Operation.Add
    | 1 -> E.Reader.Operation.Subtract
    | 2 -> E.Reader.Operation.Multiply
    | 3 -> E.Reader.Operation.Divide
    | 4 -> E.Reader.Operation.Modulus
    | _ -> failwith "bad operation"

  let num_operations = 5

  let safe_divide a b =
    if b = 0 then
      0x7fffffff
    else if a = -0x80000000 && b = -1 then
      0x7fffffff
    else
      a / b

  let safe_mod a b =
    if b = 0 then
      0x7fffffff
    else if a = -0x80000000 && b = -1 then
      0x7fffffff
    else
      a mod b


  (* C++ benchmark invokes undefined behavior on int32_t overflow. :-\
     We have to pick a convention for how we want to handle such cases. *)
  let clamp_int32 x =
    if x < -0x80000000 then
      -0x80000000
    else if x > 0x7fffffff then
      0x7fffffff
    else
      x


  let rec make_expression builder depth =
    let operation = operation_of_int (FastRand.int num_operations) in
    E.Builder.Expression.op_set builder operation;

    let left_value =
      let left = E.Builder.Expression.left_get builder in
      if FastRand.int 8 < depth then begin
        let lv = (FastRand.int 128) + 1 in
        E.Builder.Expression.Left.value_set_int_exn left lv;
        lv
      end else
        let nested_exp = E.Builder.Expression.Left.expression_init left in
        make_expression nested_exp (depth + 1)
    in

    let right_value =
      let right = E.Builder.Expression.right_get builder in
      if FastRand.int 8 < depth then begin
        let rv = (FastRand.int 128) + 1 in
        E.Builder.Expression.Right.value_set_int_exn right rv;
        rv
      end else
        let nested_exp = E.Builder.Expression.Right.expression_init right in
        make_expression nested_exp (depth + 1)
    in

    match E.Builder.Expression.op_get builder with
    | E.Builder.Operation.Add ->
        clamp_int32 (left_value + right_value)
    | E.Builder.Operation.Subtract ->
        clamp_int32 (left_value - right_value)
    | E.Builder.Operation.Multiply ->
        clamp_int32 (left_value * right_value)
    | E.Builder.Operation.Divide ->
        safe_divide left_value right_value
    | E.Builder.Operation.Modulus ->
        safe_mod left_value right_value
    | E.Builder.Operation.Undefined _ ->
        failwith "undefined operation"


  let rec evaluate_expression reader =
    let left_value =
      let left  = E.Reader.Expression.left_get reader in
      match E.Reader.Expression.Left.get left with
      | E.Reader.Expression.Left.Value x32 ->
          Int32.to_int_exn x32
      | E.Reader.Expression.Left.Expression nested_exp ->
          evaluate_expression nested_exp
      | E.Reader.Expression.Left.Undefined _ ->
          failwith "undefined left expression"
    in

    let right_value =
      let right = E.Reader.Expression.right_get reader in
      match E.Reader.Expression.Right.get right with
      | E.Reader.Expression.Right.Value x32 ->
          Int32.to_int_exn x32
      | E.Reader.Expression.Right.Expression nested_exp ->
          evaluate_expression nested_exp
      | E.Reader.Expression.Right.Undefined _ ->
          failwith "undefined right expression"
    in

    match E.Reader.Expression.op_get reader with
    | E.Reader.Operation.Add ->
        clamp_int32 (left_value + right_value)
    | E.Reader.Operation.Subtract ->
        clamp_int32 (left_value - right_value)
    | E.Reader.Operation.Multiply ->
        clamp_int32 (left_value * right_value)
    | E.Reader.Operation.Divide ->
        safe_divide left_value right_value
    | E.Reader.Operation.Modulus ->
        safe_mod left_value right_value
    | E.Reader.Operation.Undefined _ ->
        failwith "undefined operation"


  let setup_request () =
    let builder = E.Builder.Expression.init_root ~message_size:1024 () in
    (builder, make_expression builder 0)

  let handle_request reader =
    let v = evaluate_expression reader in
    let builder = E.Builder.EvaluationResult.init_root ~message_size:1024 () in
    E.Builder.EvaluationResult.value_set_int_exn builder v;
    builder

  let check_response result expected =
    (E.Reader.EvaluationResult.value_get_int_exn result) = expected

end
