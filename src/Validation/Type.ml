type operand_type =
  | Descriptor      (* single type descriptor *)
  | ClassSpec
  | MethodSpec
  | FieldSpec
  | RegisterIndex
  | BranchReference
  | Value
  | IntValue
  | StringValue

  (*
let capture_operand_have_type = fun (operand :AST.operand) (operand_type :operand_type) ->
  match operand with
  | AST.CaptureName (_) -> true
  | ASt.Name {value = value; _} ->
     (match operand_type with
      | Descriptor
   *)
