type operand_type =
  | Descriptor
  | ClassSpec
  | MethodSpec
  | FieldSpec
  | RegisterIndex
  | BranchReference
  | Value
  | IntValue
  | StringValue

type builtin_instruction =
  | Nop         | I2c         | New         | Instanceof  | Checkcast
  | Imul        | Ineg        | Irem        | Isub        | Idiv
  | Iadd        | Iinc        | Goto        | Ifeq        | Ifne
  | If_acmpeq   | If_acmpne   | Ifnull      | Ifnonnull   | If_icmpeq
  | If_icmpgt   | If_icmplt   | If_icmple   | If_icmpge   | If_icmpne
  | Ireturn     | Areturn     | Return      | Aload       | Astore
  | Iload       | Istore      | Dup         | Pop         | Swap
  | Ldc         | Aconst_null | Getfield    | Putfield    | Invokevirtual
  | Invokenonvirtual
                          
let instruction_info =
  [
    (* AST Name        , Operands *) 
    (  Nop             , [                       ]);
    (  I2c             , [                       ]);
    (  New             , [ClassSpec              ]);
    (  Instanceof      , [ClassSpec              ]);
    (  Checkcast       , [ClassSpec              ]);
    (  Imul            , [                       ]);
    (  Ineg            , [                       ]);
    (  Irem            , [                       ]);
    (  Isub            , [                       ]);
    (  Idiv            , [                       ]);
    (  Iadd            , [                       ]);
    (  Iinc            , [RegisterIndex; IntValue]);
    (  Goto            , [BranchReference        ]);
    (  Ifeq            , [BranchReference        ]);
    (  Ifne            , [BranchReference        ]);
    (  If_acmpeq       , [BranchReference        ]);
    (  If_acmpne       , [BranchReference        ]);
    (  Ifnull          , [BranchReference        ]);
    (  Ifnonnull       , [BranchReference        ]);
    (  If_icmpeq       , [BranchReference        ]);
    (  If_icmpgt       , [BranchReference        ]);
    (  If_icmplt       , [BranchReference        ]);
    (  If_icmple       , [BranchReference        ]);
    (  If_icmpge       , [BranchReference        ]);
    (  If_icmpne       , [BranchReference        ]);
    (  Ireturn         , [                       ]);
    (  Areturn         , [                       ]);
    (  Return          , [                       ]);
    (  Aload           , [RegisterIndex          ]);
    (  Astore          , [RegisterIndex          ]);
    (  Iload           , [RegisterIndex          ]);
    (  Istore          , [RegisterIndex          ]);
    (  Dup             , [                       ]);
    (  Pop             , [                       ]);
    (  Swap            , [                       ]);
    (  Ldc             , [Value                  ]);
    (  Aconst_null     , [                       ]);
    (  Getfield        , [FieldSpec; Descriptor  ]);
    (  Putfield        , [FieldSpec; Descriptor  ]);
    (  Invokevirtual   , [MethodSpec             ]);
    (  Invokenonvirtual, [MethodSpec             ])
  ]

let query_instruction_info = fun (instruction :builtin_instruction) ->
  try
    List.find (fun (instruction', _) -> instruction' = instruction) instruction_info
  with Not_found ->
    raise (Failure "no record found in the instruction info data set")

let select_operands_info = fun (_, operands) -> operands

let translate_from_ast_node = fun (ast_node :AST.instruction) ->
  match ast_node with
  | AST.InstNop (_)              -> Nop
  | AST.InstI2c (_)              -> I2c
  | AST.InstNew (_)              -> New
  | AST.InstInstanceof (_)       -> Instanceof
  | AST.InstCheckcast (_)        -> Checkcast
  | AST.InstImul (_)             -> Imul
  | AST.InstIneg (_)             -> Ineg
  | AST.InstIrem (_)             -> Irem
  | AST.InstIsub (_)             -> Isub
  | AST.InstIdiv (_)             -> Idiv
  | AST.InstIadd (_)             -> Iadd
  | AST.InstIinc (_)             -> Iinc
  | AST.InstGoto (_)             -> Goto
  | AST.InstIfeq (_)             -> Ifeq
  | AST.InstIfne (_)             -> Ifne
  | AST.InstIf_acmpeq (_)        -> If_acmpeq
  | AST.InstIf_acmpne (_)        -> If_acmpne
  | AST.InstIfnull (_)           -> Ifnull
  | AST.InstIfnonnull (_)        -> Ifnonnull
  | AST.InstIf_icmpeq (_)        -> If_icmpeq
  | AST.InstIf_icmpgt (_)        -> If_icmpgt
  | AST.InstIf_icmplt (_)        -> If_icmplt
  | AST.InstIf_icmple (_)        -> If_icmple
  | AST.InstIf_icmpge (_)        -> If_icmpge
  | AST.InstIf_icmpne (_)        -> If_icmpne
  | AST.InstIreturn (_)          -> Ireturn
  | AST.InstAreturn (_)          -> Areturn
  | AST.InstReturn (_)           -> Return
  | AST.InstAload (_)            -> Aload
  | AST.InstAstore (_)           -> Astore
  | AST.InstIload (_)            -> Iload
  | AST.InstIstore (_)           -> Istore
  | AST.InstDup (_)              -> Dup
  | AST.InstPop (_)              -> Pop
  | AST.InstSwap (_)             -> Swap
  | AST.InstLdc (_)              -> Ldc
  | AST.InstAconst_null (_)      -> Aconst_null
  | AST.InstGetfield (_)         -> Getfield
  | AST.InstPutfield (_)         -> Putfield
  | AST.InstInvokevirtual (_)    -> Invokevirtual
  | AST.InstInvokenonvirtual (_) -> Invokenonvirtual
  | AST.InstUserDefined (_)      -> raise (invalid_arg "cannot directly translate user defined alias")
