open Al.Ast
open Reference_interpreter
open Source

(* Helper *)
let fv f v = ConstructV (f, [v])

(* Construct types *)
(*
let al_of_null = function
  | Types.NoNull -> ConstructV ("NULL", [ OptV None ])
  | Types.Null -> ConstructV ("NULL", [ OptV (Some (listV [])) ])

let al_of_final = function
  | Types.NoFinal -> OptV None
  | Types.Final -> OptV (Some (singleton "FINAL"))

let al_of_mut = function
  | Types.Cons -> OptV None
  | Types.Var -> OptV (Some (singleton "MUT"))

let rec al_of_storage_type = function
  | Types.ValStorageT vt -> al_of_val_type vt
  | Types.PackStorageT ps ->
    (Types.packed_size ps * 8)
    |> string_of_int
    |> Printf.sprintf "I%s"
    |> singleton

and al_of_field_type = function
  | Types.FieldT (mut, st) ->
    PairV (al_of_mut mut, al_of_storage_type st)

and al_of_result_type rt = List.map al_of_val_type rt |> listV

and al_of_str_type = function
  | Types.DefStructT (StructT ftl) ->
    let al_ftl = List.map al_of_field_type ftl |> listV in
    ConstructV ("STRUCT", [ al_ftl ])
  | Types.DefArrayT (ArrayT ft) ->
    ConstructV ("ARRAY", [ al_of_field_type ft ])
  | Types.DefFuncT (FuncT (rt1, rt2)) ->
    ConstructV ("FUNC", [ ArrowV (
      al_of_result_type rt1,
      al_of_result_type rt2
    )])

and al_of_sub_type = function
  | Types.SubT (fin, htl, st) ->
    ConstructV ("SUBD", [
      al_of_final fin;
      List.map al_of_heap_type htl |> listV;
      al_of_str_type st
    ])

and al_of_rec_type = function
  | Types.RecT stl ->
    let al_stl = List.map al_of_sub_type stl |> listV in
    ConstructV ("REC", [ al_stl ])

and al_of_def_type = function
  | Types.DefT (rt, i) ->
    ConstructV ("DEF", [al_of_rec_type rt; NumV (Int64.of_int32 i)])

and al_of_heap_type = function
  | Types.VarHT (StatX i) ->
    ConstructV ("_IDX", [ NumV (Int64.of_int32 i) ])
  | Types.VarHT (RecX i) ->
    ConstructV ("REC", [ NumV (Int64.of_int32 i) ])
  | Types.DefHT dt -> al_of_def_type dt
  | Types.BotHT -> singleton "BOT"
  | ht ->
    Types.string_of_heap_type ht
    |> String.uppercase_ascii
    |> singleton

*)

let al_of_ref_type = function
| Types.FuncRefType -> ConstructV ("REF", [ ConstructV ("NULL", [ OptV None ]); singleton "FUNC" ])
| Types.ExternRefType -> ConstructV ("REF", [ ConstructV ("NULL", [ OptV None ]); singleton "EXTERN" ])
let al_of_val_type = function
| Types.RefType rt -> al_of_ref_type rt
| vt ->
  Types.string_of_value_type vt
  |> String.uppercase_ascii
  |> singleton
let al_of_result_type rt = List.map al_of_val_type rt |> listV
let al_of_func_type func_type = match func_type.it with
| Types.FuncType (rt1, rt2) ->
  let ft = ConstructV ("FUNC", [ ArrowV (
    al_of_result_type rt1,
    al_of_result_type rt2
  )]) in
  ConstructV ("TYPE", [
    ConstructV ("REC", [ listV [
      ConstructV ("SUBD", [
        OptV (Some (singleton "FINAL"));
        listV [];
        ft
    ])]])
  ])

(* Construct value *)

let int64_of_int32_u x = x |> Int64.of_int32 |> Int64.logand 0x0000_0000_ffff_ffffL
let al_of_num n =
  let t, v = match n with
    | Values.I32 i -> "I32", i |> I32.to_bits |> int64_of_int32_u
    | Values.I64 i -> "I64", i |> I64.to_bits
    | Values.F32 f -> "F32", f |> F32.to_bits |> int64_of_int32_u
    | Values.F64 f -> "F64", f |> F64.to_bits in
  let t, v = ConstructV(t, []), NumV v in
  ConstructV ("CONST", [ t; v ])

let al_of_ref = function
  | Values.NullRef rt ->
    ConstructV ("REF.NULL", [ al_of_ref_type rt ])
  (*
  | I31.I31Ref i ->
    ConstructV ("REF.I31_NUM", [ NumV (Int64.of_int i) ])
  | Aggr.StructRef a ->
    ConstructV ("REF.STRUCT_ADDR", [ NumV (int64_of_int32_u a) ])
  | Aggr.ArrayRef a ->
    ConstructV ("REF.ARRAY_ADDR", [ NumV (int64_of_int32_u a) ])
  | Instance.FuncRef a ->
    ConstructV ("REF.FUNC_ADDR", [ NumV (int64_of_int32_u a) ])
  | Script.HostRef a ->
    ConstructV ("REF.HOST_ADDR", [ NumV (int64_of_int32_u a) ])
  | Extern.ExternRef r ->
    ConstructV ("REF.EXTERN", [ al_of_ref r ])
    *)
  | r -> Values.string_of_ref r |> failwith

let al_of_value = function
  | Values.Num n -> al_of_num n
  | Values.Vec _v -> failwith "TODO"
  | Values.Ref r -> al_of_ref r

(* Construct type *)

let al_of_blocktype = function
| Ast.VarBlockType idx -> ConstructV ("_IDX", [ NumV (Int64.of_int32 idx.it) ])
| Ast.ValBlockType None -> ConstructV ("_RESULT", [ OptV None ])
| Ast.ValBlockType (Some val_type) -> ConstructV ("_RESULT", [ OptV (Some (al_of_val_type val_type)) ])


(* Construct instruction *)

let al_of_unop_int = function
  | Ast.IntOp.Clz -> StringV "Clz"
  | Ast.IntOp.Ctz -> StringV "Ctz"
  | Ast.IntOp.Popcnt -> StringV "Popcnt"
  | Ast.IntOp.ExtendS Types.Pack8 -> StringV "Extend8S"
  | Ast.IntOp.ExtendS Types.Pack16 -> StringV "Extend16S"
  | Ast.IntOp.ExtendS Types.Pack32 -> StringV "Extend32S"
  | Ast.IntOp.ExtendS Types.Pack64 -> StringV "Extend64S"
let al_of_unop_float = function
  | Ast.FloatOp.Neg -> StringV "Neg"
  | Ast.FloatOp.Abs -> StringV "Abs"
  | Ast.FloatOp.Ceil -> StringV "Ceil"
  | Ast.FloatOp.Floor -> StringV "Floor"
  | Ast.FloatOp.Trunc -> StringV "Trunc"
  | Ast.FloatOp.Nearest -> StringV "Nearest"
  | Ast.FloatOp.Sqrt -> StringV "Sqrt"

let al_of_binop_int = function
  | Ast.IntOp.Add -> StringV "Add"
  | Ast.IntOp.Sub -> StringV "Sub"
  | Ast.IntOp.Mul -> StringV "Mul"
  | Ast.IntOp.DivS -> StringV "DivS"
  | Ast.IntOp.DivU -> StringV "DivU"
  | Ast.IntOp.RemS -> StringV "RemS"
  | Ast.IntOp.RemU -> StringV "RemU"
  | Ast.IntOp.And -> StringV "And"
  | Ast.IntOp.Or -> StringV "Or"
  | Ast.IntOp.Xor -> StringV "Xor"
  | Ast.IntOp.Shl -> StringV "Shl"
  | Ast.IntOp.ShrS -> StringV "ShrS"
  | Ast.IntOp.ShrU -> StringV "ShrU"
  | Ast.IntOp.Rotl -> StringV "Rotl"
  | Ast.IntOp.Rotr -> StringV "Rotr"
let al_of_binop_float = function
  | Ast.FloatOp.Add -> StringV "Add"
  | Ast.FloatOp.Sub -> StringV "Sub"
  | Ast.FloatOp.Mul -> StringV "Mul"
  | Ast.FloatOp.Div -> StringV "Div"
  | Ast.FloatOp.Min -> StringV "Min"
  | Ast.FloatOp.Max -> StringV "Max"
  | Ast.FloatOp.CopySign -> StringV "CopySign"

let al_of_testop_int = function
  | Ast.IntOp.Eqz -> StringV "Eqz"

let al_of_relop_int = function
  | Ast.IntOp.Eq -> StringV "Eq"
  | Ast.IntOp.Ne -> StringV "Ne"
  | Ast.IntOp.LtS -> StringV "LtS"
  | Ast.IntOp.LtU -> StringV "LtU"
  | Ast.IntOp.GtS -> StringV "GtS"
  | Ast.IntOp.GtU -> StringV "GtU"
  | Ast.IntOp.LeS -> StringV "LeS"
  | Ast.IntOp.LeU -> StringV "LeU"
  | Ast.IntOp.GeS -> StringV "GeS"
  | Ast.IntOp.GeU -> StringV "GeU"
let al_of_relop_float = function
  | Ast.FloatOp.Eq -> StringV "Eq"
  | Ast.FloatOp.Ne -> StringV "Ne"
  | Ast.FloatOp.Lt -> StringV "Lt"
  | Ast.FloatOp.Gt -> StringV "Gt"
  | Ast.FloatOp.Le -> StringV "Le"
  | Ast.FloatOp.Ge -> StringV "Ge"

let al_of_cvtop_int bit_num = function
  | Ast.IntOp.ExtendSI32 -> "Extend", "I32", Some (singleton "S")
  | Ast.IntOp.ExtendUI32 -> "Extend", "I32", Some (singleton "U")
  | Ast.IntOp.WrapI64 -> "Wrap", "I64", None
  | Ast.IntOp.TruncSF32 -> "Trunc", "F32", Some (singleton "S")
  | Ast.IntOp.TruncUF32 -> "Trunc", "F32", Some (singleton "U")
  | Ast.IntOp.TruncSF64 -> "Trunc", "F64", Some (singleton "S")
  | Ast.IntOp.TruncUF64 -> "Trunc", "F64", Some (singleton "U")
  | Ast.IntOp.TruncSatSF32 -> "TruncSat", "F32", Some (singleton "S")
  | Ast.IntOp.TruncSatUF32 -> "TruncSat", "F32", Some (singleton "U")
  | Ast.IntOp.TruncSatSF64 -> "TruncSat", "F64", Some (singleton "S")
  | Ast.IntOp.TruncSatUF64 -> "TruncSat", "F64", Some (singleton "U")
  | Ast.IntOp.ReinterpretFloat -> "Reinterpret", ("F" ^ bit_num), None
let al_of_cvtop_float bit_num = function
  | Ast.FloatOp.ConvertSI32 -> "Convert", "I32", Some (singleton "S")
  | Ast.FloatOp.ConvertUI32 -> "Convert", "I32", Some (singleton "U")
  | Ast.FloatOp.ConvertSI64 -> "Convert", "I64", Some (singleton "S")
  | Ast.FloatOp.ConvertUI64 -> "Convert", "I64", Some (singleton "U")
  | Ast.FloatOp.PromoteF32 -> "Promote", "F32", None
  | Ast.FloatOp.DemoteF64 -> "Demote", "F64", None
  | Ast.FloatOp.ReinterpretInt -> "Reinterpret", ("I" ^ bit_num), None

let al_of_packsize p =
  let s = match p with
    | Types.Pack8 -> 8
    | Types.Pack16 -> 16
    | Types.Pack32 -> 32
    | Types.Pack64 -> 64
  in
  NumV (Int64.of_int s)

let al_of_extension = function
| Types.SX -> singleton "S"
| Types.ZX -> singleton "U"

let al_of_packsize_with_extension (p, s) =
  listV [ al_of_packsize p; al_of_extension s ]


let rec al_of_instr winstr =
  let to_int i32 = NumV (int64_of_int32_u i32.it) in
  let f name  = ConstructV (name, []) in
  (* let f_v name v = ConstructV (name, [v]) in *)
  let f_i32 name i32 = ConstructV (name, [to_int i32]) in
  let f_i32_i32 name i32 i32' = ConstructV (name, [to_int i32; to_int i32']) in

  match winstr.it with
  (* wasm values *)
  | Ast.Const num -> al_of_num num.it
  | Ast.RefNull t -> al_of_value (Values.Ref (Values.NullRef t))
  (* wasm instructions *)
  | Ast.Unreachable -> f "UNREACHABLE"
  | Ast.Nop -> f "NOP"
  | Ast.Drop -> f "DROP"
  | Ast.Unary op ->
    let (ty, op) = (
      match op with
      | Values.I32 op -> ("I32", al_of_unop_int op)
      | Values.I64 op -> ("I64", al_of_unop_int op)
      | Values.F32 op -> ("F32", al_of_unop_float op)
      | Values.F64 op -> ("F64", al_of_unop_float op))
    in
    ConstructV ("UNOP", [ singleton ty; op ])
  | Ast.Binary op ->
    let (ty, op) = (
      match op with
      | Values.I32 op -> ("I32", al_of_binop_int op)
      | Values.I64 op -> ("I64", al_of_binop_int op)
      | Values.F32 op -> ("F32", al_of_binop_float op)
      | Values.F64 op -> ("F64", al_of_binop_float op))
    in
    ConstructV ("BINOP", [ singleton ty; op ])
  | Ast.Test op ->
    let (ty, op) = (
      match op with
      | Values.I32 op -> ("I32", al_of_testop_int op)
      | Values.I64 op -> ("I64", al_of_testop_int op)
      | _ -> .)
    in
    ConstructV ("TESTOP", [ singleton ty; op ])
  | Ast.Compare op ->
    let (ty, op) = (
      match op with
      | Values.I32 op -> ("I32", al_of_relop_int op)
      | Values.I64 op -> ("I64", al_of_relop_int op)
      | Values.F32 op -> ("F32", al_of_relop_float op)
      | Values.F64 op -> ("F64", al_of_relop_float op))
    in
    ConstructV ("RELOP", [ singleton ty; op ])
  | Ast.Convert op ->
    let (ty_to, (op, ty_from, sx_opt)) = (
      match op with
      | Values.I32 op -> ("I32", al_of_cvtop_int "32" op)
      | Values.I64 op -> ("I64", al_of_cvtop_int "64" op)
      | Values.F32 op -> ("F32", al_of_cvtop_float "32" op)
      | Values.F64 op -> ("F64", al_of_cvtop_float "64" op))
    in
    ConstructV ("CVTOP", [ singleton ty_to; StringV op; singleton ty_from; OptV sx_opt ])
  | Ast.RefIsNull -> f "REF.IS_NULL"
  | Ast.RefFunc i32 -> f_i32 "REF.FUNC" i32
  | Ast.Select None -> ConstructV ("SELECT", [ OptV None ])
  | Ast.Select (Some ts) -> ConstructV ("SELECT", [ OptV (Some (listV (List.map al_of_val_type ts))) ])
  | Ast.LocalGet i32 -> f_i32 "LOCAL.GET" i32
  | Ast.LocalSet i32 -> f_i32 "LOCAL.SET" i32
  | Ast.LocalTee i32 -> f_i32 "LOCAL.TEE" i32
  | Ast.GlobalGet i32 -> f_i32 "GLOBAL.GET" i32
  | Ast.GlobalSet i32 -> f_i32 "GLOBAL.SET" i32
  | Ast.TableGet i32 -> f_i32 "TABLE.GET" i32
  | Ast.TableSet i32 -> f_i32 "TABLE.SET" i32
  | Ast.TableSize i32 -> f_i32 "TABLE.SIZE" i32
  | Ast.TableGrow i32 -> f_i32 "TABLE.GROW" i32
  | Ast.TableFill i32 -> f_i32 "TABLE.FILL" i32
  | Ast.TableCopy (i32, i32') -> f_i32_i32 "TABLE.COPY" i32 i32'
  | Ast.TableInit (i32, i32') -> f_i32_i32 "TABLE.INIT" i32 i32'
  | Ast.ElemDrop i32 -> f_i32 "ELEM.DROP" i32
  | Ast.Block (bt, instrs) ->
      ConstructV
        ("BLOCK", [
            al_of_blocktype bt;
            listV (instrs |> al_of_instrs)])
  | Ast.Loop (bt, instrs) ->
      ConstructV
        ("LOOP", [
            al_of_blocktype bt;
            listV (instrs |> al_of_instrs)])
  | Ast.If (bt, instrs1, instrs2) ->
      ConstructV
        ("IF", [
            al_of_blocktype bt;
            listV (instrs1 |> al_of_instrs);
            listV (instrs2 |> al_of_instrs);
            ])
  | Ast.Br i32 -> f_i32 "BR" i32
  | Ast.BrIf i32 -> f_i32 "BR_IF" i32
  | Ast.BrTable (i32s, i32) ->
      ConstructV
        ("BR_TABLE", [ listV (i32s |> List.map to_int); to_int i32 ])
  (*
  | Ast.BrOnNull i32 -> f_i32 "BR_ON_NULL" i32
  | Ast.BrOnNonNull i32 -> f_i32 "BR_ON_NON_NULL" i32
  | Ast.BrOnCast (i32, rt1, rt2) ->
      ConstructV
        ("BR_ON_CAST", [
            to_int i32;
            al_of_ref_type rt1;
            al_of_ref_type rt2;
            ])
  | Ast.BrOnCastFail (i32, rt1, rt2) ->
      ConstructV
        ("BR_ON_CAST_FAIL", [
            to_int i32;
            al_of_ref_type rt1;
            al_of_ref_type rt2;
            ])
  *)
  | Ast.Return -> f "RETURN"
  | Ast.Call i32 -> f_i32 "CALL" i32
  (* | Ast.CallRef i32 -> f_v "CALL_REF" (OptV (Some (to_int i32))) *)
  | Ast.CallIndirect (i32, i32') -> f_i32_i32 "CALL_INDIRECT" i32 i32'
  (*
  | Ast.ReturnCall i32 -> f_i32 "RETURN_CALL"i32
  | Ast.ReturnCallRef i32 -> f_v "RETURN_CALL_REF" (OptV (Some (to_int i32)))
  | Ast.ReturnCallIndirect (i32, i32') -> f_i32_i32 "RETURN_CALL_INDIRECT" i32 i32'
  *)
  | Ast.Load {ty = ty; align = align; offset = offset; pack = pack} ->
      ConstructV
        ("LOAD", [
            al_of_val_type (Types.NumType ty);
            OptV (Option.map al_of_packsize_with_extension pack);
            NumV 0L;
            RecordV (Record.empty
              |> Record.add "ALIGN" (NumV (Int64.of_int align))
              |> Record.add "OFFSET" (NumV (int64_of_int32_u offset))
            )
        ])
  | Ast.Store {ty = ty; align = align; offset = offset; pack = pack} ->
      ConstructV
        ("STORE", [
            al_of_val_type (Types.NumType ty);
            OptV (Option.map al_of_packsize pack);
            NumV 0L;
            RecordV (Record.empty
              |> Record.add "ALIGN" (NumV (Int64.of_int align))
              |> Record.add "OFFSET" (NumV (int64_of_int32_u offset))
            )
        ])
  | Ast.MemorySize -> ConstructV ("MEMORY.SIZE", [ NumV 0L ])
  | Ast.MemoryGrow -> ConstructV ("MEMORY.GROW", [ NumV 0L ])
  | Ast.MemoryFill -> ConstructV ("MEMORY.FILL", [ NumV 0L ])
  | Ast.MemoryCopy -> ConstructV ("MEMORY.COPY", [ NumV 0L; NumV 0L ])
  | Ast.MemoryInit i32 -> ConstructV ("MEMORY.INIT", [ NumV 0L; to_int i32 ])
  | Ast.DataDrop i32 -> f_i32 "DATA.DROP" i32
  (*
  | Ast.RefAsNonNull -> f "REF.AS_NON_NULL"
  | Ast.RefTest rt -> ConstructV ("REF.TEST", [ al_of_ref_type rt ])
  | Ast.RefCast rt -> ConstructV ("REF.CAST", [ al_of_ref_type rt ])
  | Ast.RefEq -> f "REF.EQ"
  | Ast.RefI31 -> f "REF.I31"
  | Ast.I31Get sx -> ConstructV ("I31.GET", [ al_of_extension sx ])
  | Ast.StructNew (i32, Ast.Explicit) -> f_i32 "STRUCT.NEW" i32
  | Ast.StructNew (i32, Ast.Implicit) -> f_i32 "STRUCT.NEW_DEFAULT" i32
  | Ast.StructGet (i32, i32', sx_opt) ->
      ConstructV ("STRUCT.GET", [
        OptV (Option.map al_of_extension sx_opt);
        to_int i32;
        to_int i32'
      ])
  | Ast.StructSet (i32, i32') -> f_i32_i32 "STRUCT.SET" i32 i32'
  | Ast.ArrayNew (i32, Ast.Explicit) -> f_i32 "ARRAY.NEW" i32
  | Ast.ArrayNew (i32, Ast.Implicit) -> f_i32 "ARRAY.NEW_DEFAULT" i32
  | Ast.ArrayNewFixed (i32, n) -> ConstructV ("ARRAY.NEW_FIXED", [ to_int i32; NumV (int64_of_int32_u n) ])
  | Ast.ArrayNewElem (i32, i32') -> f_i32_i32 "ARRAY.NEW_ELEM" i32 i32'
  | Ast.ArrayNewData (i32, i32') -> f_i32_i32 "ARRAY.NEW_DATA" i32 i32'
  | Ast.ArrayGet (i32, sx_opt) ->
      ConstructV
        ("ARRAY.GET", [
            OptV (Option.map al_of_extension sx_opt);
            to_int i32
        ])
  | Ast.ArraySet i32 -> f_i32 "ARRAY.SET" i32
  | Ast.ArrayLen -> f "ARRAY.LEN"
  | Ast.ArrayCopy (i32, i32') -> f_i32_i32 "ARRAY.COPY" i32 i32'
  | Ast.ArrayFill i32 -> f_i32 "ARRAY.FILL" i32
  | Ast.ArrayInitData (i32, i32') -> f_i32_i32 "ARRAY.INIT_DATA" i32 i32'
  | Ast.ArrayInitElem (i32, i32') -> f_i32_i32 "ARRAY.INIT_ELEM" i32 i32'
  | Ast.ExternConvert Ast.Internalize -> f "ANY.CONVERT_EXTERN"
  | Ast.ExternConvert Ast.Externalize -> f "EXTERN.CONVERT_ANY"
  *)
  | _ -> ConstructV ("TODO: Unconstructed Wasm instruction (al_of_instr)", [])

and al_of_instrs winstrs = List.map al_of_instr winstrs



(* Construct module *)

let it phrase = phrase.it

let al_of_func wasm_func =

  let ftype =
    NumV (Int64.of_int32 wasm_func.it.Ast.ftype.it)
  in

  (* Construct locals *)
  let locals =
    List.map
      (fun t ->
        ConstructV ("LOCAL", [ al_of_val_type t ]))
      wasm_func.it.Ast.locals
  in

  (* Construct code *)
  let code = al_of_instrs wasm_func.it.Ast.body in

  (* Construct func *)
  ConstructV ("FUNC", [ftype; listV locals; listV code])

let al_of_global wasm_global =
  let expr = al_of_instrs wasm_global.it.Ast.ginit.it in

  ConstructV ("GLOBAL", [ StringV "Yet: global type"; listV expr ])

let al_of_limits limits max =
  let max =
    match limits.Types.max with
    | Some v -> int64_of_int32_u v
    | None -> max
  in

  PairV (NumV (int64_of_int32_u limits.Types.min), NumV max)

let al_of_table wasm_table =

  let Types.TableType (limits, ref_ty) = wasm_table.it.Ast.ttype in
  let pair = al_of_limits limits 4294967295L in

  let expr = [ fv "REF.NULL" (singleton "FUNC") ] in

  ConstructV ("TABLE", [ PairV(pair, al_of_val_type (Types.RefType ref_ty)); listV expr ])

let al_of_memory wasm_memory =
  let Types.MemoryType (limits) = wasm_memory.it.Ast.mtype in
  let pair = al_of_limits limits 65536L in

  ConstructV ("MEMORY", [ ConstructV ("I8", [ pair]) ])

let al_of_segment wasm_segment = match wasm_segment.it with
  | Ast.Passive -> singleton "PASSIVE"
  | Ast.Active { index = index; offset = offset } ->
      ConstructV (
        "ACTIVE",
        [
          NumV (int64_of_int32_u index.it);
          listV (al_of_instrs offset.it)
        ]
      )
  | Ast.Declarative -> singleton "DECLARE"

let al_of_elem wasm_elem =
  let reftype = al_of_val_type (Types.RefType wasm_elem.it.Ast.etype) in

  let al_of_const const = listV (al_of_instrs const.it) in
  let instrs = wasm_elem.it.Ast.einit |> List.map al_of_const in

  let mode = al_of_segment wasm_elem.it.Ast.emode in

  ConstructV ("ELEM", [ reftype; listV instrs; mode ])

let al_of_data wasm_data =
  (* TODO: byte list list *)
  let init = wasm_data.it.Ast.dinit in

  let f chr acc = NumV (Int64.of_int (Char.code chr)) :: acc in
  let byte_list = String.fold_right f init [] in

  let mode = al_of_segment wasm_data.it.Ast.dmode in

  ConstructV ("DATA", [ listV byte_list; mode ])

let al_of_import_desc _wasm_module idesc = match idesc.it with
  | Ast.FuncImport _x ->
      (*
      let dts = Ast.def_types_of wasm_module in
      let dt = Lib.List32.nth dts x.it |> al_of_def_type in
      ConstructV ("FUNC", [ dt ])
      *)
      ConstructV ("FUNC",  [ StringV "Yet: FuncImport" ])
  | Ast.TableImport ty ->
    let Types.TableType (limits, ref_ty) = ty in
    let pair = al_of_limits limits 4294967295L in
    ConstructV ("TABLE", [ pair; al_of_val_type (Types.RefType ref_ty) ])
  | Ast.MemoryImport ty ->
    let Types.MemoryType (limits) = ty in
    let pair = al_of_limits limits 65536L in
    ConstructV ("MEM", [ pair ])
  | Ast.GlobalImport _ -> ConstructV ("GLOBAL", [ StringV "Yet: global type" ])

let al_of_import wasm_module wasm_import =

  let module_name = StringV (wasm_import.it.Ast.module_name |> Utf8.encode) in
  let item_name = StringV (wasm_import.it.Ast.item_name |> Utf8.encode) in

  let import_desc = al_of_import_desc wasm_module wasm_import.it.Ast.idesc in

  ConstructV ("IMPORT", [ module_name; item_name; import_desc ])

let al_of_export_desc export_desc = match export_desc.it with
  | Ast.FuncExport n -> ConstructV ("FUNC", [ NumV (int64_of_int32_u n.it) ])
  | Ast.TableExport n -> ConstructV ("TABLE", [ NumV (int64_of_int32_u n.it) ])
  | Ast.MemoryExport n -> ConstructV ("MEM", [ NumV (int64_of_int32_u n.it) ])
  | Ast.GlobalExport n -> ConstructV ("GLOBAL", [ NumV (int64_of_int32_u n.it) ])

let al_of_start wasm_start =
  ConstructV ("START", [ NumV (int64_of_int32_u wasm_start.it.Ast.sfunc.it) ])

let al_of_export wasm_export =

  let name = StringV (wasm_export.it.Ast.name |> Utf8.encode) in
  let export_desc = al_of_export_desc wasm_export.it.Ast.edesc in

  ConstructV ("EXPORT", [ name; export_desc ])

let al_of_module wasm_module =

  (* Construct types *)
  let type_list =
    List.map al_of_func_type wasm_module.it.Ast.types
  in

  (* Construct imports *)
  let import_list =
    List.map (al_of_import wasm_module) wasm_module.it.imports
  in

  (* Construct functions *)
  let func_list =
    List.map al_of_func wasm_module.it.funcs
  in

  (* Construct global *)
  let global_list =
    List.map al_of_global wasm_module.it.globals
  in

  (* Construct table *)
  let table_list =
    List.map al_of_table wasm_module.it.tables
  in

  (* Construct memory *)
  let memory_list =
    List.map al_of_memory wasm_module.it.memories
  in

  (* Construct elem *)
  let elem_list =
    List.map al_of_elem wasm_module.it.elems
  in

  (* Construct data *)
  let data_list =
    List.map al_of_data wasm_module.it.datas
  in

  (* Construct start *)
  let start_opt =
    Option.map al_of_start wasm_module.it.start
  in

  (* Construct export *)
  let export_list =
    List.map al_of_export wasm_module.it.exports
  in

  (* print_endline "";
  Print.string_of_value (listV import_list) |> print_endline;
  Print.string_of_value (listV func_list) |> print_endline;
  Print.string_of_value (listV global_list) |> print_endline;
  Print.string_of_value (listV table_list) |> print_endline;
  Print.string_of_value (listV memory_list) |> print_endline;
  Print.string_of_value (listV elem_list) |> print_endline;
  Print.string_of_value (listV data_list) |> print_endline;
  Print.string_of_value (listV export_list) |> print_endline;*)

  ConstructV (
    "MODULE",
    [
      listV type_list;
      listV import_list;
      listV func_list;
      listV global_list;
      listV table_list;
      listV memory_list;
      listV elem_list;
      listV data_list;
      OptV  start_opt;
      listV export_list
    ]
  )

let fail ty v =
  Al.Print.string_of_value v
  |> Printf.sprintf "Invalid %s: %s" ty
  |> failwith

(*
open Types

let al_to_null: value -> null = function
  | ConstructV ("NULL", [ OptV None ]) -> NoNull
  | ConstructV ("NULL", [ OptV _ ]) -> Null
  | v -> fail "null" v

let al_to_final: value -> final = function
  | OptV None -> NoFinal
  | OptV (Some (ConstructV ("FINAL", []))) -> Final
  | v -> fail "final" v

let al_to_mut: value -> mut = function
  | OptV None -> Cons
  | OptV (Some (ConstructV ("MUT", []))) -> Var
  | v -> fail "mut" v

let rec al_to_storage_type: value -> storage_type = function
  | ConstructV ("I8", []) -> PackStorageType Pack8
  | ConstructV ("I16", []) -> PackStorageType Pack16
  | v -> ValStorageType (al_to_val_type v)

and al_to_field_type: value -> field_type = function
  | PairV (mut, st) ->
    FieldType (al_to_mut mut, al_to_storage_type st)
  | v -> fail "field type" v

and al_to_result_type: value -> result_type = function
  | ListV vtl ->
    let vtl' = Array.to_list !vtl in
    List.map al_to_val_type vtl'
  | v -> fail "result type" v

and al_to_str_type: value -> str_type = function
  | ConstructV ("STRUCT", [ ListV ftl ]) ->
    let ftl' = Array.to_list !ftl in
    DefStructType (StructType (List.map al_to_field_type ftl'))
  | ConstructV ("ARRAY", [ ft ]) ->
    DefArrayType (ArrayType (al_to_field_type ft))
  | ConstructV ("FUNC", [ ArrowV (rt1, rt2) ]) ->
    DefFuncType (FuncType (al_to_result_type rt1, (al_to_result_type rt2)))
  | v -> fail "str type" v

and al_to_sub_type: value -> sub_type = function
  | ConstructV ("SUBD", [ fin; ListV htl; st ]) ->
    let htl' = Array.to_list !htl in
    SubType (
      al_to_final fin,
      List.map al_to_heap_type htl',
      al_to_str_type st
    )
  | v -> fail "sub type" v

and al_to_rec_type: value -> rec_type = function
  | ConstructV ("REC", [ ListV stl ]) ->
    let stl' = Array.to_list !stl in
    RecType (List.map al_to_sub_type stl')
  | v -> fail "rec type" v

and al_to_def_type: value -> def_type = function
  | ConstructV ("DEF", [ rt; NumV i ]) ->
    DefType (al_to_rec_type rt, Int64.to_int32 i)
  | v -> fail "def type" v

and al_to_heap_type: value -> heap_type = function
  | ConstructV ("_IDX", [ NumV i ]) ->
    VarHType (StatX (Int64.to_int32 i))
  | ConstructV ("REC", [ NumV i ]) ->
    VarHType (RecX (Int64.to_int32 i))
  | ConstructV ("DEF", _) as v ->
    DefHType (al_to_def_type v)
  | ConstructV (tag, []) as v ->
    begin match tag with
    | "BOT" -> BotHT
    | "ANY" -> AnyHT
    | "NONE" -> NoneHT
    | "EQ" -> EqHT
    | "I31" -> I31HT
    | "STRUCT" -> StructHT
    | "ARRAY" -> ArrayHT
    | "FUNC" -> FuncHT
    | "NOFUNC" -> NoFuncHT
    | "EXTERN" -> ExternHT
    | "NOEXTERN" -> NoExternHT
    | _ -> fail "abstract heap type" v
    end
  | v -> fail "heap type" v

and al_to_ref_type: value -> ref_type = function
  | ConstructV ("REF", [ n; ht ]) ->
    al_to_null n, al_to_heap_type ht
  | v -> fail "ref type" v

and al_to_val_type: value -> val_type = function
  | ConstructV ("I32", []) -> NumType I32T
  | ConstructV ("I64", []) -> NumType I64T
  | ConstructV ("F32", []) -> NumType F32T
  | ConstructV ("F64", []) -> NumType F64T
  | ConstructV ("V128", []) -> VecType V128T
  | ConstructV ("REF", _) as v ->
    RefType (al_to_ref_type v)
  | ConstructV ("BOT", []) -> BotT
  | v -> fail "val type" v
*)
