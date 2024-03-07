open Util
open Source
open El
open Ast
open Print

module Il = struct include Il include Ast end

module Set = Free.Set
module Map = Map.Make(String)

let filter_nl xs = List.filter_map (function Nl -> None | Elem x -> Some x) xs
let map_nl_list f xs = List.map f (filter_nl xs)
let iter_nl_list f xs = List.iter f (filter_nl xs)


(* Errors *)

let error at msg = Source.error at "type" msg

let error_atom at atom t msg =
  error at (msg ^ " `" ^ string_of_atom atom ^ "` in type `" ^ string_of_typ t ^ "`")

let error_id id msg =
  error id.at (msg ^ " `" ^ id.it ^ "`")

let error_typ at phrase t =
  error at (phrase ^ " does not match expected type `" ^ string_of_typ t ^ "`")

let error_typ2 at phrase t1 t2 reason =
  error at (phrase ^ "'s type `" ^ string_of_typ t1 ^ "`" ^
    " does not match expected type `" ^ string_of_typ t2 ^ "`" ^ reason)

type direction = Infer | Check

let error_dir_typ at phrase dir t expected =
  match dir with
  | Check -> error_typ at phrase t
  | Infer ->
    error at (phrase ^ "'s type `" ^ string_of_typ t ^ "`" ^
      " does not match expected type " ^ expected)


(* Helpers *)

let unparen_exp e =
  match e.it with
  | ParenE (e1, _) -> e1
  | _ -> e

let unseq_exp e =
  match e.it with
  | EpsE -> []
  | SeqE es -> es
  | _ -> [e]

let tup_typ' ts' at =
  match ts' with
  | [t'] -> t'
  | _ -> Il.TupT ts' $ at

let tup_exp' es' at =
  match es' with
  | [e'] -> e'
  | _ -> Il.TupE es' $$ (at, Il.TupT (List.map note es') $ at)

let lift_exp' e' iter' =
  if iter' = Opt then
    Il.OptE (Some e')
  else
    Il.ListE [e']

let cat_exp' e1' e2' =
  match e1'.it, e2'.it with
  | _, Il.ListE [] -> e1'.it
  | Il.ListE [], _ -> e2'.it
  | Il.ListE es1, Il.ListE es2 -> Il.ListE (es1 @ es2)
  | _ -> Il.CatE (e1', e2')


let rec exp_of_sym sym =
  let e =
    match sym.it with
    | VarG (id, []) -> VarE id
    | NatG n -> NatE n
    | HexG n -> HexE n
    | CharG n -> CharE n
    | TextG t -> TextE t
    | EpsG -> EpsE
    | SeqG gs -> SeqE (map_nl_list exp_of_sym gs)
    | ParenG g -> ParenE (exp_of_sym g, false)
    | TupG gs -> TupE (List.map exp_of_sym gs)
    | IterG (g, iter) -> IterE (exp_of_sym g, iter)
    | ArithG e -> e.it
    | VarG _ | AltG _ | RangeG _ | AttrG _ ->
      Source.error sym.at "syntax" "malformed expression"
  in e $ sym.at


(* Environment *)

type fwd_typ = Bad | Ok
type var_typ = typ
type syn_typ = (fwd_typ, typ * Il.deftyp) Either.t
type gram_typ = param list * typ * gram option
type rel_typ = typ * Il.rule list
type def_typ = typ * typ * Il.clause list

type env =
  { mutable vars : var_typ Map.t;
    mutable typs : syn_typ Map.t;
    mutable syms : gram_typ Map.t;
    mutable rels : rel_typ Map.t;
    mutable defs : def_typ Map.t;
  }

let new_env () =
  { vars = Map.empty
      |> Map.add "bool" (BoolT $ no_region)
      |> Map.add "nat" (NumT NatT $ no_region)
      |> Map.add "int" (NumT IntT $ no_region)
      |> Map.add "rat" (NumT RatT $ no_region)
      |> Map.add "real" (NumT RealT $ no_region)
      |> Map.add "text" (TextT $ no_region);
    typs = Map.empty;
    syms = Map.empty;
    rels = Map.empty;
    defs = Map.empty;
  }

let bound env' id = Map.mem id.it env'

let find space env' id =
  match Map.find_opt id.it env' with
  | None -> error_id id ("undeclared " ^ space)
  | Some t -> t

let bind space env' id t =
  if Map.mem id.it env' then
    error_id id ("duplicate declaration for " ^ space)
  else
    Map.add id.it t env'

let rebind _space env' id t =
  assert (Map.mem id.it env');
  Map.add id.it t env'

let find_field fs atom at t =
  match List.find_opt (fun (atom', _, _) -> atom' = atom) fs with
  | Some (_, x, _) -> x
  | None -> error_atom at atom t "unbound field"

let find_case cases atom at t =
  match List.find_opt (fun (atom', _, _) -> atom' = atom) cases with
  | Some (_, x, _) -> x
  | None -> error_atom at atom t "unknown case"


let rec prefix_id id = prefix_id' id.it $ id.at
and prefix_id' id =
  match String.index_opt id '_', String.index_opt id '\'' with
  | None, None -> id
  | None, Some n | Some n, None -> String.sub id 0 n
  | Some n1, Some n2 -> String.sub id 0 (min n1 n2)


(* Type Accessors *)

let as_defined_typid' env id at : typ' * [`Alias | `NoAlias] =
  match find "syntax type" env.typs id with
  | Either.Right (t, {it = Il.AliasT _t'; _}) -> t.it, `Alias
  | Either.Right (t, _deftyp') -> t.it, `NoAlias
  | Either.Left Ok -> VarT id, `NoAlias
  | Either.Left Bad ->
    error_id (id.it $ at) "invalid forward use of syntax type"

let rec expand' env = function
  | VarT id as t' ->
    (match as_defined_typid' env id id.at with
    | t1, `Alias -> expand' env t1
    | _ -> t'
    )
  | ParenT t -> expand' env t.it
  | t' -> t'

let expand env t = expand' env t.it

let expand_singular' env t' =
  match expand' env t' with
  | IterT (t1, (Opt | List | List1)) -> expand env t1
  | t' -> t'


let as_iter_typ phrase env dir t at : typ * iter =
  match expand' env t.it with
  | IterT (t1, iter) -> t1, iter
  | _ -> error_dir_typ at phrase dir t "(_)*"

let as_list_typ phrase env dir t at : typ =
  match expand' env t.it with
  | IterT (t1, (List | List1 | ListN _)) -> t1
  | _ -> error_dir_typ at phrase dir t "(_)*"

let as_tup_typ phrase env dir t at : typ list =
  match expand_singular' env t.it with
  | TupT ts -> ts
  | _ -> error_dir_typ at phrase dir t "(_,...,_)"


let rec as_notation_typid' phrase env id at : typ =
  match as_defined_typid' env id at with
  | VarT id', `Alias -> as_notation_typid' phrase env id' at
  | (AtomT _ | SeqT _ | InfixT _ | BrackT _ | IterT _) as t, _ -> t $ at
  | _ -> error_dir_typ at phrase Infer (VarT id $ id.at) "_ ... _"

let as_notation_typ phrase env dir t at : typ =
  match expand_singular' env t.it with
  | VarT id -> as_notation_typid' phrase env id at
  | _ -> error_dir_typ at phrase dir t "_ ... _"

let rec as_struct_typid' phrase env id at : typfield list =
  match as_defined_typid' env id at with
  | VarT id', `Alias -> as_struct_typid' phrase env id' at
  | StrT tfs, _ -> filter_nl tfs
  | _ -> error_dir_typ at phrase Infer (VarT id $ id.at) "| ..."

let as_struct_typ phrase env dir t at : typfield list =
  match expand_singular' env t.it with
  | VarT id -> as_struct_typid' phrase env id at
  | _ -> error_dir_typ at phrase dir t "{...}"

let rec as_variant_typid' phrase env id at : typcase list * dots =
  match as_defined_typid' env id at with
  | VarT id', `Alias -> as_variant_typid' phrase env id' at
  | CaseT (_dots1, ids, cases, dots2), _ ->
    let casess = map_nl_list (as_variant_typid "" env) ids in
    List.concat (filter_nl cases :: List.map fst casess), dots2
  | _ -> error_dir_typ id.at phrase Infer (VarT id $ id.at) "| ..."

and as_variant_typid phrase env id : typcase list * dots =
  as_variant_typid' phrase env id id.at

let as_variant_typ phrase env dir t at : typcase list * dots =
  match expand_singular' env t.it with
  | VarT id -> as_variant_typid' phrase env id at
  | _ -> error_dir_typ at phrase dir t "| ..."

let case_has_args env t atom at : bool =
  let cases, _ = as_variant_typ "" env Check t at in
  let ts, _prems = find_case cases atom at t in
  ts <> []


let is_x_typ as_x_typ env t =
  try ignore (as_x_typ "" env Check t no_region); true
  with Error _ -> false

let is_iter_typ = is_x_typ as_iter_typ
let is_notation_typ = is_x_typ as_notation_typ
let is_variant_typ = is_x_typ as_variant_typ


(* Type Equivalence *)

let equiv_list equiv_x xs1 xs2 =
  List.length xs1 = List.length xs2 && List.for_all2 equiv_x xs1 xs2

let rec equiv_typ env t1 t2 =
  (*
  Printf.printf "[equiv] (%s) == (%s)  eq=%b  (expanded (%s) == (%s))\n%!"
    (Print.string_of_typ t1) (Print.string_of_typ t2)
    (t1.it = t2.it)
    (Print.string_of_typ (expand env t1 $ t1.at)) (Print.string_of_typ (expand env t2 $ t2.at));
  *)
  t1.it = t2.it ||
  match expand env t1, expand env t2 with
  | VarT id1, VarT id2 -> id1.it = id2.it
  | TupT ts1, TupT ts2 -> equiv_list (equiv_typ env) ts1 ts2
  | IterT (t11, iter1), IterT (t21, iter2) ->
    equiv_typ env t11 t21 && Eq.eq_iter iter1 iter2
  | RangeT _, NumT NatT | NumT NatT, RangeT _ -> true
  | t1', t2' -> Eq.eq_typ (t1' $ t1.at) (t2' $ t2.at)

let sub_typ env t1 t2 =
  equiv_typ env t1 t2 ||
  match expand env t1, expand env t2 with
  | NumT t1', NumT t2' -> t1' < t2'
  | _ -> false


(* Hints *)

let elab_hint {hintid; hintexp} : Il.hint =
  let ss =
    match hintexp.it with
    | SeqE es -> List.map Print.string_of_exp es
    | _ -> [Print.string_of_exp hintexp]
  in
  {Il.hintid; Il.hintexp = ss}

let elab_hints = List.map elab_hint


(* Atoms and Operators *)

let elab_atom = function
  | Atom s -> Il.Atom s
  | Infinity -> Il.Infinity
  | Bot -> Il.Bot
  | Dot -> Il.Dot
  | Dot2 -> Il.Dot2
  | Dot3 -> Il.Dot3
  | Semicolon -> Il.Semicolon
  | Backslash -> Il.Backslash
  | In -> Il.In
  | Arrow -> Il.Arrow
  | Colon -> Il.Colon
  | Sub -> Il.Sub
  | Assign -> Il.Assign
  | Approx -> Il.Approx
  | SqArrow -> Il.SqArrow
  | SqArrowStar -> Il.SqArrowStar
  | Prec -> Il.Prec
  | Succ -> Il.Succ
  | Turnstile -> Il.Turnstile
  | Tilesturn -> Il.Tilesturn
  | Quest -> Il.Quest
  | Star -> Il.Star

let elab_brack = function
  | Paren -> Il.LParen, Il.RParen
  | Brack -> Il.LBrack, Il.RBrack
  | Brace -> Il.LBrace, Il.RBrace

let numtyps = [NatT; IntT; RatT; RealT]

let elab_numtyp t : Il.numtyp =
  match t with
  | NatT -> Il.NatT
  | IntT -> Il.IntT
  | RatT -> Il.RatT
  | RealT -> Il.RealT

let infer_numop fop' ts =
  List.map (fun t -> fop' (elab_numtyp t), NumT t) ts

let infer_unop = function
  | NotOp -> [Il.NotOp, BoolT]
  | PlusOp -> infer_numop (fun t -> Il.PlusOp t) (List.tl numtyps)
  | MinusOp -> infer_numop (fun t -> Il.MinusOp t) (List.tl numtyps)

let infer_binop = function
  | AndOp -> [Il.AndOp, BoolT]
  | OrOp -> [Il.OrOp, BoolT]
  | ImplOp -> [Il.ImplOp, BoolT]
  | EquivOp -> [Il.EquivOp, BoolT]
  | AddOp -> infer_numop (fun t -> Il.AddOp t) numtyps
  | SubOp -> infer_numop (fun t -> Il.SubOp t) numtyps
  | MulOp -> infer_numop (fun t -> Il.MulOp t) numtyps
  | DivOp -> infer_numop (fun t -> Il.DivOp t) numtyps
  | ExpOp -> infer_numop (fun t -> Il.ExpOp t) numtyps

let infer_cmpop = function
  | EqOp -> `Poly Il.EqOp
  | NeOp -> `Poly Il.NeOp
  | LtOp -> `Over (infer_numop (fun t -> Il.LtOp t) numtyps)
  | GtOp -> `Over (infer_numop (fun t -> Il.GtOp t) numtyps)
  | LeOp -> `Over (infer_numop (fun t -> Il.LeOp t) numtyps)
  | GeOp -> `Over (infer_numop (fun t -> Il.GeOp t) numtyps)

let elab_unop env op t1 at : Il.unop * typ =
  let ops = infer_unop op in
  match List.find_opt (fun (_, t) -> sub_typ env t1 (t $ at)) ops with
  | Some (op', t) -> op', t $ at
  | None ->
    error at ("operator is not defined for type `" ^ string_of_typ t1 ^ "`")

let elab_binop env op t1 t2 at : Il.binop * typ =
  let ops = infer_binop op in
  match
    List.find_opt (fun (_, t) ->
      sub_typ env t1 (t $ at) && sub_typ env t2 (t $ at)) ops
  with
  | Some (op', t) -> op', t $ at
  | None ->
    error at ("operator " ^ string_of_binop op ^ " is not defined for types `" ^
      string_of_typ t1 ^ "` and `" ^ string_of_typ t2 ^ "`")

let elab_cmpop env op
  : [`Poly of Il.cmpop | `Over of typ -> typ -> region -> Il.cmpop * typ] =
  match infer_cmpop op with
  | `Poly op' -> `Poly op'
  | `Over ops -> `Over (fun t1 t2 at ->
    match
      List.find_opt (fun (_, t) ->
        sub_typ env t1 (t $ at) && sub_typ env t2 (t $ at)) ops
    with
    | Some (op', t) -> op', t $ at
    | None ->
      error at ("operator " ^ string_of_cmpop op ^ " is not defined for types `" ^
        string_of_typ t1 ^ "` and `" ^ string_of_typ t2 ^ "`")
    )

let merge_mixop mixop1 mixop2 =
  match mixop1, mixop2 with
  | _, [] -> mixop1
  | [], _ -> mixop2
  | mixop1, atoms2::mixop2' ->
    let mixop1', atoms1 = Lib.List.split_last mixop1 in
    mixop1' @ [atoms1 @ atoms2] @ mixop2'


let check_atoms phrase item list at =
  let _, dups =
    List.fold_right (fun (atom, _, _) (set, dups) ->
      let s = Print.string_of_atom atom in
      if Set.mem s set then (set, s::dups) else (Set.add s set, dups)
    ) list (Set.empty, [])
  in
  if dups <> [] then
    error at (phrase ^ " contains duplicate " ^ item ^ "(s) `" ^
      String.concat "`, `" dups ^ "`")


(* Iteration *)

let rec elab_iter env iter : Il.iter =
  match iter with
  | Opt -> Il.Opt
  | List -> Il.List
  | List1 -> Il.List1
  | ListN (e, id_opt) ->
    Option.iter (fun id ->
      let t = find "variable" env.vars (prefix_id id) in
      if not (equiv_typ env t (NumT NatT $ id.at)) then
        error_typ e.at "iteration index" (NumT NatT $ id.at)
    ) id_opt;
    Il.ListN (elab_exp env e (NumT NatT $ e.at), id_opt)


(* Types *)

and elab_typ env t : Il.typ =
  match t.it with
  | VarT id ->
    (match find "syntax type" env.typs id with
    | Either.Left Bad -> error_id id "invalid forward reference to syntax type"
    | _ -> Il.VarT id $ t.at
    )
  | BoolT -> Il.BoolT $ t.at
  | NumT t' -> Il.NumT (elab_numtyp t') $ t.at
  | TextT -> Il.TextT $ t.at
  | ParenT t1 -> elab_typ env t1
  | TupT ts -> Il.TupT (List.map (elab_typ env) ts) $ t.at
  | IterT (t1, iter) ->
    (match iter with
    | List1 | ListN _ -> error t.at "illegal iterator in syntax type"
    | _ -> Il.IterT (elab_typ env t1, elab_iter env iter) $ t.at
    )
  | StrT _ | CaseT _ | RangeT _ | AtomT _ | SeqT _ | InfixT _ | BrackT _ ->
    error t.at "this type is only allowed in type definitions"

and elab_typ_definition env id t : Il.deftyp =
  (match t.it with
  | StrT tfs ->
    let tfs' = filter_nl tfs in
    check_atoms "record" "field" tfs' t.at;
    Il.StructT (map_nl_list (elab_typfield env) tfs)
  | CaseT (dots1, ids, cases, _dots2) ->
    let cases0 =
      if dots1 = Dots then fst (as_variant_typid "own type" env id) else [] in
    let casess = map_nl_list (as_variant_typid "parent type" env) ids in
    let cases' =
      List.flatten (cases0 :: List.map fst casess @ [filter_nl cases]) in
    let tcs' = List.map (elab_typcase env t.at) cases' in
    check_atoms "variant" "case" cases' t.at;
    Il.VariantT tcs'
  | RangeT tes ->
    let _ = map_nl_list (elab_typenum env) tes in
    Il.AliasT (Il.NumT Il.NatT $ t.at)
  | _ ->
    match elab_typ_notation env t with
    | false, _mixop, ts' -> Il.AliasT (tup_typ' ts' t.at)
    | true, mixop, ts' -> Il.NotationT (mixop, tup_typ' ts' t.at)
  ) $ t.at

and elab_typ_notation env t : bool * Il.mixop * Il.typ list =
  (*
  Printf.printf "[typ_not %s] %s\n%!"
    (string_of_region t.at) (string_of_typ t);
  *)
  match t.it with
  | AtomT atom ->
    true, [[elab_atom atom]], []
  | SeqT [] ->
    true, [[]], []
  | SeqT (t1::ts2) ->
    let _b1, mixop1, ts1' = elab_typ_notation env t1 in
    let _b2, mixop2, ts2' = elab_typ_notation env (SeqT ts2 $ t.at) in
    true, merge_mixop mixop1 mixop2, ts1' @ ts2'
  | InfixT (t1, atom, t2) ->
    let _b1, mixop1, ts1' = elab_typ_notation env t1 in
    let _b2, mixop2, ts2' = elab_typ_notation env t2 in
    true, merge_mixop (merge_mixop mixop1 [[elab_atom atom]]) mixop2, ts1' @ ts2'
  | BrackT (brack, t1) ->
    let l, r = elab_brack brack in
    let _b1, mixop1, ts' = elab_typ_notation env t1 in
    true, merge_mixop (merge_mixop [[l]] mixop1) [[r]], ts'
  | ParenT t1 ->
    let b1, mixop1, ts1' = elab_typ_notation env t1 in
    b1, merge_mixop (merge_mixop [[Il.LParen]] mixop1) [[Il.RParen]], ts1'
  | IterT (t1, iter) ->
    (match iter with
    | List1 | ListN _ -> error t.at "illegal iterator in notation type"
    | _ ->
      let b1, mixop1, ts' = elab_typ_notation env t1 in
      let iter' = elab_iter env iter in
      let t' = Il.IterT (tup_typ' ts' t1.at, iter') $ t.at in
      let op = match iter with Opt -> Il.Quest | _ -> Il.Star in
      b1, [List.flatten mixop1] @ [[op]], [t']
    )
  | _ ->
    false, [[]; []], [elab_typ env t]


and elab_typfield env (atom, (t, prems), hints) : Il.typfield =
  let _, _, ts' = elab_typ_notation env t in
  let dims = Multiplicity.check_typdef [t] prems in
  let dims' = Multiplicity.Env.map (List.map (elab_iter env)) dims in
  let prems' = List.map (Multiplicity.annot_prem dims')
    (map_nl_list (elab_prem env) prems) in
  let free = Free.(free_nl_list free_prem prems).varid in
  let binds' = make_binds env free dims t.at in
  ( elab_atom atom,
    (binds', tup_typ' ts' t.at, prems'),
    elab_hints hints
  )

and elab_typcase env at (atom, (ts, prems), hints) : Il.typcase =
  let tss' =
    List.map (fun (_, _, ts) -> ts) (List.map (elab_typ_notation env) ts) in
  let dims = Multiplicity.check_typdef ts prems in
  let dims' = Multiplicity.Env.map (List.map (elab_iter env)) dims in
  let prems' = List.map (Multiplicity.annot_prem dims')
    (map_nl_list (elab_prem env) prems) in
  let free = Free.(free_nl_list free_prem prems).varid in
  let binds' = make_binds env free dims at in
  ( elab_atom atom,
    (binds', tup_typ' (List.concat tss') at, prems'),
    elab_hints hints
  )

and elab_typenum env (e, eo) =
  (* TODO: for now, we simplify ranges to nat or int *)
  let _e' = elab_exp env e (NumT IntT $ e.at) in
  let _eo' = Option.map (fun e2 -> elab_exp env e2 (NumT IntT $ e2.at)) eo in
  ()

and (!!!) env t = let _, _, ts' = elab_typ_notation env t in tup_typ' ts' t.at


(* Expressions *)

and infer_exp env e : Il.exp * typ =
  let e', t = infer_exp' env e in
  e' $$ e.at % elab_typ env t, t

and infer_exp' env e : Il.exp' * typ =
  (*
  Printf.printf "[infer %s] %s\n%!"
    (string_of_region e.at) (string_of_exp e);
  *)
  match e.it with
  | VarE id ->
    Il.VarE id, find "variable" env.vars (prefix_id id)
  | AtomE _ ->
    error e.at "cannot infer type of atom"
  | BoolE b ->
    Il.BoolE b, BoolT $ e.at
  | NatE n | HexE n | CharE n ->
    Il.NatE n, NumT NatT $ e.at
  | TextE s ->
    Il.TextE s, TextT $ e.at
  | UnE (op, e1) ->
    let e1', t1 = infer_exp env e1 in
    let op', t = elab_unop env op t1 e.at in
    Il.UnE (op', cast_exp "operand" env e1' t1 t), t
  | BinE (e1, op, e2) ->
    let e1', t1 = infer_exp env e1 in
    let e2', t2 = infer_exp env e2 in
    let op', t = elab_binop env op t1 t2 e.at in
    Il.BinE (op',
      cast_exp "operand" env e1' t1 t,
      cast_exp "operand" env e2' t2 t
    ), t
  | CmpE (e1, op, ({it = CmpE (e21, _, _); _} as e2)) ->
    let e1', _t1 = infer_exp env (CmpE (e1, op, e21) $ e.at) in
    let e2', _t2 = infer_exp env e2 in
    Il.BinE (Il.AndOp, e1', e2'), BoolT $ e.at
  | CmpE (e1, op, e2) ->
    (match elab_cmpop env op with
    | `Poly op' ->
      let e1', t1 = infer_exp env e1 in
      let e2' = elab_exp env e2 t1 in
      Il.CmpE (op', e1', e2'), BoolT $ e.at
    | `Over elab_cmpop'  ->
      let e1', t1 = infer_exp env e1 in
      let e2', t2 = infer_exp env e2 in
      let op', t = elab_cmpop' t1 t2 e.at in
      Il.CmpE (op',
        cast_exp "operand" env e1' t1 t,
        cast_exp "operand" env e2' t2 t
      ), BoolT $ e.at
    )
  | IdxE (e1, e2) ->
    let e1', t1 = infer_exp env e1 in
    let t = as_list_typ "expression" env Infer t1 e1.at in
    let e2' = elab_exp env e2 (NumT NatT $ e2.at) in
    Il.IdxE (e1', e2'), t
  | SliceE (e1, e2, e3) ->
    let e1', t1 = infer_exp env e1 in
    let _t' = as_list_typ "expression" env Infer t1 e1.at in
    let e2' = elab_exp env e2 (NumT NatT $ e2.at) in
    let e3' = elab_exp env e3 (NumT NatT $ e3.at) in
    Il.SliceE (e1', e2', e3'), t1
  | UpdE (e1, p, e2) ->
    let e1', t1 = infer_exp env e1 in
    let p', t2 = elab_path env p t1 in
    let e2' = elab_exp env e2 t2 in
    Il.UpdE (e1', p', e2'), t1
  | ExtE (e1, p, e2) ->
    let e1', t1 = infer_exp env e1 in
    let p', t2 = elab_path env p t1 in
    let _t21 = as_list_typ "path" env Infer t2 p.at in
    let e2' = elab_exp env e2 t2 in
    Il.ExtE (e1', p', e2'), t1
  | StrE _ ->
    error e.at "cannot infer type of record"
  | DotE (e1, atom) ->
    let e1', t1 = infer_exp env e1 in
    let tfs = as_struct_typ "expression" env Infer t1 e1.at in
    let t, _prems = find_field tfs atom e1.at t1 in
    Il.DotE (e1', elab_atom atom), t
  | CommaE (e1, e2) ->
    let e1', t1 = infer_exp env e1 in
    let tfs = as_struct_typ "expression" env Infer t1 e1.at in
    (* TODO: this is a bit of a hack *)
    (match e2.it with
    | SeqE ({it = AtomE atom; at; _} :: es2) ->
      let _t2 = find_field tfs atom at t1 in
      let e2 = match es2 with [e2] -> e2 | _ -> SeqE es2 $ e2.at in
      let e2' = elab_exp env (StrE [Elem (atom, e2)] $ e2.at) t1 in
      Il.CompE (e1', e2'), t1
    | _ -> failwith "unimplemented: infer CommaE"
    )
  | CompE (e1, e2) ->
    let e1', t1 = infer_exp env e1 in
    let _ = as_struct_typ "record" env Infer t1 e.at in
    let e2' = elab_exp env e2 t1 in
    Il.CompE (e1', e2'), t1
  | LenE e1 ->
    let e1', t1 = infer_exp env e1 in
    let _t11 = as_list_typ "expression" env Infer t1 e1.at in
    Il.LenE e1', NumT NatT $ e.at
  | SizeE id ->
    let _ = find "grammar" env.syms id in
    NatE 0, NumT NatT $ e.at
  | ParenE (e1, _) ->
    infer_exp' env e1
  | TupE es ->
    let es', ts = List.split (List.map (infer_exp env) es) in
    Il.TupE es', TupT ts $ e.at
  | CallE (id, e2) ->
    let t2, t, _ = find "function" env.defs id in
    let e2' = elab_exp env e2 t2 in
    Il.CallE (id, e2'), t
  | EpsE -> error e.at "cannot infer type of empty sequence"
  | SeqE _ -> error e.at "cannot infer type of expression sequence"
  | InfixE _ -> error e.at "cannot infer type of infix expression"
  | BrackE _ -> error e.at "cannot infer type of bracket expression"
  | IterE (e1, iter) ->
    let e1', t1 = infer_exp env e1 in
    let iter' = elab_iterexp env iter in
    Il.IterE (e1', iter'), IterT (t1, match iter with ListN _ -> List | _ -> iter) $ e.at
  | HoleE _ -> error e.at "misplaced hole"
  | FuseE _ -> error e.at "misplaced token concatenation"


and elab_exp env e t : Il.exp =
  let e' = elab_exp' env e t in
  e' $$ e.at % elab_typ env t

and elab_exp' env e t : Il.exp' =
  (*
  Printf.printf "[elab %s] %s  :  %s\n%!"
    (string_of_region e.at) (string_of_exp e) (string_of_typ t);
  *)
  match e.it with
  | VarE _ ->
    let e', t' = infer_exp env e in
    cast_exp' "variable" env e' t' t
  | BoolE _ ->
    let e', t' = infer_exp env e in
    cast_exp' "boolean" env e' t' t
  | NatE _ | HexE _ | CharE _ ->
    let e', t' = infer_exp env e in
    cast_exp' "number" env e' t' t
  | TextE _ ->
    let e', t' = infer_exp env e in
    cast_exp' "text" env e' t' t
  | UnE _ ->
    let e', t' = infer_exp env e in
    cast_exp' "unary operator" env e' t' t
  | BinE _ ->
    let e', t' = infer_exp env e in
    cast_exp' "binary operator" env e' t' t
  | CmpE _ ->
    let e', t' = infer_exp env e in
    cast_exp' "comparison operator" env e' t' t
  | IdxE _ ->
    let e', t' = infer_exp env e in
    cast_exp' "list element" env e' t' t
  | SliceE (e1, e2, e3) ->
    let _t' = as_list_typ "expression" env Check t e1.at in
    let e1' = elab_exp env e1 t in
    let e2' = elab_exp env e2 (NumT NatT $ e2.at) in
    let e3' = elab_exp env e3 (NumT NatT $ e3.at) in
    Il.SliceE (e1', e2', e3')
  | UpdE (e1, p, e2) ->
    let e1' = elab_exp env e1 t in
    let p', t2 = elab_path env p t in
    let e2' = elab_exp env e2 t2 in
    Il.UpdE (e1', p', e2')
  | ExtE (e1, p, e2) ->
    let e1' = elab_exp env e1 t in
    let p', t2 = elab_path env p t in
    let _t21 = as_list_typ "path" env Check t2 p.at in
    let e2' = elab_exp env e2 t2 in
    Il.ExtE (e1', p', e2')
  | StrE efs ->
    let tfs = as_struct_typ "record" env Check t e.at in
    let efs' = elab_expfields env (filter_nl efs) tfs t e.at in
    Il.StrE efs'
  | DotE _ ->
    let e', t' = infer_exp env e in
    cast_exp' "projection" env e' t' t
  | CommaE (e1, e2) ->
    let e1' = elab_exp env e1 t in
    let tfs = as_struct_typ "expression" env Check t e1.at in
    (* TODO: this is a bit of a hack *)
    (match e2.it with
    | SeqE ({it = AtomE atom; at; _} :: es2) ->
      let _t2 = find_field tfs atom at t in
      let e2 = match es2 with [e2] -> e2 | _ -> SeqE es2 $ e2.at in
      let e2' = elab_exp env (StrE [Elem (atom, e2)] $ e2.at) t in
      Il.CompE (e1', e2')
    | _ -> failwith "unimplemented: check CommaE"
    )
  | CompE (e1, e2) ->
    let _ = as_struct_typ "record" env Check t e.at in
    let e1' = elab_exp env e1 t in
    let e2' = elab_exp env e2 t in
    Il.CompE (e1', e2')
  | LenE _ ->
    let e', t' = infer_exp env e in
    cast_exp' "list length" env e' t' t
  | SizeE _ ->
    let e', t' = infer_exp env e in
    cast_exp' "expansion length" env e' t' t
  | ParenE (e1, true) when is_iter_typ env t ->
    (* Significant parentheses indicate a singleton *)
    let t1, _iter = as_iter_typ "expression" env Check t e.at in
    let e1' = elab_exp env e1 t1 in
    cast_exp' "expression" env e1' t1 t
  | ParenE (e1, _) ->
    elab_exp' env e1 t
  | TupE es ->
    let ts = as_tup_typ "tuple" env Check t e.at in
    if List.length es <> List.length ts then
      error e.at "arity mismatch for expression list";
    let es' = List.map2 (elab_exp env) es ts in
    Il.TupE es'
  | CallE _ ->
    let e', t' = infer_exp env e in
    cast_exp' "function application" env e' t' t
  | SeqE [_] ->
    assert false  (* sequences cannot be singleton *)
  | EpsE | SeqE _ when is_iter_typ env t ->
    let e1 = unseq_exp e in
    elab_exp_iter' env e1 (as_iter_typ "" env Check t e.at) t e.at
  | EpsE
  | AtomE _
  | InfixE _
  | BrackE _
  | SeqE _ ->
    (* All these expression forms can only be used when checking against
     * either a defined notation/variant type or (for SeqE) an iteration type;
     * the latter case is already captured above *)
    if is_notation_typ env t then
      (elab_exp_notation env e (as_notation_typ "" env Check t e.at) t).it
    else if is_variant_typ env t then
      (elab_exp_variant env (unseq_exp e)
        (fst (as_variant_typ "" env Check t e.at)) t e.at).it
    else
      error_typ e.at "expression" t
  | IterE (e1, iter2) ->
    (* An iteration expression must match the expected type directly,
     * significant parentheses have to be used otherwise *)
    let t1, iter = as_iter_typ "iteration" env Check t e.at in
    if (iter = Opt) <> (iter2 = Opt) then
      error_typ e.at "iteration expression" t;
    let e1' = elab_exp env e1 t1 in
    let iter2' = elab_iterexp env iter2 in
    Il.IterE (e1', iter2')
  | HoleE _ -> error e.at "misplaced hole"
  | FuseE _ -> error e.at "misplaced token fuse"

and elab_expfields env efs tfs t at : Il.expfield list =
  match efs, tfs with
  | [], [] -> []
  | (atom1, e)::efs2, (atom2, (t, _prems), _)::tfs2 when atom1 = atom2 ->
    let es' = elab_exp_notation' env e t in
    let efs2' = elab_expfields env efs2 tfs2 t at in
    (elab_atom atom1, tup_exp' es' e.at) :: efs2'
  | _, (atom, (t, _prems), _)::tfs2 ->
    let atom' = string_of_atom atom in
    let e' =
      cast_empty ("omitted record field `" ^ atom' ^ "`") env t at (elab_typ env t) in
    let efs2' = elab_expfields env efs tfs2 t at in
    (elab_atom atom, e') :: efs2'
  | (atom, e)::_, [] ->
    error_atom e.at atom t "unexpected record field"

and elab_exp_iter env es (t1, iter) t at : Il.exp =
  let e' = elab_exp_iter' env es (t1, iter) t at in
  e' $$ at % elab_typ env t

and elab_exp_iter' env es (t1, iter) t at : Il.exp' =
  (*
  Printf.printf "[iteration %s] %s  :  %s = (%s)%s\n%!"
    (string_of_region at)
    (String.concat " " (List.map string_of_exp es))
    (string_of_typ t) (string_of_typ t1) (string_of_iter iter);
  *)
  match es, iter with
  (* If the sequence actually starts with a non-nullary constructor,
   * then assume this is a singleton iteration and fallback to variant *)
  | {it = AtomE atom; at = at1; _}::_, _ when is_variant_typ env t1 &&
      case_has_args env t1 atom at1 ->
    let cases, _dots = as_variant_typ "" env Check t1 at in
    lift_exp' (elab_exp_variant env es cases t1 at) iter

  (* An empty sequence represents the None case for options *)
  | [], Opt ->
    Il.OptE None
  (* An empty sequence represents the Nil case for lists *)
  | [], List ->
    Il.ListE []
  (* All other elements are either splices or (by cast injection) elements *)
  | e1::es2, List ->
    let e1' = elab_exp env e1 t in
    let e2' = elab_exp_iter env es2 (t1, iter) t at in
    cat_exp' e1' e2'

  | _, _ ->
    error_typ at "expression" t

and elab_exp_notation env e nt t : Il.exp = (*
  Printf.printf "[notation %s] %s  :  %s\n%!"
    (string_of_region e.at) (string_of_exp e) (string_of_typ t);
  *)
  (* Convert notation into applications of mixin operators *)
  let e' = tup_exp' (elab_exp_notation' env e nt) e.at in
  match elab_typ_notation env nt with
  | false, _, _ -> e'
  | true, mixop, _ -> Il.MixE (mixop, e') $$ e.at % elab_typ env t

and elab_exp_notation' env e t : Il.exp list =
  (*
  Printf.printf "[notation %s] %s  :  %s\n%!"
    (string_of_region e.at) (string_of_exp e) (string_of_typ t);
  *)
  match e.it, t.it with
  | AtomE atom, AtomT atom' ->
    if atom <> atom' then error_typ e.at "atom" t;
    []
  | InfixE (e1, atom, e2), InfixT (t1, atom', t2) ->
    if atom <> atom' then error_typ e.at "infix expression" t;
    let es1' = elab_exp_notation' env e1 t1 in
    let es2' = elab_exp_notation' env e2 t2 in
    es1' @ es2'
  | BrackE (brack, e1), BrackT (brack', t1) ->
    if brack <> brack' then error_typ e.at "bracket expression" t;
    elab_exp_notation' env e1 t1

  | SeqE [], SeqT [] ->
    []
  (* Iterations at the end of a sequence may be inlined *)
  | _, SeqT [{it = IterT _; _} as t1] ->
    elab_exp_notation' env e t1
  (* Optional iterations may always be inlined, use backtracking *)
  | SeqE (e1::es2), SeqT (({it = IterT (_, Opt); _} as t1)::ts2) ->
    (try
      let es1' = [cast_empty "omitted sequence tail" env t1 e.at (!!!env t1)] in
      let es2' = elab_exp_notation' env e (SeqT ts2 $ t.at) in
      es1' @ es2'
    with Source.Error _ ->
      (*
      Printf.printf "[backtrack %s] %s  :  %s\n%!"
        (string_of_region e.at) (string_of_exp e) (string_of_typ t);
      *)
      let es1' = elab_exp_notation' env e1 t1 in
      let es2' = elab_exp_notation' env (SeqE es2 $ e.at) (SeqT ts2 $ t.at) in
      es1' @ es2'
    )
  | SeqE (e1::es2), SeqT (t1::ts2) ->
    let es1' = elab_exp_notation' env (unparen_exp e1) t1 in
    let es2' = elab_exp_notation' env (SeqE es2 $ e.at) (SeqT ts2 $ t.at) in
    es1' @ es2'
  (* Trailing elements can be omitted if they can be epsilon *)
  | SeqE [], SeqT (t1::ts2) ->
    let e1' = cast_empty "omitted sequence tail" env t1 e.at (!!!env t1) in
    let es2' =
      elab_exp_notation' env (SeqE [] $ e.at) (SeqT ts2 $ t.at) in
    [e1'] @ es2'
  | SeqE (e1::_), SeqT [] ->
    error e1.at
      "superfluous expression does not match expected empty notation type"
  (* Since trailing elements can be omitted, a singleton may match a sequence *)
  | _, SeqT _ ->
    elab_exp_notation' env (SeqE [e] $ e.at) t

  | SeqE [e1], IterT _ ->
    [elab_exp env e1 t]
  | (EpsE | SeqE _), IterT (t1, iter) ->
    [elab_exp_notation_iter env (unseq_exp e) (t1, iter) t e.at]
  | IterE (e1, iter1), IterT (t1, iter) ->
    if iter = Opt && iter1 <> Opt then
      error_typ e.at "iteration expression" t;
    let es1' = elab_exp_notation' env e1 t1 in
    let iter1' = elab_iterexp env iter1 in
    [Il.IterE (tup_exp' es1' e1.at, iter1') $$ e.at % !!!env t]
  (* Significant parentheses indicate a singleton *)
  | ParenE (e1, true), IterT (t1, iter) ->
    let es' = elab_exp_notation' env e1 t1 in
    [lift_exp' (tup_exp' es' e.at) iter $$ e.at % elab_typ env t]
  (* Elimination forms are considered splices *)
  | (IdxE _ | SliceE _ | UpdE _ | ExtE _ | DotE _ | CallE _), IterT _ ->
    [elab_exp env e t]
  (* All other expressions are considered splices *)
  (* TODO: can't they be splices, too? *)
  | _, IterT (t1, iter) ->
    let es' = elab_exp_notation' env e t1 in
    [lift_exp' (tup_exp' es' e.at) iter $$ e.at % !!!env t]

  | ParenE (e1, _), _ ->
    elab_exp_notation' env e1 t
  | _, ParenT t1 ->
    elab_exp_notation' env e t1

  | _, _ ->
    [elab_exp env e t]

and elab_exp_notation_iter env es (t1, iter) t at : Il.exp =
  let e' = elab_exp_notation_iter' env es (t1, iter) t at in
  let _, _, ts' = elab_typ_notation env t in
  e' $$ at % tup_typ' ts' t.at

and elab_exp_notation_iter' env es (t1, iter) t at : Il.exp' =
  (*
  Printf.printf "[niteration %s] %s  :  %s\n%!"
    (string_of_region at)
    (String.concat " " (List.map string_of_exp es))
    (string_of_typ t);
  *)
  match es, iter with
  (* If the sequence actually starts with a non-nullary constructor,
   * then assume this is a singleton iteration and fallback to variant *)
  | {it = AtomE atom; at = at1; _}::_, iter when is_variant_typ env t1 &&
      case_has_args env t1 atom at1 ->
    let cases, _ = as_variant_typ "expression" env Check t1 at in
    lift_exp' (elab_exp_variant env es cases t1 at) iter

  (* An empty sequence represents the None case for options *)
  | [], Opt ->
    Il.OptE None
  (* An empty sequence represents the Nil case for lists *)
  | [], List ->
    Il.ListE []
  (* All other elements are either splices or (by cast injection) elements;
   * nested expressions must be lifted into a tuple *)
  | e1::es2, List ->
    let es1' = elab_exp_notation' env e1 t in
    let e2' = elab_exp_notation_iter env es2 (t1, iter) t at in
    cat_exp' (tup_exp' es1' e1.at) e2'

  | _, _ ->
    error_typ at "expression" t

and elab_exp_variant env es cases t at : Il.exp =
  (*
  Printf.printf "[variant %s] {%s}  :  %s\n%!"
    (string_of_region at)
    (String.concat " " (List.map string_of_exp es))
    (string_of_typ t);
  (*
    (String.concat " | "
      (List.map (fun (atom, ts, _) ->
          string_of_typ (SeqT ((AtomT atom $ at) :: ts) $ at)
        ) cases
      )
    );
  *)
  *)
  match es with
  | {it = AtomE atom; _}::es ->
    let ts, _prems = find_case cases atom at t in
    (* TODO: this is a bit hacky *)
    let e2 = SeqE es $ at in
    let es' = elab_exp_notation' env e2 (SeqT ts $ t.at) in
    let t2 = expand_singular' env t.it $ at in
    let t2' = elab_typ env t2 in
    cast_exp "variant case" env
      (Il.CaseE (elab_atom atom, tup_exp' es' at) $$ at % t2') t2 t
  | _ ->
    error_typ at "expression" t


and elab_path env p t : Il.path * typ =
  let p', t' = elab_path' env p t in
  p' $$ p.at % elab_typ env t', t'

and elab_path' env p t : Il.path' * typ =
  match p.it with
  | RootP ->
    Il.RootP, t
  | IdxP (p1, e1) ->
    let p1', t1 = elab_path env p1 t in
    let e1' = elab_exp env e1 (NumT NatT $ e1.at) in
    let t' = as_list_typ "path" env Check t1 p1.at in
    Il.IdxP (p1', e1'), t'
  | SliceP (p1, e1, e2) ->
    let p1', t1 = elab_path env p1 t in
    let e1' = elab_exp env e1 (NumT NatT $ e1.at) in
    let e2' = elab_exp env e2 (NumT NatT $ e2.at) in
    let _ = as_list_typ "path" env Check t1 p1.at in
    Il.SliceP (p1', e1', e2'), t1
  | DotP (p1, atom) ->
    let p1', t1 = elab_path env p1 t in
    let tfs = as_struct_typ "path" env Check t1 p1.at in
    let t', _prems = find_field tfs atom p1.at t1 in
    Il.DotP (p1', elab_atom atom), t'


and cast_empty phrase env t at t' : Il.exp =
  match expand env t with
  | IterT (_, Opt) -> Il.OptE None $$ at % t'
  | IterT (_, List) -> Il.ListE [] $$ at % t'
  | _ -> error_typ at phrase t

and cast_exp phrase env e' t1 t2 : Il.exp =
  let e'' = cast_exp' phrase env e' t1 t2 in
  e'' $$ e'.at % elab_typ env t2

and cast_exp' phrase env e' t1 t2 : Il.exp' =
  (*
  Printf.printf "[cast %s] (%s) <: (%s)  >>  (%s) <: (%s)  eq=%b\n%!"
    (string_of_region e'.at)
    (string_of_typ t1) (string_of_typ t2)
    (string_of_typ (expand env t1 $ t1.at))
    (string_of_typ (expand env t2 $ t2.at))
    (equiv_typ env t1 t2);
  *)
  if equiv_typ env t1 t2 then
    e'.it
  else if sub_typ env t1 t2 then
    Il.SubE (e', elab_typ env t1, elab_typ env t2)
  else
    match expand env t2 with
    | IterT (t21, Opt) ->
      Il.OptE (Some (cast_exp phrase env e' t1 t21))
    | IterT (t21, (List | List1)) ->
      Il.ListE [cast_exp phrase env e' t1 t21]
    | _ ->
      (cast_exp_variant phrase env e' t1 t2).it

and cast_exp_variant phrase env e' t1 t2 : Il.exp =
  if equiv_typ env t1 t2 then e' else
  if is_variant_typ env t1 && is_variant_typ env t2 then
    let cases1, dots1 = as_variant_typ "" env Check t1 e'.at in
    let cases2, _dots2 = as_variant_typ "" env Check t2 e'.at in
    if dots1 = Dots then
      error e'.at "used variant type is only partially defined at this point";
    (try
      List.iter (fun (atom, (ts1, _prems1), _) ->
        let ts2, _prems2 = find_case cases2 atom t1.at t2 in
        (* Shallow subtyping on variants *)
        if List.length ts1 <> List.length ts2
        || not (List.for_all2 Eq.eq_typ ts1 ts2) then
          error_atom e'.at atom t1 "type mismatch for case"
      ) cases1
    with Error (_, msg) -> error_typ2 e'.at phrase t1 t2 (", " ^ msg)
    );
    Il.SubE (e', elab_typ env t1, elab_typ env t2) $$ e'.at % elab_typ env t2
  else
    error_typ2 e'.at phrase t1 t2 ""


and elab_iterexp env iter : Il.iterexp =
  (elab_iter env iter, [])


(* Premises *)

and elab_prem env prem : Il.premise =
  match prem.it with
  | RulePr (id, e) ->
    let t, _ = find "relation" env.rels id in
    let _, mixop, _ = elab_typ_notation env t in
    let es' = elab_exp_notation' env e t in
    Il.RulePr (id, mixop, tup_exp' es' e.at) $ prem.at
  | IfPr e ->
    let e' = elab_exp env e (BoolT $ e.at) in
    Il.IfPr e' $ prem.at
  | ElsePr ->
    Il.ElsePr $ prem.at
  | IterPr (prem1, iter) ->
    let prem1' = elab_prem env prem1 in
    let iter' = elab_iterexp env iter in
    Il.IterPr (prem1', iter') $ prem.at


(* Grammars *)

and elab_gramargs env gs ps at : env =
  match gs, ps with
  | [], [] -> env
  | g::_, [] -> error g.at "superfluous grammar parameter"
  | [], _::_ -> error at "too few grammar parameter"
  | g::gs', p::ps' ->
    let env' =
      match p.it with
      | VarP id ->
        let t = find "variable" env.vars (prefix_id id) in
        let _e' = elab_exp env (exp_of_sym g) t in
        env
      | GramP (_id1, id2, iters) ->
        match g.it with
        | VarG (id, gs') ->
          let ps', t, _gram = find "grammar" env.syms id in
          let rec check_iters t iters =
            match t.it, iters with
            | _, [] -> t
            | IterT (t', iter), iter'::iters' when Eq.eq_iter iter iter' ->
              check_iters t' iters'
            | _, _ ->
              error g.at "inconsistent multiplicity for grammar argument";
          in
          let t' = check_iters t (List.rev iters) in
          let env' = elab_gramargs env gs' ps' g.at in
          let dt' =  Il.AliasT (elab_typ env' t') $ t.at in
          { env' with
            typs = bind "syntax type" env'.typs id2 (Either.Right (t, dt'));
            vars = bind "variable" env'.vars id2 (VarT id2 $ id2.at);
          }
        | _ ->
          error g.at "malformed grammar argument"
    in elab_gramargs env' gs' ps' at

and elab_sym env g : typ * env =
  match g.it with
  | VarG (id, gs) ->
    let ps, t, _gram = find "grammar" env.syms id in
    let env' = elab_gramargs env gs ps g.at in
    t, env'
  | NatG _ | HexG _ | CharG _ -> NumT NatT $ g.at, env
  | TextG _ -> TextT $ g.at, env
  | EpsG -> TupT [] $ g.at, env
  | SeqG gs ->
    let _ts, env' = elab_sym_list env (filter_nl gs) in
    TupT [] $ g.at, env'
  | AltG gs ->
    let _ = elab_sym_list env (filter_nl gs) in
    TupT [] $ g.at, env
  | RangeG (g1, g2) ->
    let t1, env1 = elab_sym env g1 in
    let t2, env2 = elab_sym env g2 in
    if env1 != env then
      error g1.at "invalid symbol in range";
    if env2 != env then
      error g2.at "invalid symbol in range";
    if not (equiv_typ env t1 t2) then
      error_typ2 g2.at "range item" t2 t1 " of other range item";
    TupT [] $ g.at, env
  | ParenG g1 -> elab_sym env g1
  | TupG gs ->
    let ts, env' = elab_sym_list env gs in
    TupT ts $ g.at, env'
  | IterG (g1, iter) ->
    let t1, env1 = elab_sym env g1 in
    let _iter' = elab_iterexp env iter in
    IterT (t1, match iter with Opt -> Opt | _ -> List) $ g.at, env1
  | ArithG e ->
    let _e', t = infer_exp env e in
    t, env
  | AttrG (g1, e) ->
    let t1, env1 = elab_sym env g1 in
    let _e' = elab_exp env1 e t1 in
    TupT [] $ g.at, env

and elab_sym_list env = function
  | [] -> [], env
  | g::gs ->
    let t, env' = elab_sym env g in
    let ts, env'' = elab_sym_list env' gs in
    t::ts, env''

and elab_prod env prod t =
  let (g, e, prems) = prod.it in
  let _e' = elab_exp env e t in
  let _prems' = map_nl_list (elab_prem env) prems in
  elab_sym env g

and elab_gram env gram t =
  let (_dots1, prods, _dots2) = gram.it in
  iter_nl_list (fun prod -> ignore (elab_prod env prod t)) prods


(* Definitions *)

and make_binds env free dims at : Il.binds =
  List.map (fun id' ->
    let id = id' $ at in
    let t = elab_typ env (find "variable" env.vars (prefix_id id)) in
    let ctx = List.map (elab_iter env) (Multiplicity.Env.find id.it dims) in
    (id, t, ctx)
  ) (Set.elements free)


let elab_params env ps : env =
  List.fold_left (fun env p ->
    match p.it with
    | VarP _id -> env
    | GramP (id1, id2, iters) ->
      let _ = List.map (elab_iter env) iters in
      let t = List.fold_left (fun t iter -> IterT(t, iter) $ p.at) (VarT id2 $ id2.at) iters in
      { env with
        typs = bind "syntax type" env.typs id2 (Either.Left Ok);
        vars = bind "variable" env.vars id2 (VarT id2 $ id2.at);
        syms = bind "grammar" env.syms id1 ([], t, None);
      }
  ) env ps


let infer_typ_definition _env t : syn_typ =
  match t.it with
  | StrT _ | CaseT _ -> Either.Left Ok
  | _ -> Either.Left Bad

let infer_syndef env d =
  match d.it with
  | SynD (id1, _id2, t, _hints) ->
    if not (bound env.typs id1) then (
      env.typs <- bind "syntax type" env.typs id1 (infer_typ_definition env t);
      env.vars <- bind "variable" env.vars id1 (VarT id1 $ id1.at);
    )
  | _ -> ()

let infer_gramdef env d =
  match d.it with
  | GramD (id1, _id2, ps, t, _gram, _hints) ->
    if not (bound env.syms id1) then (
      let env' = elab_params env ps in
      let _t' = elab_typ env' t in
      env.syms <- bind "grammar" env.syms id1 (ps, t, None);
    )
  | _ -> ()

let elab_hintdef _env hd : Il.def list =
  match hd.it with
  | SynH (id1, _id2, hints) ->
    if hints = [] then [] else
    [Il.HintD (Il.SynH (id1, elab_hints hints) $ hd.at) $ hd.at]
  | RelH (id, hints) ->
    if hints = [] then [] else
    [Il.HintD (Il.RelH (id, elab_hints hints) $ hd.at) $ hd.at]
  | DecH (id, hints) ->
    if hints = [] then [] else
    [Il.HintD (Il.DecH (id, elab_hints hints) $ hd.at) $ hd.at]
  | GramH _ | AtomH _ | VarH _ ->
    []

let elab_def env d : Il.def list =
  match d.it with
  | SynD (id1, id2, t, hints) ->
    let dt' = elab_typ_definition env id1 t in
    let t1, closed =
      match find "syntax type" env.typs id1, t.it with
      | Either.Left _, CaseT (Dots, _, _, _) ->
        error_id id1 "extension of not yet defined syntax type"
      | Either.Left _, CaseT (NoDots, _, _, dots2) ->
        t, dots2 = NoDots
      | Either.Left _, _ ->
        t, true
      | Either.Right ({it = CaseT (dots1, ids1, tcs1, Dots); at; _}, _),
          CaseT (Dots, ids2, tcs2, dots2) ->
        CaseT (dots1, ids1 @ ids2, tcs1 @ tcs2, dots2) $ over_region [at; t.at],
          dots2 = NoDots
      | Either.Right _, CaseT (Dots, _, _, _) ->
        error_id id1 "extension of non-extensible syntax type"
      | Either.Right _, _ ->
        error_id id1 "duplicate declaration for syntax type";
    in
    (*
    Printf.printf "[def %s] %s ~> %s\n%!" id1.it
      (string_of_typ t) (Il.Print.string_of_deftyp dt');
    *)
    env.typs <- rebind "syntax type" env.typs id1 (Either.Right (t1, dt'));
    (if not closed then [] else [Il.SynD (id1, dt') $ d.at])
      @ elab_hintdef env (SynH (id1, id2, hints) $ d.at)
  | GramD _ -> []
  | RelD (id, t, hints) ->
    let _, mixop, ts' = elab_typ_notation env t in
    env.rels <- bind "relation" env.rels id (t, []);
    [Il.RelD (id, mixop, tup_typ' ts' t.at, []) $ d.at]
      @ elab_hintdef env (RelH (id, hints) $ d.at)
  | RuleD (id1, id2, e, prems) ->
    let dims = Multiplicity.check_def d in
    let dims' = Multiplicity.Env.map (List.map (elab_iter env)) dims in
    let t, rules' = find "relation" env.rels id1 in
    let _, mixop, _ = elab_typ_notation env t in
    let es' = List.map (Multiplicity.annot_exp dims') (elab_exp_notation' env e t) in
    let prems' = List.map (Multiplicity.annot_prem dims')
      (map_nl_list (elab_prem env) prems) in
    let free_rh =
      Free.(Set.diff
        (free_nl_list free_prem prems).varid
        (Set.union
          (pat_exp e).varid
          (free_nl_list bound_prem prems).varid
        )
      )
    in
    if free_rh <> Free.Set.empty then
      error d.at ("rule contains unbound variable(s) `" ^
        String.concat "`, `" (Free.Set.elements free_rh) ^ "`");
    let free = (Free.free_def d).Free.varid in
    let binds' = make_binds env free dims d.at in
    let rule' = Il.RuleD (id2, binds', mixop, tup_exp' es' e.at, prems') $ d.at in
    env.rels <- rebind "relation" env.rels id1 (t, rule'::rules');
    []
  | VarD (id, t, _hints) ->
    let _t' = elab_typ env t in
    env.vars <- bind "variable" env.vars id t;
    []
  | DecD (id, e1, t2, hints) ->
    let _e1', t1 = infer_exp env e1 in
    let t1' = elab_typ env t1 in
    let t2' = elab_typ env t2 in
    env.defs <- bind "function" env.defs id (t1, t2, []);
    [Il.DecD (id, t1', t2', []) $ d.at]
      @ elab_hintdef env (DecH (id, hints) $ d.at)
  | DefD (id, e1, e2, prems) ->
    let dims = Multiplicity.check_def d in
    let dims' = Multiplicity.Env.map (List.map (elab_iter env)) dims in
    let t1, t2, clauses' = find "function" env.defs id in
    let e1' = Multiplicity.annot_exp dims' (elab_exp env e1 t1) in
    let e2' = Multiplicity.annot_exp dims' (elab_exp env e2 t2) in
    let prems' = List.map (Multiplicity.annot_prem dims')
      (map_nl_list (elab_prem env) prems) in
    let free_rh =
      Free.(Set.diff
        (Set.union
          (free_exp e2).varid
          (free_nl_list free_prem prems).varid
        )
        (Set.union
          (Set.union
            (pat_exp e1).varid
            (bound_exp e2).varid
          )
          (free_nl_list bound_prem prems).varid
        )
      )
    in
    if free_rh <> Free.Set.empty then
      error d.at ("definition contains unbound variable(s) `" ^
        String.concat "`, `" (Free.Set.elements free_rh) ^ "`");
    let free = Free.(Set.union (Set.union
      (free_exp e1).varid (free_exp e2).varid) (free_nl_list free_prem prems).varid) in
    let binds' = make_binds env free dims d.at in
    let clause' = Il.DefD (binds', e1', e2', prems') $ d.at in
    env.defs <- rebind "definition" env.defs id (t1, t2, clause'::clauses');
    []
  | SepD ->
    []
  | HintD hd ->
    elab_hintdef env hd

let elab_gramdef env d =
  match d.it with
  | GramD (id1, _id2, ps, t, gram, _hints) ->
    let env' = elab_params env ps in
    let _t' = elab_typ env' t in
    elab_gram env' gram t;
    let ps1, t1, gram1_opt = find "grammar" env.syms id1 in
    let gram' =
      match gram1_opt, gram.it with
      | None, (Dots, _, _) ->
        error_id id1 "extension of not yet defined grammar"
      | None, _ ->
        gram
      | Some {it = (dots1, prods1, Dots); at; _}, (Dots, prods2, dots2) ->
        if not Eq.(eq_list eq_param ps ps1) then
          error d.at "grammar parameters differ from previous fragment";
        if not (equiv_typ env' t t1) then
          error_typ2 d.at "grammar" t1 t " of previous fragment";
        (dots1, prods1 @ prods2, dots2) $ over_region [at; t.at]
      | Some _, (Dots, _, _) ->
        error_id id1 "extension of non-extensible grammar"
      | Some _, _ ->
        error_id id1 "duplicate declaration for grammar";
    in
    env.syms <- rebind "grammar" env.syms id1 (ps, t, Some gram')
  | _ -> ()


let populate_def env d' : Il.def =
  match d'.it with
  | Il.SynD _ | Il.HintD _ -> d'
  | Il.RelD (id, mixop, t', []) ->
    let _, rules' = find "relation" env.rels id in
    Il.RelD (id, mixop, t', List.rev rules') $ d'.at
  | Il.DecD (id, t1', t2', []) ->
    let _, _, clauses' = find "function" env.defs id in
    Il.DecD (id, t1', t2', List.rev clauses') $ d'.at
  | _ ->
    assert false


(* Scripts *)

let origins i (map : int Map.t ref) (set : Il.Free.Set.t) =
  Il.Free.Set.iter (fun id -> map := Map.add id i !map) set

let deps (map : int Map.t) (set : Il.Free.Set.t) : int array =
  Array.map (fun id ->
try
   Map.find id map
with Not_found as e -> Printf.printf "[%s]\n%!" id; raise e
 ) (Array.of_seq (Il.Free.Set.to_seq set))


let check_recursion ds' =
  List.iter (fun d' ->
    match d'.it, (List.hd ds').it with
    | Il.HintD _, _ | _, Il.HintD _
    | Il.SynD _, Il.SynD _
    | Il.RelD _, Il.RelD _
    | Il.DecD _, Il.DecD _ -> ()
    | _, _ ->
      error (List.hd ds').at (" " ^ string_of_region d'.at ^
        ": invalid recursion between definitions of different sort")
  ) ds'
  (* TODO: check that notations are non-recursive and defs are inductive? *)

let recursify_defs ds' : Il.def list =
  let open Il.Free in
  let da = Array.of_list ds' in
  let map_synid = ref Map.empty in
  let map_relid = ref Map.empty in
  let map_defid = ref Map.empty in
  let frees = Array.map Il.Free.free_def da in
  let bounds = Array.map Il.Free.bound_def da in
  Array.iteri (fun i bound ->
    origins i map_synid bound.synid;
    origins i map_relid bound.relid;
    origins i map_defid bound.defid;
  ) bounds;
  let graph =
    Array.map (fun free ->
      Array.concat
        [ deps !map_synid free.synid;
          deps !map_relid free.relid;
          deps !map_defid free.defid;
        ];
    ) frees
  in
  let sccs = Scc.sccs graph in
  List.map (fun set ->
    let ds'' = List.map (fun i -> da.(i)) (Scc.Set.elements set) in
    check_recursion ds'';
    let i = Scc.Set.choose set in
    match ds'' with
    | [d'] when Il.Free.disjoint bounds.(i) frees.(i) -> d'
    | ds'' -> Il.RecD ds'' $ Source.over_region (List.map at ds'')
  ) sccs

let elab ds : Il.script =
  let env = new_env () in
  List.iter (infer_syndef env) ds;
  let ds' = List.concat_map (elab_def env) ds in
  let ds' = List.map (populate_def env) ds' in
  List.iter (infer_gramdef env) ds;
  List.iter (elab_gramdef env) ds;
  recursify_defs ds'
