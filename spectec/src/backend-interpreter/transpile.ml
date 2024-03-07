open Al
open Al.Ast

(** helper *)
let composite_instr g f x = f x |> List.map g |> List.flatten
let composite g f x = f x |> g

let take n str =
  let len = min n (String.length str) in
  String.sub str 0 len ^ if len <= n then "" else "..."

let rec neg cond =
  match cond with
  | NotC c -> c
  | BinopC (And, c1, c2) -> BinopC (Or, neg c1, neg c2)
  | BinopC (Or, c1, c2) -> BinopC (And, neg c1, neg c2)
  | CompareC (Eq, e1, e2) -> CompareC (Ne, e1, e2)
  | CompareC (Ne, e1, e2) -> CompareC (Eq, e1, e2)
  | CompareC (Gt, e1, e2) -> CompareC (Le, e1, e2)
  | CompareC (Ge, e1, e2) -> CompareC (Lt, e1, e2)
  | CompareC (Lt, e1, e2) -> CompareC (Ge, e1, e2)
  | CompareC (Le, e1, e2) -> CompareC (Gt, e1, e2)
  | _ -> NotC cond

let both_empty cond1 cond2 =
  let get_list = function
  | CompareC (Eq, e, ListE [])
  | CompareC (Eq, ListE [], e)
  | CompareC (Eq, LengthE e, NumE 0L)
  | CompareC (Le, LengthE e, NumE 0L)
  | CompareC (Lt, LengthE e, NumE 1L)
  | CompareC (Eq, NumE 0L, LengthE e)
  | CompareC (Ge, NumE 0L, LengthE e)
  | CompareC (Ge, NumE 1L, LengthE e) -> Some e
  | _ -> None in
  match get_list cond1, get_list cond2 with
  | Some e1, Some e2 -> e1 = e2
  | _ -> false

let both_non_empty cond1 cond2 =
  let get_list = function
  | CompareC (Ne, e, ListE [])
  | CompareC (Ne, ListE [], e)
  | CompareC (Ne, LengthE e, NumE 0L)
  | CompareC (Gt, LengthE e, NumE 0L)
  | CompareC (Ge, LengthE e, NumE 1L)
  | CompareC (Ne, NumE 0L, LengthE e)
  | CompareC (Lt, NumE 0L, LengthE e)
  | CompareC (Le, NumE 1L, LengthE e) -> Some e
  | _ -> None in
  match get_list cond1, get_list cond2 with
  | Some e1, Some e2 -> e1 = e2
  | _ -> false

let eq_cond cond1 cond2 =
  cond1 = cond2
  || both_empty cond1 cond2
  || both_non_empty cond1 cond2

let list_sum = List.fold_left ( + ) 0

let rec count_instrs instrs =
  instrs
  |> List.map (function
       | IfI (_, il1, il2) | EitherI (il1, il2) ->
           10 + count_instrs il1 + count_instrs il2
       | OtherwiseI il -> 1 + count_instrs il
       | TrapI | ReturnI _ -> 0
       | _ -> 1)
  |> list_sum

let rec unify_head acc l1 l2 =
  match (l1, l2) with
  | h1 :: t1, h2 :: t2 when h1 = h2 -> unify_head (h1 :: acc) t1 t2
  | _ -> (List.rev acc, l1, l2)

let intersect_list xs ys = List.filter (fun x -> List.mem x ys) xs
let diff_list xs ys = List.filter (fun x -> not (List.mem x ys)) xs

let dedup l =
  let rec aux acc = function
    | [] -> List.rev acc
    | x :: xs ->
      if List.mem x acc then
        aux acc xs
      else
        aux (x :: acc) xs
  in
  aux [] l

(** AL -> AL transpilers *)

(* Recursively append else block to every empty if *)
let rec insert_otherwise else_body instrs =
  let walk = insert_otherwise else_body in
  List.fold_left_map
    (fun visit_if inst ->
      match inst with
      | IfI (c, il, []) ->
          let _, il' = walk il in
          (true, IfI (c, il', else_body))
      | IfI (c, il1, il2) ->
          let visit_if1, il1' = walk il1 in
          let visit_if2, il2' = walk il2 in
          let visit_if = visit_if || visit_if1 || visit_if2 in
          (visit_if, IfI (c, il1', il2'))
      | OtherwiseI il ->
          let visit_if', il' = walk il in
          let visit_if = visit_if || visit_if' in
          (visit_if, OtherwiseI il')
      | EitherI (il1, il2) ->
          let visit_if1, il1' = walk il1 in
          let visit_if2, il2' = walk il2 in
          let visit_if = visit_if || visit_if1 || visit_if2 in
          (visit_if, EitherI (il1', il2'))
      | _ -> (visit_if, inst))
    false instrs

(* Merge two consecutive blocks: *)
(* - If they share same prefix *)
(* - If the latter block of instrs is a single Otherwise *)
let merge instrs1 instrs2 =
  let head, tail1, tail2 = unify_head [] instrs1 instrs2 in
  head @ match tail2 with
  | [ OtherwiseI else_body ] ->
      let visit_if, merged = insert_otherwise else_body tail1 in
      if not visit_if then
        print_endline
          ("Warning: No corresponding if for"
          ^ take 100 (Print.string_of_instrs 0 instrs2));
      merged
  | _ -> tail1 @ tail2

(** Enhance readability of AL **)

let rec unify_if instrs =
  List.fold_right
    (fun i il ->
      let new_i =
        match i with
        | IfI (c, il1, il2) -> IfI (c, unify_if il1, unify_if il2)
        | OtherwiseI il -> OtherwiseI (unify_if il)
        | EitherI (il1, il2) -> EitherI (unify_if il1, unify_if il2)
        | _ -> i
      in
      match (new_i, il) with
      | IfI (c1, body1, []), IfI (c2, body2, []) :: rest
        when c1 = c2 ->
          (* Assumption: common should have no side effect (replace) *)
          let common, own_body1, own_body2 = unify_head [] body1 body2 in
          let body = unify_if (common @ own_body1 @ own_body2) in
          IfI (c1, body, []) :: rest
      | _ -> new_i :: il)
    instrs []

let rec infer_else instrs =
  List.fold_right
    (fun i il ->
      let new_i =
        match i with
        | IfI (c, il1, il2) -> IfI (c, infer_else il1, infer_else il2)
        | OtherwiseI il -> OtherwiseI (infer_else il)
        | EitherI (il1, il2) -> EitherI (infer_else il1, infer_else il2)
        | _ -> i
      in
      match (new_i, il) with
      | IfI (c1, then_body1, else_body1), IfI (c2, else_body2, then_body2) :: rest
        when eq_cond c1 (neg c2) ->
          IfI (c1, then_body1 @ then_body2, else_body1 @ else_body2) :: rest
      | _ -> new_i :: il)
    instrs []

let if_not_defined =
  let transpile_cond = function
  | CompareC (Eq, e, OptE None) -> NotC (IsDefinedC e)
  | c -> c in
  Walk.walk_instr { Walk.default_config with post_cond = transpile_cond }

let lift f x = [f x]

let swap_if =
  let transpile_instr = function
  | IfI (c, il, []) -> IfI (c, il, [])
  | IfI (c, il1, il2) ->
      if count_instrs il1 <= count_instrs il2 then
        IfI (c, il1, il2)
      else
        IfI (neg c, il2, il1)
  | i -> i in
  Walk.walk_instr { Walk.default_config with post_instr = lift transpile_instr }

let rec return_at_last = function
| [] -> false
| [ TrapI ] | [ ReturnI _ ] -> true
| _ :: tl -> return_at_last tl

let early_return = Walk.walk_instr { Walk.default_config with post_instr =
  function
  | IfI (c, il1, il2) as i ->
    if return_at_last il1 then IfI (c, il1, []) :: il2 else [ i ]
  | i -> [i]
}

let unify_tail instrs1 instrs2 =
  let rev = List.rev in
  let rh, rt1, rt2 = unify_head [] (rev instrs1) (rev instrs2) in
  (rev rt1, rev rt2, rev rh)

let rec unify_if_tail instr =
  let new_ = List.concat_map unify_if_tail in
  match instr with
  | IfI (c, il1, il2) ->
      let then_il, else_il, finally_il = unify_tail (new_ il1) (new_ il2) in
      IfI (c, then_il, else_il) :: finally_il
  | OtherwiseI il -> [ OtherwiseI (new_ il) ]
  | EitherI (il1, il2) -> [ EitherI (new_ il1, new_ il2) ]
  | _ -> [ instr ]

let rec remove_unnecessary_branch path_cond instr =
  let new_ = List.concat_map (remove_unnecessary_branch path_cond) in
  match instr with
  | IfI (c, il1, il2) ->
    if List.exists (eq_cond c) path_cond then
      il1
    else if List.exists (eq_cond (neg c)) path_cond then
      il2
    else
      let new_il1 = List.concat_map (remove_unnecessary_branch (c :: path_cond)) il1 in
      let new_il2 = List.concat_map (remove_unnecessary_branch ((neg c) :: path_cond)) il2 in
      [ IfI (c, new_il1, new_il2) ]
  | OtherwiseI il -> [ OtherwiseI (new_ il) ]
  | EitherI (il1, il2) -> [ EitherI (new_ il1, new_ il2) ]
  | _ -> [ instr ]

let push_either =
  let push_either' = fun i -> match i with
    | EitherI (il1, il2) -> ( match ( Util.Lib.List.split_last il1) with
      | hds, IfI (c, then_body, []) -> EitherI (hds @ [ IfI (c, then_body, il2) ], il2)
      | _ -> i )
    | _ -> i in

  Walk.walk_instr { Walk.default_config with pre_instr = lift push_either' }

let rec remove_dead_assignment' il pair = List.fold_right (fun instr (acc, bounds) ->
  match instr with
  | IfI (c, il1, il2) ->
    let il1', bounds1 = remove_dead_assignment' il1 ([], bounds) in
    let il2', bounds2 = remove_dead_assignment' il2 ([], bounds) in
    IfI (c, il1', il2') :: acc, bounds1 @ bounds2 @ Free.free_cond c
  | EitherI (il1, il2) ->
    let il1', bounds1 = remove_dead_assignment' il1 ([], bounds) in
    let il2', bounds2 = remove_dead_assignment' il2 ([], bounds) in
    EitherI (il1', il2') :: acc, bounds1 @ bounds2
  | LetI (e1, e2) ->
    let bindings = (Free.free_expr e1) in
    if intersect_list bindings bounds = [] then
      acc, bounds
    else
      (instr :: acc), (diff_list bounds bindings) @ Free.free_expr e2
  | CallI (e1, _, args, nl_iters) ->
    let bindings = (Free.free_expr e1) in
    if intersect_list bindings bounds = [] then
      acc, bounds
    else
      let new_bounds = List.concat_map Free.free_expr args @ List.concat_map Free.free_ns_iter nl_iters in
      (instr :: acc), (diff_list bounds bindings) @ new_bounds
  | _ ->
    instr :: acc, bounds @ Free.free_instr instr
) il pair

let remove_dead_assignment il = remove_dead_assignment' il ([], []) |> fst

let enhance_readability instrs =
  instrs
  |> unify_if
  |> List.concat_map if_not_defined
  |> infer_else
  |> List.concat_map unify_if_tail
  |> List.concat_map (remove_unnecessary_branch [])
  |> List.concat_map swap_if
  |> List.concat_map early_return
  |> remove_dead_assignment

(** Walker-based Translpiler **)
let rec mk_access ps base =
  match ps with
  | h :: t -> AccessE (base, h) |> mk_access t
  | [] -> base

(* Hide state and make it implicit from the prose. Can be turned off. *)
let hide_state_args = List.filter (function
  | PairE (NameE "s", NameE "f")
  | NameE "z" -> false
  | PairE (NameE s, NameE "f")
    when String.starts_with ~prefix:"s_" s -> false
  | PairE (NameE s, NameE f)
    when String.starts_with ~prefix:"s_" s
    && String.starts_with ~prefix:"f_" f
      -> false
  | NameE "s" -> false
  | NameE s when String.starts_with ~prefix:"s_" s -> false
  | _ -> true)

let hide_state_instr = function
  | ReturnI (Some (PairE (ReplaceE (e1, pl, e2), NameE "f")))
  | ReturnI (Some (PairE (NameE "s", ReplaceE (e1, pl, e2)))) ->
      let rpl = List.rev pl in
      let target =
        List.tl rpl
        |> List.fold_right
          (fun p acc -> AccessE (acc, p))
      in
      [ ReplaceI (target e1, List.hd rpl, e2) ]
  | ReturnI (Some (PairE (NameE "s", NameE "f"))) -> []
  | ReturnI (Some (PairE ((NameE s), NameE f)))
    when String.starts_with ~prefix:"s_" s
      && String.starts_with ~prefix:"f_" f -> []

  | ReturnI (Some (NameE "s")) -> []
  | ReturnI (Some (NameE s))
    when String.starts_with ~prefix:"s_" s -> []
  (* Perform *)
  | LetI (PairE (NameE s, NameE f), AppE (fname, el))
    when String.starts_with ~prefix:"s_" s
      && String.starts_with ~prefix:"f_" f -> [ PerformI (fname, el) ]
  | LetI (NameE s, AppE (fname, el))
    when String.starts_with ~prefix:"s_" s -> [ PerformI (fname, el) ]
  (* Append *)
  | LetI (NameE s, ExtendE (e1, ps, ListE [ e2 ], Back) )
    when String.starts_with ~prefix:"s_" s ->
      [ AppendI (mk_access ps e1, e2) ]
  (* Replace *)
  | LetI (NameE s, ReplaceE (e1, ps, e2))
    when String.starts_with ~prefix:"s_" s ->
      begin match List.rev ps with
      | h :: t -> [ ReplaceI (mk_access (List.rev t) e1, h, e2) ]
      | _ -> failwith "Invalid replace"
      end
  | CallI (lhs, f, args, nl_iterl) -> [ CallI (lhs, f, hide_state_args args, nl_iterl) ]
  | PerformI (f, args) -> [ PerformI (f, hide_state_args args) ]
  | i -> [ i ]


let hide_state = function
  | AppE (f, args) -> AppE (f, hide_state_args args)
  | ListE [ NameE "s"; e ]
  | ListE [ NameE "s'"; e ] -> e
  | ListE [ NameE s; e ] when String.starts_with ~prefix:"s_" s -> e
  | e -> e

let simplify_record_concat = function
  | ConcatE (e1, e2) ->
    let nonempty = function ListE [] | OptE None -> false | _ -> true in
    let remove_empty_field = function
      | RecordE r -> RecordE (Record.filter (fun _ v -> nonempty v) r)
      | e -> e in
    ConcatE (remove_empty_field e1, remove_empty_field e2)
  | e -> e

let flatten_if = function
  | IfI (c1, [IfI (c2, il1, il2)], []) -> IfI (BinopC (And, c1, c2), il1, il2)
  | i -> i

let transpiler algo =
  let walker =
    Walk.walk
      { Walk.default_config with
        post_instr = composite_instr hide_state_instr (lift flatten_if);
        post_expr = composite hide_state simplify_record_concat
      }
  in

  match walker algo with
  | RuleA (name, params, body) -> (match params with
    | PairE (_, NameE "f") :: tail ->
        RuleA (name, tail, LetI (NameE "f", GetCurFrameE) :: body |> remove_dead_assignment)
    | NameE "s" :: tail ->
        RuleA (name, tail, body)
    | _ -> RuleA(name, params, body))
  | FuncA (name, params, body) -> (match params with
    | PairE (_, NameE "f") :: tail ->
        FuncA (name, tail, LetI (NameE "f", GetCurFrameE) :: body |> remove_dead_assignment)
    | NameE "s" :: tail ->
        FuncA (name, tail, body)
    | _ -> FuncA(name, params, body))

let app_remover =
  let side_effect f e = f e; e in

  let iter_stack = ref [] in
  let names = ref [ref []] in (* Upon leavaing an expr, the head contains all names apearing in it *)
  let calls = ref [] in
  let requires = ref [] in

  (* Helper *)
  let add_call x =
    let cur_calls = List.hd !calls in
    cur_calls := x :: !cur_calls in
  let pop_calls _ =
    let hd, tl = Util.Lib.List.split_hd !calls in
    calls := tl;
    hd in
  let remove_iter_var = function
    | ListN (e, Some _) -> ListN (e, None)
    | iter -> iter in

  (* pre_expr *)
  let push_iter = side_effect (function
    | IterE (_, ns, i) -> iter_stack := (ns, i) :: !iter_stack
    | _ -> () ) in
  let init_names = side_effect (fun _ -> names := ref [] :: !names) in

  let pre_expr = composite init_names push_iter in

  (* post expr *)

  let rewrite_names (names, iter) =
    let new_names = List.concat_map (fun x ->
      x :: List.filter_map (fun (y, x') ->
        if x = x' then Some y else None
      ) !requires
    ) names |> dedup in
    (new_names, iter) in

  let call_id = ref 0 in
  let call_prefix = "r_" in
  let get_fresh () =
    let id = !call_id in
    call_id := id + 1;
    call_prefix ^ (string_of_int id) in

  let bind_app e = match e with
    | AppE (f, args) ->
      let fresh_name = get_fresh() in
      let rec to_fresh_exp iter = match iter with
      | [] -> NameE fresh_name
      | (_, i) :: tl ->
        let new_i = remove_iter_var i in
        IterE (to_fresh_exp tl, [fresh_name], new_i) in

      let iters = !iter_stack |> List.map rewrite_names in

      (* Append new CallI instruction *)
      let fresh_exp = to_fresh_exp iters in
      add_call (fresh_exp, f, args, iters);

      (* Find all required names for this new fresh variable *)
      let required_names = intersect_list
        (List.concat_map fst iters)
        !(List.hd !names)
      in
      requires := List.map (fun n -> (fresh_name, n)) required_names @ !requires;

      (* Discard all names appearing in args *)
      names := (ref []) :: (List.tl !names);

      NameE (fresh_name)
    | _ -> e in

  let pop_iter e = match e with
    | IterE (e, ns, iter) ->
      (* Pop iter from the stack *)
      iter_stack := List.tl !iter_stack;

      (* Rewrite itered names *)
      let new_ns = List.filter (fun n' ->
        List.exists (fun n -> (n' = n) || List.mem (n', n) !requires) ns
      ) !(List.hd !names) in

      (* Rewrite dim *)
      let new_iter = match iter with
      | ListN (e, Some n) -> if List.mem n new_ns then iter else ListN (e, None)
      | _ -> iter in

      IterE (e, new_ns, new_iter)
    | _ -> e in

  let pop_names = side_effect (fun e ->
    let hd1, tl1 = Util.Lib.List.split_hd !names in
    let hd2, tl2 = Util.Lib.List.split_hd tl1 in
    match e with
    | NameE n ->
      names := (ref (n :: (!hd1 @ !hd2) |> dedup)) :: tl2
    | _ ->
      names := (ref (!hd1 @ !hd2 |> dedup)) :: tl2
  ) in

  let post_expr = composite pop_names (composite pop_iter bind_app) in

  (* pre_instr *)
  let init i =
    call_id := 0;
    calls := (ref []) :: !calls;
    names := [ref []];
    [i] in

  (* post_instr *)
  let rec is_pure_name = function
  | NameE _ -> true
  | IterE (e, _, _) -> is_pure_name e
  | _ -> false in

  let quad_to_call (a, b, c, d) = CallI (a, b, c, d) in

  let remove_redundant_assign i = List.rev (
    let cur_calls = !(pop_calls ()) in
    match i with
    | LetI (lhs, rhs) when List.length cur_calls > 0 && is_pure_name rhs ->
      let (_, f, args, iters) = List.hd cur_calls in
      let new_apps = (lhs, f, args, iters) :: List.tl cur_calls in
      List.map quad_to_call new_apps
    | _ -> i :: List.map quad_to_call cur_calls
  ) in

  Walk.walk { Walk.default_config with
    pre_instr = init;
    post_instr = remove_redundant_assign;
    pre_expr = pre_expr;
    post_expr = post_expr;
  }

(* Applied for reduction rules: infer assert from if *)
let rec count_if instrs = match instrs with
  | [] -> 0
  | IfI _ :: tl -> 1 + count_if tl
  | _ :: tl -> count_if tl
let rec infer_assert instrs =
  if count_if instrs = 1 then
    let (hd, tl) = Util.Lib.List.split_last instrs in
    match tl with
    | IfI (c, il1, []) -> hd @ AssertI(c) :: (il1 |> infer_assert)
    | _ -> instrs
  else instrs

let rec enforce_return_r rinstrs =
  let rev = List.rev in
  match rinstrs with
  | [] -> []
  | tl :: hd -> match tl with
    | ReturnI _ | TrapI -> rinstrs
    | IfI (c, il1, il2) ->
      ( match enforce_return' il1, enforce_return' il2 with
      | [], [] -> enforce_return_r hd
      | new_il, [] -> rev new_il @ (AssertI c :: hd)
      | [], new_il -> rev new_il @ (AssertI (neg c) :: hd)
      | new_il1, new_il2 -> IfI (c, new_il1, new_il2) :: hd )
    | OtherwiseI il -> OtherwiseI (enforce_return' il) :: hd
    | EitherI (il1, il2) ->
      ( match enforce_return' il1, enforce_return' il2 with
      | [], [] -> enforce_return_r hd
      | new_il, []
      | [], new_il -> rev new_il @ hd
      | new_il1, new_il2 -> EitherI (new_il1, new_il2) :: hd )
    | _ -> enforce_return_r hd
and enforce_return' instrs = instrs |> List.rev |> enforce_return_r |> List.rev

let contains_return il =
  let ret = ref false in
  List.map (Walk.walk_instr { Walk.default_config with
    pre_instr = (fun i ->
      ( match i with | ReturnI _ | TrapI -> ret := true | _ -> () );
      [ i ]
    )
  }) il |> ignore;
  !ret

(** If intrs contain a return statement, make sure that every path has return statement in the end **)
let enforce_return instrs =
  if contains_return instrs then enforce_return' instrs else instrs
