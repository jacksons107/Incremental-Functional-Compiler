open Compiler
open Ast
open Elam
open Type_infer

(* [infer] returns types containing fresh [TVar (ref (Unbound n))]s whose
   exact numbering depends on how many fresh vars earlier tests allocated
   from the shared global counter. Alpha-rename each distinct still-unbound
   var (after fully pruning Link chains, including inside compound types) to
   a canonical sequential id, so tests can assert type *shape* instead of a
   specific counter value. *)
let normalize_typ ty =
  let module IntMap = Map.Make (Int) in
  let counter = ref 0 in
  let seen = ref IntMap.empty in
  let rec go ty =
    match prune ty with
    | TVar { contents = Unbound id } -> (
        match IntMap.find_opt id !seen with
        | Some n -> TVar (ref (Unbound n))
        | None ->
            let n = !counter in
            incr counter;
            seen := IntMap.add id n !seen;
            TVar (ref (Unbound n)))
    | TVar { contents = Link _ } -> assert false (* prune already followed it *)
    | TInt -> TInt
    | TBool -> TBool
    | TString -> TString
    | TConstr c -> TConstr c
    | TList t -> TList (go t)
    | TLam (a, b) -> TLam (go a, go b)
  in
  go ty

let typ = Alcotest.testable Ast.pp_typ ( = )

let check name e expected =
  Alcotest.test_case name `Quick (fun () ->
      Alcotest.check typ name (normalize_typ expected)
        (normalize_typ (infer e empty_env)))

let check_env name env e expected =
  Alcotest.test_case name `Quick (fun () ->
      Alcotest.check typ name (normalize_typ expected)
        (normalize_typ (infer e env)))

let check_raises_type_error name e =
  Alcotest.test_case name `Quick (fun () ->
      Alcotest.check_raises name (Type_error "placeholder") (fun () ->
          try ignore (infer e empty_env)
          with Type_error _ -> raise (Type_error "placeholder")))

let box_env = setup_env [ TypeDef ("box", "Box", [ TInt ]) ] empty_env

let intlist_env =
  setup_env
    [
      TypeDef ("intlist", "Nil", []);
      TypeDef ("intlist", "Cons", [ TInt; TConstr "intlist" ]);
    ]
    empty_env

let cons_1_nil =
  EApp (EApp (EConstr ("Cons", 2), EInt 1), EConstr ("Nil", 0))

let mutual_env order =
  let even_defs =
    [
      TypeDef ("even_list", "ENil", []);
      TypeDef ("even_list", "ECons", [ TInt; TConstr "odd_list" ]);
    ]
  in
  let odd_defs = [ TypeDef ("odd_list", "OCons", [ TInt; TConstr "even_list" ]) ] in
  setup_env (match order with `EvenFirst -> even_defs @ odd_defs | `OddFirst -> odd_defs @ even_defs)
    empty_env

let econs_1_ocons_2_enil =
  EApp
    ( EApp (EConstr ("ECons", 2), EInt 1),
      EApp (EApp (EConstr ("OCons", 2), EInt 2), EConstr ("ENil", 0)) )

let suite =
  [
    check "int literal" (EInt 5) TInt;
    check "bool literal" (EBool true) TBool;
    check "string literal" (EString "hi") TString;
    check "bare lambda has shape a -> a"
      (ELam ("x", EVar "x"))
      (let a = TVar (ref (Unbound 0)) in
       TLam (a, a));
    (* The crux of let-polymorphism: `id` is generalized once at its
       binding site, then instantiated at two different, unrelated types
       across two separate calls to [infer] sharing the same env -- proving
       it isn't accidentally pinned to one monomorphic type. *)
    (let id_env =
       let id_typ = infer (ELam ("x", EVar "x")) empty_env in
       Env.add "id" (generalize id_typ empty_env) empty_env
     in
     check_env "let-polymorphism: id applied to an int" id_env
       (EApp (EVar "id", EInt 5))
       TInt);
    (let id_env =
       let id_typ = infer (ELam ("x", EVar "x")) empty_env in
       Env.add "id" (generalize id_typ empty_env) empty_env
     in
     check_env "let-polymorphism: id applied to a bool (same scheme)" id_env
       (EApp (EVar "id", EBool true))
       TBool);
    check "EIf applied to all three arguments"
      (EApp (EApp (EApp (EIf, EBool true), EInt 1), EInt 2))
      TInt;
    check "ECons/EHead build and destructure a list"
      (EApp (EHead, EApp (EApp (ECons, EInt 1), EEmpty)))
      TInt;
    check "ETail preserves the list's element type"
      (EApp (ETail, EApp (EApp (ECons, EInt 1), EEmpty)))
      (TList TInt);
    check "EEq on two ints" (EApp (EApp (EEq, EInt 1), EInt 2)) TBool;
    check_env "sum-type constructor typed via setup_env" box_env
      (EApp (EConstr ("Box", 1), EInt 5))
      (TConstr "box");
    check_env "EUnpack yields the constructor field's type at that index"
      box_env
      (EUnpack ("Box", EApp (EConstr ("Box", 1), EInt 5), 0))
      TInt;
    check_env "a self-referential constructor's function type includes its \
               own type"
      intlist_env (EConstr ("Cons", 2))
      (TLam (TInt, TLam (TConstr "intlist", TConstr "intlist")));
    check_env "a value built from a self-referential constructor has the \
               recursive type"
      intlist_env cons_1_nil (TConstr "intlist");
    check_env "unpacking a self-referential constructor's recursive field \
               yields the recursive type"
      intlist_env
      (EUnpack ("Cons", cons_1_nil, 1))
      (TConstr "intlist");
    check_env "two mutually-referencing types type-check (even declared \
               first)"
      (mutual_env `EvenFirst) econs_1_ocons_2_enil (TConstr "even_list");
    check_env "two mutually-referencing types type-check regardless of \
               declaration order (odd declared first)"
      (mutual_env `OddFirst) econs_1_ocons_2_enil (TConstr "even_list");
    check_raises_type_error "applying an int as a function is a type error"
      (EApp (EInt 1, EInt 2));
    check_raises_type_error "self-application (\\x -> x x) is an infinite type"
      (ELam ("x", EApp (EVar "x", EVar "x")));
    check_raises_type_error "an unbound variable is a type error" (EVar "z");
  ]
