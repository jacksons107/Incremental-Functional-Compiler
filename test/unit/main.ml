let () =
  Alcotest.run "compiler"
    [
      ("lexer", Test_lexer.suite);
      ("parser", Test_parser.suite);
      ("desugar", Test_desugar.suite);
      ("patterns", Test_patterns.suite);
      ("type_infer", Test_type_infer.suite);
      ("lam_to_comb", Test_lam_to_comb.suite);
      ("comb_to_j", Test_comb_to_j.suite);
      ("serialize", Test_serialize.suite);
      ("dep_order", Test_dep_order.suite);
      ("driver", Test_driver.suite);
    ]
