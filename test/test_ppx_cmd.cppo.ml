open OUnit2

let show_cmd_result show_cmd = function
  | Ok cmd -> "Ok (" ^ show_cmd cmd ^ ")"
  | Error e -> "Error (" ^ e ^ ")"

type basic = { id : int; name : string [@short 'n'] }
[@@deriving cmd, show] [@@warning "-69"]

let test_basic _ =
  let assert_equal = assert_equal ~printer:(show_cmd_result show_basic) in
  assert_equal
    (Ok { id = 7; name = "joe" })
    (try_parse_basic_with [ "--id"; "7"; "--name"; "joe" ]);
  assert_equal
    (Ok { id = -518; name = "bob" })
    (try_parse_basic_with [ "--id"; "-518"; "-n"; "bob" ]);
  assert_equal (Error {|no value provided for flag "--name"|})
    (try_parse_basic_with [ "--id"; "518" ])

type types = {
  a : int;
  b : int32;
  c : Int32.t;
  d : int64;
  e : Int64.t;
  f : nativeint;
  g : Nativeint.t;
  h : float;
  i : char;
  j : int ref;
  k : string list;
  l : string array;
  m : char option;
  n : int64 Lazy.t;
  o : bool;
}
[@@deriving cmd, show] [@@warning "-69"]

let test_types _ =
  let assert_equal = assert_equal ~printer:(show_cmd_result show_types) in
  assert_equal
    (Ok
       {
         a = 5;
         b = 95l;
         c = 6l;
         d = 31L;
         e = 1351835L;
         f = -168541n;
         g = -88135125n;
         h = 4318.51685;
         i = 'q';
         j = ref 63515;
         k = [ "abc"; "xyz" ];
         l = [| "foo"; "bar" |];
         m = Some 'i';
         n = lazy 81631L;
         o = true;
       })
    (try_parse_types_with
       [
         "--a";
         "5";
         "--b";
         "95";
         "--c";
         "6";
         "--d";
         "31";
         "--e";
         "1351835";
         "--f";
         "-168541";
         "--g";
         "-88135125";
         "--h";
         "4318.51685";
         "--i";
         "q";
         "--j";
         "63515";
         "--k";
         "abc,xyz";
         "--l";
         "foo,bar";
         "--m";
         "i";
         "--n";
         "81631";
         "--o";
       ])

let suite =
  "Test deriving(cmd)"
  >::: [ "test_basic" >:: test_basic; "test_types" >:: test_types ]

let _ = run_test_tt_main suite
