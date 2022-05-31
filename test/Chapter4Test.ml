open OUnit
open MoreOCaml.Chapter4
let read_words_test _ = 
  assert_equal ["quick"; "brown"; "fox"] (read_words (input_of_string "Quick! (Brown)   Fox. "))
let chapter4_tests = [
  "Read Words from String" >:: read_words_test;
]
let chapter4_suite = "Chapter4 Suite" >::: chapter4_tests
let _ = run_test_tt_main chapter4_suite