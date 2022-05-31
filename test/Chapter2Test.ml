open OUnit
open MoreOCaml.Chapter2

let lazy_list_head_test _ = 
  assert_equal 10 (lhd (lseq 10))
let lazy_list_tail_test _ = 
  assert_equal (lhd (lseq 11)) (lhd (ltl (lseq 10)))
let lazy_take_test _ = 
  assert_equal [1; 2; 3; 4] (ltake 4 (lseq 1))

let lazy_drop_test _ = 
  assert_equal 5 (lhd (ldrop 5 (lseq 0)))
  let chapter2_tests = [
  "Should be able to get the head of lazy list" >:: lazy_list_head_test;
  "Should be able to get the tail of the lazy list" >:: lazy_list_tail_test;
  "Should be able to take values from a lazy list" >:: lazy_take_test;
  "Should be able to drop values from a lazy list" >:: lazy_drop_test;
]
let chapter2_suite = "Chapter2 Suite" >::: chapter2_tests
let _ = run_test_tt_main chapter2_suite