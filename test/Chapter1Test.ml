open MoreOCaml.Chapter1
open OUnit2

let fold_left_test _ = 
  assert_equal 6 (fold_left (+) 0 [1; 2; 3])

let fold_right_test _ = 
  assert_equal 6 (fold_right (+) 0 [1; 2; 3;])

let mem_test_positive _ = 
  assert_equal true (mem 10 [1; 10; 20; ])

let mem_test_negative _ =
  assert_equal false (mem 10 [1; 11; 20; ])

let rev_test _ = 
  assert_equal [3; 2; 1] (rev [1; 2; 3; ])

let to_set_test _ = 
  assert_equal ["orange"; "apple"] (setify ["apple"; "apple"; "apple"; "orange"; "orange"])

let chapter1_tests = [
  "should be able to fold left lists" >:: fold_left_test;
  "should be able to fold right lists" >:: fold_right_test;
  "should be able to check membership using folds positive" >:: mem_test_positive;
  "should be able to check membership using folds negative" >:: mem_test_negative;
  "should be able to reverse a list using folds" >:: rev_test;
  "should be able to dedup a list using fold" >:: to_set_test;
]
let chapter1_suite = "Chapter1 suite" >::: chapter1_tests
let _ = run_test_tt_main chapter1_suite

