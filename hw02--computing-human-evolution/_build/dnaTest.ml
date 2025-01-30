;; open Assert
;; open Dna
;; open Helices

(* The assertion library by default will run _all_ of the test cases associated
 * with this program.  However, while debugging you may prefer to have the
 * testing stop on the first failure that is encountered. Comment the line
 * below to turn off first-failure. *)
;; stop_on_failure ()


(******************************************************************************)
(********************** REMEMBER TO WRITE TESTS FIRST! ************************)
(********************* WRITE ALL TESTS BELOW THIS POINT ***********************)
(******************************************************************************)

(* If you'd like to test your helper functions, you should do that in dna.ml.
 * The only way you can test them in this file is by declaring them in
 * dna.mli - however if you do so your code won't compile with the grading
 * server because you don't submit your own dna.mli!
 *
 * If you'd like to write trees to use in your tests, put them in this file.
 * When writing trees of depth greater than 1 for tests, please indent all 
 * children by 4 spaces, separate children by line breaks, and place parentheses
 * similar to the following tree:
 *
 * LNode (
 *     LNode (
 *         LLeaf h1,
 *         h2,
 *         LLeaf h3
 *     ),
 *     h4,
 *     LLeaf h5
 * )
 *
*)

(************************ complementary_helix tests ***************************)

(* Here's an example test case.  Be sure to fill in the incomplete test cases
 * we provided. *)

let test () : bool =
  complementary_helix [T] = [A]
;; run_test "complementary_helix singleton" test

let test () : bool =
  complementary_helix [C; T; T; C] = [G; A; A; G]
;; run_test "complementary_helix multi-element list" test

let test () : bool =
  complementary_helix [C; T; A; A; T; G; T] = [G; A; T; T; A; C; A]
;; run_test "complementary_helix multi-element list with all nucleotides" test

let test () : bool = 
  complementary_helix [] = []
;; run_test "complementary_helix empty list" test

(*************************** hamming_distance tests ***************************)

(* Here's an example test case.  Be sure to add your own--we will be grading the
 * tests for this problem. *)

let test () : bool =
  hamming_distance [G; A] [G; T] = 1
;; run_test "hamming_distance one different nucleotide" test

let test () : bool =
  hamming_distance [G; A] [G; A] = 0
;; run_test "hamming_distance no different nucleotides" test

let test () : bool =
  hamming_distance [G; A; C; T; A; G; T; A; C; T; G; A] 
                   [G; T; G; A; G; A; T; A; G; G; A; A] = 8
;; run_test "hamming_distance many different nucleotides" test

let test () : bool =
  hamming_distance [] [] = 0
;; run_test "hamming_distance empty lists" test

(************************ decreasing_similarity tests *************************)

(* Be sure to add your own test cases -- we will be grading the tests for this 
 * problem. *)

(* Add your test here *)

let test () : bool = 
  decreasing_similarity []
;; run_test "decreasing_similarity empty list" test

let test () : bool = 
  decreasing_similarity [gorilla]
;; run_test "decreasing_similarity singleton list" test

let test () : bool = 
  decreasing_similarity [chimpanzee; gorilla; orangutan; siamang; 
                         pileated_gibbon; white_cheeked_gibbon; lar_gibbon]
;; run_test "decreasing_similarity all decreasing" test

let test () : bool = 
  not(decreasing_similarity [chimpanzee; gorilla; orangutan; siamang; 
                         pileated_gibbon; lar_gibbon; white_cheeked_gibbon])
;; run_test "decreasing_similarity one increasing list" test


(**************************** acids_of_helix tests ****************************)

(* Here's an example test case.  Be sure to add your own--we will be grading the
 * tests for this problem.  Also be sure to fill in the incomplete test case
 * we provided. *)

let test () : bool =
  acids_of_helix [A; G; T; A; C] = [Met]
;; run_test "acids_of_helix single codon" test

let test () : bool =
  (acids_of_helix [A; G; T; A; C; T; A; C; T; C; C]) = [Met; Met; Arg]
;; run_test "acids_of_helix multiple Mets at start" test

let test () : bool =
  (acids_of_helix [A; G; A; T; C; T; A; C; T; A; C; T; C; C]) = [Met; Met; Arg]
;; run_test "acids_of_helix END before Met" test

let test () : bool =
  (acids_of_helix [A; G; T; A; C; T; A; C; T; C; C; A; T; C; T; C; C]) 
  = [Met; Met; Arg]
;; run_test "acids_of_helix END after Met" test

let test () : bool =
  (acids_of_helix []) = []
;; run_test "acids_of_helix empty list" test

let test () : bool =
  (acids_of_helix [A; G; A; T; C; T; C; T; A; T; C; C]) = []
;; run_test "acids_of_helix no Met" test

let test () : bool =
  (acids_of_helix [T; A; C]) = [Met]
;; run_test "acids_of_helix single Met nothing else" test


(************ Kudos problem: all_acids_of_helix tests *************************)

(* These tests are commented out because they are for the kudos problem.
 * If you attempt this problem, remember to uncomment the tests. And write
 * some of your own! *)

(*
let test () : bool =
  all_acids_of_helix [T; A; C; A; C; T] = [[Met]]
;; run_test "all_acids_of_helix  [T; A; C; A; C; T]" test

let test () : bool =
  all_acids_of_helix [T; A; C; A; C; T; T; A; C; A; C; T] = [[Met]; [Met]]
;; run_test "all_acids_of_helix [T; A; C; A; C; T; T; A; C; A; C; T]" test

let test () : bool =
  all_acids_of_helix [T; A; C; A; C; T; G; A; T; A; C; A; C; T] = [[Met]; [Met]]
;; run_test "all_acids_of_helix [T; A; C; A; C; T; G; A; T; A; C; A; C; T]" test
*)


(***************************** count_leaves tests *****************************)

(* Here are two example tests cases. Both trees should contain exactly four
 * species. Don't forget to add your own test cases! *)

let test () : bool =
  count_leaves (greater_apes ()) = 4
;; run_test "count_leaves greater_apes" test

let test () : bool =
  count_leaves (lesser_apes ()) = 4
;; run_test "count_leaves lesser_apes" test

let test () : bool =
  count_leaves (Leaf orangutan) = 1
;; run_test "count_leaves 1 Leaf" test

let test () : bool =
  count_leaves (Node (greater_apes (), lesser_apes ())) = 8
;; run_test "count_leaves all apes" test

let test () : bool =
  count_leaves (Node (greater_apes (), greater_apes ())) = 8
;; run_test "count_leaves duplicates" test

(**************************** helix_of_tree tests *****************************)

(* Here's an example test case.  Be sure to add your own--we will be grading the
 * tests for this problem. *)

let test () : bool =
  helix_of_tree (LNode (LLeaf [T], [A], LLeaf [G])) = [A]
;; run_test "helix_of_tree depth-2 tree" test

let test () : bool =
  helix_of_tree (LLeaf [T]) = [T]
;; run_test "helix_of_tree lleaf" test

let test () : bool =
  helix_of_tree (LNode (LNode (LLeaf [T], [T], LLeaf [T]), [A], LLeaf [G]))
   = [A]
;; run_test "helix_of_tree depth-3 tree" test

(**************************** unlabel_tree tests ******************************)

(* Here's an example test case.  Be sure to add your own--we will be grading the
 * tests for this problem. *)

let test () : bool =
  unlabel_tree (LNode (LLeaf [T], [A], LLeaf [G])) =
  Node (Leaf [T], Leaf [G])
;; run_test "unlabel_tree depth-2 tree" test

let test () : bool =
  unlabel_tree (LLeaf [T]) = Leaf [T]
;; run_test "unlabel_tree lleaf" test

let test () : bool =
  unlabel_tree (LNode (LNode (LLeaf [T], [A], LLeaf [G]), [A], LLeaf [G])) =
  Node (Node (Leaf [T], Leaf [G]), Leaf [G])
;; run_test "unlabel_tree depth-3 tree" test


(************************* guess_parent_helix tests ***************************)

(* Here's an example test case.  Be sure to fill in the incomplete test case
 * we provided *)

let test () : bool =
  guess_parent_helix [T; C; A] [G; C; A] = [A; C; A]
;; run_test "guess_parent_helix one difference" test

let test () : bool =
  guess_parent_helix [A; C; G; G; T; A; C]
    [C; T; G; C; T; A; A] = [A; A; G; A; T; A; A]
;; run_test "guess_parent_helix multiple differences" test

let test () : bool =
  guess_parent_helix [] [] = []
;; run_test "guess_parent_helix empty lists" test

(************************ add_ancestor_labels tests ***************************)

(* Here's an example test case.  Be sure to fill in the incomplete test cases
 * we provided and don't forget to add your own test cases! *)

let test () : bool =
  add_ancestor_labels (Node (Leaf [T], Leaf [G])) =
  LNode (LLeaf [T], [A], LLeaf [G])
;; run_test "add_ancestor_labels depth-2 tree" test

let test () : bool =
  add_ancestor_labels (Node (Leaf [T; C], Leaf [T; C])) =
  LNode (LLeaf [T;C], [T;C], LLeaf [T;C])
;; run_test "add_ancestor_labels also depth-2 tree" test

let test () : bool =
  add_ancestor_labels (Leaf [T; C]) =
  LLeaf [T;C]
;; run_test "add_ancestor_labels leaf" test

let test () : bool =
  add_ancestor_labels (Node(Node(Leaf[A; T; G], Leaf[C; T; G]), Leaf[A; A; G]))=
  LNode (LNode (LLeaf[A; T; G], [A;T;G], LLeaf[C; T; G]), [A;A;G], LLeaf[A;A;G])
;; run_test "add_ancestor_labels depth-3 tree" test



(************************ parent_child_hamming tests **************************)

(* Here we give you one example test.  Add your own tests--we will grade the
 * test cases for this problem.  Be sure to test for trees of greater depth. *)

let test () : bool =
  parent_child_hamming (LNode (LLeaf [T], [A], LLeaf [G])) = 2
;; run_test "parent_child_hamming depth-2 tree, all different" test

let test () : bool =
  parent_child_hamming (LNode (LLeaf [A], [A], LLeaf [A])) = 0
;; run_test "parent_child_hamming depth-2 tree, all same" test

let test () : bool =
  parent_child_hamming (LLeaf [T]) = 0
;; run_test "parent_child_hamming lleaf" test

let test () : bool =
  parent_child_hamming (LNode (LNode (LLeaf[A; T; G], [A;T;G], LLeaf[C; T; G]),
   [A;A;G], LLeaf[A;A;G])) = 2
;; run_test "parent_child_hamming depth-3 tree, some different" test

let test () : bool =
  parent_child_hamming (LNode (LNode (LLeaf[A; T; G], [A;T;G], LLeaf[A; T; G]),
   [A;T;G], LLeaf[A;T;G])) = 0
;; run_test "parent_child_hamming depth-3 tree, all same" test

(**************************** simplest_tree tests *****************************)

(* Here are two example test cases. Don't forget to add your own test cases!
 *
 * Note: If you want to write a test that fails on purpose, then you can use
 * run_failing_test. run_failing_test will fail if the function returns true or 
 * false, and will pass if the function throws an error *)

let test () : bool =
  simplest_tree [] = (LLeaf [], 0)
;; run_failing_test "simplest_tree empty" test

let test () : bool =
  let t1 = LNode (LLeaf [A], [T], LLeaf [C]) in
  let t2 = LNode (
               LLeaf [G; T],
               [A; T],
               LNode (
                   LLeaf [T; T], 
                   [T; T], 
                   LLeaf [C; G]
               )
           ) in
  simplest_tree [t1; t2] = (t1, 2)
;; run_test "simplest_tree two tree list" test

let test () : bool =
  simplest_tree [LLeaf [T]] = (LLeaf [T], 0)
;; run_test "simplest_tree singleton" test

let test () : bool =
  let t1 = LNode (LLeaf [A], [T], LLeaf [C]) in
  let t2 = LNode (
               LLeaf [G; T],
               [A; T],
               LNode (
                   LLeaf [T; T], 
                   [T; T], 
                   LLeaf [C; G]
               )
           ) in
  let t3 = LNode (LNode (LLeaf[A; T; G], [A;T;G], LLeaf[C; T; G]),
   [A;A;G], LLeaf[A;A;G]) in
  simplest_tree [t1; t2; t3] = (t1, 2)
;; run_test "simplest_tree three tree list with two equally simple" test


(********************* simplest_greater_ape_tree tests ************************)

(* Refactoring is the process of rewriting your code to make it simpler and
 * more general without changing its behavior. Therefore, it is easy to write
 * test cases for the new version---they should compute the same answers
 * that we did before. Because of this, we don't have to write any new test
 * cases! *)

let test () : bool =
  find_simplest_tree gorilla human chimpanzee orangutan =
  simplest_greater_ape_tree ()
;; run_test "simplest_greater_ape_tree" test

let test () : bool =
  find_simplest_tree lar_gibbon pileated_gibbon siamang white_cheeked_gibbon =
  simplest_lesser_ape_tree ()
;; run_test "simplest_lesser_ape_tree" test

;; print_endline "dnaTest.ml: ran to completion."
