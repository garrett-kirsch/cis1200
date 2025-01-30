(******************************************************************************)
(* PROBLEM 5: WRITING TEST CASES                                              *)
(******************************************************************************)

(* The module `SetTest` defined below is a reuseable component that we'll use
   to test other modules that conform to the `SET` interface. When `SetTest`
   is instantiated with a particular set implementation, it will run all of
   its test cases against the set type defined in that implementation.  This
   means that the _same_ tests can be used for both the OrderedListSet and
   BSTSet implementations -- this makes sense because all implementations of
   `SET` should behave the same!

   Read through the module, then write your test cases in the space provided
   below. Make sure NOT to test for structural equality with sets.  Instead,
   use the equals function specified in the interface.  Your TAs will be
   grading the completeness of your tests. *)

module SetTest (SetImpl: SetInterface.SET) = struct
  ;; open SetImpl

  (* We first redefine the `run_test` and `run_failing_test` functions so that
     they prepend the name of the set we're testing to the test description. *)

  let run_test desc = Assert.run_test (debug_name ^ ": " ^ desc)
  let run_failing_test desc = Assert.run_failing_test (debug_name ^ ": " ^ desc)

  ;; print_endline ("\n--- Running tests for " ^ debug_name ^ " ---")

  (* Here are a couple of test cases to help get you started... *)

  let test () : bool =
    is_empty empty
  ;; run_test "is_empty: call on empty returns true" test

  (* Note that some tests in this test module (such as the one below) may not
     pass until all the functions they depend on are implemented. For
     instance, the test below will fail for sets whose `set_of_list` function
     is not yet implemented (even if `is_empty` is correct).  This is fine:
     the goal here is just to record all the tests that we expect will pass
     when we get around to implementing everything later. *)

  let test () : bool =
    let s = set_of_list [1; 2; 3] in
    not (is_empty s)
  ;; run_test "is_empty: non-empty set returns false" test

  (* This is another case where the test won't pass since the "equals"
     function hasn't been implemented yet. We would like you to complete
     this test to confirm your understanding on "=" vs "equals". What should
     you use for this test to pass? *)
   
   let test() : bool = 
      (* Uncomment the two lines below *)
      let s1 = add 1 (add 2 empty) in 
      let s2 = set_of_list [2; 1] in 
      equals s1 s2
      (* substitute the above line with the appropiate comparison 
         between s1 and s2 *)
   ;; run_test "make two sets equal" test



(* Now, it's your turn! Make sure to comprehensively test all the other
   functions defined in the `SET` interface. It will probably be helpful to
   have the file `setInterface.ml` open as you work.  Your tests should stress
   the abstract properties of what it means to be a set, as well as the
   relationships among the operations provided by the SET interface.

   One thing to be careful of: your tests should not use `=` to compare sets:
   use the `equals` function instead.

   We strongly advise you to write tests for the functions in the order they
   appear in the interface. Write tests for all of the functions here before
   you start implementing. After the tests are written, you should be able to
   implement the functions one at a time in the same order and see your tests
   incrementally pass.

   Your TAs will be manually grading the completeness of your test cases. *)

  (* ---------- Write your own test cases below. ---------- *)

   (* LIST_OF_SET *)
   
   let test () : bool =
      let s1 = set_of_list [2; 2; 1; 0; 5; 4; 7; 6; 8; 2; 3; 1] in
      list_of_set s1 = [0; 1; 2; 3; 4; 5; 6; 7; 8]
   ;; run_test "list_of_set: long set with repeats" test
   
   let test () : bool =
      let s1 = set_of_list [] in
      list_of_set s1 = []
   ;; run_test "list_of_set: empty set" test
   
   let test () : bool = 
      let s1 = (add "great" empty) in
      list_of_set s1 = ["great"]
    ;; run_test "list_of_set: length 1 set" test

   (* ADD *)

   let test () : bool =
      let s1 = set_of_list [2; 2; 1; 0; 5; 4; 7; 6; 8; 2; 3; 1; 9] in
      let s2 = add 9 (set_of_list [2; 2; 1; 0; 5; 4; 7; 6; 8; 2; 3; 1]) in
      equals s1 s2
   ;; run_test "add: long set" test

   let test () : bool = 
     let s1 = add "great" (add "great" empty) in
     let s2 = add "great" empty in
     equals s1 s2
   ;; run_test "add: duplicates" test

  let test () : bool =
      let s1 = set_of_list [2; 2; 1; 0; 5; 4; 7; 6; 8; 2; 3; 1; 9] in
      list_of_set s1 = [0; 1; 2; 3; 4; 5; 6; 7; 8; 9]
   ;; run_test "add: in order" test
    

   (* REMOVE *)

   let test () : bool = 
      let s1 = add 3 empty in
      equals (remove 3 s1) empty
   ;; run_test "remove: single removal" test

   let test () : bool = 
      let s1 = add 3 empty in
      equals (remove 2 s1) s1
   ;; run_test "remove: no removal" test

   let test () : bool = 
      let s1 = add 3 (add 3 empty) in
      equals (remove 3 s1) empty
   ;; run_test "remove: 'double' removal" test

   let test () : bool = 
      let s1 = set_of_list [2; 2; 1; 0; 5; 4; 7; 6; 8; 2; 3; 1; 9] in
      let s2 = remove 2 s1 in
      not (member 2 s2) 
   ;; run_test "remove: testing member with removal" test

   (* MEMBER *)

   let test () : bool = 
      let s1 = add 3 (add 3 empty) in
      not (member 2 s1)
   ;; run_test "member: is not a member" test

   let test () : bool = 
      let s1 = add 2 (add 3 empty) in
      member 3 s1
   ;; run_test "member: is a member" test

   let test () : bool = 
      let s1 = empty in
      not (member 0 s1)
   ;; run_test "member: empty set" test


   (* SIZE *)

   let test () : bool = 
      let s1 = add 2 (add 3 empty) in
      size s1 = 2
   ;; run_test "size: size two" test

   let test () : bool = 
      let s1 = add 3 (add 3 empty) in
      size s1 = 1
   ;; run_test "size: only one distinct element" test
   
   let test () : bool = 
      let s1 = set_of_list [2; 2; 1; 0; 5; 4; 7; 6; 8; 2; 3; 1; 9] in
      size s1 = 10
   ;; run_test "size: size ten" test

   let test () : bool = 
      let s1 = empty in
      size s1 = 0
   ;; run_test "size: empty set" test


   (* EQUALS *)

  let test () : bool =
    let s1 = add 1 (add 2 (add 3 empty)) in
    let s2 = add 3 (add 2 (add 1 empty)) in
    equals s1 s2
  ;; run_test "equals: equal sets entered in different orders" test

  let test () : bool =
    let s1 = empty in
    let s2 = empty in
    equals s1 s2
  ;; run_test "equals: empty sets" test

  let test () : bool =
    let s1 = add 3 empty in
    let s2 = add 1 empty in
    not(equals s1 s2)
  ;; run_test "equals: not equals size 1 sets" test

  let test () : bool =
    let s1 = add 1 (add 2 (add 4 empty)) in
    let s2 = add 2 (add 2 (add 1 empty)) in
    not (equals s1 s2)
  ;; run_test "equals: not equals size 3 sets" test


   (* SET_OF_LIST *)

   let test () : bool = 
      let s1 = set_of_list [1;1;2;2;3;3;4;4;5;5] in
      let s2 = add 1 (add 2 (add 3 (add 4 (add 5 empty)))) in
      equals s1 s2
   ;; run_test "set_of_list: non-empty list with repeats" test

   let test () : bool = 
      let s1 = set_of_list [1;2;3;4;5] in
      let s2 = add 1 (add 2 (add 3 (add 4 (add 5 empty)))) in
      equals s1 s2
   ;; run_test "set_of_list: non-empty list no repeats" test

   let test () : bool = 
      let s1 = set_of_list [] in
      equals s1 empty
   ;; run_test "set_of_list: empty list" test

  (* ---------- Write your own test cases above. ---------- *)

end

(* The rest of the file instantiates the above tests so they are
   executed for both OrderedListSet and BSTSet.  Don't modify anything
   below this comment. *)

module TestOrderedListSet = SetTest(ListSet.OrderedListSet)
;; print_newline ()

module TestBSTSet = SetTest(TreeSet.BSTSet)
;; print_newline ()
