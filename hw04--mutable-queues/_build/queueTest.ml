(*****************************************************************************)
(* PROBLEM 4: WRITING TEST CASES                                             *)
(*****************************************************************************)

(* `QueueTest` is a reusable module that we'll use to test other
   modules that conform to the `QUEUE` interface. When `QueueTest` is
   instantiated with a particular queue implementation, it will run all
   of its test cases against the queue type defined in that
   implementation.  This means that the _same_ tests can be used for
   both the MutableQueue and ListQueue implementations (and that both
   implementations should behave the same for these tests!).

   Read through the module, then write your test cases in the space
   provided below.  Your TAs will be grading the completeness of your
   tests. *)

module QueueTest (QImpl: QueueInterface.QUEUE) = struct

;; open QImpl

(* We first redefine local versions of the `run_test` and
   `run_failing_test` functions so that they prepend the name of the
   implementation we're testing to the test description. *)

let run_test desc = Assert.run_test (debug_name ^ ": " ^ desc)
let run_failing_test desc = Assert.run_failing_test (debug_name ^ ": " ^ desc)

;; print_endline ("\n--- Running tests for " ^ debug_name ^ " ---")

(* We'll write all our test cases for the `QUEUE` abstract type below.
   Here are a few to help get you started. *)

let test () : bool =
  is_empty (create ())
;; run_test "is_empty: call on empty returns true" test

(* Note that some tests in this test module (such as the one below)
   may not pass until all the functions they depend on are
   implemented. For instance, the test below will fail for queues
   whose `from_list` function is not yet implemented (even if
   `is_empty` is correct).  But this is fine: you can write the tests
   now (and make sure that they typecheck and compile), then fill in
   the functions and use the tests you've written to help debug them. *)

let test () : bool =
  let q = from_list [1; 2; 3] in
  not (is_empty q)
;; run_test "is_empty: non-empty set returns false" test

(* Now, it's your turn! Make sure to comprehensively test all the
   other functions defined in the `QUEUE` interface. It will probably
   be helpful to have the file queueInterface.ml open as you
   work. Your tests should stress the abstract properties of what it
   means to be a queue, as well as the relationships among the
   operations provided by the QUEUE interface.

   You do not need to test the `print` function.

   We provide many test cases for you, so your main job here is to
   finish writing tests for `truncate` and `delete`.

   Of course, you can come back and add more tests later, if
   implementing the functions makes you realize you forgot some cases.
   But you'll find the assignment easiest if you try to write a good
   set of tests first, before implementing any of the functions.

   Your TAs will be manually grading the completeness of your test
   cases. *)

(* ---------- Add your own test cases where indicated below. ---------- *)

(* ENQ TESTS *)
let test () : bool =
  let q = create () in
  enq 1 q;
  to_list q = [1]
;; run_test "enq on empty" test

let test () : bool =
  let q = create () in
  enq 1 q;
  enq 2 q;
  enq 3 q;
  to_list q = [1; 2; 3]
;; run_test "enq on non-empty" test

let test () : bool = 
  let q = create () in
  enq "1" q;
  enq "2" q;
  enq "3" q;
  enq "4" q;
  to_list q = ["1";"2";"3";"4"]
;; run_test "enq on string set" test

(* DEQ TESTS *)
let test () : bool =
  let q = create () in
  deq q;
  is_empty q
;; run_failing_test "deq on empty" test

let test () : bool =
  let q = create () in
  enq 1 q;
  let hd = deq q in
  hd = 1 && is_empty q
 ;; run_test "deq on singleton" test

let test () : bool =
  let q = create () in
  enq 2 q;
  enq 3 q;
  let hd = deq q in
  hd = 2 && to_list q = [3]
;; run_test "deq on non-empty" test

(* TO_LIST TESTS *)
let test () : bool =
  let q = create () in
  to_list q = []
;; run_test "to_list empty" test

let test () : bool =
  let q = create () in
  enq 2 q;
  enq 3 q;
  to_list q = [2; 3]
;; run_test "to_list non-empty" test

(* LENGTH TESTS *)
let test () : bool =
  let q = create () in
  length q = 0
;; run_test "length empty" test

let test () : bool =
  let q = create () in
  enq 2 q;
  enq 3 q;
  length q = 2
;; run_test "length non-empty" test

(* FROM_LIST TESTS *)
let test () : bool =
  let q = from_list [] in
  valid q && to_list q = []
;; run_test "from_list empty" test

let test () : bool =
  let q = from_list [1] in
  valid q && to_list q = [1]
 ;; run_test "from_list singleton" test

let test () : bool =
  let q = from_list [1; 2; 3] in
  valid q && to_list q = [1; 2; 3]
;; run_test "from_list non-empty" test

(* CONTAINS TESTS *)
 let test () : bool =
  let q = create () in
  not (contains 1 q)
;; run_test "contains empty" test

let test () : bool =
  let q = from_list [2; 3] in
  contains 3 q
;; run_test "contains non-empty true" test

let test () : bool =
  let q = from_list [2; 3] in
  not (contains 4 q)
;; run_test "contains non-empty false" test

(* TRUNCATE TESTS *)

let test () : bool =
  let q = create () in
  truncate 2 q;
  to_list q = []
;; run_test "truncate: empty queue" test

let test () : bool =
  let q = create () in
  enq 2 q;
  truncate 2 q;
  to_list q = [2]
;; run_test "truncate: singleton queue" test

let test () : bool = 
  let q = from_list [1;2;3;4;5;6;7;8;9] in
  truncate 4 q;
  to_list q = [1;2;3;4]
;; run_test "truncate: non empty queue no repeats" test

let test () : bool = 
  let q = from_list [1;2;3;4;4;5;6;7;8;9] in
  truncate 4 q;
  to_list q = [1;2;3;4]
;; run_test "truncate: non empty queue yes repeats" test

let test () : bool = 
  let str1 = "3" in
  let q = from_list ["1";"2";str1;"4";"5";"6"] in
  truncate str1 q;
  to_list q = ["1";"2";"3"]
;; run_test "truncate: non empty queue no aliases" test

let test () : bool = 
  let str1 = "3" in
  let str2 = str1 in
  let q = from_list ["1";"2";str2;str1;"4";"5";"6"] in
  truncate str1 q;
  to_list q = ["1";"2";"3"]
;; run_test "truncate: non empty queue with aliases" test


let test () : bool = 
  let q = from_list [1;2;3;4;5;6;7;8;9] in
  truncate 0 q;
  to_list q = [1;2;3;4;5;6;7;8;9]
;;run_test "truncate: non empty queue with no value" test

(* DELETE TESTS *)

let test () : bool = 
  let q = from_list [1;2;3;4;5;6;7;8;9;10] in
  delete 4 q;
  to_list q = [1;2;3;5;6;7;8;9;10]
;;run_test "delete: non empty queue with value" test

let test () : bool = 
  let q = from_list [1;2;3;5;6;7;8;9;10] in
  delete 4 q;
  to_list q = [1;2;3;5;6;7;8;9;10]
;;run_test "delete: non empty queue without value" test

let test () : bool = 
  let q = from_list [] in
  delete 4 q;
  to_list q = []
;; run_test "delete: empty queue" test

let test () : bool = 
  let q = from_list [1;2] in
  delete 1 q;
  to_list q = [2]
;; run_failing_test "delete: delete to "
  




(* ---------- Write your own test cases above. ---------- *)

end

(* We now instantiate the tests so they are executed for both LinkedQueue
   and SimpleQueue.

   Don't modify anything below this comment. *)

module TestSimpleQueue = QueueTest(SimpleQueue.SimpleQueue)
;; print_newline ()

module TestLinkedQueue = QueueTest(LinkedQueue.LinkedQueue)
;; print_newline ()
