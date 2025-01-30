;; open Assert
;; open HigherOrder

(******************************************************************************)
(* PROBLEM 6: ORDERED LIST SET                                                *)
(******************************************************************************)

(* Our first concrete implementation of the `SET` interface uses a simple list
   representation. Because a set is an "abstract type", it does not have a
   concrete implementation built into OCaml. Instead, it is defined in terms
   of its behavior, which is determined by the methods listed in the
   interface (the .mli files). Whether we use lists or trees, any
   implementation that conforms to the interface and maintains the invariants,
   or desired properties, is a set.

   The OCaml module system allows us to hide some information about how our
   set is represented under the hood and gives us the opportunity to control
   how values of type `'a set` can be created. This allows us to
   maintain "representation invariants" (as discussed in the lecture) so our
   lists behave like sets. *)

module OrderedListSet : SetInterface.SET = struct

  (* We begin by stating that the abstract type 'a set is actually represented
     here as an `'a list`. The list that is stored will be sorted in ascending 
     order with no duplicate elements.

     We will maintain the additional invariants that this list is sorted in
     ascending order, and contains no duplicates. *)

  type 'a set = 'a list

  let empty : 'a set = []

  let is_empty (s: 'a set) : bool =
    s = empty

  (* This function should return a list which is sorted in ascending order and
     contains no duplicate elements, which is your internal representation of 
     a set. We will be automatically testing that you are leveraging the 
     invariants to write this as efficiently as possible. *)

  let list_of_set (s: 'a set) : 'a list =
    s

  (* The next function will be a bit more complex than just cons-ing the
     element onto the list. Remember, we're maintaining the invariants that
     this list is sorted and contains no duplicate elements. You may NOT use a
     sorting function (such as `List.sort`) to accomplish this. *)

  let rec add (x: 'a) (s: 'a set) : 'a set =
    begin match s with 
    | [] -> [x]
    | h::tl -> if x < h then x :: s else if x = h then s else h::(add x tl)
    end

  (* The `remove` function returns a set without `x`. Try to use the invariants
     in order to "short-circuit" (terminate earlier than the end of the list) if
     the element is not found in its expected position. We will be automatically
     testing that this function short-circuits. *)

  let rec remove (x: 'a) (s: 'a set) : 'a set =
    begin match s with
    | [] -> []
    | h::tl -> 
      if x = h then tl 
      else if x > h then h::(remove x tl)
      else s
    end

  (* You already implemented a generic version of the next function in problem
     1, but now you should write it using `fold`. Do not add the `rec`
     keyword. *)

  let member (x: 'a) (s: 'a set) : bool =
    fold (fun h acc -> h = x || acc) false s

  (* Because one of our representation invariants requires that our list
     contain no duplicate elements, it is easy to calculate the size of the
     set.  Implement this function using `fold`. Do not add the `rec`
     keyword. *)

  let size (s: 'a set) : int =
    fold (fun h acc -> 1 + acc) 0 s

  (* We will be automatically testing that your implementation of `equals`
     makes use of the invariants to ensure efficiency. *)
 
  let rec equals (s: 'a set) (t: 'a set) : bool =
    begin match s,t with
    | [],[] -> true
    | x::xs, y::ys -> x=y && equals xs ys
    | _ -> false
    end

  (* Remember that, because it accepts an arbitrary input of type `'a list`,
     the following function will have to ensure that the resulting list
     follows the invariants. Make sure to write some tests to ensure this is
     the case! *)

  let rec set_of_list (l: 'a list) : 'a set =
    begin match l with
    | [] -> []
    | x::xs -> add x (set_of_list xs)
    end

  (* Don't modify the next line: It's just here for testing purposes. *)
  let debug_name: string = "OrderedListSet"
end
