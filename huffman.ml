exception Impossible
exception NoMoreElements


(* Implement priority queue with binary min heap*)
type 'a tree = Empty | Node of 'a * 'a tree * 'a tree

(*any non-Leaf node do not need key, only need to keep track of frequency*)
type 'a huff_tree = Leaf of 'a * int | HuffNode of 'a huff_tree * 'a huff_tree * int

(* keep track of the path that leads to the insertion of the new node in a complete binary tree*)
type direction = L | R


(* get size of a binary tree using continuations*)
let rec get_size_cont (t: 'a tree) (sc: int -> 'b): 'b =
  match t with
  | Empty -> sc (0)
  | Node(_, l, r) -> get_size_cont l (fun el -> get_size_cont r (fun er -> sc (1 + el + er)))

let get_size t = get_size_cont t (fun x -> x)

(* find the path to the last node in a complete binary tree *)
let rec find_path_to_last_cont (size: int) (sc: direction list -> 'b): 'b =
  if size <= 1 then sc []
  else (if size mod 2 = 0 then find_path_to_last_cont (size/2) (fun r -> sc(r@[L]))
  else find_path_to_last_cont (size/2) (fun r -> sc(r@[R])))

let find_path_to_last size = find_path_to_last_cont size (fun x -> x)

(* compare if huff_tree 1 has greater weight than huff_tree 2 *)
let compare_helper (x: 'a huff_tree) (y: 'a huff_tree) = match x, y with
  | Leaf(_, w1), Leaf(_, w2)   
  | Leaf(_, w1), HuffNode(_, _, w2) 
  | HuffNode(_, _, w1), Leaf(_, w2) 
  | HuffNode(_, _, w1), HuffNode(_, _, w2) -> if w1 > w2 then true else false

(* get the weight of a node in huffman tree*)
let get_huffman_node_weight (t: 'a huff_tree) = match t with
  | Leaf(_, w) -> w
  | HuffNode(_, _, w) -> w

(* find the path to the last node in a complete binary tree *)
(* insert a new node into a min heap*)
let rec insert (x: 'a huff_tree) (heap: 'a huff_tree tree): 'a huff_tree tree = 
  let heap_size = get_size heap in
  let path = find_path_to_last (heap_size + 1) in
  let rec insert' (x: 'a huff_tree) (path: direction list) (heap: 'a huff_tree tree) = match heap, path with
  | Empty, _ -> Node (x, Empty, Empty)
  | Node (v, l, r), L::tl -> if (compare_helper x v) then Node (v, insert' x tl l, r) else Node (x, insert' v tl l, r)
  | Node (v, l, r), R::tl -> if (compare_helper x v) then Node (v, l, insert' x tl r) else Node (x, l, insert' v tl r)
  | Node(_, _, _), [] -> raise Impossible (* if there is at least one node, then path cannot be empty*)
  in insert' x path heap

  (* get and remove last node in the heap *)
let rec get_last_node (heap: 'a huff_tree tree) (path: direction list): 'a huff_tree * 'a huff_tree tree = match heap, path with
  | Node (v, Empty, Empty), [] -> (v, Empty)
  | Node (v, l, r), L::tl -> let (last_node, new_l) = get_last_node l tl in (last_node, Node (v, new_l, r))
  | Node (v, l, r), R::tl -> let (last_node, new_r) = get_last_node r tl in (last_node, Node (v, l, new_r))
  | _ -> raise Impossible

(* restore the min heap property *)
let rec heapify (heap: 'a huff_tree tree): 'a huff_tree tree = match heap with
  | Empty -> Empty
  | Node (v, Empty, Empty) -> Node (v, Empty, Empty)
  | Node (v, (Node(lv, ll, lr) as l), Empty) -> if compare_helper v lv then Node (lv, heapify (Node (v, ll, lr)), Empty) else Node (v, l, Empty)
  | Node (v, Empty, (Node(rv, rl, rr) as r)) -> if compare_helper v rv then Node (rv, Empty, heapify (Node (v, rl, rr))) else Node (v, Empty, r)
  | Node (v, (Node(lv, ll, lr) as l), (Node(rv, rl, rr) as r)) when compare_helper lv rv -> 
      if compare_helper v rv then Node (rv, l, heapify (Node (v, rl, rr))) else Node (v, l, r)
  | Node (v, (Node(lv, ll, lr) as l), (Node(rv, rl, rr) as r)) -> 
      if compare_helper v lv then Node (lv, heapify (Node (v, ll, lr)), r) else Node (v, l, r)

(* remove the minimum element in the heap and restore the min heap property *)
let rec remove_min (heap: 'a huff_tree tree): 'a huff_tree * 'a huff_tree tree = match heap with
  | Empty -> raise NoMoreElements
  | Node (v, Empty, Empty) -> (v, Empty)
  | Node (v, l, r) ->
      let heap_size = get_size heap in
      let path = find_path_to_last heap_size in
      let (last_node, new_heap) = get_last_node heap path in
      (v, heapify (match new_heap with
        | Empty -> Node (last_node, Empty, Empty)
        | Node (v, l, r) -> Node (last_node, l, r)))

(* build huffman tree from a min heap *)
let rec build_tree (heap: 'a huff_tree tree): 'a huff_tree option = 
  try
    let min1, heap1 = remove_min heap in 
    try
        let min2, heap2 = remove_min heap1 in
        (* merge the two trees with least frequencies by creating a parent node with the sum of their frequencies*)
        let new_node = HuffNode (min1, min2, (get_huffman_node_weight min1 + get_huffman_node_weight min2)) in
        build_tree (insert new_node heap2)
    with NoMoreElements -> Some min1      (* if min2 is Empty then return the final huffman tree*)
  with NoMoreElements -> None             (* if min1 is Empty then the min heap is empty *)

(* build binary prefiex for each character in the leaf node, left branch -> 0 and right -> 1*)
let prefix_tree (t: 'a huff_tree) (prefix: int list) (table: (string, int list) Hashtbl.t): unit =
  let rec prefix_tree_cont t prefix table (sc: unit -> unit): unit = 
    match t with
    | Leaf (char, _) -> sc (Hashtbl.add table char prefix)
    | HuffNode (l, r, _) -> 
        prefix_tree_cont l (prefix @ [0]) table 
        (fun () -> prefix_tree_cont r (prefix @ [1]) table sc)
  in prefix_tree_cont t [] table (fun () -> ())

let generate_huffman_code (occ_list: (string * int) list): (string, int list) Hashtbl.t =
  let table = Hashtbl.create 256 in
  let heap = List.fold_left (fun acc (char, freq) -> insert (Leaf (char, freq)) acc) Empty occ_list in
  match build_tree heap with
  | Some huff_tree -> prefix_tree huff_tree [] table; table
  | None -> table

  (* Function to print the contents of the Hashtbl *)
let print_huffman_table table =
  Hashtbl.iter
    (fun key value ->
      let value_str = String.concat ", " (List.map string_of_int value) in
      Printf.printf "%s: [%s]\n" key value_str)
    table

(* let h1 = Empty;;
let h2 = insert (Leaf('a', 1)) h1;;
let h3 = insert (Leaf('b', 2)) h2;;
let h4 = insert (Leaf('c', 10)) h3;;
let h5 = insert (Leaf('d', 5)) h4;;
let h6 = insert (Leaf('e', 7)) h5;;
let h7 = insert (Leaf('f', 8)) h6;;

let r1 = remove_min h7;; *)
