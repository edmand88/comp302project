exception Impossible
exception NoMoreElements
exception Error
exception NotImplemented

(* Implement priority queue with binary min heap*)
type 'a tree = Empty | Node of 'a * 'a tree * 'a tree

(*any non-Leaf node do not need key, only need to keep track of frequency*)
type 'a huff_tree = Leaf of 'a * int | HuffNode of 'a huff_tree * 'a huff_tree * int

(* keep track of the path that leads to the insertion of the new node in a complete binary tree*)
type direction = L | R


(* get size of a binary tree using continuations*)
let rec get_size_cont (t: 'a tree) (sc: int -> 'b): 'b =
  raise NotImplemented

let get_size t = raise NotImplemented

(* find the path to the last node in a complete binary tree *)
let rec find_path_to_last_cont (size: int) (sc: direction list -> 'b): 'b =
  raise NotImplemented

let find_path_to_last size = raise NotImplemented

(* compare if huff_tree 1 has greater weight than huff_tree 2 *)
let compare_helper (x: 'a huff_tree) (y: 'a huff_tree) = 
  raise NotImplemented

(* get the weight of a node in huffman tree*)
let get_huffman_node_weight (t: 'a huff_tree) = 
  raise NotImplemented

(* insert a new node into a min heap using the above helper functions*)
let rec insert (x: 'a huff_tree) (heap: 'a huff_tree tree): 'a huff_tree tree = 
  raise NotImplemented

  (* get and remove last node in the heap *)
let rec get_last_node (heap: 'a huff_tree tree) (path: direction list): 'a huff_tree * 'a huff_tree tree = 
  raise NotImplemented
  

(* restore the min heap property by swapping the root node and the smallest children if root node larger than any of its children*)
let rec heapify (heap: 'a huff_tree tree): 'a huff_tree tree =
  raise NotImplemented

(* remove the minimum element in the heap and restore the min heap property *)
let rec remove_min (heap: 'a huff_tree tree): 'a huff_tree * 'a huff_tree tree = 
  raise NotImplemented

(* build huffman tree from a min heap *)
let rec build_tree (heap: 'a huff_tree tree): 'a huff_tree option = 
  raise NotImplemented

(* build binary prefiex for each character in the leaf node, left branch -> 0 and right -> 1 *)
let prefix_tree (t: 'a huff_tree) (prefix: int list) (table: ('a, bool list) Hashtbl.t): unit =
  raise NotImplemented

(* generate huffman code for each character in the input list and store in Hashtable*)
let generate_huffman_code (occ_list: ('a * int) list): ('a, bool list) Hashtbl.t =
  raise NotImplemented

(* below functions are here to help encoding/decoding a string *)

(* Function to print entries of a (char, bool list) Hashtbl.t *) 
let print_huffman_table (tbl: (char, bool list) Hashtbl.t) : unit = 
  let bool_list_to_bit_string (bools: bool list) : string =
    String.concat "" (List.map (fun b -> if b then "1" else "0") bools)
  in
  Hashtbl.iter (fun key value ->
      let bit_string = bool_list_to_bit_string value in
      Printf.printf "%c -> %s\n" key bit_string
    ) tbl
    
    (*Function to convert char list into the encoded string*)
let convert_tokens (input: 'a list) (hash: ('a, bool list) Hashtbl.t) : (bytes * int) =
  raise NotImplemented
;;

(*Returns the token list and the amount of bits of the encoded message to deal with padding*)
let decode_bytes (input: bytes) (tree: 'a huff_tree) (num_bits: int): 'a list = 
  raise NotImplemented
;; 

(*Encode a list of 'a type*)
let encode_generic (input: 'a list) : (string * 'a huff_tree * int) = 
  let count_elements (elements: 'a list) : ('a * int) list = 
    raise NotImplemented
  in
  
  let (table, tree_opt) = generate_huffman_code (count_elements input) in
  match tree_opt with
  | None -> raise Error
  | Some tree ->
      let (encoded_string, num_bits) = convert_tokens input table in
      (Bytes.to_string encoded_string, tree, num_bits)
;;

(*Preprocessor that removes the necessary bytes conversion*)
let decode_generic (input: string) (t: 'a huff_tree) (num_bits: int): 'a list =
  decode_bytes (Bytes.of_string input) t num_bits
;;
