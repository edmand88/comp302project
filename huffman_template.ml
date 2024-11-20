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
let convert_tokens (input: char list) (hash: ('a, bool list) Hashtbl.t) : (bytes * int) =
  (* Calculate total bits needed for the compressed data *)
  let total_bits = 
    List.fold_left (fun acc c ->
        let code = Hashtbl.find hash c in
        acc + List.length code
      ) 0 input
  in
  
  (* Creates the necessary output size *) 
  let length = ((total_bits + 7) / 8) in
  let output = Bytes.create length in
  
  (* Set a specific bit in a byte *)
  let set_bit byte_idx bit_idx value =
    let curr_byte = Bytes.get output byte_idx in
    let new_byte = 
      if value then
        Char.chr (Char.code curr_byte lor (1 lsl (7 - bit_idx)))
      else
        curr_byte
    in
    Bytes.set output byte_idx new_byte
  in
  
  (* Initialize output bytes to 0 *)
  Bytes.fill output 0 length (Char.chr 0);
  
  (* Convert each input char to its Huffman code and write to output *)
  let bit_pos = ref 0 in
  List.iter (fun c ->
      let code = Hashtbl.find hash c in
      List.iter (fun bit ->
          let byte_idx = !bit_pos / 8 in
          let bit_idx = !bit_pos mod 8 in
          set_bit byte_idx bit_idx bit;
          incr bit_pos
        ) code
    ) input;
  
  (output, total_bits)
;;

(*Returns the token list and the amount of bits of the encoded message to deal with padding*)
let decode_bytes (input: bytes) (tree: 'a huff_tree) (num_bits: int): 'a list =
  let len = Bytes.length input in
  let total_bits = num_bits in
  
  (* Helper to get a specific bit from the input bytes *)
  let get_bit pos =
    let byte_idx = pos / 8 in
    let bit_idx = pos mod 8 in
    if byte_idx >= len then false
    else
      let byte = Bytes.get input byte_idx in
      (Char.code byte land (1 lsl (7 - bit_idx))) <> 0
  in 
  
  (* Walk the tree following bits until we hit a leaf, then start over *)
  let rec decode_next bit_pos curr_node acc =
    match curr_node with
    | Leaf (c, _) -> 
      (* Leaf node - add token and if we have more bits, continue from root *)
        if bit_pos >= total_bits then
        (* End of input - convert accumulated chars to string, including current char *)
          acc @ [c]
        else
          decode_next bit_pos tree (acc @ [c])
    | HuffNode (left, right, _) -> 
        (match get_bit bit_pos with
         | true -> decode_next (bit_pos + 1) right acc
         | false -> decode_next (bit_pos + 1) left acc)

  in
  
  decode_next 0 tree []
;;

let encode_string (input: string) : (string * char huff_tree * int) = 
  let explode (s: string) : char list =
    let rec helper i acc =
      if i < 0 then acc
      else helper (i - 1) (s.[i] :: acc)
    in
    helper (String.length s - 1) []
  in
    
  let count_chars (str: string) : (char * int) list = 
    let counts = Hashtbl.create 256 in 
    let increment_char c =
      match Hashtbl.find_opt counts c with
      | None -> Hashtbl.add counts c 1
      | Some num -> Hashtbl.replace counts c (num + 1)
    in
  
    String.iter increment_char str; 
    Hashtbl.fold (fun k v acc -> (k, v) :: acc) counts []
  in
  
  let occ_list = count_chars input in
  let table = Hashtbl.create 256 in
  let heap = List.fold_left (fun acc (char, freq) -> insert (Leaf (char, freq)) acc) Empty occ_list in
  let tree = build_tree heap in
  match tree with
  | None -> raise Error
  | Some tree2 ->
      prefix_tree tree2 [] table;
      print_huffman_table table;
      let (encoded_string, num_bits) = convert_tokens (explode input) table in
      (Bytes.to_string encoded_string, tree2, num_bits)
;;

let decode_string (input: string) (t: char huff_tree) (num_bits: int): string =
  let char_list = decode_bytes (Bytes.of_string input) t num_bits in
  String.of_seq (List.to_seq char_list)