type 'a tree =
  | Leaf of 'a * int
  | Node of int * 'a tree * 'a tree 


let rev' l =
  let rec rev_tr l acc = match l with
    | [] -> acc
    | h::t -> rev_tr t (h::acc)
  
  in
    rev_tr l []

(*NOT TAIL RECURSIVE -> USE CONTINUATIONS*)
let get_huffman_codes (full_tree: 'a tree) : ('a, bool list) Hashtbl.t =
  let hash = Hashtbl.create 128  (*Not sure about the quantity, but assume ASCII*) 
  in  
  let rec build_codes (bits: bool list) (node: 'a tree)  =
    match node with
    | Leaf(c, _) -> Hashtbl.add hash c (rev' bits)
    | Node(_, left, right) -> 
        build_codes (false::bits) left; 
        build_codes (true::bits) right
  in
  build_codes [] full_tree;
  hash
;;