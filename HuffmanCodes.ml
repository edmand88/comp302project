exception NotImplemented

type 'a tree =
  | Leaf of int * 'a
  | Node of int * 'a tree * 'a tree 
              
type 'a priorityQueue = { 
  enqueue: 'a -> unit;
  dequeue: unit -> 'a option;
} 

let makePriorityQueue = 
  let queue = ref [] in
  
  let enqueue (toAdd: 'a) = 
    (*We can use the fact that queue is mutable to use it to store prev if we wanted to*)
    let rec enqueue_helper (toAdd: 'a) (prev: 'a list) (next: 'a list) = 
      match next with
      | [] -> queue:= (!queue)@[toAdd]
      | hd::tl ->
          if toAdd < hd then
            queue:= (prev@[toAdd]@next)
          else
            enqueue_helper toAdd (prev@[hd]) tl
    in
    enqueue_helper toAdd [] !queue
      
  in
    
  let dequeue () : 'a option =
    match !queue with 
    | [] -> None
    | hd::tl -> 
        queue:=tl;
        Some hd; 
              
  in 
  
  { 
    enqueue = enqueue;
    dequeue = dequeue;
  }
  
  
let count_sort (toCount: 'a list) : ('a * int) list =
  [] 
     
(*TAIL RECURSIVE*)
let list_to_int bits = 
  let rec build_int (lst: bool list) (acc: int): int =
    match lst with
    | [] -> acc
    | hd::tl ->
        match hd with
        | true -> build_int tl (acc*2 + 1)
        | false -> build_int tl (acc*2)
  in
  build_int bits 0


(*NOT TAIL RECURSIVE -> USE CONTINUATIONS*)
let get_huffman_codes (full_tree: 'a tree) : ('a, int * int) Hashtbl.t=
  let hash = Hashtbl.create 256 in (*Not sure about the quanity*) 
    
  let rec build_codes (bits: bool list) (node: 'a tree)  =
    match node with
    | Leaf(_, c) -> Hashtbl.add hash c ((list_to_int bits), List.length bits)
    | Node(_, left, right) -> 
        build_codes (false::bits) left; 
        build_codes (true::bits) right
  in
  build_codes [] full_tree;
  hash;; 
          
    
let huffmanEncode (toEncode: 'a list) : bool list = 
  (*Might wanna make this variable for the type of data structure this is encoding, if we wanna encode for something else than bytes/chars*)
  
  (*We now have a completed tree*)
  raise NotImplemented
;; 