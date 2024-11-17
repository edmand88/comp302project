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

(*NOT TAIL RECURSIVE -> USE CONTINUATIONS*)
let get_huffman_codes (full_tree: 'a tree) : ('a, bool list) Hashtbl.t =
  let hash = Hashtbl.create 128 in (*Not sure about the quantity, but assume ASCII*) 
    
  let rec build_codes (bits: bool list) (node: 'a tree)  =
    match node with
    | Leaf(_, c) -> Hashtbl.add hash c bits
    | Node(_, left, right) -> 
        build_codes (false::bits) left; 
        build_codes (true::bits) right
  in
  build_codes [] full_tree;
  hash
;; 

let convert_bytes (input: bytes) (hash: (char, bool list) Hashtbl.t) : bytes =
  (* Calculate total bits needed for the compressed data *)
  let total_bits = 
    Bytes.fold_left (fun acc c ->
        let code = Hashtbl.find hash c in
        acc + List.length code
      ) 0 input
  in
  
  (* Creates the necessary output size *) 
  let length = ((total_bits + 7) / 8) in
  let output = Bytes.create length in
  
  (* Set a specific bit in a byte ; maybe inefficient because it must be called 
     a lot but it's not recursive and is easier to use than having to move bytes*)
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
  
  (* Convert each input byte to its Huffman code  and write to output *)
  let bit_pos = ref 0 in
  Bytes.iter (fun c ->
      let code = Hashtbl.find hash c in
      List.iter (fun bit ->
          let byte_idx = !bit_pos / 8 in
          let bit_idx = !bit_pos mod 8 in
          set_bit byte_idx bit_idx bit;
          incr bit_pos
        ) code
    ) input;
  
  output
;;

(*Reads entire file - VERY MEMORY INTENSIVE FOR LARGER FILES, DOES NOT USE BUFFERS*)
let read_file (filename: string) : (bytes) =
  let channel = open_in_bin filename in
  let length = in_channel_length channel in
  let bytes = Bytes.create length in
  really_input channel bytes 0 length;
  close_in channel;
  bytes
;;
    
let huffmanEncode (filename: string) : bytes = 
  (*We now have a completed tree, use Marshal.to_channel to push into the encoded file*)
  raise NotImplemented
;; 

let huffmanDecode (filename: string) : bytes =
  raise NotImplemented
;;
