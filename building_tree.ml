type 'a tree =
  | Leaf of 'a * int
  | Node of int * 'a tree * 'a tree 
              
type 'a priorityQueue = { 
  enqueue: ('a tree) -> unit;
  dequeue: unit -> ('a tree) option;
  size: unit -> int
} 

let makePriorityQueue = 
  let queue = ref [] in
  let size = ref 0 in
  
  let enqueue (toAdd: 'a tree) = 
    (*We can use the fact that queue is mutable to use it to store prev if we wanted to*)
    let rec enqueue_helper (toAdd: 'a tree) (prev: 'a tree list) (next: 'a tree list) = 
      match toAdd, next with
      | _ , [] -> queue:= (!queue)@[toAdd]; size := !size + 1
      | Leaf (c1, occ1), (Leaf (c2, occ2) as hd)::tl ->
          if occ1 < occ2 then
            (queue := prev @ (toAdd :: next); size := !size + 1)
          else
            enqueue_helper toAdd (prev@[hd]) tl
      | Leaf (c1, occ1), (Node (occ2, _, _) as hd):: tl -> 
        if occ1 < occ2 then
          (queue := prev @ (toAdd :: next); size := !size + 1)
        else
          enqueue_helper toAdd (prev@[hd]) tl
      | Node (occ1, _, _), (Node (occ2, _, _) as hd):: tl ->
        if occ1 < occ2 then
          (queue := prev @ (toAdd :: next); size := !size + 1)
        else
          enqueue_helper toAdd (prev@[hd]) tl
      | Node (occ1, _, _), (Leaf (c2, occ2) as hd)::tl ->
        if occ1 < occ2 then
          (queue := prev @ (toAdd :: next); size := !size + 1)
        else
          enqueue_helper toAdd (prev@[hd]) tl

    in
    enqueue_helper toAdd [] !queue
      
  in
    
  let dequeue () : 'a tree option =
    match !queue with 
    | [] -> None
    | hd::tl -> 
        queue:=tl; size := (!size -1);
        Some hd 
              
  in 

  let size () = !size in
  
  {
    enqueue;
    dequeue;
    size
  }

let build_tree (l: ('a * int) list) : ('a tree) =
  let pq = makePriorityQueue
  in
  let leaf_list = List.map (fun (char, occ) -> Leaf (char, occ)) l
  in
  let rec insert_all_in_pq (rem: 'a tree list) : unit =
    match rem with
    | [] -> ()
    | hd::tl -> pq.enqueue hd; insert_all_in_pq tl
  in
  insert_all_in_pq leaf_list;

  let extract_int tree =
    match tree with
    | Leaf (_, i) -> i
    | Node (i, _, _) -> i
  in

  let rec builder () : ('a tree) = 
    match pq.dequeue () with
    | None -> failwith "Priority queue is empty"
    | Some tree1 ->
      if pq.size () = 0 then tree1
      else
        match pq.dequeue () with
        | None -> tree1
        | Some tree2 ->
            let int1 = extract_int tree1 in
            let int2 = extract_int tree2 in
            let node = Node (int1 + int2, tree2, tree1) in
            pq.enqueue node;
            builder ()
in

builder ()