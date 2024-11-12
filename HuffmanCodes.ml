exception NotImplemented

type 'a tree =
  | Leaf of 'a
  | Node of 'a tree * 'a tree 
              
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