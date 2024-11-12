let store : (char, int) Hashtbl.t = Hashtbl.create 1000

(*Sorts a list of ('a*list)*)
let naiveSort (toCount: ('a * int) list) : ('a*int) list =
  let decreasing (c1, occ1) (c2, occ2) = compare occ1 occ2
  in
  List.sort decreasing toCount

(*transform a list of 'a to a list with the occurence*)
let count_list (toCount: 'a list) : ('a * int) list =
  let rec helper (l: 'a list) : unit =
    match l with
    | [] -> ()
    | h::t -> 
      if Hashtbl.mem store h then
        Hashtbl.replace store h ((Hashtbl.find store h) + 1)
      else
        Hashtbl.add store h 1;
      
      helper t
  in
  helper toCount;
    
  Hashtbl.fold (fun key value acc -> (key, value) :: acc) store []

(*explode the string into a list of char*)
let explode (s: string) : char list =
  let rec helper i acc =
    if i < 0 then acc
    else helper (i - 1) (s.[i] :: acc)
  in
  helper (String.length s - 1) []

(*Main function. This function returns the list of occurence of each character given the text*)
let occurence (s: string) : ('a*int) list =
  let char_list = explode s;
  in
  let char_occ = count_list char_list;
  in
  let sorted_char_occ = naiveSort char_occ; 
  in
  let _ = Hashtbl.clear store;
  in
  sorted_char_occ