type 'a vector = 'a array ref * int ref

exception Not_found

let length ((t,s) : 'a vector) = !s

let make n (x : 'a) : 'a vector = (ref (Array.make n x), ref n)

let of_list (l : 'a list) : 'a vector = (ref (Array.of_list l), ref (List.length l))

let get ((t,s) : 'a vector) i =
  Array.get !t i
  
let set ((t,s) : 'a vector) i x =
  Array.set !t i x

let add ((t,s) : 'a vector) x =
  if !s < Array.length !t
    then (!t.(!s) <- x; incr s)
    else begin
      let t2 = Array.make (if !s = 0 then 1 else 2 * !s) x in
      for i=0 to (Array.length !t) - 1 do
        t2.(i) <- !t.(i)
      done;
      incr s;
      t := t2;
    end

let fold_left f x (t,s) =
  let res = ref x in
  for i=0 to !s-1 do
    res := f !res !t.(i)
  done;
  !res

let iter f ((t,s) : 'a vector) =
  for i=0 to !s-1 do
    f !t.(i)
  done
  
let find f ((t,s) : 'a vector) =
  let res = ref None in
  for i=0 to !s-1 do
    if !res = None && f !t.(i)
      then res := Some i
  done;
  match !res with
  | None -> raise Not_found;
  | Some r -> r

