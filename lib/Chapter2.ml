type 'a lazylist = Cons of ('a * (unit -> 'a lazylist))

let rec lseq n = Cons (n, fun () -> lseq (n + 1))

let lhd (Cons (n, _)) = n
let ltl (Cons (_, tf)) = tf ()

let rec ltake n l = match n with 
  | 0 -> []
  | _ -> (lhd l) :: ltake (n - 1) (ltl l)

let rec ldrop n l = match n with 
  | 0 -> l 
  | _ -> ldrop (n - 1) (ltl l)

let rec lmap f (Cons (h, tf)) = Cons (f h, fun () -> lmap f (tf()))
let rec lfilter f (Cons (h, tf)) = if f h then Cons (h, fun () -> lfilter f (tf ())) else lfilter f (tf ())