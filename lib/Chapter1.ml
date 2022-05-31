let rec fold_left f a = function
  | [] -> a
  | h :: t -> fold_left f (f a h) t

let rec fold_right f a = function 
  | [] -> a
  | h :: t -> f (fold_right f a t) h

let mem a l = fold_left (fun x y -> x || y = a) false l

let rev l = fold_left (fun x y -> y :: x) [] l

let setify l = fold_left (fun x y -> if mem y x then x else y :: x) [] l