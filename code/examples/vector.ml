(*
   An implementation of SML vector interface (immutable arrays with update possibility).

   We use random access lists with O(1) lists operations and O(log n) (expected O(log i)) array operations.
   We try to keep the implementation simple and show some nice features (non-uniform polymorphic recursion).
*)

module Vector : sig
  type 'a vector 
  val fromList : 'a list -> 'a vector
  val length : 'a vector -> int
  val sub : 'a vector * int -> 'a
  val update : 'a vector * int * 'a -> 'a vector
  val map  : ('a -> 'b) -> 'a vector -> 'b vector
  val foldl  : ('a * 'b -> 'b) -> 'b -> 'a vector -> 'b
  (* other operations (not implemented but simple to derive from others) *)
  (* val tabulate : int * (int -> 'a) -> 'a vector *)
  (* val concat : 'a vector list -> 'a vector *)
  (* val appi : (int * 'a -> unit) -> 'a vector -> unit *)
  (* val app  : ('a -> unit) -> 'a vector -> unit *)
  (* val mapi : (int * 'a -> 'b) -> 'a vector -> 'b vector *)
  (* val foldli : (int * 'a * 'b -> 'b) -> 'b -> 'a vector -> 'b *)
  (* val foldri : (int * 'a * 'b -> 'b) -> 'b -> 'a vector -> 'b *)
  (* val foldr  : ('a * 'b -> 'b) -> 'b -> 'a vector -> 'b *)
  (* val findi : (int * 'a -> bool) -> 'a vector -> (int * 'a) option *)
  (* val find  : ('a -> bool) -> 'a vector -> 'a option *)
  (* val exists : ('a -> bool) -> 'a vector -> bool *)
  (* val all : ('a -> bool) -> 'a vector -> bool *)
  (* val collate : ('a * 'a -> order) -> 'a vector * 'a vector -> order *)

  (* 
    basic operations not included in the standard interface
    also include list operations
  *)
  val empty : 'a vector
  val isEmpty : 'a vector -> bool
  val cons : 'a * 'a vector -> 'a vector
  val head : 'a vector -> 'a
  val tail : 'a vector -> 'a vector
  val lookup : int * 'a vector -> 'a
  val update_map : int * ('a -> 'a) * 'a vector -> 'a vector
end =
struct
  (* basic operations *)
  type 'a vector = Nil | Zero of ('a * 'a) vector | One of 'a * ('a * 'a) vector
  let empty = Nil
  let isEmpty xs = xs = Nil
  let rec cons : type a . a * a vector -> a vector = function
    | x, Nil -> One (x,Nil)
    (* 0+x = x *)
    | x, Zero ps -> One (x,ps)
    (* 1 + 1 = 10 *)
    | x, One (y,ps) -> Zero (cons ((x,y),ps))
  let rec uncons : type a . a vector -> a * a vector = function
    | One (x,Nil) -> x,Nil
    | One (x,ps) -> (x,Zero ps)
    | Zero ps -> 
      let ((x,y), ps') = uncons ps in
      (x, One (y,ps'))
    | Nil -> failwith "domain issue"
  let head xs = fst (uncons xs)
  let tail xs = snd (uncons xs)
  let rec lookup : type a . int * a vector -> a = function
    | _, Nil -> failwith "out of bounds"
    | 0, One (x,ps) -> x
    | i, One (x,ps) -> lookup (i-1, Zero ps)
    | i, Zero ps -> 
      let (x,y) = lookup (i / 2, ps) in
      if i mod 2 = 0 then x else y

  let rec update_map : type a . int * (a -> a) * a vector -> a vector = function
    | _, _, Nil -> failwith "out of bounds"
    | 0, f, One (x,ps) -> One (f x,ps)
    | i, f, One (x,ps) -> cons (x, update_map (i-1, f, Zero ps))
    | i, f, Zero ps -> 
      let f' (x,y) = if i mod 2 = 0 then (f x, y) else (x, f y) in
      Zero (update_map (i/2, f', ps))


  (*  derived function ontop for the vector interface *)
  let fromList xs = List.fold_left (fun acc x -> cons (x,acc)) empty xs
  let sub (xs,i) = lookup (i,xs)
  let update (xs,i,x) = update_map (i, (fun _ -> x), xs)

  (* simple map with same complexity but larger constant *)
  (* let rec map f xs = 
    if isEmpty xs then empty
    else cons (f (head xs), map f (tail xs)) *)
  let rec map : type a b. (a -> b) -> a vector -> b vector = fun f -> 
    let f_pair (x,y) = (f x, f y) in
    function
    | Nil -> Nil
    | One (x,ps) -> One (f x, map f_pair ps)
    | Zero ps -> Zero (map f_pair ps)

  let rec foldl : type a b. (a * b -> b) -> b -> a vector -> b = fun f acc -> 
    let f_pair ((x,y),acc) = f (y,(f (x,acc))) in
    function
    | Nil -> acc
    | One (x,ps) -> foldl f_pair (f (x,acc)) ps
    | Zero ps -> foldl f_pair acc ps

  let rec length : type a . a vector -> int = function
    | Nil -> 0
    | One (x,ps) -> 1 + 2 * length ps
    | Zero ps -> 2 * length ps


end
