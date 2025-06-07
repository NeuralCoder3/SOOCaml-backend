(*

  See https://github.com/NeuralCoder3/ocaml_bignum
  for more information.


  To use this file (space in #use to avoid SOOCaml's recursive includes)

  # use "bignum.ml";;
  open (BigInt (BigNum))
  open BigIntNotation (BigInt (BigNum))
*)

type cmp = Lt | Eq | Gt

(*
   General interface for arbitrary precision numbers.
   Shared between BigNum and BigInt.
*)
module type BigNum = sig
  type t
  val bignum_of_int : int -> t
  val int_of_bignum : t -> int
  val string_of_bignum : ?base:int -> t -> string
  val bignum_of_string : string -> t
  val add : t -> t -> t
  val sub : t -> t -> t
  val mul : t -> t -> t
  val cmp : t -> t -> cmp
  val is_zero : t -> bool
  val modulo : t -> t -> t
  val div : t -> t -> t
  val divmod : t -> t -> t * t
  val shr : t -> t -> t
  val shl : t -> t -> t
  val abs : t -> t
  val pow : t -> t -> t
end

(* Arbitrary precision natural numbers.  *)
module BigNum : BigNum = struct
  (* we want all operations in O(log^k n) *)

  (* reversed list of bits representing a bignum *)
  (* would be faster with base 2^31 *)
  type t = bool list

  let bit2int b = if b then 1 else 0

  let rec bignum_of_int (n:int) =
    if n < 0 then failwith "bignum_of_int: negative number" else
    if n = 0 then []
    else (n mod 2 = 1) :: bignum_of_int (n / 2)

  let rec int_of_bignum b : int =
    match b with
    | [] -> 0
    | h::t -> (bit2int h) + 2 * int_of_bignum t

  let rec is_zero b =
    match b with
    | [] -> true
    | h::t -> not h && is_zero t

  let bignum_of_string s : t =
    let len = String.length s in
    let rec aux s i =
      if i = len then []
      else
        (s.[i] = '1') :: aux s (i + 1)
    in
    aux s 0 |> List.rev

  let add b1 b2 =
    let rec aux b1 b2 (carry:bool) =
      match b1, b2, carry with
      | [], b2, false -> b2
      | [], b2, true -> aux [true] b2 false
      | b1, [], false -> b1
      | b1, [], true -> aux b1 [true] false
      | h1::t1, h2::t2, c ->
        let sum = bit2int h1 + bit2int h2 + bit2int c in
        (sum mod 2 = 1) :: aux t1 t2 (sum > 1)
    in
    aux b1 b2 false

  let sub b1 b2 =
    let rec aux b1 b2 (carry:bool) =
      match b1, b2,carry with
      (* truncated sub 0-x = 0 *)
      | [], _, c -> (c || not(is_zero b2),[])
      | _, [], false -> (false,b1)
      | _, [], true -> aux b1 [true] false
      (* (t1*2+h1)-(t2*2+h2) = (t1-t2)*2+(h1-h2) *)
      | h1::t1, h2::t2, c ->
        (* h1-h2-c => h, c *)
        let diff = (bit2int h1) - (bit2int h2) - (bit2int c) in
        let (trunc, r) = aux t1 t2 (diff < 0) in
        (trunc, ((abs diff) mod 2 = 1) :: r)
    in
    let (trunc, r) = aux b1 b2 false in
    if trunc then [] else r

  let rec mul b1 b2 =
    match b1 with
    | [] -> []
    (* (2*t1+0)*b2 = (t1*b2)*2+0  *)
    | false::t1 -> false::mul t1 b2
    (* (2*t1+1)*b2 = (t1*b2)*2+b2 *)
    | true::t1 -> add (false::mul t1 b2) b2

  let rec cmp b1 b2 =
    match b1, b2 with
    | [], [] -> Eq
    | [], _ -> if is_zero b2 then Eq else Lt
    | _, [] -> if is_zero b1 then Eq else Gt
    | h1::t1, h2::t2 ->
      match cmp t1 t2 with
      | Eq -> if h1 = h2 then Eq else if h1 then Gt else Lt
      | c -> c

  (* long division *)
  let divmod b1 b2 =
    if is_zero b2 then failwith "divmod: division by zero" else
    let n = List.rev b1 in
    let d = b2 in
    let rec aux n r q =
      match n with
      | [] -> (q,r)
      | n0::nr ->
        let r' = n0::r in
        if cmp r' d = Lt then
          aux nr r' (false::q)
        else
          aux nr (sub r' d) (true::q)
    in
    aux n [] []

  let modulo b1 b2 = snd (divmod b1 b2)
  let div b1 b2 = fst (divmod b1 b2)

  let string_of_bignum ?(base=2) b : string =
    if is_zero b then "0" else
    if base = 2 then
      let rec aux b =
        match b with
        | [] -> ""
        | h::t -> aux t ^ (if h then "1" else "0")
      in
      aux b
    else
      let rec aux b =
        if is_zero b then ""
        else
          let (q, r) = divmod b (bignum_of_int base) in
          aux q ^ string_of_int (int_of_bignum r)
      in
      aux b

  let shr b1 b2 =
    let rec aux b1 n =
      if n = 0 then b1
      else
        match b1 with
        | [] -> []
        | _::t -> aux t (n - 1)
    in
    aux b1 (int_of_bignum b2)

  let shl b1 b2 =
    let rec aux b1 n =
      if n = 0 then b1
      else aux (false::b1) (n - 1)
    in
    aux b1 (int_of_bignum b2)

  let abs = Fun.id

  let rec pow b1 b2 =
    if is_zero b2 then bignum_of_int 1
    else
      match b2 with
      | [] -> assert false
      | false::t ->
        pow (mul b1 b1) t
      | true::t ->
        mul b1 (pow (mul b1 b1) t)

end

(*
  Arbitrary precision integers based on a natural number implementation
  The functor takes a bignum implementation and converts it to a
  bignum implementation that handles general integers (positive and negative).
*)
module BigInt ( N : BigNum ) : BigNum
=  struct
  type sign = Neg | Pos
  type t = sign * N.t

  let bignum_of_int n = ((if n < 0 then Neg else Pos), N.bignum_of_int (abs n))
  let int_of_bignum (s, b) = (if s = Neg then -1 else 1) * N.int_of_bignum b

  let string_of_bignum ?(base=2) (s, b) = (if s = Neg then "-" else "") ^ N.string_of_bignum ~base b
  let bignum_of_string s =
    if s.[0] = '-' then (Neg, N.bignum_of_string (String.sub s 1 (String.length s - 1)))
    else (Pos, N.bignum_of_string s)

  let opp = function Neg -> Pos | Pos -> Neg

  let sub (s1, b1) (s2, b2) =
    match s1, s2 with
    | Pos, Neg -> (Pos, N.add b1 b2)
    | Neg, Pos -> (Neg, N.add b1 b2)
    | Pos, Pos | Neg, Neg ->
      let sign s = if s1 = Pos then s else opp s in
      begin
        match N.cmp b1 b2 with
        | Lt -> (sign Neg, N.sub b2 b1)
        | Eq -> (Pos, N.bignum_of_int 0)
        | Gt -> (sign Pos, N.sub b1 b2)
      end

  let add (s1, b1) (s2, b2) = sub (s1, b1) (opp s2, b2)

  let mul (s1, b1) (s2, b2) =
    ((if s1 = s2 then Pos else Neg), N.mul b1 b2)

  let cmp (s1, b1) (s2, b2) =
    if N.is_zero b1 && N.is_zero b2 then Eq
    else
    match s1, s2 with
    | Neg, Pos -> Lt
    | Pos, Neg -> Gt
    | Neg, Neg -> N.cmp b2 b1
    | Pos, Pos -> N.cmp b1 b2

  let is_zero (_, b) = N.is_zero b

  let modulo (s1, b1) (_, b2) =
    (s1, N.modulo b1 b2)

  let abs (_, b) = (Pos, b)

  let div (s1, b1) (s2, b2) =
    ((if s1 = s2 then Pos else Neg), N.div b1 b2)

  let divmod (s1, b1) (s2, b2) =
    let (q, r) = N.divmod b1 b2 in
    ((if s1 = s2 then Pos else Neg), q),
    (s1, r)

  let shr (s, b) (_, b2) = (s, N.shr b b2)
  let shl (s, b) (_, b2) = (s, N.shl b b2)

  let pow (s1, b1) (s2, b2) =
    if s2 = Neg then failwith "pow: negative exponent" else
      if is_zero (s2,b2) then (Pos, N.bignum_of_int 1)
      else
        let even = N.cmp (N.modulo b2 (N.bignum_of_int 2)) (N.bignum_of_int 0) = Eq in
        ((if even then Pos else s1), N.pow b1 b2)

end

(* Convenient module to overwrite the integer operations *)
module BigIntNotation (M:BigNum) = struct
  type bignum = M.t
  let ( + ) = M.add
  let ( - ) = M.sub
  let ( * ) = M.mul
  let ( / ) = M.div
  let ( mod ) = M.modulo
  let ( < ) a b = M.cmp a b = Lt
  let ( > ) a b = M.cmp a b = Gt
  let ( <= ) a b = M.cmp a b <> Gt
  let ( >= ) a b = M.cmp a b <> Lt
  (* let ( = ) a b = M.cmp a b = Eq *)
  (* let ( <> ) a b = M.cmp a b <> Eq *)
  let ( ** ) = M.pow

  let i2b = M.bignum_of_int
  let b2i = M.int_of_bignum
  let s2b = M.bignum_of_string
  let b2s = M.string_of_bignum ~base:10
end
