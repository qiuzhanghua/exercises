(*
https://ocaml.org/exercises
   *)
let rec last lst =
  match lst with [] -> None | [ x ] -> Some x | _ :: tl -> last tl

let () =
  assert (last [ "a"; "b"; "c"; "d" ] = Some "d");
  assert (last [] = None)

let rec last_two lst =
  match lst with
  | [] | [ _ ] -> None
  | [ x; y ] -> Some (x, y)
  | _ :: tl -> last_two tl

let () =
  assert (last_two [ "a"; "b"; "c"; "d" ] = Some ("c", "d"));
  assert (last_two [ "a" ] = None);
  assert (last_two [] = None)

let () =
  assert (List.nth [ "a"; "b"; "c"; "d" ] 2 = "c");
  try
    let _ = List.nth [ "a" ] 2 in
    ()
  with Failure _ -> (
    assert true;
    (* or *)
    match List.nth [] 2 with
    | exception Failure _ -> assert true
    | _ -> assert false)

let length lst =
  let rec aux acc = function [] -> acc | _ :: tl -> aux (acc + 1) tl in
  aux 0 lst

let () =
  assert (length [ "a"; "b"; "c" ] = 3);
  assert (length [] = 0)

let () = assert (List.rev [ "a"; "b"; "c" ] = [ "c"; "b"; "a" ])
let is_palindrome lst = lst = List.rev lst

let () =
  assert (is_palindrome [ "x"; "a"; "m"; "a"; "x" ]);
  assert (not (is_palindrome [ "a"; "b" ]))

type 'a node = One of 'a | Many of 'a node list

let flatten list =
  let rec aux acc = function
    | [] -> acc
    | One x :: tl -> aux (x :: acc) tl
    | Many l :: tl -> aux (aux acc l) tl
  in
  List.rev (aux [] list)

let () =
  assert (
    flatten [ One "a"; Many [ One "b"; Many [ One "c"; One "d" ]; One "e" ] ]
    = [ "a"; "b"; "c"; "d"; "e" ])

let compress lst =
  let rec aux acc = function
    | [] -> acc
    | [ x ] -> x :: acc
    | x :: (y :: _ as tl) -> if x = y then aux acc tl else aux (x :: acc) tl
  in
  List.rev (aux [] lst)

let () =
  assert (
    compress [ "a"; "a"; "a"; "a"; "b"; "c"; "c"; "a"; "a"; "d"; "e"; "e"; "e" ]
    = [ "a"; "b"; "c"; "a"; "d"; "e" ])

let pack lst =
  let rec aux current acc = function
    | [] -> []
    | [ x ] -> (x :: current) :: acc
    | a :: (b :: _ as t) ->
        if a = b then aux (a :: current) acc t
        else aux [] ((a :: current) :: acc) t
  in
  List.rev (aux [] [] lst)

let () =
  assert (
    pack [ "a"; "a"; "a"; "a"; "b"; "c"; "c"; "a"; "a"; "d"; "e"; "e"; "e" ]
    = [
        [ "a"; "a"; "a"; "a" ];
        [ "b" ];
        [ "c"; "c" ];
        [ "a"; "a" ];
        [ "d" ];
        [ "e"; "e"; "e" ];
      ])

let encode lst =
  let rec aux count acc = function
    | [] -> []
    | [ x ] -> (count + 1, x) :: acc
    | a :: (b :: _ as t) ->
        if a = b then aux (count + 1) acc t else aux 0 ((count + 1, a) :: acc) t
  in
  List.rev (aux 0 [] lst)

let () =
  assert (
    encode [ "a"; "a"; "a"; "a"; "b"; "c"; "c"; "a"; "a"; "d"; "e"; "e"; "e" ]
    = [ (4, "a"); (1, "b"); (2, "c"); (2, "a"); (1, "d"); (3, "e") ])

type 'a rle = One of 'a | Many of int * 'a

let encode2 lst =
  let rle count x = if count = 1 then One x else Many (count, x) in
  let rec aux count acc = function
    | [] -> []
    | [ x ] -> rle (count + 1) x :: acc
    | a :: (b :: _ as t) ->
        if a = b then aux (count + 1) acc t
        else aux 0 (rle (count + 1) a :: acc) t
  in
  List.rev (aux 0 [] lst)

let () =
  assert (
    encode2 [ "a"; "a"; "a"; "a"; "b"; "c"; "c"; "a"; "a"; "d"; "e"; "e"; "e" ]
    = [
        Many (4, "a");
        One "b";
        Many (2, "c");
        Many (2, "a");
        One "d";
        Many (3, "e");
      ])

let decode lst =
  let rec many acc n x = if n = 0 then acc else many (x :: acc) (n - 1) x in
  let rec aux acc = function
    | [] -> acc
    | One x :: tl -> aux (x :: acc) tl
    | Many (n, x) :: tl -> aux (many acc n x) tl
  in
  List.rev (aux [] lst)

let () =
  assert (
    decode
      [
        Many (4, "a");
        One "b";
        Many (2, "c");
        Many (2, "a");
        One "d";
        Many (3, "e");
      ]
    = [ "a"; "a"; "a"; "a"; "b"; "c"; "c"; "a"; "a"; "d"; "e"; "e"; "e" ])

let encode3 lst =
  let rle count x = if count = 1 then One x else Many (count, x) in
  let rec aux count acc = function
    | [] -> []
    | [ x ] -> rle (count + 1) x :: acc
    | a :: (b :: _ as t) ->
        if a = b then aux (count + 1) acc t
        else aux 0 (rle (count + 1) a :: acc) t
  in
  List.rev (aux 0 [] lst)

let () =
  assert (
    encode3 [ "a"; "a"; "a"; "a"; "b"; "c"; "c"; "a"; "a"; "d"; "e"; "e"; "e" ]
    = [
        Many (4, "a");
        One "b";
        Many (2, "c");
        Many (2, "a");
        One "d";
        Many (3, "e");
      ])

let duplicate lst =
  let rec aux acc = function
    | [] -> acc
    | hd :: tl -> aux (hd :: hd :: acc) tl
  in
  List.rev (aux [] lst)

let () = assert (duplicate [ "a"; "b" ] = [ "a"; "a"; "b"; "b" ])

let replicate lst n =
  let rec many n acc x = if n = 0 then acc else many (n - 1) (x :: acc) x in
  let rec aux acc = function [] -> acc | hd :: tl -> aux (many n acc hd) tl in
  List.rev (aux [] lst)

let () = assert (replicate [ "a"; "b" ] 3 = [ "a"; "a"; "a"; "b"; "b"; "b" ])

let drop lst n =
  let rec aux acc count = function
    | [] -> acc
    | hd :: tl ->
        if count = 1 then aux acc n tl else aux (hd :: acc) (count - 1) tl
  in
  List.rev (aux [] n lst)

let () =
  assert (
    drop [ "a"; "b"; "c"; "d"; "e"; "f"; "g"; "h"; "i"; "j" ] 3
    = [ "a"; "b"; "d"; "e"; "g"; "h"; "j" ])

let split lst n =
  let rec aux acc count = function
    | [] -> (List.rev acc, [])
    | h :: t ->
        if count = 0 then (List.rev acc, h :: t)
        else aux (h :: acc) (count - 1) t
  in
  aux [] n lst

let () =
  assert (
    split [ "a"; "b"; "c"; "d"; "e"; "f"; "g"; "h"; "i"; "j" ] 3
    = ([ "a"; "b"; "c" ], [ "d"; "e"; "f"; "g"; "h"; "i"; "j" ]))

let slice lst i k =
  let rec aux acc i' k' = function
    | [] -> acc
    | h :: t ->
        if i' = 0 then if k' = 0 then h :: acc else aux (h :: acc) 0 (k' - 1) t
        else aux acc (i' - 1) (k' - 1) t
  in
  List.rev (aux [] i k lst)

let () =
  assert (
    slice [ "a"; "b"; "c"; "d"; "e"; "f"; "g"; "h"; "i"; "j" ] 2 6
    = [ "c"; "d"; "e"; "f"; "g" ])

let rotate lst n =
  let rec aux acc count = function
    | [] -> acc
    | h :: t ->
        if count = 0 then (h :: t) @ List.rev acc
        else aux (h :: acc) (count - 1) t
  in
  aux [] n lst

let () =
  assert (
    rotate [ "a"; "b"; "c"; "d"; "e"; "f"; "g"; "h" ] 3
    = [ "d"; "e"; "f"; "g"; "h"; "a"; "b"; "c" ])

let remove_at n lst =
  let rec aux acc count = function
    | [] -> acc
    | h :: t ->
        if count = 0 then List.rev acc @ t else aux (h :: acc) (count - 1) t
  in
  aux [] n lst

let () = assert (remove_at 1 [ "a"; "b"; "c"; "d" ] = [ "a"; "c"; "d" ])
