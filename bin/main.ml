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
