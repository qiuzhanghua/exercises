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

let insert_at x n lst =
  let rec aux acc count = function
    | [] -> acc
    | h :: t ->
        if count = 0 then List.rev acc @ (x :: h :: t)
        else aux (h :: acc) (count - 1) t
  in
  aux [] n lst

let () =
  assert (
    insert_at "alfa" 1 [ "a"; "b"; "c"; "d" ] = [ "a"; "alfa"; "b"; "c"; "d" ])

let range a b =
  let rec aux acc a' b' =
    if a' = b' then b' :: acc else aux (b' :: acc) a' (b' - 1)
  in
  aux [] a b

let () = assert (range 4 9 = [ 4; 5; 6; 7; 8; 9 ])

let rand_select n lst =
  Random.self_init ();
  let rec extract_at k acc = function
    | [] -> (None, [])
    | hd :: tl ->
        if k = 0 then (Some hd, acc @ tl) else extract_at (k - 1) (hd :: acc) tl
  in
  let len = List.length lst in
  let rec aux acc n lst len =
    (* Printf.printf "list: %s\n" (String.concat ";" lst); *)
    if n = 0 || len <= 0 then acc
    else
      let picked, rest = extract_at (Random.int len) [] lst in
      (* Printf.printf "pick: %s\n" (match picked with None -> "None" | Some x -> x);
         Printf.printf "rest: %s\n" (String.concat ";" rest); *)
      match picked with
      | None -> acc
      | Some x -> aux (x :: acc) (n - 1) rest (len - 1)
  in
  aux [] n lst len

let () =
  Printf.printf "%s\n"
    (String.concat ";" (rand_select 8 [ "a"; "b"; "c"; "d"; "e"; "f"; "g" ]))

let lotto_select k m = rand_select k (range 1 m)

let () =
  Printf.printf "%s\n"
    (String.concat ";" (List.map string_of_int (lotto_select 6 50)))

let permutation lst =
  Random.self_init ();
  let rec select_at k acc = function
    | [] -> raise Not_found
    | hd :: tl ->
        if k = 0 then (hd, acc @ tl) else select_at (k - 1) (hd :: acc) tl
  in
  let select_rand k lst = select_at k [] lst in
  let len = List.length lst in
  let rec aux k acc lst =
    if k = 0 then acc
    else
      let m = Random.int k in
      let answer, rest = select_rand m lst in
      aux (k - 1) (answer :: acc) rest
  in
  aux len [] lst

let () =
  Printf.printf "%s\n"
    (String.concat ";" (permutation [ "a"; "b"; "c"; "d"; "e"; "f" ]))

let extract k lst =
  let rec aux acc k lst =
    if k <= 0 then [ acc ]
    else
      match lst with
      | [] -> []
      | hd :: tl ->
          let with_hd = aux (hd :: acc) (k - 1) tl in
          let without_hd = aux acc k tl in
          with_hd @ without_hd
  in
  aux [] k lst

let () =
  Printf.printf "%s\n"
    (String.concat ";"
       (List.map (String.concat "") (extract 3 [ "a"; "b"; "c"; "d"; "e" ])))

let length_sort lst =
  List.sort (fun a b -> compare (List.length a) (List.length b)) lst

let feq_sort lst =
  List.sort
    (fun a b ->
      compare
        (List.length (List.filter (fun x -> x = a) lst))
        (List.length (List.filter (fun x -> x = b) lst)))
    lst

let () =
  Printf.printf "%s\n"
    (String.concat ";"
       (List.map (String.concat "")
          (length_sort
             [
               [ "a"; "b"; "c" ];
               [ "d"; "e" ];
               [ "f"; "g"; "h" ];
               [ "d"; "e" ];
               [ "i"; "j"; "k"; "l" ];
               [ "m"; "n" ];
               [ "o" ];
             ])));
  Printf.printf "%s\n"
    (String.concat ";"
       (List.map (String.concat "")
          (feq_sort
             [
               [ "a"; "b"; "c" ];
               [ "d"; "e" ];
               [ "f"; "g"; "h" ];
               [ "d"; "e" ];
               [ "i"; "j"; "k"; "l" ];
               [ "m"; "n" ];
               [ "o" ];
               [ "a"; "b"; "c" ];
               [ "a"; "b"; "c" ];
             ])))

let is_prime n =
  let rec aux d =
    if d * d > n then true else if n mod d = 0 then false else aux (d + 1)
  in
  n > 1 && aux 2

let () = assert (not (is_prime 1))
let () = assert (is_prime 7)

let gcd a b =
  let rec aux a b =
    if b = 0 then a else aux b (a mod b)
  in
  aux (abs a) (abs b)

let () = assert (gcd 65 26 = 13)