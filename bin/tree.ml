type 'a tree = Leaf | Node of 'a node
and 'a node = { data : 'a; left : 'a tree; right : 'a tree }

let rec fold_tree f acc t =
  match t with
  | Leaf -> acc
  | Node { data; left; right } ->
    let acc = f acc data in
    let acc = fold_tree f acc left in
    fold_tree f acc right

let () = assert (fold_tree (+) 0 (Node { data = 1; left = Leaf; right = Leaf }) = 1)
