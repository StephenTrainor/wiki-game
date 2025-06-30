type 'a t =
  { mutable arr : 'a array
  ; mutable size : int
  ; mutable max_length : int
  ; initial_value : int
  }

let create initial_value =
  { arr = Array.make 4 initial_value
  ; size = 0
  ; max_length = 4
  ; initial_value
  }
;;

let size t = t.size

let swap t i j =
  let temp = t.arr.(i) in
  t.arr.(i) <- t.arr.(j);
  t.arr.(j) <- temp
;;

let resize t =
  let new_arr = Array.make (t.max_length * 2) t.initial_value in
  for i = 0 to t.size - 1 do
    new_arr.(i) <- t.arr.(i)
  done;
  t.max_length <- t.max_length * 2;
  t.arr <- new_arr
;;

let desize t =
  if t.max_length > 4
  then (
    let new_arr = Array.make (t.max_length / 2) t.initial_value in
    for i = 0 to t.size - 1 do
      new_arr.(i) <- t.arr.(i)
    done;
    t.max_length <- t.max_length / 2;
    t.arr <- new_arr)
;;

let append t v =
  if t.size = t.max_length then resize t;
  t.arr.(t.size) <- v;
  t.size <- t.size + 1
;;

let pop t =
  let output = t.arr.(t.size - 1) in
  t.size <- t.size - 1;
  if t.size * 4 < t.max_length then desize t;
  output
;;
