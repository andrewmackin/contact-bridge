let random_key_pair d =
  let () = Random.self_init () in
  let rec random_key_pair_helper acc = function
    | [] -> acc
    | h::t -> 
      let pair = (Random.int 500, h) in
      random_key_pair_helper (pair::acc) t
  in random_key_pair_helper [] d

let compare_keys (key_1,_) (key_2,_) =
  Stdlib.compare key_1 key_2

let shuffle lst =
  lst |> random_key_pair |> List.sort compare_keys |> List.split |> snd