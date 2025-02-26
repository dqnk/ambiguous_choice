module Task = Domainslib.Task;;

Random.self_init ();;

let f () =
  let top = Random.int 1_000_000 in
  let x = ref 0 in
  for _ = 0 to top do
    x := !x + 1;
  done;
  !x < 1000;;

(* Helper function to verify what f does, mostly irrelevant *)
let single_thread_verify () =
  let cnt = ref 0 in
  for _ = 0 to 10_000 do
    if f () then
      begin
        cnt := !cnt + 1;
        end
  done;
  print_int !cnt;;

(* return result and remaining lifespan *)
let rec run_test lifespan = match lifespan with
  | 0 -> false, 0
  | _ -> if f () then true, lifespan else run_test (lifespan - 1)

let single_thread_test lifespan =
  run_test lifespan;;

(* convert return values to option for Task.parallel_find *)
let wrap b = match b with
  | false, _ -> None
  | true, x -> Some (true, x);;


(* Convert back to bool - true if any of the threads had found the needle and false if none did *)
let search_needle pool lifespan = match
  Task.parallel_find pool ~start:0 ~finish:(lifespan) ~body:(fun _ ->
    wrap (run_test 1)
  ) with
    | Some (true, x) -> true, x
    | _ -> false, 0;;

let multi_thread_test lifespan num_domains =
  let pool = Task.setup_pool ~num_domains ~name:"pool" () in
  let res = Task.run pool (fun () -> search_needle pool lifespan) in
  Task.teardown_pool pool;
  res;;

print_string "Single thread:\n";
let result = single_thread_test 1_000_000 in
print_string (string_of_bool (fst result) ^ " " ^ string_of_int (snd result));

print_newline ();

print_string "Multi thread:\n";
let result = multi_thread_test 1_000_000 4 in
print_string (string_of_bool (fst result) ^ " " ^ string_of_int (snd result));
