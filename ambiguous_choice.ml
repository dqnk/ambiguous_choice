module Task = Domainslib.Task;;

Random.self_init ()

let f () =
  let rnd = Random.int 1_000_000 in
  let x = ref 0 in
  for _ = 0 to rnd do
    x := !x + 1
  done;
  (* return a boolean value which is rarely true and the value of x for debugging *)
  (!x < 1000, !x)

(* helper function to verify what f does, mostly irrelevant *)
let single_thread_verify () =
  let cnt = ref 0 in
  for _ = 0 to 10_000 do
    let res = f () in
    if fst res then cnt := !cnt + 1
  done;
  print_int !cnt

(* return result and remaining fuel *)
let rec run_test fuel =
  let res, rnd = f () in
  match fuel with
  | 0 -> (false, 0)
  (* you could also return fuel instead of rnd if you wanted for debugging purposes *)
  | _ -> if res then (true, rnd) else run_test (fuel - 1)

let single_thread_test ~fuel = run_test fuel

(* convert return values to option for Task.parallel_find *)
let wrap b = match b with false, _ -> None | true, x -> Some (true, x)

let search_needle pool fuel =
  match
    (* parallel ambiguous choice, picks a thread that returns Some a,
       not guaranteed to stop as early as possible *)
    Task.parallel_find pool ~start:0 ~finish:fuel ~body:(fun _ ->
        wrap (run_test 1))
    (* convert back to bool - true if any of the threads had found the needle and false if none did *)
  with
  | Some (true, x) -> (true, x)
  | _ -> (false, 0)

let multi_thread_test ~fuel ~num_domains =
  let pool = Task.setup_pool ~num_domains ~name:"pool" () in
  let res = Task.run pool (fun () -> search_needle pool fuel) in
  Task.teardown_pool pool;
  res

let fuel = 1_000_000;;

print_string "Single thread:\n";
let result = single_thread_test ~fuel in
print_string (string_of_bool (fst result) ^ " " ^ string_of_int (snd result));

print_newline ()

let num_domains = 4;;

print_string "Multi thread:\n";
let result = multi_thread_test ~fuel ~num_domains in
print_string (string_of_bool (fst result) ^ " " ^ string_of_int (snd result));

print_newline ()
