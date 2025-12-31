open! Core
open! Hardcaml
open! Hardcaml_waveterm

module Simulator = Cyclesim.With_interface (Day01.Top.I) (Day01.Top.O)

let check_int ~name ~got ~expect =
  if got <> expect then failwithf "%s: expected %d, got %d" name expect got ()
;;

let read_top (o : Bits.t ref Day01.Top.O.t) =
  let is_zero    = Bits.to_int !(o.is_zero) in
  let next_state = Bits.to_int !(o.next_state) in
  let count      = Bits.to_int !(o.count) in
  is_zero, next_state, count
;;

let parse_line (line : string) : (char * int) option =
  let s = String.strip line in
  if String.is_empty s then None
  else
    let dir = s.[0] in
    let n = Int.of_string (String.sub s ~pos:1 ~len:(String.length s - 1)) in
    Some (dir, n)
;;

let amount_of (dir : char) (n : int) : int =
  let n0 = n mod 100 in
  match Char.uppercase dir with
  | 'R' -> n0
  | 'L' -> (100 - n0) mod 100
  | _ -> failwithf "Invalid direction '%c'" dir ()
;;

let testbench () =
  let scope = Scope.create ~flatten_design:true () in
  let sim = Simulator.create (Day01.Top.create scope) in
  let i = Cyclesim.inputs sim in

  let step ~reset ~valid ~amount =
    i.reset  := (if reset = 1 then Bits.vdd else Bits.gnd);
    i.valid  := (if valid = 1 then Bits.vdd else Bits.gnd);
    i.amount := Bits.of_int ~width:7 amount;

    Cyclesim.cycle_check sim;
    Cyclesim.cycle_before_clock_edge sim;
    let before = read_top (Cyclesim.outputs ~clock_edge:Before sim) in

    Cyclesim.cycle_at_clock_edge sim;
    Cyclesim.cycle_after_clock_edge sim;
    let after = read_top (Cyclesim.outputs sim) in

    before, after
  in

  (* AOC 2025 Day 1 input stream *)
  let cmds =
    In_channel.read_lines "inputs/day01_inputs.txt"
    |> List.filter_map ~f:parse_line
  in

  let exp_state = ref 50 in
  let exp_count = ref 0 in

  (* Apply reset for 1 cycle (synchronous clear) *)
  let (_b, (_iz_a, ns_a, count_a)) = step ~reset:1 ~valid:0 ~amount:0 in
  check_int ~name:"reset.after.next_state" ~got:ns_a ~expect:50;
  check_int ~name:"reset.after.count" ~got:count_a ~expect:0;

  (* One valid transaction per line *)
  cmds
  |> List.iteri ~f:(fun idx (dir, n) ->
       let amount = amount_of dir n in
       let expected_next = (!exp_state + amount) mod 100 in
       let expected_is_zero = if expected_next = 0 then 1 else 0 in
       let expected_count_after = !exp_count + expected_is_zero in

       let ((iz_b, ns_b, count_b), (_iz_a, _ns_a, count_a)) =
         step ~reset:0 ~valid:1 ~amount
       in

       (* Check this transaction’s computed next state and is_zero, similar structure to
       unit zd testbench in zd_sim.ml *)
       check_int ~name:(sprintf "[%d] before.next_state" idx) ~got:ns_b ~expect:expected_next;
       check_int ~name:(sprintf "[%d] before.is_zero" idx) ~got:iz_b ~expect:expected_is_zero;
       check_int ~name:(sprintf "[%d] before.count" idx) ~got:count_b ~expect:!exp_count;

       (* Ensure accumulator has incremented if is_zero == 1 *)
       check_int ~name:(sprintf "[%d] after.count" idx) ~got:count_a ~expect:expected_count_after;

       (* Advance testbench *)
       exp_state := expected_next;
       exp_count := expected_count_after);

  printf "%d total inputs\n" (List.length cmds);
  printf "Final expected count = %d\n" !exp_count;

  check_int ~name:"final.expected_count" ~got:!exp_count ~expect:980;
  printf "Day 01 Part 1 PASSED.\n";
;;

let () = testbench ()


