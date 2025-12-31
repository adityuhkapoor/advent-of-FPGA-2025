open! Core
open! Hardcaml
open! Hardcaml_waveterm

module Simulator = Cyclesim.With_interface(Day01.Zero_detector.I)(Day01.Zero_detector.O)

let testbench() = 
  let scope = Scope.create ~flatten_design:true () in
  let sim = Simulator.create (Day01.Zero_detector.create scope) in
  let inputs = Cyclesim.inputs sim in

  let check_int ~name ~got ~expect =
    if got <> expect then
      failwithf "%s: expected %d, got %d" name expect got ()
  in

  let read_zd (o : Bits.t ref Day01.Zero_detector.O.t) =
    let is_zero    = Bits.to_int !(o.is_zero) in
    let next_state = Bits.to_int !(o.next_state) in
    let state      = Bits.to_int !(o.state) in
    is_zero, next_state, state
  in

  let step ~reset ~valid ~amount =
    inputs.reset := if reset = 1 then Bits.vdd else Bits.gnd;
    inputs.valid := if valid = 1 then Bits.vdd else Bits.gnd;
    inputs.amount := Bits.of_int ~width:7 amount;

    Cyclesim.cycle_check sim;
    Cyclesim.cycle_before_clock_edge sim;
    let before = read_zd (Cyclesim.outputs ~clock_edge:Before sim) in
    (* checking inputs used to drive before edge *)

    Cyclesim.cycle_at_clock_edge sim;
    Cyclesim.cycle_after_clock_edge sim;
    let after = read_zd (Cyclesim.outputs sim) in

    before, after
  in

  let (_before, (iz_a, ns_a, st_a)) =
    step ~reset:1 ~valid:0 ~amount:0
  in
  check_int ~name:"after.is_zero" ~got:iz_a ~expect:0;
  check_int ~name:"after.next_state" ~got:ns_a ~expect:50;
  check_int ~name:"after.state" ~got:st_a ~expect:50;
  (*Checks post edge values of is_zero, next_state and state after reset is called on zd. *)

  let ((iz_b, ns_b, st_b), (iz_a, _ns_a, st_a)) =
    step ~reset:0 ~valid:1 ~amount:10
  in
  
  check_int ~name:"before.is_zero" ~got:iz_b ~expect:0;
  check_int ~name:"before.next_state" ~got:ns_b ~expect:60;
  check_int ~name:"before.state" ~got:st_b ~expect:50;

  check_int ~name:"after.is_zero" ~got:iz_a ~expect:0;
  check_int ~name:"after.state" ~got:st_a ~expect:60;

  let ((iz_b, ns_b, st_b), (_iz_a, _ns_a, st_a)) =
    step ~reset:0 ~valid:1 ~amount:40
  in
  
  check_int ~name:"before.is_zero" ~got:iz_b ~expect:1;
  check_int ~name:"before.next_state" ~got:ns_b ~expect:0;
  check_int ~name:"before.state" ~got:st_b ~expect:60;

  check_int ~name:"after.state" ~got:st_a ~expect:0;

  let ((iz_b, ns_b, st_b), (_iz_a, _ns_a, st_a)) =
    step ~reset:0 ~valid:0 ~amount:0
  in

  check_int ~name:"before.is_zero" ~got:iz_b ~expect:0;
  check_int ~name:"before.next_state" ~got:ns_b ~expect:0;
  check_int ~name:"before.state" ~got:st_b ~expect:0;

  check_int ~name:"after.state" ~got:st_a ~expect:0;

  let ((iz_b, ns_b, st_b), (_iz_a, _ns_a, st_a)) =
    step ~reset:0 ~valid:1 ~amount:0
  in

  check_int ~name:"before.is_zero" ~got:iz_b ~expect:1;
  check_int ~name:"before.next_state" ~got:ns_b ~expect:0;
  check_int ~name:"before.state" ~got:st_b ~expect:0;

  check_int ~name:"after.state" ~got:st_a ~expect:0;

  let ((iz_b, ns_b, st_b), (_iz_a, _ns_a, st_a)) =
    step ~reset:0 ~valid:1 ~amount:50
  in

  check_int ~name:"before.is_zero" ~got:iz_b ~expect:0;
  check_int ~name:"before.next_state" ~got:ns_b ~expect:50;
  check_int ~name:"before.state" ~got:st_b ~expect:0;

  check_int ~name:"after.state" ~got:st_a ~expect:50;

  let ((iz_b, ns_b, st_b), (_iz_a, _ns_a, st_a)) =
    step ~reset:0 ~valid:1 ~amount:50
  in

  check_int ~name:"before.is_zero" ~got:iz_b ~expect:1;
  check_int ~name:"before.next_state" ~got:ns_b ~expect:0;
  check_int ~name:"before.state" ~got:st_b ~expect:50;

  check_int ~name:"after.state" ~got:st_a ~expect:0;

  let ((iz_b, ns_b, st_b), (_iz_a, _ns_a, st_a)) =
    step ~reset:0 ~valid:1 ~amount:50
  in

  check_int ~name:"before.is_zero" ~got:iz_b ~expect:0;
  check_int ~name:"before.next_state" ~got:ns_b ~expect:50;
  check_int ~name:"before.state" ~got:st_b ~expect:0;

  check_int ~name:"after.state" ~got:st_a ~expect:50;

  
  let ((iz_b, ns_b, st_b), (_iz_a, _ns_a, st_a)) =
    step ~reset:0 ~valid:1 ~amount:100
  in

  check_int ~name:"before.is_zero" ~got:iz_b ~expect:0;
  check_int ~name:"before.next_state" ~got:ns_b ~expect:50;
  check_int ~name:"before.state" ~got:st_b ~expect:50;

  check_int ~name:"after.state" ~got:st_a ~expect:50;

  let ((iz_b, ns_b, st_b), (_iz_a, _ns_a, st_a)) =
    step ~reset:0 ~valid:1 ~amount:122
  in

  check_int ~name:"before.is_zero" ~got:iz_b ~expect:0;
  check_int ~name:"before.next_state" ~got:ns_b ~expect:72;
  check_int ~name:"before.state" ~got:st_b ~expect:50;

  check_int ~name:"after.state" ~got:st_a ~expect:72;

  let ((iz_b, ns_b, st_b), (_iz_a, _ns_a, st_a)) =
    step ~reset:0 ~valid:1 ~amount:28
  in

  check_int ~name:"before.is_zero" ~got:iz_b ~expect:1;
  check_int ~name:"before.next_state" ~got:ns_b ~expect:0;
  check_int ~name:"before.state" ~got:st_b ~expect:72;

  check_int ~name:"after.state" ~got:st_a ~expect:0;

  (* valid=0: state must hold, but next_state is still a combinational preview *)
  let ((iz_b, ns_b, st_b), (_iz_a, _ns_a, st_a)) =
    step ~reset:0 ~valid:0 ~amount:13
  in
  check_int ~name:"valid0_13.before.is_zero" ~got:iz_b ~expect:0;
  check_int ~name:"valid0_13.before.next_state" ~got:ns_b ~expect:13;
  check_int ~name:"valid0_13.before.state" ~got:st_b ~expect:0;
  check_int ~name:"valid0_13.after.state" ~got:st_a ~expect:0;

  (* valid=0 even when next_state would be 0: is_zero must still be 0 *)
  let ((iz_b, ns_b, st_b), (_iz_a, _ns_a, st_a)) =
    step ~reset:0 ~valid:0 ~amount:100  (* from state 0 -> next_state preview 0 *)
  in
  check_int ~name:"valid0_100.before.is_zero" ~got:iz_b ~expect:0;
  check_int ~name:"valid0_100.before.next_state" ~got:ns_b ~expect:0;
  check_int ~name:"valid0_100.before.state" ~got:st_b ~expect:0;
  check_int ~name:"valid0_100.after.state" ~got:st_a ~expect:0;

  (* reset should override valid updates (synchronous clear) *)
  let (_before, (_iz_a, _ns_a, st_a)) =
    step ~reset:1 ~valid:1 ~amount:33
  in
  check_int ~name:"reset_overrides_valid.after.state" ~got:st_a ~expect:50;

  (* hold reset for another cycle *)
  let (_before, (_iz_a, _ns_a, st_a)) =
    step ~reset:1 ~valid:0 ~amount:0
  in
  check_int ~name:"reset_held.after.state" ~got:st_a ~expect:50;

  (* Drive to 99, then wrap with +1 -> 0 *)
  let ((iz_b, ns_b, st_b), (_iz_a, _ns_a, st_a)) =
    step ~reset:0 ~valid:1 ~amount:49  (* 50 -> 99 *)
  in
  check_int ~name:"to_99.before.is_zero" ~got:iz_b ~expect:0;
  check_int ~name:"to_99.before.next_state" ~got:ns_b ~expect:99;
  check_int ~name:"to_99.before.state" ~got:st_b ~expect:50;
  check_int ~name:"to_99.after.state" ~got:st_a ~expect:99;

  let ((iz_b, ns_b, st_b), (_iz_a, _ns_a, st_a)) =
    step ~reset:0 ~valid:1 ~amount:1   (* 99 -> 0 *)
  in
  check_int ~name:"wrap99p1.before.is_zero" ~got:iz_b ~expect:1;
  check_int ~name:"wrap99p1.before.next_state" ~got:ns_b ~expect:0;
  check_int ~name:"wrap99p1.before.state" ~got:st_b ~expect:99;
  check_int ~name:"wrap99p1.after.state" ~got:st_a ~expect:0;

  (* Repeated valid transactions that yield 0 should fire again each time *)
  let ((iz_b, ns_b, st_b), (_iz_a, _ns_a, st_a)) =
    step ~reset:0 ~valid:1 ~amount:0   (* 0 -> 0 *)
  in
  check_int ~name:"zero_repeat1.before.is_zero" ~got:iz_b ~expect:1;
  check_int ~name:"zero_repeat1.before.next_state" ~got:ns_b ~expect:0;
  check_int ~name:"zero_repeat1.before.state" ~got:st_b ~expect:0;
  check_int ~name:"zero_repeat1.after.state" ~got:st_a ~expect:0;

  let ((iz_b, ns_b, st_b), (_iz_a, _ns_a, st_a)) =
    step ~reset:0 ~valid:1 ~amount:0   (* 0 -> 0 again *)
  in
  check_int ~name:"zero_repeat2.before.is_zero" ~got:iz_b ~expect:1;
  check_int ~name:"zero_repeat2.before.next_state" ~got:ns_b ~expect:0;
  check_int ~name:"zero_repeat2.before.state" ~got:st_b ~expect:0;
  check_int ~name:"zero_repeat2.after.state" ~got:st_a ~expect:0;

  (* Exercise subtract-200 path: make state=90 then add 127 -> 217 mod 100 = 17 *)
  let ((iz_b, ns_b, st_b), (_iz_a, _ns_a, st_a)) =
    step ~reset:0 ~valid:1 ~amount:90  (* 0 -> 90 *)
  in
  check_int ~name:"to_90.before.is_zero" ~got:iz_b ~expect:0;
  check_int ~name:"to_90.before.next_state" ~got:ns_b ~expect:90;
  check_int ~name:"to_90.before.state" ~got:st_b ~expect:0;
  check_int ~name:"to_90.after.state" ~got:st_a ~expect:90;

  let ((iz_b, ns_b, st_b), (_iz_a, _ns_a, st_a)) =
    step ~reset:0 ~valid:1 ~amount:127 (* 90 -> 17 *)
  in
  check_int ~name:"add127_from90.before.is_zero" ~got:iz_b ~expect:0;
  check_int ~name:"add127_from90.before.next_state" ~got:ns_b ~expect:17;
  check_int ~name:"add127_from90.before.state" ~got:st_b ~expect:90;
  check_int ~name:"add127_from90.after.state" ~got:st_a ~expect:17;

  (* valid=0 hold at non-zero state while preview changes *)
  let ((iz_b, ns_b, st_b), (_iz_a, _ns_a, st_a)) =
    step ~reset:0 ~valid:0 ~amount:127  (* preview: 17+127=144 mod100=44; state holds 17 *)
  in
  check_int ~name:"hold17_preview44.before.is_zero" ~got:iz_b ~expect:0;
  check_int ~name:"hold17_preview44.before.next_state" ~got:ns_b ~expect:44;
  check_int ~name:"hold17_preview44.before.state" ~got:st_b ~expect:17;
  check_int ~name:"hold17_preview44.after.state" ~got:st_a ~expect:17;

  (* Another wrap boundary: reset -> 50 -> 98 -> +2 -> 0 *)
  let (_before, (_iz_a, _ns_a, st_a)) =
    step ~reset:1 ~valid:0 ~amount:0
  in
  check_int ~name:"reset_for_98.after.state" ~got:st_a ~expect:50;

  let ((iz_b, ns_b, st_b), (_iz_a, _ns_a, st_a)) =
    step ~reset:0 ~valid:1 ~amount:48  (* 50 -> 98 *)
  in
  check_int ~name:"to_98.before.is_zero" ~got:iz_b ~expect:0;
  check_int ~name:"to_98.before.next_state" ~got:ns_b ~expect:98;
  check_int ~name:"to_98.before.state" ~got:st_b ~expect:50;
  check_int ~name:"to_98.after.state" ~got:st_a ~expect:98;

  let ((iz_b, ns_b, st_b), (_iz_a, _ns_a, st_a)) =
    step ~reset:0 ~valid:1 ~amount:2   (* 98 -> 0 *)
  in
  check_int ~name:"wrap98p2.before.is_zero" ~got:iz_b ~expect:1;
  check_int ~name:"wrap98p2.before.next_state" ~got:ns_b ~expect:0;
  check_int ~name:"wrap98p2.before.state" ~got:st_b ~expect:98;
  check_int ~name:"wrap98p2.after.state" ~got:st_a ~expect:0;

;;

let () = testbench ()
  