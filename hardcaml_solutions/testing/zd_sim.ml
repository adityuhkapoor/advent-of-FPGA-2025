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
;;

let () = testbench ()
  