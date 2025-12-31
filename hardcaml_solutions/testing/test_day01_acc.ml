open! Core
open! Hardcaml
open! Hardcaml_waveterm

module Simulator = Cyclesim.With_interface(Day01.Accumulator.I)(Day01.Accumulator.O)

let testbench() = 
  let scope = Scope.create ~flatten_design:true () in
  let sim = Simulator.create (Day01.Accumulator.create scope) in
  let inputs = Cyclesim.inputs sim in
  let outputs = Cyclesim.outputs sim in
  let step ~reset ~inc ~expect_count =
    inputs.reset := if reset = 1 then Bits.vdd else Bits.gnd;
    inputs.inc   := if inc   = 1 then Bits.vdd else Bits.gnd;
    Cyclesim.cycle sim;
    let got = Bits.to_int !(outputs.count) in
    if got <> expect_count then
      failwithf
        "Mismatch: reset=%d inc=%d expected=%d got=%d"
        reset inc expect_count got
        ()
  in
  step ~reset:0 ~inc:0 ~expect_count:0;
  step ~reset:0 ~inc:1 ~expect_count:1;
  step ~reset:0 ~inc:1 ~expect_count:2;
  step ~reset:1 ~inc:0 ~expect_count:0;
  step ~reset:0 ~inc:0 ~expect_count:0;
  step ~reset:0 ~inc:0 ~expect_count:0;
  
  
  step ~reset:1 ~inc:0 ~expect_count:0;

  for n = 1 to 990 do
    step ~reset:0 ~inc:1 ~expect_count:n
  done
;;

let () = testbench ()