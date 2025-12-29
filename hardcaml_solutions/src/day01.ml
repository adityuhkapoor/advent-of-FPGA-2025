open! Core
open! Hardcaml
open! Signal

module Zero_detector = struct
  module I = struct
    type 'a t =
      { clk    : 'a
      ; reset  : 'a
      ; valid  : 'a
      ; amount : 'a [@bits 7]
      }
    [@@deriving sexp_of, hardcaml]
  end

  module O = struct
    type 'a t =
      { is_zero    : 'a
      ; next_state : 'a [@bits 9]
      }
    [@@deriving sexp_of, hardcaml]
  end

  let ge_sub_once a b = mux2 (a <: b) a (a -: b)

  let mod_100_upto_600 a =
    let w = width a in
    let k n = of_int ~width:w n in
    let a0 = ge_sub_once a  (k 500) in
    let a1 = ge_sub_once a0 (k 400) in
    let a2 = ge_sub_once a1 (k 200) in
    let a3 = ge_sub_once a2 (k 100) in
    a3
  ;;

  let create (_scope : Scope.t) (i : _ I.t) : _ O.t =
    let state_w = 9 in
    let r =
      Reg_spec.override
        (Reg_spec.create ~clock:i.clk ())
        ~reset:i.reset
        ~reset_to:(of_int ~width:state_w 50)
    in

    let amount_9 = uresize i.amount state_w in

    let state = wire state_w in

    let next_state_comb = mod_100_upto_600 (state +: amount_9) in

    let d = mux2 i.valid next_state_comb state in
    state <== reg r d;

    let is_zero = i.valid &: (next_state_comb ==: zero state_w) in
    { O.is_zero; next_state = next_state_comb }
end


module Accumulator = struct
  let width = 16

  module I = struct
    type 'a t =
      { clk   : 'a
      ; reset : 'a
      ; inc   : 'a
      }
    [@@deriving sexp_of, hardcaml]
  end

  module O = struct
    type 'a t = { count : 'a [@bits width] }
    [@@deriving sexp_of, hardcaml]
  end

  let create (_scope : Scope.t) (i : _ I.t) = 
    { O.count =
      reg_fb 
        (Reg_spec.create ~clock:i.clk ~clear:i.reset ())
        ~enable:i.inc
        ~width:width
        ~f:(fun d -> d +:. 1)
    }
  ;;
end

module Top = struct
  module I = struct
    type 'a t =
      { clk    : 'a
      ; reset  : 'a
      ; valid  : 'a
      ; amount : 'a [@bits 7]
      }
    [@@deriving sexp_of, hardcaml]
  end

  module O = struct
    type 'a t =
      { is_zero    : 'a
      ; next_state : 'a [@bits 9]
      ; count      : 'a [@bits 16]
      }
    [@@deriving sexp_of, hardcaml]
  end

  let create (scope : Scope.t) (i : _ I.t) : _ O.t =
    let zd =
      Zero_detector.create scope
        { Zero_detector.I.clk = i.clk
        ; reset  = i.reset
        ; valid  = i.valid
        ; amount = i.amount
        }
    in
    let acc =
      Accumulator.create scope
        { Accumulator.I.clk = i.clk
        ; reset = i.reset
        ; inc   = zd.is_zero
        }
    in
    { O.is_zero = zd.is_zero
    ; next_state = zd.next_state
    ; count = acc.count
    }
end
