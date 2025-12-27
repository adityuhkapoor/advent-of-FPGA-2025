open! Core
open! Hardcaml
open! Signal

module Zero_detector = struct
  module I = struct
    type 'a t = {
      clk: 'a;
      reset: 'a;
      enable: 'a;
      norm_amount: 'a [@bits 7]
    } [@@deriving hardcaml]
  end
  module O = struct
    type 'a t = {
      is_zero: 'a
    } [@@deriving hardcaml]
  end

  let create (_scope : Scope.t)
  ({
    clk;
    reset;
    enable;
    norm_amount;
  }: _ I.t): _ O.t =
    let open Always in
    let spec = Reg_spec.create ~clock:clk () in 
    (* defines common register template for future registers 
    to come using common clock assumption*)

    let state = Variable.reg spec ~width:7 in
    let zero_sig = Variable.reg spec ~width:1 in

    let sum = (uresize state.value ~width:8) +: (uresize norm_amount ~width:8) in
    (* Potential cases where state + next state >= 100. To work around this for part 1,
    we take the sum in a separate variable of width 8. If greater than >= 100, sum = sum- 100*)
    let ge_100 = sum >: (of_int ~width:8 99) in
    let next_state_8 = mux2 ge_100 (sum -: (of_int ~width:8 100)) sum in
    let next_state = uresize next_state_8 ~width:7 in

    compile
    [
      zero_sig <-- gnd;
      if_ reset [
        state <--. 50;
        zero_sig <-- gnd;
      ]
      [
        when_ enable [ when_ (next_state ==:. 0) [ zero_sig <-- vdd ]
              ; state <-- next_state
              ]
        ]
      ];

      {
        is_zero = zero_sig.value
      }

;;
end
    
module Part_one_input_normalizer = struct
  module I = struct
    type 'a t = { 
      is_r : 'a; 
      amount : 'a [@bits 12]
       } [@@deriving hardcaml]
  end

  module O = struct
    type 'a t = { 
      norm_amount : 'a [@bits 7] 
      } [@@deriving hardcaml]
  end

  let create (_scope : Scope.t) ({ 
    is_r; 
    amount
     } : _ I.t) : _ O.t =
    let open Always in

    let amount_sig = Variable.wire ~default:gnd ~width:7 in

    let sub_if_ge x c =
      let ge = ~:(x <:. c) in
      mux2 ge (x -: of_int ~width:(width x) c) x
    in

    let amount_mod100_12 =
      amount
      |> fun x -> sub_if_ge x 3200
      |> fun x -> sub_if_ge x 1600
      |> fun x -> sub_if_ge x 800
      |> fun x -> sub_if_ge x 400
      |> fun x -> sub_if_ge x 200
      |> fun x -> sub_if_ge x 100
    in
    let amount_mod100 = uresize amount_mod100_12 ~width:7 in

    let left_as_add =
      mux2
        (amount_mod100 ==:. 0)
        (of_int ~width:7 0)
        ((of_int ~width:7 100) -: amount_mod100)
    in

    compile
      [
        amount_sig <-- amount_mod100;
        if_ is_r
          [ amount_sig <-- amount_mod100 ]
          [ amount_sig <-- left_as_add ];
      ];

    { norm_amount = amount_sig.value }
end

  module I = struct
    type 'a t = {
      is_r: 'a;
      amount: 'a [@bits 12]
    } [@@deriving hardcaml]
  end
  module O = struct
    type 'a t = {
      norm_amount: 'a [@bits 7]
    } [@@deriving hardcaml]
  end
  let create (_scope : Scope.t)
  ({
    is_r;
    amount
  }: _ I.t): _ O.t =
    let open Always in

    let sub_if_ge x c = 
      let ge = ~:(x <:. c) in
      mux2 ge (x -: of_int ~width:(width x) c) x
    in (* helper function to reduce amount modulo 100, assumption being made that
    max input amount is < 2^12 *)

    let amount_mod100_12 =  amount
  |> fun x -> sub_if_ge x 3200
  |> fun x -> sub_if_ge x 1600
  |> fun x -> sub_if_ge x 800
  |> fun x -> sub_if_ge x 400
  |> fun x -> sub_if_ge x 200
  |> fun x -> sub_if_ge x 100
  in
  let amount_mod100 = uresize amount_mod100_12 ~width:7 in

    let left_as_add =
    mux2
      (amount_mod100 ==:. 0)
      (of_int ~width:7 0)
      ((of_int ~width:7 100) -: amount_mod100)
    in

    compile
      [
        amount_sig <-- amount_mod100;
        if_ is_r
          [ amount_sig <-- amount_mod100 ]
          [ amount_sig <-- left_as_add ];
      ];
  { norm_amount = amount_sig.value }