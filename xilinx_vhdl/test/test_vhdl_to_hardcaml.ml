open! Import

let test component =
  print_endline (Vhdl_to_hardcaml.vhdl_component_to_ocaml_module component)
;;

let%expect_test "trivial component" =
  test { name = "component_name"; generics = []; ports = [] };
  [%expect
    {|
    module Component_name = struct
      module type P = sig

      end
      module P : P = struct

      end
      module Make (P : P) = struct
        let params = [

        ]
        module I = Hardcaml.Interface.Empty
        module O = Hardcaml.Interface.Empty
        module T = Hardcaml.Interface.Empty

        open struct include Hardcaml.Instantiation.With_interface(I)(O) end
        let create ?lib ?arch ?attributes ?instance ?(name="component_name") ?parameters inputs =
          let parameters = Option.value ~default:params parameters in
          create ?lib ?arch ?instance ?attributes ~parameters ~name inputs
      end
    end
    |}]
;;

let ports =
  List.map Vhdl.Port_direction.all ~f:(fun dir ->
    { Vhdl.Port.name = concat [ "port_"; dir |> Vhdl.Port_direction.to_string ]
    ; dir
    ; type_ = Bit
    ; default = None
    })
;;

let%expect_test "generics" =
  test { name = "component_name"; generics = ports; ports = [] };
  [%expect
    {|
    module Component_name = struct
      module type P = sig
        val port_in : Hardcaml.Logic.Std_logic.t
        val port_inout : Hardcaml.Logic.Std_logic.t
        val port_out : Hardcaml.Logic.Std_logic.t
      end
      module P : P = struct
        let port_in = Hardcaml.Logic.Std_logic.U
        let port_inout = Hardcaml.Logic.Std_logic.U
        let port_out = Hardcaml.Logic.Std_logic.U
      end
      module Make (P : P) = struct
        let params = [
          Hardcaml.Parameter.create ~name:"port_In" ~value:(Std_logic P.port_in);
          Hardcaml.Parameter.create ~name:"port_Inout" ~value:(Std_logic P.port_inout);
          Hardcaml.Parameter.create ~name:"port_Out" ~value:(Std_logic P.port_out);
        ]
        module I = Hardcaml.Interface.Empty
        module O = Hardcaml.Interface.Empty
        module T = Hardcaml.Interface.Empty

        open struct include Hardcaml.Instantiation.With_interface(I)(O) end
        let create ?lib ?arch ?attributes ?instance ?(name="component_name") ?parameters inputs =
          let parameters = Option.value ~default:params parameters in
          create ?lib ?arch ?instance ?attributes ~parameters ~name inputs
      end
    end
    |}]
;;

let%expect_test "ports" =
  test { name = "component_name"; generics = []; ports };
  [%expect
    {|
    module Component_name = struct
      module type P = sig

      end
      module P : P = struct

      end
      module Make (P : P) = struct
        let params = [

        ]

        module I = struct
          type 'a t = {
            port_in : 'a[@bits 1][@rtlname "port_In"];
          }[@@deriving hardcaml]
        end

        module O = struct
          type 'a t = {
            port_out : 'a[@bits 1][@rtlname "port_Out"];
          }[@@deriving hardcaml]
        end

        module T = struct
          type 'a t = {
            port_inout : 'a[@bits 1][@rtlname "port_Inout"];
          }[@@deriving hardcaml]
        end

        open struct include Hardcaml.Instantiation.With_interface(I)(O) end
        let create ?lib ?arch ?attributes ?instance ?(name="component_name") ?parameters inputs =
          let parameters = Option.value ~default:params parameters in
          create ?lib ?arch ?instance ?attributes ~parameters ~name inputs
      end
    end
    |}]
;;

let%expect_test "generic" =
  test
    { name = "component_name"
    ; generics =
        [ { name = "std_logic_vector_no_range"
          ; dir = In
          ; type_ = Std_logic_vector None
          ; default = Some (String "11011")
          }
        ; { name = "std_logic_Vector_with_range"
          ; dir = In
          ; type_ = Std_logic_vector (Some (Int 1, Down, Int 0))
          ; default = Some (String "10")
          }
        ; { name = "bit_vector"
          ; dir = In
          ; type_ = Bit_vector (Some (Int 1, Down, Int 0))
          ; default = Some (String "10")
          }
        ; { name = "booleans"; dir = In; type_ = Boolean; default = Some (Id "TRUE") }
        ]
    ; ports = []
    };
  [%expect
    {|
    module Component_name = struct
      module type P = sig
        val std_logic_vector_no_range : Hardcaml.Logic.Std_logic_vector.t
        val std_logic_vector_with_range : Hardcaml.Logic.Std_logic_vector.t
        val bit_vector : Hardcaml.Logic.Bit_vector.t
        val booleans : string
      end
      module P : P = struct
        let std_logic_vector_no_range = Hardcaml.Logic.Std_logic_vector.of_string"11011"
        let std_logic_vector_with_range = Hardcaml.Logic.Std_logic_vector.of_string"10"
        let bit_vector = Hardcaml.Logic.Bit_vector.of_string "10"
        let booleans = "TRUE"
      end
      module Make (P : P) = struct
        let params = [
          Hardcaml.Parameter.create ~name:"std_logic_vector_no_range" ~value:(Std_logic_vector P.std_logic_vector_no_range);
          Hardcaml.Parameter.create ~name:"std_logic_Vector_with_range" ~value:(Std_logic_vector P.std_logic_vector_with_range);
          Hardcaml.Parameter.create ~name:"bit_vector" ~value:(Bit_vector P.bit_vector);
          Hardcaml.Parameter.create ~name:"booleans" ~value:(String P.booleans);
        ]
        module I = Hardcaml.Interface.Empty
        module O = Hardcaml.Interface.Empty
        module T = Hardcaml.Interface.Empty

        open struct include Hardcaml.Instantiation.With_interface(I)(O) end
        let create ?lib ?arch ?attributes ?instance ?(name="component_name") ?parameters inputs =
          let parameters = Option.value ~default:params parameters in
          create ?lib ?arch ?instance ?attributes ~parameters ~name inputs
      end
    end
    |}]
;;
