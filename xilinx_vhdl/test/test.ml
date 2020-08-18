open! Import

(* This is required by the (stupid) parser *)
let package s = Printf.sprintf {|
      library IEEE;
      use IEEE.STD_LOGIC_1164.all;
      package TOP is
        %s
      end TOP;
  |} s

(* Parse VHDL and print as a sexp *)
let print_vhdl string =
  print_s [%message
    "" ~_:(Xilinx_vhdl.Parse.parse_string string : Vhdl.Component.t list Or_error.t)]

(* Tests *)

let%expect_test "empty file" =
  print_vhdl "";
  [%expect {| (Error ("parse error" (at_line 1))) |}]

let%expect_test "empty package" =
  print_vhdl (package "");
  [%expect {| (Ok ()) |}]

let%expect_test "simplest component" =
  print_vhdl (package "component foo end component;");
  [%expect {|
    (Ok ((
      (name foo)
      (generics ())
      (ports    ())))) |}]

let%expect_test "2 componenents" =
  print_vhdl @@ package @@
  {|
  component a end component;
  component b end component;
  |};
  [%expect {|
    (Ok (
      ((name a) (generics ()) (ports ()))
      ((name b) (generics ()) (ports ())))) |}]

let%expect_test "comment" =
  print_vhdl @@ package @@
  {|
  -- this is a
  component a end component;
  -- this is b
  component b end component;
  |};
  [%expect {|
    (Ok (
      ((name a) (generics ()) (ports ()))
      ((name b) (generics ()) (ports ())))) |}]

let%expect_test "bit port types" =
  print_vhdl @@ package @@
  {|
  component a
    port (
      a : in std_logic := '1';
      b : out std_ulogic := 'H';
      c : in bit
    );
  end component;
  |};
  [%expect {|
    (Ok ((
      (name a)
      (generics ())
      (ports (
        ((name  a)
         (dir   In)
         (type_ Std_logic)
         (default ((Char 1))))
        ((name  b)
         (dir   Out)
         (type_ Std_logic)
         (default ((Char H))))
        ((name  c)
         (dir   In)
         (type_ Bit)
         (default ()))))))) |}]

let%expect_test "vector port types" =
  print_vhdl @@ package @@
  {|
  component a
    port (
      a : in std_logic_vector;
      b : out bit_vector
    );
  end component;
  |};
  [%expect {|
    (Ok ((
      (name a)
      (generics ())
      (ports (
        ((name a)
         (dir  In)
         (type_ (Std_logic_vector ()))
         (default ()))
        ((name b)
         (dir  Out)
         (type_ (Bit_vector ()))
         (default ()))))))) |}]


let%expect_test "other port types" =
  print_vhdl @@ package @@
  {|
  component a
    port (
      a : in integer;
      b : out real;
      c : out boolean;
      d : out string
    );
  end component;
  |};
  [%expect {|
    (Ok ((
      (name a)
      (generics ())
      (ports (
        ((name  a)
         (dir   In)
         (type_ Integer)
         (default ()))
        ((name  b)
         (dir   Out)
         (type_ Real)
         (default ()))
        ((name  c)
         (dir   Out)
         (type_ Boolean)
         (default ()))
        ((name  d)
         (dir   Out)
         (type_ String)
         (default ()))))))) |}]

let%expect_test "generics" =
  print_vhdl @@ package @@
  {|
  component a
    generic (
      a : in integer := 0;
      b : out real := 0.3;
      c : out boolean := false;
      d : out string
    );
  end component;
  |};
  [%expect {|
    (Ok ((
      (name a)
      (generics (
        ((name  a)
         (dir   In)
         (type_ Integer)
         (default ((Int 0))))
        ((name  b)
         (dir   Out)
         (type_ Real)
         (default ((Float 0.3))))
        ((name  c)
         (dir   Out)
         (type_ Boolean)
         (default ((Id false))))
        ((name  d)
         (dir   Out)
         (type_ String)
         (default ()))))
      (ports ())))) |}]

(* a few operator precedence tests *)

let%expect_test "precedence [+] [-] [&] same" =
  print_vhdl @@ package @@
  {| component a generic (a : in integer := a + b - c & d); end component; |};
  [%expect {|
    (Ok ((
      (name a)
      (generics ((
        (name  a)
        (dir   In)
        (type_ Integer)
        (default ((
          Op &
          (Op -
            (Op +
              (Id a)
              (Id b))
            (Id c))
          (Id d)))))))
      (ports ())))) |}]

let%expect_test "precedence [*] [/] same" =
  print_vhdl @@ package @@
  {| component a generic (a : in integer := a * b / c * d); end component; |};
  [%expect {|
    (Ok ((
      (name a)
      (generics ((
        (name  a)
        (dir   In)
        (type_ Integer)
        (default ((
          Op *
          (Op /
            (Op *
              (Id a)
              (Id b))
            (Id c))
          (Id d)))))))
      (ports ())))) |}]

let%expect_test "precedence [*] > [+]" =
  print_vhdl @@ package @@
  {| component a generic ( a : in integer := a + b * c
                         ; b : in integer := a * b + c); end component; |};
  [%expect {|
    (Ok ((
      (name a)
      (generics (
        ((name  a)
         (dir   In)
         (type_ Integer)
         (default ((
           Op +
           (Id a)
           (Op *
             (Id b)
             (Id c))))))
        ((name  b)
         (dir   In)
         (type_ Integer)
         (default ((
           Op +
           (Op *
             (Id a)
             (Id b))
           (Id c)))))))
      (ports ())))) |}]

let%expect_test "precedence [/] > [-]" =
  print_vhdl @@ package @@
  {| component a generic ( a : in integer := a - b / c
                         ; b : in integer := a / b - c); end component; |};
  [%expect {|
    (Ok ((
      (name a)
      (generics (
        ((name  a)
         (dir   In)
         (type_ Integer)
         (default ((
           Op -
           (Id a)
           (Op /
             (Id b)
             (Id c))))))
        ((name  b)
         (dir   In)
         (type_ Integer)
         (default ((
           Op -
           (Op /
             (Id a)
             (Id b))
           (Id c)))))))
      (ports ())))) |}]

(* A couple of components extracted from [unisim_VCOMP.vhd] (2017.4) *)
let%expect_test "LUT6" =
  print_vhdl @@ package @@
  {|
----- component LUT6 -----
component LUT6
  generic (
     INIT : bit_vector := X"0000000000000000"
  );
  port (
     O : out std_ulogic;
     I0 : in std_ulogic;
     I1 : in std_ulogic;
     I2 : in std_ulogic;
     I3 : in std_ulogic;
     I4 : in std_ulogic;
     I5 : in std_ulogic
  );
end component;
attribute BOX_TYPE of
  LUT6 : component is "PRIMITIVE";
  |};
  [%expect {|
    (Ok ((
      (name LUT6)
      (generics ((
        (name INIT)
        (dir  In)
        (type_ (Bit_vector ()))
        (default ((Hex 0000000000000000))))))
      (ports (
        ((name  O)
         (dir   Out)
         (type_ Std_logic)
         (default ()))
        ((name  I0)
         (dir   In)
         (type_ Std_logic)
         (default ()))
        ((name  I1)
         (dir   In)
         (type_ Std_logic)
         (default ()))
        ((name  I2)
         (dir   In)
         (type_ Std_logic)
         (default ()))
        ((name  I3)
         (dir   In)
         (type_ Std_logic)
         (default ()))
        ((name  I4)
         (dir   In)
         (type_ Std_logic)
         (default ()))
        ((name  I5)
         (dir   In)
         (type_ Std_logic)
         (default ()))))))) |}]

let%expect_test "DCM_ADV" =
  print_vhdl @@ package @@
  {|
----- component DCM_ADV -----
component DCM_ADV
  generic (
     CLKDV_DIVIDE : real := 2.0;
     CLKFX_DIVIDE : integer := 1;
     CLKFX_MULTIPLY : integer := 4;
     CLKIN_DIVIDE_BY_2 : boolean := false;
     CLKIN_PERIOD : real := 10.0;
     CLKOUT_PHASE_SHIFT : string := "NONE";
     CLK_FEEDBACK : string := "1X";
     DCM_AUTOCALIBRATION : boolean := true;
     DCM_PERFORMANCE_MODE : string := "MAX_SPEED";
     DESKEW_ADJUST : string := "SYSTEM_SYNCHRONOUS";
     DFS_FREQUENCY_MODE : string := "LOW";
     DLL_FREQUENCY_MODE : string := "LOW";
     DUTY_CYCLE_CORRECTION : boolean := true;
     FACTORY_JF : bit_vector := X"F0F0";
     PHASE_SHIFT : integer := 0;
     SIM_DEVICE : string := "VIRTEX4";
     STARTUP_WAIT : boolean := false
  );
  port (
     CLK0 : out std_ulogic := '0';
     CLK180 : out std_ulogic := '0';
     CLK270 : out std_ulogic := '0';
     CLK2X : out std_ulogic := '0';
     CLK2X180 : out std_ulogic := '0';
     CLK90 : out std_ulogic := '0';
     CLKDV : out std_ulogic := '0';
     CLKFX : out std_ulogic := '0';
     CLKFX180 : out std_ulogic := '0';
     DO : out std_logic_vector(15 downto 0) := "0000000000000000";
     DRDY : out std_ulogic := '0';
     LOCKED : out std_ulogic := '0';
     PSDONE : out std_ulogic := '0';
     CLKFB : in std_ulogic := '0';
     CLKIN : in std_ulogic := '0';
     DADDR : in std_logic_vector(6 downto 0) := "0000000";
     DCLK : in std_ulogic := '0';
     DEN : in std_ulogic := '0';
     DI : in std_logic_vector(15 downto 0) := "0000000000000000";
     DWE : in std_ulogic := '0';
     PSCLK : in std_ulogic := '0';
     PSEN : in std_ulogic := '0';
     PSINCDEC : in std_ulogic := '0';
     RST : in std_ulogic := '0'
  );
end component;
attribute BOX_TYPE of
  DCM_ADV : component is "PRIMITIVE";
  |};
  [%expect {|
    (Ok ((
      (name DCM_ADV)
      (generics (
        ((name  CLKDV_DIVIDE)
         (dir   In)
         (type_ Real)
         (default ((Float 2))))
        ((name  CLKFX_DIVIDE)
         (dir   In)
         (type_ Integer)
         (default ((Int 1))))
        ((name  CLKFX_MULTIPLY)
         (dir   In)
         (type_ Integer)
         (default ((Int 4))))
        ((name  CLKIN_DIVIDE_BY_2)
         (dir   In)
         (type_ Boolean)
         (default ((Id false))))
        ((name  CLKIN_PERIOD)
         (dir   In)
         (type_ Real)
         (default ((Float 10))))
        ((name  CLKOUT_PHASE_SHIFT)
         (dir   In)
         (type_ String)
         (default ((String NONE))))
        ((name  CLK_FEEDBACK)
         (dir   In)
         (type_ String)
         (default ((String 1X))))
        ((name  DCM_AUTOCALIBRATION)
         (dir   In)
         (type_ Boolean)
         (default ((Id true))))
        ((name  DCM_PERFORMANCE_MODE)
         (dir   In)
         (type_ String)
         (default ((String MAX_SPEED))))
        ((name  DESKEW_ADJUST)
         (dir   In)
         (type_ String)
         (default ((String SYSTEM_SYNCHRONOUS))))
        ((name  DFS_FREQUENCY_MODE)
         (dir   In)
         (type_ String)
         (default ((String LOW))))
        ((name  DLL_FREQUENCY_MODE)
         (dir   In)
         (type_ String)
         (default ((String LOW))))
        ((name  DUTY_CYCLE_CORRECTION)
         (dir   In)
         (type_ Boolean)
         (default ((Id true))))
        ((name FACTORY_JF)
         (dir  In)
         (type_ (Bit_vector ()))
         (default ((Hex F0F0))))
        ((name  PHASE_SHIFT)
         (dir   In)
         (type_ Integer)
         (default ((Int 0))))
        ((name  SIM_DEVICE)
         (dir   In)
         (type_ String)
         (default ((String VIRTEX4))))
        ((name  STARTUP_WAIT)
         (dir   In)
         (type_ Boolean)
         (default ((Id false))))))
      (ports (
        ((name  CLK0)
         (dir   Out)
         (type_ Std_logic)
         (default ((Char 0))))
        ((name  CLK180)
         (dir   Out)
         (type_ Std_logic)
         (default ((Char 0))))
        ((name  CLK270)
         (dir   Out)
         (type_ Std_logic)
         (default ((Char 0))))
        ((name  CLK2X)
         (dir   Out)
         (type_ Std_logic)
         (default ((Char 0))))
        ((name  CLK2X180)
         (dir   Out)
         (type_ Std_logic)
         (default ((Char 0))))
        ((name  CLK90)
         (dir   Out)
         (type_ Std_logic)
         (default ((Char 0))))
        ((name  CLKDV)
         (dir   Out)
         (type_ Std_logic)
         (default ((Char 0))))
        ((name  CLKFX)
         (dir   Out)
         (type_ Std_logic)
         (default ((Char 0))))
        ((name  CLKFX180)
         (dir   Out)
         (type_ Std_logic)
         (default ((Char 0))))
        ((name DO)
         (dir  Out)
         (type_ (Std_logic_vector (((Int 15) Down (Int 0)))))
         (default ((String 0000000000000000))))
        ((name  DRDY)
         (dir   Out)
         (type_ Std_logic)
         (default ((Char 0))))
        ((name  LOCKED)
         (dir   Out)
         (type_ Std_logic)
         (default ((Char 0))))
        ((name  PSDONE)
         (dir   Out)
         (type_ Std_logic)
         (default ((Char 0))))
        ((name  CLKFB)
         (dir   In)
         (type_ Std_logic)
         (default ((Char 0))))
        ((name  CLKIN)
         (dir   In)
         (type_ Std_logic)
         (default ((Char 0))))
        ((name DADDR)
         (dir  In)
         (type_ (Std_logic_vector (((Int 6) Down (Int 0)))))
         (default ((String 0000000))))
        ((name  DCLK)
         (dir   In)
         (type_ Std_logic)
         (default ((Char 0))))
        ((name  DEN)
         (dir   In)
         (type_ Std_logic)
         (default ((Char 0))))
        ((name DI)
         (dir  In)
         (type_ (Std_logic_vector (((Int 15) Down (Int 0)))))
         (default ((String 0000000000000000))))
        ((name  DWE)
         (dir   In)
         (type_ Std_logic)
         (default ((Char 0))))
        ((name  PSCLK)
         (dir   In)
         (type_ Std_logic)
         (default ((Char 0))))
        ((name  PSEN)
         (dir   In)
         (type_ Std_logic)
         (default ((Char 0))))
        ((name  PSINCDEC)
         (dir   In)
         (type_ Std_logic)
         (default ((Char 0))))
        ((name  RST)
         (dir   In)
         (type_ Std_logic)
         (default ((Char 0))))))))) |}]
