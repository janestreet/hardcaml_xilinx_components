open Core
open Xilinx_vhdl_generator

let commands =
  Command.group
    ~summary:"Parse Xilinx VHDL components declarations"
    [ "generate", Generator.command_generator
    ; "list", Generator.command_list_modules
    ; "versioned", Versioned_generator.command
    ]
;;

let () = Command_unix.run commands
