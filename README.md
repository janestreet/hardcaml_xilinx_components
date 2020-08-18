"hardcaml_xilinx_components"
============================

`hardcaml_xilinx_components` contains an incomplete vhdl parser that
is able to parse the component definitions for the Xilinx Unisim and
XPM libraries.

An application is provided which can generate Hardcaml interfaces
for specific Xilinx components from the library definitions.

To use the application pass a (list of) filters for the module names
you want included and reference the required VHDL file ie

```
$ hardcaml_xilinx_components.exe generate \
   -filter "LUT" -filter "RAMB" \
   -vhdl /opt/xilinx/Vivado/2019.1/data/vhdl/src/unisims/unisim_VCOMP.vhd
   > xilinx_components.ml
```

If no filters are specified, all components are output. If no vhdl is
specified, stdin is used.

It has been tested with Vivado versions 2016.4 upto 2019.1.
