mkdir -p src/main/resources/fp32
cd src/main/resources/fp32

flopoco="/home/ltr/flopoco/build/flopoco"

$flopoco outputFile=ieee2flopoco.vhdl frequency=10 InputIEEE wEIn=8 wFIn=23 wEOut=8 wFOut=23
$flopoco outputFile=flopoco2ieee.vhdl frequency=10 OutputIEEE wEIn=8 wFIn=23 wEOut=8 wFOut=23
$flopoco outputFile=singlePrecisionAdd.vhdl frequency=10 FPAdd wE=8 wF=23

# convert vhdl to verilog

ghdl -a -fsynopsys -fexplicit singlePrecisionAdd.vhdl
ghdl synth -fsynopsys -fexplicit --out=verilog FPAdd_8_23_F10_uid2 > singlePrecisionAdd.v

ghdl -a -fsynopsys -fexplicit flopoco2ieee.vhdl
ghdl synth -fsynopsys -fexplicit --out=verilog OutputIEEE_8_23_to_8_23 > flopoco2ieee.v

ghdl -a -fsynopsys -fexplicit ieee2flopoco.vhdl
ghdl synth -fsynopsys -fexplicit --out=verilog InputIEEE_8_23_to_8_23 > ieee2flopoco.v