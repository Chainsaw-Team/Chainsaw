package require ::quartus::project

## 覆写project
project_new -overwrite -revision Acq250Top Acq250Top

## project配置

# 指定device
set_global_assignment -name FAMILY "Cyclone V"
set_global_assignment -name DEVICE 5CGXFC9D6F27I7
set_global_assignment -name LAST_QUARTUS_VERSION "18.0.0 Standard Edition"

# 指定输出文件夹
set_global_assignment -name PROJECT_OUTPUT_DIRECTORY output_files

# adding sources(verilog, ip, qsys, etc) to project

# all-in-one verilog top module
set_global_assignment -name VERILOG_FILE Acq250Top.v

# xillybus files
# from the generated files
set_global_assignment -name VERILOG_FILE xillybus.v
set_global_assignment -name QXP_FILE xillybus_core.qxp
# from the cycloneV demo
set_global_assignment -name VERILOG_FILE pcie_c5_4x.v
set_global_assignment -name QSYS_FILE pcie_reconfig.qsys

# timing constraint
set_global_assignment -name SDC_FILE Acq250.sdc

# signal tap
set_global_assignment -name USE_SIGNALTAP_FILE Acq250.stp

# clock from oscillator & reset

set_location_assignment PIN_R20 -to clk
set_instance_assignment -name IO_STANDARD LVDS -to clk
set_location_assignment PIN_P20 -to "clk(n)"
set_location_assignment PIN_Y24 -to rstn
set_instance_assignment -name IO_STANDARD "3.3-V LVCMOS" -to rstn

# PCIe
set_location_assignment PIN_U22 -to pcie_perstn
set_instance_assignment -name IO_STANDARD "3.3-V LVTTL" -to pcie_perstn
set_location_assignment PIN_V6 -to pcie_refclk
set_instance_assignment -name IO_STANDARD HCSL -to pcie_refclk

set_location_assignment PIN_V2 -to pcie_rx[3]
set_instance_assignment -name IO_STANDARD "1.5-V PCML" -to pcie_rx[3]
set_location_assignment PIN_Y2 -to pcie_rx[2]
set_instance_assignment -name IO_STANDARD "1.5-V PCML" -to pcie_rx[2]
set_location_assignment PIN_AB2 -to pcie_rx[1]
set_instance_assignment -name IO_STANDARD "1.5-V PCML" -to pcie_rx[1]
set_location_assignment PIN_AD2 -to pcie_rx[0]
set_instance_assignment -name IO_STANDARD "1.5-V PCML" -to pcie_rx[0]
set_location_assignment PIN_V1 -to "pcie_rx[3](n)"
set_instance_assignment -name IO_STANDARD "1.5-V PCML" -to "pcie_rx[3](n)"
set_location_assignment PIN_Y1 -to "pcie_rx[2](n)"
set_instance_assignment -name IO_STANDARD "1.5-V PCML" -to "pcie_rx[2](n)"
set_location_assignment PIN_AB1 -to "pcie_rx[1](n)"
set_instance_assignment -name IO_STANDARD "1.5-V PCML" -to "pcie_rx[1](n)"
set_location_assignment PIN_AD1 -to "pcie_rx[0](n)"
set_instance_assignment -name IO_STANDARD "1.5-V PCML" -to "pcie_rx[0](n)"

set_location_assignment PIN_W4 -to pcie_tx[3]
set_instance_assignment -name IO_STANDARD "1.5-V PCML" -to pcie_tx[3]
set_location_assignment PIN_AA4 -to pcie_tx[2]
set_instance_assignment -name IO_STANDARD "1.5-V PCML" -to pcie_tx[2]
set_location_assignment PIN_AC4 -to pcie_tx[1]
set_instance_assignment -name IO_STANDARD "1.5-V PCML" -to pcie_tx[1]
set_location_assignment PIN_AE4 -to pcie_tx[0]
set_instance_assignment -name IO_STANDARD "1.5-V PCML" -to pcie_tx[0]
set_location_assignment PIN_W3 -to "pcie_tx[3](n)"
set_instance_assignment -name IO_STANDARD "1.5-V PCML" -to "pcie_tx[3](n)"
set_location_assignment PIN_AA3 -to "pcie_tx[2](n)"
set_instance_assignment -name IO_STANDARD "1.5-V PCML" -to "pcie_tx[2](n)"
set_location_assignment PIN_AC3 -to "pcie_tx[1](n)"
set_instance_assignment -name IO_STANDARD "1.5-V PCML" -to "pcie_tx[1](n)"
set_location_assignment PIN_AE3 -to "pcie_tx[0](n)"
set_instance_assignment -name IO_STANDARD "1.5-V PCML" -to "pcie_tx[0](n)"

# unused PCIe pins
set_location_assignment PIN_W20 -to PCIE_SMBCLK
set_instance_assignment -name IO_STANDARD "3.3-V LVTTL" -to PCIE_SMBCLK
set_location_assignment PIN_W21 -to PCIE_SMBDAT
set_instance_assignment -name IO_STANDARD "3.3-V LVTTL" -to PCIE_SMBDAT
set_location_assignment PIN_Y23 -to PCIE_WAKE_n
set_instance_assignment -name IO_STANDARD "3.3-V LVTTL" -to PCIE_WAKE_n

####################
# DDS chip
####################

set_location_assignment PIN_U19 -to AD9959_csn
set_instance_assignment -name IO_STANDARD "3.3-V LVCMOS" -to AD9959_csn
set_location_assignment PIN_R9 -to AD9959_rst
set_instance_assignment -name IO_STANDARD "3.3-V LVCMOS" -to AD9959_rst
set_location_assignment PIN_P8 -to AD9959_sclk
set_instance_assignment -name IO_STANDARD "3.3-V LVCMOS" -to AD9959_sclk
set_location_assignment PIN_AB24 -to AD9959_io_update
set_instance_assignment -name IO_STANDARD "3.3-V LVCMOS" -to AD9959_io_update

set_location_assignment PIN_T7 -to AD9959_p[3]
set_instance_assignment -name IO_STANDARD "3.3-V LVCMOS" -to AD9959_p[3]
set_location_assignment PIN_T8 -to AD9959_p[2]
set_instance_assignment -name IO_STANDARD "3.3-V LVCMOS" -to AD9959_p[2]
set_location_assignment PIN_V8 -to AD9959_p[1]
set_instance_assignment -name IO_STANDARD "3.3-V LVCMOS" -to AD9959_p[1]
set_location_assignment PIN_R8 -to AD9959_p[0]
set_instance_assignment -name IO_STANDARD "3.3-V LVCMOS" -to AD9959_p[0]

set_location_assignment PIN_AC24 -to AD9959_sdio[3]
set_instance_assignment -name IO_STANDARD "3.3-V LVCMOS" -to AD9959_sdio[3]
set_location_assignment PIN_AA23 -to AD9959_sdio[2]
set_instance_assignment -name IO_STANDARD "3.3-V LVCMOS" -to AD9959_sdio[2]
set_location_assignment PIN_AA22 -to AD9959_sdio[1]
set_instance_assignment -name IO_STANDARD "3.3-V LVCMOS" -to AD9959_sdio[1]
set_location_assignment PIN_V20 -to AD9959_sdio[0]
set_instance_assignment -name IO_STANDARD "3.3-V LVCMOS" -to AD9959_sdio[0]
set_instance_assignment -name IO_STANDARD "3.3-V LVCMOS" -to "AD9959_sdio[3](n)"

# following clk signals are generated by the AD9959
set_location_assignment PIN_K25 -to adc_clk
set_instance_assignment -name IO_STANDARD LVDS -to adc_clk
set_location_assignment PIN_K26 -to "adc_clk(n)"
set_instance_assignment -name IO_STANDARD LVDS -to "adc_clk(n)"
set_location_assignment PIN_T21 -to data_clk
set_instance_assignment -name IO_STANDARD LVDS -to data_clk
set_location_assignment PIN_T22 -to "data_clk(n)"
set_instance_assignment -name IO_STANDARD LVDS -to "data_clk(n)"

####################
# adc data in
####################


####################
# sma output
####################

set_location_assignment PIN_F26 -to sma0p
set_location_assignment PIN_Y25 -to sma1p
set_location_assignment PIN_G26 -to sma0n
set_location_assignment PIN_Y26 -to sma1n
set_instance_assignment -name IO_STANDARD "2.5 V" -to sma0p
set_instance_assignment -name IO_STANDARD "2.5 V" -to sma0n
set_instance_assignment -name IO_STANDARD "2.5 V" -to sma1p
set_instance_assignment -name IO_STANDARD "2.5 V" -to sma1n

set_instance_assignment -name SLEW_RATE 0 -to sma0p
set_instance_assignment -name SLEW_RATE 0 -to sma0n

####################
# gain control
####################

set_location_assignment PIN_R10 -to gain[5]
set_instance_assignment -name IO_STANDARD "3.3-V LVCMOS" -to gain[5]
set_location_assignment PIN_Y9 -to gain[4]
set_instance_assignment -name IO_STANDARD "3.3-V LVCMOS" -to gain[4]
set_location_assignment PIN_Y8 -to gain[3]
set_instance_assignment -name IO_STANDARD "3.3-V LVCMOS" -to gain[3]
set_location_assignment PIN_AA7 -to gain[2]
set_instance_assignment -name IO_STANDARD "3.3-V LVCMOS" -to gain[2]
set_location_assignment PIN_AB6 -to gain[1]
set_instance_assignment -name IO_STANDARD "3.3-V LVCMOS" -to gain[1]
set_location_assignment PIN_AA6 -to gain[0]
set_instance_assignment -name IO_STANDARD "3.3-V LVCMOS" -to gain[0]

####################
# timing constraints
####################

# set_instance_assignment -name PLL_COMPENSATION_MODE SOURCE_SYNCHRONOUS -to adc_clk

project_close