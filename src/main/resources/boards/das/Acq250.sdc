####################
# creating clocks
####################
create_clock -period 8.0 -name data_clk [get_ports data_clk]
create_clock -period 10.0 -name clk [get_ports clk]
create_clock -period 128.0 -name dds_clk [get_nets {pcieArea_ddsCtrl_sclkReg}]
create_clock -period 4.0 -name adc_clk [get_ports adc_clk]
create_clock -period 10.0 -name pcie_clk [get_ports pcie_refclk]
create_clock -name pcie_data_clk -period 8.000 -waveform {0.000 4.000} { xillybus_1|xillybus_1|pcie|pcie_reconfig|pcie|altpcie_av_hip_ast_hwtcl|altpcie_av_hip_128bit_atom|g_cavhip.arriav_hd_altpe2_hip_top|coreclkout }
create_clock -name lvds_clk -period 16.000 -waveform {0.000 8.000} { lvdsDebug_1|lvds_pll_ADCA|lvdspll_inst|altera_pll_i|general[2].gpll~PLL_OUTPUT_COUNTER|divclk }


# Automatically constrain PLL and other generated clocks
derive_pll_clocks -create_base_clocks

# Automatically calculate clock uncertainty to jitter and other effects.
derive_clock_uncertainty

####################
# I/O false path
####################
set_false_path -from [get_ports pcie_perstn]
set_false_path -from [get_ports rstn]
# set_false_path -to [get_ports user_led[*]]

####################
# inter-clock false path
####################

# data processing clock domain -> PCIe,connected by StreamFifoCC
set_false_path -from [get_clocks data_clk] -to [get_clocks pcie_clk]
set_false_path -from [get_clocks pcie_clk] -to [get_clocks data_clk]
# PCIe sampling 1-bit from a slower clock domain(dds_clk), ignore
set_false_path -from [get_clocks dds_clk] -to [get_clocks pcie_data_clk]
set_false_path -from [get_clocks pcie_data_clk] -to [get_clocks dds_clk]
# LVDS <-> data processing clock domain,connected by StreamFifoCC
set_false_path -from [get_clocks {lvds_clk}] -to [get_clocks {data_clk}]
set_false_path -from [get_clocks {data_clk}] -to [get_clocks {data_clk}]

# adc input delay

#set_input_delay   -clock [get_clocks {Adc_clk}] -max 0.55 [get_ports {adc_a[0]}] -add_delay
#set_input_delay   -clock [get_clocks {Adc_clk}] -min -0.55 [get_ports {adc_a[0]}] -add_delay
#set_input_delay   -clock [get_clocks {Adc_clk}] -max 0.55 [get_ports {adc_a[1]}] -add_delay
#set_input_delay   -clock [get_clocks {Adc_clk}] -min -0.55 [get_ports {adc_a[1]}] -add_delay
#set_input_delay   -clock [get_clocks {Adc_clk}] -max 0.55 [get_ports {adc_a[2]}] -add_delay
#set_input_delay   -clock [get_clocks {Adc_clk}] -min -0.55 [get_ports {adc_a[2]}] -add_delay
#set_input_delay   -clock [get_clocks {Adc_clk}] -max 0.55 [get_ports {adc_a[3]}] -add_delay
#set_input_delay   -clock [get_clocks {Adc_clk}] -min -0.55 [get_ports {adc_a[3]}] -add_delay
#set_input_delay   -clock [get_clocks {Adc_clk}] -max 0.55 [get_ports {adc_a[4]}] -add_delay
#set_input_delay   -clock [get_clocks {Adc_clk}] -min -0.55 [get_ports {adc_a[4]}] -add_delay
#set_input_delay   -clock [get_clocks {Adc_clk}] -max 0.55 [get_ports {adc_a[5]}] -add_delay
#set_input_delay   -clock [get_clocks {Adc_clk}] -min -0.55 [get_ports {adc_a[5]}] -add_delay
#set_input_delay   -clock [get_clocks {Adc_clk}] -max 0.55 [get_ports {adc_a[6]}] -add_delay
#set_input_delay   -clock [get_clocks {Adc_clk}] -min -0.55 [get_ports {adc_a[6]}] -add_delay

#set_input_delay   -clock [get_clocks {Adc_clk}] -max 0.55 [get_ports {adc_b[0]}] -add_delay
#set_input_delay   -clock [get_clocks {Adc_clk}] -min -0.55 [get_ports {adc_b[0]}] -add_delay
#set_input_delay   -clock [get_clocks {Adc_clk}] -max 0.55 [get_ports {adc_b[1]}] -add_delay
#set_input_delay   -clock [get_clocks {Adc_clk}] -min -0.55 [get_ports {adc_b[1]}] -add_delay
#set_input_delay   -clock [get_clocks {Adc_clk}] -max 0.55 [get_ports {adc_b[2]}] -add_delay
#set_input_delay   -clock [get_clocks {Adc_clk}] -min -0.55 [get_ports {adc_b[2]}] -add_delay
#set_input_delay   -clock [get_clocks {Adc_clk}] -max 0.55 [get_ports {adc_b[3]}] -add_delay
#set_input_delay   -clock [get_clocks {Adc_clk}] -min -0.55 [get_ports {adc_b[3]}] -add_delay
#set_input_delay   -clock [get_clocks {Adc_clk}] -max 0.55 [get_ports {adc_b[4]}] -add_delay
#set_input_delay   -clock [get_clocks {Adc_clk}] -min -0.55 [get_ports {adc_b[4]}] -add_delay
#set_input_delay   -clock [get_clocks {Adc_clk}] -max 0.55 [get_ports {adc_b[5]}] -add_delay
#set_input_delay   -clock [get_clocks {Adc_clk}] -min -0.55 [get_ports {adc_b[5]}] -add_delay
#set_input_delay   -clock [get_clocks {Adc_clk}] -max 0.55 [get_ports {adc_b[6]}] -add_delay
#set_input_delay   -clock [get_clocks {Adc_clk}] -min -0.55 [get_ports {adc_b[6]}] -add_delay