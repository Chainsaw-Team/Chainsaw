create_clock -period 8.0 -name data_clk [get_ports data_clk]
create_clock -period 8.0 -name clk [get_ports clk]
create_clock -period 128.0 -name dds_clk [get_nets {pcieArea_ddsCtrl_sclkReg}]
create_clock -period 4.0 -name adc_clk [get_ports adc_clk]
create_clock -period 10.0 -name pcie_clk [get_ports pcie_refclk]

# PCIe <-> data processing clock domain
set_false_path -from [get_clocks data_clk] -to [get_clocks pcie_clk]
set_false_path -from [get_clocks pcie_clk] -to [get_clocks data_clk]

# adc <-> data processing clock domain




