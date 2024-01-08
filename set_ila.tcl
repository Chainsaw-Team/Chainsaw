create_debug_core u_ila_0 ila
set_property C_DATA_DEPTH 1024 [get_debug_cores u_ila_0]
set_property C_TRIGIN_EN false [get_debug_cores u_ila_0]
set_property C_TRIGOUT_EN false [get_debug_cores u_ila_0]
set_property C_ADV_TRIGGER false [get_debug_cores u_ila_0]
set_property C_INPUT_PIPE_STAGES 1 [get_debug_cores u_ila_0]
set_property C_EN_STRG_QUAL false [get_debug_cores u_ila_0]
set_property ALL_PROBE_SAME_MU true [get_debug_cores u_ila_0]
set_property ALL_PROBE_SAME_MU_CNT 1 [get_debug_cores u_ila_0]
startgroup 
set_property C_EN_STRG_QUAL true [get_debug_cores u_ila_0 ]
set_property ALL_PROBE_SAME_MU true [get_debug_cores u_ila_0 ]
set_property ALL_PROBE_SAME_MU_CNT 2 [get_debug_cores u_ila_0 ]
endgroup
connect_debug_port u_ila_0/clk [get_nets [list xillybusWrapper_1/xillybus_1/pcie/inst/pcie_uplus_gt_top_i/diablo_gt.diablo_gt_phy_wrapper/phy_clk_i/PHY_CORECLK ]]
set_property port_width 32 [get_debug_ports u_ila_0/probe0]
set_property PROBE_TYPE DATA_AND_TRIGGER [get_debug_ports u_ila_0/probe0]
connect_debug_port u_ila_0/probe0 [get_nets [list {xillybusWrapper_1/streamToHost_0_payload[0]} {xillybusWrapper_1/streamToHost_0_payload[1]} {xillybusWrapper_1/streamToHost_0_payload[2]} {xillybusWrapper_1/streamToHost_0_payload[3]} {xillybusWrapper_1/streamToHost_0_payload[4]} {xillybusWrapper_1/streamToHost_0_payload[5]} {xillybusWrapper_1/streamToHost_0_payload[6]} {xillybusWrapper_1/streamToHost_0_payload[7]} {xillybusWrapper_1/streamToHost_0_payload[8]} {xillybusWrapper_1/streamToHost_0_payload[9]} {xillybusWrapper_1/streamToHost_0_payload[10]} {xillybusWrapper_1/streamToHost_0_payload[11]} {xillybusWrapper_1/streamToHost_0_payload[12]} {xillybusWrapper_1/streamToHost_0_payload[13]} {xillybusWrapper_1/streamToHost_0_payload[14]} {xillybusWrapper_1/streamToHost_0_payload[15]} {xillybusWrapper_1/streamToHost_0_payload[16]} {xillybusWrapper_1/streamToHost_0_payload[17]} {xillybusWrapper_1/streamToHost_0_payload[18]} {xillybusWrapper_1/streamToHost_0_payload[19]} {xillybusWrapper_1/streamToHost_0_payload[20]} {xillybusWrapper_1/streamToHost_0_payload[21]} {xillybusWrapper_1/streamToHost_0_payload[22]} {xillybusWrapper_1/streamToHost_0_payload[23]} {xillybusWrapper_1/streamToHost_0_payload[24]} {xillybusWrapper_1/streamToHost_0_payload[25]} {xillybusWrapper_1/streamToHost_0_payload[26]} {xillybusWrapper_1/streamToHost_0_payload[27]} {xillybusWrapper_1/streamToHost_0_payload[28]} {xillybusWrapper_1/streamToHost_0_payload[29]} {xillybusWrapper_1/streamToHost_0_payload[30]} {xillybusWrapper_1/streamToHost_0_payload[31]} ]]
create_debug_port u_ila_0 probe
set_property port_width 1 [get_debug_ports u_ila_0/probe1]
set_property PROBE_TYPE DATA_AND_TRIGGER [get_debug_ports u_ila_0/probe1]
connect_debug_port u_ila_0/probe1 [get_nets [list xillybusWrapper_1/streamToHost_0_ready ]]
create_debug_port u_ila_0 probe
set_property port_width 1 [get_debug_ports u_ila_0/probe2]
set_property PROBE_TYPE DATA_AND_TRIGGER [get_debug_ports u_ila_0/probe2]
connect_debug_port u_ila_0/probe2 [get_nets [list xillybusWrapper_1/streamToHost_0_valid ]]