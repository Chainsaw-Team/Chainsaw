// altera message_off 10230 10335
`timescale 1ns / 10ps

module xillybus(pcie_perstn, pcie_refclk, pcie_rx, pcie_tx, bus_clk, quiesce,
  user_led, user_r_read_32_rden, user_r_read_32_data, user_r_read_32_empty,
  user_r_read_32_eof, user_r_read_32_open, user_w_write_32_wren,
  user_w_write_32_data, user_w_write_32_full, user_w_write_32_open,
  user_r_read_8_rden, user_r_read_8_data, user_r_read_8_empty,
  user_r_read_8_eof, user_r_read_8_open, user_w_write_8_wren,
  user_w_write_8_data, user_w_write_8_full, user_w_write_8_open,
  user_r_mem_32_rden, user_r_mem_32_data, user_r_mem_32_empty,
  user_r_mem_32_eof, user_r_mem_32_open, user_w_mem_32_wren,
  user_w_mem_32_data, user_w_mem_32_full, user_w_mem_32_open, user_mem_32_addr,
  user_mem_32_addr_update);

  input  pcie_perstn;
  input  pcie_refclk;
  input [3:0] pcie_rx;
  input [31:0] user_r_read_32_data;
  input  user_r_read_32_empty;
  input  user_r_read_32_eof;
  input  user_w_write_32_full;
  input [7:0] user_r_read_8_data;
  input  user_r_read_8_empty;
  input  user_r_read_8_eof;
  input  user_w_write_8_full;
  input [31:0] user_r_mem_32_data;
  input  user_r_mem_32_empty;
  input  user_r_mem_32_eof;
  input  user_w_mem_32_full;
  output [3:0] pcie_tx;
  output  bus_clk;
  output  quiesce;
  output [3:0] user_led;
  output  user_r_read_32_rden;
  output  user_r_read_32_open;
  output  user_w_write_32_wren;
  output [31:0] user_w_write_32_data;
  output  user_w_write_32_open;
  output  user_r_read_8_rden;
  output  user_r_read_8_open;
  output  user_w_write_8_wren;
  output [7:0] user_w_write_8_data;
  output  user_w_write_8_open;
  output  user_r_mem_32_rden;
  output  user_r_mem_32_open;
  output  user_w_mem_32_wren;
  output [31:0] user_w_mem_32_data;
  output  user_w_mem_32_open;
  output [15:0] user_mem_32_addr;
  output  user_mem_32_addr_update;
  wire [35:0] tx_cred0;
  wire  tx_fifo_empty0;
  wire  tx_st_ready0;
  wire [63:0] tx_st_data0;
  wire  tx_st_eop0;
  wire  tx_st_err0;
  wire  tx_st_sop0;
  wire  tx_st_valid0;
  wire  rx_fifo_empty0;
  wire [7:0] rx_st_bardec0;
  wire [7:0] rx_st_be0;
  wire [63:0] rx_st_data0;
  wire  rx_st_eop0;
  wire  rx_st_err0;
  wire  rx_st_sop0;
  wire  rx_st_valid0;
  wire  rx_st_ready0;
  wire  rx_st_mask0;
  wire  trn_terr_drop_n;
  wire  app_msi_ack;
  wire  app_msi_req;
  wire [3:0] tl_cfg_add;
  wire [31:0] tl_cfg_ctl;
  wire [52:0] tl_cfg_sts;
  wire [7:0] ko_cpl_spc_header;
  wire [11:0] ko_cpl_spc_data;
  wire  recv_dma_idle;
  wire  core_clk_out;
  wire  pme_to_sr;
  wire  pld_clk;
  wire  reconfig_clk;
  wire  cpl_pending;
  wire [16:0] reconfig_fromgxb;
  wire [3:0] reconfig_togxb;
  wire  serdes_pll_locked;
  wire  pld_clk_inuse;

   assign 	       bus_clk = core_clk_out;
   assign 	       trn_terr_drop_n = 1; // Compatible with Xilinx

   assign 	       pld_clk = core_clk_out;
   assign 	       cpl_pending = !recv_dma_idle;

  pcie_c5_4x pcie
    (
     .app_int_ack (),
     .app_int_sts_vec (8'd0), // No legacy interrupts
     .app_msi_ack (app_msi_ack),
     .app_msi_num (5'd0),
     .app_msi_req (app_msi_req),
     .app_msi_tc (3'd0),
     .busy_xcvr_reconfig(1'b0), // Handled by Qsys internally
     .reconfig_from_xcvr(),
     .reconfig_to_xcvr(350'd0),
     .clk250_out (),
     .clk500_out (),
     .coreclkout (core_clk_out),
     .cpl_err (6'd0), // No errors reported at all
     .cpl_pending ({ 7'd0, cpl_pending}),
     .ko_cpl_spc_header(ko_cpl_spc_header),
     .ko_cpl_spc_data(ko_cpl_spc_data),
     .tl_hpg_ctrl_er (5'd0),
     .lane_act (),
     .lmi_ack (),
     .lmi_addr (12'd0),
     .lmi_din (32'd0),
     .lmi_dout (),
     .lmi_rden (1'b0),
     .lmi_wren (1'b0),
     .npor (pcie_perstn),
     .pin_perst(pcie_perstn),
     .pld_clk (pld_clk),
     .serdes_pll_locked(serdes_pll_locked),
     .pld_core_ready(serdes_pll_locked),
     .pld_clk_inuse(pld_clk_inuse),
     .pm_auxpwr (1'b0),
     .pm_data (10'd0), // No power consumption data
     .pm_event (1'b0),
     .pme_to_cr (pme_to_sr), // Shortcircuit req->ack as in example code
     .pme_to_sr (pme_to_sr),
     .refclk (pcie_refclk),
     .rx_in0 (pcie_rx[0]),
     .rx_in1 (pcie_rx[1]),
     .rx_in2 (pcie_rx[2]),
     .rx_in3 (pcie_rx[3]),
     .rx_st_bar (),
     .rx_st_mask(1'b0),
     .rx_st_data (rx_st_data0),
     .rx_st_eop (rx_st_eop0),
     .rx_st_err (rx_st_err0),
     .rx_st_ready (rx_st_ready0),
     .rx_st_sop (rx_st_sop0),
     .rx_st_valid (rx_st_valid0),
     .test_in (40'd0),
     .tl_cfg_add (tl_cfg_add),
     .tl_cfg_ctl (tl_cfg_ctl),
     .tl_cfg_sts (tl_cfg_sts),
     .tx_out0 (pcie_tx[0]),
     .tx_out1 (pcie_tx[1]),
     .tx_out2 (pcie_tx[2]),
     .tx_out3 (pcie_tx[3]),
     .tx_st_data (tx_st_data0),
     .tx_st_eop (tx_st_eop0),
     .tx_st_err (tx_st_err0),
     .tx_st_ready (tx_st_ready0),
     .tx_st_sop (tx_st_sop0),
     .tx_st_valid (tx_st_valid0)
     );

  xillybus_core  xillybus_core_ins(.tx_fifo_empty0_w(tx_fifo_empty0),
    .tx_st_ready0_w(tx_st_ready0), .tx_st_data0_w(tx_st_data0),
    .tx_st_eop0_w(tx_st_eop0), .tx_st_err0_w(tx_st_err0),
    .tx_st_sop0_w(tx_st_sop0), .tx_st_valid0_w(tx_st_valid0),
    .rx_fifo_empty0_w(rx_fifo_empty0), .rx_st_bardec0_w(rx_st_bardec0),
    .rx_st_be0_w(rx_st_be0), .pcie_perstn_w(pcie_perstn),
    .rx_st_data0_w(rx_st_data0), .rx_st_eop0_w(rx_st_eop0),
    .rx_st_err0_w(rx_st_err0), .rx_st_sop0_w(rx_st_sop0),
    .rx_st_valid0_w(rx_st_valid0), .rx_st_ready0_w(rx_st_ready0),
    .rx_st_mask0_w(rx_st_mask0), .trn_terr_drop_n_w(trn_terr_drop_n),
    .app_msi_ack_w(app_msi_ack), .app_msi_req_w(app_msi_req),
    .tl_cfg_add_w(tl_cfg_add), .tl_cfg_ctl_w(tl_cfg_ctl),
    .tl_cfg_sts_w(tl_cfg_sts), .ko_cpl_spc_header_w(ko_cpl_spc_header),
    .ko_cpl_spc_data_w(ko_cpl_spc_data), .recv_dma_idle_w(recv_dma_idle),
    .bus_clk_w(bus_clk), .user_r_read_32_rden_w(user_r_read_32_rden),
    .user_r_read_32_data_w(user_r_read_32_data), .user_r_read_32_empty_w(user_r_read_32_empty),
    .user_r_read_32_eof_w(user_r_read_32_eof), .user_r_read_32_open_w(user_r_read_32_open),
    .user_w_write_32_wren_w(user_w_write_32_wren),
    .user_w_write_32_data_w(user_w_write_32_data),
    .user_w_write_32_full_w(user_w_write_32_full),
    .user_w_write_32_open_w(user_w_write_32_open), .quiesce_w(quiesce),
    .user_r_read_8_rden_w(user_r_read_8_rden), .user_r_read_8_data_w(user_r_read_8_data),
    .user_r_read_8_empty_w(user_r_read_8_empty), .user_r_read_8_eof_w(user_r_read_8_eof),
    .user_r_read_8_open_w(user_r_read_8_open), .user_led_w(user_led),
    .user_w_write_8_wren_w(user_w_write_8_wren), .user_w_write_8_data_w(user_w_write_8_data),
    .user_w_write_8_full_w(user_w_write_8_full), .user_w_write_8_open_w(user_w_write_8_open),
    .user_r_mem_32_rden_w(user_r_mem_32_rden), .user_r_mem_32_data_w(user_r_mem_32_data),
    .user_r_mem_32_empty_w(user_r_mem_32_empty), .user_r_mem_32_eof_w(user_r_mem_32_eof),
    .user_r_mem_32_open_w(user_r_mem_32_open), .tx_cred0_w(tx_cred0),
    .user_w_mem_32_wren_w(user_w_mem_32_wren), .user_w_mem_32_data_w(user_w_mem_32_data),
    .user_w_mem_32_full_w(user_w_mem_32_full), .user_w_mem_32_open_w(user_w_mem_32_open),
    .user_mem_32_addr_w(user_mem_32_addr), .user_mem_32_addr_update_w(user_mem_32_addr_update));

endmodule
