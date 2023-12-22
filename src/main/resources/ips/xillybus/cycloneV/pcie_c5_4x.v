module pcie_c5_4x (
		input          npor,               //               npor.npor
		input          pin_perst,          //                   .pin_perst
		input  [31:0]  test_in,            //           hip_ctrl.test_in
		input          simu_mode_pipe,     //                   .simu_mode_pipe
		input          pld_clk,            //            pld_clk.clk
		output         coreclkout,         //     coreclkout_hip.clk
		input          refclk,             //             refclk.clk
		input          rx_in0,             //         hip_serial.rx_in0
		input          rx_in1,             //                   .rx_in1
		input          rx_in2,             //                   .rx_in2
		input          rx_in3,             //                   .rx_in3
		output         tx_out0,            //                   .tx_out0
		output         tx_out1,            //                   .tx_out1
		output         tx_out2,            //                   .tx_out2
		output         tx_out3,            //                   .tx_out3
		output         rx_st_valid,        //              rx_st.valid
		output         rx_st_sop,          //                   .startofpacket
		output         rx_st_eop,          //                   .endofpacket
		input          rx_st_ready,        //                   .ready
		output         rx_st_err,          //                   .error
		output [63:0]  rx_st_data,         //                   .data
		output [7:0]   rx_st_bar,          //          rx_bar_be.rx_st_bar
		input          rx_st_mask,         //                   .rx_st_mask
		input          tx_st_valid,        //              tx_st.valid
		input          tx_st_sop,          //                   .startofpacket
		input          tx_st_eop,          //                   .endofpacket
		output         tx_st_ready,        //                   .ready
		input          tx_st_err,          //                   .error
		input  [63:0]  tx_st_data,         //                   .data
		output         tx_fifo_empty,      //            tx_fifo.fifo_empty
		output [11:0]  tx_cred_datafccp,   //            tx_cred.tx_cred_datafccp
		output [11:0]  tx_cred_datafcnp,   //                   .tx_cred_datafcnp
		output [11:0]  tx_cred_datafcp,    //                   .tx_cred_datafcp
		output [5:0]   tx_cred_fchipcons,  //                   .tx_cred_fchipcons
		output [5:0]   tx_cred_fcinfinite, //                   .tx_cred_fcinfinite
		output [7:0]   tx_cred_hdrfccp,    //                   .tx_cred_hdrfccp
		output [7:0]   tx_cred_hdrfcnp,    //                   .tx_cred_hdrfcnp
		output [7:0]   tx_cred_hdrfcp,     //                   .tx_cred_hdrfcp
		input          sim_pipe_pclk_in,   //           hip_pipe.sim_pipe_pclk_in
		output [1:0]   sim_pipe_rate,      //                   .sim_pipe_rate
		output [4:0]   sim_ltssmstate,     //                   .sim_ltssmstate
		output [2:0]   eidleinfersel0,     //                   .eidleinfersel0
		output [2:0]   eidleinfersel1,     //                   .eidleinfersel1
		output [2:0]   eidleinfersel2,     //                   .eidleinfersel2
		output [2:0]   eidleinfersel3,     //                   .eidleinfersel3
		output [1:0]   powerdown0,         //                   .powerdown0
		output [1:0]   powerdown1,         //                   .powerdown1
		output [1:0]   powerdown2,         //                   .powerdown2
		output [1:0]   powerdown3,         //                   .powerdown3
		output         rxpolarity0,        //                   .rxpolarity0
		output         rxpolarity1,        //                   .rxpolarity1
		output         rxpolarity2,        //                   .rxpolarity2
		output         rxpolarity3,        //                   .rxpolarity3
		output         txcompl0,           //                   .txcompl0
		output         txcompl1,           //                   .txcompl1
		output         txcompl2,           //                   .txcompl2
		output         txcompl3,           //                   .txcompl3
		output [7:0]   txdata0,            //                   .txdata0
		output [7:0]   txdata1,            //                   .txdata1
		output [7:0]   txdata2,            //                   .txdata2
		output [7:0]   txdata3,            //                   .txdata3
		output         txdatak0,           //                   .txdatak0
		output         txdatak1,           //                   .txdatak1
		output         txdatak2,           //                   .txdatak2
		output         txdatak3,           //                   .txdatak3
		output         txdetectrx0,        //                   .txdetectrx0
		output         txdetectrx1,        //                   .txdetectrx1
		output         txdetectrx2,        //                   .txdetectrx2
		output         txdetectrx3,        //                   .txdetectrx3
		output         txelecidle0,        //                   .txelecidle0
		output         txelecidle1,        //                   .txelecidle1
		output         txelecidle2,        //                   .txelecidle2
		output         txelecidle3,        //                   .txelecidle3
		output         txswing0,           //                   .txswing0
		output         txswing1,           //                   .txswing1
		output         txswing2,           //                   .txswing2
		output         txswing3,           //                   .txswing3
		output [2:0]   txmargin0,          //                   .txmargin0
		output [2:0]   txmargin1,          //                   .txmargin1
		output [2:0]   txmargin2,          //                   .txmargin2
		output [2:0]   txmargin3,          //                   .txmargin3
		output         txdeemph0,          //                   .txdeemph0
		output         txdeemph1,          //                   .txdeemph1
		output         txdeemph2,          //                   .txdeemph2
		output         txdeemph3,          //                   .txdeemph3
		input          phystatus0,         //                   .phystatus0
		input          phystatus1,         //                   .phystatus1
		input          phystatus2,         //                   .phystatus2
		input          phystatus3,         //                   .phystatus3
		input  [7:0]   rxdata0,            //                   .rxdata0
		input  [7:0]   rxdata1,            //                   .rxdata1
		input  [7:0]   rxdata2,            //                   .rxdata2
		input  [7:0]   rxdata3,            //                   .rxdata3
		input          rxdatak0,           //                   .rxdatak0
		input          rxdatak1,           //                   .rxdatak1
		input          rxdatak2,           //                   .rxdatak2
		input          rxdatak3,           //                   .rxdatak3
		input          rxelecidle0,        //                   .rxelecidle0
		input          rxelecidle1,        //                   .rxelecidle1
		input          rxelecidle2,        //                   .rxelecidle2
		input          rxelecidle3,        //                   .rxelecidle3
		input  [2:0]   rxstatus0,          //                   .rxstatus0
		input  [2:0]   rxstatus1,          //                   .rxstatus1
		input  [2:0]   rxstatus2,          //                   .rxstatus2
		input  [2:0]   rxstatus3,          //                   .rxstatus3
		input          rxvalid0,           //                   .rxvalid0
		input          rxvalid1,           //                   .rxvalid1
		input          rxvalid2,           //                   .rxvalid2
		input          rxvalid3,           //                   .rxvalid3
		output         reset_status,       //            hip_rst.reset_status
		output         serdes_pll_locked,  //                   .serdes_pll_locked
		output         pld_clk_inuse,      //                   .pld_clk_inuse
		input          pld_core_ready,     //                   .pld_core_ready
		output         testin_zero,        //                   .testin_zero
		input  [11:0]  lmi_addr,           //                lmi.lmi_addr
		input  [31:0]  lmi_din,            //                   .lmi_din
		input          lmi_rden,           //                   .lmi_rden
		input          lmi_wren,           //                   .lmi_wren
		output         lmi_ack,            //                   .lmi_ack
		output [31:0]  lmi_dout,           //                   .lmi_dout
		input          pm_auxpwr,          //         power_mngt.pm_auxpwr
		input  [9:0]   pm_data,            //                   .pm_data
		input          pme_to_cr,          //                   .pme_to_cr
		input          pm_event,           //                   .pm_event
		output         pme_to_sr,          //                   .pme_to_sr
		input  [349:0] reconfig_to_xcvr,   //   reconfig_to_xcvr.reconfig_to_xcvr
		input          busy_xcvr_reconfig, //                   .busy_xcvr_reconfig
		output [229:0] reconfig_from_xcvr, // reconfig_from_xcvr.reconfig_from_xcvr
		input  [4:0]   app_msi_num,        //            int_msi.app_msi_num
		input          app_msi_req,        //                   .app_msi_req
		input  [2:0]   app_msi_tc,         //                   .app_msi_tc
		output         app_msi_ack,        //                   .app_msi_ack
		input          app_int_sts_vec,    //                   .app_int_sts
		input  [4:0]   tl_hpg_ctrl_er,     //          config_tl.hpg_ctrler
		output [31:0]  tl_cfg_ctl,         //                   .tl_cfg_ctl
		input  [6:0]   cpl_err,            //                   .cpl_err
		output [3:0]   tl_cfg_add,         //                   .tl_cfg_add
		output [52:0]  tl_cfg_sts,         //                   .tl_cfg_sts
		input  [0:0]   cpl_pending,        //                   .cpl_pending
		output         tl_cfg_ctl_wr,      //                   .tl_cfg_ctl_wr
		output         tl_cfg_sts_wr,      //                   .tl_cfg_sts_wr
		output [1:0]   dl_current_speed,   //         hip_status.currentspeed
		output         derr_cor_ext_rcv0,  //                   .derr_cor_ext_rcv
		output         derr_cor_ext_rpl,   //                   .derr_cor_ext_rpl
		output         derr_rpl,           //                   .derr_rpl
		output         dlup_exit,          //                   .dlup_exit
		output [4:0]   dl_ltssm,           //                   .ltssmstate
		output         ev128ns,            //                   .ev128ns
		output         ev1us,              //                   .ev1us
		output         hotrst_exit,        //                   .hotrst_exit
		output [3:0]   int_status,         //                   .int_status
		output         l2_exit,            //                   .l2_exit
		output [3:0]   lane_act,           //                   .lane_act
		output [7:0]   ko_cpl_spc_header,  //                   .ko_cpl_spc_header
		output [11:0]  ko_cpl_spc_data     //                   .ko_cpl_spc_data
	);

    pcie_reconfig pcie_reconfig (
        .npor_npor                        (npor),                        //           npor.npor
        .npor_pin_perst                   (pin_perst),                   //               .pin_perst
        .hip_ctrl_test_in                 (test_in),                 //       hip_ctrl.test_in
        .hip_ctrl_simu_mode_pipe          (simu_mode_pipe),          //               .simu_mode_pipe
        .refclk_clk                       (refclk),                       //         refclk.clk
        .hip_pipe_sim_pipe_pclk_in        (sim_pipe_pclk_in),        //       hip_pipe.sim_pipe_pclk_in
        .hip_pipe_sim_pipe_rate           (sim_pipe_rate),           //               .sim_pipe_rate
        .hip_pipe_sim_ltssmstate          (sim_ltssmstate),          //               .sim_ltssmstate
        .hip_pipe_eidleinfersel0          (eidleinfersel0),          //               .eidleinfersel0
        .hip_pipe_eidleinfersel1          (eidleinfersel1),          //               .eidleinfersel1
        .hip_pipe_eidleinfersel2          (eidleinfersel2),          //               .eidleinfersel2
        .hip_pipe_eidleinfersel3          (eidleinfersel3),          //               .eidleinfersel3
        .hip_pipe_powerdown0              (powerdown0),              //               .powerdown0
        .hip_pipe_powerdown1              (powerdown1),              //               .powerdown1
        .hip_pipe_powerdown2              (powerdown2),              //               .powerdown2
        .hip_pipe_powerdown3              (powerdown3),              //               .powerdown3
        .hip_pipe_rxpolarity0             (rxpolarity0),             //               .rxpolarity0
        .hip_pipe_rxpolarity1             (rxpolarity1),             //               .rxpolarity1
        .hip_pipe_rxpolarity2             (rxpolarity2),             //               .rxpolarity2
        .hip_pipe_rxpolarity3             (rxpolarity3),             //               .rxpolarity3
        .hip_pipe_txcompl0                (txcompl0),                //               .txcompl0
        .hip_pipe_txcompl1                (txcompl1),                //               .txcompl1
        .hip_pipe_txcompl2                (txcompl2),                //               .txcompl2
        .hip_pipe_txcompl3                (txcompl3),                //               .txcompl3
        .hip_pipe_txdata0                 (txdata0),                 //               .txdata0
        .hip_pipe_txdata1                 (txdata1),                 //               .txdata1
        .hip_pipe_txdata2                 (txdata2),                 //               .txdata2
        .hip_pipe_txdata3                 (txdata3),                 //               .txdata3
        .hip_pipe_txdatak0                (txdatak0),                //               .txdatak0
        .hip_pipe_txdatak1                (txdatak1),                //               .txdatak1
        .hip_pipe_txdatak2                (txdatak2),                //               .txdatak2
        .hip_pipe_txdatak3                (txdatak3),                //               .txdatak3
        .hip_pipe_txdetectrx0             (txdetectrx0),             //               .txdetectrx0
        .hip_pipe_txdetectrx1             (txdetectrx1),             //               .txdetectrx1
        .hip_pipe_txdetectrx2             (txdetectrx2),             //               .txdetectrx2
        .hip_pipe_txdetectrx3             (txdetectrx3),             //               .txdetectrx3
        .hip_pipe_txelecidle0             (txelecidle0),             //               .txelecidle0
        .hip_pipe_txelecidle1             (txelecidle1),             //               .txelecidle1
        .hip_pipe_txelecidle2             (txelecidle2),             //               .txelecidle2
        .hip_pipe_txelecidle3             (txelecidle3),             //               .txelecidle3
        .hip_pipe_txswing0                (txswing0),                //               .txswing0
        .hip_pipe_txswing1                (txswing1),                //               .txswing1
        .hip_pipe_txswing2                (txswing2),                //               .txswing2
        .hip_pipe_txswing3                (txswing3),                //               .txswing3
        .hip_pipe_txmargin0               (txmargin0),               //               .txmargin0
        .hip_pipe_txmargin1               (txmargin1),               //               .txmargin1
        .hip_pipe_txmargin2               (txmargin2),               //               .txmargin2
        .hip_pipe_txmargin3               (txmargin3),               //               .txmargin3
        .hip_pipe_txdeemph0               (txdeemph0),               //               .txdeemph0
        .hip_pipe_txdeemph1               (txdeemph1),               //               .txdeemph1
        .hip_pipe_txdeemph2               (txdeemph2),               //               .txdeemph2
        .hip_pipe_txdeemph3               (txdeemph3),               //               .txdeemph3
        .hip_pipe_phystatus0              (phystatus0),              //               .phystatus0
        .hip_pipe_phystatus1              (phystatus1),              //               .phystatus1
        .hip_pipe_phystatus2              (phystatus2),              //               .phystatus2
        .hip_pipe_phystatus3              (phystatus3),              //               .phystatus3
        .hip_pipe_rxdata0                 (rxdata0),                 //               .rxdata0
        .hip_pipe_rxdata1                 (rxdata1),                 //               .rxdata1
        .hip_pipe_rxdata2                 (rxdata2),                 //               .rxdata2
        .hip_pipe_rxdata3                 (rxdata3),                 //               .rxdata3
        .hip_pipe_rxdatak0                (rxdatak0),                //               .rxdatak0
        .hip_pipe_rxdatak1                (rxdatak1),                //               .rxdatak1
        .hip_pipe_rxdatak2                (rxdatak2),                //               .rxdatak2
        .hip_pipe_rxdatak3                (rxdatak3),                //               .rxdatak3
        .hip_pipe_rxelecidle0             (rxelecidle0),             //               .rxelecidle0
        .hip_pipe_rxelecidle1             (rxelecidle1),             //               .rxelecidle1
        .hip_pipe_rxelecidle2             (rxelecidle2),             //               .rxelecidle2
        .hip_pipe_rxelecidle3             (rxelecidle3),             //               .rxelecidle3
        .hip_pipe_rxstatus0               (rxstatus0),               //               .rxstatus0
        .hip_pipe_rxstatus1               (rxstatus1),               //               .rxstatus1
        .hip_pipe_rxstatus2               (rxstatus2),               //               .rxstatus2
        .hip_pipe_rxstatus3               (rxstatus3),               //               .rxstatus3
        .hip_pipe_rxvalid0                (rxvalid0),                //               .rxvalid0
        .hip_pipe_rxvalid1                (rxvalid1),                //               .rxvalid1
        .hip_pipe_rxvalid2                (rxvalid2),                //               .rxvalid2
        .hip_pipe_rxvalid3                (rxvalid3),                //               .rxvalid3
        .hip_serial_rx_in0                (rx_in0),                //     hip_serial.rx_in0
        .hip_serial_rx_in1                (rx_in1),                //               .rx_in1
        .hip_serial_rx_in2                (rx_in2),                //               .rx_in2
        .hip_serial_rx_in3                (rx_in3),                //               .rx_in3
        .hip_serial_tx_out0               (tx_out0),               //               .tx_out0
        .hip_serial_tx_out1               (tx_out1),               //               .tx_out1
        .hip_serial_tx_out2               (tx_out2),               //               .tx_out2
        .hip_serial_tx_out3               (tx_out3),               //               .tx_out3
	// According to Table 16-7 in Altera Transciever PHY IP Core User Guide,
	// mgmt_clk_clk MUST go to refclk on certain V-series devices, and for
	// others 100 MHz is always OK. So the conclusion is that refclk is used
	// to run the reconfiguration logic.
        .reconfig_clk_clk                 (refclk),                 //   reconfig_clk.clk
	// It has been verified that reconfig_reset_reset_n is indeed active
	// low, and noone said anything about what drives this reset. So the
	// global PCIe reset (which is synchronized in the block)
        .reconfig_reset_reset_n           (pin_perst),           // reconfig_reset.reset_n
        .rx_st_valid                      (rx_st_valid),                      //          rx_st.valid
        .rx_st_startofpacket              (rx_st_sop),              //               .startofpacket
        .rx_st_endofpacket                (rx_st_eop),                //               .endofpacket
        .rx_st_ready                      (rx_st_ready),                      //               .ready
        .rx_st_error                      (rx_st_err),                      //               .error
        .rx_st_data                       (rx_st_data),                       //               .data
        .rx_bar_be_rx_st_bar              (rx_st_bar),              //      rx_bar_be.rx_st_bar
        .rx_bar_be_rx_st_mask             (rx_st_mask),             //               .rx_st_mask
        .tx_st_valid                      (tx_st_valid),                      //          tx_st.valid
        .tx_st_startofpacket              (tx_st_sop),              //               .startofpacket
        .tx_st_endofpacket                (tx_st_eop),                //               .endofpacket
        .tx_st_ready                      (tx_st_ready),                      //               .ready
        .tx_st_error                      (tx_st_err),                      //               .error
        .tx_st_data                       (tx_st_data),                       //               .data
        .tx_fifo_fifo_empty               (tx_fifo_empty),               //        tx_fifo.fifo_empty
        .tx_cred_tx_cred_datafccp         (tx_cred_datafccp),         //        tx_cred.tx_cred_datafccp
        .tx_cred_tx_cred_datafcnp         (tx_cred_datafcnp),         //               .tx_cred_datafcnp
        .tx_cred_tx_cred_datafcp          (tx_cred_datafcp),          //               .tx_cred_datafcp
        .tx_cred_tx_cred_fchipcons        (tx_cred_fchipcons),        //               .tx_cred_fchipcons
        .tx_cred_tx_cred_fcinfinite       (tx_cred_fcinfinite),       //               .tx_cred_fcinfinite
        .tx_cred_tx_cred_hdrfccp          (tx_cred_hdrfccp),          //               .tx_cred_hdrfccp
        .tx_cred_tx_cred_hdrfcnp          (tx_cred_hdrfcnp),          //               .tx_cred_hdrfcnp
        .tx_cred_tx_cred_hdrfcp           (tx_cred_hdrfcp),           //               .tx_cred_hdrfcp
        .hip_rst_reset_status             (reset_status),             //        hip_rst.reset_status
        .hip_rst_serdes_pll_locked        (serdes_pll_locked),        //               .serdes_pll_locked
        .hip_rst_pld_clk_inuse            (pld_clk_inuse),            //               .pld_clk_inuse
        .hip_rst_pld_core_ready           (pld_core_ready),           //               .pld_core_ready
        .hip_rst_testin_zero              (testin_zero),              //               .testin_zero
        .lmi_lmi_addr                     (lmi_addr),                     //            lmi.lmi_addr
        .lmi_lmi_din                      (lmi_din),                      //               .lmi_din
        .lmi_lmi_rden                     (lmi_rden),                     //               .lmi_rden
        .lmi_lmi_wren                     (lmi_wren),                     //               .lmi_wren
        .lmi_lmi_ack                      (lmi_ack),                      //               .lmi_ack
        .lmi_lmi_dout                     (lmi_dout),                     //               .lmi_dout
        .power_mngt_pm_auxpwr             (pm_auxpwr),             //     power_mngt.pm_auxpwr
        .power_mngt_pm_data               (pm_data),               //               .pm_data
        .power_mngt_pme_to_cr             (pme_to_cr),             //               .pme_to_cr
        .power_mngt_pm_event              (pm_event),              //               .pm_event
        .power_mngt_pme_to_sr             (pme_to_sr),             //               .pme_to_sr
        .int_msi_app_msi_num              (app_msi_num),              //        int_msi.app_msi_num
        .int_msi_app_msi_req              (app_msi_req),              //               .app_msi_req
        .int_msi_app_msi_tc               (app_msi_tc),               //               .app_msi_tc
        .int_msi_app_msi_ack              (app_msi_ack),              //               .app_msi_ack
        .int_msi_app_int_sts              (app_int_sts_vec),              //               .app_int_sts
        .config_tl_hpg_ctrler             (tl_hpg_ctrl_er),             //      config_tl.hpg_ctrler
        .config_tl_tl_cfg_ctl             (tl_cfg_ctl),             //               .tl_cfg_ctl
        .config_tl_cpl_err                (cpl_err),                //               .cpl_err
        .config_tl_tl_cfg_add             (tl_cfg_add),             //               .tl_cfg_add
        .config_tl_tl_cfg_sts             (tl_cfg_sts),             //               .tl_cfg_sts
        .config_tl_cpl_pending            (cpl_pending),            //               .cpl_pending
        .config_tl_tl_cfg_ctl_wr          (tl_cfg_ctl_wr),          //               .tl_cfg_ctl_wr
        .config_tl_tl_cfg_sts_wr          (tl_cfg_sts_wr),          //               .tl_cfg_sts_wr
        .hip_status_derr_cor_ext_rcv      (derr_cor_ext_rcv0),      //     hip_status.derr_cor_ext_rcv
        .hip_status_derr_cor_ext_rpl      (derr_cor_ext_rpl),      //               .derr_cor_ext_rpl
        .hip_status_derr_rpl              (derr_rpl),              //               .derr_rpl
        .hip_status_dlup_exit             (dlup_exit),             //               .dlup_exit
        .hip_status_ltssmstate            (dl_ltssm),            //               .ltssmstate
        .hip_status_ev128ns               (ev128ns),               //               .ev128ns
        .hip_status_ev1us                 (ev1us),                 //               .ev1us
        .hip_status_hotrst_exit           (hotrst_exit),           //               .hotrst_exit
        .hip_status_int_status            (int_status),            //               .int_status
        .hip_status_l2_exit               (l2_exit),               //               .l2_exit
        .hip_status_lane_act              (lane_act),              //               .lane_act
        .hip_status_ko_cpl_spc_header     (ko_cpl_spc_header),     //               .ko_cpl_spc_header
        .hip_status_ko_cpl_spc_data       (ko_cpl_spc_data),       //               .ko_cpl_spc_data
        .pld_clk_clk                      (pld_clk),                      //        pld_clk.clk
        .pld_clk_1_clk                    (pld_clk),                    //      pld_clk_1.clk
        .coreclkout_hip_clk               (coreclkout),               // coreclkout_hip.clk
        .hip_status_drv_derr_cor_ext_rcv  (derr_cor_ext_rcv0),  // hip_status_drv.derr_cor_ext_rcv
        .hip_status_drv_derr_cor_ext_rpl  (derr_cor_ext_rpl),  //               .derr_cor_ext_rpl
        .hip_status_drv_derr_rpl          (derr_rpl),          //               .derr_rpl
        .hip_status_drv_dlup_exit         (dlup_exit),         //               .dlup_exit
        .hip_status_drv_ev128ns           (ev128ns),           //               .ev128ns
        .hip_status_drv_ev1us             (ev1us),             //               .ev1us
        .hip_status_drv_hotrst_exit       (hotrst_exit),       //               .hotrst_exit
        .hip_status_drv_int_status        (int_status),        //               .int_status
        .hip_status_drv_l2_exit           (l2_exit),           //               .l2_exit
        .hip_status_drv_lane_act          (lane_act),          //               .lane_act
        .hip_status_drv_ltssmstate        (dl_ltssm),        //               .ltssmstate
        .hip_status_drv_ko_cpl_spc_header (ko_cpl_spc_header), //               .ko_cpl_spc_header
        .hip_status_drv_ko_cpl_spc_data   (ko_cpl_spc_data)    //               .ko_cpl_spc_data
    );


endmodule

