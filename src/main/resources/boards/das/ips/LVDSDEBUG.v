module LVDSDEBUG(
    adc_clk,
    adc_a, adc_b,
    rstn,
    DOUTA, DOUTB, DOUTC, DOUTD, DOUTBA, DOUTBB, DOUTBC, DOUTBD, // 应该通过名字反映其顺序关系
    lvds_clk
);

    input adc_clk, rstn;
    input[6:0] adc_b, adc_a;
    output lvds_clk;

    output reg[13:0] DOUTA, DOUTB, DOUTC, DOUTD;
    output reg[13:0] DOUTBA, DOUTBB, DOUTBC, DOUTBD;

    wire[55:0] rx_out_sig;

    reg[55:0] rx_out_sigr;
    wire[55:0] rx_out_sigB;
    reg[55:0] rx_out_sigrB;

    wire LVDS_lvds_clkA, rx_enable, LVDS_SLOW_CLK, LVDS_PARALLEL_CLK, locked, LVDS_PARALLEL_CLKB, LVDS_PARALLEL_CLKC;
    wire LVDS_FAST_CLK0, LVDS_FAST_CLK1, LVDS_FAST_CLK2, LVDS_FAST_CLK3, LVDS_FAST_CLK4, LVDS_FAST_CLK5, LVDS_FAST_CLK6;
    wire[13:0] DATA0, DATA1, DATA2, DATA3;
    wire[13:0] DATAB0, DATAB1, DATAB2, DATAB3;

    assign lvds_clk = LVDS_PARALLEL_CLK;

    reg[15:0] PULSE_CNT;
    reg PULSE;
    always @(negedge rstn or posedge LVDS_PARALLEL_CLK)
        begin
            if (~rstn)
                begin
                    PULSE <= 0;
                    PULSE_CNT <= 0;
                end
            else if (PULSE_CNT == 2)
                begin
                    PULSE <= ~PULSE;
                    PULSE_CNT <= 0;
                end
            else
                begin
                    PULSE <= PULSE;
                    PULSE_CNT <= PULSE_CNT+1; // FIXME: +1 lead to a 32-bit result
                end
        end

    always @(posedge LVDS_PARALLEL_CLK)
        begin
            DOUTA <= DATA0;
            DOUTB <= DATA1;
            DOUTC <= DATA2;
            DOUTD <= DATA3;
        end
    always @(posedge LVDS_PARALLEL_CLK)
        begin
            DOUTBA <= DATAB0;
            DOUTBB <= DATAB1;
            DOUTBC <= DATAB2;
            DOUTBD <= DATAB3;
        end

    always @(posedge LVDS_PARALLEL_CLK)
        begin
            rx_out_sigr <= rx_out_sig;
        end
    always @(posedge LVDS_PARALLEL_CLK)
        begin
            rx_out_sigrB <= rx_out_sigB;
        end

    assign DATA3 = {rx_out_sigr[54], rx_out_sigr[55], rx_out_sigr[46], rx_out_sigr[47], rx_out_sigr[38], rx_out_sigr[39], rx_out_sigr[30], rx_out_sigr[31], rx_out_sigr[22], rx_out_sigr[23], rx_out_sigr[14], rx_out_sigr[15], rx_out_sigr[6], rx_out_sigr[7]};
    assign DATA2 = {rx_out_sigr[52], rx_out_sigr[53], rx_out_sigr[44], rx_out_sigr[45], rx_out_sigr[36], rx_out_sigr[37], rx_out_sigr[28], rx_out_sigr[29], rx_out_sigr[20], rx_out_sigr[21], rx_out_sigr[12], rx_out_sigr[13], rx_out_sigr[4], rx_out_sigr[5]};
    assign DATA1 = {rx_out_sigr[50], rx_out_sigr[51], rx_out_sigr[42], rx_out_sigr[43], rx_out_sigr[34], rx_out_sigr[35], rx_out_sigr[26], rx_out_sigr[27], rx_out_sigr[18], rx_out_sigr[19], rx_out_sigr[10], rx_out_sigr[11], rx_out_sigr[2], rx_out_sigr[3]};
    assign DATA0 = {rx_out_sigr[48], rx_out_sigr[49], rx_out_sigr[40], rx_out_sigr[41], rx_out_sigr[32], rx_out_sigr[33], rx_out_sigr[24], rx_out_sigr[25], rx_out_sigr[16], rx_out_sigr[17], rx_out_sigr[8], rx_out_sigr[9], rx_out_sigr[0], rx_out_sigr[1]};
    assign DATAB3 = {rx_out_sigrB[54], rx_out_sigrB[55], rx_out_sigrB[46], rx_out_sigrB[47], rx_out_sigrB[38], rx_out_sigrB[39], rx_out_sigrB[30], rx_out_sigrB[31], rx_out_sigrB[22], rx_out_sigrB[23], rx_out_sigrB[14], rx_out_sigrB[15], rx_out_sigrB[6], rx_out_sigrB[7]};
    assign DATAB2 = {rx_out_sigrB[52], rx_out_sigrB[53], rx_out_sigrB[44], rx_out_sigrB[45], rx_out_sigrB[36], rx_out_sigrB[37], rx_out_sigrB[28], rx_out_sigrB[29], rx_out_sigrB[20], rx_out_sigrB[21], rx_out_sigrB[12], rx_out_sigrB[13], rx_out_sigrB[4], rx_out_sigrB[5]};
    assign DATAB1 = {rx_out_sigrB[50], rx_out_sigrB[51], rx_out_sigrB[42], rx_out_sigrB[43], rx_out_sigrB[34], rx_out_sigrB[35], rx_out_sigrB[26], rx_out_sigrB[27], rx_out_sigrB[18], rx_out_sigrB[19], rx_out_sigrB[10], rx_out_sigrB[11], rx_out_sigrB[2], rx_out_sigrB[3]};
    assign DATAB0 = {rx_out_sigrB[48], rx_out_sigrB[49], rx_out_sigrB[40], rx_out_sigrB[41], rx_out_sigrB[32], rx_out_sigrB[33], rx_out_sigrB[24], rx_out_sigrB[25], rx_out_sigrB[16], rx_out_sigrB[17], rx_out_sigrB[8], rx_out_sigrB[9], rx_out_sigrB[0], rx_out_sigrB[1]};

    LVDSPLL lvds_pll_ADCA(
        .refclk(adc_clk),   //  refclk.clk
        .rst(~rstn),      //   reset.reset
        .outclk_0(LVDS_FAST_CLK0), // outclk0.clk
        .outclk_1(LVDS_SLOW_CLK), // outclk1.clk
        .outclk_2(LVDS_PARALLEL_CLK), // outclk1.clk
        .outclk_3(LVDS_FAST_CLK1)// outclk1.clk
    );

    LVDS14 LVDS14_ADCA(
        .rx_enable(LVDS_SLOW_CLK),//rx_enable之后的1.5个LVDS_FAST_CLK周期，rx_out_sig有效；有效的数来自2~2+F个LVDS_FAST_CLK周期之前的F个数（F为解串系数）
        .rx_in(adc_a),//所以rx_enable决定了对齐的方式，其频率反映解串因子，其周期决定其高低位顺序。
        .rx_inclock(LVDS_FAST_CLK0),//rx_inclock的延时，则决定了取样时上升沿处于输入字符的何种位置，理论上说180度延时，是保证上升沿正好在字符中央。
        .rx_out(rx_out_sig)
    );
    LVDS14 LVDS14_ADCB(
        .rx_enable(LVDS_SLOW_CLK),//rx_enable之后的1.5个LVDS_FAST_CLK周期，rx_out_sig有效；有效的数来自2~2+F个LVDS_FAST_CLK周期之前的F个数（F为解串系数）
        .rx_in(adc_b),//所以rx_enable决定了对齐的方式，其频率反映解串因子，其周期决定其高低位顺序。
        .rx_inclock(LVDS_FAST_CLK0),//rx_inclock的延时，则决定了取样时上升沿处于输入字符的何种位置，理论上说180度延时，是保证上升沿正好在字符中央。
        .rx_out(rx_out_sigB)
    );

endmodule
