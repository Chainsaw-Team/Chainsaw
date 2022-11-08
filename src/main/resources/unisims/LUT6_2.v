// modified LUT6_2 model

`timescale 1 ps/1 ps

`celldefine

module LUT6_2 #(
`ifdef XIL_TIMING
    parameter LOC = "UNPLACED",
`endif
    parameter [63:0] INIT = 64'h0000000000000000
) (
    output O5,
    output O6,

    input I0,
    input I1,
    input I2,
    input I3,
    input I4,
    input I5
);

  assign O5 = INIT[{I4, I3, I2, I1, I0}];
  assign O6 = INIT[{I5, I4, I3, I2, I1, I0}];

endmodule

`endcelldefine
