module dfd_0(
//f4 (1 args)
input [7:0] node_1, //arg 0
output [7:0] node_0
);
wire [7:0] node_2;
wire [7:0] node_5;
wire [7:0] node_4;
wire [7:0] node_6;
wire [7:0] node_9;
wire [7:0] node_8;
wire [7:0] node_10;
wire [7:0] node_11;
wire [7:0] node_7;
wire [7:0] node_3;

assign node_2 = node_1;
assign node_5 = 0;
assign node_4 = node_2 < node_5;
assign node_6 = 0;
assign node_9 = 0;
assign node_8 = node_2 > node_9;
assign node_10 = 1;
assign node_11 = 2;
assign node_7 = node_8 ? node_10 : node_11;
assign node_3 = node_4 ? node_6 : node_7;
assign node_0 = node_3;
endmodule

