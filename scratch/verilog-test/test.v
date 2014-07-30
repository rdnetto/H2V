module dfd_57_cmb(
	//Input args: fib' (3 args)
	input clock,
	input ready,
	output reg done,
	output reg recurse,
	input [7:0] node_58,
	input [7:0] node_59,
	input [7:0] node_60,
	output reg [7:0] outArg_0,
	output reg [7:0] outArg_1,
	output reg [7:0] outArg_2,
	output reg [7:0] result
	);

	//Control signals & logic
	wire valid_0, ready_0, done_0;
	wire [7:0] result_0;
	wire node_60_done;
	wire [7:0] node_63;
	wire node_63_done;
	wire [7:0] node_62;
	wire node_62_done;
	wire node_58_done;
	wire [7:0] node_64;
	wire node_64_done;
	wire valid_1, ready_1, done_1;
	wire [7:0] outArg_1_0;
	wire [7:0] outArg_1_1;
	wire [7:0] outArg_1_2;
	
	wire node_59_done;
	wire [7:0] node_66;
	wire node_66_done;
	wire [7:0] node_65;
	wire node_65_done;
	wire [7:0] node_69;
	wire node_69_done;
	wire [7:0] node_67;
	wire node_67_done;
	wire [7:0] node_71;
	wire node_71_done;
	wire [7:0] node_70;
	wire node_70_done;
	assign valid_0 = node_62;
	assign ready_0 = 1;
	assign done_0 = 1;
	assign result_0 = node_64;
	assign node_60_done = ready;
	assign node_63 = 0;
	assign node_63_done = 1;
	assign node_62 = node_60 == node_63;
	assign node_62_done = node_60_done & node_63_done;
	assign node_58_done = ready;
	assign node_64 = node_58;
	assign node_64_done = node_58_done;
	assign valid_1 = ~node_62;
	assign ready_1 = 1;
	assign done_1 = 0;
	assign outArg_1_0 = node_66;
	assign outArg_1_1 = node_69;
	assign outArg_1_2 = node_70;
	
	assign node_59_done = ready;
	assign node_66 = node_59;
	assign node_66_done = node_59_done;
	assign node_65 = node_58;
	assign node_65_done = node_58_done;
	assign node_69 = node_65 + node_66;
	assign node_69_done = node_65_done & node_66_done;
	assign node_67 = node_60;
	assign node_67_done = node_60_done;
	assign node_71 = 1;
	assign node_71_done = 1;
	assign node_70 = node_67 - node_71;
	assign node_70_done = node_67_done & node_71_done;

	always @(*) begin
		if(valid_0) begin
			recurse = 0;
			done = done_0;
			result = result_0;
			outArg_0 = 8'hXX;
			outArg_1 = 8'hXX;
			outArg_2 = 8'hXX;
	
		end
		else if(valid_1) begin
			recurse = 1;
			done = done_1;
			result = 8'hXX;
			outArg_0 = outArg_1_0;
			outArg_1 = outArg_1_1;
			outArg_2 = outArg_1_2;
	
		end
		else 
		begin
			//This should never happen, but is needed to remove latches
			recurse = 1'bX;
			done = 1'bX;
			result = 8'dX;
			outArg_0 = 8'hXX;
			outArg_1 = 8'hXX;
			outArg_2 = 8'hXX;
		end
	end

endmodule

module dfd_57(
	//fib' (3 args)
	input clock,
	input ready,
	output reg done,
	input [7:0] inArg_0,
	input [7:0] inArg_1,
	input [7:0] inArg_2,
	output [7:0] result
	);

	wire advance, recurse;
	reg running;
	reg [7:0] nextArg_0;
	reg [7:0] nextArg_1;
	reg [7:0] nextArg_2;
	wire [7:0] outArg_0;
	wire [7:0] outArg_1;
	wire [7:0] outArg_2;

	dfd_57_cmb cmb(clock, ready, advance, recurse,
	  nextArg_0,
	  nextArg_1,
	  nextArg_2,
	  outArg_0,
	  outArg_1,
	  outArg_2,
	 result);
	wire nowDone;
	assign nowDone = advance & ~recurse;

	always @(posedge clock) begin
		if(ready ^ running) begin
			running <= ready;
			done <= 0;
		end
	
		if(ready & ~running) begin
			nextArg_0 <= inArg_0;
			nextArg_1 <= inArg_1;
			nextArg_2 <= inArg_2;
		end
	
		if(running & ~nowDone) begin
			nextArg_0 <= outArg_0;
			nextArg_1 <= outArg_1;
			nextArg_2 <= outArg_2;
		end
		done <= nowDone;
	end

endmodule

module test(
input clock, input ready, output done,
//fib (1 args)
input [7:0] node_55,

output [7:0] result
);
wire [7:0] node_73;
wire node_73_done;
wire [7:0] node_74;
wire node_74_done;
wire node_55_done;
wire [7:0] node_56;
wire node_56_done;
wire [7:0] node_72;
wire node_72_ready, node_72_done;
assign node_73 = 0;
assign node_73_done = 1;
assign node_74 = 1;
assign node_74_done = 1;
assign node_55_done = ready;
assign node_56 = node_55;
assign node_56_done = node_55_done;
assign node_72_ready = node_73_done & node_74_done & node_56_done;
dfd_57 fcall_72(clock, node_72_ready, node_72_done, node_73, node_74, node_56,  node_72);


assign result = node_72;
assign done = node_72_done;
endmodule

module dfd_3(
input clock, input ready, output done,
//f4 (1 args)
input [7:0] node_44,

output [7:0] result
);
wire node_44_done;
wire [7:0] node_45;
wire node_45_done;
wire [7:0] node_48;
wire node_48_done;
wire [7:0] node_47;
wire node_47_done;
wire [7:0] node_49;
wire node_49_done;
wire [7:0] node_52;
wire node_52_done;
wire [7:0] node_51;
wire node_51_done;
wire [7:0] node_53;
wire node_53_done;
wire [7:0] node_54;
wire node_54_done;
wire [7:0] node_50;
wire node_50_done;
wire [7:0] node_46;
wire node_46_done;
assign node_44_done = ready;
assign node_45 = node_44;
assign node_45_done = node_44_done;
assign node_48 = 0;
assign node_48_done = 1;
assign node_47 = node_45 < node_48;
assign node_47_done = node_45_done & node_48_done;
assign node_49 = 0;
assign node_49_done = 1;
assign node_52 = 0;
assign node_52_done = 1;
assign node_51 = node_45 > node_52;
assign node_51_done = node_45_done & node_52_done;
assign node_53 = 1;
assign node_53_done = 1;
assign node_54 = 2;
assign node_54_done = 1;
assign node_50 = node_51 ? node_53 : node_54;
assign node_50_done = node_51_done & node_53_done & node_54_done;
assign node_46 = node_47 ? node_49 : node_50;
assign node_46_done = node_47_done & node_49_done & node_50_done;

assign result = node_46;
assign done = node_46_done;
endmodule

module dfd_2(
input clock, input ready, output done,
//f3 (1 args)
input [7:0] node_31,

output [7:0] result
);
wire node_31_done;
wire [7:0] node_34;
wire node_34_done;
wire [7:0] node_33;
wire node_33_done;
wire [7:0] node_35;
wire node_35_done;
wire [7:0] node_36;
wire node_36_done;
wire [7:0] node_39;
wire node_39_done;
wire [7:0] node_38;
wire node_38_done;
wire [7:0] node_41;
wire node_41_done;
wire [7:0] node_42;
wire node_42_done;
wire [7:0] node_40;
wire node_40_done;
wire [7:0] node_43;
wire node_43_done;
wire [7:0] node_37;
wire node_37_done;
wire [7:0] node_32;
wire node_32_done;
assign node_31_done = ready;
assign node_34 = 0;
assign node_34_done = 1;
assign node_33 = node_31 == node_34;
assign node_33_done = node_31_done & node_34_done;
assign node_35 = 0;
assign node_35_done = 1;
assign node_36 = node_31;
assign node_36_done = node_31_done;
assign node_39 = 0;
assign node_39_done = 1;
assign node_38 = node_36 < node_39;
assign node_38_done = node_36_done & node_39_done;
assign node_41 = 0;
assign node_41_done = 1;
assign node_42 = 1;
assign node_42_done = 1;
assign node_40 = node_41 - node_42;
assign node_40_done = node_41_done & node_42_done;
assign node_43 = 1;
assign node_43_done = 1;
assign node_37 = node_38 ? node_40 : node_43;
assign node_37_done = node_38_done & node_40_done & node_43_done;
assign node_32 = node_33 ? node_35 : node_37;
assign node_32_done = node_33_done & node_35_done & node_37_done;

assign result = node_32;
assign done = node_32_done;
endmodule

module dfd_1(
input clock, input ready, output done,
//f1 (1 args)
input [7:0] node_19,

output [7:0] result
);
wire node_19_done;
wire [7:0] node_20;
wire node_20_done;
wire [7:0] node_23;
wire node_23_done;
wire [7:0] node_24;
wire node_24_done;
wire [7:0] node_26;
wire node_26_done;
wire [7:0] node_25;
wire node_25_done;
wire [7:0] node_27;
wire node_27_done;
wire [7:0] node_21;
wire node_21_done;
wire [7:0] node_22;
wire node_22_done;
wire [7:0] node_28;
wire node_28_done;
wire [7:0] node_29;
wire node_29_done;
wire [7:0] node_30;
wire node_30_done;
assign node_19_done = ready;
assign node_20 = node_19;
assign node_20_done = node_19_done;
assign node_23 = 2;
assign node_23_done = 1;
assign node_24 = node_23;
assign node_24_done = node_23_done;
assign node_26 = 2;
assign node_26_done = 1;
assign node_25 = node_24 + node_26;
assign node_25_done = node_24_done & node_26_done;
assign node_27 = node_25;
assign node_27_done = node_25_done;
assign node_21 = 1;
assign node_21_done = 1;
assign node_22 = node_21;
assign node_22_done = node_21_done;
assign node_28 = node_27 - node_22;
assign node_28_done = node_27_done & node_22_done;
assign node_29 = node_28;
assign node_29_done = node_28_done;
assign node_30 = node_20 + node_29;
assign node_30_done = node_20_done & node_29_done;

assign result = node_30;
assign done = node_30_done;
endmodule

module dfd_0(
input clock, input ready, output done,
//f (4 args)
input [7:0] node_5,
input [7:0] node_6,
input [7:0] node_7,
input [7:0] node_8,

output [7:0] result
);
wire node_6_done;
wire [7:0] node_10;
wire node_10_done;
wire [7:0] node_13;
wire node_13_done;
wire [7:0] node_14;
wire node_14_done;
wire node_5_done;
wire [7:0] node_9;
wire node_9_done;
wire [7:0] node_17;
wire node_17_done;
wire node_7_done;
wire [7:0] node_11;
wire node_11_done;
wire [7:0] node_18;
wire node_18_done;
wire [7:0] node_16;
wire node_16_done;
wire node_8_done;
wire [7:0] node_12;
wire node_12_done;
wire [7:0] node_15;
wire node_15_done;
assign node_6_done = ready;
assign node_10 = node_6;
assign node_10_done = node_6_done;
assign node_13 = node_10 * node_10;
assign node_13_done = node_10_done & node_10_done;
assign node_14 = node_13;
assign node_14_done = node_13_done;
assign node_5_done = ready;
assign node_9 = node_5;
assign node_9_done = node_5_done;
assign node_17 = node_14 * node_9;
assign node_17_done = node_14_done & node_9_done;
assign node_7_done = ready;
assign node_11 = node_7;
assign node_11_done = node_7_done;
assign node_18 = node_11 * node_9;
assign node_18_done = node_11_done & node_9_done;
assign node_16 = node_17 + node_18;
assign node_16_done = node_17_done & node_18_done;
assign node_8_done = ready;
assign node_12 = node_8;
assign node_12_done = node_8_done;
assign node_15 = node_16 + node_12;
assign node_15_done = node_16_done & node_12_done;

assign result = node_15;
assign done = node_15_done;
endmodule

