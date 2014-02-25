module f (x, a, b, c);
	input [31:0] x;
	input [31:0] a;
	input [31:0] b;
	input [31:0] c;
	output [31:0] res;

	wire [31:0] w0;
	wire [31:0] w1;
	wire [31:0] w2;
	wire [31:0] w3;
	wire [31:0] w4;

	assign w0 = w1 + c;
	assign w1 = w2 + w3;
	assign w2 = w4 * x;
	assign w4 = a * a;
	assign w3 = b * x;
	assign res = w0;
endmodule;


