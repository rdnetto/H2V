module testbench(input CLOCK_50, input [1:0] KEY, output [7:0] LED);
reg ready, running;
reg [7:0] x;
dfd_4 fib(CLOCK_50, ready, done, x, result);

initial x = -8'd1;
initial ready = 0;
initial running = 0;
assign LED = 8'd0;

always @(posedge CLOCK_50) begin
	if(running) begin
		
		if(ready & done)
			ready <= 0;
		else if(~ready) begin
			x <= x + 1;
			ready <= 1;
		end			
	
	end else if(~KEY[0]) begin
		running <= 1;
	end else if(~KEY[1]) begin
		running <= 0;
		ready <= 0;
		x <= -1;
	end
end
	
endmodule