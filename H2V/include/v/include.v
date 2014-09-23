module BoundedEnum(
    input clock,
    input ready,

    input [7:0] min,
    input [7:0] step,
    input [7:0] max,

	input      req,
	output reg ack,
	output     eol,
	output reg [7:0] value
    );

    reg lastReady, lastAck;
    assign eol = (value >= max || min > value);

	always @(posedge clock) begin
        lastReady <= ready;

        if(ready & req) begin
            if(~lastReady)
                value <= min;
            else if(~eol)
                value <= value + 1;

            ack <= 1;
        end else begin
            ack <= 0;
        end
	end
endmodule
