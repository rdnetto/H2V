module BoundedEnum(
    input clock,
    input ready,

    input signed [7:0] min,
    input [7:0] step,
    input signed [7:0] max,

	input      req,
	output reg ack,
	output     eol,
	output reg signed [7:0] value
    );

    reg lastReq;
    reg initialized;
    assign eol = (initialized || min == max) && (value >= max || value < min);

	always @(posedge clock) begin
        lastReq <= req;

        if(ready) begin
            if(req & ~lastReq & (~initialized | ~eol)) begin
                if(initialized)
                    value <= value + step;
                else
                    value <= min;

                initialized <= 1;
                ack <= 1;

            end else begin
                ack <= 0;
            end

        end else begin
            ack <= 0;
            initialized <= 0;
            value <= 8'hXX;
        end
	end
endmodule
