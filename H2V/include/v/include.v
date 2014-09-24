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
    assign eol = (initialized || min == max) && (value > $signed(max - step) || value < min);

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

module Concat(
    input clock,
    input ready,

	output reg  listA_req,
    input       listA_ack,
    input       listA_eol,
    input [7:0] listA_value,

	output reg  listB_req,
    input       listB_ack,
    input       listB_eol,
    input [7:0] listB_value,

	input            req,
	output reg       ack,
	output reg       eol,
	output reg [7:0] value
    );

    reg selectA, lastReq;

    always @(posedge clock) begin
        lastReq <= req;

        if(ready) begin
            if(~lastReq & req & listA_eol)
                selectA <= 0;
        end else begin
            selectA <= 1;
        end
    end

    always @(*) begin
        if(selectA) begin
            listA_req = req;
            ack = listA_ack;
            eol = 0;
            value = listA_value;
            listB_req = 0;

        end else begin
            listB_req = req;
            ack = listB_ack;
            eol = listB_eol;
            value = listB_value;
            listA_req = 0;
        end
    end

endmodule
