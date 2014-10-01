module BoundedEnum(
    input clock,
    input ready,

    input signed [7:0] min,
    input [7:0] step,
    input signed [7:0] max,

	input      req,
	output reg ack,
	output reg signed [7:0] value,
    output reg value_valid
    );

    reg lastReq;
    reg initialized;
    wire signed [7:0] nextValue;
    assign nextValue = value + step;

	always @(posedge clock) begin
        lastReq <= req;

        if(ready) begin
            if(req & ~lastReq) begin
                if(initialized) begin
                    if(nextValue > max || nextValue < min) begin
                        value_valid <= 0;

                    end else begin
                        value <= nextValue;
                        value_valid <= 1;
                    end

                end else begin
                    initialized <= 1;
                    value <= min;
                    value_valid <= 1;
                end

                ack <= 1;

            end else begin
                ack <= 0;
            end

        end else begin
            ack <= 0;
            initialized <= 0;
            value <= 8'hXX;
            value_valid <= 0;
        end
	end
endmodule

module Concat(
    input clock,
    input ready,

	output reg  listA_req,
    input       listA_ack,
    input [7:0] listA_value,
    input       listA_value_valid,

	output reg  listB_req,
    input       listB_ack,
    input [7:0] listB_value,
    input       listB_value_valid,

	input            req,
	output reg       ack,
	output reg [7:0] value,
	output reg       value_valid
    );

    reg lastSelectA;
    wire selectA;
    assign selectA = lastSelectA & (listA_ack ? listA_value_valid : 1'b1);

    always @(posedge clock) begin
        if(ready)
            lastSelectA <= selectA;
        else
            lastSelectA <= 1;
    end

    always @(*) begin
        if(selectA) begin
            listA_req = req;
            ack = listA_ack;
            value = listA_value;
            value_valid = listA_value_valid;
            listB_req = 0;

        end else begin
            listB_req = req;
            ack = listB_ack;
            value = listB_value;
            value_valid = listB_value_valid;
            listA_req = 0;
        end
    end
endmodule

module Cons(
    input clock,
    input ready,
    input [7:0] head,

	output reg  tail_req,
    input       tail_ack,
    input [7:0] tail_value,
    input       tail_value_valid,

	input            req,
	output reg       ack,
	output reg [7:0] value,
    output reg       value_valid
    );

    reg headShown;
    reg selectHead;
    reg lastReq;
    reg headAck;

    always @(posedge clock) begin
        lastReq <= req;

        if(ready) begin
            if(~lastReq & req) begin
                headAck <= 1;
                headShown <= 1;

                if(headShown)
                    selectHead <= 0;

            end else begin
                headAck <= 0;
            end

        end else begin
            headShown <= 0;
            selectHead <= 1;
            headAck <= 0;
        end
    end

    always @(*) begin
        if(selectHead) begin
            ack = headAck;
            value = head;
            value_valid = 1;
            tail_req = 0;

        end else begin
            tail_req = req;
            ack = tail_ack;
            value = tail_value;
            value_valid = tail_value_valid;
        end
    end
endmodule

module Hold(
    //Utility module for keeping y high after x goes high (while ready is high). Used in generated code.
    input clock,
    input ready,
    input x,
    output reg y
    );

    always @(posedge clock)
        y <= ready ? y | x : 0;
endmodule

module ListMux(
    //Utility module for implementing the ternary operator on lists.
    input cond,
    output reg  out_req,
    input       out_ack,
    input [7:0] out_value,
    input       out_value_valid,

    input            true_req,
    output reg       true_ack,
    output reg [7:0] true_value,
    output reg       true_value_valid,

    input            false_req,
    output reg       false_ack,
    output reg [7:0] false_value,
    output reg       false_value_valid
    );

    always @(*) begin
        if(cond) begin
            out_req = true_req;
            true_ack = out_ack;
            true_value = out_value;
            true_value_valid = out_value_valid;

        end else begin
            out_req = false_req;
            false_ack = out_ack;
            false_value = out_value;
            false_value_valid = out_value_valid;
        end
    end
endmodule

module Decons(
    input clock,
    input ready,
    output reg  done,

	output reg  list_req,
    input       list_ack,
    input [7:0] list_value,
    input       list_value_valid,

    output reg [7:0] head,
    output reg       head_valid,

	input            tail_req,
	output reg       tail_ack,
	output reg [7:0] tail_value,
    output reg       tail_value_valid
    );

    always @(posedge clock) begin
        if(ready) begin
            if(~done & list_ack) begin
                done <= 1'b1;
                head <= list_value;
                head_valid <= list_value_valid;
            end

        end else begin
            done <= 1'b0;
            head <= 8'hFF;
            head_valid = 1'b0;
        end
    end

    always @(*) begin
        list_req = ready & ~done;

        if(done) begin
            list_req = tail_req;
            tail_ack = list_ack;
            tail_value = list_value;
            tail_value_valid = list_value_valid;
        end else begin
            list_req = ready;
            tail_ack = 1'b0;
            tail_value = 8'hFF;
            tail_value_valid = 1'b0;
        end
    end
endmodule
