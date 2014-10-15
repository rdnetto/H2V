module testbench(input CLOCK_50, input [1:0] KEY, output [7:0] LED);
    reg ready;
    reg req;
    reg running;
    reg [7:0] reqCount;
    reg [24:0] timer;

    dfd_5 dfdUT(CLOCK_50, running, done,
        1, result);

    initial running = 0;
    assign LED = 8'd0;
    assign done = 1;

    always @(posedge CLOCK_50) begin
        if(running) begin
				 if(reqCount < 8'd10) begin
					  reqCount <= reqCount + 8'd1;
				 end else begin
					  reqCount <= 8'd0;
					  req <= ~req;
				 end

        end else begin
            req <= 1'b0;
            reqCount <= 8'd0;
        end

        timer <= timer + 25'd1;
        if(timer == 25'd0)
            running <= ~running;

        if(timer == 25'd0 && ~running)
            req <= 1'b1;
    end
endmodule



module list_testbench(input CLOCK_50, input [1:0] KEY, output [7:0] LED);
    reg ready;
    reg req;
    reg running;
    reg [7:0] reqCount;
    reg [24:0] timer;

    dfd_0 dfdUT(CLOCK_50, running, done,
        1,
        req, ack,
        value_0, value_1, value_2,
		value_0_valid, value_1_valid, value_2_valid);

    initial running = 0;
    assign LED = 8'd0;
    assign done = 1;

    always @(posedge CLOCK_50) begin
        if(running) begin
            if(ack & value_2_valid) begin
                req <= 1'b0;
                reqCount <= 8'd0;

            end else begin
                if(reqCount < 10) begin
                    reqCount <= reqCount + 8'd1;
                end else begin
                    reqCount <= 8'd0;
                    req <= ~req;
                end
            end

        end else begin
            req <= 1'b0;
            reqCount <= 8'd0;
        end

        timer <= timer + 25'd1;
        if(timer == 25'd0)
            running <= ~running;

        if(timer == 25'd0 && ~running)
            req <= 1'b1;
    end
endmodule

