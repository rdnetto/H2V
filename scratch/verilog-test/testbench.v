module testbench(input CLOCK_50, input [1:0] KEY, output [7:0] LED);
    reg ready;
    reg req;
    reg running;
    reg [7:0] reqCount;
    reg [24:0] timer;

    dfd_5 dfdUT(CLOCK_50, running, done,
        -2, 4,
        req, ack, eol, value);

    initial running = 0;
    assign LED = 8'd0;
    assign done = 1;

    always @(posedge CLOCK_50) begin
        if(running) begin
            if(ack) begin
                req <= 0;
                reqCount <= 0;

            end else begin
                if(reqCount < 5) begin
                    reqCount <= reqCount + 1;
                end else begin
                    reqCount <= 0;
                    req <= ~req;
                end
            end

        end else begin
            req <= 0;
            reqCount <= 0;
        end

        timer <= timer + 1;
        if(timer == 0)
            running <= ~running;

        if(timer == 0 && ~running)
            req <= 1;
    end
endmodule

