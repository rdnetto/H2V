module main(
    input CLOCK_50,
    input [1:0] KEY,
    output [7:0] LED
);

    //fib(clk,      ready,   done,  input, output)
    dfd_4(CLOCK_50, ~KEY[0], LED[7], 8'd5, LED[6:0]);

endmodule
