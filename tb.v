module testbench();
reg clk;
reg reset = 1;
// instantiate device to be tested
top dut (clk, reset);
//initialize test
initial begin
    #2;
    reset <= ~reset;
  end
// generate clock to sequence tests
always
begin
clk <= 1; # 5; clk <= 0; # 5;
end
// check results
always @(negedge clk)
begin
    // #100
    // $stop;
if (dut.memwrite) begin
if (dut.dataadr === 84 & dut.writedata === -33022) begin
$display("Simulation succeeded");
$stop;
end else if (dut.dataadr !== 80) begin
$display("Simulation failed");
$stop;
end
end
end
endmodule