module testbench();
reg clk;
reg reset = 1;
wire [31:0] writedata, adr;
wire memwrite;
// instantiate device to be tested
top dut (clk, reset, writedata, adr, memwrite);
//initialize test
initial begin
    #11;
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
      if (dut.memwrite) begin
        if (dut.adr === 84 & dut.writedata === 7) begin
          $display("Simulation succeeded");
          $stop;
        end else if (dut.adr !== 80) begin
          $display("Simulation failed");
          $stop;
        end
      end
    end
endmodule