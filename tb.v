module testbench();
  reg clk;
  reg reset;
  wire [31:0] adr, writedata;
  wire memwrite;

  // instantiate device to be tested
  top dut (clk, reset, writedata, adr, mewrite);
  // initialize test
  initial
    begin
      reset <= 1; # 4; reset <= 0;
    end
  // generate clock to sequence tests
  always
    begin
      clk <= 1; # 5; clk <= 0; # 5;
    end
  // check results
  always @(negedge clk)
    begin
      if (memwrite) begin
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