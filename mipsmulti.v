//-------------------------------------------------------
// Multicycle MIPS processor
//------------------------------------------------

module mips(input        clk, reset,
            output [31:0] adr, writedata,
            output        memwrite,
            input [31:0] readdata);

  wire        zero, pcen, irwrite, regwrite,
               alusrca, iord, memtoreg, regdst;
  wire [1:0]  alusrcb, pcsrc;
  wire [2:0]  alucontrol;
  wire [5:0]  op, funct;

  controller c(clk, reset, op, funct, zero,
               pcen, memwrite, irwrite, regwrite,
               alusrca, iord, memtoreg, regdst, 
               alusrcb, pcsrc, alucontrol);
  datapath dp(clk, reset, 
              pcen, irwrite, regwrite,
              alusrca, iord, memtoreg, regdst,
              alusrcb, pcsrc, alucontrol,
              op, funct, zero,
              adr, writedata, readdata);
endmodule

// Todo: Implement controller module
module controller(input       clk, reset,
                  input [5:0] op, funct,
                  input       zero,
                  output       pcen, memwrite, irwrite, regwrite,
                  output       alusrca, iord, memtoreg, regdst,
                  output [1:0] alusrcb, pcsrc,
                  output [2:0] alucontrol);

wire [1:0] aluop;
maindec md(clk, reset, op, zero, iord, memwrite,  memtoreg, pcen, irwrite, alusrca, alusrcb, regdst, regwrite, aluop);
aludec ad(funct, aluop, alucontrol);
 
endmodule

// Todo: Implement datapath
module datapath(input        clk, reset,
                input        pcen, irwrite, regwrite,
                input        alusrca, iord, memtoreg, regdst,
                input [1:0]  alusrcb, pcsrc, 
                input [2:0]  alucontrol,
                output [5:0]  op, funct,
                output        zero,
                output [31:0] adr, writedata, 
                input [31:0] readdata);
    

endmodule

module maindec(input clk, reset,
                input [5:0]op, 
                input zero, 
                output reg iord, memwrite,  memtoreg, pcen, irwrite, alusrca, 
                output reg [1:0]alusrcb,
                output reg regdst, regwrite, aluop);

    wire branch;
    wire pcwrite;
    assign pcen = (branch & zero) | pcwrite;

    always @(posedge clk) begin
        
    end

endmodule

module aludec(input  [5:0] funct,
input  [1:0] aluop,
output reg [2:0] alucontrol);
always @*
case(aluop)
2'b00: alucontrol <= 3'b010; // add (for lw/sw/addi)
2'b01: alucontrol <= 3'b110; // sub (for beq)
2'b11: alucontrol <= 3'b001; // or (for ori)
default: case(funct) // R-type instructions
6'b100000: alucontrol <= 3'b010; // add
6'b100010: alucontrol <= 3'b110; // sub
6'b100100: alucontrol <= 3'b000; // and
6'b100101: alucontrol <= 3'b001; // or
6'b101010: alucontrol <= 3'b111; // slt
default: alucontrol <= 3'bxxx; // ???
endcase
endcase
endmodule

module adder(input  [31:0] a, b,
output  [31:0] y);
assign y = a + b;
endmodule

module sl2(input  [31:0] a,
output  [31:0] y);
// shift left by 2
assign y = {a[29:0], 2'b00};
endmodule

module signext(input  [15:0] a,
output  [31:0] y);
assign y = {{16{a[15]}}, a};
endmodule

module flopr #(parameter WIDTH = 8)
(input  clk, reset,
input  [WIDTH-1:0] d,
output reg [WIDTH-1:0] q);
always @(posedge clk, posedge reset)
if (reset) q <= 0;
else q <= d;
endmodule

module mux2 #(parameter WIDTH = 8)
(input  [WIDTH-1:0] d0, d1,
input  s,
output  [WIDTH-1:0] y);
assign y = s ? d1 : d0;
endmodule

module mux4 #(parameter WIDTH = 8)
(
    input [WIDTH - 1:0] d0, d1, d2, d3,              
    input [1:0] sel,               
    output [WIDTH - 1:0] out);
    assign out = sel[1] ? (sel[0] ? d3 : d2) : (sel[0] ? d1 : d0);
endmodule
//We combine the zero extend and the mux, if aluop is 11 then output zeroextend else output srcb
module zeroextend(input [15:0]a,
                input [2:0]alucontrol,
                input [31:0]srcb,
                output [31:0]b);
    assign b = (alucontrol == 3'b001) ? {{16{1'b0}}, a} : {srcb};
endmodule