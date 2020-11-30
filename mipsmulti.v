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
maindec md(clk, reset, op, zero, iord, memwrite,  memtoreg, pcen, irwrite, alusrca, alusrcb, regdst, regwrite, aluop, alucontrol);
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

    wire [31:0] pc1, pc2, aluout, instr, data, wd3. rd1, rd2, immext;
    wire [4:0] a3;

    flopren #(32) pc2(clk, reset, pcen, pc1, pc2);
    mux2 #(32) adrin(pc2, aluout, iord, adr);
    flopren #(32) instrin(clk, reset, irwrite, readdata, instr);
    flopr #(32) datain(clk, reset, readdata, data);
    op <= instr[31:26];
    funct <= instr[5:0];
    mux2 #(5) a3in(instr[20:16], instr[15:11], regdst, a3);
    mux2 #(32) wd3in(aluout, data, memtoreg, wd3);
    signext signextend(instr[15:0], immext);
    regfile registerfile(clk, regwrite, instr[25:21], instr[20:16], a3, rd1, rd2);
    
endmodule

module regfile(input  clk,
    input  we3,
    input  [4:0] ra1, ra2, wa3,
    input  [31:0] wd3,
    output  [31:0] rd1, rd2);
    reg [31:0] rf[31:0];
    // three ported register file
    // read two ports combinationally
    // write third port on rising edge of clk
    // register 0 hardwired to 0
    // note: for pipelined processor, write third port
    // on falling edge of clk
    always @(posedge clk)
        if (we3) rf[wa3] <= wd3;
        assign rd1 = (ra1 != 0) ? rf[ra1] : 0;
        assign rd2 = (ra2 != 0) ? rf[ra2] : 0;
endmodule

module maindec(input clk, reset,
                input [5:0]op, 
                input zero, 
                output reg iord, memwrite,  memtoreg, pcen, irwrite, alusrca, 
                output reg [1:0]alusrcb,
                output reg regdst, regwrite, aluop,
                output reg [2:0] alucontrol);

    wire branch;
    wire pcwrite;
    assign pcen = (branch & zero) | pcwrite;

    parameter FETCH = 4'b0000
    parameter DECODE = 4'b0001
    parameter MEMADR = 4'b0010
    parameter MEMREAD = 4'b0011
    parameter MEMWRITEBACK = 4'b0100
    parameter MEMWRITE = 4'b0101
    parameter EXECUTE = 4'b0110
    parameter ALUWRITEBACK = 4'b0111
    parameter BRANCH = 4'b1000
    parameter ADDIEXECUTE = 4'b1001
    parameter ADDIWRITEBACK = 4'b1010
    parameter JUMP = 4'b1011

    reg [3:0] = currstate, nextstate;

    always @(posedge clk) begin
        if (reset) currstate <= FETCH;
        else currstate <= nextstate;
    end
        always @(currstate or op) begin
            case(currstate)
                FETCH: nextstate <= DECODE;
                DECODE: case(op)
                        6'b100011: nextstate <= MEMADR;
                        6'b101011: nextstate <= MEMADR;
                        6'b000000: nextstate <= EXECUTE;
                        6'b000100: nextstate <= BRANCH;
                        6'b001000: nextstate <= ADDIEXECUTE;
                        6'b000010: nextstate <= JUMP;
                        default: nextstate <= FETCH;
                    endcase
                MEMADR: case(op)
                        6'b100011: nextstate <= MEMREAD;
                        6'b101011: nextstate <= MEMWRITE;
                        default: nextstate <= FETCH;
                        endcase
                MEMREAD: nextstate <= MEMWRITEBACK;
                MEMWRITEBACK: nextstate <= FETCH;
                MEMWRITE: nextstate <= FETCH;
                EXECUTE: nextstate <= ALUWRITEBACK;
                ALUWRITEBACK: nextstate <= FETCH;
                BRANCH: nextstate <= FETCH;
                ADDIEXECUTE: nextstate <= ADDIWRITEBACK;
                ADDIWRITEBACK: nextstate <= FETCH;
                JUMP: nextstate <= FETCH;
                default: nextstate <= FETCH;
            endcase
        end

        always @(currstate) begin
            iord = 1'b0;
            memwrite = 1'b0;
            irwrite = 1'b0;
            regdst = 1'b0;
            memtoreg = 1'b0;
            regwrite = 1'b0;
            alusrca = 1'b0;
            alusrcb = 2'b00;
            alucontrol = 3'b000;
            pcsrc = 2'b00;
            pcen = 1'b0;
            case (currstate)
                FETCH: begin
                    iord = 1'b0;
                    alusrca = 1'b0;
                    alusrcb = 2'b01;
                    aluop = 2'b00;
                    pcsrc = 2'b00;
                    irwrite = 1'b1;
                    pcwrite = 1'b1;
                end
                DECODE:begin
                    alusrca = 1'b0;
                    alusrcb = 2'b11;
                    aluop = 2'b00;
                end
                MEMADR: begin
                    alusrca = 1'b1;
                    alusrcb = 2'b10;
                    aluop = 2'b00;
                end
                MEMREAD: begin
                    iord = 1'b1;
                end
                MEMWRITEBACK: begin
                    regdst = 1'b0;
                    memtoreg = 1'b1;
                    regwrite = 1'b1;
                end
                MEMWRITE: begin
                    iord = 1'b1;
                    memwrite = 1'b1;
                end
                EXECUTE: begin
                    alusrca = 1'b1;
                    alusrcb = 2'b00;
                    aluop = 2'b10;
                end
                ALUWRITEBACK: begin
                    regdst = 1'b1;
                    memtoreg = 1'b0;
                    regwrite = 1'b1;
                end
                BRANCH: begin
                    alusrca = 1'b1;
                    alusrcb = 2'b00;
                    aluop = 2'b01;
                    pcsrc = 2'b01;
                    branch = 1'b1;
                end
                ADDIEXECUTE: begin
                    alusrca = 1'b1;
                    alusrcb = 2'b10;
                    aluop = 2'b00;
                end
                ADDIWRITEBACK: begin
                    regdst = 1'b0;
                    memtoreg = 1'b0;
                    regwrite = 1'b1;
                end
                JUMP: begin
                    pcsrc = 2'b10;
                    pcwrite = 1'b1;
                end
            endcase
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

module sl2 #(parameter WIDTH = 8)
    (input  [WIDTH - 1:0] a,
    output  [WIDTH - 1:0] y);
    // shift left by 2
    assign y = {a[WIDTH - 3:0], 2'b00};
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

module flopren #(parameter WIDTH = 8)
(input  clk, reset, en,
input  [WIDTH-1:0] d,
output reg [WIDTH-1:0] q);
always @(posedge clk, posedge reset)
if (reset) q <= 0;
else if (en) q <= d;
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

module zeroextend(input [15:0]a,
                input [2:0]alucontrol,
                input [31:0]srcb,
                output [31:0]b);
    assign b = (alucontrol == 3'b001) ? {{16{1'b0}}, a} : {srcb};
endmodule