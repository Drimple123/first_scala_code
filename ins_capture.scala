package ins_capture

import chisel3._
import chisel3.util._

/**
  * Compute GCD using subtraction method.
  * Subtracts the smaller from the larger until register y is zero.
  * value in register x is then the GCD
  */

// class Adder extends Module {
//     val io = IO(new Bundle{
//         val a = Input(UInt(32.W))
//         val b = Output(UInt(32.W))
//     })
//     io.b := io.a + 4
// }

class addr_extract_jal extends Module{
  val io = IO(new Bundle{
    val ins     = Input(UInt(32.W))
    val addr3   = Output(SInt(32.W))
  })
  val addr2 = Wire(SInt(32.W))
  val addr1 = Wire(UInt(20.W))
  //sort the imm of the ins
  addr1 := Cat(Cat(io.ins(31),io.ins(19,12)),Cat(io.ins(20),io.ins(30,21)))
  //sign extend
  
  addr2 := (Cat(Fill(12,addr1(19)),addr1) << 1).asSInt
  io.addr3 := addr2
}

class stack(val depth: Int) extends Module {
  val io = IO(new Bundle {
    val push    = Input(Bool())
    val pop     = Input(Bool())
    val en      = Input(Bool())
    val dataIn  = Input(UInt(32.W))
    val dataOut = Output(UInt(32.W))
    val full    = Output(Bool())
    val empty   = Output(Bool())
  })
 
  val stack_mem = Mem(depth, UInt(32.W))
  val sp = RegInit(0.U(log2Ceil(depth).W))
  val out = RegInit(0.U(32.W))
  val full_reg = RegInit(false.B)
  val empty_reg = RegInit(true.B)
 
  when (io.en) {
    when(io.push) {
      when(sp < depth.asUInt){
        stack_mem(sp) := io.dataIn
        sp := sp + 1.U
        empty_reg := false.B
    }
      .otherwise{
        full_reg := true.B
      }
    } 
    .elsewhen(io.pop) {
      when(sp > 0.U){
      out := stack_mem(sp - 1.U)
      sp := sp - 1.U
      full_reg := false.B
      }
      .otherwise{
        empty_reg := true.B
      }
    }
  }
 
  io.dataOut := out
  io.full := full_reg
  io.empty := empty_reg
}

class ins_capture extends Module {
  val io = IO(new Bundle {
    val pc              = Input(UInt(32.W))
    val valid           = Input(Bool())
    val ins             = Input(UInt(32.W))
    val pc_add4         = Output(UInt(32.W))
    val is_jal          = Output(Bool())
    val addr_jump       = Output(UInt(32.W))
  })

  val is_jal_reg = RegInit(false.B)
  val pc_add4_reg = RegInit(0.U)
  val addr_jump_reg = RegInit(0.S)

  val extract = Module(new addr_extract_jal)

  val Stack = Module(new stack(8))
  Stack.io.en := false.B
  Stack.io.push := false.B 
  Stack.io.pop := false.B
  Stack.io.dataIn := 0.U

  extract.io.ins := io.ins

  when(io.valid){
    when(io.ins(6,0) === "b110_1111".U){
      is_jal_reg := true.B 
      pc_add4_reg := io.pc + 4.U   
      
      addr_jump_reg := extract.io.addr3 + io.pc.asSInt
    }
    .otherwise{
      is_jal_reg := false.B 
    }
  }
  .otherwise{
    is_jal_reg := false.B
  }
  io.pc_add4 := pc_add4_reg
  io.is_jal := is_jal_reg
  io.addr_jump := addr_jump_reg.asUInt

  when(io.is_jal){
    Stack.io.en := true.B
    Stack.io.push := true.B 
    Stack.io.dataIn := io.pc_add4
  }
  .otherwise{
    Stack.io.en := false.B
    Stack.io.push := false.B
  }
    
}
