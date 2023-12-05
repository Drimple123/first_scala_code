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

class Addr_extract_jal extends Module{
  val io = IO(new Bundle{
    val ins     = Input(UInt(32.W))
    val addr3   = Output(SInt(40.W))
  })
  val addr2 = Wire(SInt(40.W))
  val addr1 = Wire(UInt(20.W))
  //sort the imm of the ins
  addr1 := Cat(Cat(io.ins(31),io.ins(19,12)),Cat(io.ins(20),io.ins(30,21)))
  //sign extend
  
  addr2 := (Cat(Fill(20,addr1(19)),addr1) << 1).asSInt
  io.addr3 := addr2
}

class Stack(val depth: Int) extends Module {
  val io = IO(new Bundle {
    val push    = Input(Bool())
    val pop     = Input(Bool())
    val en      = Input(Bool())
    val dataIn  = Input(UInt(40.W))
    val dataOut = Output(UInt(40.W))
    val full    = Output(Bool())
    val empty   = Output(Bool())
  })
 
  val stack_mem = Mem(depth, UInt(40.W))
  val sp = RegInit(0.U(log2Ceil(depth).W))
  //val sp_2 = RegInit(0.U(log2Ceil(depth).W))
  val out = RegInit(0.U(40.W))
  val full_reg = RegInit(false.B)
  val empty_reg = RegInit(true.B)
 
  when (io.en) {
    when(io.push) {
      when(sp < depth.asUInt){
        stack_mem(sp) := io.dataIn
        sp := sp + 1.U
        //sp_2 := sp
        empty_reg := false.B
    }
      .otherwise{
        full_reg := true.B
      }
    } 
    .elsewhen(io.pop) {
      when(sp > 0.U){
      out := stack_mem(sp-1.U)
      sp := sp - 1.U
      //sp:= sp_2
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

class Ins_capture extends Module {
  val io = IO(new Bundle {
    val pc              = Input(UInt(40.W))
    val valid           = Input(Bool())
    val ins             = Input(UInt(32.W))
    val ra              = Input(UInt(40.W))
    val pc_add4         = Output(UInt(40.W))
    val is_jal          = Output(Bool())
    val is_ret          = Output(Bool())
    val addr_jump       = Output(UInt(40.W))
    val addr_ret        = Output(UInt(40.W))
    val ret_good        = Output(Bool())
  })
  //define output duiyingde reg signal
  val is_jal_reg = RegInit(false.B)
  val is_ret_reg = RegInit(false.B)
  val pc_add4_reg = RegInit(0.U)
  val addr_jump_reg = RegInit(0.S)
  val addr_ret_reg = RegInit(0.U)
  val ret_good_reg = RegInit(true.B)
  //pipeline 2 stage
  val is_ret_reg_2 = RegInit(false.B) 
  is_ret_reg_2 := is_ret_reg
  val is_ret_reg_3 = RegInit(false.B) 
  is_ret_reg_3 := is_ret_reg_2
  val addr_ret_reg_2 = RegInit(0.U)
  addr_ret_reg_2 := addr_ret_reg
  val ra_reg = RegInit(0.U)
  ra_reg := io.ra
  val ra_reg_2 = RegInit(0.U)
  val ra_reg_3 = RegInit(0.U)
  ra_reg_2 := ra_reg
  ra_reg_3 := ra_reg_2
  
  val extract = Module(new Addr_extract_jal)

  val stack = Module(new Stack(8))
  stack.io.en := true.B
  stack.io.dataIn := io.pc_add4
  addr_ret_reg := stack.io.dataOut

  extract.io.ins := io.ins
  //check is bu is jal or ret
  when(io.valid){
    when(io.ins(6,0) === "b110_1111".U){
      is_jal_reg := true.B 
      is_ret_reg := false.B
      pc_add4_reg := io.pc + 4.U   
      
      addr_jump_reg := extract.io.addr3 + io.pc.asSInt
    }
    .elsewhen(io.ins === "b0000_0000_0000_0001_000_00000_1100111".U){
      is_ret_reg := true.B 
      is_jal_reg := false.B
    }
    .otherwise{
      is_jal_reg := false.B 
      is_ret_reg := false.B 
    }
  }
  .otherwise{
    is_jal_reg := false.B
    is_ret_reg := false.B 
  }

  io.pc_add4 := pc_add4_reg
  io.is_jal := is_jal_reg
  io.addr_jump := addr_jump_reg.asUInt
  io.is_ret := is_ret_reg

  //if jal, push ret_address,  if ret ,pop ret_address
  when(io.is_jal){
    stack.io.push := true.B 
    stack.io.pop := false.B 
    //printf(stack.io.en)
  }
  .elsewhen(io.is_ret){
    stack.io.pop := true.B 
    stack.io.push := false.B 
    //addr_ret_reg := stack.io.dataOut
  }
  .otherwise{
    stack.io.push := false.B
    stack.io.pop := false.B
  }
  //compare ra and stack's
  when(is_ret_reg_3){
    when(ra_reg_3 === addr_ret_reg){
      ret_good_reg := true.B
    }
    .otherwise{
      ret_good_reg := false.B
    }
  }
  io.ret_good := ret_good_reg
  io.addr_ret := addr_ret_reg
}

// object ins_capture_v extends App{
//   emitVerilog(new ins_capture, Array("--target-dir", "generated"))
// }
