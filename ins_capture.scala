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
    val ret_good        = Output(Bool())
    //val is_jalr         = Output(Bool())
  })
  //define output duiyingde reg signal
  val is_jal_reg = RegInit(false.B)
  val is_ret_reg = RegInit(false.B)
  val addr_ret_reg = RegInit(0.U)
  val ret_good_reg = RegInit(true.B)
  //pipeline 2 stage
  val is_ret_reg_2 = RegNext(is_ret_reg,false.B) 
  val ra_reg = RegNext(io.ra,0.U)
  val ra_reg_2 = RegNext(ra_reg,0.U)

  val stack = Module(new Stack(64))
  stack.io.en := true.B
  stack.io.dataIn := io.ra 
  val pop_wire = WireDefault(false.B)
  val push_wire = WireDefault(false.B)
  stack.io.pop := pop_wire
  stack.io.push := push_wire
  addr_ret_reg := stack.io.dataOut

  //check is bu is jal or ret
  when(io.valid){
    when(((io.ins(6,0) === "b110_1111".U)||(io.ins(6,0) === "b110_0111".U))&&(io.ins =/= "h8067".U)){
      is_jal_reg := true.B 
      is_ret_reg := false.B
      pop_wire := false.B
      push_wire := true.B 
    }
    .elsewhen(io.ins === "h8067".U){// is ret instruction
      is_ret_reg := true.B 
      is_jal_reg := false.B
      pop_wire := true.B
      push_wire := false.B 
    }
    .otherwise{
      is_jal_reg := false.B 
      is_ret_reg := false.B 
      pop_wire := false.B
      push_wire := false.B 
    }
  }
  .otherwise{
    is_jal_reg := false.B
    is_ret_reg := false.B 
    pop_wire := false.B
    push_wire := false.B 
  }

  //compare ra and stack's
  when(is_ret_reg_2){
    when(ra_reg_2 === addr_ret_reg){
      ret_good_reg := true.B
    }
    .otherwise{
      ret_good_reg := false.B
    }
  }
  io.ret_good := ret_good_reg
}
