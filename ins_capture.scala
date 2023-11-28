package ins_capture

import chisel3._

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


class ins_capture extends Module {
  val io = IO(new Bundle {
    val pc              = Input(UInt(32.W))
    val valid           = Input(Bool())
    val ins             = Input(UInt(32.W))
    val pc_add4         = Output(UInt(32.W))
    val is_jal          = Output(Bool())
    //val addr_jump       = Output(UInt(32.W))
  })

  val is_jal_reg = RegInit(false.B)
  val pc_add4_reg = RegInit(0.U)
  when(io.valid){
    when(io.ins(6,0) === "b110_1111".U){
      is_jal_reg := true.B 
      pc_add4_reg := io.pc + 4.U     
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
}
