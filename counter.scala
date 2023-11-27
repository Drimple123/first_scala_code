import chisel3._
import chisel3.util._

class Counter extends Module {
    val io = IO(new Bundle {
        val wb_valid = Input(UInt(8.W))
        val count = Output(UInt(8.W))
    })
    
    val cntReg = RegInit(0.U(8.W))
    when(io.wb_valid===1.U) {
    cntReg := cntReg + 1.U
}
    io.count := cntReg
}

