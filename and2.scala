package and2

import chisel3._

class and2 extends Module {
    val io = IO(new Bundle {
        val a = Input(UInt(2.W))
        val b = Input(UInt(2.W))
        val out = Output(UInt(2.W))
    })

    io.out := io.a & io.b
}