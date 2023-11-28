package counter

import chisel3._

class Adder extends Module {
    val io = IO(new Bundle{
        val a = Input(UInt(8.W))
        val b = Input(UInt(8.W))
        val y = Output(UInt(8.W))
    })

    io.y := io.a + io.b
}

class Register extends Module{
    val io = IO(new Bundle{
        val d = Input(UInt(8.W))
        val q = Output(UInt(8.W))
    })
    val reg = RegInit(0.U)
    io.q := reg
    reg := io.d
}

class counter extends Module{
    val io = IO(new Bundle{
            val dout = Output(UInt(8.W))
        })

    val add = Module(new Adder)
    val reg = Module(new Register)

    val count = reg.io.q

    add.io.a := 1.U
    add.io.b := count
    val result = add.io.y

    val next = Mux(result === 10.U, 0.U, result)

    reg.io.d := next

    io.dout := count
}
