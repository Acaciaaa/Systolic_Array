package PEArray1

import chisel3._
import chisel3.util._
import chisel3.experimental.ChiselEnum

class PE[T<:Bits with Num[T]](dtype:T)extends Module{
  val a_in = IO(Input(dtype))
  val a_out = IO(Output(dtype))
  val b_in = IO(Input(dtype))
  val b_out = IO(Output(dtype))
  val c_in = IO(Input(dtype))
  val c_out = IO(Output(dtype))
  val control = IO(Input(Bool()))

  def systolicInput(din:T, dout:T):T={
    val r = Reg(dtype)
    r := din
    dout := r
    r
  }
  def stationaryInput(din:T, dout:T):T={
    val r = Reg(dtype)
    when(control){r := din}.otherwise{r := r}
    dout := r
    r
  }
  def systolicOutput(din:T, dout:T):Unit={
    val r = Reg(dtype)
    r := din + compute_result
    dout := r
  }

  val data_B = stationaryInput(b_in, b_out)
  val data_A = systolicInput(a_in, a_out)
  val compute_result = Wire(dtype)
  compute_result := data_A * data_B
  systolicOutput(c_in, c_out)
}

class PEArray[T<:Bits with Num[T]](dtype:T) extends Module{
  // TODO
  val n = 4
  val a_in = IO(Input(Vec(4, dtype)))
  val b_in = IO(Input(Vec(4, dtype)))
  val c_in = IO(Input(Vec(4, dtype)))
  val stationaryCtrl = IO(Input(Bool()))
  val c_out = IO(Output(Vec(4, dtype)))

  val PEs = for(i<-0 until n) yield
              for(j<-0 until n) yield
                Module(new PE(dtype))
  for(i<-0 until n; j<-0 until n)
    PEs(i)(j).control := stationaryCtrl
  
  def systolicInputConnectA(PEs:Seq[Seq[PE[T]]], dx:Int, dy:Int):Unit={
    for(i<-0 until n)
      PEs(0)(i).a_in := a_in(i)
    for(i<-0 until n; j<-0 until n)
      if(0<=i+dx && i+dx<n && 0<=j+dy && j+dy<n){
        PEs(i+dx)(j+dy).a_in := PEs(i)(j).a_out
      }
  }
  def stationaryInputConnectB(PEs:Seq[Seq[PE[T]]]):Unit={
    for(i<-0 until n)
      PEs(i)(0).b_in := b_in(i)
    for(i<-0 until n; j<-0 until n-1)
      PEs(i)(j+1).b_in := PEs(i)(j).b_out
  }
  def systolicOutputConnectC(PEs:Seq[Seq[PE[T]]], dx:Int, dy:Int):Unit={
    for(i<-0 until n)
      PEs(i)(0).c_in := c_in(i)
    for(i<-0 until n; j<-0 until n)
      if(0<=i+dx && i+dx<n && 0<=j+dy && j+dy<n){
        PEs(i+dx)(j+dy).c_in := PEs(i)(j).c_out
      }
    for(i<-0 until n)
      c_out(i) := PEs(i)(n-1).c_out
  }

  stationaryInputConnectB(PEs)
  systolicInputConnectA(PEs, 1, 0)
  systolicOutputConnectC(PEs, 0, 1)
}
