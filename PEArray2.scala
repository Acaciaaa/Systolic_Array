package PEArray2

import chisel3._
import chisel3.util._
import chisel3.experimental.ChiselEnum

class PE[T<:Bits with Num[T]](dtype:T)extends Module{
  val a_in = IO(Input(dtype))
  val a_out = IO(Output(dtype))
  val b_in = IO(Input(dtype))
  val b_out = IO(Output(dtype))
  val control = IO(Input(Bool()))

  def stationaryInput(din:T, dout:T):T={
    val r = Reg(dtype)
    when(control){r := din}.otherwise{r := r}
    dout := r
    r
  }
  def multicastInput(din:T, dout:T):T={
    dout := din
    din
  }

  val data_A = stationaryInput(a_in, a_out)
  val data_B = multicastInput(b_in, b_out)
  val compute_result = IO(Output(dtype))
  compute_result := data_A * data_B
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
  
  def stationaryInputConnectA(PEs:Seq[Seq[PE[T]]]):Unit={
    for(i<-0 until n)
      PEs(0)(i).a_in := a_in(i)
    for(i<-0 until n-1; j<-0 until n)
      PEs(i+1)(j).a_in := PEs(i)(j).a_out
  }
  def multicastInputConnectB(PEs:Seq[Seq[PE[T]]], dx:Int, dy:Int):Unit={
    for(i<-0 until n)
      PEs(0)(i).b_in := b_in(i)
    for(i<-0 until n; j<-0 until n)
      if(0<=i+dx && i+dx<n && 0<=j+dy && j+dy<n){
        PEs(i+dx)(j+dy).b_in := PEs(i)(j).b_out
      }
  }
  def multicastOutputConnectC(PEs:Seq[Seq[PE[T]]]):Unit={
    for(i<-0 until n){
        val r = Reg(dtype)
        r := c_in(i) + adderTree(PEs(i))
        c_out(i) := r
    }
  }
   def adderTree(a:Seq[PE[T]]):T={
    val b = Reg(Vec(2, dtype))
    for (i<-0 until 4 by 2) 
        b(i/2) := a(i).compute_result + a(i+1).compute_result
    val c = Reg(dtype)
    c := b(0) + b(1)
    c
    }

  stationaryInputConnectA(PEs)
  multicastInputConnectB(PEs, 1, 0)
  multicastOutputConnectC(PEs)
}
