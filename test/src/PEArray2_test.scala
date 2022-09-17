package PEArray2

import chisel3._
import chiseltest._
import chisel3.experimental.VecLiterals._
import org.scalatest.flatspec.AnyFlatSpec

import scala.util.Random
import java.lang.System


class PEArrayTest2 extends AnyFlatSpec with ChiselScalatestTester{
  behavior of "PEArray2"


  it should "handle 4x4x4 case" in {
    val dtype = UInt(8.W)
    val (a, b, c) = makeData(8, 4, 4, 4)
    test(new PEArray(dtype)){ dut =>
      {
        loadStationaryData(dut, 8, a)
        fork{
          for (j<-0 until 4) {
            dut.b_in.poke(Vec.Lit((for (k<-0 until 4) yield b(k)(j).U(8.W)): _*))
            dut.c_in.poke(Vec.Lit((for (i<-0 until 4) yield 0.U(8.W)): _*))
            dut.clock.step(1)
          }
          dut.b_in.poke(Vec.Lit((for (i<-0 until 4) yield 0.U(8.W)): _*))
        }.fork{
          dut.clock.step(3)
          for (j<-0 until 4) {
            dut.c_out.expect(Vec.Lit((for (i<-0 until 4) yield c(i)(j).U(8.W)): _*))
            dut.clock.step(1)
          }
        }.join()
      }
    }
  }

  def makeData(w:Int, n:Int, m:Int, k:Int) = {
    val r = new Random(System.currentTimeMillis())
    val a = Seq.fill(n,k)(r.nextInt(1<<(w/2)))
    val b = Seq.fill(k,m)(r.nextInt(1<<(w/2)))
    val c = Seq.tabulate(n,m){case (i,j) => ((for (t <- 0 until k) yield a(i)(t) * b(t)(j)).sum) & ((1<<w)-1)}
    (a, b, c)
  }
  def loadStationaryData(dut: PEArray[UInt], w:Int, data:Seq[Seq[Int]]) = {
    dut.stationaryCtrl.poke(1.B)
    dut.b_in.poke(Vec.Lit((for (i<-0 until 4) yield 0.U(w.W)): _*))
    dut.c_in.poke(Vec.Lit((for (i<-0 until 4) yield 0.U(w.W)): _*))
    for (i<-3 to 0 by -1) {
        dut.a_in.poke(Vec.Lit((for (j<-0 until 4) yield data(i)(j).U(w.W)):_*))
        dut.clock.step(1)
    }
    dut.stationaryCtrl.poke(0.B)
  }
}