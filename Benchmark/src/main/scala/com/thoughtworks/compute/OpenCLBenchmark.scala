package com.thoughtworks.compute

import java.nio.ByteBuffer

import com.thoughtworks.continuation._
import com.thoughtworks.feature.Factory
import com.thoughtworks.feature.mixins.ImplicitsSingleton
import com.thoughtworks.future._
import com.thoughtworks.raii.asynchronous._
import org.lwjgl.opencl.CL10._
import org.openjdk.jmh.annotations.{Benchmark, Param, Scope, State}

import scalaz.std.stream._
import scalaz.syntax.all._

/**
  * @author 杨博 (Yang Bo)
  */
object OpenCLBenchmark {

  trait TestKernels extends OpenCL with OpenCL.CommandQueuePool {
    @transient
    private lazy val compiledProgram: Long = {
      // TODO: blocking compilation
      ???
    }

    override def monadicClose: UnitContinuation[Unit] = {
      UnitContinuation.delay {
        OpenCL.checkErrorCode(clReleaseProgram(compiledProgram))
      } >> super.monadicClose
    }

    def test(input: DeviceBuffer[Float], output: DeviceBuffer[Float], weight: DeviceBuffer[Float]): Future[Unit] = {
      ???
    }

  }

  val ConvolutionalKernelWeight = 3
  val ConvolutionalKernelHeight = 3
  val ConvolutionalKernelSize = ConvolutionalKernelWeight * ConvolutionalKernelHeight

  private val handleOpenCLNotification = { (errorInfo: String, buffer: ByteBuffer) =>
    if (buffer.remaining > 0) {
      val hexText = for (i <- (buffer.position until buffer.limit).view) yield {
        f"${buffer.get(i)}%02X"
      }
      Console.err.println(hexText.mkString(errorInfo, " ", ""))
    } else {
      Console.err.println(errorInfo)
    }
  }
}

@State(Scope.Benchmark)
class OpenCLBenchmark {
  import OpenCLBenchmark._

  @Param(Array("8", "32"))
  var width: Int = _
  @Param(Array("8", "32"))
  var height: Int = _

  @Param(Array("8", "128"))
  var batchSize: Int = _

  @Param(Array("32"))
  var numberOfConcurrentLayers: Int = _

  @Param(Array("256"))
  var totalLayers: Int = _

  @Benchmark
  def test(): Unit = {

    val doOpenCL = Do.monadicCloseable {
      Factory[
        TestKernels with OpenCL with OpenCL.UseAllDevices with OpenCL.UseFirstPlatform with ImplicitsSingleton with OpenCL.CommandQueuePool]
        .newInstance(
          handleOpenCLNotification = handleOpenCLNotification,
          numberOfCommandQueues = numberOfConcurrentLayers
        )
    }

    doOpenCL
      .flatMap { opencl2 =>
        val opencl = opencl2 // Workaround for https://github.com/milessabin/shapeless/issues/749
        import opencl.implicits._

        val sliceSize = width * height * batchSize

        for {
          inputDeviceBuffer <- opencl.allocateBuffer[Float](sliceSize * totalLayers)
          inputSlices <- (0 until totalLayers).toStream.traverse { i =>
            inputDeviceBuffer.slice(sliceSize * i, sliceSize)
          }
          outputDeviceBuffer <- opencl.allocateBuffer[Float](sliceSize * totalLayers)
          outputSlices <- (0 until totalLayers).toStream.traverse { i =>
            outputDeviceBuffer.slice(sliceSize * i, sliceSize)
          }
          weightDeviceBuffer <- opencl.allocateBuffer[Float](ConvolutionalKernelSize * totalLayers)
          weightSlices <- (0 until totalLayers).toStream.traverse { i =>
            weightDeviceBuffer.slice(sliceSize * i, sliceSize)
          }

          _ <- Do.garbageCollected((0 until totalLayers).toStream.traverse { layerIndex =>
            opencl.test(
              inputSlices(layerIndex),
              outputSlices(layerIndex),
              weightSlices(layerIndex),
            )
          })
        } yield {
          ()
        }

      }
      .run
      .blockingAwait
  }

}
