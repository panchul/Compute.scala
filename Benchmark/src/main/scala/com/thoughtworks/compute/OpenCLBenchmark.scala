package com.thoughtworks.compute

import java.nio.ByteBuffer

import com.dongxiguo.fastring.Fastring
import com.dongxiguo.fastring.Fastring.Implicits._
import com.thoughtworks.continuation._
import com.thoughtworks.feature.{Factory, ImplicitApply}
import com.thoughtworks.feature.mixins.ImplicitsSingleton
import com.thoughtworks.future._
import com.thoughtworks.raii.asynchronous._
import org.lwjgl.opencl.CL10._
import org.lwjgl.opencl.CLCapabilities
import org.lwjgl.system.MemoryStack._
import org.lwjgl.system.MemoryUtil._
import org.openjdk.jmh.annotations.{Benchmark, Param, Scope, State}
import shapeless.Witness

import scalaz.std.stream._
import scalaz.syntax.all._

/**
  * @author 杨博 (Yang Bo)
  */
object OpenCLBenchmark {

  trait TestKernels extends OpenCL with OpenCL.CommandQueuePool {

    @transient
    private lazy val compiledProgram: Program = {
      val program = createProgramWithSource(fastraw"""
      float sample(float* input, ptrdiff_t x, ptrdiff_t y, ptrdiff_t width, ptrdiff_t height) {
        if (x >= 0 && x < width && y >= 0 && y < height) {
          return input[y * width + x];
        } else {
          return 0.0f;
        }
      }

      kernel void benchmark(float* input, float* output, float* weight) {
        size_t x = get_global_id(0);
        size_t width = get_global_size(0);
        size_t y = get_global_id(1);
        size_t height = get_global_size(1);
        output[y * width + x] = ${(for {
        offsetX <- ConvolutionalKernelX
        offsetY <- ConvolutionalKernelY
      } yield fast"sample(input, x + ($offsetX), y + ($offsetY), width, height)").mkFastring(" + ")};
      }
      """)

      program.build()
      program

    }

    override def monadicClose: UnitContinuation[Unit] = {
      compiledProgram.monadicClose >> super.monadicClose
    }

    def test(input: DeviceBuffer[Float],
             output: DeviceBuffer[Float],
             weight: DeviceBuffer[Float],
             width: Int,
             height: Int): Future[Unit] = {
      Do.monadicCloseable(compiledProgram.firstKernel)
        .flatMap { kernel =>
          kernel(0) = input
          kernel(1) = output
          kernel(2) = weight
          val self: this.type = this
          kernel.enqueue(width, height)(Witness(self)).flatMap { event =>
            Do.garbageCollected(event.waitForComplete())
          }
        }
        .run

    }
  }

  final val ConvolutionalKernelX = -1 to 1
  final val ConvolutionalKernelY = -1 to 1
  final val ConvolutionalKernelSize: Int = ConvolutionalKernelX.length * ConvolutionalKernelY.length

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

    val numberOfCommandQueuesForDevice = { (deviceId: Long, capabilities: CLCapabilities) =>
      numberOfConcurrentLayers
    }
    val doOpenCL = Do.monadicCloseable {
      Factory[
        TestKernels with OpenCL with OpenCL.UseAllDevices with OpenCL.UseFirstPlatform with ImplicitsSingleton with OpenCL.CommandQueuePool]
        .newInstance(
          handleOpenCLNotification = handleOpenCLNotification,
          numberOfCommandQueuesForDevice = numberOfCommandQueuesForDevice
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
              width,
              height
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
