package com.thoughtworks.compute

import java.io.Closeable
import java.nio.{ByteBuffer, IntBuffer}
import java.util.concurrent.atomic.AtomicReference

import org.lwjgl.opencl._
import CL10._
import CL12._
import CL11._
import CL20._
import KHRICD._
import org.lwjgl.{BufferUtils, PointerBuffer}
import org.lwjgl.system.MemoryUtil._
import org.lwjgl.system.MemoryStack._
import org.lwjgl.system.Pointer._

import scala.collection.mutable
import com.thoughtworks.compute.Memory.Box
import com.thoughtworks.compute.OpenCL.checkErrorCode
import org.lwjgl.system.jni.JNINativeInterface
import org.lwjgl.system._

import scala.util.control.Exception.Catcher
import scala.util.control.{NonFatal, TailCalls}
import scala.util.control.TailCalls.TailRec
import scala.util.{Failure, Success, Try}
import scalaz.{-\/, \/, \/-}
import scalaz.syntax.all._
import com.thoughtworks.continuation._
import com.thoughtworks.feature.mixins.ImplicitsSingleton
import com.thoughtworks.future._
import com.thoughtworks.raii.asynchronous._
import com.thoughtworks.raii.covariant._
import com.thoughtworks.tryt.covariant._
import shapeless.Witness

import scala.language.higherKinds

/**
  * @author 杨博 (Yang Bo)
  */
object OpenCL {

  @volatile
  var defaultLogger: (String, ByteBuffer) => Unit = { (errorInfo: String, data: ByteBuffer) =>
    // TODO: Add a test for in the case that Context is closed
    Console.err.println(raw"""An OpenCL notify comes out after its corresponding handler is freed
  message: $errorInfo
  data: $data""")
  }
  private val contextCallback: CLContextCallback = CLContextCallback.create(new CLContextCallbackI {
    override def invoke(errInfo: Long, privateInfo: Long, size: Long, userData: Long): Unit = {
      val errorInfo = memASCII(errInfo)
      val data = memByteBuffer(privateInfo, size.toInt)
      memGlobalRefToObject[OpenCL](userData) match {
        case null =>
          defaultLogger(memASCII(errInfo), memByteBuffer(privateInfo, size.toInt))
        case opencl =>
          if (size.isValidInt) {
            opencl.handleOpenCLNotification(memASCII(errInfo), memByteBuffer(privateInfo, size.toInt))
          } else {
            throw new IllegalArgumentException(s"numberOfBytes($size) is too large")
          }
      }
    }
  })
  object Exceptions {

    final class PlatformNotFoundKhr extends IllegalStateException

    final class DeviceNotFound extends IllegalArgumentException

    final class DeviceNotAvailable extends IllegalStateException

    final class CompilerNotAvailable extends IllegalStateException

    final class MemObjectAllocationFailure extends IllegalStateException

    final class OutOfResources extends IllegalStateException

    final class OutOfHostMemory extends IllegalStateException

    final class ProfilingInfoNotAvailable extends IllegalStateException

    final class MemCopyOverlap extends IllegalStateException

    final class ImageFormatMismatch extends IllegalStateException

    final class ImageFormatNotSupported extends IllegalStateException

    final class BuildProgramFailure extends IllegalStateException

    final class MapFailure extends IllegalStateException

    final class InvalidValue extends IllegalArgumentException

    final class InvalidDeviceType extends IllegalArgumentException

    final class InvalidPlatform extends IllegalArgumentException

    final class InvalidDevice extends IllegalArgumentException

    final class InvalidContext extends IllegalArgumentException

    final class InvalidQueueProperties extends IllegalArgumentException

    final class InvalidCommandQueue extends IllegalArgumentException

    final class InvalidHostPtr extends IllegalArgumentException

    final class InvalidMemObject extends IllegalArgumentException

    final class InvalidImageFormatDescriptor extends IllegalArgumentException

    final class InvalidImageSize extends IllegalArgumentException

    final class InvalidSampler extends IllegalArgumentException

    final class InvalidBinary extends IllegalArgumentException

    final class InvalidBuildOptions extends IllegalArgumentException

    final class InvalidProgram extends IllegalArgumentException

    final class InvalidProgramExecutable extends IllegalArgumentException

    final class InvalidKernelName extends IllegalArgumentException

    final class InvalidKernelDefinition extends IllegalArgumentException

    final class InvalidKernel extends IllegalArgumentException

    final class InvalidArgIndex extends IllegalArgumentException

    final class InvalidArgValue extends IllegalArgumentException

    final class InvalidArgSize extends IllegalArgumentException

    final class InvalidKernelArgs extends IllegalArgumentException

    final class InvalidWorkDimension extends IllegalArgumentException

    final class InvalidWorkGroupSize extends IllegalArgumentException

    final class InvalidWorkItemSize extends IllegalArgumentException

    final class InvalidGlobalOffset extends IllegalArgumentException

    final class InvalidEventWaitList extends IllegalArgumentException

    final class InvalidEvent extends IllegalArgumentException

    final class InvalidOperation extends IllegalArgumentException

    final class InvalidBufferSize extends IllegalArgumentException

    final class InvalidGlobalWorkSize extends IllegalArgumentException

    final class UnknownErrorCode(errorCode: Int) extends IllegalStateException(s"Unknown error code: $errorCode")

    def fromErrorCode(errorCode: Int): Exception = errorCode match {
      case CL_PLATFORM_NOT_FOUND_KHR          => new Exceptions.PlatformNotFoundKhr
      case CL_DEVICE_NOT_FOUND                => new Exceptions.DeviceNotFound
      case CL_DEVICE_NOT_AVAILABLE            => new Exceptions.DeviceNotAvailable
      case CL_COMPILER_NOT_AVAILABLE          => new Exceptions.CompilerNotAvailable
      case CL_MEM_OBJECT_ALLOCATION_FAILURE   => new Exceptions.MemObjectAllocationFailure
      case CL_OUT_OF_RESOURCES                => new Exceptions.OutOfResources
      case CL_OUT_OF_HOST_MEMORY              => new Exceptions.OutOfHostMemory
      case CL_PROFILING_INFO_NOT_AVAILABLE    => new Exceptions.ProfilingInfoNotAvailable
      case CL_MEM_COPY_OVERLAP                => new Exceptions.MemCopyOverlap
      case CL_IMAGE_FORMAT_MISMATCH           => new Exceptions.ImageFormatMismatch
      case CL_IMAGE_FORMAT_NOT_SUPPORTED      => new Exceptions.ImageFormatNotSupported
      case CL_BUILD_PROGRAM_FAILURE           => new Exceptions.BuildProgramFailure
      case CL_MAP_FAILURE                     => new Exceptions.MapFailure
      case CL_INVALID_VALUE                   => new Exceptions.InvalidValue
      case CL_INVALID_DEVICE_TYPE             => new Exceptions.InvalidDeviceType
      case CL_INVALID_PLATFORM                => new Exceptions.InvalidPlatform
      case CL_INVALID_DEVICE                  => new Exceptions.InvalidDevice
      case CL_INVALID_CONTEXT                 => new Exceptions.InvalidContext
      case CL_INVALID_QUEUE_PROPERTIES        => new Exceptions.InvalidQueueProperties
      case CL_INVALID_COMMAND_QUEUE           => new Exceptions.InvalidCommandQueue
      case CL_INVALID_HOST_PTR                => new Exceptions.InvalidHostPtr
      case CL_INVALID_MEM_OBJECT              => new Exceptions.InvalidMemObject
      case CL_INVALID_IMAGE_FORMAT_DESCRIPTOR => new Exceptions.InvalidImageFormatDescriptor
      case CL_INVALID_IMAGE_SIZE              => new Exceptions.InvalidImageSize
      case CL_INVALID_SAMPLER                 => new Exceptions.InvalidSampler
      case CL_INVALID_BINARY                  => new Exceptions.InvalidBinary
      case CL_INVALID_BUILD_OPTIONS           => new Exceptions.InvalidBuildOptions
      case CL_INVALID_PROGRAM                 => new Exceptions.InvalidProgram
      case CL_INVALID_PROGRAM_EXECUTABLE      => new Exceptions.InvalidProgramExecutable
      case CL_INVALID_KERNEL_NAME             => new Exceptions.InvalidKernelName
      case CL_INVALID_KERNEL_DEFINITION       => new Exceptions.InvalidKernelDefinition
      case CL_INVALID_KERNEL                  => new Exceptions.InvalidKernel
      case CL_INVALID_ARG_INDEX               => new Exceptions.InvalidArgIndex
      case CL_INVALID_ARG_VALUE               => new Exceptions.InvalidArgValue
      case CL_INVALID_ARG_SIZE                => new Exceptions.InvalidArgSize
      case CL_INVALID_KERNEL_ARGS             => new Exceptions.InvalidKernelArgs
      case CL_INVALID_WORK_DIMENSION          => new Exceptions.InvalidWorkDimension
      case CL_INVALID_WORK_GROUP_SIZE         => new Exceptions.InvalidWorkGroupSize
      case CL_INVALID_WORK_ITEM_SIZE          => new Exceptions.InvalidWorkItemSize
      case CL_INVALID_GLOBAL_OFFSET           => new Exceptions.InvalidGlobalOffset
      case CL_INVALID_EVENT_WAIT_LIST         => new Exceptions.InvalidEventWaitList
      case CL_INVALID_EVENT                   => new Exceptions.InvalidEvent
      case CL_INVALID_OPERATION               => new Exceptions.InvalidOperation
      case CL_INVALID_BUFFER_SIZE             => new Exceptions.InvalidBufferSize
      case CL_INVALID_GLOBAL_WORK_SIZE        => new Exceptions.InvalidGlobalWorkSize
      case _                                  => new Exceptions.UnknownErrorCode(errorCode)
    }

  }

  def checkErrorCode(errorCode: Int): Unit = {
    errorCode match {
      case CL_SUCCESS =>
      case _          => throw Exceptions.fromErrorCode(errorCode)
    }
  }

  trait UseFirstPlatform {
    @transient @volatile
    protected lazy val platformId: Long = {
      val stack = stackPush()
      try {
        val platformIdBuffer = stack.mallocPointer(1)
        checkErrorCode(clGetPlatformIDs(platformIdBuffer, null: IntBuffer))
        platformIdBuffer.get(0)
      } finally {
        stack.close()
      }
    }
  }

  private def deviceIdsByType(platformId: Long, deviceType: Int): Seq[Long] = {
    val Array(numberOfDevices) = {
      val a = Array(0)
      checkErrorCode(clGetDeviceIDs(platformId, deviceType, null, a))
      a
    }
    val stack = stackPush()
    try {
      val deviceIds = stack.mallocPointer(numberOfDevices)
      checkErrorCode(clGetDeviceIDs(platformId, deviceType, deviceIds, null: IntBuffer))
      for (i <- 0 until deviceIds.capacity()) yield {
        val deviceId = deviceIds.get(i)
        deviceId
      }
    } finally {
      stack.close()
    }
  }

  trait UseAllDevices {

    protected val platformId: Long

    @transient @volatile
    protected lazy val deviceIds: Seq[Long] = {
      deviceIdsByType(platformId, CL_DEVICE_TYPE_ALL)
    }

  }

  object CommandQueuePool {
    sealed trait State
  }

  trait CommandQueuePool {
    // TODO: write buffer
    // TODO: read buffer

    // TODO: enqueue kernel
    // TODO: TDD
    protected def commandQueue: Do[Long] = {
      ???
    }

    protected val context: Long

    private val state: AtomicReference[CommandQueuePool.State] = ???

    protected val numberOfCommandQueues: Int

  }

  /** A [[https://www.khronos.org/registry/OpenCL/sdk/2.1/docs/man/xhtml/abstractDataTypes.html cl_mem]]
    * whose [[org.lwjgl.opencl.CL10.CL_MEM_TYPE CL_MEM_TYPE]] is buffer [[org.lwjgl.opencl.CL10.CL_MEM_OBJECT_BUFFER CL_MEM_OBJECT_BUFFER]].
    * @param handle The underlying `cl_mem`.
    */
  final case class DeviceBuffer[Owner <: Singleton, Element](handle: Long) extends AnyVal {
    def slice(offset: Int, size: Int)(implicit
                                      memory: Memory[Element]): Do[DeviceBuffer[Owner, Element]] = {

      val bufferContinuation: UnitContinuation[Resource[UnitContinuation, Success[DeviceBuffer[Owner, Element]]]] =
        UnitContinuation.delay {
          val newHandle = {
            val stack = stackPush()
            try {
              val errorCode = stack.ints(0)
              val region = CLBufferRegion.mallocStack(stack)
              region.set(offset.toLong * memory.numberOfBytesPerElement, size.toLong * memory.numberOfBytesPerElement)
              val newHandle = nclCreateSubBuffer(handle,
                                                 CL_MEM_READ_WRITE,
                                                 CL_BUFFER_CREATE_TYPE_REGION,
                                                 region.address(),
                                                 memAddress(errorCode))
              checkErrorCode(errorCode.get(0))
              newHandle
            } finally {
              stack.close()
            }
          }
          Resource(value = Success(DeviceBuffer[Owner, Element](newHandle)), release = UnitContinuation.delay {
            checkErrorCode(clReleaseMemObject(newHandle))
          })
        }
      Do(TryT(ResourceT(bufferContinuation)))

    }

    def numberOfBytes: Int = {
      val sizeBuffer: Array[Long] = Array(0L)
      checkErrorCode(clGetMemObjectInfo(handle, CL_MEM_SIZE, sizeBuffer, null))
      val Array(value) = sizeBuffer
      if (value.isValidInt) {
        value.toInt
      } else {
        throw new IllegalStateException(s"Buffer's numberOfBytes($value) is too large")
      }
    }

    def length(implicit memory: Memory[Element]): Int = numberOfBytes / memory.numberOfBytesPerElement

    def read[HostBuffer](hostBuffer: HostBuffer)(implicit witnessOwner: Witness.Aux[Owner],
                                                 memory: Memory.Aux[Element, HostBuffer]) = {
      ???
    }

    def toHostBuffer(implicit witnessOwner: Witness.Aux[Owner], memory: Memory[Element]): Do[memory.HostBuffer] = {
      Do(TryT(ResourceT(UnitContinuation.delay {
        val hostBuffer = memory.allocate(length)
        Resource(value = Success(hostBuffer), release = UnitContinuation.delay { memory.free(hostBuffer) })
      }))).map { hostBuffer =>
        ???
        hostBuffer
      }
    }
  }

}

trait OpenCL extends MonadicCloseable[UnitContinuation] with ImplicitsSingleton {

  def monadicClose: UnitContinuation[Unit] = UnitContinuation.delay {
    checkErrorCode(clReleaseContext(context))
  }

  protected def handleOpenCLNotification(errorInfo: String, privateInfo: ByteBuffer): Unit

  import OpenCL._

  protected val platformId: Long
  protected val deviceIds: Seq[Long]

  @transient @volatile
  protected lazy val context: Long = {
    val stack = stackPush()
    try {
      val errorCodeBuffer = stack.ints(CL_SUCCESS)
      val contextProperties = stack.pointers(CL_CONTEXT_PLATFORM, platformId, 0)
      val deviceIdBuffer = stack.pointers(deviceIds: _*)
      val context =
        clCreateContext(contextProperties,
                        deviceIdBuffer,
                        OpenCL.contextCallback,
                        JNINativeInterface.NewWeakGlobalRef(this),
                        errorCodeBuffer)
      checkErrorCode(errorCodeBuffer.get(0))
      context
    } finally {
      stack.close()
    }
  }
  trait ImplicitsApi {}
  type Implicits <: ImplicitsApi

  val implicits: Implicits

  type DeviceBuffer[Element] = OpenCL.DeviceBuffer[this.type, Element]

  /** Returns an uninitialized buffer of `Element` on device.
    */
  def allocateBuffer[Element: Memory](size: Long): Do[DeviceBuffer[Element]] = ???

  /** Returns a buffer of `Element` on device whose content is copied from `hostBuffer`.
    */
  def allocateBufferFrom[Element, HostBuffer](hostBuffer: HostBuffer)(
      implicit memory: Memory.Aux[Element, HostBuffer]): Do[DeviceBuffer[Element]] = ???

}
//  {
//  def platforms: Seq[Platform] = {
//    val Array(numberOfPlatformIDs) = {
//      val a = Array(0)
//      checkErrorCode(clGetPlatformIDs(null, a))
//      a
//    }
//    val stack = stackPush()
//    try {
//      val platformIdBuffer = stack.mallocPointer(numberOfPlatformIDs)
//      checkErrorCode(clGetPlatformIDs(platformIdBuffer, null: IntBuffer))
//      for (i <- (0 until platformIdBuffer.capacity).view) yield {
//        val platformId = platformIdBuffer.get(i)
//        val platformCapabilities = CL.createPlatformCapabilities(platformId)
//        Platform(platformId, platformCapabilities)
//      }
//    } finally {
//      stack.close()
//    }
//  }
//
//  final case class Device private[OpenCL] (id: Long, capabilities: CLCapabilities) {
//
//    def maxWorkItemDimensions: Int = intInfo(CL_DEVICE_MAX_WORK_ITEM_DIMENSIONS)
//
//    def maxWorkItemSizes: PointerBuffer = {
//      val size = maxWorkItemDimensions
//      val buffer = BufferUtils.createPointerBuffer(size)
//      checkErrorCode(clGetDeviceInfo(id, CL_DEVICE_MAX_WORK_ITEM_SIZES, buffer, null))
//      buffer
//    }
//
//    def maxWorkGroupSize: Long = {
//      pointerInfo(CL_DEVICE_MAX_WORK_GROUP_SIZE)
//    }
//
//    def intInfo(paramName: Int): Int = {
//      val buffer = Array[Int](0)
//      checkErrorCode(clGetDeviceInfo(id, paramName, buffer, null))
//      val Array(value) = buffer
//      value
//    }
//
//    def pointerInfo(paramName: Int): Long = {
//      val stack = stackPush()
//      try {
//        val buffer = stack.mallocPointer(1)
//        checkErrorCode(clGetDeviceInfo(id, paramName, buffer, null))
//        buffer.get(0)
//      } finally {
//        stack.close()
//      }
//    }
//
//    def longInfo(paramName: Int): Long = {
//      val buffer = Array[Long](0L)
//      checkErrorCode(clGetDeviceInfo(id, paramName, buffer, null))
//      val Array(value) = buffer
//      value
//    }
//
//    /**
//      * Describes the command-queue properties supported by the device.
//      * @see [[org.lwjgl.opencl.CL10.CL_DEVICE_QUEUE_PROPERTIES]]
//      * @return
//      */
//    def queueProperties: Long = longInfo(CL_DEVICE_QUEUE_PROPERTIES)
//
//    def deviceType: Long = longInfo(CL_DEVICE_TYPE)
//  }
//
////  trait Context extends Pointer {
////
////    def createCommandQueuePool
////
////  }
//
//  override protected def finalize(): Unit = {
//    contextCallback.close()
//  }
//
//
//
//  final class Platform private[OpenCL] (id: Long, capabilities: CLCapabilities) {
//
//    def cpus: Seq[Device] = devicesByType(CL_DEVICE_TYPE_CPU)
//
//    def gpus: Seq[Device] = devicesByType(CL_DEVICE_TYPE_GPU)
//
//    def accelerators: Seq[Device] = devicesByType(CL_DEVICE_TYPE_ACCELERATOR)
//
//    def devices: Seq[Device] = devicesByType(CL_DEVICE_TYPE_ALL)
//
//    private def devicesByType(deviceType: Int): Seq[Device] = {
//      val Array(numberOfDevices) = {
//        val a = Array(0)
//        checkErrorCode(clGetDeviceIDs(Platform.this.id, deviceType, null, a))
//        a
//      }
//      val stack = stackPush()
//      try {
//        val deviceIds = stack.mallocPointer(numberOfDevices)
//        checkErrorCode(clGetDeviceIDs(Platform.this.id, deviceType, deviceIds, null: IntBuffer))
//        for (i <- 0 until deviceIds.capacity()) yield {
//          val deviceId = deviceIds.get(i)
//          val deviceCapabilities = CL.createDeviceCapabilities(deviceId, Platform.this.capabilities)
//          Device(deviceId, deviceCapabilities)
//        }
//      } finally {
//        stack.close()
//      }
//    }
//
//    trait Context {}
//
//    object Context {
//
//      def apply(logger: (String, ByteBuffer) => Unit, forDevices: Device*): Do[Context] = {
//        val continuation = UnitContinuation.delay {
//          val stack = stackPush()
//          try {
//            val errorCodeBuffer = stack.ints(CL_SUCCESS)
//            val contextProperties = stack.pointers(CL_CONTEXT_PLATFORM, Platform.this.id, 0)
//            val deviceIds = stack.pointers(devices.map(_.id): _*)
//            val loggerReference = JNINativeInterface.NewGlobalRef(logger)
//            val context =
//              clCreateContext(contextProperties, deviceIds, contextCallback, loggerReference, errorCodeBuffer)
//            checkErrorCode(errorCodeBuffer.get(0))
//            new Pointer.Default(context) with Context with Releasable[UnitContinuation, Success[Context]] {
//
//              override def value: Success[Context] = Success(this)
//
//              override def release: UnitContinuation[Unit] = UnitContinuation.delay {
//                checkErrorCode(clReleaseContext(address))
//                JNINativeInterface.DeleteGlobalRef(loggerReference)
//              }
//            }
//          } catch {
//            case NonFatal(e) =>
//              new Releasable[UnitContinuation, Failure[Context]] {
//                override def value: Failure[Context] = Failure(e)
//                override def release: UnitContinuation[Unit] = UnitContinuation.now(())
//              }
//          } finally {
//            stack.close()
//          }
//        }
//
//        Do(TryT(ResourceT(continuation)))
//
//      }
//
//    }
//
//  }
//
//}
