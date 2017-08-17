package com.thoughtworks.compute

import java.nio.IntBuffer

import com.thoughtworks.feature.Factory.inject
import org.lwjgl.opencl.{CL, CLCapabilities}
import org.lwjgl.system.Pointer
import shapeless.Widen

import scala.reflect.ClassTag
import scala.util.hashing.MurmurHash3
import org.lwjgl.opencl._
import CL10._
import KHRICD._
import com.thoughtworks.compute.Closeables.{AssertionAutoCloseable, AssertionFinalizer}
import org.lwjgl.{BufferUtils, PointerBuffer}
import org.lwjgl.system.MemoryUtil._
import org.lwjgl.system.MemoryStack._
import org.lwjgl.system.Pointer._

import scala.collection.mutable
import com.thoughtworks.compute.Memory.Box
import org.lwjgl.system.jni.JNINativeInterface
import org.lwjgl.system._

import scala.util.control.Exception.Catcher
import scala.util.control.TailCalls
import scala.util.control.TailCalls.TailRec
import scala.util.{Failure, Success, Try}
import scalaz.{-\/, \/, \/-}
import com.thoughtworks.continuation._
import com.thoughtworks.feature.Factory
import com.thoughtworks.future._

/**
  * @example xxx
  *          {{{
  *          val openCL = Factory[OpenCL10].newInstance()
  *          }}}
  * @author 杨博 (Yang Bo)
  */
trait OpenCL10 extends Pointers {
  import com.thoughtworks.compute.OpenCL10._
  trait PlatformApi extends org.lwjgl.system.Pointer {

    @transient lazy val capabilities: CLCapabilities = CL.createPlatformCapabilities(address)

  }

  type Platform <: Pointer with PlatformApi

  protected def fromErrorCode(errorCode: Int): Exception = errorCode match {
    case CL_PLATFORM_NOT_FOUND_KHR          => new PlatformNotFoundKhr
    case CL_DEVICE_NOT_FOUND                => new DeviceNotFound
    case CL_DEVICE_NOT_AVAILABLE            => new DeviceNotAvailable
    case CL_COMPILER_NOT_AVAILABLE          => new CompilerNotAvailable
    case CL_MEM_OBJECT_ALLOCATION_FAILURE   => new MemObjectAllocationFailure
    case CL_OUT_OF_RESOURCES                => new OutOfResources
    case CL_OUT_OF_HOST_MEMORY              => new OutOfHostMemory
    case CL_PROFILING_INFO_NOT_AVAILABLE    => new ProfilingInfoNotAvailable
    case CL_MEM_COPY_OVERLAP                => new MemCopyOverlap
    case CL_IMAGE_FORMAT_MISMATCH           => new ImageFormatMismatch
    case CL_IMAGE_FORMAT_NOT_SUPPORTED      => new ImageFormatNotSupported
    case CL_BUILD_PROGRAM_FAILURE           => new BuildProgramFailure
    case CL_MAP_FAILURE                     => new MapFailure
    case CL_INVALID_VALUE                   => new InvalidValue
    case CL_INVALID_DEVICE_TYPE             => new InvalidDeviceType
    case CL_INVALID_PLATFORM                => new InvalidPlatform
    case CL_INVALID_DEVICE                  => new InvalidDevice
    case CL_INVALID_CONTEXT                 => new InvalidContext
    case CL_INVALID_QUEUE_PROPERTIES        => new InvalidQueueProperties
    case CL_INVALID_COMMAND_QUEUE           => new InvalidCommandQueue
    case CL_INVALID_HOST_PTR                => new InvalidHostPtr
    case CL_INVALID_MEM_OBJECT              => new InvalidMemObject
    case CL_INVALID_IMAGE_FORMAT_DESCRIPTOR => new InvalidImageFormatDescriptor
    case CL_INVALID_IMAGE_SIZE              => new InvalidImageSize
    case CL_INVALID_SAMPLER                 => new InvalidSampler
    case CL_INVALID_BINARY                  => new InvalidBinary
    case CL_INVALID_BUILD_OPTIONS           => new InvalidBuildOptions
    case CL_INVALID_PROGRAM                 => new InvalidProgram
    case CL_INVALID_PROGRAM_EXECUTABLE      => new InvalidProgramExecutable
    case CL_INVALID_KERNEL_NAME             => new InvalidKernelName
    case CL_INVALID_KERNEL_DEFINITION       => new InvalidKernelDefinition
    case CL_INVALID_KERNEL                  => new InvalidKernel
    case CL_INVALID_ARG_INDEX               => new InvalidArgIndex
    case CL_INVALID_ARG_VALUE               => new InvalidArgValue
    case CL_INVALID_ARG_SIZE                => new InvalidArgSize
    case CL_INVALID_KERNEL_ARGS             => new InvalidKernelArgs
    case CL_INVALID_WORK_DIMENSION          => new InvalidWorkDimension
    case CL_INVALID_WORK_GROUP_SIZE         => new InvalidWorkGroupSize
    case CL_INVALID_WORK_ITEM_SIZE          => new InvalidWorkItemSize
    case CL_INVALID_GLOBAL_OFFSET           => new InvalidGlobalOffset
    case CL_INVALID_EVENT_WAIT_LIST         => new InvalidEventWaitList
    case CL_INVALID_EVENT                   => new InvalidEvent
    case CL_INVALID_OPERATION               => new InvalidOperation
    case CL_INVALID_BUFFER_SIZE             => new InvalidBufferSize
    case CL_INVALID_GLOBAL_WORK_SIZE        => new InvalidGlobalWorkSize
    case _                                  => new UnknownErrorCode(errorCode)
  }

  protected def checkErrorCode(errorCode: Int): Unit = {
    errorCode match {
      case CL_SUCCESS =>
      case _          => throw fromErrorCode(errorCode)
    }
  }

  @inject
  protected val platformFactory: Factory[Platform]

  @inject
  protected def platformFactoryConstructor: platformFactory.Constructor <:< (IntPtr => Platform)

  final def platforms: Seq[Platform] = {
    val Array(numberOfPlatformIDs) = {
      val a = Array(0)
      checkErrorCode(clGetPlatformIDs(null, a))
      a
    }
    val stack = stackPush()
    try {
      val platformIdBuffer = stack.mallocPointer(numberOfPlatformIDs)
      checkErrorCode(clGetPlatformIDs(platformIdBuffer, null: IntBuffer))
      for (i <- 0 until platformIdBuffer.remaining) yield {
        val platformId = platformIdBuffer.get(i)
        val platformCapabilities = CL.createPlatformCapabilities(platformId)
        Platform(platformId)
      }
    } finally {
      stack.close()
    }
  }

  object Platform {
    def apply(platformId: IntPtr): Platform = {
      platformFactoryConstructor(platformFactory.newInstance)(platformId)
    }

    def apply(platformId: Long): Platform = {
      apply(IntPtr(platformId))
    }
  }
}

object OpenCL10 {

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

}
