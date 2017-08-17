package com.thoughtworks.compute

import java.nio.IntBuffer

import org.lwjgl.opencl.CL
import org.lwjgl.opencl.CL10._
import org.lwjgl.opencl.KHRICD.CL_PLATFORM_NOT_FOUND_KHR
import org.lwjgl.system.MemoryStack.stackPush

object opencl10 {

  implicit final class PlatformIdOps10(platformId: PlatformId) {}

  private[opencl10] trait OpacityTypes {
    type PlatformId <: Long
    def PlatformId(longPtr: Long): PlatformId
  }

  val opacityTypes: OpacityTypes = new OpacityTypes {
    override type PlatformId = Long

    override def PlatformId(longPtr: Long): Long = longPtr
  }

  type PlatformId = opacityTypes.PlatformId

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

  def platforms: Seq[PlatformId] = {

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
        opacityTypes.PlatformId(platformId)
      }
    } finally {
      stack.close()
    }
  }

}
