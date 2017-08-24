package com.thoughtworks.compute

import scala.collection.JavaConverters._
import java.nio.{ByteBuffer, IntBuffer}
import java.util.concurrent.ConcurrentHashMap
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
import scalaz.{-\/, Memo, \/, \/-}
import scalaz.syntax.all._
import com.thoughtworks.continuation._
import com.thoughtworks.feature.mixins.ImplicitsSingleton
import com.thoughtworks.future._
import com.thoughtworks.raii.AsynchronousPool
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

  trait CommandQueuePool extends OpenCL {
    // TODO: write buffer
    // TODO: read buffer

    // TODO: enqueue kernel
    // TODO: TDD

    protected val numberOfCommandQueuesForDevice: (Long, CLCapabilities) => Int

    @transient private lazy val commandQueues: Seq[Long] = {
      deviceIds.flatMap { deviceId =>
        val capabilities = deviceCapabilities(deviceId)
        for (i <- 0 until numberOfCommandQueuesForDevice(deviceId, capabilities)) yield {
          val supportedProperties = deviceLongInfo(deviceId, CL_DEVICE_QUEUE_PROPERTIES)
          val properties = Map(
            CL_QUEUE_PROPERTIES -> (supportedProperties | CL_QUEUE_ON_DEVICE)
          )
          createCommandQueue(deviceId, properties)
        }
      }
    }

    val Resource(acquireCommandQueue, shutdownCommandQueues) = AsynchronousPool.preloaded(commandQueues)

    private def deviceLongInfo(deviceId: Long, paramName: Int): Long = {
      val buffer = Array[Long](0L)
      checkErrorCode(clGetDeviceInfo(deviceId, paramName, buffer, null))
      val Array(value) = buffer
      value
    }

    override def monadicClose: UnitContinuation[Unit] = {
      shutdownCommandQueues >> {
        for (commandQueue <- commandQueues) {
          checkErrorCode(clReleaseCommandQueue(commandQueue))
        }
        super.monadicClose
      }
    }
  }
  private[OpenCL] object Event {
    private[OpenCL] def delay[Owner <: OpenCL with Singleton](handle: => Long): Do[Event[Owner]] = {
      val bufferContinuation = UnitContinuation.delay {
        Resource(value = Success(Event[Owner](handle)), release = UnitContinuation.delay {
          checkErrorCode(clReleaseMemObject(handle))
        })
      }
      Do(TryT(ResourceT(bufferContinuation)))
    }

    val eventCallback: CLEventCallback = CLEventCallback.create(new CLEventCallbackI {
      final def invoke(event: Long, status: Int, userData: Long): Unit = {
        val scalaCallback = try { memGlobalRefToObject[Int => Unit](userData) } finally {
          JNINativeInterface.DeleteGlobalRef(userData)
        }
        scalaCallback(status)
      }
    })
  }

  private[OpenCL] final case class Event[Owner <: Singleton with OpenCL](handle: Long) extends AnyVal {
    type Status = Int
    def waitForStatus(callbackType: Status): UnitContinuation[Status] = UnitContinuation.async { (continue: Status => Unit) =>
      val userData = JNINativeInterface.NewGlobalRef(continue)
      try {
        checkErrorCode(
          clSetEventCallback(
            handle,
            callbackType,
            Event.eventCallback,
            userData
          )
        )
      } catch {
        case NonFatal(e) =>
          JNINativeInterface.DeleteGlobalRef(userData)
          throw e
      }
    }

    def waitFor(callbackType: Status): Future[Unit] = {
      Future(TryT(waitForStatus(callbackType).map {
        case `callbackType`             => Success(())
        case errorCode if errorCode < 0 => Failure(Exceptions.fromErrorCode(errorCode))
        case status                     => throw new IllegalStateException(raw"""Invalid event status $status""")
      }))
    }

    def waitForComplete(): Future[Unit] = waitFor(CL_COMPLETE)

  }

  object DeviceBuffer {
    private[OpenCL] def delay[Owner <: Singleton with OpenCL, Element](
        handle: => Long): Do[DeviceBuffer[Owner, Element]] = {
      val bufferContinuation = UnitContinuation.delay {
        Resource(value = Success(DeviceBuffer[Owner, Element](handle)), release = UnitContinuation.delay {
          checkErrorCode(clReleaseMemObject(handle))
        })
      }
      Do(TryT(ResourceT(bufferContinuation)))
    }
  }

  /** A [[https://www.khronos.org/registry/OpenCL/sdk/2.1/docs/man/xhtml/abstractDataTypes.html cl_mem]]
    * whose [[org.lwjgl.opencl.CL10.CL_MEM_TYPE CL_MEM_TYPE]] is buffer [[org.lwjgl.opencl.CL10.CL_MEM_OBJECT_BUFFER CL_MEM_OBJECT_BUFFER]].
    * @param handle The underlying `cl_mem`.
    */
  final case class DeviceBuffer[Owner <: OpenCL with Singleton, Element](handle: Long) extends AnyVal { deviceBuffer =>
    def slice(offset: Int, size: Int)(implicit
                                      memory: Memory[Element]): Do[DeviceBuffer[Owner, Element]] = {
      DeviceBuffer.delay {
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

    private def enqueueReadBuffer[Destination](hostBuffer: Destination, preconditionEvents: Event[Owner]*)(
        implicit
        witnessOwner: Witness.Aux[Owner],
        memory: Memory.Aux[Element, Destination]): Do[Event[Owner]] = {

      witnessOwner.value.acquireCommandQueue.flatMap { commandQueue =>
        Event.delay[Owner] {
          val outputEvent = {
            val stack = stackPush()
            try {
              val (inputEventBufferSize, inputEventBufferAddress) = if (preconditionEvents.isEmpty) {
                (0, NULL)
              } else {
                val inputEventBuffer = stack.pointers(preconditionEvents.view.map(_.handle): _*)
                (preconditionEvents.length, inputEventBuffer.address())
              }
              val outputEventBuffer = stack.pointers(0L)
              checkErrorCode(
                nclEnqueueReadBuffer(
                  commandQueue,
                  deviceBuffer.handle,
                  CL_FALSE,
                  0,
                  memory.remainingBytes(hostBuffer),
                  memory.address(hostBuffer),
                  inputEventBufferSize,
                  inputEventBufferAddress,
                  outputEventBuffer.address()
                )
              )
              outputEventBuffer.get(0)
            } finally {
              stack.close()
            }
          }
          checkErrorCode(clFlush(handle))
          outputEvent
        }
      }
    }

    final def toHostBuffer(implicit witnessOwner: Witness.Aux[Owner],
                           memory: Memory[Element]): Do[memory.HostBuffer] = {
      Do(TryT(ResourceT(UnitContinuation.delay {
        val hostBuffer = memory.allocate(length)
        Resource(value = Success(hostBuffer), release = UnitContinuation.delay { memory.free(hostBuffer) })
      }))).flatMap { hostBuffer =>
        enqueueReadBuffer[memory.HostBuffer](hostBuffer)(witnessOwner, memory)
          .flatMap { event =>
            Do.garbageCollected(event.waitForComplete())
          }
          .intransitiveMap { _: Unit =>
            hostBuffer
          }
      }
    }
  }

  private[compute] final case class Program[Owner <: OpenCL with Singleton](handle: Long) extends AnyVal {

    private def startBuild(deviceIds: PointerBuffer, options: CharSequence, callback: Unit => Unit) = {
      val userData = JNINativeInterface.NewGlobalRef(callback)
      try {
        // `userData` will be deleted in Program.programCallback if clBuildProgram returns CL_SUCCESS,
        checkErrorCode(clBuildProgram(handle, deviceIds, options, Program.programCallback, userData))
      } catch {
        case NonFatal(e) =>
          // `userData` will be deleted here if the build is not started successfully
          JNINativeInterface.DeleteGlobalRef(userData)
          throw e
      }
    }

    def build(deviceIds: Seq[Long], options: CharSequence = ""): UnitContinuation[Unit] = UnitContinuation.async {
      (handler: Unit => Unit) =>
        val stack = stackPush()
        try {
          startBuild(stack.pointers(deviceIds: _*), options, handler)
        } finally {
          stack.close()
        }
    }

    def build(options: CharSequence): UnitContinuation[Unit] = UnitContinuation.async { (handler: Unit => Unit) =>
      startBuild(null, options, handler)
    }

    def build(): UnitContinuation[Unit] = build("")
  }

  object Program {

    override def finalize(): Unit = {
      programCallback.close()
    }

    val programCallback = CLProgramCallback.create(new CLProgramCallbackI {
      override def invoke(program: Long, userData: Long): Unit = {
        val scalaCallback = try {
          memGlobalRefToObject[Unit => Unit](userData)
        } finally {
          JNINativeInterface.DeleteGlobalRef(userData)
        }
        scalaCallback(())
      }
    })

  }

}

trait OpenCL extends MonadicCloseable[UnitContinuation] with ImplicitsSingleton {
  type Program = OpenCL.Program[this.type]
  protected def createProgramWithSource(sourceCode: TraversableOnce[CharSequence]): Program = {
    val stack = stackPush()
    try {
      val errorCodeBuffer = stack.ints(CL_SUCCESS)
      val codeBuffers = (for {
        snippet <- sourceCode
        if snippet.length > 0
      } yield memUTF8(snippet, false)).toArray
      val pointers = memAllocPointer(codeBuffers.length)
      val lengths = memAllocPointer(codeBuffers.length)
      for (buffer <- codeBuffers) {
        pointers.put(buffer)
        lengths.put(buffer.remaining)
      }
      pointers.position(0)
      lengths.position(0)
      try {
        val programHandle = clCreateProgramWithSource(context, pointers, lengths, errorCodeBuffer)
        checkErrorCode(errorCodeBuffer.get(0))
        new Program(programHandle)
      } finally {
        memFree(pointers)
        memFree(lengths)
        codeBuffers.foreach(memFree)
      }
    } finally {
      stack.close()
    }
  }

  protected def acquireCommandQueue: Do[Long]

  def monadicClose: UnitContinuation[Unit] = UnitContinuation.delay {
    checkErrorCode(clReleaseContext(context))
  }

  protected def handleOpenCLNotification(errorInfo: String, privateInfo: ByteBuffer): Unit

  import OpenCL._

  protected val platformId: Long
  protected val deviceIds: Seq[Long]

  @transient
  private lazy val platformCapabilities: CLCapabilities = {
    CL.createPlatformCapabilities(platformId)
  }

  protected def createCommandQueue(deviceId: Long, properties: Map[Int, Long]): Long = {
    if (deviceCapabilities(deviceId).OpenCL20) {
      val cl20Properties = (properties.view.flatMap { case (key, value) => Seq(key, value) } ++ Seq(0L)).toArray
      val a = Array(0)
      val commandQueue =
        clCreateCommandQueueWithProperties(platformId, deviceId, cl20Properties, a)
      checkErrorCode(a(0))
      commandQueue
    } else {
      val cl10Properties = properties.getOrElse(CL_QUEUE_PROPERTIES, 0L)
      val a = Array(0)
      val commandQueue = clCreateCommandQueue(platformId, deviceId, cl10Properties, a)
      checkErrorCode(a(0))
      commandQueue
    }
  }

  @transient
  protected lazy val deviceCapabilities: Long => CLCapabilities = {
    Memo.mutableMapMemo(new ConcurrentHashMap[Long, CLCapabilities].asScala) { deviceId =>
      CL.createDeviceCapabilities(deviceId, platformCapabilities)
    }
  }

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
  def allocateBuffer[Element](size: Long)(implicit memory: Memory[Element]): Do[DeviceBuffer[Element]] =
    DeviceBuffer.delay[this.type, Element] {
      val stack = stackPush()
      try {
        val errorCodeBuffer = stack.ints(CL_SUCCESS)
        val buffer =
          clCreateBuffer(context, CL_MEM_READ_WRITE, memory.numberOfBytesPerElement * size, errorCodeBuffer)
        checkErrorCode(errorCodeBuffer.get(0))
        buffer
      } finally {
        stack.pop()
      }
    }

  /** Returns a buffer of `Element` on device whose content is copied from `hostBuffer`.
    */
  def allocateBufferFrom[Element, HostBuffer](hostBuffer: HostBuffer)(
      implicit memory: Memory.Aux[Element, HostBuffer]): Do[DeviceBuffer[Element]] =
    DeviceBuffer.delay[this.type, Element] {
      val stack = stackPush()
      try {
        val errorCodeBuffer = stack.ints(CL_SUCCESS)
        val buffer = nclCreateBuffer(context,
                                     CL_MEM_COPY_HOST_PTR | CL_MEM_READ_WRITE,
                                     memory.remainingBytes(hostBuffer),
                                     memory.address(hostBuffer),
                                     memAddress(errorCodeBuffer))
        checkErrorCode(errorCodeBuffer.get(0))
        buffer
      } finally {
        stack.pop()
      }
    }

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
