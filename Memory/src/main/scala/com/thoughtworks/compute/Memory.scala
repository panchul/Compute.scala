package com.thoughtworks.compute

import java.nio._

import org.lwjgl.PointerBuffer
import org.lwjgl.system.{CustomBuffer, MemoryUtil, Pointer}
import shapeless.HNil

/**
  * @author 杨博 (Yang Bo) &lt;pop.atry@gmail.com&gt;
  */
trait Memory[Element] {
  type Buffer
  def fromByteBuffer(byteBuffer: ByteBuffer): Buffer
  def numberOfBytesPerElement: Int
  def address(buffer: Buffer): Long
  def remaining(buffer: Buffer): Int
  def remainingBytes(buffer: Buffer): Int = numberOfBytesPerElement * remaining(buffer)
  def get(buffer: Buffer, index: Int): Element
  def put(buffer: Buffer, index: Int, value: Element): Unit
}

object Memory extends LowPriorityMemory {

  def apply[Element](implicit memory: Memory[Element]): memory.type = memory

  type Aux[Element, Buffer0] = Memory[Element] {
    type Buffer = Buffer0
  }

  trait NioMemory[Element] extends Memory[Element] {
    type Buffer <: java.nio.Buffer

    override def remaining(buffer: Buffer): Int = buffer.remaining
  }

  trait CustomMemory[Element] extends Memory[Element] {
    type Buffer <: CustomBuffer[Buffer]

    override def remaining(buffer: Buffer): Int = buffer.remaining

    override def address(buffer: Buffer): Long = (buffer.address)
  }

  implicit object PointerMemory extends CustomMemory[Pointer] {
    override type Buffer = PointerBuffer

    override def numberOfBytesPerElement: Int = Pointer.POINTER_SIZE

    override def fromByteBuffer(byteBuffer: ByteBuffer): PointerBuffer = {
      PointerBuffer.create(byteBuffer)
    }

    override def get(buffer: PointerBuffer, index: Int): Pointer = new Pointer.Default(buffer.get(index)) {}

    override def put(buffer: PointerBuffer, index: Int, value: Pointer): Unit = buffer.put(index, value)

  }

  implicit object HNilMemory extends NioMemory[HNil] {
    override type Buffer = ByteBuffer

    override def fromByteBuffer(byteBuffer: ByteBuffer): ByteBuffer = byteBuffer

    override def numberOfBytesPerElement: Int = 0

    override def address(buffer: ByteBuffer): Long = (MemoryUtil.memAddress(buffer))

    override def get(buffer: ByteBuffer, index: Int): HNil = HNil

    override def put(buffer: ByteBuffer, index: Int, value: HNil): Unit = {}
  }

  implicit object IntMemory extends NioMemory[Int] {
    override type Buffer = IntBuffer

    override def fromByteBuffer(byteBuffer: ByteBuffer): IntBuffer = byteBuffer.asIntBuffer

    override def numberOfBytesPerElement: Int = java.lang.Integer.BYTES

    override def address(buffer: IntBuffer): Long = (MemoryUtil.memAddress(buffer))

    override def get(buffer: IntBuffer, index: Int): Int = buffer.get(index)

    override def put(buffer: IntBuffer, index: Int, value: Int): Unit = buffer.put(index, value)

  }
  implicit object LongMemory extends NioMemory[Long] {
    override type Buffer = LongBuffer

    override def fromByteBuffer(byteBuffer: ByteBuffer): LongBuffer = byteBuffer.asLongBuffer

    override def numberOfBytesPerElement: Int = java.lang.Long.BYTES

    override def address(buffer: LongBuffer): Long = (MemoryUtil.memAddress(buffer))

    override def get(buffer: LongBuffer, index: Int): Long = buffer.get(index)

    override def put(buffer: LongBuffer, index: Int, value: Long): Unit = buffer.put(index, value)

  }
  implicit object DoubleMemory extends NioMemory[Double] {
    override type Buffer = DoubleBuffer

    override def fromByteBuffer(byteBuffer: ByteBuffer): DoubleBuffer = byteBuffer.asDoubleBuffer

    override def numberOfBytesPerElement: Int = java.lang.Double.BYTES

    override def address(buffer: DoubleBuffer): Long = (MemoryUtil.memAddress(buffer))

    override def get(buffer: DoubleBuffer, index: Int): Double = buffer.get(index)

    override def put(buffer: DoubleBuffer, index: Int, value: Double): Unit = buffer.put(index, value)

  }
  implicit object FloatMemory extends NioMemory[Float] {
    override type Buffer = FloatBuffer

    override def fromByteBuffer(byteBuffer: ByteBuffer): FloatBuffer = byteBuffer.asFloatBuffer

    override def numberOfBytesPerElement: Int = java.lang.Float.BYTES

    override def address(buffer: FloatBuffer): Long = (MemoryUtil.memAddress(buffer))

    override def get(buffer: FloatBuffer, index: Int): Float = buffer.get(index)

    override def put(buffer: FloatBuffer, index: Int, value: Float): Unit = buffer.put(index, value)

  }
  implicit object ByteMemory extends NioMemory[Byte] {
    override type Buffer = ByteBuffer

    override def fromByteBuffer(byteBuffer: ByteBuffer): ByteBuffer = byteBuffer

    override def numberOfBytesPerElement: Int = java.lang.Byte.BYTES

    override def address(buffer: ByteBuffer): Long = (MemoryUtil.memAddress(buffer))

    override def get(buffer: ByteBuffer, index: Int): Byte = buffer.get(index)

    override def put(buffer: ByteBuffer, index: Int, value: Byte): Unit = buffer.put(index, value)

  }

  implicit object ShortMemory extends NioMemory[Short] {
    override type Buffer = ShortBuffer

    override def fromByteBuffer(byteBuffer: ByteBuffer): ShortBuffer = byteBuffer.asShortBuffer()

    override def numberOfBytesPerElement: Int = java.lang.Short.BYTES

    override def address(buffer: ShortBuffer): Long = (MemoryUtil.memAddress(buffer))

    override def get(buffer: ShortBuffer, index: Int): Short = buffer.get(index)

    override def put(buffer: ShortBuffer, index: Int, value: Short): Unit = buffer.put(index, value)

  }

  // TODO: short, bool, char

  trait Box[Boxed] {
    type Raw

    def box(raw: Raw): Boxed

    def unbox(boxed: Boxed): Raw
  }

  object Box {
    type Aux[Boxed, Raw0] = Box[Boxed] {
      type Raw = Raw0
    }
  }

  final class BoxedMemory[Raw, Boxed, Buffer0](implicit box: Box.Aux[Boxed, Raw], rawMemory: Memory.Aux[Raw, Buffer0])
      extends Memory[Boxed] {
    override type Buffer = Buffer0
    override def fromByteBuffer(byteBuffer: ByteBuffer): Buffer = {
      rawMemory.fromByteBuffer(byteBuffer)
    }
    override def numberOfBytesPerElement: Int = {
      rawMemory.numberOfBytesPerElement
    }
    override def remaining(buffer: Buffer): Int = {
      rawMemory.remaining(buffer)
    }
    override def get(buffer: Buffer, index: Int): Boxed = {
      box.box(rawMemory.get(buffer, index))
    }
    override def put(buffer: Buffer, index: Int, value: Boxed): Unit = {
      rawMemory.put(buffer, index, box.unbox(value))
    }

    override def address(buffer: Buffer): Long = {
      rawMemory.address(buffer)
    }
  }

}

private[compute] trait LowPriorityMemory { this: Memory.type =>

  implicit def boxedMemory[Raw, Boxed, Buffer0](
      implicit box: Box.Aux[Boxed, Raw],
      rawMemory: Memory.Aux[Raw, Buffer0]): BoxedMemory[Raw, Boxed, Buffer0] = {
    new BoxedMemory[Raw, Boxed, Buffer0]
  }

}
