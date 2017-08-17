package com.thoughtworks.compute

/**
  * @author 杨博 (Yang Bo)
  */
trait Bit32Pointers extends Pointers {

  override type IntPtr = Int

  final def IntPtr(longPtr: Long): IntPtr = {
    require((longPtr >>> 32) == 0, raw"""`longPtr`($longPtr) should not have more than 32 bits.""")
    longPtr.toInt
  }

  trait PointerApi extends super.PointerApi {
    override def address(): Long = intPtr
  }

  type Pointer <: PointerApi

}
