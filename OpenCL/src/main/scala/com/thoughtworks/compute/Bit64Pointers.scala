package com.thoughtworks.compute

/**
  * @author 杨博 (Yang Bo)
  */
trait Bit64Pointers extends Pointers {

  override type IntPtr = Long

  final def IntPtr(longPtr: Long): IntPtr = {
    longPtr
  }

  trait PointerApi extends super.PointerApi {
    override def address(): Long = intPtr
  }

  type Pointer <: PointerApi

}
