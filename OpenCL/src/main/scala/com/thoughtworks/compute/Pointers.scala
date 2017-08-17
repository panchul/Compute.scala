package com.thoughtworks.compute

import com.thoughtworks.feature.Factory.inject
import shapeless.Widen

/**
  * @author 杨博 (Yang Bo)
  */
trait Pointers {

  type IntPtr

  def IntPtr(long: Long): IntPtr

  type Pointer <: PointerApi

  trait PointerApi extends org.lwjgl.system.Pointer {

    val intPtr: IntPtr

    override def hashCode(): Int = {
      intPtr.hashCode
    }

    override def equals(rightHandSide: scala.Any): Boolean = {
      rightHandSide match {
        case p: PointerApi =>
          address == p.address
        case _ =>
          false
      }
    }

    override def toString: String = {
      raw"""Pointer($intPtr)"""
    }
  }

}
