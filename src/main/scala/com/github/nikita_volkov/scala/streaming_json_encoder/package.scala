package com.github.nikita_volkov.scala

import java.io.{ByteArrayOutputStream, OutputStream}

import com.fasterxml.jackson.core.{JsonFactory, JsonGenerator}

import scalaz.{Contravariant, Divisible}

package object streaming_json_encoder {
  
  private val jsonFactory =
    new JsonFactory()
  
  sealed trait NodeEncoder[node] {
    
    def apply(input: node, generator: JsonGenerator): Unit
    
    /**
      * Encode, writing to an output stream.
      */
    final def apply(input: node, outputStream: OutputStream): Unit = {
      val generator = jsonFactory.createGenerator(outputStream)
      apply(input, generator)
      generator.close()
    }
    
    /**
      * Encode to a string.
      */
    final def apply(input: node): String = {
      val stream = new ByteArrayOutputStream()
      apply(input, stream)
      val string = stream.toString
      stream.close()
      string
    }
    
  }
  
  /**
    * Contravariant functor instance.
    */
  implicit val nodeEncoderContravariant =
    new Contravariant[NodeEncoder] {
      @inline
      override def contramap[A, B](fa: NodeEncoder[A])(f: (B) => A) =
        new NodeEncoder[B] {
          override def apply(input: B, generator: JsonGenerator) =
            fa(f(input), generator)
        }
    }
  
  /**
    * A composable encoder of an arbitrary data-structure as an array.
    */
  trait ArrayEncoder[array] {
    def apply(input: array, generator: JsonGenerator)
  }
  
  implicit val arrayEncoderContravariant =
    new Contravariant[ArrayEncoder] {
      @inline
      override def contramap[A, B](fa: ArrayEncoder[A])(f: (B) => A) =
        new ArrayEncoder[B] {
          override def apply(input: B, generator: JsonGenerator) =
            fa(f(input), generator)
        }
    }
  
  /**
    * A composable encoder of an arbitrary data-structure as an object.
    */
  trait ObjectEncoder[object_] {
    def apply(input: object_, generator: JsonGenerator)
  }
  
  implicit val objectEncoderDivisible =
    new Divisible[ObjectEncoder] {
      
      @inline
      override def conquer[A] =
        new ObjectEncoder[A] {
          override def apply(input: A, generator: JsonGenerator) = {}
        }
      
      @inline
      override def divide[A, B, C](fa: ObjectEncoder[A], fb: ObjectEncoder[B])(f: (C) => (A, B)) =
        new ObjectEncoder[C] {
          override def apply(input: C, generator: JsonGenerator) =
            f(input) match {
              case (a, b) =>
                fa(a, generator)
                fb(b, generator)
            }
        }
  
      @inline
      override def contramap[A, B](fa: ObjectEncoder[A])(f: (B) => A) =
        new ObjectEncoder[B] {
          override def apply(input: B, generator: JsonGenerator) =
            fa(f(input), generator)
        }
      
    }
  
  val boolean =
    new NodeEncoder[Boolean] {
      @inline
      override def apply(input: Boolean, generator: JsonGenerator) =
        generator.writeBoolean(input)
    }
  
  /**
    * String encoder.
    */
  val string =
    new NodeEncoder[String] {
      @inline
      override def apply(input: String, generator: JsonGenerator) =
        generator.writeString(input)
    }
  
  val numberFromInt =
    new NodeEncoder[Int] {
      @inline
      override def apply(input: Int, generator: JsonGenerator) =
        generator.writeNumber(input)
    }
  
  val numberFromLong =
    new NodeEncoder[Long] {
      @inline
      override def apply(input: Long, generator: JsonGenerator) =
        generator.writeNumber(input)
    }
  
  val numberFromFloat =
    new NodeEncoder[Float] {
      @inline
      override def apply(input: Float, generator: JsonGenerator) =
        generator.writeNumber(input)
    }
  
  val numberFromDouble =
    new NodeEncoder[Double] {
      @inline
      override def apply(input: Double, generator: JsonGenerator) =
        generator.writeNumber(input)
    }
  
  
  /**
    * Encodes any iterable value.
    */
  @inline
  def array[element](elementEncoder: NodeEncoder[element]) =
    new NodeEncoder[Iterable[element]] {
      @inline
      override def apply(input: Iterable[element], generator: JsonGenerator) = {
        generator.writeStartArray()
        for (element <- input) {
          elementEncoder(element, generator)
        }
        generator.writeEndArray()
      }
    }
  
  /**
    * Encodes an arbitrary data-structure as an array.
    */
  @inline
  def array[array](arrayEncoder: ArrayEncoder[array]) =
    new NodeEncoder[array] {
      @inline
      override def apply(input: array, generator: JsonGenerator) = {
        generator.writeStartArray()
        arrayEncoder(input, generator)
        generator.writeEndArray()
      }
    }
  
  @inline
  def object_[key, value](keyToString: key => String, valueEncoder: NodeEncoder[value]) =
    new NodeEncoder[Iterable[(key, value)]] {
      @inline
      override def apply(input: Iterable[(key, value)], generator: JsonGenerator) = {
        generator.writeStartObject()
        for ((key, value) <- input) {
          generator.writeFieldName(keyToString(key))
          valueEncoder(value, generator)
        }
        generator.writeEndObject()
      }
    }
  
  @inline
  def object_[object_](objectEncoder: ObjectEncoder[object_]) =
    new NodeEncoder[object_] {
      @inline
      override def apply(input: object_, generator: JsonGenerator) = {
        generator.writeStartObject()
        objectEncoder.apply(input, generator)
        generator.writeEndObject()
      }
    }
  
  @inline
  def at[value](key: String, valueEncoder: NodeEncoder[value]) =
    new ObjectEncoder[value] {
      @inline
      override def apply(input: value, generator: JsonGenerator) = {
        generator.writeFieldName(key)
        valueEncoder(input, generator)
      }
    }
  
}
