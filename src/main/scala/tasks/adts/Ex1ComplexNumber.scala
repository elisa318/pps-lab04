package tasks.adts

package u04lab

/*  Exercise 1: 
 *  Complete the implementation of ComplexADT trait below, so that it passes
 *  the test in ComplexTest.
 */

object Ex1ComplexNumbers:

  trait ComplexADT:
    type Complex
    def complex(re: Double, im: Double): Complex
    extension (complex: Complex)
      def re(): Double
      def im(): Double
      def sum(other: Complex): Complex
      def subtract(other: Complex): Complex
      def asString(): String

  object BasicComplexADT extends ComplexADT:

    // Change assignment below: should probably define a case class and use it?
    type Real = Double;
    type Imag = Double;

    type Complex = ComplexImpl;
    case class ComplexImpl(real : Real, imaginary : Imag);

    def complex(re: Double, im: Double): Complex = ComplexImpl(re, im)
    extension (complex: Complex)
      def re(): Double = complex.real
      def im(): Double = complex.imaginary
      def sum(other: Complex): Complex = ComplexImpl(complex.re() + other.re(), complex.im() + other.im() )
      def subtract(other: Complex): Complex = ComplexImpl(complex.re() - other.re(), complex.im() - other.im() )
      def asString(): String = complex match
        case ComplexImpl(re, 0) => complex.re().toString
        case ComplexImpl(0, im) => complex.im().toString + "i"
        case ComplexImpl(0, 0) => "0.0"
        case _ => complex.re().toString + " + " + complex.im().toString + "i"


