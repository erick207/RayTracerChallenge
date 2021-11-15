package Util

import utest._
import raytracer._
import collection.mutable.ArrayDeque

object Chapter3 extends TestSuite{
        
  def tests = Tests{
/*********************************************************************/
/*                           CHAPTER 3                               */
/*                            MATRICES                               */
/*                                                                   */
/*********************************************************************/

    test("Constructing and inspecting a 4x4 matrix"){
      val result = Matrix4(1, 2, 3, 4,
                          5.5, 6.5, 7.5, 8.5,
                          9, 0, 1, 2,
                          13.5, 14.5, 15.5, 16.5)
      assert(Utilities.equal(result.m(0)(0), 1) &&
             Utilities.equal(result.m(0)(3), 4) &&
             Utilities.equal(result.m(1)(0), 5.5) &&
             Utilities.equal(result.m(1)(2), 7.5) &&
             Utilities.equal(result.m(2)(2), 1) &&
             Utilities.equal(result.m(3)(0), 13.5) &&
             Utilities.equal(result.m(3)(2), 15.5))

      result
    }
    test("Constructing and inspecting a 3x3 matrix"){
      val result = Matrix3(-3, 5, 0,
                           1, -2, -7,
                           0, 1, 1)
      assert(Utilities.equal(result.m(0)(0), -3) &&
             Utilities.equal(result.m(1)(1), -2) &&
             Utilities.equal(result.m(2)(2), 1))

      result
    }
    test("Constructing and inspecting a 2x2 matrix"){
      val result = Matrix2(-3, 5,
                           1, -2)
      assert(Utilities.equal(result.m(0)(0), -3) &&
             Utilities.equal(result.m(0)(1), 5) &&
             Utilities.equal(result.m(1)(0), 1) &&
             Utilities.equal(result.m(1)(1), -2))

      result
    }
    test("equality of equal 4x4 matrices"){
      val a = Matrix4(1, 2, 3, 4,
                      5, 6, 7, 8,
                      9, 8, 7, 6,
                      5, 4, 3, 2)
      
      val b = Matrix4(1, 2, 3, 4,
                      5, 6, 7, 8,
                      9, 8, 7, 6,
                      5, 4, 3, 2)
      val result = a.equal(b) 
      assert(result)

      result
    }
    test("equality of different 4x4 matrices"){
      val a = Matrix4(1, 2, 3, 4,
                      5, 6, 7, 8,
                      9, 8, 7, 6,
                      5, 4, 3, 2)
      
      val b = Matrix4(2, 3, 4, 5,
                      6, 7, 8, 9,
                      8, 7, 6, 5,
                      4, 3, 2, 1)
      val result = a.equal(b) 
      assert(!result)

      result
    }
    test("multiplying two matrices"){
      val a = Matrix4(1, 2, 3, 4,
                      5, 6, 7, 8,
                      9, 8, 7, 6,
                      5, 4, 3, 2)
      
      val b = Matrix4(-2, 1, 2, 3,
                      3, 2, 1, -1,
                      4, 3, 6, 5,
                      1, 2, 7, 8)
      val correctResult = Matrix4(20, 22, 50, 48,
                                  44, 54, 114, 108,
                                  40, 58, 110, 102,
                                  16, 26, 46, 42)
      val result = a.multiply(b) 
      assert(result.equal(correctResult))

      result
    }
    test("A matrix multiplied by a tuple"){
      val a = Matrix4(1, 2, 3, 4,
                      2, 4, 4, 2,
                      8, 6, 4, 1,
                      0, 0, 0, 1)
      
      val b = Tuple(1, 2, 3, 1)
      val correctResult = Tuple(18, 24, 33, 1)
      
      val result = a.multiply(b) 
      assert(result.equal(correctResult))

      result
    }
    test("A matrix multiplied by the identity matrix"){
      val a = Matrix4(0, 1, 2, 4,
                      1, 2, 4, 8,
                      2, 4, 8, 6,
                      4, 8, 6, 2)
      
      val b = Matrix4.identityMatrix
      
      val result = a.multiply(b) 
      assert(result.equal(a))

      result
    }
    test("transposing a matrix"){
      val a = Matrix4(0, 9, 3, 0,
                      9, 8, 0, 8,
                      1, 8, 5, 3,
                      0, 0, 5, 8)
      
      val correct = Matrix4(0, 9, 1, 0,
                            9, 8, 8, 0,
                            3, 0, 5, 5,
                            0, 8, 3, 8)
      
      val result = Matrix4.transpose(a) 
      assert(result.equal(correct))

      result
    }
    test("transposing the identity matrix"){
      val id = Matrix4.identityMatrix
      
      val result = Matrix4.transpose(id) 
      assert(result.equal(id))

      result
    }
    test("Identity matrix multiplied by a tuple"){
      val id = Matrix4.identityMatrix
      
      val a = Tuple(1, 2, 3, 4)
      
      
      val result = id.multiply(a) 
      assert(result.equal(a))

      result
    }
    test("Determinant of a 2x2 matrix"){
      val m = Matrix2(1,5,
                      -3,2)
      assert(m.determinant == 17)
      m
    }
    test("A submatrix of a 3x3 matrix"){
      val a = Matrix3(1, 5, 0,
                      -3, 2, 7,
                      0, 6, -3)
      val correct = Matrix2(-3, 2,
                            0, 6) 
      
      val result = a.submatrix(0,2)
      assert(result.equal(correct))
      result
    }
    test("A submatrix of a 3x3 matrix"){
      val a = Matrix3(1, 5, 0,
                      -3, 2, 7,
                      0, 6, -3)
      val correct = Matrix2(1, 0,
                            0, -3) 
      
      val result = a.submatrix(1,1)
      assert(result.equal(correct))
      result
    }
    test("A submatrix of a 4x4 matrix"){
      val a = Matrix4(-6, 1, 1, 6,
                      -8, 5, 8, 6,
                      -1, 0, 8, 2,
                      -7, 1, -1, 1)
      val correct = Matrix3(-6, 1, 6,
                            -8, 8, 6,
                            -7, -1, 1)
      
      val result = a.submatrix(2,1)
      assert(result.equal(correct))
      result
    }
    test("Minor of a 3x3 matrix (determinant of submatrix)"){
      val a = Matrix3(3, 5, 0,
                      2, -1, -7,
                      6, -1, 5)
      
      val result = a.minor(1,0)
      assert(Utilities.equal(result, 25))
      result
    }
    test("Calculating a cofactor of a 3x3 matrix"){
      val a = Matrix3(3, 5, 0,
                      2, -1, -7,
                      6, -1, 5)
      
      val mi1 = a.minor(0,0)
      val co1 = a.cofactor(0,0)
      
      val mi2 = a.minor(1,0)
      val co2 = a.cofactor(1,0)
      
      assert(Utilities.equal(mi1, -12) &&
             Utilities.equal(co1, -12) &&
             Utilities.equal(mi2, 25) &&
             Utilities.equal(co2, -25))
      (mi1, co1, mi2, co2)
    }
    test("Calculating the determinant of a 3x3 matrix"){
      val a = Matrix3(1, 2, 6,
                      -5, 8, -4,
                      2, 6, 4)
      
      val co00 = a.minor(0,0)
      val co01 = a.cofactor(0,1)
      val co02 = a.cofactor(0,2)
      val det = a.determinant
      
      assert(Utilities.equal(co00, 56) &&
             Utilities.equal(co01, 12) &&
             Utilities.equal(co02, -46) &&
             Utilities.equal(det, -196))
      
      (co00, co01, co02, det)
    }
    test("Calculating the determinant of a 4x4 matrix"){
      val a = Matrix4(-2, -8, 3, 5,
                      -3, 1, 7, 3,
                      1, 2, -9, 6,
                      -6, 7, 7, -9)
                      
                      
      
      val co00 = a.cofactor(0,0)
      val co01 = a.cofactor(0,1)
      val co02 = a.cofactor(0,2)
      val co03 = a.cofactor(0,3)
      val det = a.determinant
      
      assert(Utilities.equal(co00, 690) &&
             Utilities.equal(co01, 447) &&
             Utilities.equal(co02, 210) &&
             Utilities.equal(co03, 51) &&
             Utilities.equal(det, -4071))
      
      (co00, co01, co02, co03, det)
    }
    test("Testing an invertible matrix for invertibility"){
      val a = Matrix4(6, 4, 4, 4,
                      5, 5, 7, 6,
                      4, -9, 3, -7,
                      9, 1, 7, -6)
      // det(a) must be -2120
      val det = a.determinant
      assert(Utilities.equal(det, -2120))
      det
    }
    test("Testing a noninvertible matrix for invertibility"){
      val a = Matrix4(-4, 2, -2, -3,
                      9, 6, 2, 6,
                      0, -5, 1, -5,
                      0, 0, 0, 0)
      // det(a) must be 0
      val det = a.determinant 
      assert(Utilities.equal(det, 0))
      det
    }
    test("calculating the inverse of a matrix"){
      val a = Matrix4(-5, 2, 6, -8,
                      1, -5, 1, 8,
                      7, 7, -6, -7,
                      1, -3, 7, 4)
      
      val detA = a.determinant                
      val invA = a.inverse.getOrElse(Matrix4.newZeroMatrix4)
      val co23 = a.cofactor(2,3)
      val co32 = a.cofactor(3,2)
                      
      val correct = Matrix4(0.21805, 0.45113, 0.24060, -0.04511,
                            -0.80827, -1.45677, -0.44361, 0.52068,
                            -0.07895, -0.22368, -0.05263, 0.19737,
                            -0.52256, -0.81391, -0.30075, 0.30639)
                                  
      assert(Utilities.equal(detA, 532) &&
             Utilities.equal(co23, -160) &&
             Utilities.equal(co32, 105) &&
             Utilities.equal(invA.m(3)(2), -160.0 / 532) &&
             Utilities.equal(invA.m(2)(3), 105.0 / 532) && 
             invA.equal(correct) 
             )
      (detA, co23, co32, invA.m(3)(2), invA.m(2)(3), "\n\n", invA)
    }
    test("calculating the inverse of a second matrix"){
      val a = Matrix4(8, -5, 9, 2,
                      7, 5, 6, 1,
                      -6, 0, 9, 6,
                      -3, 0, -9, -4)
      
      val invA = a.inverse.getOrElse(Matrix4.newZeroMatrix4)
                      
      val correct = Matrix4(-0.15385, -0.15385, -0.28205, -0.53846,
                            -0.07692, 0.12308, 0.02564, 0.03077,
                             0.35897, 0.35897, 0.43590, 0.92308,
                            -0.69231, -0.69231, -0.76923, -1.92308)
                            
      assert(invA.equal(correct))
      invA
    }
    test("calculating the inverse of a third matrix"){
      val a = Matrix4(9, 3, 0, 9,
                      -5, -2, -6, -3,
                      -4, 9, 6, 4,
                      -7, 6, 6, 2)
      
      val invA = a.inverse.getOrElse(Matrix4.newZeroMatrix4)
                      
      val correct = Matrix4(-0.04074, -0.07778, 0.14444, -0.22222,
                            -0.07778, 0.03333, 0.36667, -0.33333,
                            -0.02901, -0.14630, -0.10926, 0.12963,
                            0.17778, 0.06667, -0.26667, 0.33333)
                            
      assert(invA.equal(correct))
      invA
    }
    test("Multiplying a product by its inverse"){
      val a = Matrix4(3, -9, 7, 3,
                      3, -8, 2, -9,
                      -4, 4, 4, 1,
                      -6, 5, -1, 1)
      
      val b = Matrix4(8, 2, 2, 2,
                      3, -1, 7, 0,
                      7, 0, 5, 4,
                      6, -2, 0, 5)
      
      val c = a.multiply(b)
      
      val invB = b.inverse.getOrElse(Matrix4.newZeroMatrix4)
      
      val c_mul_invB = c.multiply(invB)
      
      assert(c_mul_invB.equal(a))
      c_mul_invB
    }
  }   

}
