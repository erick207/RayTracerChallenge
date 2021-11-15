package Util

import utest._
import raytracer._
import collection.mutable.ArrayDeque

object Chapter1 extends TestSuite{
        
  def tests = Tests{
      /*test("hello"){
            val result = Example.hello()
            assert(result == "Hello World")
            result
        }*/
      
      
/*********************************************************************/
/*                           CHAPTER 1                               */
/*                        GETTING STARTED                            */
/*                                                                   */
/*********************************************************************/

    test("aprox equal Doubles"){
      val result = Utilities.equal(4.0, 4.000009)
      assert(result == true)
      result
    }
    //fails using bigger Utilities.EPSILON values
    /*
    test("aprox unequal Doubles"){
      val result = Utilities.equal(5.2, 5.200021)
      assert(result == false)
      result
    }*/
    test("aprox equal Tuple"){
      val result = Tuple(5.0, 3.0, 6.6, -5.2).equal(Tuple(5.0, 3.000009, 6.6, -5.200009))
      assert(result == true)
      result
    }
    test("add Tuple"){
      val result = Tuple(3, -2, 5, 1).add(Tuple(-2, 3, 1, 0))
      assert(result.equal(Tuple(1, 1, 6, 1)))
      result
    }
    test("substracting two points"){
      val result = Tuple(3, 2, 1, 1).subtract(Tuple(5, 6, 7, 1))
      assert(result.equal(Tuple(-2, -4, -6, 0)))
      result
    }
    test("substracting a vector from a point"){
      val result = Tuple(3, 2, 1, 1).subtract(Tuple(5, 6, 7, 0))
      assert(result.equal(Tuple(-2, -4, -6, 1)))
      result
    }
    test("substracting two vectors"){
      val result = Tuple(3, 2, 1, 0).subtract(Tuple(5, 6, 7, 0))
      assert(result.equal(Tuple(-2, -4, -6, 0)))
      result
    }
    test("negate a tuple"){
      val result = Tuple(1, -2, 3, -4).negate()
      assert(result.equal(Tuple(-1, 2, -3, 4)))
      result
    }
    test("multiply by a scalar"){
      val result = Tuple(1, -2, 3, -4).multiply(3.5)
      assert(result.equal(Tuple(3.5, -7, 10.5, -14)))
      result
    }
    test("divide by a scalar"){
      val result = Tuple(1, -2, 3, -4).divide(2)
      assert(result.equal(Tuple(0.5, -1, 1.5, -2)))
      result
    }
    test("magnitude of unit vector x"){
      val result = Tuple(1, 0, 0, 0).magnitude()
      assert(result == 1)
      result
    }
    test("magnitude of unit vector y"){
      val result = Tuple(0, 1, 0, 0).magnitude()
      assert(result == 1)
      result
    }
    test("magnitude of unit vector z"){
      val result = Tuple(0, 0, 1, 0).magnitude()
      assert(result == 1)
      result
    }
    test("magnitude of a vector"){
      val result = Tuple(1, 2, 3, 0).magnitude
      assert(Utilities.equal(result, math.sqrt(14).toDouble))
      result
    }
    test("magnitude of a vector"){
      val result = Tuple(-1, -2, -3, 0).magnitude
      assert(Utilities.equal(result, math.sqrt(14).toDouble))
      result
    }
    test("normalize vector (4,0,0) to (1,0,0)"){
      val result = Tuple(4, 0, 0, 0).normalize
      assert(result.equal(Tuple(1, 0, 0, 0)))
      result
    }
    test("normalize vector (1,2,3) to (1/sqrt(14), 2/sqrt(14), 3/sqrt(14))"){
      val result = Tuple(1, 2, 3, 0).normalize
      assert(result.equal(Tuple(0.26726, 0.53452, 0.80178, 0)))
      result
    }
    test("magnitude of a normalized vector must be 1"){
      val norm = Tuple(1, 2, 3, 0).normalize
      val result = norm.magnitude
      assert(Utilities.equal(result, 1))
      result
    }
    test("dot product"){
      val result = Tuple(1, 2, 3, 0).dot(Tuple(2, 3, 4, 0))
      assert(result == 20)
      result
    }
    test("cross product a x b"){
      val result = Tuple(1, 2, 3, 0).cross(Tuple(2, 3, 4, 0))
      assert(result.equal(Tuple(-1, 2, -1, 0)))
      result
    }
    test("cross product a x b"){
      val result = Tuple(2, 3, 4, 0).cross(Tuple(1, 2, 3, 0))
      assert(result.equal(Tuple(1, -2, 1, 0)))
      result
    }
    test("multiplying colors, hadamard/schur product"){
      val result = Color(1, 0.2, 0.4).multiplyColor(Color(0.9, 1, 0.1))
      assert(result.equal(Color(0.9, 0.2, 0.04)))
      result
    }
    
  }
  
}
