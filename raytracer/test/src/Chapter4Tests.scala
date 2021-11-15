package Util

import utest._
import raytracer._
import collection.mutable.ArrayDeque

object Chapter4 extends TestSuite{
        
  def tests = Tests{
/*********************************************************************/
/*                           CHAPTER 4                               */
/*                        MATRIX TRANSFORMATIONS                     */
/*                                                                   */
/*********************************************************************/

    test("Multiplying by a translation matrix"){
      val transform = Matrix4.translation(5, -3, 2)
      val p = Tuple(-3, 4, 5, 1)
      assert(transform.multiply(p).equal(Tuple(2, 1, 7, 1)))
      true
    }
    test("Multiplying by the inverse of a translation matrix"){
      val transform = Matrix4.translation(5, -3, 2)
      val inv = transform.inverse.getOrElse(Matrix4.newZeroMatrix4)
      val p = Tuple(-3, 4, 5, 1)
      assert(inv.multiply(p).equal(Tuple(-8, 7, 3, 1)))
      true
    }
    test("Translation doesn't affect vectors"){
      val transform = Matrix4.translation(5, -3, 2)
      val v = Tuple(-3, 4, 5, 0)
      assert(transform.multiply(v).equal(v))
      true
    }
    test("A scaling matrix applied to a point"){
      val transform = Matrix4.scaling(2, 3, 4)
      val p = Tuple(-4, 6, 8, 1)
      assert(transform.multiply(p).equal(Tuple(-8, 18, 32, 1)))
      true
    }
    test("A scaling matrix applied to a vector"){
      val transform = Matrix4.scaling(2, 3, 4)
      val v = Tuple(-4, 6, 8, 0)
      assert(transform.multiply(v).equal(Tuple(-8, 18, 32, 0)))
      true
    }
    test("Multiplying by the inverse of a scaling matrix"){
      val transform = Matrix4.scaling(2, 3, 4)
      val inv = transform.inverse.getOrElse(Matrix4.newZeroMatrix4)
      val v = Tuple(-4, 6, 8, 0)
      assert(inv.multiply(v).equal(Tuple(-2, 2, 2, 0)))
      true
    }
    test("Reflection by scaling by a negative value"){
      val transform = Matrix4.scaling(-1, 1, 1)
      val p = Tuple(2, 3, 4, 1)
      assert(transform.multiply(p).equal(Tuple(-2, 3, 4, 1)))
      true
    }
    test("Rotating a point around the x axis"){
      val p = Tuple(0, 1, 0, 1)
      val half_quarter = Matrix4.rotation_x(math.Pi.toDouble / 4)
      val full_quarter = Matrix4.rotation_x(math.Pi.toDouble / 2)
      assert(half_quarter.multiply(p).equal(Tuple(0, math.sqrt(2).toDouble/2, math.sqrt(2).toDouble/2, 1)) &&
             full_quarter.multiply(p).equal(Tuple(0, 0, 1, 1)))
      true
    }
    test("The inverse of an x-rotation rotates in the opposite direction"){
      val p = Tuple(0, 1, 0, 1)
      val half_quarter = Matrix4.rotation_x(math.Pi.toDouble / 4)
      val inv = half_quarter.inverse.getOrElse(Matrix4.newZeroMatrix4)
      assert(inv.multiply(p).equal(Tuple(0, math.sqrt(2).toDouble/2, -math.sqrt(2).toDouble/2, 1)))
      true
    }
    test("Rotating a point around the y axis"){
      val p = Tuple(0, 0, 1, 1)
      val half_quarter = Matrix4.rotation_y(math.Pi.toDouble / 4)
      val full_quarter = Matrix4.rotation_y(math.Pi.toDouble / 2)
      assert(half_quarter.multiply(p).equal(Tuple(math.sqrt(2).toDouble/2, 0, math.sqrt(2).toDouble/2, 1)) &&
             full_quarter.multiply(p).equal(Tuple(1, 0, 0, 1)))
      true
    }
    test("Rotating a point around the z axis"){
      val p = Tuple(0, 1, 0, 1)
      val half_quarter = Matrix4.rotation_z(math.Pi.toDouble / 4)
      val full_quarter = Matrix4.rotation_z(math.Pi.toDouble / 2)
      assert(half_quarter.multiply(p).equal(Tuple(- math.sqrt(2).toDouble/2, math.sqrt(2).toDouble/2, 0, 1)) &&
             full_quarter.multiply(p).equal(Tuple(-1, 0, 0, 1)))
      true
    }
    test("A shearing transformation moves x in proportion to y"){
      val transform = Matrix4.shearing(1, 0, 0, 0, 0, 0)
      val p = Tuple(2, 3, 4, 1)
      assert(transform.multiply(p).equal(Tuple(5, 3, 4, 1)))
      true
    }
    test("A shearing transformation moves x in proportion to z"){
      val transform = Matrix4.shearing(0, 1, 0, 0, 0, 0)
      val p = Tuple(2, 3, 4, 1)
      assert(transform.multiply(p).equal(Tuple(6, 3, 4, 1)))
      true
    }
    test("A shearing transformation moves y in proportion to x"){
      val transform = Matrix4.shearing(0, 0, 1, 0, 0, 0)
      val p = Tuple(2, 3, 4, 1)
      assert(transform.multiply(p).equal(Tuple(2, 5, 4, 1)))
      true
    }
    test("A shearing transformation moves y in proportion to z"){
      val transform = Matrix4.shearing(0, 0, 0, 1, 0, 0)
      val p = Tuple(2, 3, 4, 1)
      assert(transform.multiply(p).equal(Tuple(2, 7, 4, 1)))
      true
    }
    test("A shearing transformation moves z in proportion to x"){
      val transform = Matrix4.shearing(0, 0, 0, 0, 1, 0)
      val p = Tuple(2, 3, 4, 1)
      assert(transform.multiply(p).equal(Tuple(2, 3, 6, 1)))
      true
    }
    test("A shearing transformation moves z in proportion to y"){
      val transform = Matrix4.shearing(0, 0, 0, 0, 0, 1)
      val p = Tuple(2, 3, 4, 1)
      assert(transform.multiply(p).equal(Tuple(2, 3, 7, 1)))
      true
    }
    test("Individual transformations are applied in sequence"){
      val p = Tuple(1, 0, 1, 1)
      
      val a = Matrix4.rotation_x(math.Pi.toDouble / 2)
      val b = Matrix4.scaling(5, 5, 5)
      val c = Matrix4.translation(0, 5, 7)
      
      // Rotation first, then scaling, then translation
      val p2 = a.multiply(p)
      val p3 = b.multiply(p2)
      val p4 = c.multiply(p3)
      
      
      assert(p2.equal(Tuple(1, -1, 0, 1)) &&
             p3.equal(Tuple(5, -5, 0, 1)) &&
             p4.equal(Tuple(5, 0, 7, 1)))
      true
    }
      
  }

}
