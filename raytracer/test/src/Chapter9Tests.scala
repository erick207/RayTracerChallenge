package Util

import utest._
import raytracer._
import collection.mutable.ArrayDeque

object Chapter9 extends TestSuite{
        
  def tests = Tests{
/*********************************************************************/
/*                           CHAPTER 9                               */
/*                             PLANES                                */
/*                                                                   */
/*********************************************************************/
    test("The normal of a plane is constant everywhere"){
      val p = Plane()
      val n1 = p.localNormalAt(Tuple(0, 0, 0, 1))
      val n2 = p.localNormalAt(Tuple(0, 0, -0, 1))
      val n3 = p.localNormalAt(Tuple(-5, 0, 0, 1))
      assert(Tuple(0, 1, 0, 0).equal(n1) &&
             n1.equal(n2) &&
             n2.equal(n3))
      n1
    }
    test("Intersect with a ray parallel to the plane"){
      val p = Plane()
      val r = Ray(Tuple(0, 0, 0, 1), Tuple(0, 0, 1, 0))
      p.localIntersect(r)

      assert(r.intersections.isEmpty)
      r.intersections.length
    }
    test("Intersect with a coplanar ray"){
      val p = Plane()
      val r = Ray(Tuple(0, 0, 0, 1), Tuple(0, 0, 1, 0))
      p.localIntersect(r)

      assert(r.intersections.isEmpty)
      r.intersections.length
    }
    test("A ray intersecting a plane from above"){
      val p = Plane()
      val r = Ray(Tuple(0, 1, 0, 1), Tuple(0, -1, 0, 0))
      p.localIntersect(r)

      assert(r.intersections.length == 1 &&
             Utilities.equal(r.intersections(0).t, 1) &&
             r.intersections(0).shape.equals(p))
      r.intersections.length
    }
    test("A ray intersecting a plane fron below"){
      val p = Plane()
      val r = Ray(Tuple(0, -1, 0, 1), Tuple(0, 1, 0, 0))
      p.localIntersect(r)

      assert(r.intersections.length == 1 &&
             Utilities.equal(r.intersections(0).t, 1) &&
             r.intersections(0).shape.equals(p))
      r.intersections.length
    }
  }
  
}
