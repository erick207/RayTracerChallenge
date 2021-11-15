package Util

import utest._
import raytracer._
import collection.mutable.ArrayDeque

object Chapter5 extends TestSuite{
        
  def tests = Tests{
/*********************************************************************/
/*                           CHAPTER 5                               */
/*                      RAY-SPHERE INTERSECTIONS                     */
/*                                                                   */
/*********************************************************************/
    test("Creating and querying a ray"){
      val origin = Tuple(1, 2, 3, 1)
      val direction = Tuple(4, 5, 6, 0)
      val r = Ray(origin, direction)
      
      assert(r.origin.equals(origin) &&
             r.direction.equals(direction))
      r
    }
    test("Computing a point from a distance"){
      val r = Ray(Tuple(2, 3, 4, 1), Tuple(1, 0, 0, 0))
            
      assert(r.position(0).equal(Tuple(2, 3, 4, 1)) &&
             r.position(1).equal(Tuple(3, 3, 4, 1)) &&
             r.position(-1).equal(Tuple(1, 3, 4, 1)) &&
             r.position(2.5).equal(Tuple(4.5, 3, 4, 1)))
             
      r
    }
    test("A ray intersects a sphere at two points"){
      val s = Sphere()
      var r = Ray(Tuple(0, 0, -5, 1), Tuple(0, 0, 1, 0))
              // this needs to be added to the Ray to pass this test after refactoring in chapter 9
              .transform(s.transform.inverse.getOrElse(Matrix4.newZeroMatrix4))
      // need to call the wrapped function too
      s.localIntersect(r)

      assert(r.intersections.length == 2 &&
             Utilities.equal(r.intersections(0).t, 4) &&
             Utilities.equal(r.intersections(1).t, 6)
            )
      r.intersections.length
    }
    test("A ray intersects a sphere at a tangent"){
      val s = Sphere()
      val r = Ray(Tuple(0, 1, -5, 1), Tuple(0, 0, 1, 0))
              // this needs to be added to the Ray to pass this test after chapter 9
              .transform(s.transform.inverse.getOrElse(Matrix4.newZeroMatrix4))
      s.localIntersect(r)
      
      assert(r.intersections.length == 2 &&
             Utilities.equal(r.intersections(0).t, 5) &&
             Utilities.equal(r.intersections(1).t, 5)
            )
      r.intersections.length
    }
    test("A ray misses a sphere"){
      val s = Sphere()
      val r = Ray(Tuple(0, 2, -5, 1), Tuple(0, 0, 1, 0))
              // this needs to be added to the Ray to pass this test after refactoring in chapter 9
              .transform(s.transform.inverse.getOrElse(Matrix4.newZeroMatrix4))
      // need to call the wrapped function too
      s.localIntersect(r)

      
      
      assert(r.intersections.length == 0)
      r.intersections.length
    }
    test("Edge case: A ray originates inside a sphere"){
      val s = Sphere()
      val r = Ray(Tuple(0, 0, 0, 1), Tuple(0, 0, 1, 0))
              // this needs to be added to the Ray to pass this test after refactoring in chapter 9
              .transform(s.transform.inverse.getOrElse(Matrix4.newZeroMatrix4))
      // need to call the wrapped function too
      s.localIntersect(r)
      
      assert(r.intersections.length == 2 &&
             Utilities.equal(r.intersections(0).t, -1) &&
             Utilities.equal(r.intersections(1).t, 1)
            )
      r.intersections.length
    }
    test("Edge case: A sphere is behind the ray"){
      val s = Sphere()
      val r = Ray(Tuple(0, 0, 5, 1), Tuple(0, 0, 1, 0))
              // this needs to be added to the Ray to pass this test after refactoring in chapter 9
              .transform(s.transform.inverse.getOrElse(Matrix4.newZeroMatrix4))
      // need to call the wrapped function too
      s.localIntersect(r)
      
      assert(r.intersections.length == 2 &&
             Utilities.equal(r.intersections(0).t, -6) &&
             Utilities.equal(r.intersections(1).t, -4)
            )
      r.intersections.length
    }
    test("An intersection encapsulates t and object"){
      val s = Sphere()
      val i = Intersection(3.5, s)
      assert(Utilities.equal(i.t, 3.5) &&
             i.shape.equals(s))
      (i.t, i.shape)
    }
    test("Aggregating intersections"){
      val s = Sphere()
      val i1 = Intersection(1, s)
      val i2 = Intersection(2, s)
      val xs = Array(i1, i2)
      
      assert(xs.length == 2 &&
             xs(0).t == 1 &&
             xs(1).t == 2)
      (xs.length, xs(0).t, xs(1).t)
    }
    test("intersect sets the object on the intersection"){
      val s = Sphere()
      val r = Ray(Tuple(0, 0, -5, 1), Tuple(0, 0, 1, 0))
              // this needs to be added to the Ray to pass this test after refactoring in chapter 9
              .transform(s.transform.inverse.getOrElse(Matrix4.newZeroMatrix4))
      // need to call the wrapped function too
      s.localIntersect(r)
      
      assert(r.intersections.length == 2 &&
             r.intersections(0).shape.equals(s) &&
             r.intersections(1).shape.equals(s))
      
      (r.intersections.length, r.intersections(0).shape, r.intersections(1).shape)
    }
    test("The hit, when all intersections have positive t"){
      val r = Ray(Tuple(0, 0, 0, 1), Tuple(0, 0, 0, 0))
      val s = Sphere()
      val i1 = Intersection(1, s)
      val i2 = Intersection(2, s)
      r.addIntersections(ArrayDeque(i1, i2))
      
      assert(r.hit.get eq i1)
      r.hit.get.t
    }
    test("The hit, when some intersections have negative t"){
      val r = Ray(Tuple(0, 0, 0, 1), Tuple(0, 0, 0, 0))
      val s = Sphere()
      val i1 = Intersection(-1, s)
      val i2 = Intersection(1, s)
      r.addIntersections(ArrayDeque(i1, i2))
      
      assert(r.hit.get eq i2)
      r.hit.get.t
    }
    test("The hit, when all intersections have negative t"){
      val r = Ray(Tuple(0, 0, 0, 1), Tuple(0, 0, 0, 0))
      val s = Sphere()
      val i1 = Intersection(-2, s)
      val i2 = Intersection(-1, s)
      r.addIntersections(ArrayDeque(i1, i2))
      
      assert(r.hit.isEmpty)
      r.hit.isEmpty
    }
    test("The hit is always the lowest nonnegative intersection"){
      val r = Ray(Tuple(0, 0, 0, 1), Tuple(0, 0, 0, 0))
      val s = Sphere()
      val i1 = Intersection(5, s)
      val i2 = Intersection(7, s)
      val i3 = Intersection(-3, s)
      val i4 = Intersection(2, s)

      r.addIntersections(ArrayDeque(i1, i2, i3, i4))
      
      assert(r.hit.get eq i4)
      r.hit.get.t
    }
    test("Translating a ray"){
      val r = Ray(Tuple(1, 2, 3, 1), Tuple(0, 1, 0, 0))
      val m = Matrix4.translation(3, 4, 5)
      val r2 = r.transform(m)
      assert(r2.origin.equal(Tuple(4, 6, 8, 1)) &&
             r2.direction.equal(Tuple(0, 1, 0, 0)))
      (r2.origin, "\n", r2.direction)
    }
    test("Scaling a ray"){
      val r = Ray(Tuple(1, 2, 3, 1), Tuple(0, 1, 0, 0))
      val m = Matrix4.scaling(2, 3, 4)
      val r2 = r.transform(m)
      assert(r2.origin.equal(Tuple(2, 6, 12, 1)) &&
             r2.direction.equal(Tuple(0, 3, 0, 0)))
      (r2.origin, "\n", r2.direction)
    }
    test("A sphere's default transformation"){
      val s = Sphere()
      assert(s.transform.equal(Matrix4.identityMatrix()))
      s.transform
    }
    test("Changing a sphere's transformation(translation)"){
      val s = Sphere()
      val t = Matrix4.translation(2, 3, 4)
      s.setTransform(t)
      assert(s.transform.equal(t))
      s.transform
    }
    test("Intersecting a scaled sphere with a ray"){
      val s = Sphere()
      s.setTransform(Matrix4.scaling(2, 2, 2))
      val r = Ray(Tuple(0, 0, -5, 1), Tuple(0, 0, 1, 0))
              // this needs to be added to the Ray to pass this test after refactoring in chapter 9
              .transform(s.transform.inverse.getOrElse(Matrix4.newZeroMatrix4))
      // need to call the wrapped function too
      s.localIntersect(r)
      
      assert(r.intersections.length == 2 &&
             Utilities.equal(3, r.intersections(0).t) &&
             Utilities.equal(7, r.intersections(1).t))
      (r.intersections.length, r.intersections(0).t, r.intersections(1).t)
    }
    test("Intersecting a translated sphere with a ray"){
      val r = Ray(Tuple(0, 0, -5, 1), Tuple(0, 0, 1, 0))
      val s = Sphere()
      s.setTransform(Matrix4.translation(5, 0, 0))
      s.intersect(r)
      assert(r.intersections.length == 0)
      r.intersections.length
    }
  }
  
}
