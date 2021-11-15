package Util

import utest._
import raytracer._
import collection.mutable.ArrayDeque

object Chapter12 extends TestSuite{
        
  def tests = Tests{
/*********************************************************************/
/*                           CHAPTER 12                              */
/*                              CUBES                                */
/*                                                                   */
/*********************************************************************/
    /*
    test("A ray intersects a cube +x"){
      val c = Cube()
      val r = Ray(Tuple(5, 0.5, 0, 1), Tuple(-1, 0, 0, 0))
      c.localIntersect(r)
      
      assert(r.intersections.length == 2 &&
             Utilities.equal(r.intersections(0).t, 4) &&
             Utilities.equal(r.intersections(1).t, 6))
      
      r.intersections.length
    }
    test("A ray intersects a cube -x"){
      val c = Cube()
      val r = Ray(Tuple(-5, 0.5, 0, 1), Tuple(1, 0, 0, 0))
      c.localIntersect(r)
      
      assert(r.intersections.length == 2 &&
             Utilities.equal(r.intersections(0).t, 4) &&
             Utilities.equal(r.intersections(1).t, 6))
      
      r.intersections.length
    }
    test("A ray intersects a cube +y"){
      val c = Cube()
      val r = Ray(Tuple(0.5, 5, 0, 1), Tuple(0, -1, 0, 0))
      c.localIntersect(r)
      
      assert(r.intersections.length == 2 &&
             Utilities.equal(r.intersections(0).t, 4) &&
             Utilities.equal(r.intersections(1).t, 6))
      
      r.intersections.length
    }
    test("A ray intersects a cube -y"){
      val c = Cube()
      val r = Ray(Tuple(0.5, -5, 0, 1), Tuple(0, 1, 0, 0))
      c.localIntersect(r)
      
      assert(r.intersections.length == 2 &&
             Utilities.equal(r.intersections(0).t, 4) &&
             Utilities.equal(r.intersections(1).t, 6))
      
      r.intersections.length
    }
    test("A ray intersects a cube +z"){
      val c = Cube()
      val r = Ray(Tuple(0.5, 0, 5, 1), Tuple(0, 0, -1, 0))
      c.localIntersect(r)
      
      assert(r.intersections.length == 2 &&
             Utilities.equal(r.intersections(0).t, 4) &&
             Utilities.equal(r.intersections(1).t, 6))
      
      r.intersections.length
    }
    test("A ray intersects a cube -z"){
      val c = Cube()
      val r = Ray(Tuple(0.5, 0, -5, 1), Tuple(0, 0, 1, 0))
      c.localIntersect(r)
      
      assert(r.intersections.length == 2 &&
             Utilities.equal(r.intersections(0).t, 4) &&
             Utilities.equal(r.intersections(1).t, 6))
      
      r.intersections.length
    }
    test("A ray intersects a cube from inside"){
      val c = Cube()
      val r = Ray(Tuple(0, 0.5, 0, 1), Tuple(0, 0, 1, 0))
      c.localIntersect(r)
      
      assert(r.intersections.length == 2 &&
             Utilities.equal(r.intersections(0).t, -1) &&
             Utilities.equal(r.intersections(1).t, 1))
      
      r.intersections.length
    }
    test("A ray misses a cube test 1"){
      val c = Cube()
      val r = Ray(Tuple(-2, 0, 0, 1), Tuple(0.2673, 0.5345, 0.8018, 0))
      c.localIntersect(r)
      
      assert(r.intersections.length == 0)
      
      r.intersections.length
    }
    test("A ray misses a cube test 2"){
      val c = Cube()
      val r = Ray(Tuple(0, -2, 0, 1), Tuple(0.8018, 0.2673, 0.5345, 0))
      c.localIntersect(r)
      
      assert(r.intersections.length == 0)
      
      r.intersections.length
    }
    test("A ray misses a cube test 3"){
      val c = Cube()
      val r = Ray(Tuple(0, 0, -2, 1), Tuple(0.5345, 0.8018, 0.2673, 0))
      c.localIntersect(r)
      
      assert(r.intersections.length == 0)
      
      r.intersections.length
    }
    test("A ray misses a cube test 4"){
      val c = Cube()
      val r = Ray(Tuple(2, 0, 2, 1), Tuple(0, 0, -1, 0))
      c.localIntersect(r)
      
      assert(r.intersections.length == 0)
      
      r.intersections.length
    }
    test("A ray misses a cube test 5"){
      val c = Cube()
      val r = Ray(Tuple(0, 2, 2, 1), Tuple(0, -1, 0, 0))
      c.localIntersect(r)
      
      assert(r.intersections.length == 0)
      
      r.intersections.length
    }
    test("A ray misses a cube test 6"){
      val c = Cube()
      val r = Ray(Tuple(2, 2, 0, 1), Tuple(-1, 0, 0, 0))
      c.localIntersect(r)
      
      assert(r.intersections.length == 0)
      
      r.intersections.length
    }
    test("The normal on the surface of a cube 1"){
      val c = Cube()
      val p = Tuple(1, 0.5, -0.8, 1)
      val n = c.localNormalAt(p)
      
      assert(n.equal(Tuple(1, 0, 0, 0)))
      
      n
    }
    test("The normal on the surface of a cube 2"){
      val c = Cube()
      val p = Tuple(-1, -0.2, 0.9, 1)
      val n = c.localNormalAt(p)
      
      assert(n.equal(Tuple(-1, 0, 0, 0)))
      
      n
    }
    test("The normal on the surface of a cube 3"){
      val c = Cube()
      val p = Tuple(-0.4, 1, -0.1, 1)
      val n = c.localNormalAt(p)
      
      assert(n.equal(Tuple(0, 1, 0, 0)))
      
      n
    }
    test("The normal on the surface of a cube 4"){
      val c = Cube()
      val p = Tuple(0.3, -1, -0.7, 1)
      val n = c.localNormalAt(p)
      
      assert(n.equal(Tuple(0, -1, 0, 0)))
      
      n
    }
    test("The normal on the surface of a cube 5"){
      val c = Cube()
      val p = Tuple(-0.6, 0.3, 1, 1)
      val n = c.localNormalAt(p)
      
      assert(n.equal(Tuple(0, 0, 1, 0)))
      
      n
    }
    test("The normal on the surface of a cube 6"){
      val c = Cube()
      val p = Tuple(0.4, 0.4, -1, 1)
      val n = c.localNormalAt(p)
      
      assert(n.equal(Tuple(0, 0, -1, 0)))
      
      n
    }
    test("The normal on the surface of a cube 7"){
      val c = Cube()
      val p = Tuple(1, 1, 1, 1)
      val n = c.localNormalAt(p)
      
      assert(n.equal(Tuple(1, 0, 0, 0)))
      
      n
    }
    test("The normal on the surface of a cube 8"){
      val c = Cube()
      val p = Tuple(-1, -1, -1, 1)
      val n = c.localNormalAt(p)
      
      assert(n.equal(Tuple(-1, 0, 0, 0)))
      
      n
    }
    
    */
  }
  
}
