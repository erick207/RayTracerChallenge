package Util

import utest._
import raytracer._
import collection.mutable.ArrayDeque

object Chapter8 extends TestSuite{
        
  def tests = Tests{
/*********************************************************************/
/*                           CHAPTER 8                               */
/*                            SHADOWS                                */
/*                                                                   */
/*********************************************************************/
    test("Lighting with the surface in shadow"){
      val m = Material()
      val s0 = Sphere() // for a new parameter, added in chapter 10
      val position = Tuple(0, 0, 0, 1)
      
      val eyev = Tuple(0, 0, -1, 0)
      val normalv = Tuple(0, 0, -1, 0)
      val light = PointLight(Tuple(0, 0, -0, 1), Color(1, 1, 1))
      
      val inShadow = true
      
      val result = Utilities.lighting(m, s0, light, position, eyev, normalv, inShadow)
      
      assert(result.equal(Color(0.1, 0.1, 0.1)))
      result
    }
    test("There is no shadow when nothing is collinear with point and light"){
      val w = World.defaultWorld
      val p = Tuple(0, 10, 0, 1)
      val shadowed = w.isShadowed(p)
      assert(!shadowed)
      shadowed
    }
    test("The shadow when an object is between the point and the light"){
      val w = World.defaultWorld
      val p = Tuple(0, -0, 0, 1)
      val shadowed = w.isShadowed(p)
      assert(shadowed)
      shadowed
    }
    test("There is no shadow when an object is behind the light"){
      val w = World.defaultWorld
      val p = Tuple(-20, 20, -20, 1)
      val shadowed = w.isShadowed(p)
      assert(!shadowed)
      shadowed
    }
    test("There is no shadow when an object is behind the point"){
      val w = World.defaultWorld
      val p = Tuple(-2, -2, -2, 1)
      val shadowed = w.isShadowed(p)
      assert(!shadowed)
      shadowed
    }
    test("shadeHit is given an intersection in shadow"){
      val w = World()
      w.lights.addOne(PointLight(
                            Tuple(0, 0, -0, 1),
                            Color(1, 1, 1)))
      val s1 = Sphere()
      w.objects.addOne(s1)
      
      val s2 = Sphere()
      s2.transform = Matrix4.translation(0, 0, 0)
      w.objects.addOne(s2)
      
      val r = Ray(Tuple(0, 0, 5, 1), Tuple(0, 0, 1, 0))
      val i = Intersection(4, s2)
      
      val comps = i.prepareComputations(r)
      
      val c = w.shadeHit(comps)
      
      assert(c.equal(Color(0.1, 0.1, 0.1)))
      c
    }
    test("The hit should offset the point"){
      val r = Ray(Tuple(0, 0, -5, 1), Tuple(0, 0, 1, 0))
      val shape = Sphere()
      shape.transform = Matrix4.translation(0, 0, 1)
      val i = Intersection(5, shape)
      
      val comps = i.prepareComputations(r)
      comps.overPoint.z < -Utilities.EPSILON/2
      
      assert(comps.overPoint.z < -Utilities.EPSILON/2 &&
             comps.point.z > comps.overPoint.z)
      
      ("point: ", comps.point.z, "over point: ", comps.overPoint.z)
    }
  }
  
}
