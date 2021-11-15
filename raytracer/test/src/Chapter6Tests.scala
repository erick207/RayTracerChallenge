package Util

import utest._
import raytracer._
import collection.mutable.ArrayDeque

object Chapter6 extends TestSuite{
        
  def tests = Tests{
/*********************************************************************/
/*                           CHAPTER 6                               */
/*                        LIGHT AND SHADING                          */
/*                                                                   */
/*********************************************************************/
    test("The normal on a sphere at a point on the x axis"){
      val s = Sphere()
      val n = s.normalAt(Tuple(1, 0, 0, 1))
      assert(n.equal(Tuple(1, 0, 0, 0)))
      n
    }
    test("The normal on a sphere at a point on the y axis"){
      val s = Sphere()
      val n = s.normalAt(Tuple(0, 1, 0, 1))
      assert(n.equal(Tuple(0, 1, 0, 0)))
      n
    }
    test("The normal on a sphere at a point on the z axis"){
      val s = Sphere()
      val n = s.normalAt(Tuple(0, 0, 1, 1))
      assert(n.equal(Tuple(0, 0, 1, 0)))
      n
    }
    test("The normal on a sphere at a non axial point"){
      val s = Sphere()
      val n = s.normalAt(Tuple(math.sqrt(3).toDouble / 3,
                               math.sqrt(3).toDouble / 3, 
                               math.sqrt(3).toDouble / 3,
                               1))
      assert(n.equal(Tuple(math.sqrt(3).toDouble / 3,
                           math.sqrt(3).toDouble / 3, 
                           math.sqrt(3).toDouble / 3,
                           0)))
      n
    }
    test("The normal is a normalized vector"){
      val s = Sphere()
      val n = s.normalAt(Tuple(math.sqrt(3).toDouble / 3,
                               math.sqrt(3).toDouble / 3, 
                               math.sqrt(3).toDouble / 3,
                               1))
      assert(n.equal(n.normalize()))
      n
    }
    test("Computing the normal on a translated sphere"){
      val s = Sphere()
      s.setTransform(Matrix4.translation(0, 1, 0))
      val n = s.normalAt(Tuple(0,
                               1.70711, 
                               -0.70711,
                               1))
      assert(n.equal(Tuple(0,
                           0.70711, 
                           -0.70711,
                           0)))
      n
    }
    test("Computing the normal on a transformed sphere"){
      val s = Sphere()
      val m4 = Matrix4.scaling(1, 0.5, 1)
                      .multiply(Matrix4.rotation_z((math.Pi / 5).toDouble))
      s.setTransform(m4)
      val n = s.normalAt(Tuple(0,
                               math.sqrt(2).toDouble / 2, 
                               (- math.sqrt(2).toDouble) / 2,
                               1))
      assert(n.equal(Tuple(0,
                           0.97014, 
                           -0.24254,
                           0)))
      n
    }
    test("Reflecting a vector apporoaching at 45°"){
      val v = Tuple(1, -1, 0, 0)
      val n = Tuple(0, 1, 0, 0)
      val r = v.reflect(n)
      assert(r.equal(Tuple(1, 1, 0, 0)))
      r
    }
    test("Reflecting a vector off a slanted surface"){
      val v = Tuple(0, -1, 0, 0)
      val n = Tuple(math.sqrt(2).toDouble / 2,
                    math.sqrt(2).toDouble / 2,
                    0,
                    0)
      val r = v.reflect(n)
      assert(r.equal(Tuple(1, 0, 0, 0)))
      r
    }
    test("A point light has a position and intensity"){
      val position = Tuple(0, 0, 0, 1)
      val intensity = Color(1, 1, 1)
      val light = PointLight(position, intensity)
      assert(light.position.equal(position) &&
             light.intensity.equal(intensity))
      (light.position, "\n", light.intensity)
    }
    test("The default material"){
      val m = Material()
      assert(m.color.equal(Color(1, 1, 1)) &&
             Utilities.equal(m.ambient ,0.1) &&
             Utilities.equal(m.diffuse ,0.9) &&
             Utilities.equal(m.specular ,0.9) &&
             Utilities.equal(m.shininess ,200.0))
      m
    }
    test("A sphere has a default material"){
      val s = Sphere()
      val m = s.material
      assert(m.equal(Material()))
      m
    }
    test("A sphere may be assigned a material"){
      val s = Sphere()
      val m = Material()
      m.ambient = 1
      s.material = m
      assert(s.material.equal(m))
      s.material
    }
    test("Lighting with the eye between the light and the surface"){
      val s0 = Sphere() // for a new parameter, added in chapter 10
      val m = Material()
      val position = Tuple(0, 0, 0, 1)
      
      val eyev = Tuple(0, 0, -1, 0)
      val normalv = Tuple(0, 0, -1, 0)
      val light = PointLight(Tuple(0, 0, -10, 1), Color(1, 1, 1))
      val result = Utilities.lighting(m, s0, light, position, eyev, normalv, false)
      
      assert(result.equal(Color(1.9, 1.9, 1.9)))
      result
    }
    test("Lighting with the eye between light and surface, eye offset 45°"){
      val m = Material()
      val s0 = Sphere() // for a new parameter, added in chapter 10
      val position = Tuple(0, 0, 0, 1)
      
      val eyev = Tuple(0,
                       math.sqrt(2) / 2,
                       math.sqrt(2) / 2,
                       0)
      val normalv = Tuple(0, 0, -1, 0)
      val light = PointLight(Tuple(0, 0, -10, 1), Color(1, 1, 1))
      val result = Utilities.lighting(m, s0, light, position, eyev, normalv, false)
      
      assert(result.equal(Color(1.0, 1.0, 1.0)))
      result
    }
    test("Lighting with eye opposite surface, light offset 45°"){
      val m = Material()
      val s0 = Sphere() // for a new parameter, added in chapter 10
      val position = Tuple(0, 0, 0, 1)
      
      val eyev = Tuple(0, 0, -1, 0)
      val normalv = Tuple(0, 0, -1, 0)
      val light = PointLight(Tuple(0, 10, -10, 1), Color(1, 1, 1))
      val result = Utilities.lighting(m, s0, light, position, eyev, normalv, false)
      
      assert(result.equal(Color(0.7364, 0.7364, 0.7364)))
      result
    }
    test("Lighting with eye in the path of the reflection vector"){
      val m = Material()
      val s0 = Sphere() // for a new parameter, added in chapter 10
      val position = Tuple(0, 0, 0, 1)
      
      val eyev = Tuple(0,
                       -math.sqrt(2) / 2,
                       -math.sqrt(2) / 2,
                       0)
      val normalv = Tuple(0, 0, -1, 0)
      val light = PointLight(Tuple(0, 10, -10, 1), Color(1, 1, 1))
      val result = Utilities.lighting(m, s0, light, position, eyev, normalv, false)
      
      assert(result.equal(Color(1.63639, 1.63639, 1.63639)))
      result
    }
    test("Lighting with the light behind the surface"){
      val m = Material()
      val s0 = Sphere() // for a new parameter, added in chapter 10
      val position = Tuple(0, 0, 0, 1)
      
      val eyev = Tuple(0, 0, -1, 0)
      val normalv = Tuple(0, 0, -1, 0)
      val light = PointLight(Tuple(0, 0, 10, 1), Color(1, 1, 1))
      val result = Utilities.lighting(m, s0, light, position, eyev, normalv, false)
      
      assert(result.equal(Color(0.1, 0.1, 0.1)))
      result
    }
  }
  
}
