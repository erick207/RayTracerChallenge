package Util

import utest._
import raytracer._
import collection.mutable.ArrayDeque

object Chapter7 extends TestSuite{
        
  def tests = Tests{
/*********************************************************************/
/*                           CHAPTER 7                               */
/*                         MAKING A SCENE                            */
/*                                                                   */
/*********************************************************************/
    test("Creating a world"){
      val w = World()
      assert(w.objects.length == 0 &&
             w.lights.length == 0)
      w
    }
    test("The default world"){
      val w = World.defaultWorld()
      assert(w.objects(0).material.color.equal(Color(0.8, 1.0, 0.6))  &&
             w.objects(0).material.diffuse == 0.7 &&
             w.objects(0).material.specular == 0.2 &&
             
             w.objects(1).transform.equal(Matrix4.scaling(0.5, 0.5, 0.5)))
      w
    }
    test("Intersect a world with a ray"){
      val w = World.defaultWorld()
      val r = Ray(Tuple(0, 0, -5, 1), Tuple(0, 0, 1, 0))
      
      w.intersect(r)
      val xs = r.intersections
      
      assert(xs.length == 4 &&
             xs(0).t == 4 &&
             xs(1).t == 4.5 &&
             xs(2).t == 5.5 &&
             xs(3).t == 6)
      (xs(0).t, xs(1).t, xs(2).t, xs(3).t)
    }
    test("Precomputing the state of an intersection"){
      val r = Ray(Tuple(0, 0, -5, 1), Tuple(0, 0, 1, 0))
      val shape = Sphere()
      val i = Intersection(4, shape)
      
      val comps = i.prepareComputations(r)
      
      assert(Utilities.equal(comps.t, i.t) &&
             (comps.shape eq i.shape) &&  // reference equality
             comps.point.equal(Tuple(0, 0, -1, 1)) &&
             comps.eyev.equal(Tuple(0, 0, -1, 0)) &&
             comps.normalv.equal(Tuple(0, 0, -1, 0)))
      
      (comps.t, comps.shape, comps.point, comps.eyev, comps.normalv)
    }
    test("The hit, when an intersection occurs on the outside"){
      val r = Ray(Tuple(0, 0, -5, 1), Tuple(0, 0, 1, 0))
      val shape = Sphere()
      val i = Intersection(4, shape)
      
      val comps = i.prepareComputations(r)
      
      assert(!comps.inside)
      comps.inside
    }
    test("The hit, when an intersection occurs on the inside"){
      val r = Ray(Tuple(0, 0, 0, 1), Tuple(0, 0, 1, 0))
      val shape = Sphere()
      val i = Intersection(4, shape)
      
      val comps = i.prepareComputations(r)
      
      assert(comps.inside &&
             comps.normalv.equal(Tuple(0, 0, -1, 0)))
      (comps.inside, comps.normalv)
    }
    test("Shading an intersection"){
      val w = World.defaultWorld()
      val r = Ray(Tuple(0, 0, -5, 1), Tuple(0, 0, 1, 0))
      val shape = w.objects(0)
      val i = Intersection(4, shape)
      
      val comps = i.prepareComputations(r)
      
      val c = w.shadeHit(comps)
      
      assert(c.equal(Color(0.38066, 0.47583, 0.2855)))
      c
    }
    test("Shading an intersection from the inside"){
      val w = World.defaultWorld()
      w.lights(0) = PointLight(Tuple(0, 0.25, 0, 1), Color(1, 1, 1))
      val r = Ray(Tuple(0, 0, 0, 1), Tuple(0, 0, 1, 0))
      val shape = w.objects(1)
      val i = Intersection(0.5, shape)
      
      val comps = i.prepareComputations(r)
      
      val c = w.shadeHit(comps)
      
      assert(c.equal(Color(0.90498, 0.90498, 0.90498)))
      c
    }
    test("The color when a ray misses"){
      val w = World.defaultWorld()
      val r = Ray(Tuple(0, 0, -5, 1), Tuple(0, 1, 0, 0))
      
      val c = w.colorAt(r)
      
      assert(c.equal(Color(0, 0, 0)))
      c
    }
    test("The color when a ray hits"){
      val w = World.defaultWorld()
      val r = Ray(Tuple(0, 0, -5, 1), Tuple(0, 0, 1, 0))
      
      val c = w.colorAt(r)
      
      assert(c.equal(Color(0.38066, 0.47583, 0.2855)))
      c
    }
    test("The color with an intersection behind the ray"){
      val w = World.defaultWorld()
      
      val outer = w.objects(0)
      outer.material.ambient = 1
      
      val inner = w.objects(1)
      inner.material.ambient = 1
      
      val r = Ray(Tuple(0, 0, 0.75, 1), Tuple(0, 0, -1, 0))
      
      val c = w.colorAt(r)
      
      assert(c.equal(inner.material.color))
      c
    }
    test("The transformation matrix for the default orientation"){
      val from = Tuple(0, 0, 0, 1)
      val to = Tuple(0, 0, -1, 1)
      val up = Tuple(0, 1, 0, 0)
      
      val t = Matrix4.viewTransform(from, to, up)
      
      assert(t.equal(Matrix4.identityMatrix))
      t
    }
    test("A view transformation matrix looking in positive z direction"){
      val from = Tuple(0, 0, 0, 1)
      val to = Tuple(0, 0, 1, 1)
      val up = Tuple(0, 1, 0, 0)
      
      val t = Matrix4.viewTransform(from, to, up)
      
      assert(t.equal(Matrix4.scaling(-1, 1, -1)))
      t
    }
    test("The view transformation moves the world"){
      val from = Tuple(0, 0, 8, 1)
      val to = Tuple(0, 0, 0, 1)
      val up = Tuple(0, 1, 0, 0)
      
      val t = Matrix4.viewTransform(from, to, up)
      
      assert(t.equal(Matrix4.translation(0, 0, -8)))
      t
    }
    test("An arbitrary view transformation"){
      val from = Tuple(1, 3, 2, 1)
      val to = Tuple(4, -2, 8, 1)
      val up = Tuple(1, 1, 0, 0)
      
      val t = Matrix4.viewTransform(from, to, up)
      
      val tAssert = Matrix4(-0.50709,  0.50709,  0.67612, -2.36643,
                             0.76772,  0.60609,  0.12122, -2.82843,
                            -0.35857,  0.59761, -0.71714,  0.00000,
                             0.00000,  0.00000,  0.00000,  1.00000)
      
      assert(t.equal(tAssert))
      t
    }
    test("Constructing a camera"){
      val hSize = 160 
      val vSize = 120
      val fov = math.Pi.toDouble/2
      
      val c = Camera(hSize, vSize, fov)
      
      assert(c.hSize == hSize &&
             c.vSize == vSize &&
             Utilities.equal(c.fov, fov) &&
             c.transform.equal(Matrix4.identityMatrix))
      c
    }
    test("The pixel size for a horizontal canvas"){
      val c = Camera(200, 125, math.Pi.toDouble/2)
      assert(Utilities.equal(c.pixelSize, 0.01))
      c
    }
    test("The pixel size for a vertical canvas"){
      val c = Camera(125, 200, math.Pi.toDouble/2)
      assert(Utilities.equal(c.pixelSize, 0.01))
      c
    }
    test("Constructing a ray trought the center of the canvas"){
      val c = Camera(201, 101, math.Pi.toDouble/2)
      val r = c.rayForPixel(100, 50)
      assert(r.origin.equal(Tuple(0, 0, 0, 1)) &&
             r.direction.equal(Tuple(0, 0, -1, 0)))
      r
    }
    test("Constructing a ray trought a corner of the canvas"){
      val c = Camera(201, 101, math.Pi.toDouble/2)
      val r = c.rayForPixel(0, 0)
      assert(r.origin.equal(Tuple(0, 0, 0, 1)) &&
             r.direction.equal(Tuple(0.66519, 0.33259, -0.66851, 0)))
      r
    }
    test("Constructing a ray when the camera is transformed"){
      val c = Camera(201, 101, math.Pi.toDouble/2)
      c.transform = Matrix4.rotation_y(math.Pi.toDouble/4).multiply(
                    Matrix4.translation(0, -2, 5))
      val r = c.rayForPixel(100, 50)
      assert(r.origin.equal(Tuple(0, 2, -5, 1)) &&
             r.direction.equal(Tuple(math.sqrt(2).toDouble/2, 0, -math.sqrt(2).toDouble/2, 0)))
      r
    }
    test("Rendering a world with a camera"){
      val w = World.defaultWorld
      val c = Camera(11, 11,math.Pi.toDouble/2)
      val from = Tuple(0, 0, -5, 1)
      val to = Tuple(0, 0, 0, 1)
      val up = Tuple(0, 1, 0, 0)
      c.transform = Matrix4.viewTransform(from, to, up)
      
      val image = c.render(w)
      
      assert(image.pixelAt(5, 5).equal(Color(0.38066, 0.47583, 0.2855)))
      image.pixelAt(5, 5)
    }

  }
  
}
