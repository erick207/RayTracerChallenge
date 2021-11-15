package Util

import utest._
import raytracer._
import collection.mutable.ArrayDeque

object Chapter10 extends TestSuite{
        
  def tests = Tests{
/*********************************************************************/
/*                           CHAPTER 10                              */
/*                            PATTERNS                               */
/*                                                                   */
/*********************************************************************/
    test("Creating a stripe pattern"){
      val pattern = StripePattern(Color.WHITE, Color.BLACK) 

      assert(pattern.a.equal(Color.WHITE) &&
             pattern.b.equal(Color.BLACK))
      pattern
    }
    test("A stripe pattern is constant in y"){
      val pattern = StripePattern(Color.WHITE, Color.BLACK) 
      
      assert(pattern.colorAtPatternPoint(Tuple(0, 0, 0, 1)).equal(Color.WHITE) &&
             pattern.colorAtPatternPoint(Tuple(0, 1, 0, 1)).equal(Color.WHITE) &&
             pattern.colorAtPatternPoint(Tuple(0, 2, 0, 1)).equal(Color.WHITE) 
            ) 
      pattern
    }
    test("A stripe pattern is constant in z"){
      val pattern = StripePattern(Color.WHITE, Color.BLACK) 
      
      assert(pattern.colorAtPatternPoint(Tuple(0, 0, 0, 1)).equal(Color.WHITE) &&
             pattern.colorAtPatternPoint(Tuple(0, 0, 1, 1)).equal(Color.WHITE) &&
             pattern.colorAtPatternPoint(Tuple(0, 0, 2, 1)).equal(Color.WHITE)
            ) 
      pattern
    }
    test("A stripe pattern alternates in x"){
      val pattern = StripePattern(Color.WHITE, Color.BLACK) 
      
      assert(pattern.colorAtPatternPoint(Tuple(0, 0, 0, 1)).equal(Color.WHITE) &&
             pattern.colorAtPatternPoint(Tuple(0.9, 0, 0, 1)).equal(Color.WHITE) &&
             pattern.colorAtPatternPoint(Tuple(1, 0, 0, 1)).equal(Color.BLACK) &&
             pattern.colorAtPatternPoint(Tuple(-0.1, 0, 0, 1)).equal(Color.BLACK) &&
             pattern.colorAtPatternPoint(Tuple(-1, 0, 0, 1)).equal(Color.BLACK) &&
             pattern.colorAtPatternPoint(Tuple(-1.1, 0, 0, 1)).equal(Color.WHITE)
             ) 
      pattern
    }
    test("Lighting with a pattern applied"){
      val m = Material()
      val s0 = Sphere() // for a new parameter, added in chapter 10
      m.setPattern(StripePattern(Color.WHITE, Color.BLACK))
      m.ambient = 1
      m.diffuse = 0
      m.specular = 0
      val eyev = Tuple(0, 0, -1, 0)
      val normalv = Tuple(0, 0, -1, 0)
      val light = PointLight(Tuple(0, 0, -0, 1), Color(1, 1, 1))
      
      val c1 = Utilities.lighting(m, s0, light, Tuple(0.9, 0, 0, 1), eyev, normalv, false)
      val c2 = Utilities.lighting(m, s0, light, Tuple(1.1, 0, 0, 1), eyev, normalv, false)
      
      assert(c1.equal(Color.WHITE) &&
             c2.equal(Color.BLACK))
      (c1, c2)
    }
    test("Stripes with an object transformation"){
      val obj = Sphere()
      val pattern = StripePattern(Color.WHITE, Color.BLACK)
      obj.transform = Matrix4.scaling(2, 2, 2)
      
      val c = pattern.colorAtObjectPoint(obj, Tuple(1.5, 0, 0, 1))
      
      assert(c.equal(Color.WHITE))
      c
    }
    test("Stripes with a pattern tranformation"){
      val obj = Sphere()
      val pattern = StripePattern(Color.WHITE, Color.BLACK)
      pattern.setTransform(Matrix4.scaling(2, 2, 2))
      
      val c = pattern.colorAtObjectPoint(obj, Tuple(1.5, 0, 0, 1))
      
      assert(c.equal(Color.WHITE))
      c
    }
    test("Stripes with both an object and a pattern transformation"){
      val obj = Sphere()
      val pattern = StripePattern(Color.WHITE, Color.BLACK)
      obj.transform = Matrix4.scaling(2, 2, 2)
      pattern.setTransform(Matrix4.translation(0.5, 0, 0))

      val c = pattern.colorAtObjectPoint(obj, Tuple(2.5, 0, 0, 1))

      assert(c.equal(Color.WHITE))
      c
    }
    test("A gradient linearly interpolates between colors"){
      val p = GradientPattern(Color.WHITE, Color.BLACK)
      
      assert(p.colorAtPatternPoint(Tuple(0, 0, 0, 1)).equal(Color.WHITE) &&
             p.colorAtPatternPoint(Tuple(0.25, 0, 0, 1)).equal(Color(0.75, 0.75, 0.75)) &&
             p.colorAtPatternPoint(Tuple(0.5, 0, 0, 1)).equal(Color(0.5, 0.5, 0.5)) &&
             p.colorAtPatternPoint(Tuple(0.75, 0, 0, 1)).equal(Color(0.25, 0.25, 0.25)))
      p
    }
    test("A ring should extend in both x and z"){
      val p = RingPattern(Color.WHITE, Color.BLACK)
      
      assert(p.colorAtPatternPoint(Tuple(0, 0, 0, 1)).equal(Color.WHITE) &&
             p.colorAtPatternPoint(Tuple(1, 0, 0, 1)).equal(Color.BLACK) &&
             p.colorAtPatternPoint(Tuple(0, 0, 1, 1)).equal(Color.BLACK) &&
             p.colorAtPatternPoint(Tuple(0.708, 0, 0.708, 1)).equal(Color.BLACK))
      p
    }
    test("Checkers should repeat in x"){
      val p = CheckersPattern(Color.WHITE, Color.BLACK)
      
      assert(p.colorAtPatternPoint(Tuple(0, 0, 0, 1)).equal(Color.WHITE) &&
             p.colorAtPatternPoint(Tuple(0.99, 0, 0, 1)).equal(Color.WHITE) &&
             p.colorAtPatternPoint(Tuple(1.01, 0, 0, 1)).equal(Color.BLACK)
             )
      p
    }
    test("Checkers should repeat in y"){
      val p = CheckersPattern(Color.WHITE, Color.BLACK)
      
      assert(p.colorAtPatternPoint(Tuple(0, 0, 0, 1)).equal(Color.WHITE) &&
             p.colorAtPatternPoint(Tuple(0, 0.99, 0, 1)).equal(Color.WHITE) &&
             p.colorAtPatternPoint(Tuple(0, 1.01, 0, 1)).equal(Color.BLACK)
             )
      p
    }
    test("Checkers should repeat in z"){
      val p = CheckersPattern(Color.WHITE, Color.BLACK)
      
      assert(p.colorAtPatternPoint(Tuple(0, 0, 0, 1)).equal(Color.WHITE) &&
             p.colorAtPatternPoint(Tuple(0, 0, 0.99, 1)).equal(Color.WHITE) &&
             p.colorAtPatternPoint(Tuple(0, 0, 1.01, 1)).equal(Color.BLACK)
             )
      p
    }
  }
  
}
