package Util

import utest._
import raytracer._
import raytracer.Operation._
import collection.mutable.ArrayDeque

object Chapter16 extends TestSuite{
        
  def tests = Tests{
/*********************************************************************/
/*                           CHAPTER 16                              */
/*                    CONSTRUCTIVE SOLID GEOMETRY                    */
/*                              (CSG)                                */
/*********************************************************************/
    test("CSG is created with an operation and two shapes"){
      val s1 = Sphere()
      val s2 = Cube()
      val c = CSG(UnionOp, s1, s2)
      
      assert((c.left eq s1) &&
             (c.right eq s2) &&
             (s1.parent.get eq c) &&
             (s2.parent.get eq c))
      c
    }
    /*test(""){
      val s
      
      assert()
      s
    }*/
  }
  
}
