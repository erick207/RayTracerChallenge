package Util

import utest._
import raytracer._
import collection.mutable.ArrayDeque

object Chapter2 extends TestSuite{
        
  def tests = Tests{
/*********************************************************************/
/*                           CHAPTER 2                               */
/*                        DRAWING ON A CANVAS                        */
/*                                                                   */
/*********************************************************************/

    test("Creating a canvas"){      
      val width = 10
      val height = 20
      val result = new Canvas(width, height)
      val black = Color(0, 0, 0)
      assert(result.canvas(0).length == width &&
             result.canvas.length == height && 
             result.canvas.forall(row => row.forall(pixel => black.equal(pixel)))
             )
      result
    }
    test("Writing pixel to a canvas"){
      val result = new Canvas(10, 20)
      val red = Color(1, 0, 0)
      
      result.writePixel(8, 12, red)
      //println("canvas: " + result.pixelAt(8, 12).toString + " \ncolor: " + red.toString)
      assert(result.pixelAt(8, 12).equal(red))
      result
    }
    test("Constructing the PPM header"){
      val correctHeader = "P3\n5 3\n255"
      val cv = new Canvas(5, 3)
      val result = PPM.canvasToPPM(cv)
      assert(result.startsWith(correctHeader))
      result
    }
    test("Constructing the PPM pixel data"){
      val correctData = 
      "255 0 0 0 0 0 0 0 0 0 0 0 0 0 0\n0 0 0 0 0 0 0 128 0 0 0 0 0 0 0\n0 0 0 0 0 0 0 0 0 0 0 0 0 0 255"
      
      val cv = new Canvas(5, 3)
      cv.writePixel(0,0, Color(1.5, 0, 0))
      cv.writePixel(2,1, Color(0, 0.5, 0))
      cv.writePixel(4,2, Color(-0.5, 0, 1))
      val result = PPM.canvasToPPM(cv)
      
      assert(result.split("\n").drop(3).mkString("\n").startsWith(correctData))
      result
    }
    test("PPM files are terminated by a newline character"){
      val cv = new Canvas(5, 3)
      val result = PPM.canvasToPPM(cv)
      assert(result.endsWith("\n"))
      result
    }
  }
  
}
