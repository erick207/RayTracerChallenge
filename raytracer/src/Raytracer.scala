package raytracer

import raytracer.Matrix4._
import raytracer.Color._
import scala.math._
import raytracer.Utilities._
import raytracer.Scenes._
import raytracer.Operation._
import raytracer.Material._


object RayTracer{

  def main(args: Array[String]): Unit = {
    println("Ray Tracer Challenge\n--------------\n\n")
    
    val w = World()
    
    val light = PointLight(Tuple(-99, 99, -99, 1), WHITE)
    w.lights += light
    
    val room = Cube()
    room.setTransform(translation(0, 100, 0).multiply(scaling(100, 100, 100)))
    
    val roomPat = CheckersPattern(SNOW, PEACHPUFF)
    roomPat.setTransform(translation(0.01, 0.01, 0.01).multiply(scaling(0.1, 0.1, 0.1)))
    
    room.material.setPattern(roomPat)
    room.material.specular = 0
    room.material.reflective = 0.15
    
    w.objects += room
    
    val topDice = dice(translation(10,33,0).multiply(rotation_x(Pi).multiply(rotation_y((3.0/2) * Pi ))),
                        colorMaterialWithReflection(CYAN, 0.1),
                        colorMaterial(ANTIQUEWHITE))
    w.objects += topDice
    
    val rightDice = dice(translation(25,11,0).multiply(rotation_x(Pi)),
                         colorMaterialWithReflection(FORESTGREEN, 0.1),
                         colorMaterial(ANTIQUEWHITE))
    w.objects += rightDice
    
    val leftDice = dice(translation(-5,11,0).multiply(rotation_y((21.0 / 12) * Pi)),
                       colorMaterialWithReflection(TOMATO, 0.1),
                       colorMaterial(ANTIQUEWHITE))
    w.objects += leftDice
    
    val whiteDice = dice(translation(-25, 11, 35).multiply(rotation_y((22.5 / 24) * Pi)),
                       colorMaterialWithReflection(ANTIQUEWHITE, 0.1),
                       colorMaterial(ALMOSTBLACK))
    
    w.objects += whiteDice
    
    w.objects.map(_.divide(1))

    val c = Camera(1920, 1080, math.Pi.toDouble/3)
    c.transform = Matrix4.viewTransform(Tuple(-99, 85, -99, 1),
                                        Tuple(10, 24, 0, 1),
                                        Tuple(0, 1, 0, 0))
    val canvas = c.render(w)
        
    val ppm = PPM.canvasToPPM(canvas)
    PPM.writeFile(ppm)
  }
}
