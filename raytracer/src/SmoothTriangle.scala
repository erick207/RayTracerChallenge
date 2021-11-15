package raytracer

import collection.mutable.ArrayDeque
import raytracer.Utilities.EPSILON
import scala.math._

case class SmoothTriangle(var p1: Tuple,
                          var p2: Tuple,
                          var p3: Tuple,
                          var n1: Tuple,
                          var n2: Tuple,
                          var n3: Tuple) extends Shape {
    
    var e1 = p2.subtract(p1)
    var e2 = p3.subtract(p1)
    var normal = e2.cross(e1).normalize

    var box = BBox()
    box.add(p1)
    box.add(p2)
    box.add(p3)
    
    var bbox = box

    def localIntersect(r: Ray): Unit = {
        val dir_cross_e2 = r.direction.cross(e2)
        val det = e1.dot(dir_cross_e2)
        
        if(abs(det) < EPSILON) ()
        else {
            val f = 1 / det
            val p1_to_origin = r.origin.subtract(p1)
            val u = f * (p1_to_origin.dot(dir_cross_e2))
            if(u < 0 || u > 1) ()
            else {
                val origin_cross_e1 = p1_to_origin.cross(e1)
                val v = f * r.direction.dot(origin_cross_e1)
                if(v < 0 || (u + v) > 1) ()
                else {
                    val t = f * e2.dot(origin_cross_e1)
                    r.addIntersections(ArrayDeque[Intersection](Intersection.withUV(t, this, u, v)))
                }
            }
            
        }
    }

    def localNormalAt(localPoint: Tuple, i: Option[Intersection] = None): Tuple = {
           val u = i.get.u.get
           val v = i.get.v.get
           
           n2.multiply(u).add(
           n3.multiply(v)).add(
           n1.multiply(1 - u - v))
    }
    
    override def toString: String =
		"A SMOOTH TRIANGLE\n"

}

object SmoothTriangle {
	def apply(p1: Tuple,
              p2: Tuple,
              p3: Tuple,
              n1: Tuple,
              n2: Tuple,
              n3: Tuple) = new SmoothTriangle(p1, p2, p3, n1, n2, n3)
}
