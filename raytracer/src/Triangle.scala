package raytracer

import collection.mutable.ArrayDeque
import raytracer.Utilities.EPSILON
import scala.math._

case class Triangle(var p1: Tuple,
                    var p2: Tuple,
                    var p3: Tuple) extends Shape {
    
    var e1 = p2.subtract(p1)
    var e2 = p3.subtract(p1)
    var normal = e2.cross(e1).normalize
    
    def localNormalAt(localPoint: Tuple, i: Option[Intersection] = None): Tuple = this.normal

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
                    r.addIntersections(ArrayDeque[Intersection](Intersection(t, this)))
                }
            }
            
        }
    }
    
    var box = BBox()
    box.add(p1)
    box.add(p2)
    box.add(p3)
    
    var bbox = box
    
    override def toString: String =
		"TRIANGLE\n" + "P1: " + p1.toString + "\nP2: " + p2.toString + "\nP3: " + p3.toString + "\n"
    
}

object Triangle {
	def apply(p1: Tuple,
              p2: Tuple,
              p3: Tuple) = new Triangle(p1, p2, p3)
}
