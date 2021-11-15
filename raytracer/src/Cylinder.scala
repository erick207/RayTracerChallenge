package raytracer

import collection.mutable.ArrayDeque
import raytracer.Utilities.EPSILON
import scala.math.{sqrt, pow}
import scala.Double.{PositiveInfinity, NegativeInfinity}


case class Cylinder(var minimum: Double = Double.NegativeInfinity,
                    var maximum: Double = Double.PositiveInfinity,
                    var closed: Boolean = false) extends Shape {	
            
    var bbox: BBox = BBox(Tuple(-1, NegativeInfinity, -1, 1),
                          Tuple(1, PositiveInfinity, 1, 1))

	def setMinimum(min: Double): Unit = {
        minimum = min
        bbox.min.y = min
    }
    
    def setMaximum(max: Double): Unit = {
        maximum = max
        bbox.max.y = max
    }
    
	def localIntersect(r: Ray): Unit = {
        val a = pow(r.direction.x, 2) + pow(r.direction.z, 2)
        
        if(Utilities.equal(a, 0)) this.intersectCaps(r)  // ray is parallel to the y axis, simply call intersectCaps
        else {
            val b = 2 * r.origin.x * r.direction.x +
                    2 * r.origin.z * r.direction.z
            
            val c = pow(r.origin.x, 2) + pow(r.origin.z, 2) - 1
            
            val disc = pow(b, 2) - 4 * a * c
            
            if(disc < 0) () // ray does not intersect the cylinder
            else {
                var t0 = (-b - sqrt(disc)) / (2 * a)
                var t1 = (-b + sqrt(disc)) / (2 * a)
                
                if(t0 > t1) {
                    var temp = t0
                    t0 = t1
                    t1 = temp
                }
                
                val xs = ArrayDeque[Intersection]()
                
                val y0 = r.origin.y + t0 * r.direction.y
                if(this.minimum < y0 && y0 < this.maximum) {
                    xs += Intersection(t0, this)
                }
                
                val y1 = r.origin.y + t1 * r.direction.y
                if(this.minimum < y1 && y1 < this.maximum){
                    xs += Intersection(t1, this)
                }
                
                r.addIntersections(xs)
                
                intersectCaps(r)
            }
        }
    }
    
    def localNormalAt(localPoint: Tuple, i: Option[Intersection] = None): Tuple = {
        val dist = pow(localPoint.x, 2) + pow(localPoint.z, 2)
        
        if(dist < 1 && localPoint.y >= maximum - Utilities.EPSILON) Tuple(0, 1, 0, 0)
        else if(dist < 1 && localPoint.y <= minimum + Utilities.EPSILON) Tuple(0, -1, 0, 0)
        else Tuple(localPoint.x, 0, localPoint.z, 0)
    }

    def checkCap(r: Ray, t: Double): Boolean = {
        val x = r.origin.x + t * r.direction.x
        val z = r.origin.z + t * r.direction.z
        
        (pow(x, 2) + pow(z, 2)) <= 1.0
    }
    
    def intersectCaps(r: Ray): Unit = {
        if(!closed || Utilities.equal(r.direction.y, 0)) ()
        else {
            val xs = ArrayDeque[Intersection]()
            
            val t0 = (minimum - r.origin.y) / r.direction.y
            if(checkCap(r, t0)) xs += Intersection(t0, this)
            
            val t1 = (maximum - r.origin.y) / r.direction.y
            if(checkCap(r, t1)) xs += Intersection(t1, this)
            
            r.addIntersections(xs)
        }
    }
    
    

    override def toString: String =
		s" a cylinder "
    
}

object Cylinder {
    def apply(minimum: Double = Double.NegativeInfinity,
              maximum: Double = Double.PositiveInfinity,
              closed: Boolean = false): Cylinder = new Cylinder(minimum, maximum, closed)
}
