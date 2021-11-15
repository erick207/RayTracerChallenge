package raytracer

import collection.mutable.ArrayDeque
import raytracer.Utilities.EPSILON
import scala.math.{sqrt, pow, abs, max}
import scala.Double.{PositiveInfinity, NegativeInfinity}


case class Cone(var minimum: Double = Double.NegativeInfinity,
                var maximum: Double = Double.PositiveInfinity,
                var closed: Boolean = false) extends Shape {	
	
    var bbox: BBox = BBox(Tuple(NegativeInfinity, NegativeInfinity, NegativeInfinity, 1),
                          Tuple(PositiveInfinity, PositiveInfinity, PositiveInfinity, 1))
    
    def setLimit(): Unit = {
        val a = abs(minimum)
        val b = abs(maximum)
        val limit = max(a, b)
        
        bbox.min.x = -limit
        bbox.min.z = -limit
        
        bbox.max.x = limit
        bbox.max.z = limit
    }
    
	def setMinimum(min: Double): Unit = {
        minimum = min
        bbox.min.y = min
        setLimit
    }
    
    def setMaximum(max: Double): Unit = {
        maximum = max
        bbox.max.y = max
        setLimit
    }    
    
	def localIntersect(r: Ray): Unit = {
        
        val a = pow(r.direction.x, 2) -
                pow(r.direction.y, 2) +
                pow(r.direction.z, 2)
        
        val b = 2 * r.origin.x * r.direction.x -
                2 * r.origin.y * r.direction.y +
                2 * r.origin.z * r.direction.z

        val c = pow(r.origin.x, 2) -
                pow(r.origin.y, 2) +
                pow(r.origin.z, 2)
        
        if(Utilities.equal(a, 0)) {
            // the ray misses if b = 0 too
            if(!Utilities.equal(b, 0)) {
                val t = - c / (2 * b)
                r.addIntersections(ArrayDeque[Intersection](Intersection(t, this)))
            }
        }else {
            // same algorithm as for cylinders
            
            val disc = pow(b, 2) - 4 * a * c
            
            if(disc < 0) () // ray does not intersect the cone
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
    
    def checkCap(r: Ray, t: Double, radius: Double): Boolean = {
        val x = r.origin.x + t * r.direction.x
        val z = r.origin.z + t * r.direction.z
        
        (pow(x, 2) + pow(z, 2)) <= pow(radius, 2)
        //(pow(x, 2) + pow(z, 2)) <= abs(radius)
    }
    
    def intersectCaps(r: Ray): Unit = {
        if(!closed || Utilities.equal(r.direction.y, 0)) ()
        else {
            val xs = ArrayDeque[Intersection]()
            
            val t0 = (minimum - r.origin.y) / r.direction.y
            if(checkCap(r, t0, minimum)) xs += Intersection(t0, this)
            
            val t1 = (maximum - r.origin.y) / r.direction.y
            if(checkCap(r, t1, maximum)) xs += Intersection(t1, this)
            
            r.addIntersections(xs)
        }
    }
    
    def localNormalAt(localPoint: Tuple, i: Option[Intersection] = None): Tuple = {
        val dist = pow(localPoint.x, 2) + pow(localPoint.z, 2)
        
        if(dist < 1 && localPoint.y >= maximum - EPSILON) Tuple(0, 1, 0, 0)
        else if(dist < 1 && localPoint.y <= minimum + EPSILON) Tuple(0, -1, 0, 0)
        else {
            var y = sqrt(pow(localPoint.x, 2) + pow(localPoint.z, 2))
            if(localPoint.y > 0) y = -y
            
            Tuple(localPoint.x, y, localPoint.z, 0)
        }
    }
    
    override def toString: String =
		s" a cone "
}

object Cone {
    def apply(minimum: Double = Double.NegativeInfinity,
              maximum: Double = Double.PositiveInfinity,
              closed: Boolean = false): Cone = new Cone(minimum, maximum, closed)
}
