package raytracer

import collection.mutable.ArrayDeque
import raytracer.Utilities.EPSILON

case class Cube() extends Shape {

	var bbox = BBox(Tuple(-1, -1, -1, 1), Tuple(1, 1, 1, 1))
        
	def localIntersect(r: Ray): Unit = {
                val (xtmin, xtmax) = checkAxis(r.origin.x, r.direction.x)
                val (ytmin, ytmax) = checkAxis(r.origin.y, r.direction.y)
                val (ztmin, ztmax) = checkAxis(r.origin.z, r.direction.z)
        
                val tmin = xtmin max ytmin max ztmin
                val tmax = xtmax min ytmax min ztmax
        
                if(tmin > tmax)
                        ()
                else
                        r.addIntersections(ArrayDeque[Intersection](Intersection(tmin, this), Intersection(tmax, this)))
        }
    
    def checkAxis(origin: Double, direction: Double): (Double, Double) = {
        val tminNumerator = (-1 - origin)
        val tmaxNumerator = (1 - origin)
        
        val (tmin, tmax) = if(math.abs(direction).toDouble >= EPSILON) 
                                (tminNumerator / direction, tmaxNumerator / direction)
                           else 
                                (tminNumerator * Double.PositiveInfinity, tmaxNumerator * Double.PositiveInfinity)
                                
        if(tmin > tmax) (tmax, tmin)
        else (tmin, tmax)
    }
    
    def localNormalAt(localPoint: Tuple, i: Option[Intersection] = None): Tuple = {
        val absX = math.abs(localPoint.x) 
        val absY = math.abs(localPoint.y)
        val absZ = math.abs(localPoint.z)
        
        val maxc = absX max absY max absZ
        
        if(maxc == absX) Tuple(localPoint.x, 0, 0, 0)
        else if(maxc == absY) Tuple(0, localPoint.y, 0, 0)
        else Tuple(0, 0, localPoint.z, 0)
        
    }
    
    override def toString: String =
		s" a cube "
}

object Cube {
    
	def apply() = new Cube()
    
}
