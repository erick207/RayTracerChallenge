package raytracer

import scala.Double.{PositiveInfinity, NegativeInfinity}
import raytracer.Utilities.EPSILON
import collection.mutable.ArrayDeque
import scala.math._

class BBox(var min: Tuple = Tuple(PositiveInfinity, PositiveInfinity, PositiveInfinity, 1),
           var max: Tuple = Tuple(NegativeInfinity, NegativeInfinity, NegativeInfinity, 1)) {
    
    def add(p: Tuple): Unit = {
        min.x = min.x min p.x
        min.y = min.y min p.y
        min.z = min.z min p.z
        
        max.x = max.x max p.x
        max.y = max.y max p.y
        max.z = max.z max p.z
    }
    
    def add(bbox: BBox): Unit = {
        add(bbox.min)
        add(bbox.max)
    }
    
    def contains(p: Tuple): Boolean = {
        p.x >= min.x && p.x <= max.x &&
        p.y >= min.y && p.y <= max.y &&
        p.z >= min.z && p.z <= max.z 
    }
    
    def contains(bbox: BBox): Boolean = contains(bbox.min) && contains(bbox.max)
    
    def transform(m: Matrix4): BBox = {
        val p =Array(min,
                Tuple(min.x, min.y, max.z, 1),
                Tuple(min.x, max.y, min.z, 1),
                Tuple(min.x, max.y, max.z, 1),
                Tuple(max.x, min.y, min.z, 1),
                Tuple(max.x, min.y, max.z, 1),
                Tuple(max.x, max.y, min.z, 1),
                max)
        
        val bbox = BBox()
        (0 to 7).map(i => bbox.add(m.multiply(p(i))))
        bbox
    }
    
    def checkAxis(origin: Double, direction: Double, min: Double, max: Double): (Double, Double) = {
        val tminNumerator = (min - origin)
        val tmaxNumerator = (max - origin)
        
        val (tmin, tmax) = if(math.abs(direction) >= EPSILON) 
                                (tminNumerator / direction, tmaxNumerator / direction)
                           else 
                                (tminNumerator * PositiveInfinity, tmaxNumerator * PositiveInfinity)
                                
        if(tmin > tmax) (tmax, tmin)
        else (tmin, tmax)
        
    }
    
    def intersect(r: Ray): Boolean = {
        val (xtmin, xtmax) = checkAxis(r.origin.x, r.direction.x, min.x, max.x)
        val (ytmin, ytmax) = checkAxis(r.origin.y, r.direction.y, min.y, max.y)
        val (ztmin, ztmax) = checkAxis(r.origin.z, r.direction.z, min.z, max.z)
        
        val tmin = xtmin max ytmin max ztmin
        val tmax = xtmax min ytmax min ztmax
        
        if(tmin > tmax)
            false
        else
            true
        }
        
    def splitBounds(): (BBox, BBox) = {
        //val dx = abs(min.x - max.x)
        //val dy = abs(min.y - max.y)
        //val dz = abs(min.z - max.z)
        
        val dx = max.x - min.x
        val dy = max.y - min.y
        val dz = max.z - min.z
        
        val greatest = dx max dy max dz
        
        // helpers
        val t0 = Tuple(min.x, min.y, min.z, 1)
        val t1 = Tuple(max.x, max.y, max.z, 1)
        
        if(greatest == dx) {
            t0.x = min.x + dx / 2
            t1.x = t0.x
        }else if(greatest == dy) {
            t0.y = min.y + dy / 2
            t1.y = t0.y
        } else {
            t0.z = min.z + dz / 2
            t1.z = t0.z
        }
        
        val midMin = t0
        val midMax = t1
        
        (BBox(min, midMax),
         BBox(midMin, max))
    }
    
    override def toString: String =
        "BBox:\nMin: " + min.toString + "\nMax: " + max.toString
}

object BBox {
	def apply(min: Tuple = Tuple(PositiveInfinity, PositiveInfinity, PositiveInfinity, 1),
              max: Tuple = Tuple(NegativeInfinity, NegativeInfinity, NegativeInfinity, 1))
                  = new BBox(min, max)
}
