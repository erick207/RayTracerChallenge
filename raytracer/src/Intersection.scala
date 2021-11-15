package raytracer

import collection.mutable.ArrayDeque
import util.control.Breaks._

class Intersection(var t: Double,
                   var shape: Shape) extends Ordered[Intersection] {
	
        var _u: Option[Double] = None
        var _v: Option[Double] = None

        def u(): Option[Double] = _u
        def v(): Option[Double] = _v
        
        def setU(u: Double): Unit = 
                _u = Some(u)
                
        
        def setV(v: Double): Unit = 
                _v = Some(v)
        
	def prepareComputations(r: Ray): Computations = {
		val point = r.position(this.t)
		var normalv = this.shape.normalAt(point, Some(this))
		val eyev = Tuple(0, 0, 0, 0).subtract(r.direction)
		val inside = if(normalv.dot(eyev) < 0) {
			normalv = normalv.multiply(-1)
			true
		} else false
		val overPoint = point.add(normalv.multiply(Utilities.EPSILON))
		val underPoint = point.subtract(normalv.multiply(Utilities.EPSILON))

		
		val reflectv = r.direction.reflect(normalv)
		
		// Chapter 11, calculating n1 and n2
		val (n1, n2) = findNs(r)

		
		Computations(this.t,
		             this.shape,
		             point,
		             eyev,
		             normalv,
		             inside,
		             overPoint,
		             underPoint,
		             reflectv,
		             n1,
		             n2
		             )
	}
	
	def compare(that: Intersection) = {
		//if(this.t == that.t)
		if(Utilities.equal(this.t, that.t))
		  0
		else if(this.t > that.t)
		  1
		else
		  -1
	}
	
	def findNs(r: Ray): (Double, Double) = {
		
		val containers = ArrayDeque[Shape]()
		var n1 = 0.0
		var n2 = 0.0
		 
		for(i <- r.intersections){
			if(i eq r.hit.getOrElse(Intersection(-9, Sphere()))) {
				if(containers.isEmpty)
					n1 = 1
				else
					n1 = containers.last.material.refractiveIndex
			}

			if(containers.contains(i.shape))
				containers -= i.shape
			else
				containers += i.shape

			if(i eq r.hit.getOrElse(Intersection(-9, Sphere()))) {
				if(containers.isEmpty)
					n2 = 1
				else
					n2 = containers.last.material.refractiveIndex
	
				val a: Double = n1
				val b: Double = n2
				return (a, b) // terminate loop
			}
		}
		(n1, n2)
	}
	
	override def toString: String =
		s"\nIntersection: $t\n\tshape:\t$shape u: " + u + " v:" + v
}

object Intersection {
	def apply(t: Double, shape: Shape) = new Intersection(t, shape)
        
        def withUV(t: Double,
                   shape: Shape,
                   u: Double,
                   v: Double): Intersection = {
                val i = new Intersection(t, shape)
                i.setU(u)
                i.setV(v)
                i
        }
	
}
