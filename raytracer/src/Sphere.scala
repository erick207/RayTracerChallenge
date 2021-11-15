package raytracer

import collection.mutable.ArrayDeque

case class Sphere() extends Shape {
        
        var bbox = BBox(Tuple(-1, -1, -1, 1), Tuple(1, 1, 1, 1))
        
	def localIntersect(r: Ray): Unit = {
		val sphereToRay: Tuple = r.origin.subtract(Tuple(0, 0, 0, 1))
		
		val a: Double = r.direction.dot(r.direction)
		val b: Double = r.direction.dot(sphereToRay) * 2
		val c: Double = sphereToRay.dot(sphereToRay) - 1
		
		val discriminant = scala.math.pow(b, 2) - 4 * a * c
                
		if(discriminant <  0){
                        // Don't add intersections
			()
		}else{
				val t1 = (-b - math.sqrt(discriminant)) / (2 * a)
				val t2 = (-b + math.sqrt(discriminant)) / (2 * a)
                                
				
				val ts = Array[Double](t1, t2).sorted
				
				val ts2 = ArrayDeque[Intersection](Intersection(ts(0), this), Intersection(ts(1), this))
				
				r.addIntersections(ts2)
		}
	}
	
	def localNormalAt(localPoint:Tuple, i: Option[Intersection] = None): Tuple = {
		val localNormal = localPoint.subtract(Tuple(0, 0, 0, 1))
		localNormal
	}
	
	override def toString: String =
		s" a sphere "
}

object Sphere {
	def apply() = new Sphere()
	
	def glassSphere(): Sphere = {
		val s = new Sphere()
		
		s.material.transparency = 1.0
		s.material.refractiveIndex = 1.5
		
		s
	}
}
