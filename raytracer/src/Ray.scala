package raytracer

import collection.mutable.ArrayDeque

class Ray(var origin: Tuple, var direction: Tuple) {
	
	val intersections: ArrayDeque[Intersection] = ArrayDeque[Intersection]()

	def addIntersections(ts: ArrayDeque[Intersection]): Unit = {
		intersections ++= ts
		intersections.sortInPlace
	} 
	
	def position(t: Double): Tuple = {
		origin.add(direction.multiply(t))
	}
	
	def hit(): Option[Intersection] = {
		intersections.find(_.t >= 0)
	}
	
	def transform(m: Matrix4): Ray = {
		val to = m.multiply(this.origin)
		val td = m.multiply(this.direction)
		Ray(to, td)
	}
	
        def getIntersectionsString(): String = intersections.map(_.t.toString).mkString(", ")
        
	override def toString: String =
		s"\nRay:\tO:\t$origin \n     \tD:\t$direction"
}

object Ray {
	def apply(origin: Tuple, direction: Tuple) = new Ray(origin, direction)
}
