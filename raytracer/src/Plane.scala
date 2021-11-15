package raytracer

import collection.mutable.ArrayDeque
import scala.Double.{PositiveInfinity, NegativeInfinity}


case class Plane() extends Shape {

        var bbox: BBox = BBox(Tuple(PositiveInfinity,0,PositiveInfinity, 1),
                              Tuple(NegativeInfinity,0,NegativeInfinity, 1))


	def localNormalAt(p: Tuple, i: Option[Intersection] = None): Tuple = 
		Tuple(0, 1, 0, 0)
		
	def localIntersect(r: Ray): Unit = {
		// paraller or coplanar ray, no intersections
		if(math.abs(r.direction.y).toDouble < Utilities.EPSILON)
			r.addIntersections(ArrayDeque[Intersection]())
		else {
			// intersecting from above or below
			val t = (-r.origin.y) / r.direction.y
			val ts = ArrayDeque[Intersection](Intersection(t, this))
			r.addIntersections(ts)
		}
		
		
	}
}
