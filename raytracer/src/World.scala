package raytracer

import collection.mutable.ArrayDeque

case class World() {
	val objects = ArrayDeque[Shape]()
	val lights = ArrayDeque[PointLight]()
	
	// called `intersect_world(w, r)` in the book
	// the ray keeps the intersections internally and sorted,
	// this just calls intersect for every object and loads the intersections
	// into the Ray.intersections attribute 
	def intersect(r: Ray): Unit = {
				
		objects
		.map(_.intersect(r))
		
		//r.intersections
	}
	
        
	// Single light source supported only. Page 96 for details on multiple light source support.
	def shadeHit(comps: Computations, remaining: Int = 1): Color = {
		
		val surface = Utilities.lighting(comps.shape.material,
						   comps.shape,
		                   this.lights(0),
		                   comps.point,
		                   comps.eyev,
		                   comps.normalv,
		                   this.isShadowed(comps.overPoint))
		
		val reflected = this.reflectedColor(comps, remaining)
		val refracted = this.refractedColor(comps, remaining)
		
		val material = comps.shape.material
		if(material.reflective > 0 && material.transparency > 0) {
			val reflectance = this.schlick(comps)
			surface
			.add(reflected.multiply(reflectance))
			.add(refracted.multiply(1 - reflectance))
		} else {
			surface
			.add(reflected)
			.add(refracted)
		}
	}
	
	def schlick(comps: Computations): Double = {
		var cos: Double = comps.eyev.dot(comps.normalv)
		
		if(comps.n1 > comps.n2) {
			val n = comps.n1 / comps.n2
			val sin2t = (Math.pow(n, 2) * (1 - Math.pow(cos, 2))).toDouble
			if(sin2t > 1) return 1
			
			val cost = Math.sqrt(1 - sin2t).toDouble
			
			cos = cost
		}
		
		val r0: Double = Math.pow(((comps.n1 - comps.n2) / (comps.n1 + comps.n2)), 2).toDouble
		
		r0 + (1 - r0) * Math.pow((1 - cos), 5).toDouble	
	}
	
	def isShadowed(p: Tuple): Boolean = {
		val v = this.lights(0).position.subtract(p)
		val distance = v.magnitude
		val direction = v.normalize
		
		val r = Ray(p, direction)
		this.intersect(r)
		
		// Option[Intersection]
		val h = r.hit
		// if there is a hit less than distance, isShadowed
		h.exists(_.t < distance)
		
		// alternatively:
		//h.exists(_.t - distance < -Utilities.EPSILON)
	}
	
	def colorAt(r: Ray, remaining: Int = 1): Color = {
		// call intersectWorld to find the intersections of the given ray with the given world
		this.intersect(r)
		
		// find the hit from the resulting intersections, return BLACK if no hit is found
		r.hit() match {
			case None => Color(0, 0, 0)
			// Otherwise, precompute and call shadeHit
			case Some(i) => {
				val comps = i.prepareComputations(r)
				this.shadeHit(comps, remaining)
			}
		}
		
	}
	
	def reflectedColor(comps: Computations, remaining: Int): Color = {
		if(remaining < 1 || Utilities.equal(comps.shape.material.reflective, 0)) Color.BLACK
		else {
			val reflectRay = Ray(comps.overPoint, comps.reflectv)
			val color = this.colorAt(reflectRay, remaining - 1)
			color.multiply(comps.shape.material.reflective)
		}
	}
	
	def refractedColor(comps: Computations, remaining: Int): Color = 
		if(remaining < 1 || Utilities.equal(comps.shape.material.transparency, 0)) Color.BLACK
		else {
			// Snell's Law
			val nRatio: Double = comps.n1 / comps.n2
			val cosi = comps.eyev.dot(comps.normalv)
			val sin2t: Double = (Math.pow(nRatio, 2) * (1 - Math.pow(cosi, 2))).toDouble
			
			if(sin2t > 1) Color.BLACK // there is total internal reflection
			else {
				// finding the refracted color
				val cost = Math.sqrt(1 - sin2t).toDouble
				val direction = comps.normalv
				                     .multiply(nRatio * cosi - cost)
				                     .subtract(comps.eyev
				                               .multiply(nRatio))
				                               
				val refractRay = Ray(comps.underPoint, direction)
				
				val color = this.colorAt(refractRay, remaining - 1)
				            .multiply(comps.shape.material.transparency)
				
				color
			}
		}
	
	override def toString: String =
	s" a world "
}

object World {
	def apply() = new World()
	
	def defaultWorld(): World = {
		// outer, first object
		val s1 = Sphere()
		s1.material.color = Color(0.8, 1.0, 0.6)
		s1.material.diffuse = 0.7
		s1.material.specular = 0.2
		
		// inner, second object
		val s2 = Sphere()
		s2.setTransform(Matrix4.scaling(0.5, 0.5, 0.5))
		
		val light = PointLight(Tuple(-10, 10, -10, 1), Color(1, 1, 1))
		
		val w = World()
		
		w.objects.addOne(s1)
		w.objects.addOne(s2)
		
		w.lights.addOne(light)
		
		w
	}
}
