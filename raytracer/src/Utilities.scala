package raytracer

import scala.math._
import raytracer.Color._
import raytracer.Matrix4._

object Utilities {
	// book: 0.00001
	val EPSILON = 0.00001

	def equal(a: Double, b: Double): Boolean = {
		val epsilon = EPSILON
		if (math.abs(a - b) < epsilon) true
		else false
	}
        
	// Phong Reflection Model
	def lighting(m: Material,
                     obj: Shape,
	             light: PointLight,
	             point: Tuple, 
	             eyev: Tuple, 
	             normalv: Tuple,
	             inShadow: Boolean): Color = {
		
		val color = m.pattern.fold(m.color)(_.colorAtObjectPoint(obj, point))
		
		if(inShadow) {
			val effectiveColor = color.multiplyColor(light.intensity)
			val ambient = effectiveColor.multiply(m.ambient)
			ambient
		}else{
		
		var diffuse = Color.BLACK
		var specular = Color.BLACK
		
		// Combine the surface color with the light's color/intensity
		val effectiveColor = color.multiplyColor(light.intensity)
		
		// Find the direction of the light source
		// val lightv = light.position.normalize.subtract(point)
		val lightv = light.position.subtract(point).normalize

		
		// Compute the ambient contribution
		val ambient = effectiveColor.multiply(m.ambient)
		
		// lightDotNormal represents the cosine of the angles between the
		// light vector and the normal vector. A negative number means the 
		// light is on the other side of the surface.
		val lightDotNormal = lightv.dot(normalv)
		if(lightDotNormal < 0) {
			diffuse = Color.BLACK
			specular = Color.BLACK
		}else {
			// Compute the diffuse contribution
			diffuse = effectiveColor.multiply(lightDotNormal * m.diffuse)
			
			
			// reflectDotEye represents the cosine of the angle between the
			// reflection vector and the eye vector. A negative number means the
			// light reflects away from the eye.
			val reflectv = lightv.multiply(-1).reflect(normalv)
			val reflectDotEye = reflectv.dot(eyev)
			if(reflectDotEye <= 0) {
				specular = Color.BLACK
			}else {
				// Compute the specular contribution
				val factor = math.pow(reflectDotEye.toDouble, m.shininess.toDouble).toDouble
				specular = light.intensity.multiply(m.specular * factor)
			}
		}
		
		// Add the three contributions together to get the final shading
		ambient.
		add(diffuse).
		add(specular)
	
		}
		
	}
        
        def spiral(a: Double, t: Double): (Double, Double) = {
                (a * t * cos(t).toDouble, a * t * sin(t).toDouble)
        }
        
        def circumferenceCoorGenerator(ox: Double, oy: Double, r: Double, n: Int):IndexedSeq[(Double, Double)] = {
                val theta: Double = 2 * Pi.toDouble / n
                (1 to n).map(i => (ox + r * cos(2 * Pi.toDouble - theta * i).toDouble,
                                   oy + r * sin(2 * Pi.toDouble - theta * i).toDouble))
        }
        
        def hexagonCorner(): Sphere = {
                val corner = Sphere()
                corner.setTransform(translation(0, 0, -1).multiply(
                                    scaling(0.25, 0.25, 0.25)))
                corner
        }
        
        def hexagonEdge(): Cylinder = {
                val edge = Cylinder()
                edge.minimum = 0
                edge.maximum = 1
                edge.setTransform(translation(0, 0, -1).multiply(
                                  rotation_y(-Pi/6).multiply(
                                  rotation_z(-Pi/2).multiply(
                                  scaling(0.25, 1, 0.25)))))
                edge
        }
        
        def hexagonSide(): Group = {
                val side = Group()
                
                side.add(hexagonCorner())
                side.add(hexagonEdge())
                
                side
        }
        
        def hexagon(): Group = {
                val hex = Group()
                
                (0 to 5).map( n => { val side = hexagonSide()
                                     side.setTransform(rotation_y(n * (Pi/3)))
                                     hex.add(side)
                                   }
                )
                hex
        }
        
}

object Operation extends Enumeration {
  type Operation = Value
  val UnionOp, IntersectionOp, DifferenceOp = Value
}
