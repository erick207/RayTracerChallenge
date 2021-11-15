package raytracer

class PointLight(var position: Tuple, var intensity: Color) {
	
}

object PointLight {
	def apply(position: Tuple, intensity: Color) = new PointLight(position, intensity)	
}
