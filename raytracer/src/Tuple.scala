package raytracer

class Tuple(var x: Double = 0, var y: Double = 0, var z: Double = 0, var w: Double = 0) {
	override def toString: String =
		s"TUPLE $x, $y, $z, $w"
		
	def equal(t: Tuple):Boolean =
		Utilities.equal(x, t.x) && Utilities.equal(y, t.y) && Utilities.equal(z, t.z) && Utilities.equal(w, t.w)
		
	def add(t: Tuple): Tuple = 
		Tuple(x + t.x, y + t.y, z + t.z, w + t.w)
    
	def subtract(t: Tuple): Tuple =
		Tuple(x - t.x, y - t.y, z - t.z, w - t.w)
    
	def negate(): Tuple =
		Tuple(-x, -y, -z, -w)
    
	def multiply(k: Double): Tuple =
		Tuple(x * k, y * k, z * k, w * k)
    
	def divide(k: Double): Tuple =
		// evaluates to Tuple
		multiply(1 / k)
    
	def magnitude(): Double =
		math.sqrt(x * x +
              y * y +
              z * z +
              w * w).toDouble
    
	def normalize(): Tuple = {
		val ma = this.magnitude
		Tuple(x / ma, y / ma, z / ma, w / ma)
	}
  
	def dot(t: Tuple): Double = 
		x * t.x +
		y * t.y +
		z * t.z +
		w * t.w
    
	def cross(t: Tuple): Tuple =
		Tuple(y * t.z - z * t.y,
		z * t.x - x * t.z,
		x * t.y - y * t.x,
		0)
		
	def reflect(normal: Tuple) = 
		this.subtract(normal.
		              multiply(2).
			      multiply(this.dot(normal)))
}

object Tuple {
	def apply(x: Double = 0, y: Double = 0, z: Double = 0, w: Double = 0) = new Tuple(x,y,z,w)
}
