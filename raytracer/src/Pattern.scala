package raytracer

trait Pattern {
	var transform = Matrix4.identityMatrix()
	
	def setTransform(t: Matrix4): Unit =
		this.transform = t
		
	def colorAtPatternPoint(p: Tuple): Color
	
	def colorAtObjectPoint(obj: Shape, worldPoint: Tuple): Color = {
		//val objPoint = obj.transform.inverse.getOrElse(Matrix4.newZeroMatrix4).multiply(worldPoint)
                val objPoint = obj.worldToObject(worldPoint)
		val patternPoint = this.transform.inverse.getOrElse(Matrix4.newZeroMatrix4).multiply(objPoint)
		colorAtPatternPoint(patternPoint)
	}
		
	override def toString: String = " a pattern "
}

case class CheckersPattern(var a: Color, var b: Color) extends Pattern {
	def colorAtPatternPoint(p: Tuple): Color = {
		val cond = (math.floor(p.x) + math.floor(p.y) + math.floor(p.z)) % 2 == 0
		
		if(cond) a
		else b
	}
		
	override def toString: String = " a checkers pattern "

}

case class GradientPattern(var a: Color, var b: Color) extends Pattern {
	def colorAtPatternPoint(p: Tuple): Color = {
		val distance = b.subtract(a)
		val fraction = p.x - math.floor(p.x)
		
		a.add(distance.multiply(fraction.toDouble))
	}
	
	override def toString: String = " a gradient pattern "

}

case class RingPattern(var a: Color, var b: Color) extends Pattern {
	def colorAtPatternPoint(p: Tuple): Color = {
		val cond = math.floor(math.sqrt((p.x * p.x) + (p.z * p.z))) % 2 == 0
		
		if(cond) a
		else b
	}
	
	override def toString: String = " a ring pattern "

}

case class StripePattern(var a: Color, var b: Color) extends Pattern {
	def colorAtPatternPoint(p: Tuple): Color = {
		if(math.floor(p.x) % 2 == 0) a
		else b
	}
	
	override def toString: String = " a stripe pattern "

}
