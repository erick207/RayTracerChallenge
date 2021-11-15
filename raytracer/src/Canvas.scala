package raytracer

class Canvas(val width: Int, val height: Int) {
	var canvas = initCanvas(width, height)
	
	private def initCanvas(width: Int, height: Int): Array[Array[Color]] = {
		// y == height; x == width; internal structure of Scala's 2D array
		val cv = Array.ofDim[Color](height, width)
		for (i <- 0 until width; j <- 0 until height) 
			cv(j)(i) = Color(0, 0, 0) 
		cv
	}
	
	def writePixel(x: Int, y: Int, c: Color): Unit = {
		if(x >= width || y >= height) {
			println(s"WARNING: writePixel out of range ( $width , $height ), x = $x , y = $y")
		}
		else {
			//println(s"canvas(y)(x) where x= $x  y= $y ")
			canvas(y)(x) = c
		}
	}
	
	def pixelAt(x: Int, y: Int): Color = {
		canvas(y)(x)
	}
	
	override def toString: String =
		s"A canvas of $width x $height"
}


object Canvas {
	//var canvas:Array[Array[Color]] = null
	
	def apply(width: Int, height: Int): Canvas = new Canvas(width, height)
}
