package raytracer

class Color(var r: Double = 0, var g: Double = 0, var b: Double = 0) {
    
    def equal(c: Color):Boolean =
	Utilities.equal(r, c.r) && Utilities.equal(g, c.g) && Utilities.equal(b, c.b)
     
    def add(c: Color): Color = 
	Color(r + c.r, g + c.g, b + c.b)
    
    def subtract(c: Color): Color =
	Color(r - c.r, g - c.g, b - c.b)
    
    def multiply(k: Double): Color =
	Color(r * k, g * k, b * k)
    
    def multiplyColor(c: Color): Color =
	Color(r * c.r, g * c.g, b * c.b) 
    
    override def toString: String = s"COLOR $r $g $b"
    
}

object Color {
    def apply(r: Double = 0, g: Double = 0, b: Double = 0) = new Color(r,g,b)
    
    def noOverflow(r: Double, g: Double, b: Double): Color = {
        var newR = math.min(1, r)
        var newG = math.min(1, g)
        var newB = math.min(1, b)
        
        Color(newR, newG, newB)
    }
    
    val r = scala.util.Random
    
    def randomColor(): Color = Color(r.nextDouble, r.nextDouble, r.nextDouble)
    
    val BLACK = Color(0, 0, 0)
    val WHITE = Color(1, 1, 1)
    
    val SNOW = Color(0.98, 0.98, 0.98)
    val GHOSTWHITE = Color(0.2, 0.2, 1)
    val ANTIQUEWHITE = Color(0.98, 0.92, 0.84)
    val MISTYROSE = Color(1, 0.89, 0.88)
    val LIGHTGREY = Color(0.83, 0.83, 0.83)
    val DIMGRAY = Color(0.27, 0.27, 0.27)
    val ALMOSTBLACK = Color(0.06, 0.06, 0.06)
    
    val CRIMSON = Color(0.88, 0.08, 0.24)
    val PEACHPUFF = Color(1, 0.85, 0.73)
    val PALETURQUOISE = Color(0.69, 0.93, 0.93)
    val ROYALBLUE = Color(0.25, 0.41, 1)
    val LIGHTPINK = Color(1, 0.71, 0.75)
    val TOMATO = Color(1, 0.38, 0.27)
    val PALEGREEN = Color(0.60, 0.98, 0.60)
    val FORESTGREEN = Color(0.13, 0.55, 0.13)
    val DARKGREEN = Color(0, 0.39, 0)
    val PLUM = Color(0.87, 0.63, 0.87)
    val CYAN = Color(0, 1, 1)
    val LIGHTCYAN = Color(0.88, 1, 1)
    val TEAL = Color(0, 0.5, 0.5)
    val LIGHTBLUE = Color(0.8, 0.7, 0.1)
    val MAGENTA = Color(1, 0, 1)
    val DARKORANGE = Color(1, 0.9, 0)
    val ORANGE = Color(1, 0.4, 0)

}
