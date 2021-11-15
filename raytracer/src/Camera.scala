package raytracer

class Camera(var hSize: Int, var vSize: Int, var fov: Double) {
	var transform = Matrix4.identityMatrix
    
    var halfWidth = 0.0
    var halfHeight = 0.0
    
    val pixelSize = {
        val halfView = math.tan(fov / 2)
        val aspect = hSize.toDouble / vSize
        
        if(aspect >= 1) {
            halfWidth = halfView
            halfHeight = halfView / aspect
        } else {
            halfWidth = halfView * aspect
            halfHeight = halfView
        }
        
        ((halfWidth * 2) / hSize)
    }
    
    // computes the world coordinates at the center of the given pixel,
    // constructs a ray that passes throught that point
    def rayForPixel(px: Int, py: Int): Ray = {
        //offset from canvas' edge to pixel's center
        val xoffset = (px + 0.5) * this.pixelSize
        val yoffset = (py + 0.5) * this.pixelSize
        
        // unstranformed world space pixel coordinates
        // (camera looks towards -z, so +x is to the left)
        val worldX = this.halfWidth - xoffset
        val worldY = this.halfHeight - yoffset
        
        // using the camera matrix, transform the camera point and origin,
        // and then compute the ray's direction vector.
        // (the canvas is at z = -1)
        val pixel = this.transform.inverse.getOrElse(Matrix4.identityMatrix).multiply(Tuple(worldX, worldY, -1, 1))
        val origin = this.transform.inverse.getOrElse(Matrix4.identityMatrix).multiply(Tuple(0, 0, 0, 1))
        val direction = pixel.subtract(origin).normalize
        
        Ray(origin, direction)
    }
    
    def render(world: World): Canvas = {
        val image = Canvas(this.hSize, this.vSize)
        
        // count render time
        val t1 = System.nanoTime
        
        (0 to vSize - 1)
        .map(y => {//println(y) prints the horizontal line being computed on screen
                   (0 to hSize - 1)
                   .map(x => { val ray = this.rayForPixel(x, y)
                               val color = world.colorAt(ray, 5)
                               image.writePixel(x, y, color)
                             }
                        )
                   }   
        )
        
        val duration = (System.nanoTime - t1) / 1e9d
        println(f"$duration%.1f seconds rendering")
        
        image
    }
	
    override def toString: String =
		s"A camera with horizontal $hSize vertical $vSize field of view $fov pixel size $pixelSize"
}

object Camera {
	def apply(hSize: Int, vSize: Int, fov: Double): Camera = new Camera(hSize, vSize, fov)
}
