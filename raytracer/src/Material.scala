package raytracer

class Material(var color: Color = Color(1, 1, 1),
               var ambient: Double = 0.1,
               var diffuse: Double = 0.9,
               var specular: Double = 0.9,
               var shininess: Double = 200.0,
	       var reflective: Double = 0.0,
	       var transparency: Double = 0.0,
	       var refractiveIndex: Double = 1.0) {
	
	private var _pattern: Option[Pattern] = None
	
	def pattern(): Option[Pattern] = _pattern
	
	def setPattern(p: Pattern): Unit = {
	  _pattern = Some(p)
	}
	
	def equal(m: Material):Boolean =
	        color.equal(m.color) &&
		Utilities.equal(ambient, m.ambient) &&
		Utilities.equal(diffuse, m.diffuse) &&
		Utilities.equal(specular, m.specular) &&
		Utilities.equal(shininess, m.shininess) &&
		Utilities.equal(reflective, m.reflective)
	
	override def toString: String = 
	  s"\nMATERIAL $color $ambient $diffuse $specular $shininess Pattern: $pattern"
}

object Material {
	def apply(color: Color = Color(1, 1, 1),
	      ambient: Double = 0.1,
              diffuse: Double = 0.9,
              specular: Double = 0.9,
              shininess: Double = 200.0) = new Material(color, ambient, diffuse, specular, shininess)
              
        def colorMaterial(color: Color): Material = {
                val m = Material()
                m.color = color
                m
        }
        
        def colorMaterialWithReflection(color: Color, reflec: Double): Material = {
                val m = colorMaterial(color)
                m.reflective = reflec
                m
        }
}
