package raytracer


// What the book generally calls ´object´ I call shape instead due to object being a keyword in Scala
class Computations(var t: Double,
                  var shape: Shape,
                  var point: Tuple,
                  var eyev: Tuple,
                  var normalv: Tuple,
                  var inside: Boolean,
                  var overPoint: Tuple,
                  var underPoint: Tuple,
                  var reflectv: Tuple,
                  var n1: Double,
                  var n2: Double) {
    
}

object Computations {
    def apply(t: Double,
              shape: Shape,
              point: Tuple,
              eyev: Tuple,
              normalv: Tuple,
              inside: Boolean,
              overPoint: Tuple,
              underPoint: Tuple,
              reflectv: Tuple,
              n1: Double,
              n2: Double) = new Computations(t, shape, point, eyev, normalv, inside, overPoint, underPoint, reflectv, n1, n2)
}
