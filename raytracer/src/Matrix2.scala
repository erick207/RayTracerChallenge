package raytracer

class Matrix2(var m00: Double, var m01: Double,
              var m10: Double, var m11: Double) {
				  
	val m = Array.ofDim[Double](2,2)
	// 1st row
	m(0)(0) = m00
	m(0)(1) = m01
	// 2nd row
	m(1)(0) = m10
	m(1)(1) = m11
	
	def determinant(): Double = {
		m(0)(0) * m(1)(1) - m(0)(1) * m(1)(0)
	}
	
	def load(a: Array[Array[Double]]): Unit = {
	// 1st row
	m(0)(0) = a(0)(0)
	m(0)(1) = a(0)(1)
	// 2nd row
	m(1)(0) = a(1)(0)
	m(1)(1) = a(1)(1)

	}
	
	def equal(a: Matrix2): Boolean =
		Utilities.equal(m(0)(0), a.m(0)(0)) && Utilities.equal(m(0)(1), a.m(0)(1)) &&
		Utilities.equal(m(1)(0), a.m(1)(0)) && Utilities.equal(m(1)(1), a.m(1)(1))
	
	override def toString(): String = {
		m.map(_.mkString(" ")).mkString("\n")
	}
}

object Matrix2 {
	def apply(m00: Double, m01: Double,
	          m10: Double, m11: Double): Matrix2 = new Matrix2(m00, m01,
	                                                         m10, m11)
	 
	def newZeroMatrix2(): Matrix2 = {
		Matrix2(0,0,
		        0,0)
	}
	                                                         
}
