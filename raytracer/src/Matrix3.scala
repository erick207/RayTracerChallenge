package raytracer

class Matrix3(var m00: Double, var m01: Double, var m02: Double,
              var m10: Double, var m11: Double, var m12: Double,
              var m20: Double, var m21: Double, var m22: Double) {
				  
	val m = Array.ofDim[Double](3,3)
	// 1st row
	m(0)(0) = m00
	m(0)(1) = m01
	m(0)(2) = m02
	// 2nd row
	m(1)(0) = m10
	m(1)(1) = m11
	m(1)(2) = m12
	// 3rd row
	m(2)(0) = m20
	m(2)(1) = m21
	m(2)(2) = m22

	def submatrix(x: Int, y: Int): Matrix2 = {
		val i = (0 to 2).filterNot(_ == x)
		val j = (0 to 2).filterNot(_ == y)
		
		val sub = Matrix2.newZeroMatrix2
		
		// i.map(a => j.map(b => sub.m(a)(b) = m(a)(b)))
		(0 to 1).map(a => (0 to 1).map(b => sub.m(a)(b) = m(i(a))(j(b))))
		sub
	}
	
	def load(a: Array[Array[Double]]): Unit = {
		// 1st row
		m(0)(0) = a(0)(0)
		m(0)(1) = a(0)(1)
		m(0)(2) = a(0)(2)
		// 2nd row
		m(1)(0) = a(1)(0)
		m(1)(1) = a(1)(1)
		m(1)(2) = a(1)(2)
		// 3rd row
		m(2)(0) = a(2)(0)
		m(2)(1) = a(2)(1)
		m(2)(2) = a(2)(2)
	}
	
	def minor(i: Int, j: Int): Double = {
		submatrix(i, j).determinant
	}
	
	def cofactor(i: Int, j: Int): Double = {
		val co = minor(i,j)
		
		if(Matrix3.cofactorMask(i)(j)) co
		else -co
	}
	
	def determinant(): Double = {
		val co00 = minor(0,0)
		val co01 = cofactor(0,1)
		val co02 = cofactor(0,2)
		
		m(0)(0) * co00 +
		m(0)(1) * co01 +
		m(0)(2) * co02 
	}

	def equal(a: Matrix3): Boolean =
		Utilities.equal(m(0)(0), a.m(0)(0)) && Utilities.equal(m(0)(1), a.m(0)(1)) && Utilities.equal(m(0)(2), a.m(0)(2)) &&
		Utilities.equal(m(1)(0), a.m(1)(0)) && Utilities.equal(m(1)(1), a.m(1)(1)) && Utilities.equal(m(1)(2), a.m(1)(2)) &&
		Utilities.equal(m(2)(0), a.m(2)(0)) && Utilities.equal(m(2)(1), a.m(2)(1)) && Utilities.equal(m(2)(2), a.m(2)(2))

	override def toString(): String = {
		m.map(_.mkString(" ")).mkString("\n")
	}
}


object Matrix3 {
	def apply(m00: Double, m01: Double, m02: Double,
	          m10: Double, m11: Double, m12: Double,
	          m20: Double, m21: Double, m22: Double): Matrix3 =
	          
	    new Matrix3(m00, m01, m02,
	                m10, m11, m12,
	                m20, m21, m22)
	
	def newZeroMatrix3(): Matrix3 = {
		Matrix3(0,0,0,
		        0,0,0,
		        0,0,0)
		        
	}
	
	val cofactorMask: Array[Array[Boolean]] =
		Array(Array(true, false, true),
		      Array(false, true, false),
		      Array(true, false, true))
}
