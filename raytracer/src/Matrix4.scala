package raytracer

class Matrix4(var m00: Double, var m01: Double, var m02: Double, var m03: Double,
              var m10: Double, var m11: Double, var m12: Double, var m13: Double,
              var m20: Double, var m21: Double, var m22: Double, var m23: Double,
              var m30: Double, var m31: Double, var m32: Double, var m33: Double) {
				  
	val m = Array.ofDim[Double](4,4)
	// 1st row
	m(0)(0) = m00
	m(0)(1) = m01
	m(0)(2) = m02
	m(0)(3) = m03
	// 2nd row
	m(1)(0) = m10
	m(1)(1) = m11
	m(1)(2) = m12
	m(1)(3) = m13
	// 3rd row
	m(2)(0) = m20
	m(2)(1) = m21
	m(2)(2) = m22
	m(2)(3) = m23
	// 4th row
	m(3)(0) = m30
	m(3)(1) = m31
	m(3)(2) = m32
	m(3)(3) = m33
	
	def equal(a: Matrix4): Boolean =
		Utilities.equal(m(0)(0), a.m(0)(0)) && Utilities.equal(m(0)(1), a.m(0)(1)) && Utilities.equal(m(0)(2), a.m(0)(2)) && Utilities.equal(m(0)(3), a.m(0)(3)) &&
		Utilities.equal(m(1)(0), a.m(1)(0)) && Utilities.equal(m(1)(1), a.m(1)(1)) && Utilities.equal(m(1)(2), a.m(1)(2)) && Utilities.equal(m(1)(3), a.m(1)(3)) &&
		Utilities.equal(m(2)(0), a.m(2)(0)) && Utilities.equal(m(2)(1), a.m(2)(1)) && Utilities.equal(m(2)(2), a.m(2)(2)) && Utilities.equal(m(2)(3), a.m(2)(3)) &&
		Utilities.equal(m(3)(0), a.m(3)(0)) && Utilities.equal(m(3)(1), a.m(3)(1)) && Utilities.equal(m(3)(2), a.m(3)(2)) && Utilities.equal(m(3)(3), a.m(3)(3))
	
	def multiply(b: Matrix4): Matrix4 = {
		
		val result = Matrix4.newZeroMatrix4()
		
		for(i <- 0 to 3; j <- 0 to 3) {
			result.m(i)(j) = m(i)(0) * b.m(0)(j) +
			                 m(i)(1) * b.m(1)(j) +
			                 m(i)(2) * b.m(2)(j) +
			                 m(i)(3) * b.m(3)(j)
		}
		result
	}
	def multiply(b: Tuple): Tuple = {
		
		val result = Tuple(0, 0, 0, 0)
		
		result.x = m(0)(0) * b.x +
		           m(0)(1) * b.y +
		           m(0)(2) * b.z +
	               m(0)(3) * b.w
	               
	    result.y = m(1)(0) * b.x +
		           m(1)(1) * b.y +
		           m(1)(2) * b.z +
	               m(1)(3) * b.w
	    
	    result.z = m(2)(0) * b.x +
		           m(2)(1) * b.y +
		           m(2)(2) * b.z +
	               m(2)(3) * b.w
	    
	    result.w = m(3)(0) * b.x +
		           m(3)(1) * b.y +
		           m(3)(2) * b.z +
	               m(3)(3) * b.w
		
		result
	}
	
	def submatrix(x: Int, y: Int): Matrix3 = {
		val i = (0 to 3).filterNot(_ == x)
		val j = (0 to 3).filterNot(_ == y)
		
		val sub = Matrix3.newZeroMatrix3
		
		// i.map(a => j.map(b => sub.m(a)(b) = m(a)(b)))
		(0 to 2).map(a => (0 to 2).map(b => sub.m(a)(b) = m(i(a))(j(b))))
		sub
	}
	
	def load(a: Array[Array[Double]]): Unit = {
		// 1st row
		m(0)(0) = a(0)(0)
		m(0)(1) = a(0)(1)
		m(0)(2) = a(0)(2)
		m(0)(3) = a(0)(3)
		// 2nd row
		m(1)(0) = a(1)(0)
		m(1)(1) = a(1)(1)
		m(1)(2) = a(1)(2)
		m(1)(3) = a(1)(3)
		// 3rd row
		m(2)(0) = a(2)(0)
		m(2)(1) = a(2)(1)
		m(2)(2) = a(2)(2)
		m(2)(3) = a(2)(3)
		// 4th row
		m(3)(0) = a(3)(0)
		m(3)(1) = a(3)(1)
		m(3)(2) = a(3)(2)
		m(3)(3) = a(3)(3)
	}
	
	def minor(i: Int, j: Int): Double = {
		submatrix(i, j).determinant
	}
	
	def cofactor(i: Int, j: Int): Double = {
		val co = minor(i,j)
		
		if(Matrix4.cofactorMask(i)(j)) co
		else -co
	}
	
	lazy val determinant: Double = {
		val co00 = minor(0,0)
		val co01 = cofactor(0,1)
		val co02 = cofactor(0,2)
		val co03 = cofactor(0,3)
		
		m(0)(0) * co00 +
		m(0)(1) * co01 + 
		m(0)(2) * co02 + 
		m(0)(3) * co03
	}
	
        lazy val inverse: Option[Matrix4] = {
		val det = determinant
		if(Utilities.equal(det, 0)) None
		else {
			val m2 = Matrix4.newZeroMatrix4
			(0 to 3).map(i => (0 to 3).map(j => m2.m(j)(i) = cofactor(i, j) / det))
			Some(m2)
		} 
	}
        
	
        
        
	
	override def toString: String =
		"MATRIX4: \n" + m.map(_.mkString(", ")).mkString("\n")
}


object Matrix4 {
	def apply(m00: Double, m01: Double, m02: Double, m03: Double,
	          m10: Double, m11: Double, m12: Double, m13: Double,
	          m20: Double, m21: Double, m22: Double, m23: Double,
	          m30: Double, m31: Double, m32: Double, m33: Double): Matrix4 =
	          
	    new Matrix4(m00, m01, m02, m03,
	                m10, m11, m12, m13,
	                m20, m21, m22, m23,
	                m30, m31, m32, m33)
	
	val cofactorMask: Array[Array[Boolean]] =
		
		Array(Array(true, false, true, false),
		      Array(false, true, false, true),
		      Array(true, false, true, false),
		      Array(false, true, false, true)
		      )
	
	def newZeroMatrix4(): Matrix4 = {
		Matrix4(0,0,0,0,
		        0,0,0,0,
		        0,0,0,0,
		        0,0,0,0)
	}
	
	def identityMatrix(): Matrix4 = {
		Matrix4(1,0,0,0,
		        0,1,0,0,
		        0,0,1,0,
		        0,0,0,1)
	}
	
	def transpose(a: Matrix4): Matrix4 = {
		val t = newZeroMatrix4
		t.load(a.m.transpose)
		t
	}
	
	def translation(x: Double, y: Double, z: Double): Matrix4 = {
		val t = identityMatrix
		t.m(0)(3) = x
		t.m(1)(3) = y
		t.m(2)(3) = z
		t
	}
	
	def scaling(x: Double, y: Double, z: Double): Matrix4 = {
		val t = identityMatrix
		t.m(0)(0) = x
		t.m(1)(1) = y
		t.m(2)(2) = z
		t
	}
	
	def rotation_x(r: Double): Matrix4 = {
		val t = identityMatrix
		t.m(1)(1) = math.cos(r).toDouble
		t.m(1)(2) = - math.sin(r).toDouble
		t.m(2)(1) = math.sin(r).toDouble
		t.m(2)(2) = math.cos(r).toDouble
		t
	}
	
	def rotation_y(r: Double): Matrix4 = {
		val t = identityMatrix
		t.m(0)(0) = math.cos(r).toDouble
		t.m(0)(2) = math.sin(r).toDouble
		t.m(2)(0) = - math.sin(r).toDouble
		t.m(2)(2) = math.cos(r).toDouble
		t
	}
	
	def rotation_z(r: Double): Matrix4 = {
		val t = identityMatrix
		t.m(0)(0) = math.cos(r).toDouble
		t.m(0)(1) = - math.sin(r).toDouble
		t.m(1)(0) = math.sin(r).toDouble
		t.m(1)(1) = math.cos(r).toDouble
		t
	}
	
	def shearing(x_y: Double, x_z: Double, y_x: Double, y_z: Double, z_x: Double, z_y: Double): Matrix4 = {
		val t = identityMatrix
		t.m(0)(1) = x_y
		t.m(0)(2) = x_z
		t.m(1)(0) = y_x
		t.m(1)(2) = y_z
		t.m(2)(0) = z_x
		t.m(2)(1) = z_y
		t
	}
	
	// from: eye (point)
	// to: where the eye is looking at (point)
	// up: vector showing which direction is `up` (vector)
	def viewTransform(from: Tuple, to: Tuple, up: Tuple): Matrix4 = {
		val forward = to
		              .subtract(from)
		              .normalize
		val left = forward
		           .cross(up.normalize)
		val trueUp = left
		             .cross(forward)
		             
		// with those 3 values we can costruct the ´orientation´ transformation matrix
		val orientation = Matrix4(left.x,      left.y,     left.z,    0,
		                          trueUp.x,    trueUp.y,   trueUp.z,  0,
		                          -forward.x, -forward.y, -forward.z, 0,
		                          0,          0,         0,        1)
		
		// append a translation to that transformation to move the scene into place before orienting it
		val translation =  Matrix4.translation(-from.x, -from.y, -from.z)
		
		orientation.multiply(translation)
	}
}
