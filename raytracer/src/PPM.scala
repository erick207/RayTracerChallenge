package raytracer

import java.io._
import raytracer._
import java.util.Calendar

object PPM {
	
	// takes a color component Double [0, 1] (can be out of range) and converts to Int [0, 255]
    private def quantizeComponent(f: Double): String = {
		if(f < 0) "0"
		else if (f >= 1) "255"
		else ((f * 256).toInt).toString
    }
	
	// no line in a PPM file should be longer than 70 characters, don't split numbers!
	private def splitRow(row: Array[String]): String = {
		val sb = new StringBuilder("")
		
		var acc = 0
		for(i <- 0 until row.length) {
			if(acc + row(i).length <= 70) {
				acc = acc + row(i).length
				sb ++= row(i)
				if(!(acc == 70) && (i != row.length - 1)) { 
					sb += ' '
					acc = acc + 1
				}
			}
			else {
				sb ++= ("\n" + row(i) + " ")
				acc = row(i).length + 1
			}
		}
		sb += '\n'
		
		sb.toString
	}
	
	
	def buildRow(row: Array[Color]): String = {
		val quantizedRow = row.flatMap(c => Array[String](quantizeComponent(c.r),
														   quantizeComponent(c.g),
														   quantizeComponent(c.b)))
		
		splitRow(quantizedRow)
	}
	
	private def header(cv: Canvas): String = {
		val first = "P3\n"
		val second = cv.width.toString + " " + cv.height.toString + "\n"
		val third = "255\n"
		
		val header = first + second + third
		
		header
		
	}
	
	// Evaluates to PPM formatted string
	def canvasToPPM(cv: Canvas): String = {
		
		val head = header(cv)
		val body = cv.canvas.map(row => buildRow(row))
		
		val sb = new StringBuilder("")
		sb ++= head
		for(i <- 0 until body.length)
			sb ++= body(i)
			
		sb.toString
	}
	
	def writeFile(s: String): Unit = {
                val fileName = Calendar.getInstance().getTime().toString() + ".ppm"
		val file = new File(fileName)
		val bw = new BufferedWriter(new FileWriter(file))
		bw.write(s)
		bw.close()
	}
}
