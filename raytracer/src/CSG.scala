package raytracer

import collection.mutable.ArrayDeque
import raytracer.Utilities.EPSILON
import raytracer.Operation._
import scala.math.{sqrt, pow}


/*
class CSG(var operation: (Shape, Shape) => Shape, var left: Shape, var right: Shape) extends Shape {
}

object CSG {
    def union(left: Shape, right: Shape) = CSG(left, right)
}
    
*/

case class CSG(var op: Operation, var left: Shape, var right: Shape) extends Shape {
    
    def propagateMaterial(m: Material): Unit = {
        if(left.isInstanceOf[Group])
            left.asInstanceOf[Group].propagateMaterial(m)
        else if(left.isInstanceOf[CSG])
            left.asInstanceOf[CSG].propagateMaterial(m)
        else
            left.material = m
            
        if(right.isInstanceOf[Group])
            right.asInstanceOf[Group].propagateMaterial(m)
        else if(right.isInstanceOf[CSG])
            right.asInstanceOf[CSG].propagateMaterial(m)
        else
            right.material = m
    }
    
    var bbox: BBox = BBox()
    
    val leftBBox = left.parentSpaceBounds()
    val rightBBox = right.parentSpaceBounds()
    
    bbox.add(leftBBox)
    bbox.add(rightBBox)

    left.setParent(this)
    right.setParent(this)
    
    override def includes(shape: Shape): Boolean = {
                if((shape eq this) || this.left.includes(shape) || this.right.includes(shape)) true
                else false
        }
    
    def filterIntersections(xs: ArrayDeque[Intersection]): ArrayDeque[Intersection] = {
        var inl = false
        var inr = false
        
        val result = ArrayDeque[Intersection]()
        
        xs.foreach {
            i => {
                val lhit = this.left.includes(i.shape)
                
                if(intersectionAllowed(op, lhit, inl, inr)) {
                    result += i
                    result.sortInPlace
                }
                
                if(lhit) inl = !inl
                else inr = !inr
            }
        }
        result
    }

    def intersectionAllowed(op: Operation, lhit: Boolean, inl: Boolean, inr: Boolean): Boolean = {
        if(op == UnionOp) 
            (lhit && !inr) || (!lhit && !inl)
        else if(op == IntersectionOp)
            (lhit && inr) || (!lhit && inl)
        else 
            (lhit && !inr) || (!lhit && inl)
    }
    
    def localIntersect(r: Ray): Unit = {
        val rLeft = Ray(r.origin, r.direction)
        val rRight = Ray(r.origin, r.direction)
        
        left.intersect(rLeft)
        right.intersect(rRight)
        
        val xs = rLeft.intersections ++= rRight.intersections
        
        xs.sortInPlace
        
        r.addIntersections(filterIntersections(xs))
    }
    
    def localNormalAt(localPoint: Tuple, i: Option[Intersection] = None): Tuple = ???
    
    override def divide(threshold: Int): Shape = {
        left.divide(threshold)
        right.divide(threshold)
        this
    }

}
