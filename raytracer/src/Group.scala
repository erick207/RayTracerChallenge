package raytracer

import collection.mutable.ArrayDeque
import raytracer.Utilities.EPSILON
import scala.math.{sqrt, pow}

class Group() extends Shape {
    
    def propagateMaterial(m: Material): Unit = {
        children.map(child => {
                if(child.isInstanceOf[Group])
                    child.asInstanceOf[Group].propagateMaterial(m)
                else if(child.isInstanceOf[CSG])
                    child.asInstanceOf[Group].propagateMaterial(m)
                else child.material = m
            })
    }
    
    override def includes(shape: Shape): Boolean = {
        if((shape eq this) || children.exists(child => child.includes(shape))) true
        else false
    }
    
    val children: ArrayDeque[Shape] = ArrayDeque[Shape]()
    
    def partitionChildren(): (ArrayDeque[Shape],ArrayDeque[Shape]) = {
        val (lBox, rBox) = bbox.splitBounds()
        
        val (left, right) = (ArrayDeque[Shape](), ArrayDeque[Shape]())
                
        children.foreach {
            child => {
                val inLeft = lBox.contains(child.parentSpaceBounds)
                val inRight = rBox.contains(child.parentSpaceBounds)
                
                if(inLeft) {
                    left += child
                } else if(inRight) {
                    right += child
                }
            }
        }
        
        (left, right)
    }    
    
    def divideProfiler(threshold: Int): Shape = {
        val t1 = System.nanoTime
        
        val divided = divide(threshold)
        
        val duration = (System.nanoTime - t1) / 1e9d
        println(f"$duration%.1f seconds in divide()")
        
        divided
    }
    
    override def divide(threshold: Int): Shape = {

        if(threshold <= children.length) {
            val (l, r) = partitionChildren()
            
            l.map(c => children.filterInPlace(child => !(child eq c)))
            r.map(c => children.filterInPlace(child => !(child eq c)))
            
            
            if(r.nonEmpty) {
                makeSubgroup(r)
            }
            if(l.nonEmpty) {
                makeSubgroup(l)
            }
        }

        children.mapInPlace(_.divide(threshold))
        this
    }
    
    def makeSubgroup(shapes: ArrayDeque[Shape]): ArrayDeque[Shape] = {
        val subg = Group()
        shapes.map(subg.add(_))
        
        this.add(subg)
    }
    
    def add(child: Shape): ArrayDeque[Shape] = {
        children += child
        // sets parent attribute in the children
        child.setParent(this)
        bbox.add(child.parentSpaceBounds) // update BBox as new elem is added
        children
    }
    
    var bbox: BBox = BBox()
    
    def localIntersect(r: Ray): Unit =
        if(bbox.intersect(r)) children.map(_.intersect(r))
        else ()
    
    def localNormalAt(localPoint: Tuple, i: Option[Intersection] = None): Tuple = ???
    
    override def toString: String =
		" a group of " + children.length.toString + " children"
}

object Group {
    def apply(): Group = new Group()
}
