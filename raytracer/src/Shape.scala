package raytracer

import collection.mutable.ArrayDeque
import raytracer.Matrix4._

trait Shape {
        def includes(shape: Shape): Boolean = {
                if(shape eq this) true
                else false
        }
        
        def divide(threshold: Int): Shape = this
        
        var bbox: BBox
        
        def parentSpaceBounds(): BBox = {
                this.bbox.transform(this.transform)
        }
        
        var transform = Matrix4.identityMatrix()
	
	def setTransform(t: Matrix4): Unit =
		this.transform = t
	
	// default material
	var material = Material()
	
        private var _parent: Option[Shape] = None // changed from Group to Shape to support CSG as parent
        
        def parent(): Option[Shape] = _parent
        
        def setParent(s: Shape): Unit = {
                _parent = Some(s)
        }
        
	// SIDE-EFFECT: modifies the `intersections` ArrayDeque in the ray
	// when intersecting the shape with a ray, all shapes need to first convert the ray into object space,
	// transforming it by the inverse of the shape's transformation matrix
	// r.transform(this.transform.inverse.getOrElse(Matrix4.newZeroMatrix4))
	
	def localIntersect(r: Ray): Unit
	
	def intersect(r: Ray): Unit = {
		val r2 = r.transform(this.transform.inverse.getOrElse(Matrix4.newZeroMatrix4))
		this.localIntersect(r2)
		r.addIntersections(r2.intersections)
	}
	   
	def localNormalAt(localPoint: Tuple, i: Option[Intersection] = None): Tuple                    
	
        def worldToObject(point: Tuple): Tuple = {
                val p =  if(parent.isEmpty) point
                         else parent.get.worldToObject(point)
                
                transform.inverse.get.multiply(p)
        }
        
        def normalToWorld(normal: Tuple): Tuple = {
                val n = transpose(transform.inverse.get)
                        .multiply(normal)
                n.w = 0
                
                parent.fold(n.normalize)(_.normalToWorld(n.normalize))
        }
        
        def normalAt(worldPoint:Tuple, i: Option[Intersection] = None): Tuple = {
                val localPoint = worldToObject(worldPoint)
                val localNormal = localNormalAt(localPoint, i)
                normalToWorld(localNormal)
        }
        
	/*def normalAt(worldPoint:Tuple): Tuple = {
		val localPoint = this.transform.inverse
		                                .getOrElse(Matrix4.newZeroMatrix4)
		                                .multiply(worldPoint)
		                                
		val localNormal = this.localNormalAt(localPoint)
		
		val worldNormal = Matrix4.transpose(this.transform
		                                        .inverse.getOrElse(Matrix4.newZeroMatrix4))
		                         .multiply(localNormal)
		worldNormal.w = 0
		worldNormal.normalize
	}
        
        */
}

