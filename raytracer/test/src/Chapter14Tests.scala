package Util

import utest._
import raytracer._
import collection.mutable.ArrayDeque
import raytracer.Matrix4._
import scala.math._
import scala.Double.{PositiveInfinity, NegativeInfinity}
import raytracer.Utilities.equal

object Chapter14 extends TestSuite{
        
  def tests = Tests{

/*********************************************************************/
/*                           CHAPTER 14                              */
/*                             GROUPS                                */
/*                                                                   */
/*********************************************************************/
    /*
    test("Creating a new group"){
        val g = Group()
        assert(g.transform.equal(identityMatrix) && g.children.isEmpty)
        g.transform.toString + "\n" + g.children.length.toString
    }
    test("A shape has a parent attribute"){
        val s = Sphere()
        assert(s.parent.isEmpty)
        s.parent
    }
    test("Adding a child to a group"){
        val g = Group()
        val s = Sphere()
        g.add(s)
        assert(g.children.nonEmpty &&
               (g.children(0) eq s) &&
               (s.parent.get eq g))
        g.children.toString + "\n" + s.parent.toString
    }
    test("Intersecting a ray with an empty group"){
        val g = Group()
        val r = Ray(Tuple(0, 0, 0, 1), Tuple(0, 0, 1, 0))
        g.localIntersect(r)
        
        assert(r.intersections.isEmpty)
        r.intersections.length
    }  
    test("Intersecting a ray with a nonempty group"){
        val g = Group()
        val s1 = Sphere()
        val s2 = Sphere()
        val s3 = Sphere()
        
        s2.setTransform(translation(0, 0, -3))
        s3.setTransform(translation(5, 0, 0))
        
        val r = Ray(Tuple(0, 0, -5, 1), Tuple(0, 0, 1, 0))
        
        g.children ++= ArrayDeque[Shape](s1, s2, s3)
        
        g.localIntersect(r)
        
        assert(r.intersections.length == 4 &&
               (r.intersections(0).shape eq s2) &&
               (r.intersections(1).shape eq s2) &&
               (r.intersections(2).shape eq s1) &&
               (r.intersections(3).shape eq s1))
        r.intersections.length
    }
    test("Intersecting a transformed group"){
        val g = Group()
        g.setTransform(scaling(2, 2, 2))
        
        val s = Sphere()
        s.setTransform(translation(5, 0, 0))
        
        g.children += s
        
        val r = Ray(Tuple(10, 0, -10, 1), Tuple(0, 0, 1, 0))
        
        g.intersect(r)
        
        assert(r.intersections.length == 2)
        r.intersections.length
    }
    test("Converting a point from world to object space"){
        val g1 = Group()
        g1.setTransform(rotation_y(Pi/2))
        val g2 = Group()
        g2.setTransform(scaling(2, 2, 2))
        
        g1.add(g2)
        
        val s = Sphere()
        s.setTransform(translation(5, 0, 0))
        g2.add(s)
        
        val p = s.worldToObject(Tuple(-2, 0, -10, 1))
        
        assert(p.equal(Tuple(0, 0, -1, 1)))
        p
    }
    /*test("Finding the normal on a child object"){
        val g1 = Group()
        g1.setTransform(rotation_y(Pi/2))
        val g2 = Group()
        g2.setTransform(scaling(1, 2, 3))
        
        g1.add(g2)
        
        val s = Sphere()
        s.setTransform(translation(5, 0, 0))
        g2.add(s)
        
        val n = s.normalAt(Tuple(1.7321, 1.1547, -5.5774, 1))
        
        assert(n.equal(Tuple(0.2857, 0.4286, -0.8571, 0)))
        n
    }*/
    
/*********************************************************************/
/*                           CHAPTER 14                              */
/*                          BOUNDING BOX                             */
/*                         (BONUS CHAPTER)                           */
/*********************************************************************/
    test("Creating an empty bounding box"){
        val box = BBox()
        
        assert(// min
               box.min.x.isPosInfinity &&
               box.min.y.isPosInfinity &&
               box.min.z.isPosInfinity &&
               box.min.w == 1.0 &&
               // max
               box.max.x.isNegInfinity &&
               box.max.y.isNegInfinity &&
               box.max.z.isNegInfinity &&
               box.max.w == 1.0)
        box
    }
    test("Creating a bounding box with volume"){
        val box = BBox(Tuple(-1, -2, -3, 1), Tuple(3, 2, 1, 1))
        assert(box.min.equal(Tuple(-1, -2, -3, 1)) &&
               box.max.equal(Tuple(3, 2, 1, 1)))
        box
    }
    test("Adding points to an empty bounding box"){
        val box = BBox()
        val p1 = Tuple(-5, 2, 0, 1)
        val p2 = Tuple(7, 0, -3, 1)
        
        box.add(p1)
        box.add(p2)
        
        assert(box.min.equal(Tuple(-5, 0, -3, 1)) &&
               box.max.equal(Tuple(7, 2, 0, 1)))
        box
    }
    test("A sphere has a bounding box"){
        val s = Sphere()
        val box = s.bbox
        
        assert(box.min.equal(Tuple(-1, -1, -1, 1)) &&
               box.max.equal(Tuple(1, 1, 1, 1)))
        s.bbox.toString
    }
    test("A plane has a bounding box"){
        val s = Plane()
        val box = s.bbox
        
        assert(// min
               box.min.x.isPosInfinity &&
               box.min.y == 0.0 &&
               box.min.z.isPosInfinity &&
               box.min.w == 1.0 &&
               // max
               box.max.x.isNegInfinity &&
               box.max.y == 0.0 &&
               box.max.z.isNegInfinity &&
               box.max.w == 1.0)
        box.toString
    }
    test("A cube has a bounding box"){
        val s = Cube()
        val box = s.bbox
        
        assert(box.min.equal(Tuple(-1, -1, -1, 1)) &&
               box.max.equal(Tuple(1, 1, 1, 1)))
        s.bbox.toString
    }
    test("An unbounded cylinder has a bounding box"){
        val s = Cylinder()
        val box = s.bbox
        
        assert(// min
               box.min.x == -1 &&
               box.min.y.isNegInfinity &&
               box.min.z == -1 &&
               box.min.w == 1.0 &&
               // max
               box.max.x == 1 &&
               box.max.y.isPosInfinity &&
               box.max.z == 1 &&
               box.max.w == 1.0)
        box.toString
    }
    test("A bounded cylinder has a bounding box"){
        val s = Cylinder()
        s.setMinimum(-5)
        s.setMaximum(3)
        val box = s.bbox
        
        assert(// min
               box.min.x == -1 &&
               box.min.y == -5 &&
               box.min.z == -1 &&
               box.min.w == 1.0 &&
               // max
               box.max.x == 1 &&
               box.max.y == 3 &&
               box.max.z == 1 &&
               box.max.w == 1.0)
        box.toString
    }
    test("An unbounded cone has a bounding box"){
        val s = Cone()
        val box = s.bbox
        
        assert(// min
               box.min.x.isNegInfinity &&
               box.min.y.isNegInfinity &&
               box.min.z.isNegInfinity &&
               box.min.w == 1.0 &&
               // max
               box.max.x.isPosInfinity &&
               box.max.y.isPosInfinity &&
               box.max.z.isPosInfinity &&
               box.max.w == 1.0)
        box.toString
    }
    test("A bounded cone has a bounding box"){
        val s = Cone()
        s.setMinimum(-5)
        s.setMaximum(3)
        val box = s.bbox
        
        assert(// min
               box.min.x == -5 &&
               box.min.y == -5 &&
               box.min.z == -5 &&
               box.min.w == 1.0 &&
               // max
               box.max.x == 5 &&
               box.max.y == 3 &&
               box.max.z == 5 &&
               box.max.w == 1.0)
        box.toString
    }
    test("Adding one bounding box to another"){
        val box1 = BBox(Tuple(-5, -2, 0, 1),
                        Tuple(7, 4, 4, 1))
        val box2 = BBox(Tuple(8, -7, -2, 1),
                        Tuple(14, 2, 8, 1))
        
        box1.add(box2)
        
        assert(// min
               box1.min.x == -5 &&
               box1.min.y == -7 &&
               box1.min.z == -2 &&
               box1.min.w == 1.0 &&
               // max
               box1.max.x == 14 &&
               box1.max.y ==  4 &&
               box1.max.z ==  8 &&
               box1.max.w == 1.0)
        box1.toString
    }
    test("Checking to see if a box contains a given point"){
        val box = BBox(Tuple(5, -2, 0, 1),
                       Tuple(11, 4, 7, 1))
        
        val pointArr = Array(Tuple(5, -2, 0, 1),
                         Tuple(11, 4, 7, 1),
                         Tuple(8, 1, 3, 1),
                         Tuple(3, 0, 3, 1),
                         Tuple(8, -4, 3, 1),
                         Tuple(8, 1, -1, 1),
                         Tuple(13, 1, 3, 1),
                         Tuple(8, 5, 3, 1),
                         Tuple(8, 1, 8, 1))
        
        val boolArr = Array(true,
                            true,
                            true,
                            false,
                            false,
                            false,
                            false,
                            false,
                            false)
        
        
        assert(!(0 to 8).exists {
                        i => box.contains(pointArr(i)) != boolArr(i)
                })
        box
    }
    test("Checking to see if a box contains a given box"){
        val box = BBox(Tuple(5, -2, 0, 1),
                       Tuple(11, 4, 7, 1))
        //////////////////////////////////
        val box1 = BBox(Tuple(5, -2, 0, 1),
                       Tuple(11, 4, 7, 1))

        val box2 = BBox(Tuple(6, -1, 1, 1),
                       Tuple(10, 3, 6, 1))
                       
        val box3 = BBox(Tuple(4, -3, -1, 1),
                       Tuple(10, 3, 6, 1))
                       
        val box4 = BBox(Tuple(6, -1, 1, 1),
                       Tuple(12, 5, 8, 1))
        
        
        assert(box.contains(box1) &&
               box.contains(box2) &&
               !box.contains(box3) &&
               !box.contains(box4))
        box
    }
    test("Transforming a bounding box"){
        val box1 = BBox(Tuple(-1, -1, -1, 1),
                        Tuple(1, 1, 1, 1))
        
        val m = rotation_x(Pi/4).multiply(
                rotation_y(Pi/4))
                
        val box2 = box1.transform(m)
        assert(// min
               equal(box2.min.x, -1.41421) &&
               equal(box2.min.y, -1.7071) &&
               equal(box2.min.z, -1.7071) &&
               equal(box2.min.w, 1.0) &&
               // max
               equal(box2.max.x, 1.41421) &&
               equal(box2.max.y, 1.7071) &&
               equal(box2.max.z, 1.7071) &&
               equal(box2.max.w, 1.0))
        box2
    }
    test("Querying a shape's bounding box in its parent's space"){
        val s = Sphere()
        s.setTransform(translation(1, -3, 5).multiply(
                       scaling(0.5, 2, 4)))
        
        val box = s.parentSpaceBounds
        
        assert(// min
               equal(box.min.x, 0.5) &&
               equal(box.min.y, -5) &&
               equal(box.min.z, 1) &&
               equal(box.min.w, 1.0) &&
               // max
               equal(box.max.x, 1.5) &&
               equal(box.max.y, -1) &&
               equal(box.max.z, 9) &&
               equal(box.max.w, 1.0))
        box
    }
    test("A group has a bounding box that contains its children"){
        val s = Sphere()
        s.setTransform(translation(2, 5, -3).multiply(
                       scaling(2, 2, 2)))
        val c = Cylinder()
        c.setMinimum(-2)
        c.setMaximum(2)
        c.setTransform(translation(-4, -1, 4).multiply(
                       scaling(0.5, 1, 0.5)))
                       
        var g = Group()
        g.add(s)
        g.add(c)
        
        val box = g.bbox
                
        assert(// min
               equal(box.min.x, -4.5) &&
               equal(box.min.y, -3) &&
               equal(box.min.z, -5) &&
               equal(box.min.w, 1.0) &&
               // max
               equal(box.max.x, 4) &&
               equal(box.max.y, 7) &&
               equal(box.max.z, 4.5) &&
               equal(box.max.w, 1.0))
        box
    }
    test("Intersecting a ray with a bounding box at the origin"){
        val box = BBox(Tuple(-1, -1, -1, 1),
                       Tuple(1, 1, 1, 1))
        
        val pointArr = Array(Tuple(5, 0.5, 0, 1),
                             Tuple(-5, 0.5, 0, 1),
                             Tuple(0.5, 5, 0, 1),
                             Tuple(0.5, -5, 0, 1),
                             Tuple(0.5, 0, 5, 1),
                             Tuple(0.5, 0, -5, 1),
                             Tuple(0, 0.5, 0, 1),
                             Tuple(-2, 0, 0, 1),
                             Tuple(0, -2, 0, 1),
                             Tuple(0, 0, -2, 1),
                             Tuple(2, 0, 2, 1),
                             Tuple(0, 2, 2, 1),
                             Tuple(2, 2, 0, 1))
        
        val vectorArr = Array(Tuple(-1, 0, 0, 0),
                              Tuple(1, 0, 0, 0),
                              Tuple(0, -1, 0, 0),
                              Tuple(0, 1, 0, 0),
                              Tuple(0, 0, -1, 0),
                              Tuple(0, 0, 1, 0),
                              Tuple(0, 0, 1, 0),
                              Tuple(2, 4, 6, 0),
                              Tuple(6, 2, 4, 0),
                              Tuple(4, 6, 2, 0),
                              Tuple(0, 0, -1, 0),
                              Tuple(0, -1, 0, 0),
                              Tuple(-1, 0, 0, 0))
                             
        val boolArr = Array(true,
                            true,
                            true,
                            true,
                            true,
                            true,
                            true,
                            false,
                            false,
                            false,
                            false,
                            false,
                            false)
        
        
        assert(!(0 to 12).exists {
                        i => box.intersect(Ray(pointArr(i), vectorArr(i))) != boolArr(i)
                })
        box
    }
    test("Intersecting a ray with a non-cubic bounding box"){
        val box = BBox(Tuple(5, -2, 0, 1),
                       Tuple(11, 4, 7, 1))
        
        val pointArr = Array(Tuple(15, 1, 2, 1),
                             Tuple(-5, -1, 4, 1),
                             Tuple(7, 6, 5, 1),
                             Tuple(9, -5, 6, 1),
                             Tuple(8, 2, 12, 1),
                             Tuple(6, 0, -5, 1),
                             Tuple(8, 1, 3.5, 1),
                             Tuple(9, -1, -8, 1),
                             Tuple(8, 3, -4, 1),
                             Tuple(9, -1, -2, 1),
                             Tuple(4, 0, 9, 1),
                             Tuple(8, 6, -1, 1),
                             Tuple(12, 5, 4, 1))
        
        val vectorArr = Array(Tuple(-1, 0, 0, 0),
                              Tuple(1, 0, 0, 0),
                              Tuple(0, -1, 0, 0),
                              Tuple(0, 1, 0, 0),
                              Tuple(0, 0, -1, 0),
                              Tuple(0, 0, 1, 0),
                              Tuple(0, 0, 1, 0),
                              Tuple(2, 4, 6, 0),
                              Tuple(6, 2, 4, 0),
                              Tuple(4, 6, 2, 0),
                              Tuple(0, 0, -1, 0),
                              Tuple(0, -1, 0, 0),
                              Tuple(-1, 0, 0, 0))
                             
        val boolArr = Array(true,
                            true,
                            true,
                            true,
                            true,
                            true,
                            true,
                            false,
                            false,
                            false,
                            false,
                            false,
                            false)
        
        
        assert(!(0 to 12).exists {
                        i => box.intersect(Ray(pointArr(i), vectorArr(i))) != boolArr(i)
                })
        box
    }
/*********************************************************************/
/*                           CHAPTER 14                              */
/*                BOUNDING VOLUME HIERARCHIES (BVH)                  */
/*                         (BONUS CHAPTER)                           */
/*********************************************************************/

    test("Splitting a perfect cube"){
        val box = BBox(Tuple(-1, -4, -5, 1),
                       Tuple(9, 6, 5, 1))
        
        val (left, right) = box.splitBounds()
        assert(left.min.equal(Tuple(-1, -4, -5, 1)) &&
               left.max.equal(Tuple(4, 6, 5, 1)) &&
               right.min.equal(Tuple(4, -4, -5, 1)) &&
               right.max.equal(Tuple(9, 6, 5, 1)))
        left.toString + "\n" + right.toString
    }
    test("Splitting an x-wide box"){
        val box = BBox(Tuple(-1, -2, -3, 1),
                       Tuple(9, 5.5, 3, 1))
        
        val (left, right) = box.splitBounds()
        assert(left.min.equal(Tuple(-1, -2, -3, 1)) &&
               left.max.equal(Tuple(4, 5.5, 3, 1)) &&
               right.min.equal(Tuple(4, -2, -3, 1)) &&
               right.max.equal(Tuple(9, 5.5, 3, 1)))
        left.toString + "\n" + right.toString
    }
    test("Splitting an y-wide box"){
        val box = BBox(Tuple(-1, -2, -3, 1),
                       Tuple(5, 8, 3, 1))
        
        val (left, right) = box.splitBounds()
        assert(left.min.equal(Tuple(-1, -2, -3, 1)) &&
               left.max.equal(Tuple(5, 3, 3, 1)) &&
               right.min.equal(Tuple(-1, 3, -3, 1)) &&
               right.max.equal(Tuple(5, 8, 3, 1)))
        left.toString + "\n" + right.toString
    }
    test("Splitting an z-wide box"){
        val box = BBox(Tuple(-1, -2, -3, 1),
                       Tuple(5, 3, 7, 1))
        
        val (left, right) = box.splitBounds()
        assert(left.min.equal(Tuple(-1, -2, -3, 1)) &&
               left.max.equal(Tuple(5, 3, 2, 1)) &&
               right.min.equal(Tuple(-1, -2, 2, 1)) &&
               right.max.equal(Tuple(5, 3, 7, 1)))
        left.toString + "\n" + right.toString
    }
    /*test("Partitioning a group's children"){
        val s1 = Sphere()
        s1.setTransform(translation(-2, 0, 0))
        val s2 = Sphere()
        s2.setTransform(translation(2, 0, 0))
        val s3 = Sphere()
        val g = Group()
        g.add(s1)
        g.add(s2)
        g.add(s3)
        
        val (left, right) = g.partitionChildren()
        
        println("Left: " + left.toString)
        println("g: " + g.children.toString)
        println("Right: " + right.toString)
        
        assert(g.children.length == 1 &&
               left.length == 1 &&
               right.length == 1 &&
               g.children.contains(s3) &&
               left.contains(s1) &&
               right.contains(s2))
        g.bbox
    } */
    test("Creating a subgroup from a list of children"){
        val s1 = Sphere()
        val s2 = Sphere()
        val g = Group()
        
        g.makeSubgroup(ArrayDeque[Shape](s1, s2))
        
        println("g.children: " + g.children.toString)
        println("g.children(0): " + g.children(0).toString)
        
        val gchild: Group = g.children(0).asInstanceOf[Group]
                
        assert(g.children.length == 1 &&
               gchild.children.length == 2 &&
               gchild.children.contains(s1) &&
               gchild.children.contains(s2))
        g.children.length.toString + "\n" + gchild.children.toString
    }
    test("Subdividing a primitive shape does nothing"){
        val shape = Sphere()
        val divided = shape.divide(1)
        
        assert(divided eq shape)
        divided
    }
    test("Subdividing a group partitions its children"){
        val s1 = Sphere()
        s1.setTransform(translation(-2, -2, 0))
        val s2 = Sphere()
        s2.setTransform(translation(-2, 2, 0))
        val s3 = Sphere()
        s3.setTransform(translation(4, 4, 4))
        
        val g = Group()
        g.add(s1)
        g.add(s2)
        g.add(s3)
        
        g.divide(1)
        
        val gchild0: Group = g.children(0).asInstanceOf[Group]
        val gchild1: Group = g.children(1).asInstanceOf[Group]
        val gchild10: Group = gchild1.children(0).asInstanceOf[Group]
        val gchild11: Group = gchild1.children(1).asInstanceOf[Group]

        assert((gchild0.children.contains(s1)) &&
               (gchild10.children.contains(s2)) &&
               (gchild11.children.contains(s3)))
               
        (g, gchild0, gchild1, gchild10, gchild11)
    }
    test("Subdividing a group with too few children"){
        val s1 = Sphere()
        s1.setTransform(translation(-2, 0, 0))
        val s2 = Sphere()
        s2.setTransform(translation(2, 1, 0))
        val s3 = Sphere()
        s3.setTransform(translation(2, -1, 0))
        
        val subg = Group()
        subg.add(s1)
        subg.add(s2)
        subg.add(s3)
        
        val s4 = Sphere()
        
        val g = Group()
        g.add(subg)
        g.add(s4)
        
        g.divide(3)
        
        val gchild0: Group = g.children(0).asInstanceOf[Group]
        val gchild00: Group = gchild0.children(0).asInstanceOf[Group]
        val gchild01: Group = gchild0.children(1).asInstanceOf[Group]

        assert(g.children.contains(s4) &&
               gchild00.children.contains(s1) &&
               gchild01.children.contains(s2) &&
               gchild01.children.contains(s3))
        g.children
    }
    
    */
    
    // TODO: Scenario: A CSG shape has a bounding box that contains its children
    
    /*test(""){
        val box = BBox()
        val p1 = Tuple(, , , 1)
        val p2 = Tuple(, , , 1)
        
        box.add(p1)
        box.add(p2)
        
        assert(box.min.equal(Tuple(, , , 1)) &&
               box.max.equal(Tuple(, , , 1))))
        r
    }*/
    
    /*test(""){
        
        assert()
        r
    }*/
  }
  
}
