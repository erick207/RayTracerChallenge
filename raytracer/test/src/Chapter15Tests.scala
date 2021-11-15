package Util

import utest._
import raytracer._
import collection.mutable.ArrayDeque

object Chapter15 extends TestSuite{
        
  def tests = Tests{
/*********************************************************************/
/*                           CHAPTER 15                              */
/*                            TRIANGLES                              */
/*                                                                   */
/*********************************************************************/
    test("Constructing a triangle"){
      val p1 = Tuple(0, 1, 0, 1)
      val p2 = Tuple(-1, 0, 0, 1)
      val p3 = Tuple(1, 0, 0, 1)
      
      val t = Triangle(p1, p2, p3)
      
      assert(t.p1.equal(Tuple(0, 1, 0, 1)) &&
             t.p2.equal(Tuple(-1, 0, 0, 1)) &&
             t.p3.equal(Tuple(1, 0, 0, 1)) &&
             t.e1.equal(Tuple(-1, -1, 0, 0)) &&
             t.e2.equal(Tuple(1, -1, 0, 0)) &&
             t.normal.equal(Tuple(0, 0, -1, 0)))
      t
    }
    
    test("Finding the normal on a triangle"){
      val p1 = Tuple(0, 1, 0, 1)
      val p2 = Tuple(-1, 0, 0, 1)
      val p3 = Tuple(1, 0, 0, 1)
      
      val t = Triangle(p1, p2, p3)
      
      val n1 = t.localNormalAt(Tuple(0, 0.5, 0))
      val n2 = t.localNormalAt(Tuple(-0.5, 0.75, 0))
      val n3 = t.localNormalAt(Tuple(0.5, 0.25, 0))
      
      assert(n1.equal(t.normal) &&
             n2.equal(t.normal) &&
             n3.equal(t.normal))
      t.normal
    }
    test("Intersecting a ray parallel to the triangle"){
      val p1 = Tuple(0, 1, 0, 1)
      val p2 = Tuple(-1, 0, 0, 1)
      val p3 = Tuple(1, 0, 0, 1)
      
      val t = Triangle(p1, p2, p3)
      
      val origin = Tuple(0, -1, -2, 1)
      val direction = Tuple(0, 1, 0, 0)
      
      val r = Ray(origin, direction)
      
      t.intersect(r)
      
      assert(r.intersections.isEmpty)
      r.intersections.length
    }
    test("A ray misses the p1-p3 edge"){
      val p1 = Tuple(0, 1, 0, 1)
      val p2 = Tuple(-1, 0, 0, 1)
      val p3 = Tuple(1, 0, 0, 1)
      
      val t = Triangle(p1, p2, p3)
      
      val origin = Tuple(1, 1, -2, 1)
      val direction = Tuple(0, 0, 1, 0)
      
      val r = Ray(origin, direction)
      
      t.intersect(r)
      
      assert(r.intersections.isEmpty)
      r.intersections.length
    }
    test("A ray misses the p1-p2 edge"){
      val p1 = Tuple(0, 1, 0, 1)
      val p2 = Tuple(-1, 0, 0, 1)
      val p3 = Tuple(1, 0, 0, 1)
      
      val t = Triangle(p1, p2, p3)
      
      val origin = Tuple(-1, 1, -2, 1)
      val direction = Tuple(0, 0, 1, 0)
      
      val r = Ray(origin, direction)
      
      t.intersect(r)
      
      assert(r.intersections.isEmpty)
      r.intersections.length
    }
    test("A ray misses the p2-p3 edge"){
      val p1 = Tuple(0, 1, 0, 1)
      val p2 = Tuple(-1, 0, 0, 1)
      val p3 = Tuple(1, 0, 0, 1)
      
      val t = Triangle(p1, p2, p3)
      
      val origin = Tuple(0, -1, -2, 1)
      val direction = Tuple(0, 0, 1, 0)
      
      val r = Ray(origin, direction)
      
      t.intersect(r)
      
      assert(r.intersections.isEmpty)
      r.intersections.length
    }
    test("A ray strikes a triangle"){
      val p1 = Tuple(0, 1, 0, 1)
      val p2 = Tuple(-1, 0, 0, 1)
      val p3 = Tuple(1, 0, 0, 1)
      
      val t = Triangle(p1, p2, p3)
      
      val origin = Tuple(0, 0.5, -2, 1)
      val direction = Tuple(0, 0, 1, 0)
      
      val r = Ray(origin, direction)
      
      t.intersect(r)
      
      assert(r.intersections.length == 1 &&
             Utilities.equal(r.intersections(0).t, 2))
      r.intersections.length
    }
    /*
    
    test("Ignoring unrecognized lines"){
      val objFile = ObjParser("/gibberish.obj")
      
      assert(objFile.ignored == 5)
      objFile.ignored
    }
    test("Vertex records"){
      val objFile = ObjParser("/vertexRecords.obj")
      
      assert(objFile.vertices(0).equal(Tuple(-1, 1, 0, 1)) &&
             objFile.vertices(1).equal(Tuple(-1, 0.5, 0, 1)) &&
             objFile.vertices(2).equal(Tuple(1, 0, 0, 1)) &&
             objFile.vertices(3).equal(Tuple(1, 1, 0, 1)))
      objFile.vertices.length
    }
    test("Parsing triangle faces"){
      val objFile = ObjParser("/triangleFaces.obj")
      
      val tris = objFile.getGroup.children(0).asInstanceOf[Group].children
      
      assert(tris(0).asInstanceOf[Triangle].p1.equal(objFile.vertices(0)) &&
             tris(0).asInstanceOf[Triangle].p2.equal(objFile.vertices(1)) &&
             tris(0).asInstanceOf[Triangle].p3.equal(objFile.vertices(2)) &&
             tris(1).asInstanceOf[Triangle].p1.equal(objFile.vertices(0)) &&
             tris(1).asInstanceOf[Triangle].p2.equal(objFile.vertices(2)) &&
             tris(1).asInstanceOf[Triangle].p3.equal(objFile.vertices(3)))
      tris(0).toString + tris(1).toString
    }
    test("Triangulating polygons"){
      val objFile = ObjParser("/triangulatingPolygons.obj")
      
      val tris = objFile.getGroup.children(0).asInstanceOf[Group].children
            
      assert(tris(0).asInstanceOf[Triangle].p1.equal(objFile.vertices(0)) &&
             tris(0).asInstanceOf[Triangle].p2.equal(objFile.vertices(1)) &&
             tris(0).asInstanceOf[Triangle].p3.equal(objFile.vertices(2)) &&
             tris(1).asInstanceOf[Triangle].p1.equal(objFile.vertices(0)) &&
             tris(1).asInstanceOf[Triangle].p2.equal(objFile.vertices(2)) &&
             tris(1).asInstanceOf[Triangle].p3.equal(objFile.vertices(3)) &&
             tris(2).asInstanceOf[Triangle].p1.equal(objFile.vertices(0)) &&
             tris(2).asInstanceOf[Triangle].p2.equal(objFile.vertices(3)) &&
             tris(2).asInstanceOf[Triangle].p3.equal(objFile.vertices(4)))
      tris(0).toString + tris(1).toString + tris(2).toString
    }
    test("Triangles in groups"){
      val objFile = ObjParser("/triangleGroups.obj")
            
      val t1 = objFile.getGroup.children(1).asInstanceOf[Group].children(0).asInstanceOf[Triangle]
      val t2 = objFile.getGroup.children(2).asInstanceOf[Group].children(0).asInstanceOf[Triangle]
      
      assert(t1.p1.equal(objFile.vertices(0)) &&
             t1.p2.equal(objFile.vertices(1)) &&
             t1.p3.equal(objFile.vertices(2)) &&
             t2.p1.equal(objFile.vertices(0)) &&
             t2.p2.equal(objFile.vertices(2)) &&
             t2.p3.equal(objFile.vertices(3)))
      t1.toString + t2.toString
    }
    test("Constructing a smooth triangle"){
      val p1 = Tuple(0, 1, 0, 1)
      val p2 = Tuple(-1, 0, 0, 1)
      val p3 = Tuple(1, 0, 0, 1)
      val n1 = Tuple(0, 1, 0, 0)
      val n2 = Tuple(-1, 0, 0, 0)
      val n3 = Tuple(1, 0, 0, 0)
      val tri = SmoothTriangle(p1, p2, p3, n1, n2, n3)
      
      assert(tri.p1.equal(p1) &&
             tri.p2.equal(p2) &&
             tri.p3.equal(p3) &&
             tri.n1.equal(n1) &&
             tri.n2.equal(n2) &&
             tri.n3.equal(n3))
      tri
    }
    test("An intersection can encapsulate u and v"){
      val s = Sphere()
      val i = Intersection.withUV(3.5, s, 0.2, 0.4)
      
      assert(Utilities.equal(i.u.get, 0.2) &&
             Utilities.equal(i.v.get, 0.4))
      i
    }
    test("An intersection with a smooth triangle stores u/v"){
      val p1 = Tuple(0, 1, 0, 1)
      val p2 = Tuple(-1, 0, 0, 1)
      val p3 = Tuple(1, 0, 0, 1)
      val n1 = Tuple(0, 1, 0, 0)
      val n2 = Tuple(-1, 0, 0, 0)
      val n3 = Tuple(1, 0, 0, 0)
      val tri = SmoothTriangle(p1, p2, p3, n1, n2, n3)
      
      val r = Ray(Tuple(-0.2, 0.3, -2, 1), Tuple(0, 0, 1, 0))
      
      tri.localIntersect(r)
      
      assert(Utilities.equal(r.intersections(0).u.get, 0.45) &&
             Utilities.equal(r.intersections(0).v.get, 0.25))
      r.intersections(0)
    }
    test("A smooth triangle uses u/v to interpolate the normal"){
      val p1 = Tuple(0, 1, 0, 1)
      val p2 = Tuple(-1, 0, 0, 1)
      val p3 = Tuple(1, 0, 0, 1)
      val n1 = Tuple(0, 1, 0, 0)
      val n2 = Tuple(-1, 0, 0, 0)
      val n3 = Tuple(1, 0, 0, 0)
      val tri = SmoothTriangle(p1, p2, p3, n1, n2, n3)
      
      val i = Intersection.withUV(1, tri, 0.45, 0.25)
      val n = tri.normalAt(Tuple(0, 0, 0, 1), Some(i))
      
      assert(n.equal(Tuple(-0.5547, 0.83205, 0, 0)))
      n
    }
    test("Preparing the normal on a smooth triangle"){
      val p1 = Tuple(0, 1, 0, 1)
      val p2 = Tuple(-1, 0, 0, 1)
      val p3 = Tuple(1, 0, 0, 1)
      val n1 = Tuple(0, 1, 0, 0)
      val n2 = Tuple(-1, 0, 0, 0)
      val n3 = Tuple(1, 0, 0, 0)
      val tri = SmoothTriangle(p1, p2, p3, n1, n2, n3)
      
      val i = Intersection.withUV(1, tri, 0.45, 0.25)
      val r = Ray(Tuple(-0.2, 0.3, -2, 1), Tuple(0, 0, 1, 0))
      val comps = i.prepareComputations(r)
      assert(comps.normalv.equal(Tuple(-0.5547, 0.83205, 0, 0)))
      comps
    }
    test("Vertex normal records"){
      val objFile = ObjParser("/vertexNormalRecords.obj")
      
      assert(objFile.normals(0).equal(Tuple(0, 0, 1, 0)) &&
             objFile.normals(1).equal(Tuple(0.707, 0, -0.707, 0)) &&
             objFile.normals(2).equal(Tuple(1, 2, 3, 0)))
      objFile.normals.length
    }
    test("Faces with normals"){
      val objFile = ObjParser("/facesWithNormals.obj")
      
      
      val g0 = objFile.getGroup.children(0).asInstanceOf[Group]
      val t1 = g0.children(0).asInstanceOf[SmoothTriangle]
      val t2 = g0.children(1).asInstanceOf[SmoothTriangle]
      
      assert(objFile.vertices(0).equal(t1.p1) &&
             objFile.vertices(1).equal(t1.p2) &&
             objFile.vertices(2).equal(t1.p3) &&
             objFile.normals(2).equal(t1.n1) &&
             objFile.normals(0).equal(t1.n2) &&
             objFile.normals(1).equal(t1.n3) &&
             objFile.vertices(0).equal(t2.p1) &&
             objFile.vertices(1).equal(t2.p2) &&
             objFile.vertices(2).equal(t2.p3) &&
             objFile.normals(2).equal(t2.n1) &&
             objFile.normals(0).equal(t2.n2) &&
             objFile.normals(1).equal(t2.n3))
      objFile.normals.length
    }
    */
  }
}
