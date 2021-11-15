package raytracer

import scala.io.Source
import collection.mutable.ArrayDeque
import raytracer._

case class ObjParser(filename: String) {
    
    val t1 = System.nanoTime
    
    var ignored = 0 
    
    val vertices = ArrayDeque[Tuple]()
    val normals = ArrayDeque[Tuple]()
    val triangles = ArrayDeque[Shape]()
    val defaultGroup = Group()
    var currentGroup = Group()
    
    val lines = Source.fromFile(filename).getLines.toArray
    
    parseLines(lines)
    
    val duration = (System.nanoTime - t1) / 1e9d
    println(f"$duration%.1f seconds parsing OBJ file")
    
    def parseLines(lines: Array[String]): Unit = {
        lines.map(
            l => {
                if(l.startsWith("v ")) {
                    vertices += getVert(l)
                } else if(l.startsWith("f ")) {
                    triangles ++= getTriangles(l)
                } else if(l.startsWith("vn ")) {
                    normals += getNormal(l)
                } else if(l.startsWith("g ")) {
                    triangles.map(currentGroup.add(_))
                    defaultGroup.add(currentGroup)
                    currentGroup = Group()
                    triangles.clear()
                } else {
                    ignored = ignored + 1
                }
                
            })
            triangles.map(currentGroup.add(_))
            defaultGroup.add(currentGroup)
            triangles.clear()
    }
    
    def getVert(l: String): Tuple = {
        val vert = l.drop(2).split(" ").map(_.toDouble)
        Tuple(vert(0), vert(1), vert(2), 1)
    }
    
    def getNormal(l: String): Tuple = {
        val n = l.drop(3).split(" ").map(_.toDouble)
        Tuple(n(0), n(1), n(2), 0)
    }
    
    def getTriangles(l: String): ArrayDeque[Shape] = {
        val tris = ArrayDeque[Shape]()
        
        val verts = l.drop(2).split(" ")
        
        var vertIndex: Array[Int] = Array()
        var texIndex: Array[Int] = Array()
        var norIndex: Array[Int] = Array()
        
        var hasTex = false
        var hasNor = false
        
        if(verts(0).contains("/")){ 
            val vertTexNors = verts.flatMap(_.split("/"))
            
            hasTex= vertTexNors(1).nonEmpty
            hasNor= vertTexNors(2).nonEmpty
            
            vertIndex = (vertTexNors.zipWithIndex 
                         collect { case (e, i) if(i % 3 == 0) => e}).map(_.toInt).map(_ - 1)
            
            if(hasTex) texIndex =
                (vertTexNors.zipWithIndex
                 collect { case (e, i) if(i % 3 == 1) => e}).map(_.toInt).map(_ - 1)
            
            if(hasNor) norIndex =
                (vertTexNors.zipWithIndex
                 collect { case (e, i) if(i % 3 == 2) => e}).map(_.toInt).map(_ - 1)
            
            } else {
            vertIndex = verts.map(_.toInt).map(_ - 1)
        }
        
        if(vertIndex.length > 3) {
            (1 to (vertIndex.length - 2)).map(i => {
                    val tri: Shape = 
                        if(hasNor)
                            SmoothTriangle(vertices(0),
                                        vertices(i),
                                        vertices(i + 1),
                                        normals(norIndex(0)),
                                        normals(norIndex(i)),
                                        normals(norIndex(i + 1)))
                        else Triangle(vertices(0), vertices(i), vertices(i + 1))
                    
                    tris += tri
                })
        } else {
            if(hasNor) {
                tris += SmoothTriangle(vertices(vertIndex(0)),
                                       vertices(vertIndex(1)),
                                       vertices(vertIndex(2)),
                                       normals(norIndex(0)),
                                       normals(norIndex(1)),
                                       normals(norIndex(2)))
            } else tris += Triangle(vertices(vertIndex(0)),
                                    vertices(vertIndex(1)),
                                    vertices(vertIndex(2)))
        }
            
        tris
    }
    
    def getGroup: Group = defaultGroup
}

object OBJ {
    def apply(filename: String): ObjParser = new ObjParser(filename)
}
