package raytracer

import raytracer.Matrix4._
import raytracer.Color._
import raytracer.Operation._
import raytracer.Material._
import scala.math._

object Scenes {
    
    val BIG = 1.9
    val SMALL = 0.5
    
    def diceDotX(): Sphere = {
            val c = Sphere()
            c.setTransform(scaling(SMALL, BIG, BIG))
            c
    }
    
    def diceDotY(): Sphere = {
            val c = Sphere()
            c.setTransform(scaling(BIG, SMALL, BIG))
            c
    }
    
    def diceDotZ(): Sphere = {
            val c = Sphere()
            c.setTransform(scaling(BIG, BIG, SMALL))
            c
    }
    
    def diceDots(): Group = {
            val g = Group()
            
            // Z
            // +z is 2 dots
            val zpDot1 = diceDotZ()
            zpDot1.setTransform(translation(-5, -5, 11).multiply(zpDot1.transform))
            val zpDot2 = diceDotZ()
            zpDot2.setTransform(translation(5, 5, 11).multiply(zpDot2.transform))
            
            g.add(zpDot1)
            g.add(zpDot2)
            
            // -z is 5 dots
            val znDot1 = diceDotZ()
            znDot1.setTransform(translation(0, 0, -11).multiply(znDot1.transform))
            val znDot2 = diceDotZ()
            znDot2.setTransform(translation(-5, -5, -11).multiply(znDot2.transform))
            val znDot3 = diceDotZ()
            znDot3.setTransform(translation(5, -5, -11).multiply(znDot3.transform))
            val znDot4 = diceDotZ()
            znDot4.setTransform(translation(-5, 5, -11).multiply(znDot4.transform))
            val znDot5 = diceDotZ()
            znDot5.setTransform(translation(5, 5, -11).multiply(znDot5.transform))
            
            g.add(znDot1)
            g.add(znDot2)
            g.add(znDot3)
            g.add(znDot4)
            g.add(znDot5)
            
            // Y
            // +y is 6 dots
            val ypDot1 = diceDotY()
            ypDot1.setTransform(translation(-5, 11, -5).multiply(ypDot1.transform))
            val ypDot2 = diceDotY()
            ypDot2.setTransform(translation(-5, 11, 0).multiply(ypDot2.transform))
            val ypDot3 = diceDotY()
            ypDot3.setTransform(translation(-5, 11, 5).multiply(ypDot3.transform))
            val ypDot4 = diceDotY()
            ypDot4.setTransform(translation(5, 11, -5).multiply(ypDot4.transform))
            val ypDot5 = diceDotY()
            ypDot5.setTransform(translation(5, 11, 0).multiply(ypDot5.transform))
            val ypDot6 = diceDotY()
            ypDot6.setTransform(translation(5, 11, 5).multiply(ypDot6.transform))
            
            g.add(ypDot1)
            g.add(ypDot2)
            g.add(ypDot3)
            g.add(ypDot4)
            g.add(ypDot5)
            g.add(ypDot6)
            
            // -y is 1 dot
            val ynDot1 = diceDotY()
            ynDot1.setTransform(translation(0, -11, 0).multiply(ynDot1.transform))
            
            g.add(ynDot1)
            
            // X
            // +x is 4 dots
            val xpDot1 = diceDotX()
            xpDot1.setTransform(translation(11, -5, -5).multiply(xpDot1.transform))
            val xpDot2 = diceDotX()
            xpDot2.setTransform(translation(11, 5, 5).multiply(xpDot2.transform))
            val xpDot3 = diceDotX()
            xpDot3.setTransform(translation(11, -5, 5).multiply(xpDot3.transform))
            val xpDot4 = diceDotX()
            xpDot4.setTransform(translation(11, 5, -5).multiply(xpDot4.transform))
            
            g.add(xpDot1)
            g.add(xpDot2)
            g.add(xpDot3)
            g.add(xpDot4)
            
            // -x is 3 dots
            val xnDot1 = diceDotX()
            xnDot1.setTransform(translation(-11, -5, -5).multiply(xnDot1.transform))
            val xnDot2 = diceDotX()
            xnDot2.setTransform(translation(-11, 0, 0).multiply(xnDot2.transform))
            val xnDot3 = diceDotX()
            xnDot3.setTransform(translation(-11, 5, 5).multiply(xnDot3.transform))
            
            g.add(xnDot1)
            g.add(xnDot2)
            g.add(xnDot3)
            
            g
    }
    
    def dice(tr: Matrix4 = Matrix4.identityMatrix,
             cubeMaterial: Material = colorMaterial(SNOW),
             dotMaterial: Material = colorMaterial(ALMOSTBLACK)): CSG = {
        
        val cube = roundedCube()
        cube.propagateMaterial(cubeMaterial)
        val dots = diceDots()
        val dotm = dotMaterial
        dotm.specular = 0.0
        dots.propagateMaterial(dotm)
        
        val csg = CSG(DifferenceOp, cube, dots)
        csg.setTransform(tr)
        csg
    }
    
    def roundedCube(): CSG = {
            val cube = Cube()
            cube.setTransform(scaling(11, 11, 11))
            
            val sphere = Sphere()
            sphere.setTransform(scaling(15, 15, 15))
            
            CSG(IntersectionOp, cube, sphere)
            
            /*val cx = Cube()
            cx.setTransform(scaling(11, 10, 10))
            
            val cy = Cube()
            cy.setTransform(scaling(10, 11, 10))
            
            val cz = Cube()
            cz.setTransform(scaling(10, 10, 11))
            
            val frame = roundedCubeFrame()
            
            val g = Group()
            g.add(cx)
            g.add(cy)
            g.add(cz)
            g.add(frame)
            
            g*/
            
    }
    
    def roundedCubeFrame(tr: Matrix4 = Matrix4.identityMatrix): Group = {
            val upper = roundedCubeSideFrame(translation(0, 10, 0))
            val lower = roundedCubeSideFrame(translation(0, -10, 0))
            val left = roundedCubeSideFrame(translation(-10, 0, 0).multiply(
                                            rotation_z(Pi/2)))
            val right = roundedCubeSideFrame(translation(10, 0, 0).multiply(
                                            rotation_z(Pi/2)))
        
            val g = Group()
            g.add(upper)
            g.add(lower)
            g.add(left)
            g.add(right)
            g.setTransform(tr)
            g
    }
    
    def roundedCubeSideFrame(tr: Matrix4 = Matrix4.identityMatrix): Group = {
            val g = Group()
            
            val vert1 = Sphere()
            vert1.setTransform(translation(-10, 0, 10))
            val vert2 = Sphere()
            vert2.setTransform(translation(10, 0, 10))
            val vert3 = Sphere()
            vert3.setTransform(translation(-10, 0, -10))
            val vert4 = Sphere()
            vert4.setTransform(translation(10, 0, -10))
            
            g.add(vert1)
            g.add(vert2)
            g.add(vert3)
            g.add(vert4)
            
            val cyl1 = Cylinder(-10, 10, true)
            cyl1.setTransform(translation(0,0,10).multiply(
                             rotation_z(Pi/2)))
            val cyl2 = Cylinder(-10, 10, true)
            cyl2.setTransform(translation(10,0,0).multiply(
                             rotation_x(Pi/2)))
            val cyl3 = Cylinder(-10, 10, true)
            cyl3.setTransform(translation(0,0,-10).multiply(
                             rotation_z(Pi/2)))
            val cyl4 = Cylinder(-10, 10, true)
            cyl4.setTransform(translation(-10,0,0).multiply(
                             rotation_x(Pi/2)))
                             
            g.add(cyl1)
            g.add(cyl2)
            g.add(cyl3)
            g.add(cyl4)
            
            g.setTransform(tr)
            g
    }
    
    def checkersCube(cubeTr: Matrix4 = Matrix4.identityMatrix,
                     patternTr: Matrix4 = Matrix4.identityMatrix,
                     c1: Color = LIGHTGREY,
                     c2: Color = SNOW): Cube = {
            val cube = Cube()
            
            val pat = CheckersPattern(c1, c2)
            val patTr = translation(0.01, 0.01, 0.01).multiply(patternTr)
            pat.setTransform(patTr)
            
            cube.material.setPattern(pat)
            
            cube.setTransform(cubeTr)
            
            cube            
            
    }
    
    def checkersFloor(tr: Matrix4 = Matrix4.identityMatrix,
                      c1: Color = LIGHTGREY,
                      c2: Color = SNOW): Plane = {
        val floor = Plane()
        
        val floorPat = CheckersPattern(c1, c2)
        val floorPatTr = translation(0, 0.01, 0).multiply(tr)
        
        floor.material.reflective = 0.24
        floor.material.specular = 0
    
        
        floorPat.setTransform(floorPatTr)
        floor.material.setPattern(floorPat)
        
        floor

    }
    
    def planesRoom(): World = {
        // room scene
    
        // floor
        val floor = Plane()
        
        val floorPat = CheckersPattern(SNOW, Color(0.06, 0.06, 0.06))
        val floorPatTr = translation(0, 0.05, 0).multiply(
                         scaling(1.3, 1.3, 1.3))
        
        floor.material.reflective = 0.35
        floor.material.specular = 0
    
        
        floorPat.setTransform(floorPatTr)
        floor.material.setPattern(floorPat)
        
        // ceiling
        val ceiling = Plane()
        
        val ceilingTr = translation(0, 10, 0)
        ceiling.setTransform(ceilingTr)
        
        ceiling.material.color = LIGHTBLUE
        ceiling.material.reflective = 0.24
        ceiling.material.specular = 0
        
        // wallXPat
        val wallXPat = StripePattern(PEACHPUFF, PLUM)
        wallXPat.setTransform(rotation_y(Pi.toDouble / 2).multiply(
                              scaling(0.3, 0.3, 0.3)))
        
        // wall X pos
        val wallXpos = Plane()
        
        val wallXposTr = translation(10, 0, 0).multiply(
                     rotation_z(Pi.toDouble / 2))
        wallXpos.setTransform(wallXposTr)
        
        wallXpos.material.setPattern(wallXPat)
        wallXpos.material.reflective = 0.24
        wallXpos.material.specular = 0
        
        // wallXneg
        val wallXneg = Plane()
        
        val wallXnegTr = translation(-10, 0, 0).multiply(
                     rotation_z(Pi.toDouble / 2))
        wallXneg.setTransform(wallXnegTr)
        
        wallXneg.material.setPattern(wallXPat)
        wallXneg.material.specular = 0
        
        // wallZPat
        val wallZPat = StripePattern(MISTYROSE, LIGHTGREY)
        wallZPat.setTransform(scaling(0.3, 0.3, 0.3))
        
        // wallZpos
        val wallZpos = Plane()
        
        val wallZposTr = translation(0, 0, 10).multiply(
                     rotation_x(Pi.toDouble / 2))
        wallZpos.setTransform(wallZposTr)
        
        wallZpos.material.setPattern(wallZPat)
        wallZpos.material.reflective = 0.24
        wallZpos.material.specular = 0
        
        // wallZneg
        val wallZneg = Plane()
        
        val wallZnegTr = translation(0, 0, -10).multiply(
                     rotation_x(Pi.toDouble / 2))
        wallZneg.setTransform(wallZnegTr)
        
        wallZneg.material.setPattern(wallZPat)
        wallZneg.material.specular = 0
                
        val w = World()
                
        // room
        w.objects.addOne(floor)
        w.objects.addOne(ceiling)
        w.objects.addOne(wallXpos)
        w.objects.addOne(wallXneg)
        w.objects.addOne(wallZpos)
        w.objects.addOne(wallZneg)
        
        w
    }
    
    def chapter11Spheres(): World = {
        // room scene
    
        // floor
        val floor = Plane()
        
        val floorPat = CheckersPattern(SNOW, Color(0.06, 0.06, 0.06))
        val floorPatTr = translation(0, 0.1, 0).multiply(
                         scaling(13, 13, 13))
        
        floor.material.reflective = 0.24
        floor.material.specular = 0
    
        
        floorPat.setTransform(floorPatTr)
        floor.material.setPattern(floorPat)
        
        // ceiling
        val ceiling = Plane()
        
        val ceilingTr = translation(0, 10, 0)
        ceiling.setTransform(ceilingTr)
        
        ceiling.material.color = LIGHTBLUE
        ceiling.material.specular = 0
        
        // wallXPat
        val wallXPat = StripePattern(PEACHPUFF, PLUM)
        wallXPat.setTransform(rotation_y(Pi.toDouble / 2).multiply(
                              scaling(3, 3, 3)))
        
        // wall X pos
        val wallXpos = Plane()
        
        val wallXposTr = translation(10, 0, 0).multiply(
                     rotation_z(Pi.toDouble / 2))
        wallXpos.setTransform(wallXposTr)
        
        wallXpos.material.setPattern(wallXPat)
        wallXpos.material.specular = 0
        
        // wallXneg
        val wallXneg = Plane()
        
        val wallXnegTr = translation(-10, 0, 0).multiply(
                     rotation_z(Pi.toDouble / 2))
        wallXneg.setTransform(wallXnegTr)
        
        wallXneg.material.setPattern(wallXPat)
        wallXneg.material.specular = 0
        
        // wallZPat
        val wallZPat = StripePattern(MISTYROSE, LIGHTGREY)
        wallZPat.setTransform(scaling(3, 3, 3))
        
        // wallZpos
        val wallZpos = Plane()
        
        val wallZposTr = translation(0, 0, 10).multiply(
                     rotation_x(Pi.toDouble / 2))
        wallZpos.setTransform(wallZposTr)
        
        wallZpos.material.setPattern(wallZPat)
        wallZpos.material.specular = 0
        
        // wallZneg
        val wallZneg = Plane()
        
        val wallZnegTr = translation(0, 0, -10).multiply(
                     rotation_x(Pi.toDouble / 2))
        wallZneg.setTransform(wallZnegTr)
        
        wallZneg.material.setPattern(wallZPat)
        wallZneg.material.specular = 0
        
        // mirrorSphere1
        val mirrorSphere = Sphere()
        
        val mst = translation(0.5, 40, 0.5).multiply(
                   scaling(8.6, 8.6, 8.6))
        mirrorSphere.setTransform(mst)
        mirrorSphere.material.reflective = 1
    
        
        // redSphere
        val redSphere = Sphere()
        
        val rst = translation(20, 12.1, 20).multiply(
                   scaling(12, 12, 12))
        redSphere.setTransform(rst)
        redSphere.material.color = CRIMSON
        redSphere.material.reflective = 0.1
    
        
        // blueSphere
        val blueSphere = Sphere()
        
        val bst = translation(-20, 12.1, 20).multiply(
                   scaling(12, 12, 12))
        blueSphere.setTransform(bst)
        blueSphere.material.color = ROYALBLUE
        blueSphere.material.reflective = 0.1
        
        // orangeSphere
        val orangeSphere = Sphere()
        
        val ost = translation(-20, 12.1, -20).multiply(
                   scaling(12, 12, 12))
        orangeSphere.setTransform(ost)
        orangeSphere.material.color = DARKORANGE
        orangeSphere.material.reflective = 0.1
        
        // greenSphere
        val greenSphere = Sphere()
        
        val gst = translation(20, 12.1, -20).multiply(
                   scaling(12, 12, 12))
        greenSphere.setTransform(gst)
        greenSphere.material.color = DARKGREEN
        greenSphere.material.reflective = 0.1
        
        val l = PointLight(Tuple(0, 99, 0, 1), Color(1, 1, 1))
        
        val w = World()
        
        w.lights.addOne(l)
        
        // room
        w.objects.addOne(floor)
        w.objects.addOne(ceiling)
        w.objects.addOne(wallXpos)
        w.objects.addOne(wallXneg)
        w.objects.addOne(wallZpos)
        w.objects.addOne(wallZneg)
        
        // spheres
        w.objects.addOne(mirrorSphere)
        w.objects.addOne(redSphere)
        w.objects.addOne(blueSphere)
        w.objects.addOne(orangeSphere)
        w.objects.addOne(greenSphere)
        
        w
    }
}
