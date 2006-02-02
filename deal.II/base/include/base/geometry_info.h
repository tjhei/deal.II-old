//---------------------------------------------------------------------------
//    $Id$
//    Version: $Name$
//
//    Copyright (C) 1998, 1999, 2000, 2001, 2002, 2003, 2004, 2005, 2006 by the deal.II authors
//
//    This file is subject to QPL and may not be  distributed
//    without copyright and license information. Please refer
//    to the file deal.II/doc/license.html for the  text  and
//    further information on this license.
//
//---------------------------------------------------------------------------
#ifndef __deal2__geometry_info_h
#define __deal2__geometry_info_h


#include <base/config.h>
#include <base/exceptions.h>
#include <base/point.h>


template <int dim> class GeometryInfo;




/**
 * Topological description of zero dimensional cells,
 * i.e. points. This class might not look too useful but often is if
 * in a certain dimension we would like to enquire information about
 * objects with dimension one lower than the present, e.g. about
 * faces.
 *
 * This class contains as static members information on vertices and
 * faces of a @p dim-dimensional grid cell. The interface is the same
 * for all dimensions. If a value is of no use in a low dimensional
 * cell, it is (correctly) set to zero, e.g. #subfaces_per_face in
 * 1d.
 *
 * This information should always replace hard-coded numbers of
 * vertices, neighbors and so on, since it can be used dimension
 * independently.
 *
 * @ingroup grid geomprimitives
 * @author Wolfgang Bangerth, 1998
 */
template <>
struct GeometryInfo<0> 
{

				     /**
				      * Number of children a cell has.
				      */
    static const unsigned int children_per_cell = 1;

				     /**
				      * Number of faces a cell has.
				      */
    static const unsigned int faces_per_cell    = 0;

				     /**
				      * Number of children each face has
				      * when the adjacent cell is refined.
				      */
    static const unsigned int subfaces_per_face = 0;

				     /**
				      * Number of vertices a cell has.
				      */
    static const unsigned int vertices_per_cell = 1;

				     /**
				      * Number of vertices each face has.
				      * Since this is not useful in one
				      * dimension, we provide a useless
				      * number (in the hope that a compiler
				      * may warn when it sees constructs like
				      * <tt>for (i=0; i<vertices_per_face; ++i)</tt>,
				      * at least if @p i is an <tt>unsigned int</tt>.
				      */
    static const unsigned int vertices_per_face = 0;

				     /**
				      * Number of lines each face has.
				      */
    static const unsigned int lines_per_face    = 0;
    
				     /**
				      * Number of quads on each face.
				      */
    static const unsigned int quads_per_face    = 0;

				     /**
				      * Number of lines of a cell.
				      */
    static const unsigned int lines_per_cell    = 0;

				     /**
				      * Number of quadrilaterals of a
				      * cell.
				      */
    static const unsigned int quads_per_cell    = 0;

				     /**
				      * Number of hexahedra of a
				      * cell.
				      */
    static const unsigned int hexes_per_cell    = 0;
};





/**
 * This class provides dimension independent information to all topological
 * structures that make up the unit, or
 * @ref GlossReferenceCell "reference cell".
 *
 * It is the one central point in the library where information about the
 * numbering of vertices, lines, or faces of the reference cell is
 * collected. Consequently, the information of this class is used extensively
 * in the geometric description of Triangulation objects, as well as in
 * various other parts of the code. In particular, it also serves as the focus
 * of writing code in a dimension independent way; for example, instead of
 * writing a loop over vertices 0<=v<4 in 2d, one would write it as
 * 0<=v<GeometryInfo<dim>::vertices_per_cell, thus allowing the code to work
 * in 3d as well without changes.
 *
 * The most frequently used parts of the class are its static members like
 * vertices_per_cell, faces_per_cell, etc. However, the class also offers
 * information about more abstract questions like the orientation of faces,
 * etc. The following documentation gives a textual description of many of
 * these concepts.
 *
 *
 * <h3>Implementation conventions for two spatial dimensions</h3>
 * 
 * From version 5.2 onwards deal.II is based on a numbering scheme
 * that uses a lexicographic ordering (with x running fastest)
 * wherever possible, hence trying to adopt a kind of 'canonical'
 * ordering.
 *
 * The ordering of vertices and faces (lines) in 2d is defined by
 *
 * N1) vertices are numbered in lexicographic ordering
 *
 * N2) faces (lines in 2d): first the two faces with normals in x-
 * and then y-direction. For each two faces: first the face with
 * normal in negative coordinate direction, then the one with normal
 * in positive direction, i.e. the faces are ordered according to
 * their normals pointing in -x, x, -y, y direction.
 *
 * N3) the direction of a line is represented by the direction of
 * point 0 towards point 1 and is always in one of the coordinate
 * directions
 * 
 * N4/ face lines in 3d are ordered, such that the induced 2d local
 * coordinate system (x,y) implies (right hand rule) a normal in
 * face normal direction, see N2/.
 *
 * The resulting numbering of vertices and faces (lines) in 2d as
 * well as the directions of lines is shown in the following.
 * @verbatim
 *       3
 *    2-->--3
 *    |     |
 *   0^     ^1
 *    |     |
 *    0-->--1
 *        2
 * @endverbatim
 * 
 * Note that the orientation of lines has to be correct upon construction of a
 * grid; however, it is automatically preserved upon refinement.
 *
 * Further we define that child lines have the same direction as their parent,
 * i.e. that <tt>line->child(0)->vertex(0)==line->vertex(0)</tt> and
 * <tt>line->child(1)->vertex(1)==line->vertex(1)</tt>. This also implies,
 * that the first sub-line (<tt>line->child(0)</tt>) is the one at vertex(0)
 * of the old line.
 *
 * Similarly we define, that the four children of a quad are adjacent to the
 * vertex with the same number of the old quad.
 *
 * Note that information about several of these conventions can be
 * extracted at run- or compile-time from the member functions and
 * variables of the present class.
 *
 *
 * <h4>Coordinate systems</h4>
 *
 * When explicit coordinates are required for points in a cell (e.g for
 * quadrature formulae or the point of definition of trial functions), we
 * define the following coordinate system for the unit cell:
 * @verbatim
 *  y^   2-----3
 *   |   |     |
 *   |   |     |
 *   |   |     |
 *   |   0-----1
 *   *------------>x
 * @endverbatim
 * 
 * Here, vertex 0 is the origin of the coordinate system, vertex 1 has
 * coordinates <tt>(1,0)</tt>, vertex 2 at <tt>(0,1)</tt> and vertex 3 at
 * <tt>(1,1)</tt>. The GeometryInfo<dim>::unit_cell_vertex() function can be
 * used to query this information at run-time.
 *
 *
 * <h3>Implementation conventions for three spatial dimensions</h3>
 *
 * By convention, we will use the following numbering conventions
 * for vertices, lines and faces of hexahedra in three space
 * dimensions. Before giving these conventions we declare the
 * following sketch to be the standard way of drawing 3d pictures of
 * hexahedra:
 * @verbatim
 *                       *-------*        *-------*
 *                      /|       |       /       /|
 *                     / |       |      /       / |
 *  z                 /  |       |     /       /  |
 *  ^                *   |       |    *-------*   |
 *  |   ^y           |   *-------*    |       |   *
 *  |  /             |  /       /     |       |  /
 *  | /              | /       /      |       | /
 *  |/               |/       /       |       |/
 *  *------>x        *-------*        *-------*
 * @endverbatim
 * The left part of the picture shows the left, bottom and back face of the
 * cube, while the right one shall be the top, right and front face. You may
 * recover the whole cube by moving the two parts together into one.
 *
 * Note again that information about several of the following
 * conventions can be extracted at run- or compile-time from the
 * member functions and variables of the present class.
 *
 * <h4>Vertices</h4>
 *  
 * The ordering of vertices in 3d is defined by the same rules as in
 * the 2d case, i.e.
 *
 * N1) vertices are numbered in lexicographic ordering.
 *
 * Hence, the vertices are numbered as follows   
 * @verbatim
 *       6-------7        6-------7
 *      /|       |       /       /|
 *     / |       |      /       / |
 *    /  |       |     /       /  |
 *   4   |       |    4-------5   |
 *   |   2-------3    |       |   3
 *   |  /       /     |       |  /
 *   | /       /      |       | /
 *   |/       /       |       |/
 *   0-------1        0-------1
 * @endverbatim
 * 
 * We note, that first the vertices on the bottom face (z=0) are numbered
 * exactly the same way as are the vertices on a quadrilateral. Then the
 * vertices on the top face (z=1) are numbered similarly by moving the bottom
 * face to the top. Again, the GeometryInfo<dim>::unit_cell_vertex() function
 * can be used to query this information at run-time.
 *
 * 
 * <h4>Lines</h4>
 *
 * Here, the same holds as for the vertices:
 *
 * N4) line ordering in 3d:
 * <ul>
 *   <li>first the lines of face (z=0) in 2d line ordering,
 *   <li>then the lines of face (z=1) in 2d line ordering,
 *   <li>finally the lines in z direction in lexicographic ordering
 * </ul>
 * @verbatim
 *       *---7---*        *---7---*
 *      /|       |       /       /|
 *     4 |       11     4       5 11
 *    /  10      |     /       /  |
 *   *   |       |    *---6---*   |
 *   |   *---3---*    |       |   *
 *   |  /       /     |       9  /
 *   8 0       1      8       | 1
 *   |/       /       |       |/
 *   *---2---*        *---2---*
 * @endverbatim
 * As in 2d lines are directed in coordinate directions, see N3.
 * @verbatim
 *       *--->---*        *--->---*
 *      /|       |       /       /|
 *     ^ |       ^      ^       ^ ^
 *    /  ^       |     /       /  |
 *   *   |       |    *--->---*   |
 *   |   *--->---*    |       |   *
 *   |  /       /     |       ^  /
 *   ^ ^       ^      ^       | ^
 *   |/       /       |       |/
 *   *--->---*        *--->---*
 * @endverbatim
 *
 * The fact that edges (just as vertices and faces) are entities that
 * are stored in their own right rather than constructed from cells
 * each time they are needed, means that adjacent cells actually have
 * pointers to edges that are thus shared between them. This implies
 * that the convention that sets of parallel edges have parallel
 * directions is not only a local condition. Before a list of cells is
 * passed to an object of the Triangulation class for creation of a
 * triangulation, you therefore have to make sure that cells are
 * oriented in a compatible fashion, so that edge directions are
 * globally according to above convention. However, the GridReordering
 * class can do this for you, by reorienting cells and edges of an
 * arbitrary list of input cells that need not be already sorted.
 * 
 * <h4>Faces</h4>
 *
 * The numbering of faces in 3d is defined by a rule analogous to 2d:
 *
 * N2a) faces (quads in 3d): first the two faces with normals in x-,
 * then y- and z-direction. For each two faces: first the face with
 * normal in negative coordinate direction, then the one with normal
 * in positive direction, i.e. the faces are ordered according to
 * their normals pointing in -x, x, -y, y, -z, z direction.
 *
 * Therefore, the faces are numbered in the ordering: left, right,
 * front, back, bottom and top face:
 * @verbatim
 *       *-------*        *-------*
 *      /|       |       /       /|
 *     / |   3   |      /   5   / |
 *    /  |       |     /       /  |
 *   *   |       |    *-------*   |
 *   | 0 *-------*    |       | 1 *
 *   |  /       /     |       |  /
 *   | /   4   /      |   2   | /
 *   |/       /       |       |/
 *   *-------*        *-------*
 * @endverbatim
 *
 * The <em>standard</em> direction of the faces is such, that the
 * induced 2d local coordinate system (x,y) implies (right hand
 * rule) a normal in face normal direction, see N2a).  In the
 * following we show the local coordinate system and the numbering
 * of face lines:
 * <ul>
 * <li> Faces 0 and 1:
 *  @verbatim
 *          Face 0           Face 1
 *        *-------*        *-------*
 *       /|       |       /       /|
 *      3 1       |      /       3 1
 *    y/  |       |     /      y/  |
 *    *   |x      |    *-------*   |x
 *    |   *-------*    |       |   *
 *    0  /       /     |       0  /
 *    | 2       /      |       | 2
 *    |/       /       |       |/
 *    *-------*        *-------*
 *  @endverbatim
 * 
 * <li> Faces 2 and 3:
 *  @verbatim
 *        x Face 3           Face 2
 *        *---1---*        *-------*
 *       /|       |       /       /|
 *      / |       3      /       / |
 *     /  2       |    x/       /  |
 *    *   |       |    *---1---*   |
 *    |   *---0---*y   |       |   *
 *    |  /       /     |       3  /
 *    | /       /      2       | /
 *    |/       /       |       |/
 *    *-------*        *---0---*y
 *  @endverbatim 
 * 
 * <li> Faces 4 and 5:
 *  @verbatim
 *          Face 4         y Face 5
 *        *-------*        *---3---*
 *       /|       |       /       /|
 *      / |       |      0       1 |
 *     /  |       |     /       /  |
 *    *   |y      |    *---2---* x |
 *    |   *---3---*    |       |   *
 *    |  /       /     |       |  /
 *    | 0       1      |       | /
 *    |/       /       |       |/
 *    *---2---* x      *-------*
 *  @endverbatim
 * </ul>
 *
 * The face line numbers (0,1,2,3) correspond to following cell line
 * numbers.
 * <ul>
 * <li> Face 0: lines 8, 10, 0, 4;
 * <li> Face 1: lines 9, 11, 1, 5;
 * <li> Face 2: lines 2, 6, 8, 9;
 * <li> Face 3: lines 3, 7, 10, 11;
 * <li> Face 4: lines 0, 1, 2, 3;
 * <li> Face 5: lines 4, 5, 6, 7;
 * </ul>
 * You can get these numbers using the
 * GeometryInfo<3>::face_to_cell_lines() function.
 *
 * The face normals can be deduced from the face orientation by
 * applying the right hand side rule (x,y -> normal).  We note, that
 * in the standard orientation of faces in 2d, faces 0 and 2 have
 * normals that point into the cell, and faces 1 and 3 have normals
 * pointing outward. In 3d, faces 0, 2, and 4
 * have normals that point into the cell, while the normals of faces
 * 1, 3, and 5 point outward. This information, again, can be queried from
 * GeometryInfo<dim>::unit_normal_orientation.
 *
 * However, it turns out that a significant number of 3d meshes cannot
 * satisfy this convention. This is due to the fact that the face
 * convention for one cell already implies something for the
 * neighbor, since they share a common face and fixing it for the
 * first cell also fixes the normal vectors of the opposite faces of
 * both cells. It is easy to construct cases of loops of cells for
 * which this leads to cases where we cannot find orientations for
 * all faces that are consistent with this convention.
 *
 * For this reason, above convention is only what we call the <em>standard
 * orientation</em>. deal.II actually allows faces in 3d to have either the
 * standard direction, or its opposite, in which case the lines that make up a
 * cell would have reverted orders, and the above line equivalences would not
 * hold any more. You can ask a cell whether a given face has standard
 * orientation by calling <tt>cell->face_orientation(face_no)</tt>: if the
 * result is @p true, then the face has standard orientation, otherwise its
 * normal vector is pointing the other direction. There are not very many
 * places in application programs where you need this information actually,
 * but a few places in the library make use of this. Note that in 2d, the
 * result is always @p true. More information on the topic can be found in the
 * @ref GlossFaceOrientation "glossary" article on this topic.
 *
 *
 * <h4>Children</h4>
 *
 * The eight children of a cell are numbered according to the vertices they
 * are adjacent to:
 * @verbatim
 *       *-------*        *-------*
 *      /| 6   7 |       / 6   7 /|
 *     /6|       |      /       /7|
 *    /  |       |     / 4   5 /  |
 *   *   | 2   3 |    *-------*5 3|
 *   |4 2*-------*    | 4   5 |   *
 *   |  / 2   3 /     |       |  /
 *   |0/       /      |       |1/
 *   |/0    1 /       | 0   1 |/
 *   *-------*        *-------*
 * @endverbatim
 *
 * Taking into account the orientation of the faces, the following
 * children are adjacent to the respective faces:
 * <ul>
 * <li> Face 0: children 0, 2, 4, 6;
 * <li> Face 1: children 1, 3, 5, 7;
 * <li> Face 2: children 0, 4, 1, 5;
 * <li> Face 3: children 2, 6, 3, 7;
 * <li> Face 4: children 0, 1, 2, 3;
 * <li> Face 5: children 4, 5, 6, 7.
 * </ul>
 * You can get these numbers using the
 * GeometryInfo<3>::child_cell_on_face() function. As each child is
 * adjacent to the vertex with the same number these numbers are
 * also given by the GeometryInfo<3>::face_to_cell_vertices()
 * function.
 *
 * Note that, again, the above list only holds for faces in their
 * standard orientation. If a face is not in standard orientation,
 * then the children at positions 1 and 2 (counting from 0 to 3)
 * would be swapped. In fact, this is what the child_cell_on_face
 * and the face_to_cell_vertices functions of GeometryInfo<3> do,
 * when invoked with a <tt>face_orientation=false</tt> argument.
 *
 * The information which child cell is at which position of which face
 * is most often used when computing jump terms across faces with
 * hanging nodes, using objects of type FESubfaceValues. Sitting on
 * one cell, you would look at a face and figure out which child of
 * the neighbor is sitting on a given subface between the present and
 * the neighboring cell. To avoid having to query the standard
 * orientation of the faces of the two cells every time in such cases,
 * you should use a function call like
 * <tt>cell->neighbor_child_on_subface(face_no,subface_no)</tt>, which
 * returns the correct result both in 2d (where face orientations are
 * immaterial) and 3d (where it is necessary to use the face
 * orientation as additional argument to
 * <tt>GeometryInfo<3>::child_cell_on_face</tt>).
 *
 * <h4>Coordinate systems</h4>
 *
 * We define the following coordinate system for the explicit coordinates of
 * the vertices of the unit cell:
 * @verbatim
 *                       6-------7        6-------7
 *                      /|       |       /       /|
 *                     / |       |      /       / |
 *  z                 /  |       |     /       /  |
 *  ^                4   |       |    4-------5   |
 *  |   ^y           |   2-------3    |       |   3
 *  |  /             |  /       /     |       |  /
 *  | /              | /       /      |       | /
 *  |/               |/       /       |       |/
 *  *------>x        0-------1        0-------1
 * @endverbatim
 *
 * By the convention laid down as above, the vertices have the following
 * coordinates (lexicographic, with x running fastest):
 * <ul>
 *    <li> Vertex 0: <tt>(0,0,0)</tt>;
 *    <li> Vertex 1: <tt>(1,0,0)</tt>;
 *    <li> Vertex 2: <tt>(0,1,0)</tt>;
 *    <li> Vertex 3: <tt>(1,1,0)</tt>;
 *    <li> Vertex 4: <tt>(0,0,1)</tt>;
 *    <li> Vertex 5: <tt>(1,0,1)</tt>;
 *    <li> Vertex 6: <tt>(0,1,1)</tt>;
 *    <li> Vertex 7: <tt>(1,1,1)</tt>.
 * </ul>
 *
 * 
 *
 * @note Instantiations for this template are provided for dimensions 1,2,3,4,
 * and there is a specialization for dim=0 (see the section on @ref
 * Instantiations in the manual).
 *
 * @ingroup grid geomprimitives
 * @author Wolfgang Bangerth, 1998, Ralf Hartmann, 2005
 */
template <int dim>
struct GeometryInfo
{
    
				     /**
				      * Number of children of a refined cell.
				      */
    static const unsigned int children_per_cell = 1 << dim;

				     /**
				      * Number of faces of a cell.
				      */
    static const unsigned int faces_per_cell = 2 * dim;

				     /**
				      * Number of children each face has
				      * when the adjacent cell is refined.
				      */
    static const unsigned int subfaces_per_face = GeometryInfo<dim-1>::children_per_cell;

				     /**
				      * Number of vertices of a cell.
				      */
    static const unsigned int vertices_per_cell = 1 << dim;

				     /**
				      * Number of vertices on each
				      * face.
				      */
    static const unsigned int vertices_per_face = GeometryInfo<dim-1>::vertices_per_cell;

				     /**
				      * Number of lines on each face.
				      */
    static const unsigned int lines_per_face
    = GeometryInfo<dim-1>::lines_per_cell;
    
				     /**
				      * Number of quads on each face.
				      */
    static const unsigned int quads_per_face
    = GeometryInfo<dim-1>::quads_per_cell;

				     /**
				      * Number of lines of a cell.
				      *
				      * The formula to compute this makes use
				      * of the fact that when going from one
				      * dimension to the next, the object of
				      * the lower dimension is copied once
				      * (thus twice the old number of lines)
				      * and then a new line is inserted
				      * between each vertex of the old object
				      * and the corresponding one in the copy.
				      */
    static const unsigned int lines_per_cell
    = (2*GeometryInfo<dim-1>::lines_per_cell +
       GeometryInfo<dim-1>::vertices_per_cell);

				     /**
				      * Number of quadrilaterals of a
				      * cell.
				      *
				      * This number is computed recursively
				      * just as the previous one, with the
				      * exception that new quads result from
				      * connecting an original line and its
				      * copy.
				      */
    static const unsigned int quads_per_cell
    = (2*GeometryInfo<dim-1>::quads_per_cell +
       GeometryInfo<dim-1>::lines_per_cell);

				     /**
				      * Number of hexahedra of a
				      * cell.
				      */
    static const unsigned int hexes_per_cell
    = (2*GeometryInfo<dim-1>::hexes_per_cell +
       GeometryInfo<dim-1>::quads_per_cell);

				     /**
				      * Rearrange vertices for UCD
				      * output.  For a cell being
				      * written in UCD format, each
				      * entry in this field contains
				      * the number of a vertex in
				      * <tt>deal.II</tt> that corresponds
				      * to the UCD numbering at this
				      * location.
				      *
				      * Typical example: write a cell
				      * and arrange the vertices, such
				      * that UCD understands them.
				      *
				      * \begin{verbatim}
				      * for (i=0; i< n_vertices; ++i)
				      *   out << cell->vertex(ucd_to_deal[i]);
				      * \end{verbatim}
				      *
				      * As the vertex numbering in
				      * deal.II versions <= 5.1
				      * happened to coincide with the
				      * UCD numbering, this field can
				      * also be used like a
				      * old_to_lexicographic mapping.
				      */
    static const unsigned int ucd_to_deal[vertices_per_cell];

				     /**
				      * Rearrange vertices for OpenDX
				      * output.  For a cell being
				      * written in OpenDX format, each
				      * entry in this field contains
				      * the number of a vertex in
				      * <tt>deal.II</tt> that corresponds
				      * to the DX numbering at this
				      * location.
				      *
				      * Typical example: write a cell
				      * and arrange the vertices, such
				      * that OpenDX understands them.
				      *
				      * \begin{verbatim}
				      * for (i=0; i< n_vertices; ++i)
				      *   out << cell->vertex(dx_to_deal[i]);
				      * \end{verbatim}
				      */
    static const unsigned int dx_to_deal[vertices_per_cell];
    
				     /**
				      * This field stores which child
				      * cells are adjacent to a
				      * certain face of the mother
				      * cell.
				      *
				      * For example, in 2D the layout of
				      * a cell is as follows:
				      * @verbatim
				      * .      3
				      * .   2-->--3
				      * .   |     |
				      * . 0 ^     ^ 1
				      * .   |     |
				      * .   0-->--1
				      * .      2
				      * @endverbatim
				      * Vertices and faces are indicated
				      * with their numbers, faces also with
				      * their directions.
				      *
				      * Now, when refined, the layout is
				      * like this:
				      * @verbatim
				      * *--*--*
				      * | 2|3 |
				      * *--*--*
				      * | 0|1 |
				      * *--*--*
				      * @endverbatim
				      *
				      * Thus, the child cells on face
				      * 0 are (ordered in the
				      * direction of the face) 0 and
				      * 2, on face 3 they are 2 and 3,
				      * etc.
				      *
				      * For three spatial dimensions, the
				      * exact order of the children is laid
				      * down in the general documentation of
				      * this class. Through the
				      * <tt>face_orientation</tt> argument
				      * this function handles faces oriented
				      * in both, the standard and non-standard
				      * orientation.
				      * <tt>face_orientation</tt> defaults to
				      * <tt>true</tt> (standard orientation)
				      * and has no effect in 2d. The concept
				      * of face orientations is explained in
				      * this @ref GlossFaceOrientation "glossary"
				      * entry.
				      */
    static unsigned int child_cell_on_face (const unsigned int face,
					    const unsigned int subface,
					    const bool face_orientation = true);
    
				     /**
				      * Map line vertex number to cell
				      * vertex number, i.e. give the
				      * cell vertex number of the
				      * <tt>vertex</tt>th vertex of
				      * line <tt>line</tt>, e.g.
				      * <tt>GeometryInfo<2>::line_to_cell_vertices(3,0)=2</tt>.
				      *
				      * The order of the lines, as well as
				      * their direction (which in turn
				      * determines which is the first and
				      * which the second vertex on a line) is
				      * the canonical one in deal.II, as
				      * described in the general documentation
				      * of this class.
				      *
				      * For <tt>dim=2</tt> this call
				      * is simply passed down to the
				      * face_to_cell_vertices()
				      * function.
				      */
    static unsigned int line_to_cell_vertices (const unsigned int line,
					       const unsigned int vertex);

				     /**
				      * Map face vertex number to cell
				      * vertex number, i.e. give the
				      * cell vertex number of the
				      * <tt>vertex</tt>th vertex of
				      * face <tt>face</tt>, e.g.
				      * <tt>GeometryInfo<2>::face_to_cell_vertices(3,0)=2</tt>.
				      *
				      * Through the
				      * <tt>face_orientation</tt>
				      * argument this function handles
				      * faces oriented in both, the
				      * standard and non-standard
				      * orientation.
				      * <tt>face_orientation</tt>
				      * defaults to <tt>true</tt>
				      * (standard orientation) and has
				      * no effect in 2d.
				      *
				      * As the children of a cell are
				      * ordered according to the
				      * vertices of the cell, this
				      * call is passed down to the
				      * child_cell_on_face() function.
				      * Hence this function is simply
				      * a wrapper of
				      * child_cell_on_face() giving it
				      * a suggestive name.
				      */
    static unsigned int face_to_cell_vertices (const unsigned int face,
					       const unsigned int vertex,
					       const bool face_orientation = true);

				     /**
				      * Map face line number to cell
				      * line number, i.e. give the
				      * cell line number of the
				      * <tt>line</tt>th line of face
				      * <tt>face</tt>, e.g.
				      * <tt>GeometryInfo<3>::face_to_cell_lines(5,0)=4</tt>.
				      *
				      * Through the
				      * <tt>face_orientation</tt>
				      * argument this function handles
				      * faces oriented in both, the
				      * standard and non-standard
				      * orientation.
				      * <tt>face_orientation</tt>
				      * defaults to <tt>true</tt>
				      * (standard orientation) and has
				      * no effect in 2d.
				      */
    static unsigned int face_to_cell_lines (const unsigned int face,
					    const unsigned int line,
					    const bool face_orientation = true);
    
				     /**
				      * Return the position of the @p ith
				      * vertex on the unit cell. The order of
				      * vertices is the canonical one in
				      * deal.II, as described in the general
				      * documentation of this class.
				      */
    static Point<dim> unit_cell_vertex (const unsigned int vertex);

				     /**
				      * Given a point @p p in unit
				      * coordinates, return the number
				      * of the child cell in which it
				      * would lie in. If the point
				      * lies on the interface of two
				      * children, return any one of
				      * their indices. The result is
				      * always less than
				      * GeometryInfo<dimension>::children_per_cell.
				      *
				      * The order of child cells is described
				      * the general documentation of this
				      * class.
				      */
    static unsigned int child_cell_from_point (const Point<dim> &p);

				     /**
				      * Given coordinates @p p on the
				      * unit cell, return the values
				      * of the coordinates of this
				      * point in the coordinate system
				      * of the given child. Neither
				      * original nor returned
				      * coordinates need actually be
				      * inside the cell, we simply
				      * perform a scale-and-shift
				      * operation with a shift that
				      * depends on the number of the
				      * child.
				      */
    static Point<dim> cell_to_child_coordinates (const Point<dim>    &p,
						 const unsigned int child_index);

				     /**
				      * The reverse function to the
				      * one above: take a point in the
				      * coordinate system of the
				      * child, and transform it to the
				      * coordinate system of the
				      * mother cell.
				      */
    static Point<dim> child_to_cell_coordinates (const Point<dim>    &p,
						 const unsigned int child_index);

				     /**
				      * Return true if the given point
				      * is inside the unit cell of the
				      * present space dimension.
				      */
    static bool is_inside_unit_cell (const Point<dim> &p);
    
				     /**
				      * For each face of the reference
				      * cell, this field stores the
				      * coordinate direction in which
				      * its normal vector points. In
				      * <tt>dim</tt> dimension these
				      * are the <tt>2*dim</tt> first
				      * entries of
				      * <tt>{0,0,1,1,2,2,3,3}</tt>.
				      *
				      * Note that this is only the
				      * coordinate number. The actual
				      * direction of the normal vector
				      * is obtained by multiplying the
				      * unit vector in this direction
				      * with #unit_normal_orientation.
				      */
    static const unsigned int unit_normal_direction[faces_per_cell];

				     /**
				      * Orientation of the unit normal
				      * vector of a face of the
				      * reference cell. In
				      * <tt>dim</tt> dimension these
				      * are the <tt>2*dim</tt> first
				      * entries of
				      * <tt>{-1,1,-1,1,-1,1,-1,1}</tt>.
				      *
				      * Each value is either
				      * <tt>1</tt> or <tt>-1</tt>,
				      * corresponding to a normal
				      * vector pointing in the
				      * positive or negative
				      * coordinate direction,
				      * respectively.
				      *
				      * Note that this is only the
				      * <em>standard orientation</em>
				      * of faces. At least in 3d,
				      * actual faces of cells in a
				      * triangulation can also have
				      * the opposite orientation,
				      * depending on a flag that one
				      * can query from the cell it
				      * belongs to. For more
				      * information, see the
				      * @ref GlossFaceOrientation "glossary"
				      * entry on
				      * face orientation.
				      */
    static const int unit_normal_orientation[faces_per_cell];

				     /**
				      * List of numbers which denotes which
				      * face is opposite to a given face. Its
				      * entries are the first <tt>2*dim</tt>
				      * entries of
				      * <tt>{ 1, 0, 3, 2, 5, 4, 7, 6}</tt>.
				      */
    static const unsigned int opposite_face[faces_per_cell];


                                     /**
				      * Exception
				      */
    DeclException1 (ExcInvalidCoordinate,
		    double,
		    << "The coordinates must satisfy 0 <= x_i <= 1, "
		    << "but here we have x_i=" << arg1);
};




#ifndef DOXYGEN


/* -------------- declaration of explicit specializations ------------- */

template <>
const unsigned int GeometryInfo<1>::unit_normal_direction[faces_per_cell];
template <>
const unsigned int GeometryInfo<2>::unit_normal_direction[faces_per_cell];
template <>
const unsigned int GeometryInfo<3>::unit_normal_direction[faces_per_cell];
template <>
const unsigned int GeometryInfo<4>::unit_normal_direction[faces_per_cell];

template <>
const int GeometryInfo<1>::unit_normal_orientation[faces_per_cell];
template <>
const int GeometryInfo<2>::unit_normal_orientation[faces_per_cell];
template <>
const int GeometryInfo<3>::unit_normal_orientation[faces_per_cell];
template <>
const int GeometryInfo<4>::unit_normal_orientation[faces_per_cell];

template <>
const unsigned int GeometryInfo<1>::opposite_face[faces_per_cell];
template <>
const unsigned int GeometryInfo<2>::opposite_face[faces_per_cell];
template <>
const unsigned int GeometryInfo<3>::opposite_face[faces_per_cell];
template <>
const unsigned int GeometryInfo<4>::opposite_face[faces_per_cell];


/* -------------- inline functions ------------- */


template <>
inline
Point<1>
GeometryInfo<1>::unit_cell_vertex (const unsigned int vertex)
{
  Assert (vertex < vertices_per_cell,
	  ExcIndexRange (vertex, 0, vertices_per_cell));

  return Point<1>(static_cast<double>(vertex));
}



template <>
inline
Point<2>
GeometryInfo<2>::unit_cell_vertex (const unsigned int vertex)
{
  Assert (vertex < vertices_per_cell,
	  ExcIndexRange (vertex, 0, vertices_per_cell));

  return Point<2>(vertex%2, vertex/2);
}



template <>
inline
Point<3>
GeometryInfo<3>::unit_cell_vertex (const unsigned int vertex)
{
  Assert (vertex < vertices_per_cell,
	  ExcIndexRange (vertex, 0, vertices_per_cell));

  return Point<3>(vertex%2, vertex/2%2, vertex/4);
}



template <int dim>
inline
Point<dim>
GeometryInfo<dim>::unit_cell_vertex (const unsigned int)
{
  Assert(false, ExcNotImplemented());

  return Point<dim> ();  
}



template <>
inline
unsigned int
GeometryInfo<1>::child_cell_from_point (const Point<1> &p)
{
  Assert ((p[0] >= 0) && (p[0] <= 1), ExcInvalidCoordinate(p[0]));
  
  return (p[0] <= 0.5 ? 0 : 1);
}



template <>
inline
unsigned int
GeometryInfo<2>::child_cell_from_point (const Point<2> &p)
{
  Assert ((p[0] >= 0) && (p[0] <= 1), ExcInvalidCoordinate(p[0]));
  Assert ((p[1] >= 0) && (p[1] <= 1), ExcInvalidCoordinate(p[1]));
  
  return (p[0] <= 0.5 ?
	  (p[1] <= 0.5 ? 0 : 2) :
	  (p[1] <= 0.5 ? 1 : 3));
}



template <>
inline
unsigned int
GeometryInfo<3>::child_cell_from_point (const Point<3> &p)
{
  Assert ((p[0] >= 0) && (p[0] <= 1), ExcInvalidCoordinate(p[0]));
  Assert ((p[1] >= 0) && (p[1] <= 1), ExcInvalidCoordinate(p[1]));
  Assert ((p[2] >= 0) && (p[2] <= 1), ExcInvalidCoordinate(p[2]));
  
  return (p[0] <= 0.5 ?
	  (p[1] <= 0.5 ?
	   (p[2] <= 0.5 ? 0 : 4) :
	   (p[2] <= 0.5 ? 2 : 6)) :
	  (p[1] <= 0.5 ?
	   (p[2] <= 0.5 ? 1 : 5) :
	   (p[2] <= 0.5 ? 3 : 7)));
}


template <int dim>
inline
unsigned int
GeometryInfo<dim>::child_cell_from_point (const Point<dim> &)
{
  Assert(false, ExcNotImplemented());

  return 0;
}



template <int dim>
inline
Point<dim>
GeometryInfo<dim>::cell_to_child_coordinates (const Point<dim>    &p,
					      const unsigned int child_index)
{
  Assert (child_index < GeometryInfo<dim>::children_per_cell,
	  ExcIndexRange (child_index, 0, GeometryInfo<dim>::children_per_cell));

  return 2*p - unit_cell_vertex(child_index);
}



template <int dim>
inline
Point<dim>
GeometryInfo<dim>::child_to_cell_coordinates (const Point<dim>    &p,
					      const unsigned int child_index)
{
  Assert (child_index < GeometryInfo<dim>::children_per_cell,
	  ExcIndexRange (child_index, 0, GeometryInfo<dim>::children_per_cell));

  return (p + unit_cell_vertex(child_index))/2;
}


template <>
inline
bool
GeometryInfo<1>::is_inside_unit_cell (const Point<1> &p)
{
  return (p[0] >= 0.) && (p[0] <= 1.);
}



template <>
inline
bool
GeometryInfo<2>::is_inside_unit_cell (const Point<2> &p)
{
  return (p[0] >= 0.) && (p[0] <= 1.) &&
	 (p[1] >= 0.) && (p[1] <= 1.);
}



template <>
inline
bool
GeometryInfo<3>::is_inside_unit_cell (const Point<3> &p)
{
  return (p[0] >= 0.) && (p[0] <= 1.) &&
	 (p[1] >= 0.) && (p[1] <= 1.) &&
	 (p[2] >= 0.) && (p[2] <= 1.);
}

#endif // DOXYGEN

#endif
