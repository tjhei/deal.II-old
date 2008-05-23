//---------------------------------------------------------------------------
//    $Id$
//    Version: $Name$
//
//    Copyright (C) 1998, 1999, 2000, 2001, 2002, 2003, 2004, 2005, 2006, 2007, 2008 by the deal.II authors
//
//    This file is subject to QPL and may not be  distributed
//    without copyright and license information. Please refer
//    to the file deal.II/doc/license.html for the  text  and
//    further information on this license.
//
//---------------------------------------------------------------------------
#ifndef __deal2__fe_h
#define __deal2__fe_h

#include <base/config.h>
#include <fe/fe_base.h>

DEAL_II_NAMESPACE_OPEN

template <int dim> class FEValuesData;
template <int dim> class FEValuesBase;
template <int dim> class FEValues;
template <int dim> class FEFaceValues;
template <int dim> class FESubfaceValues;
template <int dim> class FESystem;
namespace hp
{
  template <int dim> class FECollection;
}


/**
 * Base class for finite elements in arbitrary dimensions. This class
 * provides several fields which describe a specific finite element
 * and which are filled by derived classes. It more or less only
 * offers the fields and access functions which makes it possible to
 * copy finite elements without knowledge of the actual type (linear,
 * quadratic, etc). In particular, the functions to fill the data
 * fields of FEValues and its derived classes are declared.
 *
 * The interface of this class is very restrictive. The reason is that
 * finite element values should be accessed only by use of FEValues
 * objects. These, together with FiniteElement are responsible to
 * provide an optimized implementation.
 *
 * This class declares the shape functions and their derivatives on
 * the unit cell $[0,1]^d$. The means to transform them onto a given
 * cell in physical space is provided by the FEValues class with a
 * Mapping object.
 *
 * The different matrices are initialized with the correct size, such
 * that in the derived (concrete) finite element classes, their
 * entries only have to be filled in; no resizing is needed. If the
 * matrices are not defined by a concrete finite element, they should
 * be resized to zero. This way functions using them can find out,
 * that they are missing. On the other hand, it is possible to use
 * finite element classes without implementation of the full
 * functionality, if only part of it is needed. The functionality
 * under consideration here is hanging nodes constraints and grid
 * transfer, respectively.
 *
 * <h3>Components and blocks</h3>
 *
 * For vector valued elements shape functions may have nonzero entries
 * in one or several @ref GlossComponent "components" of the vector
 * valued function. If the element is @ref GlossPrimitive "primitive",
 * there is indeed a single component with a nonzero entry for each
 * shape function. This component can be determined by
 * system_to_component_index(), the number of components is
 * FiniteElementData::n_components().
 *
 * Furthermore, you may want to split your linear system into @ref
 * GlossBlock "blocks" for the use in BlockVector, BlockSparseMatrix,
 * BlockMatrixArray and so on. If you use non-primitive elements, you
 * cannot determine the block number by
 * system_to_component_index(). Instead, you can use
 * system_to_block_index(), which will automatically take care of the
 * additional components occupied by vector valued elements. The
 * number of generated blocks can be determined by
 * FiniteElementData::n_blocks().
 *
 * If you decide to operate by base element and multiplicity, the
 * function first_block_of_base() will be helpful.
 *
 * <h3>Support points</h3>
 *
 * Since a FiniteElement does not have information on the actual grid
 * cell, it can only provide @ref GlossSupport "support points" on the
 * unit cell. Support points on the actual grid cell must be computed
 * by mapping these points. The class used for this kind of operation
 * is FEValues. In most cases, code of the following type will serve
 * to provide the mapped support points.
 *
 * @code
 * Quadrature<dim> dummy_quadrature (fe.get_unit_support_points());
 * FEValues<dim>   fe_values (mapping, fe, dummy_quadrature,
 *                            update_quadrature_points);
 * fe_values.reinit (cell);
 * Point<dim>& mapped_point = fe_values.quadrature_point (i);
 * @endcode
 *
 * Alternatively, the points can be transformed one-by-one:
 * @code
 * const vector<Point<dim> >& unit_points =
 *    fe.get_unit_support_points();
 *
 * Point<dim> mapped_point =
 *    mapping.transform_unit_to_real_cell (cell, unit_points[i]);
 * @endcode
 * This is a shortcut, and as all shortcuts should be used cautiously.
 * If the mapping of all support points is needed, the first variant should
 * be preferred for efficiency.
 *
 * <h3>Notes on the implementation of derived classes</h3>
 *
 * The following sections list the information to be provided by
 * derived classes, depending on the space dimension. They are
 * followed by a list of functions helping to generate these values.
 *
 * <h4>Finite elements in one dimension</h4>
 *
 * Finite elements in one dimension need only set the #restriction
 * and #prolongation matrices. The constructor of this class in one
 * dimension presets the #interface_constraints matrix to have
 * dimension zero. Changing this behaviour in derived classes is
 * generally not a reasonable idea and you risk getting into trouble.
 * 
 * <h4>Finite elements in two dimensions</h4>
 * 
 * In addition to the fields already present in 1D, a constraint
 * matrix is needed, if the finite element has node values located on
 * edges or vertices. These constraints are represented by an $m\times
 * n$-matrix #interface_constraints, where <i>m</i> is the number of
 * degrees of freedom on the refined side without the corner vertices
 * (those dofs on the middle vertex plus those on the two lines), and
 * <i>n</i> is that of the unrefined side (those dofs on the two
 * vertices plus those on the line). The matrix is thus a rectangular
 * one. The $m\times n$ size of the #interface_constraints matrix can
 * also be accessed through the interface_constraints_size() function.
 *
 * The mapping of the dofs onto the indices of the matrix on the
 * unrefined side is as follows: let $d_v$ be the number of dofs on a
 * vertex, $d_l$ that on a line, then $n=0...d_v-1$ refers to the dofs
 * on vertex zero of the unrefined line, $n=d_v...2d_v-1$ to those on
 * vertex one, $n=2d_v...2d_v+d_l-1$ to those on the line.
 *
 * Similarly, $m=0...d_v-1$ refers to the dofs on the middle vertex of
 * the refined side (vertex one of child line zero, vertex zero of
 * child line one), $m=d_v...d_v+d_l-1$ refers to the dofs on child
 * line zero, $m=d_v+d_l...d_v+2d_l-1$ refers to the dofs on child
 * line one.  Please note that we do not need to reserve space for the
 * dofs on the end vertices of the refined lines, since these must be
 * mapped one-to-one to the appropriate dofs of the vertices of the
 * unrefined line.
 *
 * It should be noted that it is not possible to distribute a constrained
 * degree of freedom to other degrees of freedom which are themselves
 * constrained. Only one level of indirection is allowed. It is not known
 * at the time of this writing whether this is a constraint itself.
 *
 * 
 * <h4>Finite elements in three dimensions</h4>
 *
 * For the interface constraints, almost the same holds as for the 2D case.
 * The numbering for the indices $n$ on the mother face is obvious and keeps
 * to the usual numbering of degrees of freedom on quadrilaterals.
 *
 * The numbering of the degrees of freedom on the interior of the refined
 * faces for the index $m$ is as follows: let $d_v$ and $d_l$ be as above,
 * and $d_q$ be the number of degrees of freedom per quadrilateral (and
 * therefore per face), then $m=0...d_v-1$ denote the dofs on the vertex at
 * the center, $m=d_v...5d_v-1$ for the dofs on the vertices at the center
 * of the bounding lines of the quadrilateral,
 * $m=5d_v..5d_v+4*d_l-1$ are for the degrees of freedom on
 * the four lines connecting the center vertex to the outer boundary of the
 * mother face, $m=5d_v+4*d_l...5d_v+4*d_l+8*d_l-1$ for the degrees of freedom
 * on the small lines surrounding the quad,
 * and $m=5d_v+12*d_l...5d_v+12*d_l+4*d_q-1$ for the dofs on the
 * four child faces. Note the direction of the lines at the boundary of the
 * quads, as shown below.
 *
 * The order of the twelve lines and the four child faces can be extracted
 * from the following sketch, where the overall order of the different
 * dof groups is depicted:
 * @verbatim
 *    *--15--4--16--*
 *    |      |      |
 *    10 19  6  20  12
 *    |      |      |
 *    1--7---0--8---2
 *    |      |      |
 *    9  17  5  18  11
 *    |      |      |
 *    *--13--3--14--*
 * @endverbatim
 * The numbering of vertices and lines, as well as the numbering of
 * children within a line is consistent with the one described in
 * Triangulation. Therefore, this numbering is seen from the
 * outside and inside, respectively, depending on the face.
 *
 * The three-dimensional case has a few pitfalls available for derived classes
 * that want to implement constraint matrices. Consider the following case:
 * @verbatim
 *          *-------*
 *         /       /|
 *        /       / |
 *       /       /  |
 *      *-------*   |
 *      |       |   *-------*
 *      |       |  /       /|
 *      |   1   | /       / |
 *      |       |/       /  |
 *      *-------*-------*   |
 *      |       |       |   *
 *      |       |       |  /
 *      |   2   |   3   | /
 *      |       |       |/
 *      *-------*-------*
 * @endverbatim
 * Now assume that we want to refine cell 2. We will end up with two faces
 * with hanging nodes, namely the faces between cells 1 and 2, as well as
 * between cells 2 and 3. Constraints have to be applied to the degrees of
 * freedom on both these faces. The problem is that there is now an edge
 * (the top right one of cell 2) which is part of both faces. The hanging
 * node(s) on this edge are therefore constrained twice, once from both
 * faces. To be meaningful, these constraints of course have to be
 * consistent: both faces have to constrain the hanging nodes on the edge to
 * the same nodes on the coarse edge (and only on the edge, as there can
 * then be no constraints to nodes on the rest of the face), and they have
 * to do so with the same weights. This is sometimes tricky since the nodes
 * on the edge may have different local numbers.
 *
 * For the constraint matrix this means the following: if a degree of freedom
 * on one edge of a face is constrained by some other nodes on the same edge
 * with some weights, then the weights have to be exactly the same as those
 * for constrained nodes on the three other edges with respect to the
 * corresponding nodes on these edges. If this isn't the case, you will get
 * into trouble with the ConstraintMatrix class that is the primary consumer
 * of the constraint information: while that class is able to handle
 * constraints that are entered more than once (as is necessary for the case
 * above), it insists that the weights are exactly the same.
 *
 * <h4>Helper functions</h4>
 *
 * Construction of a finite element and computation of the matrices
 * described above may be a tedious task, in particular if it has to
 * be performed for several space dimensions. Therefore, some
 * functions in FETools have been provided to help with these tasks.
 *
 * <h5>Computing the correct basis from "raw" basis functions</h5>
 *
 * First, aready the basis of the shape function space may be
 * difficult to implement for arbitrary order and dimension. On the
 * other hand, if the @ref GlossNodes "node values" are given, then
 * the duality relation between node functionals and basis functions
 * defines the basis. As a result, the shape function space may be
 * defined with arbitrary "raw" basis functions, such that the actual
 * finite element basis is computed from linear combinations of
 * them. The coefficients of these combinations are determined by the
 * duality of node values.
 *
 * Using this matrix allows the construction of the basis of shape
 * functions in two steps.
 * <ol>
 *
 * <li>Define the space of shape functions using an arbitrary basis
 * <i>w<sub>j</sub></i> and compute the matrix <i>M</i> of node
 * functionals <i>N<sub>i</sub></i> applied to these basis functions,
 * such that its entries are <i>m<sub>ij</sub> =
 * N<sub>i</sub>(w<sub>j</sub>)</i>.
 *
 * <li>Compute the basis <i>v<sub>j</sub></i> of the finite element
 * shape function space by applying <i>M<sup>-1</sup></i> to the basis
 * <i>w<sub>j</sub></i>.
 * </ol>
 *
 * The function computing the matrix <i>M</i> for you is
 * FETools::compute_node_matrix(). It relies on the existence of
 * #generalized_support_points and implementation of interpolate()
 * with VectorSlice argument.
 *
 * The piece of code in the constructor of a finite element
 * responsible for this looks like
 * @code
  FullMatrix<double> M(this->dofs_per_cell, this->dofs_per_cell);
  FETools::compute_node_matrix(M, *this);
  this->inverse_node_matrix.reinit(this->dofs_per_cell, this->dofs_per_cell);
  this->inverse_node_matrix.invert(M);
 * @endcode
 * Don't forget to make sure that #unit_support_points or
 * #generalized_support_points are initialized before this!
 *
 * <h5>Computing the #prolongation matrices for multigrid</h5>
 *
 * Once the shape functions are set up, the grid transfer matrices for
 * Multigrid accessed by get_prolongation_matrix() can be computed
 * automatically, using FETools::compute_embedding_matrices().
 *
 * This can be achieved by
 * @code
  for (unsigned int i=0; i<GeometryInfo<dim>::children_per_cell; ++i)
    this->prolongation[i].reinit (this->dofs_per_cell,
				  this->dofs_per_cell);
  FETools::compute_embedding_matrices (*this, &this->prolongation[0]);
 * @endcode
 *
 * <h5>Computing the #restriction matrices for error estimators</h5>
 *
 * missing...
 *
 * <h5>Computing #interface_constraints</h5>
 *
 * Constraint matrices can be computed semi-automatically using
 * FETools::compute_face_embedding_matrices(). This function computes
 * the representation of the coarse mesh functions by fine mesh
 * functions for each child of a face separately. These matrices must
 * be convoluted into a single rectangular constraint matrix,
 * eliminating degrees of freedom on common vertices and edges as well
 * as on the coarse grid vertices. See the discussion above for details.
 *
 * @ingroup febase fe
 * 
 * @author Wolfgang Bangerth, Guido Kanschat, Ralf Hartmann, 1998, 2000, 2001, 2005
 */
template <int dim>
class FiniteElement : public Subscriptor,
		      public FiniteElementData<dim>
{
  public:
				   /**
				    * Base class for internal data.
				    * Adds data for second derivatives to
				    * Mapping::InternalDataBase()
				    *
				    * For information about the
				    * general purpose of this class,
				    * see the documentation of the
				    * base class.
				    *
				    * @author Guido Kanschat, 2001
				    */
  class InternalDataBase : public Mapping<dim>::InternalDataBase
    {
      public:      
					 /**
					  * Destructor. Needed to
					  * avoid memory leaks with
					  * difference quotients.
					  */
	virtual ~InternalDataBase ();

					 /**
					  * Initialize some pointers
					  * used in the computation of
					  * second derivatives by
					  * finite differencing of
					  * gradients.
					  */
	void initialize_2nd (const FiniteElement<dim> *element,
			     const Mapping<dim>       &mapping,
			     const Quadrature<dim>    &quadrature);
	
					 /**
					  * Storage for FEValues
					  * objects needed to
					  * approximate second
					  * derivatives.
					  *
					  * The ordering is <i>p+hx</i>,
					  * <i>p+hy</i>, <i>p+hz</i>,
					  * <i>p-hx</i>, <i>p-hy</i>,
					  * <i>p-hz</i>, where unused
					  * entries in lower dimensions
					  * are missing.
					  */
	std::vector<FEValues<dim>*> differences;
    };

  public:
				     /**
				      * Constructor
				      */
    FiniteElement (const FiniteElementData<dim> &fe_data,
		   const std::vector<bool>      &restriction_is_additive_flags,
		   const std::vector<std::vector<bool> > &nonzero_components);

				     /**
				      * Virtual destructor. Makes sure
				      * that pointers to this class
				      * are deleted properly.
				      */
    virtual ~FiniteElement ();
    
				     /**
				      * Return a string that uniquely
				      * identifies a finite
				      * element. The general
				      * convention is that this is the
				      * class name, followed by the
				      * space dimension in angle
				      * brackets, and the polynomial
				      * degree and whatever else is
				      * necessary in parentheses. For
				      * example, <tt>FE_Q<2>(3)</tt> is the
				      * value returned for a cubic
				      * element in 2d.
				      *
				      * Systems of elements have their
				      * own naming convention, see the
				      * FESystem class.
				      */
    virtual std::string get_name () const = 0;

				     /**
				      * This operator returns a
				      * reference to the present
				      * object if the argument given
				      * equals to zero. While this
				      * does not seem particularly
				      * useful, it is helpful in
				      * writing code that works with
				      * both ::DoFHandler and the hp
				      * version hp::DoFHandler, since
				      * one can then write code like
				      * this:
				      * @verbatim
				      *   dofs_per_cell
				      *     = dof_handler->get_fe()[cell->active_fe_index()].dofs_per_cell;
				      * @endverbatim
				      *
				      * This code doesn't work in both
				      * situations without the present
				      * operator because
				      * DoFHandler::get_fe() returns a
				      * finite element, whereas
				      * hp::DoFHandler::get_fe()
				      * returns a collection of finite
				      * elements that doesn't offer a
				      * <code>dofs_per_cell</code>
				      * member variable: one first has
				      * to select which finite element
				      * to work on, which is done
				      * using the
				      * operator[]. Fortunately,
				      * <code>cell-@>active_fe_index()</code>
				      * also works for non-hp classes
				      * and simply returns zero in
				      * that case. The present
				      * operator[] accepts this zero
				      * argument, by returning the
				      * finite element with index zero
				      * within its collection (that,
				      * of course, consists only of
				      * the present finite element
				      * anyway).
				      */
    const FiniteElement<dim> & operator[] (const unsigned int fe_index) const;
    
				     /**
				      * @name Shape function access
				      * @{
				      */
    
				     /**
				      * Return the value of the
				      * @p ith shape function at the
				      * point @p p. @p p is a point
				      * on the reference element. If
				      * the finite element is
				      * vector-valued, then return the
				      * value of the only non-zero
				      * component of the vector value
				      * of this shape function. If the
				      * shape function has more than
				      * one non-zero component (which
				      * we refer to with the term
				      * non-primitive), then derived
				      * classes implementing this
				      * function should throw an
				      * exception of type
				      * ExcShapeFunctionNotPrimitive. In
				      * that case, use the
				      * shape_value_component()
				      * function.
				      *
				      * An
				      * ExcUnitShapeValuesDoNotExist
				      * is thrown if the shape values
				      * of the FiniteElement under
				      * consideration depends on the
				      * shape of the cell in real
				      * space.
				      */
    virtual double shape_value (const unsigned int  i,
			        const Point<dim>   &p) const;

				     /**
				      * Just like for shape_value(),
				      * but this function will be
				      * called when the shape function
				      * has more than one non-zero
				      * vector component. In that
				      * case, this function should
				      * return the value of the
				      * @p component-th vector
				      * component of the @p ith shape
				      * function at point @p p.
				      */
    virtual double shape_value_component (const unsigned int i,
					  const Point<dim>   &p,
					  const unsigned int component) const;
    
				     /**
				      * Return the gradient of the
				      * @p ith shape function at the
				      * point @p p. @p p is a point
				      * on the reference element, and
				      * likewise the gradient is the
				      * gradient on the unit cell with
				      * respect to unit cell
				      * coordinates. If
				      * the finite element is
				      * vector-valued, then return the
				      * value of the only non-zero
				      * component of the vector value
				      * of this shape function. If the
				      * shape function has more than
				      * one non-zero component (which
				      * we refer to with the term
				      * non-primitive), then derived
				      * classes implementing this
				      * function should throw an
				      * exception of type
				      * ExcShapeFunctionNotPrimitive. In
				      * that case, use the
				      * shape_grad_component()
				      * function.
				      *
				      * An
				      * ExcUnitShapeValuesDoNotExist
				      * is thrown if the shape values
				      * of the FiniteElement under
				      * consideration depends on the
				      * shape of the cell in real
				      * space.
				      */
    virtual Tensor<1,dim> shape_grad (const unsigned int  i,
				      const Point<dim>   &p) const;

				     /**
				      * Just like for shape_grad(),
				      * but this function will be
				      * called when the shape function
				      * has more than one non-zero
				      * vector component. In that
				      * case, this function should
				      * return the gradient of the
				      * @p component-th vector
				      * component of the @p ith shape
				      * function at point @p p.
				      */
    virtual Tensor<1,dim> shape_grad_component (const unsigned int i,
						const Point<dim>   &p,
						const unsigned int component) const;

				     /**
				      * Return the tensor of second
				      * derivatives of the @p ith
				      * shape function at point @p p
				      * on the unit cell. The
				      * derivatives are derivatives on
				      * the unit cell with respect to
				      * unit cell coordinates. If
				      * the finite element is
				      * vector-valued, then return the
				      * value of the only non-zero
				      * component of the vector value
				      * of this shape function. If the
				      * shape function has more than
				      * one non-zero component (which
				      * we refer to with the term
				      * non-primitive), then derived
				      * classes implementing this
				      * function should throw an
				      * exception of type
				      * ExcShapeFunctionNotPrimitive. In
				      * that case, use the
				      * shape_grad_grad_component()
				      * function.
				      *
				      * An
				      * ExcUnitShapeValuesDoNotExist
				      * is thrown if the shape values
				      * of the FiniteElement under
				      * consideration depends on the
				      * shape of the cell in real
				      * space.
				      */
    virtual Tensor<2,dim> shape_grad_grad (const unsigned int  i,
					   const Point<dim>   &p) const;

				     /**
				      * Just like for shape_grad_grad(),
				      * but this function will be
				      * called when the shape function
				      * has more than one non-zero
				      * vector component. In that
				      * case, this function should
				      * return the gradient of the
				      * @p component-th vector
				      * component of the @p ith shape
				      * function at point @p p.
				      */
    virtual Tensor<2,dim> shape_grad_grad_component (const unsigned int i,
						     const Point<dim>   &p,
						     const unsigned int component) const;
				     /**
				      * Check for non-zero values on a
				      * face in order to optimize out
				      * matrix elements.
				      *
				      * This function returns
				      * @p true, if the shape
				      * function @p shape_index has
				      * non-zero values on the face
				      * @p face_index.
				      *
				      * A default implementation is
				      * provided in this basis class
				      * which always returns @p
				      * true. This is the safe way to
				      * go.
				      */
    virtual bool has_support_on_face (const unsigned int shape_index,
				      const unsigned int face_index) const;
    
				     //@}
				     /**
				      * @name Transfer and constraint matrices
				      * @{
				      */
    
				     /**
				      * Projection from a fine grid
				      * space onto a coarse grid
				      * space. If this projection
				      * operator is associated with a
				      * matrix @p P, then the
				      * restriction of this matrix
				      * @p P_i to a single child cell
				      * is returned here.
				      *
				      * The matrix @p P is the
				      * concatenation or the sum of
				      * the cell matrices @p P_i,
				      * depending on the
				      * #restriction_is_additive_flags. This
				      * distinguishes interpolation
				      * (concatenation) and projection
				      * with respect to scalar
				      * products (summation).
				      *
				      * Row and column indices are
				      * related to coarse grid and
				      * fine grid spaces,
				      * respectively, consistent with
				      * the definition of the
				      * associated operator.
				      *
				      * If projection matrices are not
				      * implemented in the derived
				      * finite element class, this
				      * function aborts with
				      * ExcProjectionVoid.
				      */
    const FullMatrix<double> &
    get_restriction_matrix (const unsigned int child) const;

				     /**
				      * Embedding matrix between grids.
				      * 
				      * The identity operator from a
				      * coarse grid space into a fine
				      * grid space is associated with
				      * a matrix @p P. The
				      * restriction of this matrix @p P_i to
				      * a single child cell is
				      * returned here.
				      *
				      * The matrix @p P is the
				      * concatenation, not the sum of
				      * the cell matrices
				      * @p P_i. That is, if the same
				      * non-zero entry <tt>j,k</tt> exists
				      * in in two different child
				      * matrices @p P_i, the value
				      * should be the same in both
				      * matrices and it is copied into
				      * the matrix @p P only once.
				      *
				      * Row and column indices are
				      * related to fine grid and
				      * coarse grid spaces,
				      * respectively, consistent with
				      * the definition of the
				      * associated operator.
				      *
				      * These matrices are used by
				      * routines assembling the
				      * prolongation matrix for
				      * multi-level methods.  Upon
				      * assembling the transfer matrix
				      * between cells using this
				      * matrix array, zero elements in
				      * the prolongation matrix are
				      * discarded and will not fill up
				      * the transfer matrix.
				      *
				      * If projection matrices are not
				      * implemented in the derived
				      * finite element class, this
				      * function aborts with
				      * ExcEmbeddingVoid. You can
				      * check whether this is the case
				      * by calling the
				      * prolongation_is_implemented().
				      */
    const FullMatrix<double> &
    get_prolongation_matrix (const unsigned int child) const;

                                     /**
                                      * Return whether this element implements
                                      * its prolongation matrices. The return
                                      * value also indicates whether a call to
                                      * the get_prolongation_matrix()
                                      * function will generate an error or
                                      * not.
                                      *
                                      * This function is mostly here in order
                                      * to allow us to write more efficient
                                      * test programs which we run on all
                                      * kinds of weird elements, and for which
                                      * we simply need to exclude certain
                                      * tests in case something is not
                                      * implemented. It will in general
                                      * probably not be a great help in
                                      * applications, since there is not much
                                      * one can do if one needs these features
                                      * and they are not implemented. This
                                      * function could be used to check
                                      * whether a call to
                                      * <tt>get_prolongation_matrix()</tt> will
                                      * succeed; however, one then still needs
                                      * to cope with the lack of information
                                      * this just expresses.
                                      */
    bool prolongation_is_implemented () const;

                                     /**
                                      * Return whether this element implements
                                      * its restriction matrices. The return
                                      * value also indicates whether a call to
                                      * the get_restriction_matrix()
                                      * function will generate an error or
                                      * not.
                                      *
                                      * This function is mostly here in order
                                      * to allow us to write more efficient
                                      * test programs which we run on all
                                      * kinds of weird elements, and for which
                                      * we simply need to exclude certain
                                      * tests in case something is not
                                      * implemented. It will in general
                                      * probably not be a great help in
                                      * applications, since there is not much
                                      * one can do if one needs these features
                                      * and they are not implemented. This
                                      * function could be used to check
                                      * whether a call to
                                      * <tt>get_restriction_matrix()</tt> will
                                      * succeed; however, one then still needs
                                      * to cope with the lack of information
                                      * this just expresses.
                                      */
    bool restriction_is_implemented () const;

				     /**
				      * Access the
				      * #restriction_is_additive_flags
				      * field. See there for more
				      * information on its contents.
				      *
				      * The index must be between zero
				      * and the number of shape
				      * functions of this element.
				      */
    bool restriction_is_additive (const unsigned int index) const;

    				     /**
				      * Return a readonly reference to
				      * the matrix which describes the
				      * constraints at the interface
				      * between a refined and an
				      * unrefined cell.
				      * 
				      * The matrix is obviously empty
				      * in only one space dimension,
				      * since there are no constraints
				      * then.
				      *
				      * Note that some finite elements
				      * do not (yet) implement hanging
				      * node constraints. If this is
				      * the case, then this function
				      * will generate an exception,
				      * since no useful return value
				      * can be generated. If you
				      * should have a way to live with
				      * this, then you might want to
				      * use the
				      * constraints_are_implemented()
				      * function to check up front
				      * whethehr this function will
				      * succeed or generate the
				      * exception.
				      */
    const FullMatrix<double> & constraints () const;

                                     /**
                                      * Return whether this element
                                      * implements its hanging node
                                      * constraints. The return value
                                      * also indicates whether a call
                                      * to the constraints() function
                                      * will generate an error or not.
                                      *
                                      * This function is mostly here
                                      * in order to allow us to write
                                      * more efficient test programs
                                      * which we run on all kinds of
                                      * weird elements, and for which
                                      * we simply need to exclude
                                      * certain tests in case hanging
                                      * node constraints are not
                                      * implemented. It will in
                                      * general probably not be a
                                      * great help in applications,
                                      * since there is not much one
                                      * can do if one needs hanging
                                      * node constraints and they are
                                      * not implemented. This function
                                      * could be used to check whether
                                      * a call to <tt>constraints()</tt>
                                      * will succeed; however, one
                                      * then still needs to cope with
                                      * the lack of information this
                                      * just expresses.
                                      */
    bool constraints_are_implemented () const;


                                     /**
                                      * Return whether this element
                                      * implements its hanging node
                                      * constraints in the new way,
				      * which has to be used to make
				      * elements "hp compatible".
				      * That means, the element properly
				      * implements the
				      * get_face_interpolation_matrix
				      * and get_subface_interpolation_matrix
				      * methods. Therefore the return
				      * value also indicates whether a call
                                      * to the get_face_interpolation_matrix()
				      * method and the get_subface_interpolation_matrix()
				      * method will generate an error or not.
                                      *
				      * Currently the main purpose of this
				      * function is to allow the
				      * make_hanging_node_constraints method
				      * to decide whether the new procedures,
				      * which are supposed to work in the hp
				      * framework can be used, or if the old
				      * well verified but not hp capable
				      * functions should be used.  Once the
				      * transition to the new scheme for
				      * computing the interface constraints is
				      * complete, this function will be
				      * superfluous and will probably go away.
				      *
				      * Derived classes should implement this
				      * function accordingly. The default
				      * assumption is that a finite element
				      * does not provide hp capable face
				      * interpolation, and the default
				      * implementation therefore returns @p
				      * false.
                                      */
    virtual bool hp_constraints_are_implemented () const;


				     /**
				      * Return the matrix
				      * interpolating from the given
				      * finite element to the present
				      * one. The size of the matrix is
				      * then #dofs_per_cell times
				      * <tt>source.#dofs_per_cell</tt>.
				      *
				      * Derived elements will have to
				      * implement this function. They
				      * may only provide interpolation
				      * matrices for certain source
				      * finite elements, for example
				      * those from the same family. If
				      * they don't implement
				      * interpolation from a given
				      * element, then they must throw
				      * an exception of type
				      * ExcInterpolationNotImplemented.
				      */
    virtual void
    get_interpolation_matrix (const FiniteElement<dim> &source,
			      FullMatrix<double>       &matrix) const;
				     //@}

				     /**
				      * @name Functions to support hp 
				      * @{
				      */
    

				     /**
				      * Return the matrix
				      * interpolating from a face of
				      * of one element to the face of
				      * the neighboring element. 
				      * The size of the matrix is
				      * then <tt>source.#dofs_per_face</tt> times
				      * <tt>this->#dofs_per_face</tt>.
				      *
				      * Derived elements will have to
				      * implement this function. They
				      * may only provide interpolation
				      * matrices for certain source
				      * finite elements, for example
				      * those from the same family. If
				      * they don't implement
				      * interpolation from a given
				      * element, then they must throw
				      * an exception of type
				      * ExcInterpolationNotImplemented.
				      */
    virtual void
    get_face_interpolation_matrix (const FiniteElement<dim> &source,
				   FullMatrix<double>       &matrix) const;
    

				     /**
				      * Return the matrix
				      * interpolating from a face of
				      * of one element to the subface of
				      * the neighboring element. 
				      * The size of the matrix is
				      * then <tt>source.#dofs_per_face</tt> times
				      * <tt>this->#dofs_per_face</tt>.
				      *
				      * Derived elements will have to
				      * implement this function. They
				      * may only provide interpolation
				      * matrices for certain source
				      * finite elements, for example
				      * those from the same family. If
				      * they don't implement
				      * interpolation from a given
				      * element, then they must throw
				      * an exception of type
				      * ExcInterpolationNotImplemented.
				      */
    virtual void
    get_subface_interpolation_matrix (const FiniteElement<dim> &source,
				      const unsigned int        subface,
				      FullMatrix<double>       &matrix) const;
				     //@}
    

				     /**
				      * If, on a vertex, several
				      * finite elements are active,
				      * the hp code first assigns the
				      * degrees of freedom of each of
				      * these FEs different global
				      * indices. It then calls this
				      * function to find out which of
				      * them should get identical
				      * values, and consequently can
				      * receive the same global DoF
				      * index. This function therefore
				      * returns a list of identities
				      * between DoFs of the present
				      * finite element object with the
				      * DoFs of @p fe_other, which is
				      * a reference to a finite
				      * element object representing
				      * one of the other finite
				      * elements active on this
				      * particular vertex. The
				      * function computes which of the
				      * degrees of freedom of the two
				      * finite element objects are
				      * equivalent, and returns a list
				      * of pairs of global dof indices
				      * in @p identities. The first
				      * index of each pair denotes one
				      * of the vertex dofs of the
				      * present element, whereas the
				      * second is the corresponding
				      * index of the other finite
				      * element.
				      */
    virtual
    std::vector<std::pair<unsigned int, unsigned int> >
    hp_vertex_dof_identities (const FiniteElement<dim> &fe_other) const;

				     /**
				      * Same as
				      * hp_vertex_dof_indices(),
				      * except that the function
				      * treats degrees of freedom on
				      * lines.
				      */
    virtual
    std::vector<std::pair<unsigned int, unsigned int> >
    hp_line_dof_identities (const FiniteElement<dim> &fe_other) const;

				     /**
				      * Same as
				      * hp_vertex_dof_indices(),
				      * except that the function
				      * treats degrees of freedom on
				      * quads.
				      */
    virtual
    std::vector<std::pair<unsigned int, unsigned int> >
    hp_quad_dof_identities (const FiniteElement<dim> &fe_other) const;

				     /**
				      * Return whether this element dominates
				      * the one given as argument when they
				      * meet at a common face,
				      * whether it is the other way around,
				      * whether neither dominates, or if
				      * either could dominate.
				      *
				      * For a definition of domination, see
				      * FiniteElementBase::Domination and in
				      * particular the @ref hp_paper "hp paper".
				      */
    virtual
    FiniteElementDomination::Domination
    compare_for_face_domination (const FiniteElement<dim> &fe_other) const;
    
				     //@}
    
				     /**
				      * Comparison operator. We also
				      * check for equality of the
				      * constraint matrix, which is
				      * quite an expensive operation.
				      * Do therefore use this function
				      * with care, if possible only
				      * for debugging purposes.
				      *
				      * Since this function is not
				      * that important, we avoid an
				      * implementational question
				      * about comparing arrays and do
				      * not compare the matrix arrays
				      * #restriction and
				      * #prolongation.
				      */
    bool operator == (const FiniteElement<dim> &) const;

				     /**
				      * @name Index computations
				      * @{
				      */
				     /**
				      * Compute vector component and
				      * index of this shape function
				      * within the shape functions
				      * corresponding to this
				      * component from the index of a
				      * shape function within this
				      * finite element.
				      *
				      * If the element is scalar, then
				      * the component is always zero,
				      * and the index within this
				      * component is equal to the
				      * overall index.
				      *
				      * If the shape function
				      * referenced has more than one
				      * non-zero component, then it
				      * cannot be associated with one
				      * vector component, and an
				      * exception of type
				      * ExcShapeFunctionNotPrimitive
				      * will be raised.
				      *
				      * Note that if the element is
				      * composed of other (base)
				      * elements, and a base element
				      * has more than one component
				      * but all its shape functions
				      * are primitive (i.e. are
				      * non-zero in only one
				      * component), then this mapping
				      * contains valid
				      * information. However, the
				      * index of a shape function of
				      * this element within one
				      * component (i.e. the second
				      * number of the respective entry
				      * of this array) does not
				      * indicate the index of the
				      * respective shape function
				      * within the base element (since
				      * that has more than one
				      * vector-component). For this
				      * information, refer to the
				      * #system_to_base_table field
				      * and the
				      * system_to_base_index()
				      * function.
				      *
				      * The use of this function is
				      * explained extensively in the
				      * @ref step_8 "step-8" and @ref
				      * step_20 "step-20" tutorial
				      * programs as well as in the
				      * @ref vector_valued module.
				      */
    std::pair<unsigned int, unsigned int>
    system_to_component_index (const unsigned int index) const;

				     /**
				      * Compute the shape function for
				      * the given vector component and
				      * index.
				      *
				      * If the element is scalar, then
				      * the component must be zero,
				      * and the index within this
				      * component is equal to the
				      * overall index.
				      *
				      * This is the opposite operation
				      * from the system_to_component_index()
				      * function.
				      */
   unsigned int component_to_system_index(const unsigned int component,
                                          const unsigned int index) const;
  
				     /**
				      * Same as
				      * system_to_component_index(),
				      * but do it for shape functions
				      * and their indices on a
				      * face. The range of allowed
				      * indices is therefore
				      * 0..#dofs_per_face.
				      *
				      * You will rarely need this
				      * function in application
				      * programs, since almost all
				      * application codes only need to
				      * deal with cell indices, not
				      * face indices. The function is
				      * mainly there for use inside
				      * the library.
				      */
    std::pair<unsigned int, unsigned int>
    face_system_to_component_index (const unsigned int index) const;

				     /**
				      * For faces with non-standard
				      * face_orientation in 3D, the dofs on
				      * faces (quads) have to be permuted in
				      * order to be combined with the correct
				      * shape functions. Given a local dof @p
				      * index on a quad, return the local index,
				      * if the face has non-standard
				      * face_orientation, face_flip or
				      * face_rotation. In 2D and 1D there is no
				      * need for permutation and consequently
				      * an exception is thrown.
				      */
    unsigned int adjust_quad_dof_index_for_face_orientation (const unsigned int index,
							     const bool face_orientation,
							     const bool face_flip,
							     const bool face_rotation) const;

				     /**
				      * For lines with non-standard
				      * line_orientation in 3D, the dofs on
				      * lines have to be permuted in order to be
				      * combined with the correct shape
				      * functions. Given a local dof @p index on
				      * a line, return the local index, if the
				      * line has non-standard
				      * line_orientation. In 2D and 1D there is
				      * no need for permutation, so the given
				      * index is simply returned.
				      */
    unsigned int adjust_line_dof_index_for_line_orientation (const unsigned int index,
							     const bool line_orientation) const;
    
				     /**
				      * Return in which of the vector
				      * components of this finite
				      * element the @p ith shape
				      * function is non-zero. The
				      * length of the returned array
				      * is equal to the number of
				      * vector components of this
				      * element.
				      *
				      * For most finite element
				      * spaces, the result of this
				      * function will be a vector with
				      * exactly one element being
				      * @p true, since for most
				      * spaces the individual vector
				      * components are independent. In
				      * that case, the component with
				      * the single zero is also the
				      * first element of what
				      * system_to_component_index()
				      * returns.
				      *
				      * Only for those
				      * spaces that couple the
				      * components, for example to
				      * make a shape function
				      * divergence free, will there be
				      * more than one @p true entry.
				      */
    const std::vector<bool> &
    get_nonzero_components (const unsigned int i) const;

				     /**
				      * Return in how many vector
				      * components the @p ith shape
				      * function is non-zero. This
				      * value equals the number of
				      * entries equal to @p true in
				      * the result of the
				      * get_nonzero_components()
				      * function.
				      *
				      * For most finite element
				      * spaces, the result will be
				      * equal to one. It is not equal
				      * to one only for those ansatz
				      * spaces for which vector-valued
				      * shape functions couple the
				      * individual components, for
				      * example in order to make them
				      * divergence-free.
				      */
    unsigned int
    n_nonzero_components (const unsigned int i) const;

				     /**
				      * Return whether the @p ith
				      * shape function is primitive in
				      * the sense that the shape
				      * function is non-zero in only
				      * one vector
				      * component. Non-primitive shape
				      * functions would then, for
				      * example, be those of
				      * divergence free ansatz spaces,
				      * in which the individual vector
				      * components are coupled.
				      *
				      * The result of the function is
				      * @p true if and only if the
				      * result of
				      * <tt>n_nonzero_components(i)</tt> is
				      * equal to one.
				      */
    bool
    is_primitive (const unsigned int i) const;

				     /**
				      * Return whether the entire
				      * finite element is primitive,
				      * in the sense that all its
				      * shape functions are
				      * primitive. If the finite
				      * element is scalar, then this
				      * is always the case.
				      *
				      * Since this is an extremely
				      * common operation, the result
				      * is cached in the
				      * #cached_primitivity
				      * variable which is computed in
				      * the constructor.
				      */
    bool
    is_primitive () const;
    
				     /**
				      * Number of base elements in a
				      * mixed discretization.
				      *
				      * Note that even for vector
				      * valued finite elements, the
				      * number of components needs not
				      * coincide with the number of
				      * base elements, since they may
				      * be reused. For example, if you
				      * create a FESystem with
				      * three identical finite element
				      * classes by using the
				      * constructor that takes one
				      * finite element and a
				      * multiplicity, then the number
				      * of base elements is still one,
				      * although the number of
				      * components of the finite
				      * element is equal to the
				      * multiplicity.
				      */
    virtual unsigned int n_base_elements () const = 0;

				     /**
				      * Access to base element
				      * objects. If the element is
				      * scalar, then
				      * #base_element(0) is
				      * @p this.
				      */
    virtual
    const FiniteElement<dim> &
    base_element (const unsigned int index) const = 0;

                                     /**
                                      * This index denotes how often
                                      * the base element @p index is
                                      * used in a composed element. If
                                      * the element is scalar, then
                                      * the result is always equal to
                                      * one. See the documentation for
                                      * the n_base_elements()
                                      * function for more details.
                                      */
    virtual
    unsigned int
    element_multiplicity (const unsigned int index) const = 0;
    
                                     /**
                                      * Return for shape function
                                      * @p index the base element it
                                      * belongs to, the number of the
                                      * copy of this base element
                                      * (which is between zero and the
                                      * multiplicity of this element),
                                      * and the index of this shape
                                      * function within this base
                                      * element.
                                      *
                                      * If the element is not composed of
				      * others, then base and instance
				      * are always zero, and the index
				      * is equal to the number of the
				      * shape function. If the element
				      * is composed of single
				      * instances of other elements
				      * (i.e. all with multiplicity
				      * one) all of which are scalar,
				      * then base values and dof
				      * indices within this element
				      * are equal to the
				      * #system_to_component_table. It
				      * differs only in case the
				      * element is composed of other
				      * elements and at least one of
				      * them is vector-valued itself.
				      *
				      * This function returns valid
				      * values also in the case of
				      * vector-valued
				      * (i.e. non-primitive) shape
				      * functions, in contrast to the
				      * system_to_component_index()
				      * function.
                                      */
    std::pair<std::pair<unsigned int, unsigned int>, unsigned int>
    system_to_base_index (const unsigned int index) const;

                                     /**
                                      * Same as
                                      * system_to_base_index(), but
                                      * for degrees of freedom located
                                      * on a face. The range of allowed
				      * indices is therefore
				      * 0..#dofs_per_face.
				      *
				      * You will rarely need this
				      * function in application
				      * programs, since almost all
				      * application codes only need to
				      * deal with cell indices, not
				      * face indices. The function is
				      * mainly there for use inside
				      * the library.
                                      */
    std::pair<std::pair<unsigned int, unsigned int>, unsigned int>
    face_system_to_base_index (const unsigned int index) const;

				     /**
				      * Given a base element number,
				      * return the first block of a
				      * BlockVector it would generate.
				      */
    unsigned int first_block_of_base (const unsigned int b) const;
    
 				     /**
				      * For each vector component,
				      * return which base
				      * element implements this
				      * component and which vector
				      * component in this base element
				      * this is. This information is
				      * only of interest for
				      * vector-valued finite elements
				      * which are composed of several
				      * sub-elements. In that case,
				      * one may want to obtain
				      * information about the element
				      * implementing a certain vector
				      * component, which can be done
				      * using this function and the
				      * FESystem::base_element()
				      * function.
				      *
				      * If this is a scalar finite
				      * element, then the return value
				      * is always equal to a pair of
				      * zeros.
				      */
    std::pair<unsigned int, unsigned int>
    component_to_base_index (const unsigned int component) const;
    
    
				     /**
				      * Return the base element for
				      * this block and the number of
				      * the copy of the base element.
				      */
    std::pair<unsigned int,unsigned int>
    block_to_base_index (const unsigned int block) const;
    
				     /**
				      * The vector block and the index
				      * inside the block for this
				      * shape function.
				      */
    std::pair<unsigned int,unsigned int>
    system_to_block_index (const unsigned int component) const;

				     /**
				      * The vector block for this
				      * component.
				      */
    unsigned int
    component_to_block_index (const unsigned int component) const;

				     //@}
    
				     /**
				      * @name Support points and interpolation
				      * @{
				      */
    
				     /**
				      * Return the support points of
				      * the trial functions on the
				      * unit cell, if the derived
				      * finite element defines some.
				      * Finite elements that allow
				      * some kind of interpolation
				      * operation usually have support
				      * points. On the other hand,
				      * elements that define their
				      * degrees of freedom by, for
				      * example, moments on faces, or
				      * as derivatives, don't have
				      * support points. In that case,
				      * the returned field is empty.
				      *
				      * If the finite element defines
				      * support points, then their
				      * number equals the number of
				      * degrees of freedom of the
				      * element.  The order of points
				      * in the array matches that
				      * returned by the
				      * <tt>cell->get_dof_indices</tt>
				      * function.
				      *
				      * See the class documentation
				      * for details on support points.
				      */
    const std::vector<Point<dim> > &
    get_unit_support_points () const;    

				     /**
				      * Return whether a finite
				      * element has defined support
				      * points. If the result is true,
				      * then a call to the
				      * get_unit_support_points()
				      * yields a non-empty array.
				      *
				      * The result may be false if an
				      * element is not defined by
				      * interpolating shape functions,
				      * for example by P-elements on
				      * quadrilaterals. It will
				      * usually only be true if the
				      * element constructs its shape
				      * functions by the requirement
				      * that they be one at a certain
				      * point and zero at all the
				      * points associated with the
				      * other shape functions.
				      *
				      * In composed elements (i.e. for
				      * the FESystem class, the
				      * result will be true if all all
				      * the base elements have defined
				      * support points.
				      */
    bool has_support_points () const;

                                     /**
                                      * Return the position of the
                                      * support point of the
                                      * @p indexth shape function. If
                                      * it does not exist, raise an
                                      * exception.
                                      *
                                      * The default implementation
                                      * simply returns the respective
                                      * element from the array you get
                                      * from
                                      * get_unit_support_points(),
                                      * but derived elements may
                                      * overload this function. In
                                      * particular, note that the
                                      * FESystem class overloads
                                      * it so that it can return the
                                      * support points of individual
                                      * base elements, of not all the
                                      * base elements define support
                                      * points. In this way, you can
                                      * still ask for certain support
                                      * points, even if
                                      * get_unit_support_points()
                                      * only returns an empty array.
                                      */
    virtual
    Point<dim>
    unit_support_point (const unsigned int index) const;
    
				     /**
				      * Return the support points of
				      * the trial functions on the
				      * unit face, if the derived
				      * finite element defines some.
				      * Finite elements that allow
				      * some kind of interpolation
				      * operation usually have support
				      * points. On the other hand,
				      * elements that define their
				      * degrees of freedom by, for
				      * example, moments on faces, or
				      * as derivatives, don't have
				      * support points. In that case,
				      * the returned field is empty
				      *
				      * Note that elements that have
				      * support points need not
				      * necessarily have some on the
				      * faces, even if the
				      * interpolation points are
				      * located physically on a
				      * face. For example, the
				      * discontinuous elements have
				      * interpolation points on the
				      * vertices, and for higher
				      * degree elements also on the
				      * faces, but they are not
				      * defined to be on faces since
				      * in that case degrees of
				      * freedom from both sides of a
				      * face (or from all adjacent
				      * elements to a vertex) would be
				      * identified with each other,
				      * which is not what we would
				      * like to have). Logically,
				      * these degrees of freedom are
				      * therefore defined to belong to
				      * the cell, rather than the face
				      * or vertex. In that case, the
				      * returned element would
				      * therefore have length zero.
				      *
				      * If the finite element defines
				      * support points, then their
				      * number equals the number of
				      * degrees of freedom on the face
				      * (#dofs_per_face). The order
				      * of points in the array matches
				      * that returned by the
				      * <tt>cell->get_dof_indices</tt>
				      * function.
				      *
				      * See the class documentation
				      * for details on support points.
				      */
    const std::vector<Point<dim-1> > &
    get_unit_face_support_points () const;    

				     /**
				      * Return whether a finite
				      * element has defined support
				      * points on faces. If the result
				      * is true, then a call to the
				      * get_unit_face_support_points()
				      * yields a non-empty array.
				      *
				      * For more information, see the
				      * documentation for the
				      * has_support_points()
				      * function.
				      */
    bool has_face_support_points () const;

                                     /**
                                      * The function corresponding to
                                      * the unit_support_point()
                                      * function, but for faces. See
                                      * there for more information.
                                      */
    virtual
    Point<dim-1>
    unit_face_support_point (const unsigned int index) const;
    
				     /**
				      * Return a support point vector
				      * for generalized interpolation.
				      */
    const std::vector<Point<dim> > &
    get_generalized_support_points () const;    

				     /**
				      * Returns <tt>true</tt> if the
				      * class provides nonempty
				      * vectors either from
				      * get_unit_support_points() or
				      * get_generalized_support_points().
				      */
    bool has_generalized_support_points () const;

				     /**
				      *
				      */
    const std::vector<Point<dim-1> > &
    get_generalized_face_support_points () const;

				     /**
				      * Return whether a finite
				      * element has defined
				      * generalized support
				      * points on faces. If the result
				      * is true, then a call to the
				      * get_generalized_face_support_points
				      * yields a non-empty array.
				      *
				      * For more information, see the
				      * documentation for the
				      * has_support_points()
				      * function.
				      */
    bool has_generalized_face_support_points () const;

				     /**
				      * Interpolate a set of scalar
				      * values, computed in the
				      * generalized support points.
				      *
				      * @note This function is
				      * implemented in
				      * FiniteElement for the case
				      * that the element has support
				      * points. In this case, the
				      * resulting coefficients are
				      * just the values in the suport
				      * points. All other elements
				      * must reimplement it.
				      */
    virtual void interpolate(std::vector<double>&       local_dofs,
			     const std::vector<double>& values) const;
      
				     /**
				      * Interpolate a set of vector
				      * values, computed in the
				      * generalized support points.
				      *
				      * Since a finite element often
				      * only interpolates part of a
				      * vector, <tt>offset</tt> is
				      * used to determine the first
				      * component of the vector to be
				      * interpolated. Maybe consider
				      * changing your data structures
				      * to use the next function.
				      */
    virtual void interpolate(std::vector<double>&                local_dofs,
			     const std::vector<Vector<double> >& values,
			     unsigned int offset = 0) const;
      
				     /**
				      * Interpolate a set of vector
				      * values, computed in the
				      * generalized support points.
				      */
    virtual void interpolate(
      std::vector<double>& local_dofs,
      const VectorSlice<const std::vector<std::vector<double> > >& values) const;
      
				     //@}
    
				     /**
				      * Determine an estimate for the
				      * memory consumption (in bytes)
				      * of this object.
				      *
				      * This function is made virtual,
				      * since finite element objects
				      * are usually accessed through
				      * pointers to their base class,
				      * rather than the class itself.
				      */
    virtual unsigned int memory_consumption () const;
				     /**
				      * Exception
				      *
				      * @ingroup Exceptions
				      */
    DeclException1 (ExcShapeFunctionNotPrimitive,
		    int,
		    << "The shape function with index " << arg1
		    << " is not primitive, i.e. it is vector-valued and "
		    << "has more than one non-zero vector component. This "
		    << "function cannot be called for these shape functions. "
		    << "Maybe you want to use the same function with the "
		    << "_component suffix?");
				     /**
				      * Exception
				      *
				      * @ingroup Exceptions
				      */
    DeclException0 (ExcFENotPrimitive);
				     /**
				      * Exception
				      *
				      * @ingroup Exceptions
				      */
    DeclException0 (ExcUnitShapeValuesDoNotExist);

				     /**
				      * Attempt to access support
				      * points of a finite element
				      * which is not Lagrangian.
				      *
				      * @ingroup Exceptions
				      */
    DeclException0 (ExcFEHasNoSupportPoints);

				     /**
				      * Attempt to access embedding
				      * matrices of a finite element
				      * which did not implement these
				      * matrices.
				      *
				      * @ingroup Exceptions
				      */
    DeclException0 (ExcEmbeddingVoid);
    
				     /**
				      * Attempt to access restriction
				      * matrices of a finite element
				      * which did not implement these
				      * matrices.
				      *
				      * Exception
				      * @ingroup Exceptions
				      */
    DeclException0 (ExcProjectionVoid);
    
				     /**
				      * Attempt to access constraint
				      * matrices of a finite element
				      * which did not implement these
				      * matrices.
				      *
				      * Exception
				      * @ingroup Exceptions
				      */
    DeclException0 (ExcConstraintsVoid);
    
				     /**
				      * Exception
				      * @ingroup Exceptions
				      */
    DeclException2 (ExcWrongInterfaceMatrixSize,
		    int, int,
		    << "The interface matrix has a size of " << arg1
		    << "x" << arg2
		    << ", which is not reasonable in the present dimension.");
				     /**
				      * Exception
				      * @ingroup Exceptions
				      */
    DeclException2 (ExcComponentIndexInvalid,
		    int, int,
		    << "The component-index pair (" << arg1 << ", " << arg2
		    << ") is invalid, i.e. non-existent");
                                     /**
                                      * Exception
				      * @ingroup Exceptions
                                      */
    DeclException0 (ExcInterpolationNotImplemented);
    
				     /**
				      * Exception
				      *
				      * @ingroup Exceptions
				      */
    DeclException0 (ExcBoundaryFaceUsed);
				     /**
				      * Exception
				      *
				      * @ingroup Exceptions
				      */
    DeclException0 (ExcJacobiDeterminantHasWrongSign);
    
  protected:
				     /**
				      * Store whether all shape
				      * functions are primitive. Since
				      * finding this out is a very
				      * common operation, we cache the
				      * result, i.e. compute the value
				      * in the constructor for simpler
				      * access.
				      */
    const bool cached_primitivity;

 				     /**
				      * Array of projection
				      * matrices. See
				      * get_restriction_matrix()
				      * above. The constructor
				      * initializes these matrices to
				      * zero dimensions, which can be
				      * changed by derived classes
				      * implementing them.
				      */
    FullMatrix<double> restriction[GeometryInfo<dim>::children_per_cell];

    				     /**
				      * Array of embedding
				      * matrices. See
				      * <tt>get_prolongation_matrix()</tt>
				      * above. The constructor
				      * initializes these matrices to
				      * zero dimensions, which can be
				      * changed by derived classes
				      * implementing them.
				      */
    FullMatrix<double> prolongation[GeometryInfo<dim>::children_per_cell];

   				     /**
				      * Specify the constraints which
				      * the dofs on the two sides of a
				      * cell interface underly if the
				      * line connects two cells of
				      * which one is refined once.
				      *
				      * For further details see the
				      * general description of the
				      * derived class.
				      *
				      * This field is obviously
				      * useless in one space dimension
				      * and has there a zero size.
				      */
    FullMatrix<double> interface_constraints;

				     /**
				      * List of support points on the
				      * unit cell, in case the finite
				      * element has any. The
				      * constructor leaves this field
				      * empty, derived classes may
				      * write in some contents.
				      *
				      * Finite elements that allow
				      * some kind of interpolation
				      * operation usually have support
				      * points. On the other hand,
				      * elements that define their
				      * degrees of freedom by, for
				      * example, moments on faces, or
				      * as derivatives, don't have
				      * support points. In that case,
				      * this field remains empty.
				      */
    std::vector<Point<dim> > unit_support_points;

				     /**
				      * Same for the faces. See the
				      * description of the
				      * get_unit_face_support_points()
				      * function for a discussion of
				      * what contributes a face
				      * support point.
				      */
    std::vector<Point<dim-1> > unit_face_support_points;
    
				     /**
				      * Support points used for
				      * interpolation functions of
				      * non-Lagrangian elements.
				      */
    std::vector<Point<dim> > generalized_support_points;
    
				     /**
				      * Face support points used for
				      * interpolation functions of
				      * non-Lagrangian elements.
				      */    
    std::vector<Point<dim-1> > generalized_face_support_points;
    
				     /**
				      * For faces with non-standard
				      * face_orientation in 3D, the dofs on
				      * faces (quads) have to be permuted in
				      * order to be combined with the correct
				      * shape functions. Given a local dof @p
				      * index on a quad, return the shift in the
				      * local index, if the face has
				      * non-standard face_orientation,
				      * i.e. <code>old_index + shift =
				      * new_index</code>. In 2D and 1D there is
				      * no need for permutation so the vector is
				      * empty. In 3D it has the size of <code>
				      * #dofs_per_quad * 8 </code>, where 8 is
				      * the number of orientations, a face can
				      * be in (all comibinations of the three
				      * bool flags face_orientation, face_flip
				      * and face_rotation).
				      *
				      * The standard implementation fills this
				      * with zeros, i.e. no permuatation at
				      * all. Derived finite element classes have
				      * to fill this Table with the correct
				      * values.
				      */
    Table<2,int> adjust_quad_dof_index_for_face_orientation_table;

				     /**
				      * For lines with non-standard
				      * line_orientation in 3D, the dofs on
				      * lines have to be permuted in
				      * order to be combined with the correct
				      * shape functions. Given a local dof @p
				      * index on a line, return the shift in the
				      * local index, if the line has
				      * non-standard line_orientation,
				      * i.e. <code>old_index + shift =
				      * new_index</code>. In 2D and 1D there is
				      * no need for permutation so the vector is
				      * empty. In 3D it has the size of
				      * #dofs_per_line.
				      *
				      * The standard implementation fills this
				      * with zeros, i.e. no permuatation at
				      * all. Derived finite element classes have
				      * to fill this vector with the correct
				      * values.
				      */
    std::vector<int> adjust_line_dof_index_for_line_orientation_table;

                                     /**
                                      * Return the size of interface
                                      * constraint matrices. Since
                                      * this is needed in every
                                      * derived finite element class
                                      * when initializing their size,
                                      * it is placed into this
                                      * function, to avoid having to
                                      * recompute the
                                      * dimension-dependent size of
                                      * these matrices each time.
                                      *
                                      * Note that some elements do not
                                      * implement the interface
                                      * constraints for certain
                                      * polynomial degrees. In this
                                      * case, this function still
                                      * returns the size these
                                      * matrices should have when
                                      * implemented, but the actual
                                      * matrices are empty.
                                      */
    TableIndices<2>
    interface_constraints_size () const;

                                     /**
				      * Compute second derivatives by
				      * finite differences of
				      * gradients.
				      */
    void compute_2nd (const Mapping<dim>                      &mapping,
		      const typename Triangulation<dim>::cell_iterator    &cell,
		      const unsigned int                       offset,
		      typename Mapping<dim>::InternalDataBase &mapping_internal,
		      InternalDataBase                        &fe_internal,
		      FEValuesData<dim>                       &data) const;

				     /**
				      * Given the pattern of nonzero
				      * components for each shape
				      * function, compute for each
				      * entry how many components are
				      * non-zero for each shape
				      * function. This function is
				      * used in the constructor of
				      * this class.
				      */
    static
    std::vector<unsigned int>
    compute_n_nonzero_components (const std::vector<std::vector<bool> > &nonzero_components);

				     /**
				      * Determine the values a finite
				      * element should compute on
				      * initialization of data for
				      * FEValues.
				      *
				      * Given a set of flags
				      * indicating what quantities are
				      * requested from a FEValues
				      * object, update_once() and
				      * update_each() compute which
				      * values must really be
				      * computed. Then, the
				      * <tt>fill_*_values</tt> functions
				      * are called with the result of
				      * these.
				      *
				      * Furthermore, values must be
				      * computed either on the unit
				      * cell or on the physical
				      * cell. For instance, the
				      * function values of FE_Q do
				      * only depend on the quadrature
				      * points on the unit
				      * cell. Therefore, this flags
				      * will be returned by
				      * update_once(). The gradients
				      * require computation of the
				      * covariant transformation
				      * matrix. Therefore,
				      * @p update_covariant_transformation
				      * and @p update_gradients will
				      * be returned by
				      * update_each().
				      *
				      * For an example see the same
				      * function in the derived class
				      * FE_Q.
				      */
    virtual UpdateFlags update_once (const UpdateFlags flags) const = 0;
  
				     /**
				      * Complementary function for
				      * update_once().
				      *
				      * While update_once() returns
				      * the values to be computed on
				      * the unit cell for yielding the
				      * required data, this function
				      * determines the values that
				      * must be recomputed on each
				      * cell.
				      *
				      * Refer to update_once() for
				      * more details.
				      */
    virtual UpdateFlags update_each (const UpdateFlags flags) const = 0;
  
				     /**
				      * A sort of virtual copy
				      * constructor. Some places in
				      * the library, for example the
				      * constructors of FESystem as
				      * well as the hp::FECollection
				      * class, need to make copied of
				      * finite elements without
				      * knowing their exact type. They
				      * do so through this function.
				      */
    virtual FiniteElement<dim> *clone() const = 0;    
    
  private:
				     /**
				      * Store what
				      * system_to_component_index()
				      * will return.
				      */
    std::vector< std::pair<unsigned int, unsigned int> > system_to_component_table;

                                     /**
  				      * Map between linear dofs and
 				      * component dofs on face. This
 				      * is filled with default values
 				      * in the constructor, but
 				      * derived classes will have to
 				      * overwrite the information if
 				      * necessary.
 				      *
 				      * By component, we mean the
 				      * vector component, not the base
 				      * element. The information thus
 				      * makes only sense if a shape
 				      * function is non-zero in only
 				      * one component.
				      */
    std::vector< std::pair<unsigned int, unsigned int> > face_system_to_component_table;

				     /**
				      * For each shape function, store
				      * to which base element and
				      * which instance of this base
				      * element (in case its
				      * multiplicity is greater than
				      * one) it belongs, and its index
				      * within this base element. If
				      * the element is not composed of
				      * others, then base and instance
				      * are always zero, and the index
				      * is equal to the number of the
				      * shape function. If the element
				      * is composed of single
				      * instances of other elements
				      * (i.e. all with multiplicity
				      * one) all of which are scalar,
				      * then base values and dof
				      * indices within this element
				      * are equal to the
				      * #system_to_component_table. It
				      * differs only in case the
				      * element is composed of other
				      * elements and at least one of
				      * them is vector-valued itself.
				      *
				      * This array has valid values
				      * also in the case of
				      * vector-valued
				      * (i.e. non-primitive) shape
				      * functions, in contrast to the
				      * #system_to_component_table.
				      */
    std::vector<std::pair<std::pair<unsigned int,unsigned int>,unsigned int> >
    system_to_base_table;

				     /**
				      * Likewise for the indices on
				      * faces.
				      */
    std::vector<std::pair<std::pair<unsigned int,unsigned int>,unsigned int> >
    face_system_to_base_table;

				     /**
				      * For each base element, store
				      * the first block in a block
				      * vector it will generate.
				      */
    std::vector<unsigned int> first_block_of_base_table;
    
				     /**
				      * The base element establishing
				      * a component.
				      *
				      * For each component number
				      * <tt>c</tt>, the entries have
				      * the following meaning:
				      * <dl>
				      * <dt><tt>table[c].first.first</tt></dt>
				      * <dd>Number of the base element for <tt>c</tt>.</dd>
				      * <dt><tt>table[c].first.second</tt></dt>
				      * <dd>Component in the base element for <tt>c</tt>.</dd>
				      * <dt><tt>table[c].second</tt></dt>
				      * <dd>Multiple of the base element for <tt>c</tt>.</dd>
				      * </dl>
				      *
				      * This variable is set to the
				      * correct size by the
				      * constructor of this class, but
				      * needs to be initialized by
				      * derived classes, unless its
				      * size is one and the only entry
				      * is a zero, which is the case
				      * for scalar elements. In that
				      * case, the initialization by
				      * the base class is sufficient.
				      */
    std::vector<std::pair<std::pair<unsigned int, unsigned int>, unsigned int> >
    component_to_base_table;
    
				     /**
				      * Projection matrices are
				      * concatenated or summed up.
				      *
				      * This flags decides on how the
				      * projection matrices of the
				      * children of the same father
				      * are put together to one
				      * operator. The possible modes
				      * are concatenation and
				      * summation.
				      *
				      * If the projection is defined
				      * by an interpolation operator,
				      * the child matrices are
				      * concatenated, i.e. values
				      * belonging to the same node
				      * functional are identified and
				      * enter the interpolated value
				      * only once. In this case, the
				      * flag must be @p false.
				      *
				      * For projections with respect
				      * to scalar products, the child
				      * matrices must be summed up to
				      * build the complete matrix. The
				      * flag should be @p true.
				      *
				      * For examples of use of these
				      * flags, see the places in the
				      * library where it is queried.
				      * 
				      * There is one flag per shape
				      * function, indicating whether
				      * it belongs to the class of
				      * shape functions that are
				      * additive in the restriction or
				      * not.
				      *
				      * Note that in previous versions
				      * of the library, there was one
				      * flag per vector component of
				      * the element. This is based on
				      * the fact that all the shape
				      * functions that belong to the
				      * same vector component must
				      * necessarily behave in the same
				      * way, to make things
				      * reasonable. However, the
				      * problem is that it is
				      * sometimes impossible to query
				      * this flag in the vector-valued
				      * case: this used to be done
				      * with the
				      * #system_to_component_index
				      * function that returns which
				      * vector component a shape
				      * function is associated
				      * with. The point is that since
				      * we now support shape functions
				      * that are associated with more
				      * than one vector component (for
				      * example the shape functions of
				      * Raviart-Thomas, or Nedelec
				      * elements), that function can
				      * no more be used, so it can be
				      * difficult to find out which
				      * for vector component we would
				      * like to query the
				      * restriction-is-additive flags.
				      */
    const std::vector<bool> restriction_is_additive_flags;

				     /**
				      * For each shape function, give
				      * a vector of bools (with size
				      * equal to the number of vector
				      * components which this finite
				      * element has) indicating in
				      * which component each of these
				      * shape functions is non-zero.
				      *
				      * For primitive elements, there
				      * is only one non-zero
				      * component.
				      */
    const std::vector<std::vector<bool> > nonzero_components;

				     /**
				      * This array holds how many
				      * values in the respective entry
				      * of the #nonzero_components
				      * element are non-zero. The
				      * array is thus a short-cut to
				      * allow faster access to this
				      * information than if we had to
				      * count the non-zero entries
				      * upon each request for this
				      * information. The field is
				      * initialized in the constructor
				      * of this class.
				      */
    const std::vector<unsigned int> n_nonzero_components_table;

				     /**
				      * Second derivatives of shapes
				      * functions are not computed
				      * analytically, but by finite
				      * differences of the
				      * gradients. This static
				      * variable denotes the step
				      * length to be used for
				      * that. It's value is set to
				      * 1e-6.
				      */
    static const double fd_step_length;

				     /**
				      * Prepare internal data
				      * structures and fill in values
				      * independent of the
				      * cell. Returns a pointer to an
				      * object of which the caller of
				      * this function then has to
				      * assume ownership (which
				      * includes destruction when it
				      * is no more needed).
				      */
    virtual typename Mapping<dim>::InternalDataBase*
    get_data (const UpdateFlags      flags,
	      const Mapping<dim>    &mapping,
	      const Quadrature<dim> &quadrature) const = 0;

				     /**
				      * Prepare internal data
				      * structure for transformation
				      * of faces and fill in values
				      * independent of the
				      * cell. Returns a pointer to an
				      * object of which the caller of
				      * this function then has to
				      * assume ownership (which
				      * includes destruction when it
				      * is no more needed).
				      */
    virtual typename Mapping<dim>::InternalDataBase*
    get_face_data (const UpdateFlags        flags,
		   const Mapping<dim>      &mapping,
		   const Quadrature<dim-1> &quadrature) const;

				     /**
				      * Prepare internal data
				      * structure for transformation
				      * of children of faces and fill
				      * in values independent of the
				      * cell. Returns a pointer to an
				      * object of which the caller of
				      * this function then has to
				      * assume ownership (which
				      * includes destruction when it
				      * is no more needed).
				      */
    virtual typename Mapping<dim>::InternalDataBase*
    get_subface_data (const UpdateFlags        flags,
		      const Mapping<dim>      &mapping,
		      const Quadrature<dim-1> &quadrature) const;

				     /**
				      * Fill the fields of
				      * FEValues. This function
				      * performs all the operations
				      * needed to compute the data of an
				      * FEValues object.
				      *
				      * The same function in
				      * @p mapping must have been
				      * called for the same cell first!
				      */				      
    virtual void
    fill_fe_values (const Mapping<dim>                   &mapping,
		    const typename Triangulation<dim>::cell_iterator &cell,
		    const Quadrature<dim>                &quadrature,
		    typename Mapping<dim>::InternalDataBase       &mapping_internal,
		    typename Mapping<dim>::InternalDataBase       &fe_internal,
		    FEValuesData<dim>                    &data) const = 0;
    
				     /**
				      * Fill the fields of
				      * FEFaceValues. This function
				      * performs all the operations
				      * needed to compute the data of an
				      * FEFaceValues object.
				      *
				      * The same function in
				      * @p mapping must have been
				      * called for the same cell first!
				      */				      
    virtual void
    fill_fe_face_values (const Mapping<dim>                   &mapping,
			 const typename Triangulation<dim>::cell_iterator &cell,
			 const unsigned int                    face_no,
			 const Quadrature<dim-1>              &quadrature,
			 typename Mapping<dim>::InternalDataBase       &mapping_internal,
			 typename Mapping<dim>::InternalDataBase       &fe_internal,
			 FEValuesData<dim>                    &data) const = 0;
    
				     /**
				      * Fill the fields of
				      * FESubfaceValues. This function
				      * performs all the operations
				      * needed to compute the data of an
				      * FESubfaceValues object.
				      *
				      * The same function in
				      * @p mapping must have been
				      * called for the same cell first!
				      */				      
    virtual void
    fill_fe_subface_values (const Mapping<dim>                   &mapping,
			    const typename Triangulation<dim>::cell_iterator &cell,
			    const unsigned int                    face_no,
			    const unsigned int                    sub_no,
			    const Quadrature<dim-1>              &quadrature,
			    typename Mapping<dim>::InternalDataBase &mapping_internal,
			    typename Mapping<dim>::InternalDataBase &fe_internal,
			    FEValuesData<dim>                    &data) const = 0;

    friend class InternalDataBase;
    friend class FEValuesBase<dim>;
    friend class FEValues<dim>;
    friend class FEFaceValues<dim>;
    friend class FESubfaceValues<dim>;
    template <int dim_> friend class FESystem;
    friend class hp::FECollection<dim>;
};


//----------------------------------------------------------------------//


template <int dim>
inline
const FiniteElement<dim> &
FiniteElement<dim>::operator[] (const unsigned int fe_index) const
{
  Assert (fe_index == 0,
	  ExcMessage ("A fe_index of zero is the only index allowed here"));
  return *this;
}



template <int dim>  
inline
std::pair<unsigned int,unsigned int>
FiniteElement<dim>::system_to_component_index (const unsigned int index) const
{
  Assert (index < system_to_component_table.size(),
	 ExcIndexRange(index, 0, system_to_component_table.size()));
  Assert (is_primitive (index),
	  typename FiniteElement<dim>::ExcShapeFunctionNotPrimitive(index));
  return system_to_component_table[index];
}



template <int dim>  
inline
unsigned int
FiniteElement<dim>::component_to_system_index (const unsigned int component,
                                                   const unsigned int index) const
{
   std::vector< std::pair<unsigned int, unsigned int> >::const_iterator
      it = std::find(system_to_component_table.begin(), system_to_component_table.end(),
                     std::pair<unsigned int, unsigned int>(component, index));

   Assert(it != system_to_component_table.end(), ExcComponentIndexInvalid(component, index));
   return std::distance(system_to_component_table.begin(), it);
}



template <int dim>  
inline
std::pair<unsigned int,unsigned int>
FiniteElement<dim>::face_system_to_component_index (const unsigned int index) const
{
  Assert(index < face_system_to_component_table.size(),
	 ExcIndexRange(index, 0, face_system_to_component_table.size()));

                                   // in debug mode, check whether the
                                   // function is primitive, since
                                   // otherwise the result may have no
                                   // meaning
                                   //
                                   // since the primitivity tables are
                                   // all geared towards cell dof
                                   // indices, rather than face dof
                                   // indices, we have to work a
                                   // little bit...
                                   //
                                   // in 1d, the face index is equal
                                   // to the cell index
  Assert (is_primitive(this->face_to_equivalent_cell_index(index)),
          typename FiniteElement<dim>::ExcShapeFunctionNotPrimitive(index));

  return face_system_to_component_table[index];
}



template <int dim>  
inline
std::pair<std::pair<unsigned int,unsigned int>,unsigned int>
FiniteElement<dim>::system_to_base_index (const unsigned int index) const
{
  Assert (index < system_to_base_table.size(),
	 ExcIndexRange(index, 0, system_to_base_table.size()));
  return system_to_base_table[index];
}




template <int dim>  
inline
std::pair<std::pair<unsigned int,unsigned int>,unsigned int>
FiniteElement<dim>::face_system_to_base_index (const unsigned int index) const
{
  Assert(index < face_system_to_base_table.size(),
	 ExcIndexRange(index, 0, face_system_to_base_table.size()));
  return face_system_to_base_table[index];
}



template <int dim>  
inline
unsigned int
FiniteElement<dim>::first_block_of_base (const unsigned int index) const
{
  Assert(index < first_block_of_base_table.size(),
	 ExcIndexRange(index, 0, first_block_of_base_table.size()));

  return first_block_of_base_table[index];
}



template <int dim>  
inline
std::pair<unsigned int,unsigned int>
FiniteElement<dim>::component_to_base_index (const unsigned int index) const
{
  Assert(index < component_to_base_table.size(),
	 ExcIndexRange(index, 0, component_to_base_table.size()));

  return component_to_base_table[index].first;
}



template <int dim>  
inline
std::pair<unsigned int,unsigned int>
FiniteElement<dim>::block_to_base_index (const unsigned int index) const
{
  Assert(index < this->n_blocks(),
	 ExcIndexRange(index, 0, this->n_blocks()));
  
  for (int i=first_block_of_base_table.size()-1; i>=0; --i)
    if (first_block_of_base_table[i] <= index)
      return std::pair<unsigned int, unsigned int>(static_cast<unsigned int> (i),
						   index - first_block_of_base_table[i]);
  return std::make_pair(numbers::invalid_unsigned_int,
			numbers::invalid_unsigned_int);
}



template <int dim>  
inline
std::pair<unsigned int,unsigned int>
FiniteElement<dim>::system_to_block_index (const unsigned int index) const
{
  Assert (index < this->dofs_per_cell,
	  ExcIndexRange(index, 0, this->dofs_per_cell));
				   // The block is computed simply as
				   // first block of this base plus
				   // the index within the base blocks
  return std::pair<unsigned int, unsigned int>(
     first_block_of_base(system_to_base_table[index].first.first)
     + system_to_base_table[index].first.second,
     system_to_base_table[index].second);
}



template <int dim>
inline
bool
FiniteElement<dim>::restriction_is_additive (const unsigned int index) const
{
  Assert(index < this->dofs_per_cell,
	 ExcIndexRange(index, 0, this->dofs_per_cell));
  return restriction_is_additive_flags[index];
}



template <int dim>
inline
const std::vector<bool> &
FiniteElement<dim>::get_nonzero_components (const unsigned int i) const
{
  Assert (i < this->dofs_per_cell, ExcIndexRange (i, 0, this->dofs_per_cell));
  return nonzero_components[i];
}



template <int dim>
inline
unsigned int
FiniteElement<dim>::n_nonzero_components (const unsigned int i) const
{
  Assert (i < this->dofs_per_cell, ExcIndexRange (i, 0, this->dofs_per_cell));
  return n_nonzero_components_table[i];
}



template <int dim>
inline
bool
FiniteElement<dim>::is_primitive (const unsigned int i) const
{
  Assert (i < this->dofs_per_cell, ExcIndexRange (i, 0, this->dofs_per_cell));

				   // return primitivity of a shape
				   // function by checking whether it
				   // has more than one non-zero
				   // component or not. we could cache
				   // this value in an array of bools,
				   // but accessing a bit-vector (as
				   // std::vector<bool> is) is
				   // probably more expensive than
				   // just comparing against 1
				   //
				   // for good measure, short circuit the test
				   // if the entire FE is primitive
  return ((cached_primitivity == true)
	  ||
	  (n_nonzero_components_table[i] == 1));
}



template <int dim>
inline
bool
FiniteElement<dim>::is_primitive () const
{
  return cached_primitivity;
}


DEAL_II_NAMESPACE_CLOSE

#endif
