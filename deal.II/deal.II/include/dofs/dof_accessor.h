/*----------------------------   dof_iterator.h     ---------------------------*/
/*      $Id$                 */
/*      Copyright W. Bangerth, University of Heidelberg, 1998 */
#ifndef __dof_iterator_H
#define __dof_iterator_H
/*----------------------------   dof_iterator.h     ---------------------------*/


#include <grid/tria_accessor.h>
#include <lac/forward-declarations.h>
#include <vector>


// note: in non-debug mode, i.e. with optimizations, the file
// dof_accessor.templates.h is included at the end of this file.
// this includes a lot of templates and thus makes compilation
// slower, but at the same time allows for more aggressive
// inlining and thus faster code.



/**
 * Define the basis for accessors to the degrees of freedom.
 *
 * Note that it is allowed to construct an object of which the
 * #dof_handler# pointer is a Null pointer. Such an object would
 * result in a strange kind of behaviour, though every reasonable
 * operating system should disallow access through that pointer.
 * The reason we do not check for the null pointer in the
 * constructor which gets passed the #DoFHandler# pointer is that
 * if we did we could not make dof iterators member of other classes
 * (like in the #FEValues# class) if we did not know about the
 * #DoFHandler# object to be used upon construction of that object.
 * Through the way this class is implemented here, we allow the
 * creation of a kind of virgin object which only gets useful if
 * assigned to from another object before first usage.
 *
 * Opposite to construction, it is not possible to copy an object
 * which has an invalid dof handler pointer. This is to guarantee
 * that every iterator which is once assigned to is a valid
 * object. However, this assertion only holds in debug mode, when
 * the #Assert# macro is switched on.
 *
 * @author Wolfgang Bangerth, 1998
 */
template <int dim>
class DoFAccessor {
  public:
				     /**
				      * Constructor
				      */
    DoFAccessor () : dof_handler(0) {
      Assert (false, ExcInvalidObject());
    };

				     /**
				      * This should be the default constructor.
				      * We cast away the #const#ness of the
				      * pointer which clearly is EVIL but
				      * we can't help without making all
				      * functions which could somehow use
				      * iterators (directly or indirectly) make
				      * non-const, even if they preserve
				      * constness.
				      */
    DoFAccessor (const DoFHandler<dim> *dof_handler) :
		    dof_handler(const_cast<DoFHandler<dim>*>(dof_handler)) {};

				     /**
				      * Reset the DoF handler pointer.
				      */
    void set_dof_handler (DoFHandler<dim> *dh) {
      Assert (dh != 0, ExcInvalidObject());
      dof_handler = dh;
    };

				     /**
				      * Copy operator.
				      */
    DoFAccessor<dim> & operator = (const DoFAccessor<dim> &da) {
      set_dof_handler (da.dof_handler);
      return *this;
    };
    
				     /**
				      * Exception for child classes
				      */
    DeclException0 (ExcInvalidObject);
				     /**
				      * Exception
				      */
    DeclException0 (ExcVectorNotEmpty);
				     /**
				      * Exception
				      */
    DeclException0 (ExcVectorDoesNotMatch);
				     /**
				      * Exception
				      */
    DeclException0 (ExcMatrixDoesNotMatch);

  protected:
				     /**
				      * Store the address of the #DoFHandler# object
				      * to be accessed.
				      */
    DoFHandler<dim> *dof_handler;  
};




/* -------------------------------------------------------------------------- */

/**
 * This is a switch class which only declares a #typdef#. It is meant to
 * determine which class a #DoFObjectAccessor# class is to be derived
 * from. By default, #DoFObjectAccessor<celldim,dim># derives from
 * the #typedef# in the general #DoFObjectAccessor_Inheritance<celldim,dim>#
 * class, which is #TriaObjectAccessor<celldim,dim>#,
 * but if #celldim==dim#, then the specialization #DoFObjectAccessor_Inheritance<dim,dim>#
 * is used which declares its local type to be #CellAccessor<dim>#. Therefore,
 * the inheritance is automatically chosen to be from #CellAccessor# if the
 * object under consideration has full dimension, i.e. constitutes a cell.
 *
 * @author Wolfgang Bangerth, 1999
 */
template <int celldim, int dim>
class DoFObjectAccessor_Inheritance 
{
				     /**
				      * Declaration of the #typedef#.
				      * See the full documentation for
				      * more information.
				      */
    typedef TriaObjectAccessor<celldim,dim> BaseClass;
};



/**
 * This is a switch class which only declares a #typdef#. It is meant to
 * determine which class a #DoFObjectAccessor# class is to be derived
 * from. By default, #DoFObjectAccessor<celldim,dim># derives from
 * the #typedef# in the general #DoFObjectAccessor_Inheritance<celldim,dim>#
 * class, which is #TriaObjectAccessor<celldim,dim>#,
 * but if #celldim==dim#, then the specialization #DoFObjectAccessor_Inheritance<dim,dim>#
 * is used which declares its local type to be #CellAccessor<dim>#. Therefore,
 * the inheritance is automatically chosen to be from #CellAccessor# if the
 * object under consideration has full dimension, i.e. constitutes a cell.
 *
 * @author Wolfgang Bangerth, 1999
 */
template <int dim>
class DoFObjectAccessor_Inheritance<dim,dim>
{
				     /**
				      * Declaration of the #typedef#.
				      * See the full documentation for
				      * more information.
				      */
    typedef CellAccessor<dim> BaseClass;
};




/* -------------------------------------------------------------------------- */



/**
 * Common template for line, quad, hex.
 *
 * Internal: inheritance is necessary for the general template due to
 * a compiler error.
 * @author Guido Kanschat, 1999
 */
template<int celldim, int dim>
class DoFObjectAccessor : public DoFAccessor<dim>,
			  public DoFObjectAccessor_Inheritance<celldim,dim>::BaseClass
{};



/**
 * Closure class.
 */
template<int dim>
class DoFObjectAccessor<0, dim> : public DoFAccessor<dim>,
				  public DoFObjectAccessor_Inheritance<0,dim>::BaseClass
{
  public:
    typedef void AccessorData;

				     /**
				      * Constructor. Should never be called
				      * and thus throws an error.
				      */
    DoFObjectAccessor (Triangulation<dim> *,
		       const int,
		       const int,
		       const AccessorData *)
      {
	Assert (false, ExcInternalError());
      }
};


/**
 * Grant access to the degrees of freedom located on lines.
 * This class follows mainly the route laid out by the accessor library
 * declared in the triangulation library (\Ref{TriaAccessor}). It enables
 * the user to access the degrees of freedom on the lines (there are similar
 * versions for the DoFs on quads, etc), where the dimension of the underlying
 * triangulation does not really matter (i.e. this accessor works with the
 * lines in 1D-, 2D-, etc dimensions).
 *
 *
 * \subsection{Usage}
 *
 * The \Ref{DoFDimensionInfo} classes inherited by the \Ref{DoFHandler} classes
 * declare typedefs to iterators using the accessors declared in this class
 * hierarchy tree. Usage is best to happens through these typedefs, since they
 * are more secure to changes in the class naming and template interface as well
 * as they provide easier typing (much less complicated names!).
 * 
 * 
 * \subsection{Notes about the class hierarchy structure}
 *
 * Inheritance from #DoFObjectAccessor_Inheritance<1,dim>::BaseClass# yields
 * inheritance from #CellAccessor<1># if #dim==1# and from
 * #TriaObjectAccessor<1,dim># for all other #dim# values. Thus, an object
 * of this class shares all features of cells in one dimension, but behaves
 * like an ordinary line in all other cases.
 *
 * @author Wolfgang Bangerth, 1998
 */
template <int dim>
class DoFObjectAccessor<1, dim> :  public DoFAccessor<dim>,
				   public DoFObjectAccessor_Inheritance<1,dim>::BaseClass
{
  public:
				     /**
				      * Declare the data type that this accessor
				      * class expects to get passed from the
				      * iterator classes.
				      */
    typedef DoFHandler<dim> AccessorData;
    
				     /**
				      * Default constructor, unused thus
				      * not implemented.
				      */
    DoFObjectAccessor ();
    
    				     /**
				      * Constructor. The #local_data#
				      * argument is assumed to be a pointer
				      * to a #DoFHandler<dim># object.
				      */
    DoFObjectAccessor (Triangulation<dim> *tria,
		     const int           level,
		     const int           index,
		     const AccessorData *local_data) :
		    DoFAccessor<dim> (local_data),
		    DoFObjectAccessor_Inheritance<1,dim>::BaseClass (tria,level,index) {};
    
				     /**
				      * Return the index of the #i#th degree
				      * of freedom of this line.
				      */
    int dof_index (const unsigned int i) const;

    				     /**
				      * Set the index of the #i#th degree
				      * of freedom of this line to #index#.
				      */
    void set_dof_index (const unsigned int i, const int index) const;

				     /**
				      * Return the index of the #i#th degree
				      * on the #vertex#th vertex.
				      */
    int vertex_dof_index (const unsigned int vertex,
			  const unsigned int i) const;

				     /**
				      * Set the index of the #i#th degree
				      * on the #vertex#th vertex to #index#.
				      */
    void set_vertex_dof_index (const unsigned int vertex,
			       const unsigned int i,
			       const int          index) const;

    				     /**
				      * Return the indices of the dofs of this
				      * line in the standard ordering: dofs
				      * on vertex 0, dofs on vertex 1, 
				      * dofs on line.
				      *
				      * It is assumed that the vector already
				      * has the right size beforehand.
				      */
    void get_dof_indices (vector<int> &dof_indices) const;

				     /**
				      * Return the #i#th child as a DoF line
				      * iterator. This function is needed since
				      * the child function of the base
				      * class returns a line accessor without
				      * access to the DoF data.
				      */
    TriaIterator<dim,DoFObjectAccessor<1,dim> > child (const unsigned int) const;

				     /**
				      * Distribute a local (cell based) vector
				      * to a global one by mapping the local
				      * numbering of the degrees of freedom
				      * to the global one and entering the
				      * local values into the global vector.
				      *
				      * The elements are {\it added} up to
				      * the elements in the global vector,
				      * rather than just set, since this is
				      * usually what one wants.
				      */
    void distribute_local_to_global (const Vector<double> &local_source,
				     Vector<double>       &global_destination) const;

				     /**
				      * This function does much the same as the
				      * #distribute_local_to_global(dVector,dVector)#
				      * function, but operates on matrices
				      * instead of vectors. The sparse matrix
				      * is supposed to have non-zero entry
				      * slots where required.
				      */
    void distribute_local_to_global (const FullMatrix<double> &local_source,
				     SparseMatrix<double>     &global_destination) const;
    
				     /**
				      * Implement the copy operator needed
				      * for the iterator classes.
				      */
    void copy_from (const DoFObjectAccessor<1,dim> &a);
};



/**
 * Grant access to the degrees of freedom located on quads.
 *
 * @see DoFLineAccessor
 */
template <int dim>
class DoFObjectAccessor<2, dim> :  public DoFAccessor<dim>,
				   public DoFObjectAccessor_Inheritance<2,dim>::BaseClass
{
  public:
				     /**
				      * Declare the data type that this accessor
				      * class expects to get passed from the
				      * iterator classes.
				      */
    typedef DoFHandler<dim> AccessorData;
    
				     /**
				      * Default constructor, unused thus
				      * not implemented.
				      */
    DoFObjectAccessor ();

    				     /**
				      * Constructor. The #local_data#
				      * argument is assumed to be a pointer
				      * to a #DoFHandler<dim># object.
				      */
    DoFObjectAccessor (Triangulation<dim> *tria,
		     const int           level,
		     const int           index,
		     const AccessorData *local_data) :
		    DoFAccessor<dim> (local_data),
		    DoFObjectAccessor_Inheritance<2,dim>::BaseClass (tria,level,index) {};
    
				     /**
				      * Return the index of the #i#th degree
				      * of freedom of this quad.
				      */
    int dof_index (const unsigned int i) const;

    				     /**
				      * Set the index of the #i#th degree
				      * of freedom of this quad to #index#.
				      */
    void set_dof_index (const unsigned int i, const int index) const;

				     /**
				      * Return the index of the #i#th degree
				      * on the #vertex#th vertex.
				      */
    int vertex_dof_index (const unsigned int vertex,
			  const unsigned int i) const;

				     /**
				      * Set the index of the #i#th degree
				      * on the #vertex#th vertex to #index#.
				      */
    void set_vertex_dof_index (const unsigned int vertex,
			       const unsigned int i,
			       const int          index) const;

    				     /**
				      * Return the indices of the dofs of this
				      * quad in the standard ordering: dofs
				      * on vertex 0, dofs on vertex 1, etc,
				      * dofs on line 0, dofs on line 1, etc,
				      * dofs on quad 0, etc.
				      *
				      * It is assumed that the vector already
				      * has the right size beforehand.
				      */
    void get_dof_indices (vector<int> &dof_indices) const;

    				     /**
				      *  Return a pointer to the #i#th line
				      *  bounding this #Quad#.
				      */
    TriaIterator<dim,DoFObjectAccessor<1, dim> >
    line (const unsigned int i) const;
    
				     /**
				      * Return the #i#th child as a DoF quad
				      * iterator. This function is needed since
				      * the child function of the base
				      * class returns a quad accessor without
				      * access to the DoF data.
				      */
    TriaIterator<dim,DoFObjectAccessor<2, dim> >
    child (const unsigned int) const;

				     /**
				      * Distribute a local (cell based) vector
				      * to a global one by mapping the local
				      * numbering of the degrees of freedom
				      * to the global one and entering the
				      * local values into the global vector.
				      *
				      * The elements are {\it added} up to
				      * the elements in the global vector,
				      * rather than just set, since this is
				      * usually what one wants.
				      */
    void distribute_local_to_global (const Vector<double> &local_source,
				     Vector<double>       &global_destination) const;

				     /**
				      * This function does much the same as the
				      * #distribute_local_to_global(dVector,dVector)#
				      * function, but operates on matrices
				      * instead of vectors. The sparse matrix
				      * is supposed to have non-zero entry
				      * slots where required.
				      */
    void distribute_local_to_global (const FullMatrix<double> &local_source,
				     SparseMatrix<double>     &global_destination) const;
    
				     /**
				      * Implement the copy operator needed
				      * for the iterator classes.
				      */
    void copy_from (const DoFObjectAccessor<2, dim> &a);
};





/**
 * Grant access to the degrees of freedom located on hexes.
 *
 * @see DoFLineAccessor
 */
template <int dim>
class DoFObjectAccessor<3, dim> :  public DoFAccessor<dim>,
				   public DoFObjectAccessor_Inheritance<3,dim>::BaseClass
{
  public:
				     /**
				      * Declare the data type that this accessor
				      * class expects to get passed from the
				      * iterator classes.
				      */
    typedef DoFHandler<dim> AccessorData;
    
				     /**
				      * Default constructor, unused thus
				      * not implemented.
				      */
    DoFObjectAccessor ();

    				     /**
				      * Constructor. The #local_data#
				      * argument is assumed to be a pointer
				      * to a #DoFHandler<dim># object.
				      */
    DoFObjectAccessor (Triangulation<dim> *tria,
		    const int           level,
		    const int           index,
		    const AccessorData *local_data) :
		    DoFAccessor<dim> (local_data),
		    DoFObjectAccessor_Inheritance<3,dim>::BaseClass (tria,level,index) {};
    
				     /**
				      * Return the index of the #i#th degree
				      * of freedom of this quad.
				      */
    int dof_index (const unsigned int i) const;

    				     /**
				      * Set the index of the #i#th degree
				      * of freedom of this quad to #index#.
				      */
    void set_dof_index (const unsigned int i, const int index) const;

				     /**
				      * Return the index of the #i#th degree
				      * on the #vertex#th vertex.
				      */
    int vertex_dof_index (const unsigned int vertex,
			  const unsigned int i) const;

				     /**
				      * Set the index of the #i#th degree
				      * on the #vertex#th vertex to #index#.
				      */
    void set_vertex_dof_index (const unsigned int vertex,
			       const unsigned int i,
			       const int          index) const;

    				     /**
				      * Return the indices of the dofs of this
				      * quad in the standard ordering: dofs
				      * on vertex 0, dofs on vertex 1, etc,
				      * dofs on line 0, dofs on line 1, etc,
				      * dofs on quad 0, etc.
				      *
				      * It is assumed that the vector already
				      * has the right size beforehand.
				      */
    void get_dof_indices (vector<int> &dof_indices) const;

    				     /**
				      *  Return a pointer to the #i#th line
				      *  bounding this #Hex#.
				      */
    TriaIterator<dim,DoFObjectAccessor<1, dim> >
    line (const unsigned int i) const;

    				     /**
				      *  Return a pointer to the #i#th quad
				      *  bounding this #Hex#.
				      */
    TriaIterator<dim,DoFObjectAccessor<2, dim> >
    quad (const unsigned int i) const;
    
				     /**
				      * Return the #i#th child as a DoF hex
				      * iterator. This function is needed since
				      * the child function of the base
				      * class returns a hex accessor without
				      * access to the DoF data.
				      */
    TriaIterator<dim,DoFObjectAccessor<3, dim> > child (const unsigned int) const;

				     /**
				      * Distribute a local (cell based) vector
				      * to a global one by mapping the local
				      * numbering of the degrees of freedom
				      * to the global one and entering the
				      * local values into the global vector.
				      *
				      * The elements are {\it added} up to
				      * the elements in the global vector,
				      * rather than just set, since this is
				      * usually what one wants.
				      */
    void distribute_local_to_global (const Vector<double> &local_source,
				     Vector<double>       &global_destination) const;

				     /**
				      * This function does much the same as the
				      * #distribute_local_to_global(dVector,dVector)#
				      * function, but operates on matrices
				      * instead of vectors. The sparse matrix
				      * is supposed to have non-zero entry
				      * slots where required.
				      */
    void distribute_local_to_global (const FullMatrix<double> &local_source,
				     SparseMatrix<double>     &global_destination) const;
    
				     /**
				      * Implement the copy operator needed
				      * for the iterator classes.
				      */
    void copy_from (const DoFObjectAccessor<3, dim> &a);
};



/**
 * Grant access to the degrees of freedom on a cell. In fact, since all
 * access to the degrees of freedom has been enabled by the classes
 * #DoFObjectAccessor<1, 1># and #DoFObjectAccessor<2, 2># for the space dimension
 * one and two, respectively, this class only collects the pieces
 * together by deriving from the appropriate #DoF*Accessor# and the
 * right #CellAccessor<dim># and finally adding two functions which give
 * access to the neighbors and children as #DoFCellAccessor# objects
 * rather than #CellAccessor# objects (the latter function was inherited
 * from the #CellAccessor<dim># class).
 *
 * Note that since for the class we derive from, i.e. #DoFObjectAccessor<dim,dim>#,
 * the two template parameters are equal, the base class is actually derived from
 * #CellAccessor#, which makes the functions of this class available to the
 * #DoFCellAccessor# class as well.
 *
 * @author Wolfgang Bangerth, 1998
 */
template <int dim>
class DoFCellAccessor :  public DoFObjectAccessor<dim, dim> {
  public:
				     /**
				      * Declare the data type that this accessor
				      * class expects to get passed from the
				      * iterator classes.
				      */
    typedef typename DoFObjectAccessor<dim, dim>::AccessorData AccessorData;
    
    				     /**
				      * Constructor
				      */
    DoFCellAccessor (Triangulation<dim> *tria,
		     const int           level,
		     const int           index,
		     const AccessorData *local_data) :
		     DoFObjectAccessor<dim, dim> (tria,level,index,local_data) {};

				     /**
				      * Return the #i#th neighbor as a DoF cell
				      * iterator. This function is needed since
				      * the neighbor function of the base
				      * class returns a cell accessor without
				      * access to the DoF data.
				      */
    TriaIterator<dim,DoFCellAccessor<dim> >
    neighbor (const unsigned int) const;

    				     /**
				      * Return the #i#th child as a DoF cell
				      * iterator. This function is needed since
				      * the child function of the base
				      * class returns a cell accessor without
				      * access to the DoF data.
				      */
    TriaIterator<dim,DoFCellAccessor<dim> >
    child (const unsigned int) const;

    				     /**
				      * Return an iterator to the #i#th face
				      * of this cell.
				      *
				      * This function is not implemented in 1D,
				      * and maps to DoFObjectAccessor<2, dim>::line in 2D.
				      */
    TriaIterator<dim, DoFObjectAccessor<dim-1, dim> >
    face (const unsigned int i) const;

    				     /**
				      * Return the values of the given vector
				      * restricted to the dofs of this
				      * cell in the standard ordering: dofs
				      * on vertex 0, dofs on vertex 1, etc,
				      * dofs on line 0, dofs on line 1, etc,
				      * dofs on quad 0, etc.
				      *
				      * It is assumed that the vector already
				      * has the right size beforehand. This
				      * function is only callable for active
				      * cells.
				      */
    template <typename number>
    void get_dof_values (const Vector<number> &values,
			 Vector<number>       &local_values) const;

				     /**
				      * Return the interpolation of the given
				      * finite element function to the present
				      * cell. In the simplest case, the cell
				      * is a terminal one, i.e. has no children;
				      * then, the returned value is the vector
				      * of nodal values on that cell. You could
				      * then as well get the desired values
				      * through the #get_dof_values# function
				      * In the other case, when the cell has
				      * children, we use the restriction
				      * matrices provided by the finite element
				      * class to compute the interpolation
				      * from the children to the present cell.
				      *
				      * It is assumed that both vectors already
				      * have the right size beforehand. This
				      * function assumes the existence of an
				      * interpolation from child cells to the
				      * mother cell, denoted by the restriction
				      * matrices of the finite element class.
				      * Futhermore, this interpolation should
				      * be possible for each child alone, i.e.
				      * it should be possible to compute the
				      * restriction by writing values obtained
				      * from each child directly into the output
				      * vector, without, for example, computing
				      * an average over all children.
				      * These properties, however, do not exist
				      * for all elements; an example is the
				      * DG(0) element, which represents
				      * piecewise constant elements: for these
				      * the restriction to mother cell could
				      * be the average of the values of the
				      * children, maybe weighted by the measure
				      * of each child. It is not yet decided
				      * what the this function does in these
				      * cases.
				      */
    template <typename number>
    void get_interpolated_dof_values (const Vector<number> &values,
				      Vector<number>       &interpolated_values) const;

				     /**
				      * This function is the counterpart to
				      * #get_dof_values#: it takes a vector
				      * of values for the degrees of freedom
				      * of the cell pointed to by this iterator
				      * and writes these values into the global
				      * data vector #values#. This function
				      * is only callable for active cells.
				      *
				      * Note that for continuous finite
				      * elements, calling this function affects
				      * the dof values on neighboring cells as
				      * well. It may also violate continuity
				      * requirements for hanging nodes, if
				      * neighboring cells are less refined than
				      * the present one. These requirements
				      * are not taken care of and must be
				      * enforced by the user afterwards.
				      *
				      * It is assumed that both vectors already
				      * have the right size beforehand.
				      */
    template <typename number>
    void set_dof_values (const Vector<number> &local_values,
			 Vector<number>       &values) const;

				     /**
				      * This, again, is the counterpart to
				      * #get_interpolated_dof_values#: you
				      * specify the dof values on a cell and
				      * these are interpolated to the children
				      * of the present cell and set on the
				      * terminal cells.
				      *
				      * In principle, it works as follows: if
				      * the cell pointed to by this object is
				      * terminal, then the dof values are set
				      * in the global data vector by calling
				      * the #set_dof_values# function;
				      * otherwise, the values are prolonged
				      * to each of the children and this
				      * function is called for each of them.
				      *
				      * Using the #get_interpolated_dof_values#
				      * and this function, you can compute the
				      * interpolation of a finite element
				      * function to a coarser grid by first
				      * getting the interpolated solution on a
				      * cell of the coarse grid and afterwards
				      * redistributing it using this function.
				      *
				      * Note that for continuous finite
				      * elements, calling this function affects
				      * the dof values on neighboring cells as
				      * well. It may also violate continuity
				      * requirements for hanging nodes, if
				      * neighboring cells are less refined than
				      * the present one, or if their children are
				      * less refined than the children of this
				      * cell. These requirements
				      * are not taken care of and must be
				      * enforced by the user afterwards.
				      *
				      * It is assumed that both vectors already
				      * have the right size beforehand. This
				      * function relies on the existence of a
				      * natural interpolation property of
				      * finite element spaces of a cell to
				      * its children, denoted by the
				      * prolongation matrices of finite element
				      * classes. For some elements, the spaces
				      * on coarse and fine grids are not nested,
				      * in which case the interpolation to a
				      * child is not the identity; refer to the
				      * documentation of the respective finite
				      * element class for a description of what
				      * the prolongation matrices represent in
				      * this case.
				      */
    template <typename number>
    void set_dof_values_by_interpolation (const Vector<number> &local_values,
					  Vector<number>       &values) const;
    
    				     /**
				      *  Exception
				      */
    DeclException0 (ExcNotUsefulForThisDimension);
				     /**
				      * Exception
				      */
    DeclException0 (ExcNotActive);
};





// if in optimized mode: include more templates
#ifndef DEBUG
#  include "dof_accessor.templates.h"
#endif





/*----------------------------   dof_iterator.h     ---------------------------*/
/* end of #ifndef __dof_iterator_H */
#endif
/*----------------------------   dof_iterator.h     ---------------------------*/
