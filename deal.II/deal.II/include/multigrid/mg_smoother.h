//----------------------------  mg_smoother.h  ---------------------------
//    $Id$
//    Version: $Name$
//
//    Copyright (C) 1998, 1999, 2000, 2001 by the deal.II authors
//
//    This file is subject to QPL and may not be  distributed
//    without copyright and license information. Please refer
//    to the file deal.II/doc/license.html for the  text  and
//    further information on this license.
//
//----------------------------  mg_smoother.h  ---------------------------
#ifndef __deal2__mg_smoother_h
#define __deal2__mg_smoother_h


#include <base/config.h>
#include <lac/forward_declarations.h>
#include <multigrid/mg_base.h>
#include <base/smartpointer.h>
#include <vector>

template <int dim> class MGDoFHandler;

/**
 * Abstract base class for multigrid smoothers.
 * In fact, this class only provides the interface of the smoothing function.
 * Using @p{deal.II} grid handling, @p{MGSmoother} is a good point to start.
 *
 * @author Wolfgang Bangerth, Guido Kanschat, 1999
 */
class MGSmootherBase :  public Subscriptor 
{  
  public:
				     /**
				      * Virtual destructor. Does
				      * nothing in particular, but
				      * needs to be declared anyhow.
				      */
    virtual ~MGSmootherBase();
    
				     /**
				      * Smooth the residual of @p{u} on
				      * the given level. If $S$ is the
				      * smoothing operator, then this
				      * function should do the
				      * following operation:
				      * $u += S (rhs - Au)$, where @p{u} and
				      * @p{rhs} are the input parameters.
				      *
				      * This function should keep the
				      * interior level boundary
				      * values, so you may want to
				      * call @p{set_zero_interior_boundary}
				      * in the derived class
				      * @p{MGSmoother} somewhere in your
				      * derived function, or another
				      * function doing similar things.
				      */
    virtual void smooth (const unsigned int    level,
			 Vector<double>       &u,
			 const Vector<double> &rhs) const = 0;
};


/**
 * Smoother doing nothing. This class is not useful for many applications other
 * than for testing some multigrid procedures. Also some applications might
 * get convergence without smoothing and then this class brings you the
 * cheapest possible multigrid.
 *
 * @author Guido Kanschat, 1999
 */
class MGSmootherIdentity : public MGSmootherBase
{
  public:
				     /**
				      * Implementation of the
				      * interface in @p{MGSmootherBase}.
				      * This function does nothing,
				      * which by comparison with the
				      * definition of this function
				      * means that the the smoothing
				      * operator equals the null
				      * operator.
				      */
    virtual void smooth (const unsigned int   level,
			 Vector<double>       &u,
			 const Vector<double> &rhs) const;
};


/**
 * Base class for multigrid smoothers. It declares the interface
 * to smoothers by inheriting @p{MGSmootherBase} and implements some functionality for setting
 * the values
 * of vectors at interior boundaries (i.e. boundaries between differing
 * levels of the triangulation) to zero, by building a list of these degrees
 * of freedom's indices at construction time.
 *
 * @author Wolfgang Bangerth, Guido Kanschat, 1999
 */
class MGSmoother : public MGSmootherBase
{
  private:
				     /**
				      * Default constructor. Made private
				      * to prevent it being called, which
				      * is necessary since this could
				      * circumpass the set-up of the list
				      * if interior boundary dofs.
				      */
    MGSmoother ();
    
  public:

				     /**
				      * Constructor. This one collects
				      * the indices of the degrees of freedom
				      * on the interior boundaries between
				      * the different levels, which are
				      * needed by the function
				      * @p{set_zero_interior_boundaries}.
				      *
				      * Since this function is implemented
				      * a bit different in 1d (there are no
				      * faces of cells, just vertices),
				      * there are actually two sets of
				      * constructors, namely this one for 1d
				      * and the following one for all other
				      * dimensions.
				      */
    MGSmoother (const MGDoFHandler<1> &mg_dof,
		unsigned int           steps);

				     /**
				      * Constructor. This one collects
				      * the indices of the degrees of freedom
				      * on the interior boundaries between
				      * the different levels, which are
				      * needed by the function
				      * @p{set_zero_interior_boundaries}.
				      *
				      * The parameter steps indicates the number of smoothing
				      * steps to be executed by @p{smooth}.
				      */
    template <int dim>
    MGSmoother (const MGDoFHandler<dim> &mg_dof,
		unsigned int             steps);    

				     /**
				      * Reset the values of the degrees of
				      * freedom on interior boundaries between
				      * different levels to zero in the given
				      * data vector @p{u}.
				      *
				      * Since the coarsest level (@p{level==0})
				      * has no interior boundaries, this
				      * function does nothing in this case.
				      */
    void set_zero_interior_boundary (const unsigned int  level,
				     Vector<double>      &u) const;
				     /**
				      * Modify the number of smoothing steps.
				      */
    void set_steps(unsigned int steps);
				     /**
				      * How many steps should be used?
				      */
    unsigned int get_steps() const;

  private:
				     /**
				      * Number of smoothing steps.
				      */
    unsigned int steps;
				     /**
				      * For each level, we store a list of
				      * degree of freedom indices which are
				      * located on interior boundaries between
				      * differing levels of the triangulation.
				      * Since the coarsest level has no
				      * interior boundary dofs, the first
				      * entry refers to the second level.
				      *
				      * These arrays are set by the constructor.
				      * The entries for each level are sorted ascendingly.
				      */
    std::vector<std::vector<unsigned int> > interior_boundary_dofs;
};


/**
 * Implementation of a smoother using matrix builtin relaxation methods.
 * 
 * @author Wolfgang Bangerth, Guido Kanschat, 1999
 */
template<typename number>
class MGSmootherRelaxation : public MGSmoother
{
  public:
				     /**
				      * Type of the smoothing
				      * function of the matrix.
				      */
    typedef void
    (SparseMatrix<number>::* function_ptr)(Vector<double>&, const Vector<double>&,
					   typename SparseMatrix<number>::value_type) const;
    
				     /**
				      * Constructor.
				      * The constructor uses an @p{MGDoFHandler} to initialize
				      * data structures for interior level boundary handling
				      * in @p{MGSmoother}.
				      *
				      * Furthermore, it takes a pointer to the
				      * level matrices and their smoothing function.
				      * This function must perform one relaxation step
				      * like @p{SparseMatrix<number>::SOR_step} does. Do not
				      * use the preconditioning methods because they apply
				      * a preconditioning matrix to the residual.
				      *
				      * The final two parameters are the number of relaxation
				      * steps and the relaxation parameter.
				      */

    template<int dim>
    MGSmootherRelaxation(const MGDoFHandler<dim>                       &mg_dof,
			 const MGLevelObject<SparseMatrix<number> > &matrix,
			 const function_ptr                             function,
			 const unsigned int                             steps,
			 const double                                   omega = 1.);
    
				     /**
				      * Implementation of the interface in @p{MGSmootherBase}.
				      * We use the SOR method in @p{SparseMatrix} for the real work
				      * and find a way to keep the boundary values.
				      */
    virtual void smooth (const unsigned int   level,
			 Vector<double>       &u,
			 const Vector<double> &rhs) const;
  private:
				     /**
				      * Pointer to the matrices.
				      */
    SmartPointer<const MGLevelObject<SparseMatrix<number> > > matrix;
    
				     /**
				      * Pointer to the relaxation function.
				      */
    function_ptr relaxation;

				     /**
				      * Relaxation parameter.
				      */
    double omega;
    
				     /**
				      * Auxiliary vector.
				      */
    mutable Vector<double> h1;
    
				     /**
				      * Auxiliary vector.
				      */
    mutable Vector<double> h2;
};


/* ------------------------------- Inline functions -------------------------- */

inline
void
MGSmoother::set_steps(unsigned int i)
{
  steps = i;
}

inline
unsigned int
MGSmoother::get_steps() const
{
  return steps;
}


#endif
			 
