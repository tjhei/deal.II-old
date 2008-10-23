//---------------------------------------------------------------------------
//    $Id$
//    Version: $Name$
//
//    Copyright (C) 2008 by the deal.II authors
//
//    This file is subject to QPL and may not be  distributed
//    without copyright and license information. Please refer
//    to the file deal.II/doc/license.html for the  text  and
//    further information on this license.
//
//---------------------------------------------------------------------------
#ifndef __deal2__trilinos_sparse_matrix_h
#define __deal2__trilinos_sparse_matrix_h


#include <base/config.h>
#include <base/subscriptor.h>
#include <lac/sparse_matrix.h>
#include <lac/exceptions.h>
#include <lac/trilinos_vector_base.h>

#include <boost/shared_ptr.hpp>
#include <vector>
#include <cmath>
#include <memory>

#ifdef DEAL_II_USE_TRILINOS

#  define TrilinosScalar double
#  include <Epetra_FECrsMatrix.h>
#  include <Epetra_Map.h>
#  include <Epetra_CrsGraph.h>
#  ifdef DEAL_II_COMPILER_SUPPORTS_MPI
#    include <Epetra_MpiComm.h>
#    include "mpi.h"
#  else
#    include "Epetra_SerialComm.h"
#  endif

DEAL_II_NAMESPACE_OPEN

                                   // forward declarations
class SparsityPattern;
class CompressedSparsityPattern;
class CompressedSetSparsityPattern;
class CompressedSimpleSparsityPattern;

namespace TrilinosWrappers
{
                                   // forward declarations
  class VectorBase;
  class SparseMatrix;

  namespace MatrixIterators
  {
/**
 * STL conforming iterator. This class acts as an iterator walking
 * over the elements of Trilinos matrices. The implementation of this
 * class is similar to the one for PETSc matrices.
 *
 * Note that Trilinos does not give any guarantees as to the order of
 * elements within each row. Note also that accessing the elements of
 * a full matrix surprisingly only shows the nonzero elements of the
 * matrix, not all elements.
 *
 * @ingroup TrilinosWrappers
 * @author Martin Kronbichler, Wolfgang Bangerth, 2008
 */
    class const_iterator
    {
      private:
                                       /**
					* Accessor class for iterators
					*/
        class Accessor
        {
          public:
                                       /**
					* Constructor. Since we use
					* accessors only for read
					* access, a const matrix
					* pointer is sufficient.
					*/
            Accessor (const SparseMatrix  *matrix,
                      const unsigned int   row,
                      const unsigned int   index);

                                       /**
					* Row number of the element
					* represented by this object.
					*/
            unsigned int row() const;

                                       /**
					* Index in row of the element
					* represented by this object.
					*/
            unsigned int index() const;

                                       /**
					* Column number of the element
					* represented by this object.
					*/
            unsigned int column() const;

	                               /**
					* Value of this matrix entry.
					*/
            TrilinosScalar value() const;

                                       /**
					* Exception
					*/
            DeclException0 (ExcBeyondEndOfMatrix);

	                               /**
					* Exception
					*/
            DeclException3 (ExcAccessToNonlocalRow,
                            int, int, int,
                            << "You tried to access row " << arg1
                            << " of a distributed matrix, but only rows "
                            << arg2 << " through " << arg3
                            << " are stored locally and can be accessed.");
            
          private:
                                       /**
					* The matrix accessed.
					*/
            mutable SparseMatrix *matrix;

                                       /**
					* Current row number.
					*/
            unsigned int a_row;

                                       /**
					* Current index in row.
					*/
            unsigned int a_index;

                                       /**
					* Cache where we store the
					* column indices of the
					* present row. This is
					* necessary, since Trilinos
					* makes access to the elements
					* of its matrices rather hard,
					* and it is much more
					* efficient to copy all column
					* entries of a row once when
					* we enter it than repeatedly
					* asking Trilinos for
					* individual ones. This also
					* makes some sense since it is
					* likely that we will access
					* them sequentially anyway.
					*
					* In order to make copying of
					* iterators/accessor of
					* acceptable performance, we
					* keep a shared pointer to
					* these entries so that more
					* than one accessor can access
					* this data if necessary.
					*/
            boost::shared_ptr<const std::vector<unsigned int> > colnum_cache;

                                       /**
					* Similar cache for the values
					* of this row.
					*/
            boost::shared_ptr<const std::vector<TrilinosScalar> > value_cache;
            
	                               /**
					* Discard the old row caches
					* (they may still be used by
					* other accessors) and
					* generate new ones for the
					* row pointed to presently by
					* this accessor.
					*/
            void visit_present_row ();

                                       /**
					* Make enclosing class a
					* friend.
					*/
            friend class const_iterator;
        };
        
      public:
          
                                       /**
					* Constructor. Create an
					* iterator into the matrix @p
					* matrix for the given row and
					* the index within it.
					*/ 
        const_iterator (const SparseMatrix   *matrix,
                        const unsigned int  row,
                        const unsigned int  index);
          
                                       /**
					* Prefix increment.
					*/
        const_iterator& operator++ ();

                                       /**
					* Postfix increment.
					*/
        const_iterator operator++ (int);

                                       /**
					* Dereferencing operator.
					*/
        const Accessor& operator* () const;

                                       /**
					* Dereferencing operator.
					*/
        const Accessor* operator-> () const;

                                       /**
					* Comparison. True, if both
					* iterators point to the same
					* matrix position.
					*/
        bool operator == (const const_iterator&) const;

                                       /**
					* Inverse of <tt>==</tt>.
					*/
        bool operator != (const const_iterator&) const;

                                       /**
					* Comparison operator. Result
					* is true if either the first
					* row number is smaller or if
					* the row numbers are equal
					* and the first index is
					* smaller.
					*/
        bool operator < (const const_iterator&) const;

	                               /**
                                        * Exception
					*/
        DeclException2 (ExcInvalidIndexWithinRow,
                        int, int,
                        << "Attempt to access element " << arg2
                        << " of row " << arg1
                        << " which doesn't have that many elements.");
        
      private:
                                       /**
                                        * Store an object of the
                                        * accessor class.
					*/
        Accessor accessor;
    };
    
  }
  
  
/**
 * This class implements a wrapper to use the Trilinos distributed
 * sparse matrix class Epetra_FECrsMatrix. This is precisely the kind of
 * matrix we deal with all the time - we most likely get it from some
 * assembly process, where also entries not locally owned might need to
 * be written and hence need to be forwarded to the owner process.  This
 * class is designed to be used in a distributed memory architecture
 * with an MPI compiler on the bottom, but works equally well also for
 * serial processes. The only requirement for this class to work is that
 * Trilinos has been installed with the same compiler as is used for
 * generating deal.II.
 *
 * The interface of this class is modeled after the existing
 * SparseMatrix class in deal.II. It has almost the same member
 * functions, and is often exchangable. However, since Trilinos only
 * supports a single scalar type (double), it is not templated, and only
 * works with doubles.
 *
 * Note that Trilinos only guarantees that operations do what you expect
 * if the functions @p GlobalAssemble has been called after matrix
 * assembly.  Therefore, you need to call SparseMatrix::compress()
 * before you actually use the matrix. This also calls @p FillComplete
 * that compresses the storage format for sparse matrices by discarding
 * unused elements. Trilinos allows to continue with assembling the
 * matrix after calls to these functions, though.
 * 
 * @ingroup TrilinosWrappers
 * @ingroup Matrix1
 * @author Martin Kronbichler, Wolfgang Bangerth, 2008
 */
  class SparseMatrix : public Subscriptor
  {
    public:
                                       /**
                                        * A structure that describes
                                        * some of the traits of this
                                        * class in terms of its run-time
                                        * behavior. Some other classes
                                        * (such as the block matrix
                                        * classes) that take one or
                                        * other of the matrix classes as
                                        * its template parameters can
                                        * tune their behavior based on
                                        * the variables in this class.
                                        */
      struct Traits
      {
                                       /**
                                        * It is safe to elide additions
                                        * of zeros to individual
                                        * elements of this matrix.
					*/
          static const bool zero_addition_can_be_elided = true;
      };

                                       /**
                                        * Declare a typedef for the
                                        * iterator class.
                                        */
      typedef MatrixIterators::const_iterator const_iterator;

                                       /**
                                        * Declare a typedef in analogy
                                        * to all the other container
                                        * classes.
                                        */
      typedef TrilinosScalar value_type;
      
/**
 * @name Constructors and initalization.
 */
//@{
                                       /**
                                        * Default constructor. Generates
                                        * an empty (zero-size) matrix.
                                        */
      SparseMatrix ();

                                       /**
                                        * Constructor using an
				        * Epetra_Map and a maximum
				        * number of nonzero matrix
				        * entries. Note that this
				        * number does not need to be
				        * exact, and it is even
				        * allowed that the actual
				        * matrix structure has more
				        * nonzero entries than
				        * specified in the
				        * constructor.  However it is
				        * still advantageous to
				        * provide good estimates here
				        * since this will considerably
				        * increase the performance of
				        * the matrix.
                                        */
      SparseMatrix (const Epetra_Map   &InputMap,
		    const unsigned int  n_max_entries_per_row);

                                       /**
                                        * Same as before, but now use
				        * the exact number of nonzeros
				        * in each matrix row. Since we
				        * know the number of elements
				        * in the matrix exactly in
				        * this case, we can already
				        * allocate the right amount of
				        * memory, which makes the
				        * creation process including
				        * the insertion of nonzero
				        * elements by the respective
				        * SparseMatrix::reinit call
				        * considerably faster.
                                        */
      SparseMatrix (const Epetra_Map                &InputMap,
		    const std::vector<unsigned int> &n_entries_per_row);

                                       /**
                                        * This constructor is similar to
                                        * the one above, but it now
                                        * takes two different Epetra
                                        * maps for rows and
                                        * columns. This interface is
                                        * meant to be used for
                                        * generating rectangular
                                        * matrices, where one map takes
                                        * care of the columns and the
                                        * other one of the rows. Note
                                        * that there is no real
                                        * parallelism along the columns
                                        * &ndash; the processor that
                                        * owns a certain row always owns
                                        * all the column elements, no
                                        * matter how far they might be
                                        * spread out. The second
                                        * Epetra_Map is only used to
                                        * specify the number of columns
                                        * and for internal arragements
                                        * when doing matrix-vector
                                        * products with vectors based on
                                        * that column map.
					*
					* The number of columns entries
					* per row is specified as the
					* maximum number of entries
					* argument.
                                        */
      SparseMatrix (const Epetra_Map   &InputRowMap,
		    const Epetra_Map   &InputColMap,
		    const unsigned int  n_max_entries_per_row);

                                       /**
                                        * This constructor is similar to
				        * the one above, but it now
				        * takes two different Epetra
				        * maps for rows and
				        * columns. This interface is
				        * meant to be used for
				        * generating rectangular
				        * matrices, where one map
				        * specifies the parallel
				        * distribution of rows and the
				        * second one specifies the
				        * number of columns in the
				        * total matrix. It also provides
				        * information for the internal
				        * arrangement in matrix vector
				        * products, but is not used for
				        * the distribution of the
				        * columns &ndash; rather, all
				        * column elements of a row are
				        * stored on the same
				        * processor. The vector
				        * n_entries_per_row specifies
				        * the number of entries in each
				        * row of the newly generated
				        * matrix.
                                        */
      SparseMatrix (const Epetra_Map                &InputRowMap,
		    const Epetra_Map                &InputColMap,
		    const std::vector<unsigned int> &n_entries_per_row);

                                       /**
                                        * Generate a matrix that is completely
                                        * stored locally, having #m rows and
                                        * #n columns. The resulting matrix
                                        * will be completely stored locally.
					*
					* The number of columns entries
					* per row is specified as the
					* maximum number of entries
					* argument.
                                        */
      SparseMatrix (const unsigned int  m,
		    const unsigned int  n,
		    const unsigned int  n_max_entries_per_row);

                                       /**
                                        * Copy constructor. Sets the
                                        * calling matrix to be the same
                                        * as the input matrix, i.e.,
                                        * using the same sparsity
                                        * pattern and entries.
                                        */
      SparseMatrix (const SparseMatrix &InputMatrix);

                                       /**
                                        * Destructor. Made virtual so
                                        * that one can use pointers to
                                        * this class.
                                        */
      virtual ~SparseMatrix ();

                                       /**
                                        * This function initializes
				        * the Trilinos matrix with a
				        * deal.II sparsity pattern,
				        * i.e. it makes the Trilinos
				        * Epetra matrix know the
				        * position of nonzero entries
				        * according to the sparsity
				        * pattern. This function is
				        * meant for use in serial
				        * programs, where there is no
				        * need to specify how the
				        * matrix is going to be
				        * distributed among the
				        * processors. This function
				        * works in parallel, too, but
				        * it is recommended to
				        * manually specify the
				        * parallel partioning of the
				        * matrix using an
				        * Epetra_Map. When run in
				        * parallel, it is currently
				        * necessary that each
				        * processor holds the
				        * sparsity_pattern structure
				        * because each processor sets
				        * its rows.
					*
					* This is a collective
				        * operation that needs to be
				        * called on all processors in
				        * order to avoid a dead lock.
                                        */
      template<typename SparsityType>
      void reinit (const SparsityType &sparsity_pattern);

				       /**
                                        * This function is initializes
				        * the Trilinos Epetra matrix
				        * according to the specified
				        * sparsity_pattern, and also
				        * reassigns the matrix rows to
				        * different processes
				        * according to a user-supplied
				        * Epetra map. In programs
				        * following the style of the
				        * tutorial programs, this
				        * function (and the respective
				        * call for a rectangular matrix)
				        * are the natural way to
				        * initialize the matrix size,
				        * its distribution among the MPI
				        * processes (if run in parallel)
				        * as well as the locatoin of
				        * non-zero elements. Trilinos
				        * stores the sparsity pattern
				        * internally, so it won't be
				        * needed any more after this
				        * call, in contrast to the
				        * deal.II own object. In a
				        * parallel run, it is currently
				        * necessary that each processor
				        * holds the sparsity_pattern
				        * structure because each
				        * processor sets its
				        * rows.
					*
					* This is a
				        * collective operation that
				        * needs to be called on all
				        * processors in order to avoid a
				        * dead lock.
                                        */
      template<typename SparsityType>
      void reinit (const Epetra_Map    &input_map,
		   const SparsityType  &sparsity_pattern);

				       /**
                                        * This function is similar to
				        * the other initialization
				        * function above, but now also
				        * reassigns the matrix rows
				        * and columns according to two
				        * user-supplied Epetra maps.
				        * To be used for rectangular
				        * matrices.
					*
					* This is a
				        * collective operation that
				        * needs to be called on all
				        * processors in order to avoid a
				        * dead lock.
                                        */
      template<typename SparsityType>
      void reinit (const Epetra_Map    &input_row_map,
		   const Epetra_Map    &input_col_map,
		   const SparsityType  &sparsity_pattern);

				       /**
				        * This function copies the
				        * content in
				        * <tt>sparse_matrix</tt> to
				        * the calling matrix.
					*
					* This is a
				        * collective operation that
				        * needs to be called on all
				        * processors in order to avoid a
				        * dead lock.
				        */
      void reinit (const SparseMatrix &sparse_matrix);

				       /**
                                        * This function initializes
				        * the Trilinos matrix using
				        * the deal.II sparse matrix
				        * and the entries stored
				        * therein. It uses a threshold
				        * to copy only elements with
				        * modulus larger than the
				        * threshold (so zeros in the
				        * deal.II matrix can be
				        * filtered away).
					*
					* This is a
				        * collective operation that
				        * needs to be called on all
				        * processors in order to avoid a
				        * dead lock.
                                        */
      void reinit (const ::dealii::SparseMatrix<double> &dealii_sparse_matrix,
		   const double                          drop_tolerance=1e-13);

				       /**
                                        * This function initializes
				        * the Trilinos matrix using
				        * the deal.II sparse matrix
				        * and the entries stored
				        * therein. It uses a threshold
				        * to copy only elements with
				        * modulus larger than the
				        * threshold (so zeros in the
				        * deal.II matrix can be
				        * filtered away). In contrast
				        * to the other reinit function
				        * with deal.II sparse matrix
				        * argument, this function
				        * takes a parallel
				        * partitioning specified by
				        * the user instead of
				        * internally generating one.
					*
					* This is a
				        * collective operation that
				        * needs to be called on all
				        * processors in order to avoid a
				        * dead lock.
                                        */
      void reinit (const Epetra_Map                     &input_map,
		   const ::dealii::SparseMatrix<double> &dealii_sparse_matrix,
		   const double                          drop_tolerance=1e-13);

 				       /**
                                        * This function is similar to
				        * the other initialization
				        * function with deal.II sparse
				        * matrix input above, but now
				        * takes Epetra maps for both
				        * the rows and the columns of
				        * the matrix. Chosen for
				        * rectangular matrices.
					*
					* This is a
				        * collective operation that
				        * needs to be called on all
				        * processors in order to avoid a
				        * dead lock.
                                        */
      void reinit (const Epetra_Map                      &input_row_map,
		   const Epetra_Map                      &input_col_map,
		   const ::dealii::SparseMatrix<double>  &dealii_sparse_matrix,
		   const double                           drop_tolerance=1e-13);

                                       /**
                                        * This operator assigns a scalar
                                        * to a matrix. Since this does
                                        * usually not make much sense
                                        * (should we set all matrix
                                        * entries to this value? Only
                                        * the nonzero entries of the
                                        * sparsity pattern?), this
                                        * operation is only allowed if
                                        * the actual value to be
                                        * assigned is zero. This
                                        * operator only exists to allow
                                        * for the obvious notation
                                        * <tt>matrix=0</tt>, which sets
                                        * all elements of the matrix to
                                        * zero, but keeps the sparsity
                                        * pattern previously used.
                                        */
      SparseMatrix &
	operator = (const double d);

                                       /**
                                        * Release all memory and
                                        * return to a state just like
                                        * after having called the
                                        * default constructor.
					*
					* This is a
				        * collective operation that
				        * needs to be called on all
				        * processors in order to avoid a
				        * dead lock.
                                        */
      void clear ();

                                       /**
                                        * Trilinos matrices store their
                                        * own sparsity patterns. So, in
                                        * analogy to our own
                                        * SparsityPattern class, this
                                        * function compresses the
                                        * sparsity pattern and allows
                                        * the resulting matrix to be
                                        * used in all other operations
                                        * where before only assembly
                                        * functions were allowed. This
                                        * function must therefore be
                                        * called once you have assembled
                                        * the matrix. This is a
                                        * collective operation, i.e., it
                                        * needs to be run on all
                                        * processors when used in
                                        * parallel.
                                        */
      void compress ();

				       /**
					* Returns the state of the
					* matrix, i.e., whether
					* compress() needs to be called
					* after an operation requiring
					* data exchange. Does only
					* return non-true values when
					* used in <tt>debug</tt> mode,
					* since it is quite expensive to
					* keep track of all operations
					* that lead to the need for
					* compress().
					*/
      bool is_compressed () const;
//@}
/**
 * @name Information on the matrix
 */
//@{
      
                                       /**
                                        * Return the number of rows in
                                        * this matrix.
                                        */
      unsigned int m () const;

                                       /**
                                        * Return the number of columns
                                        * in this matrix.
                                        */
      unsigned int n () const;

                                       /**
                                        * Return the local dimension
                                        * of the matrix, i.e. the
                                        * number of rows stored on the
                                        * present MPI process. For
                                        * sequential matrices, this
                                        * number is the same as m(),
                                        * but for parallel matrices it
                                        * may be smaller.
					*
					* To figure out which elements
					* exactly are stored locally,
					* use local_range().
                                        */
      unsigned int local_size () const;

                                       /**
					* Return a pair of indices
					* indicating which rows of
					* this matrix are stored
					* locally. The first number is
					* the index of the first row
					* stored, the second the index
					* of the one past the last one
					* that is stored locally. If
					* this is a sequential matrix,
					* then the result will be the
					* pair (0,m()), otherwise it
					* will be a pair (i,i+n),
					* where
					* <tt>n=local_size()</tt>.
					*/
      std::pair<unsigned int, unsigned int>
	local_range () const;

				       /**
					* Return whether @p index is
					* in the local range or not,
					* see also local_range().
					*/
      bool in_local_range (const unsigned int index) const;

                                       /**
                                        * Return the number of nonzero
                                        * elements of this matrix.
                                        */
      unsigned int n_nonzero_elements () const;

                                       /**
                                        * Number of entries in a
                                        * specific row.
                                        */
      unsigned int row_length (const unsigned int row) const;

                                       /**
					* Test whether a matrix is
					* symmetric.  Default
					* tolerance is zero.  TODO:
					* Not implemented.
					*/
      bool is_symmetric (const double tol = 0.0) const;

                                       /**
					* Test whether a matrix is
					* Hermitian, i.e. it is the
					* complex conjugate of its
					* transpose.  TODO: Not
					* implemented.
					*/

      bool is_hermitian () const;
				     /**
				      * Determine an estimate for the
				      * memory consumption (in bytes)
				      * of this object. Currently not
				      * implemented for this class.
				      */
      unsigned int memory_consumption () const;

//@}
/**
 * @name Modifying entries
 */
//@{
                                       /**
                                        * Set the element (<i>i,j</i>)
                                        * to @p value.
					*
					* Just as the respective call in
					* deal.II SparseMatrix<Number>
					* class (but in contrast to the
					* situation for PETSc based
					* matrices), this function
					* throws an exception if an
					* entry does not exist in the
					* sparsity pattern. Moreover, if
					* <tt>value</tt> is not a finite
					* number an exception is thrown.
					*/
      void set (const unsigned int i,
                const unsigned int j,
                const TrilinosScalar value);

                                       /**
                                        * Add @p value to the element
                                        * (<i>i,j</i>).
					*
					* Just as the respective call in
					* deal.II SparseMatrix<Number>
					* class (but in contrast to the
					* situation for PETSc based
					* matrices), this function
					* throws an exception if an
					* entry does not exist in the
					* sparsity pattern. Moreover, if
					* <tt>value</tt> is not a finite
					* number an exception is thrown.
                                        */
      void add (const unsigned int i,
                const unsigned int j,
                const TrilinosScalar value);
      
                                       /**
                                        * Multiply the entire matrix
                                        * by a fixed factor.
                                        */
      SparseMatrix & operator *= (const TrilinosScalar factor);
    
                                       /**
                                        * Divide the entire matrix by
                                        * a fixed factor.
                                        */
      SparseMatrix & operator /= (const TrilinosScalar factor);

				       /**
					* Copy the given matrix to this
					* one.
					*
					* The function returns a
					* reference to <tt>*this</tt>.
					*/
      SparseMatrix &
	copy_from (const SparseMatrix &source);

				       /**
					* Add <tt>matrix</tt> scaled
					* by <tt>factor</tt> to this
					* matrix, i.e. the matrix
					* <tt>factor*matrix</tt> is
					* added to <tt>this</tt>.
					*/
      void add (const TrilinosScalar  factor,
		const SparseMatrix   &matrix);

                                       /**
                                        * Remove all elements from
                                        * this <tt>row</tt> by setting
                                        * them to zero. The function
                                        * does not modify the number
                                        * of allocated nonzero
                                        * entries, it only sets some
                                        * entries to zero. It may drop
                                        * them from the sparsity
                                        * pattern, though (but retains
                                        * the allocated memory in case
                                        * new entries are again added
                                        * later). Note that this is a
                                        * global operation, so this
                                        * needs to be done on all MPI
                                        * processes.
                                        *
                                        * This operation is used in
                                        * eliminating constraints
                                        * (e.g. due to hanging nodes)
                                        * and makes sure that we can
                                        * write this modification to
                                        * the matrix without having to
                                        * read entries (such as the
                                        * locations of non-zero
                                        * elements) from it &mdash;
                                        * without this operation,
                                        * removing constraints on
                                        * parallel matrices is a
                                        * rather complicated
                                        * procedure.
                                        *
                                        * The second parameter can be
                                        * used to set the diagonal
                                        * entry of this row to a value
                                        * different from zero. The
                                        * default is to set it to
                                        * zero.
                                        */
      void clear_row (const unsigned int   row,
                      const TrilinosScalar new_diag_value = 0);

                                       /**
                                        * Same as clear_row(), except
                                        * that it works on a number of
                                        * rows at once.
                                        *
                                        * The second parameter can be
                                        * used to set the diagonal
                                        * entries of all cleared rows
                                        * to something different from
                                        * zero. Note that all of these
                                        * diagonal entries get the
                                        * same value -- if you want
                                        * different values for the
                                        * diagonal entries, you have
                                        * to set them by hand.
                                        */
      void clear_rows (const std::vector<unsigned int> &rows,
                       const TrilinosScalar             new_diag_value = 0);

                                       /**
					* Make an in-place transpose
					* of a matrix.
					*/
      void transpose ();

//@}
/**
 * @name Entry Access
 */
//@{

                                       /**
                                        * Return the value of the
                                        * entry (<i>i,j</i>).  This
                                        * may be an expensive
                                        * operation and you should
                                        * always take care where to
                                        * call this function. As in
                                        * the deal.II sparse matrix
                                        * class, we throw an exception
                                        * if the respective entry
                                        * doesn't exist in the
                                        * sparsity pattern of this
                                        * class, which is requested
                                        * from Trilinos. Moreover, an
                                        * exception will be thrown
                                        * when the requested element
                                        * is not saved on the calling
                                        * process.
                                        */
      TrilinosScalar operator () (const unsigned int i,
				  const unsigned int j) const;

                                       /**
                                        * Return the value of the
                                        * matrix entry
                                        * (<i>i,j</i>). If this entry
                                        * does not exist in the
                                        * sparsity pattern, then zero
                                        * is returned. While this may
                                        * be convenient in some cases,
                                        * note that it is simple to
                                        * write algorithms that are
                                        * slow compared to an optimal
                                        * solution, since the sparsity
                                        * of the matrix is not used.
                                        */
      TrilinosScalar el (const unsigned int i,
			 const unsigned int j) const;

                                       /**
                                        * Return the main diagonal
                                        * element in the <i>i</i>th
                                        * row. This function throws an
                                        * error if the matrix is not
                                        * quadratic.
                                        */
      TrilinosScalar diag_element (const unsigned int i) const;

//@}
/**
 * @name Matrix vector multiplications
 */
//@{

                                       /**
                                        * Matrix-vector multiplication:
                                        * let <i>dst = M*src</i> with
                                        * <i>M</i> being this matrix.
                                        *
                                        * Source and destination must
                                        * not be the same vector.
					*
					* Note that both vectors have to
					* be distributed vectors
					* generated using the same Map
					* as was used for the matrix in
					* case you work on a distributed
					* memory architecture, using the
					* interface in the
					* TrilinosWrappers::VectorBase
					* class (or one of the two
					* derived classes Vector and
					* MPI::Vector).
					*
					* In case of a localized Vector,
					* this function will only work
					* when running on one processor,
					* since the matrix object is
					* inherently
					* distributed. Otherwise, and
					* exception will be thrown.
                                        */
      void vmult (VectorBase       &dst,
                  const VectorBase &src) const;

                                       /**
                                        * Matrix-vector multiplication:
                                        * let <i>dst =
                                        * M<sup>T</sup>*src</i> with
                                        * <i>M</i> being this
                                        * matrix. This function does the
                                        * same as vmult() but takes the
                                        * transposed matrix.
                                        *
                                        * Source and destination must
                                        * not be the same vector.
					*
					* Note that both vectors have to
					* be distributed vectors
					* generated using the same Map
					* as was used for the matrix in
					* case you work on a distributed
					* memory architecture, using the
					* interface in the
					* TrilinosWrappers::VectorBase
					* class (or one of the two
					* derived classes Vector and
					* MPI::Vector).
					*
					* In case of a localized Vector,
					* this function will only work
					* when running on one processor,
					* since the matrix object is
					* inherently
					* distributed. Otherwise, and
					* exception will be thrown.
                                        */
      void Tvmult (VectorBase       &dst,
		   const VectorBase &src) const;

                                       /**
                                        * Adding Matrix-vector
                                        * multiplication. Add
                                        * <i>M*src</i> on <i>dst</i>
                                        * with <i>M</i> being this
                                        * matrix.
                                        *
                                        * Source and destination must
                                        * not be the same vector.
					*
					* Note that both vectors have to
					* be distributed vectors
					* generated using the same Map
					* as was used for the matrix in
					* case you work on a distributed
					* memory architecture, using the
					* interface in the
					* TrilinosWrappers::VectorBase
					* class (or one of the two
					* derived classes Vector and
					* MPI::Vector).
					*
					* In case of a localized Vector,
					* this function will only work
					* when running on one processor,
					* since the matrix object is
					* inherently
					* distributed. Otherwise, and
					* exception will be thrown.
                                        */
      void vmult_add (VectorBase       &dst,
                      const VectorBase &src) const;

                                       /**
                                        * Adding Matrix-vector
                                        * multiplication. Add
                                        * <i>M<sup>T</sup>*src</i> to
                                        * <i>dst</i> with <i>M</i> being
                                        * this matrix. This function
                                        * does the same as vmult_add()
                                        * but takes the transposed
                                        * matrix.
                                        *
                                        * Source and destination must
                                        * not be the same vector.
					*
					* Note that both vectors have to
					* be distributed vectors
					* generated using the same Map
					* as was used for the matrix in
					* case you work on a distributed
					* memory architecture, using the
					* interface in the
					* TrilinosWrappers::VectorBase
					* class (or one of the two
					* derived classes Vector and
					* MPI::Vector).
					*
					* In case of a localized Vector,
					* this function will only work
					* when running on one processor,
					* since the matrix object is
					* inherently
					* distributed. Otherwise, and
					* exception will be thrown.
                                        */
      void Tvmult_add (VectorBase       &dst,
                       const VectorBase &src) const;

                                       /**
                                        * Return the square of the norm
                                        * of the vector $v$ with respect
                                        * to the norm induced by this
                                        * matrix, i.e.,
                                        * $\left(v,Mv\right)$. This is
                                        * useful, e.g. in the finite
                                        * element context, where the
                                        * $L_2$ norm of a function
                                        * equals the matrix norm with
                                        * respect to the mass matrix of
                                        * the vector representing the
                                        * nodal values of the finite
                                        * element function.
                                        *
                                        * Obviously, the matrix needs to
                                        * be quadratic for this
                                        * operation.
                                        *
                                        * The implementation of this
                                        * function is not as efficient
                                        * as the one in the @p
                                        * SparseMatrix class used in
                                        * deal.II (i.e. the original
                                        * one, not the Trilinos wrapper
                                        * class) since Trilinos doesn't
                                        * support this operation and
                                        * needs a temporary vector.
					*
					* Note that both vectors have to
					* be distributed vectors
					* generated using the same Map
					* as was used for the matrix in
					* case you work on a distributed
					* memory architecture, using the
					* interface in the
					* TrilinosWrappers::VectorBase
					* class (or one of the two
					* derived classes Vector and
					* MPI::Vector).
					*
					* In case of a localized Vector,
					* this function will only work
					* when running on one processor,
					* since the matrix object is
					* inherently
					* distributed. Otherwise, and
					* exception will be thrown.
                                        */
      TrilinosScalar matrix_norm_square (const VectorBase &v) const;

                                       /**
                                        * Compute the matrix scalar
                                        * product $\left(u,Mv\right)$.
                                        *
                                        * The implementation of this
                                        * function is not as efficient
                                        * as the one in the @p
                                        * SparseMatrix class used in
                                        * deal.II (i.e. the original
                                        * one, not the Trilinos
                                        * wrapper class) since
                                        * Trilinos doesn't support
                                        * this operation and needs a
                                        * temporary vector.
					*
					* Note that both vectors have to
					* be distributed vectors
					* generated using the same Map
					* as was used for the matrix in
					* case you work on a distributed
					* memory architecture, using the
					* interface in the
					* TrilinosWrappers::VectorBase
					* class (or one of the two
					* derived classes Vector and
					* MPI::Vector).
					*
					* In case of a localized Vector,
					* this function will only work
					* when running on one processor,
					* since the matrix object is
					* inherently
					* distributed. Otherwise, and
					* exception will be thrown.
                                        */
      TrilinosScalar matrix_scalar_product (const VectorBase &u,
					    const VectorBase &v) const;

                                       /**
                                        * Compute the residual of an
                                        * equation <i>Mx=b</i>, where
                                        * the residual is defined to
                                        * be <i>r=b-Mx</i>. Write the
                                        * residual into @p dst. The
                                        * <i>l<sub>2</sub></i> norm of
                                        * the residual vector is
                                        * returned.
                                        *
                                        * Source <i>x</i> and
                                        * destination <i>dst</i> must
                                        * not be the same vector.
					*
					* Note that both vectors have to
					* be distributed vectors
					* generated using the same Map
					* as was used for the matrix in
					* case you work on a distributed
					* memory architecture, using the
					* interface in the
					* TrilinosWrappers::VectorBase
					* class (or one of the two
					* derived classes Vector and
					* MPI::Vector).
					*
					* In case of a localized Vector,
					* this function will only work
					* when running on one processor,
					* since the matrix object is
					* inherently
					* distributed. Otherwise, and
					* exception will be thrown.
                                        */
      TrilinosScalar residual (VectorBase       &dst,
			       const VectorBase &x,
			       const VectorBase &b) const;

//@}
/**
 * @name Matrix norms
 */
//@{
      
                                       /**
                                        * Return the
                                        * <i>l</i><sub>1</sub>-norm of
                                        * the matrix, that is
                                        * $|M|_1=
					* \max_{\mathrm{all columns } j}
					* \sum_{\mathrm{all rows } i} 
					* |M_{ij}|$, (max. sum
                                        * of columns).  This is the
                                        * natural matrix norm that is
                                        * compatible to the l1-norm for
                                        * vectors, i.e.  $|Mv|_1 \leq
                                        * |M|_1 |v|_1$.
                                        * (cf. Haemmerlin-Hoffmann:
                                        * Numerische Mathematik)
                                        */
      TrilinosScalar l1_norm () const;

                                       /**
                                        * Return the linfty-norm of the
                                        * matrix, that is
                                        * $|M|_\infty=\max_{\mathrm{all
                                        * rows} i}\sum_{\mathrm{all
                                        * columns} j} |M_{ij}|$,
                                        * (max. sum of rows).  This is
                                        * the natural matrix norm that
                                        * is compatible to the
                                        * linfty-norm of vectors, i.e.
                                        * $|Mv|_\infty \leq |M|_\infty
                                        * |v|_\infty$.
                                        * (cf. Haemmerlin-Hoffmann:
                                        * Numerische Mathematik)
                                        */
      TrilinosScalar linfty_norm () const;

                                       /**
                                        * Return the frobenius norm of
                                        * the matrix, i.e. the square
                                        * root of the sum of squares
                                        * of all entries in the
                                        * matrix.
                                        */
      TrilinosScalar frobenius_norm () const;

//@}
/**
 * @name Iterators
 */
//@{

                                       /**
                                        * STL-like iterator with the
                                        * first entry.
                                        */
      const_iterator begin () const;

                                       /**
                                        * Final iterator.
                                        */
      const_iterator end () const;

                                       /**
                                        * STL-like iterator with the
                                        * first entry of row @p r.
                                        *
                                        * Note that if the given row
                                        * is empty, i.e. does not
                                        * contain any nonzero entries,
                                        * then the iterator returned
                                        * by this function equals
                                        * <tt>end(r)</tt>. Note also
                                        * that the iterator may not be
                                        * dereferencable in that case.
                                        */
      const_iterator begin (const unsigned int r) const;

                                       /**
                                        * Final iterator of row
                                        * <tt>r</tt>. It points to the
                                        * first element past the end
                                        * of line @p r, or past the
                                        * end of the entire sparsity
                                        * pattern.
                                        *
                                        * Note that the end iterator
                                        * is not necessarily
                                        * dereferencable. This is in
                                        * particular the case if it is
                                        * the end iterator for the
                                        * last row of a matrix.
                                        */
      const_iterator end (const unsigned int r) const;

//@}
/**
 * @name Input/Output
 */
//@{

                                       /**
					* Abstract Trilinos object
					* that helps view in ASCII
					* other Trilinos
					* objects. Currently this
					* function is not
					* implemented.  TODO: Not
					* implemented.
					*/
      void write_ascii ();
      
				       /**
					* Print the matrix to the
					* given stream, using the
					* format <tt>(line,col)
					* value</tt>, i.e. one nonzero
					* entry of the matrix per
					* line.
					*/
      void print (std::ostream &out) const;
    
                                        // TODO: Write an overloading
                                        // of the operator << for output.
                                        // Since the underlying Trilinos 
                                        // object supports it, this should 
                                        // be very easy.

//@}
    				     /** @addtogroup Exceptions
				      * @{ */
                                       /**
                                        * Exception
                                        */
      DeclException1 (ExcTrilinosError,
                      int,
                      << "An error with error number " << arg1
                      << " occured while calling a Trilinos function");

				       /**
					* Exception
					*/
      DeclException2 (ExcInvalidIndex,
		      int, int,
		      << "The entry with index <" << arg1 << ',' << arg2
		      << "> does not exist.");

                                       /**
                                        * Exception
                                        */
      DeclException0 (ExcSourceEqualsDestination);

                                       /**
                                        * Exception
                                        */
      DeclException4 (ExcAccessToNonLocalElement,
		      int, int, int, int,
		      << "You tried to access element (" << arg1
		      << "/" << arg2 << ")"
		      << " of a distributed matrix, but only rows "
		      << arg3 << " through " << arg4
		      << " are stored locally and can be accessed.");

                                       /**
                                        * Exception
                                        */
      DeclException2 (ExcAccessToNonPresentElement,
		      int, int,
		      << "You tried to access element (" << arg1
		      << "/" << arg2 << ")"
		      << " of a sparse matrix, but it appears to not"
		      << " exist in the Trilinos sparsity pattern.");
				     //@}    
    private:
                                       /**
				        * Epetra Trilinos
				        * mapping of the matrix rows
				        * that assigns parts of the
				        * matrix to the individual
				        * processes. This map is
				        * provided either via the
				        * constructor or in a reinit
				        * function.
				        */
      Epetra_Map row_map;

                                       /**
                                        * Pointer to the user-supplied
				        * Epetra Trilinos mapping of
				        * the matrix columns that
				        * assigns parts of the matrix
				        * to the individual processes.
				        */
      Epetra_Map col_map;
  
                                       /**
                                        * Trilinos doesn't allow to
                                        * mix additions to matrix
                                        * entries and overwriting them
                                        * (to make synchronisation of
                                        * parallel computations
                                        * simpler). The way we do it
                                        * is to, for each access
                                        * operation, store whether it
                                        * is an insertion or an
                                        * addition. If the previous
                                        * one was of different type,
                                        * then we first have to flush
                                        * the Trilinos buffers;
                                        * otherwise, we can simply go
                                        * on.  Luckily, Trilinos has
                                        * an object for this which
                                        * does already all the
                                        * parallel communications in
                                        * such a case, so we simply
                                        * use their model, which
                                        * stores whether the last
                                        * operation was an addition or
                                        * an insertion.
                                        */
      Epetra_CombineMode last_action;

				       /**
					* A boolean variable to hold
					* information on whether the
					* vector is compressed or not.
					*/
      bool compressed;

				       /**
					* An internal Trilinos vector
					* that is used for
					* accelerating vmult_add
					* functions (do not need to
					* allocate too many temporary
					* vectors).
					*/
      mutable VectorBase temp_vector;

    public:
                                       /**
                                        * A sparse matrix object in
                                        * Trilinos to be used for
                                        * finite element based
                                        * problems which allows for
                                        * assembling into non-local
                                        * elements.  The actual type,
                                        * a sparse matrix, is set in
                                        * the constructor.
					*
					* TODO: This object should
					* finally become private.
                                        */
      std::auto_ptr<Epetra_FECrsMatrix> matrix;

  };



// -------------------------- inline and template functions ----------------------


#ifndef DOXYGEN

  namespace MatrixIterators
  {

    inline
    const_iterator::Accessor::
    Accessor (const SparseMatrix *matrix,
              const unsigned int  row,
              const unsigned int  index)
                    :
                    matrix(const_cast<SparseMatrix*>(matrix)),
                    a_row(row),
                    a_index(index)
    {
      visit_present_row ();
    }


    inline
    unsigned int
    const_iterator::Accessor::row() const
    {
      Assert (a_row < matrix->m(), ExcBeyondEndOfMatrix());
      return a_row;
    }



    inline
    unsigned int
    const_iterator::Accessor::column() const
    {
      Assert (a_row < matrix->m(), ExcBeyondEndOfMatrix());
      return (*colnum_cache)[a_index];
    }



    inline
    unsigned int
    const_iterator::Accessor::index() const
    {
      Assert (a_row < matrix->m(), ExcBeyondEndOfMatrix());
      return a_index;
    }



    inline
    TrilinosScalar
    const_iterator::Accessor::value() const
    {
      Assert (a_row < matrix->m(), ExcBeyondEndOfMatrix());
      return (*value_cache)[a_index];
    }



    inline
    const_iterator::
    const_iterator(const SparseMatrix *matrix,
                   const unsigned int  row,
                   const unsigned int  index)
                    :
                    accessor(matrix, row, index)
    {}



    inline
    const_iterator &
    const_iterator::operator++ ()
    {
      Assert (accessor.a_row < accessor.matrix->m(), ExcIteratorPastEnd());

      ++accessor.a_index;

				        // If at end of line: do one
				        // step, then cycle until we 
				        // find a row with a nonzero
				        // number of entries.
      if (accessor.a_index >= accessor.colnum_cache->size())
        {
          accessor.a_index = 0;
          ++accessor.a_row;
      
          while ((accessor.a_row < accessor.matrix->m())
                 &&
                 (accessor.matrix->row_length(accessor.a_row) == 0))
            ++accessor.a_row;

          accessor.visit_present_row();
        }
      return *this;
    }



    inline
    const_iterator
    const_iterator::operator++ (int)
    {
      const const_iterator old_state = *this;
      ++(*this);
      return old_state;
    }



    inline
    const const_iterator::Accessor &
    const_iterator::operator* () const
    {
      return accessor;
    }



    inline
    const const_iterator::Accessor *
    const_iterator::operator-> () const
    {
      return &accessor;
    }



    inline
    bool
    const_iterator::
    operator == (const const_iterator& other) const
    {
      return (accessor.a_row == other.accessor.a_row &&
              accessor.a_index == other.accessor.a_index);
    }



    inline
    bool
    const_iterator::
    operator != (const const_iterator& other) const
    {
      return ! (*this == other);
    }



    inline
    bool
    const_iterator::
    operator < (const const_iterator& other) const
    {
      return (accessor.row() < other.accessor.row() ||
              (accessor.row() == other.accessor.row() &&
               accessor.index() < other.accessor.index()));
    }
    
  }
 


  inline
  SparseMatrix::const_iterator
  SparseMatrix::begin() const
  {
    return const_iterator(this, 0, 0);
  }



  inline
  SparseMatrix::const_iterator
  SparseMatrix::end() const
  {
    return const_iterator(this, m(), 0);
  }



  inline
  SparseMatrix::const_iterator
  SparseMatrix::begin(const unsigned int r) const
  {
    Assert (r < m(), ExcIndexRange(r, 0, m()));
    if (row_length(r) > 0)
      return const_iterator(this, r, 0);
    else
      return end (r);
  }



  inline
  SparseMatrix::const_iterator
  SparseMatrix::end(const unsigned int r) const
  {
    Assert (r < m(), ExcIndexRange(r, 0, m()));

                                     // place the iterator on the first entry
                                     // past this line, or at the end of the
                                     // matrix
    for (unsigned int i=r+1; i<m(); ++i)
      if (row_length(i) > 0)
        return const_iterator(this, i, 0);
    
                                     // if there is no such line, then take the
                                     // end iterator of the matrix
    return end();
  }


  
  inline
  bool
  SparseMatrix::in_local_range (const unsigned int index) const
  {
    int begin, end;
    begin = matrix->RowMap().MinMyGID();
    end = matrix->RowMap().MaxMyGID()+1;
    
    return ((index >= static_cast<unsigned int>(begin)) &&
            (index < static_cast<unsigned int>(end)));
  }



  inline
  bool
  SparseMatrix::is_compressed () const
  {
    return compressed;
  }

#endif // DOXYGEN      
}


DEAL_II_NAMESPACE_CLOSE


#endif // DEAL_II_USE_TRILINOS


/*----------------------------   trilinos_sparse_matrix.h     ---------------------------*/

#endif
/*----------------------------   trilinos_sparse_matrix.h     ---------------------------*/
