//-------------------------------------------------------------------
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
//-------------------------------------------------------------------
#ifndef __deal2__block_matrix_array_h
#define __deal2__block_matrix_array_h

#include <base/subscriptor.h>
#include <base/smartpointer.h>
#include <vector>

template <typename> class BlockVector;


/**
 * Block matrix composed of different single matrices.
 *
 * Given a set of arbitrary matrices @p{A_i}, this class implements a
 * block matrix with block entries of the form @p{M_{jk} = s_{jk}A_i}.
 * Each @p{A_i} may be used several times with different prefix.
 *
 * Non-zero entries are registered by the function @p{enter}, zero
 * entries are not stored at all. Using @p{enter} with the same
 * location @p{(i,j)} several times will add the corresponding
 * matrices in matrix-vector multiplications.
 *
 * @subsection{Requirements}
 *
 * The template argument @p{MATRIX} is a class providing the the
 * matrix-vector multiplication functions @p{vmult} etc. defined in
 * this class, but with arguments of type @p{VECTOR} instead of
 * @p{BlockVector<VECTOR>}. @ref{SparseMatrix} is a possible entry
 * type.
 *
 * @author Guido Kanschat, 2000, 2001
 */
template <class MATRIX>
class BlockMatrixArray : public Subscriptor
{
  public:
				     /**
				      * Constructor fixing the
				      * dimensions.
				      */
    BlockMatrixArray (const unsigned int n_block_rows,
		      const unsigned int n_block_cols);

				     /**
				      * Add a block matrix entry.
				      */
    void enter (const MATRIX& matrix,
		unsigned row,
		unsigned int col,
		double prefix = 1.,
		bool transpose = false);

				     /**
				      * Number of block-entries per
				      * column.
				      */
    unsigned int n_block_rows () const;

				     /**
				      * Number of block-entries per
				      * row.
				      */
    unsigned int n_block_cols () const;

				     /**
				      * Matrix-vector multiplication.
				      */
    template <class VECTOR>
    void vmult (BlockVector<VECTOR>& dst,
		const BlockVector<VECTOR>& src) const;
  
				     /**
				      * Matrix-vector multiplication
				      * adding to @p{dst}.
				      */
    template <class VECTOR>
    void vmult_add (BlockVector<VECTOR>& dst,
		    const BlockVector<VECTOR>& src) const;
  
				     /**
				      * Transposed matrix-vector
				      * multiplication.
				      */
    template <class VECTOR>
    void Tvmult (BlockVector<VECTOR>& dst,
		 const BlockVector<VECTOR>& src) const;
  
				     /**
				      * Transposed matrix-vector
				      * multiplication adding to
				      * @p{dst}.
				      */
    template <class VECTOR>
    void Tvmult_add (BlockVector<VECTOR>& dst,
		     const BlockVector<VECTOR>& src) const;
  
  private:
				     /**
				      * Internal data structure.
				      *
				      * For each entry of a
				      * @p{BlockMatrixArray}, its
				      * position, matrix, prefix and
				      * optional transposition must be
				      * stored. This structure
				      * encapsulates all of them.
				      *
				      * @author Guido Kanschat, 2000, 2001
				      */
    class Entry
    {
      public:
					 /**
					  * Constructor initializing all
					  * data fields.
					  */
	Entry (const MATRIX& matrix,
	       unsigned row, unsigned int col,
	       double prefix, bool transpose);
    
					 /**
					  * Row number in the block
					  * matrix.
					  */
	unsigned int row;

					 /**
					  * Column number in the block
					  * matrix.
					  */
	unsigned int col;

					 /**
					  * Factor in front of the matrix
					  * block.
					  */
	double prefix;

					 /**
					  * Indicates that matrix block
					  * must be transposed for
					  * multiplication.
					  */
	bool transpose;

					 /**
					  * The matrix block itself.
					  */
	SmartPointer<const MATRIX> matrix;
    };
  
				     /**
				      * Array of block entries in the
				      * matrix.
				      */
    vector<Entry> entries;

				     /**
				      * Number of blocks per column.
				      */
    unsigned int block_rows;
				     /**
				      * number of blocks per row.
				      */
    unsigned int block_cols;
};


template <class MATRIX>
inline
BlockMatrixArray<MATRIX>::Entry::Entry (const MATRIX& matrix,
					unsigned row, unsigned int col,
					double prefix, bool transpose)
		:
		row (row),
		col (col),
		prefix (prefix),
		transpose (transpose),
		matrix (&matrix)
{}



template <class MATRIX>
inline
BlockMatrixArray<MATRIX>::BlockMatrixArray (const unsigned int n_block_rows,
					    const unsigned int n_block_cols)
		: block_rows (n_block_rows),
		  block_cols (n_block_cols)
{}



template <class MATRIX>
inline
void
BlockMatrixArray<MATRIX>::enter (const MATRIX& matrix,
				 unsigned row, unsigned int col,
				 double prefix, bool transpose)
{
  entries.push_back(Entry(matrix, row, col, prefix, transpose));
}



template <class MATRIX>
template <class VECTOR>
inline
void
BlockMatrixArray<MATRIX>::vmult_add (BlockVector<VECTOR>& dst,
				     const BlockVector<VECTOR>& src) const
{
  Assert (dst.n_blocks() == block_rows,
	  ExcDimensionMismatch(dst.n_blocks(), block_rows));
  Assert (src.n_blocks() == block_cols,
	  ExcDimensionMismatch(src.n_blocks(), block_cols));

  typename vector<Entry>::const_iterator m = entries.begin();
  typename vector<Entry>::const_iterator end = entries.end();
  
  for (; m != end ; ++m)
    {
      Assert (m->prefix==1., ExcNotImplemented());
      if (m->transpose)
	m->matrix->Tvmult_add(dst.block(m->row),
			      src.block(m->col));
      else
	m->matrix->vmult_add(dst.block(m->row),
			     src.block(m->col));
    }
}




template <class MATRIX>
template <class VECTOR>
inline
void
BlockMatrixArray<MATRIX>::vmult (BlockVector<VECTOR>& dst,
				 const BlockVector<VECTOR>& src) const
{
  dst.equ = 0.;
  vmult_add (dst, src);
}




template <class MATRIX>
template <class VECTOR>
inline
void
BlockMatrixArray<MATRIX>::Tvmult_add (BlockVector<VECTOR>& dst,
				      const BlockVector<VECTOR>& src) const
{
  Assert (dst.n_blocks() == block_cols,
	  ExcDimensionMismatch(dst.n_blocks(), block_cols));
  Assert (src.n_blocks() == block_rows,
	  ExcDimensionMismatch(src.n_blocks(), block_rows));

  typename vector<Entry>::const_iterator m = entries.begin();
  typename vector<Entry>::const_iterator end = entries.end();
  
  for (; m != end ; ++m)
    {
      Assert (m->prefix==1., ExcNotImplemented());
      if (m->transpose)
	m->matrix->vmult_add(dst.block(m->col),
			     src.block(m->row));
      else
	m->matrix->Tvmult_add(dst.block(m->col),
			      src.block(m->row));
    }
}



template <class MATRIX>
template <class VECTOR>
inline
void
BlockMatrixArray<MATRIX>::Tvmult (BlockVector<VECTOR>& dst,
				  const BlockVector<VECTOR>& src) const
{
  dst = 0.;
  Tvmult_add (dst, src);
}




template <class MATRIX>
inline
unsigned int
BlockMatrixArray<MATRIX>::n_block_rows () const
{
  return block_rows;
}



template <class MATRIX>
inline
unsigned int
BlockMatrixArray<MATRIX>::n_block_cols () const
{
  return block_cols;
}




#endif
