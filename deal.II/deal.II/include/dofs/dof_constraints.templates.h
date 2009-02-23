//---------------------------------------------------------------------------
//    $Id$
//    Version: $Name$
//
//    Copyright (C) 1999, 2000, 2001, 2002, 2003, 2004, 2005, 2006, 2007, 2008 by the deal.II authors
//
//    This file is subject to QPL and may not be  distributed
//    without copyright and license information. Please refer
//    to the file deal.II/doc/license.html for the  text  and
//    further information on this license.
//
//---------------------------------------------------------------------------
#ifndef __deal2__dof_constraints_templates_h
#define __deal2__dof_constraints_templates_h


#include <base/config.h>
#include <dofs/dof_constraints.h>
#include <lac/vector.h>
#include <lac/full_matrix.h>
#include <lac/sparsity_pattern.h>
#include <lac/sparse_matrix.h>
#include <lac/block_sparsity_pattern.h>
#include <lac/block_sparse_matrix.h>

DEAL_II_NAMESPACE_OPEN


template<typename number>
void
ConstraintMatrix::condense (const SparseMatrix<number> &uncondensed,
			    SparseMatrix<number>       &condensed) const
{
				   // create two dummy vectors and enter the
				   // other function
  Vector<number> in (0), out(0);
  condense (uncondensed, in, condensed, out);
}



template<typename number>
void
ConstraintMatrix::condense (SparseMatrix<number> &uncondensed) const
{
  Vector<number> dummy (0);
  condense (uncondensed, dummy);
}



template <typename number>
void
ConstraintMatrix::condense (BlockSparseMatrix<number> &uncondensed) const
{
  BlockVector<number> dummy (0);
  condense (uncondensed, dummy);
}



template<class VectorType>
void
ConstraintMatrix::condense (const VectorType &uncondensed,
			    VectorType       &condensed) const
{
  Assert (sorted == true, ExcMatrixNotClosed());
  Assert (condensed.size()+n_constraints() == uncondensed.size(),
	  ExcDimensionMismatch(condensed.size()+n_constraints(),
			       uncondensed.size()));
  
				   // store for each line of the
				   // vector its new line number after
				   // compression. If the shift is -1,
				   // this line will be condensed away
  std::vector<int> new_line;

  new_line.reserve (uncondensed.size());

  std::vector<ConstraintLine>::const_iterator next_constraint = lines.begin();
  unsigned int                                shift           = 0;
  unsigned int n_rows = uncondensed.size();

  if (next_constraint == lines.end()) 
				     // if no constraint is to be handled
    for (unsigned int row=0; row!=n_rows; ++row)
      new_line.push_back (row);
  else
    for (unsigned int row=0; row!=n_rows; ++row)
      if (row == next_constraint->line)
	{
					   // this line is constrained
	  new_line.push_back (-1);
					   // note that @p lines is ordered
	  ++shift;
	  ++next_constraint;
	  if (next_constraint == lines.end())
					     // nothing more to do; finish rest
					     // of loop
	    {
	      for (unsigned int i=row+1; i<n_rows; ++i)
		new_line.push_back (i-shift);
	      break;
	    };
	}
      else
	new_line.push_back (row-shift);


  next_constraint = lines.begin();
				   // note: in this loop we need not check
				   // whether @p next_constraint is a valid
				   // iterator, since @p next_constraint is
				   // only evaluated so often as there are
				   // entries in new_line[*] which tells us
				   // which constraints exist
  for (unsigned int row=0; row<uncondensed.size(); ++row)
    if (new_line[row] != -1)
				       // line not constrained
				       // copy entry
      condensed(new_line[row]) += uncondensed(row);

    else
				       // line must be distributed
      {
	for (unsigned int q=0; q!=next_constraint->entries.size(); ++q) 
	  condensed(new_line[next_constraint->entries[q].first])
	    +=
	    uncondensed(row) * next_constraint->entries[q].second;

	++next_constraint;
      };
}



template <class VectorType>
void
ConstraintMatrix::condense (VectorType &vec) const
{
  Assert (sorted == true, ExcMatrixNotClosed());

                                   // distribute all entries, and set them to zero
  std::vector<ConstraintLine>::const_iterator constraint_line = lines.begin();
  for (; constraint_line!=lines.end(); ++constraint_line)
    {
      for (unsigned int q=0; q!=constraint_line->entries.size(); ++q) 
        vec(constraint_line->entries[q].first)
          += (vec(constraint_line->line) *
              constraint_line->entries[q].second);
      vec(constraint_line->line) = 0.;

				   // in case the constraint is
				   // inhomogeneous, this function is not
				   // appropriate. Throw an exception.
      Assert (constraint_line->inhomogeneity == 0.,
	      ExcMessage ("Inhomogeneous constraint cannot be condensed "
			  "without any matrix specified."));
    }
}



template<typename number, class VectorType>
void
ConstraintMatrix::condense (const SparseMatrix<number> &uncondensed,
			    const VectorType           &uncondensed_vector,
			    SparseMatrix<number>       &condensed,
			    VectorType                 &condensed_vector) const
{
				   // check whether we work on real vectors
				   // or we just used a dummy when calling
				   // the other function above.
  const bool use_vectors = (uncondensed_vector.size() == 0 && 
			    condensed_vector.size() == 0) ? false : true;

  const SparsityPattern &uncondensed_struct = uncondensed.get_sparsity_pattern ();
  
  Assert (sorted == true, ExcMatrixNotClosed());
  Assert (uncondensed_struct.is_compressed() == true, ExcMatrixNotClosed());
  Assert (condensed.get_sparsity_pattern().is_compressed() == true, ExcMatrixNotClosed());
  Assert (uncondensed_struct.n_rows() == uncondensed_struct.n_cols(),
	  ExcNotQuadratic());
  Assert (condensed.n() == condensed.m(),
	  ExcNotQuadratic());
  Assert (condensed.n()+n_constraints() == uncondensed.n(),
	  ExcDimensionMismatch(condensed.n()+n_constraints(), uncondensed.n()));
  if (use_vectors == true)
    {
      Assert (condensed_vector.size()+n_constraints() == uncondensed_vector.size(),
	      ExcDimensionMismatch(condensed_vector.size()+n_constraints(),
				   uncondensed_vector.size()));
      Assert (condensed_vector.size() == condensed.m(),
	      ExcDimensionMismatch(condensed_vector.size(), condensed.m()));
    }

				   // store for each line of the matrix
				   // its new line number
				   // after compression. If the shift is
				   // -1, this line will be condensed away
  std::vector<int> new_line;

  new_line.reserve (uncondensed_struct.n_rows());

  std::vector<ConstraintLine>::const_iterator next_constraint = lines.begin();
  unsigned int                                shift           = 0;
  const unsigned int n_rows = uncondensed_struct.n_rows();

  if (next_constraint == lines.end()) 
				     // if no constraint is to be handled
    for (unsigned int row=0; row!=n_rows; ++row)
      new_line.push_back (row);
  else
    for (unsigned int row=0; row!=n_rows; ++row)
      if (row == next_constraint->line)
	{
					   // this line is constrained
	  new_line.push_back (-1);
					   // note that @p lines is ordered
	  ++shift;
	  ++next_constraint;
	  if (next_constraint == lines.end())
					     // nothing more to do; finish rest
					     // of loop
	    {
	      for (unsigned int i=row+1; i<n_rows; ++i)
		new_line.push_back (i-shift);
	      break;
	    };
	}
      else
	new_line.push_back (row-shift);


  next_constraint = lines.begin();

				   // note: in this loop we need not check
				   // whether @p next_constraint is a valid
				   // iterator, since @p next_constraint is
				   // only evaluated so often as there are
				   // entries in new_line[*] which tells us
				   // which constraints exist
  for (unsigned int row=0; row<uncondensed_struct.n_rows(); ++row)
    if (new_line[row] != -1)
      {
				       // line not constrained
				       // copy entries if column will not
				       // be condensed away, distribute
				       // otherwise
	for (unsigned int j=uncondensed_struct.get_rowstart_indices()[row];
	     j<uncondensed_struct.get_rowstart_indices()[row+1]; ++j)
	  if (new_line[uncondensed_struct.get_column_numbers()[j]] != -1)
	    condensed.add (new_line[row], new_line[uncondensed_struct.get_column_numbers()[j]],
			   uncondensed.global_entry(j));
	  else 
	    {
					     // let c point to the
					     // constraint of this column
	      std::vector<ConstraintLine>::const_iterator c = lines.begin();
	      while (c->line != uncondensed_struct.get_column_numbers()[j])
		++c;

	      for (unsigned int q=0; q!=c->entries.size(); ++q)
					       // distribute to rows with
					       // appropriate weight
		condensed.add (new_line[row], new_line[c->entries[q].first],
			       uncondensed.global_entry(j) * c->entries[q].second);

				   // take care of inhomogeneity:
				   // need to subtract this element from the
				   // vector. this corresponds to an
				   // explicit elimination in the respective
				   // row of the inhomogeneous constraint in
				   // the matrix with Gauss elimination
	      if (use_vectors == true)
		condensed_vector(new_line[row]) -= uncondensed.global_entry(j) * 
		                                   c->inhomogeneity;
	    }

	if (use_vectors == true)
	  condensed_vector(new_line[row]) += uncondensed_vector(row);	  
      }
    else
				       // line must be distributed
      {
	for (unsigned int j=uncondensed_struct.get_rowstart_indices()[row];
	     j<uncondensed_struct.get_rowstart_indices()[row+1]; ++j)
					   // for each column: distribute
	  if (new_line[uncondensed_struct.get_column_numbers()[j]] != -1)
					     // column is not constrained
	    for (unsigned int q=0; q!=next_constraint->entries.size(); ++q) 
	      condensed.add (new_line[next_constraint->entries[q].first],
			     new_line[uncondensed_struct.get_column_numbers()[j]],
			     uncondensed.global_entry(j) *
			     next_constraint->entries[q].second);
	
	  else
					     // not only this line but
					     // also this col is constrained
	    {
					       // let c point to the constraint
					       // of this column
	      std::vector<ConstraintLine>::const_iterator c = lines.begin();
	      while (c->line != uncondensed_struct.get_column_numbers()[j])
		++c;
	      
	      for (unsigned int p=0; p!=c->entries.size(); ++p)
		for (unsigned int q=0; q!=next_constraint->entries.size(); ++q)
		  condensed.add (new_line[next_constraint->entries[q].first],
				 new_line[c->entries[p].first],
				 uncondensed.global_entry(j) *
				 next_constraint->entries[q].second *
				 c->entries[p].second);
	    };

				   // condense the vector
	if (use_vectors == true)
	  for (unsigned int q=0; q!=next_constraint->entries.size(); ++q) 
	    condensed_vector(new_line[next_constraint->entries[q].first])
	      +=
	      uncondensed_vector(row) * next_constraint->entries[q].second;

	++next_constraint;
      };
}



template<typename number, class VectorType>
void
ConstraintMatrix::condense (SparseMatrix<number> &uncondensed,
			    VectorType           &vec) const
{
				   // check whether we work on real vectors
				   // or we just used a dummy when calling
				   // the other function above.
  const bool use_vectors = vec.size() == 0 ? false : true;

  const SparsityPattern &sparsity = uncondensed.get_sparsity_pattern ();

  Assert (sorted == true, ExcMatrixNotClosed());
  Assert (sparsity.is_compressed() == true, ExcMatrixNotClosed());
  Assert (sparsity.n_rows() == sparsity.n_cols(),
	  ExcNotQuadratic());
  if (use_vectors == true)
    {
      Assert (vec.size() == sparsity.n_rows(), 
	      ExcDimensionMismatch(vec.size(), sparsity.n_rows()));
    }

  double average_diagonal = 0;
  for (unsigned int i=0; i<uncondensed.m(); ++i)
    average_diagonal += std::fabs (uncondensed.diag_element(i));
  average_diagonal /= uncondensed.m();
  
				   // store for each index whether it must be
				   // distributed or not. If entry is
				   // invalid_unsigned_int, no distribution is
				   // necessary.  otherwise, the number states
				   // which line in the constraint matrix
				   // handles this index
  std::vector<unsigned int> distribute (sparsity.n_rows(),
                                        numbers::invalid_unsigned_int);
  
  for (unsigned int c=0; c<lines.size(); ++c)
    distribute[lines[c].line] = c;

  const unsigned int n_rows = sparsity.n_rows();
  for (unsigned int row=0; row<n_rows; ++row)
    {
      if (distribute[row] == numbers::invalid_unsigned_int)
					 // regular line. loop over cols
        {
          for (typename SparseMatrix<number>::iterator
                 entry = uncondensed.begin(row);
               entry != uncondensed.end(row); ++entry)
            {
              const unsigned int column = entry->column();
              
                                               // end of row reached?
                                               // this should not
                                               // happen, since we only
                                               // operate on compressed
                                               // matrices!
              Assert (column != SparsityPattern::invalid_entry,
                      ExcMatrixNotClosed());
	    
              if (distribute[column] != numbers::invalid_unsigned_int)
                                                 // distribute entry at
                                                 // regular row @p row
                                                 // and irregular column
                                                 // sparsity.get_column_numbers()[j];
                                                 // set old entry to
                                                 // zero
                {
                  for (unsigned int q=0;
                       q!=lines[distribute[column]].entries.size(); ++q)
                    uncondensed.add (row,
                                     lines[distribute[column]].entries[q].first,
                                     entry->value() *
                                     lines[distribute[column]].entries[q].second);

				   // need to subtract this element from the
				   // vector. this corresponds to an
				   // explicit elimination in the respective
				   // row of the inhomogeneous constraint in
				   // the matrix with Gauss elimination
		  if (use_vectors == true)
		    vec(column) -= entry->value() * 
		                   lines[distribute[column]].inhomogeneity;

                                                   // set old value to zero
                  entry->value() = 0.;
                }
            }
        }
      else
					 // row must be distributed
        {
          for (typename SparseMatrix<number>::iterator
                 entry = uncondensed.begin(row);
               entry != uncondensed.end(row); ++entry)
            {
              const unsigned int column = entry->column();

                                               // end of row reached?
                                               // this should not
                                               // happen, since we only
                                               // operate on compressed
                                               // matrices!
              Assert (column != SparsityPattern::invalid_entry,
                      ExcMatrixNotClosed());

              if (distribute[column] == numbers::invalid_unsigned_int)
                                                 // distribute entry at
                                                 // irregular row
                                                 // @p row and regular
                                                 // column
                                                 // column. set
                                                 // old entry to zero
                {
                  for (unsigned int q=0;
                       q!=lines[distribute[row]].entries.size(); ++q) 
                    uncondensed.add (lines[distribute[row]].entries[q].first,
                                     column,
                                     entry->value() *
                                     lines[distribute[row]].entries[q].second);

                                                   // set old entry to zero
                  entry->value() = 0.;
                }
              else
                                                 // distribute entry at
                                                 // irregular row @p row and
                                                 // irregular column
                                                 // @p column set old entry
                                                 // to one on main
                                                 // diagonal, zero otherwise
                {
                  for (unsigned int p=0; p!=lines[distribute[row]].entries.size(); ++p)
		    {
		      for (unsigned int q=0;
			   q!=lines[distribute[column]].entries.size(); ++q)
			uncondensed.add (lines[distribute[row]].entries[p].first,
					 lines[distribute[column]].entries[q].first,
					 entry->value() *
					 lines[distribute[row]].entries[p].second *
					 lines[distribute[column]].entries[q].second);

		      if (use_vectors == true)
			vec(lines[distribute[row]].entries[p].first) -= 
			  entry->value() * lines[distribute[row]].entries[p].second *
			  lines[distribute[row]].inhomogeneity;
		    }

                                                   // set old entry to correct
                                                   // value
                  entry->value() = (row == column ? average_diagonal : 0. );
                }
            }

				   // take care of vector
	  if (use_vectors == true)
	    {
	      for (unsigned int q=0; q!=lines[distribute[row]].entries.size(); ++q) 
		vec(lines[distribute[row]].entries[q].first)
		  += (vec(row) * lines[distribute[row]].entries[q].second);

	      vec(lines[distribute[row]].line) = 0.;
	    }
        }
    }
}



template <typename number, class BlockVectorType>
void
ConstraintMatrix::condense (BlockSparseMatrix<number> &uncondensed,
			    BlockVectorType           &vec) const
{
				   // check whether we work on real vectors
				   // or we just used a dummy when calling
				   // the other function above.
  const bool use_vectors = vec.n_blocks() == 0 ? false : true;

  const unsigned int blocks = uncondensed.n_block_rows();
  
  const BlockSparsityPattern &
    sparsity = uncondensed.get_sparsity_pattern ();

  Assert (sorted == true, ExcMatrixNotClosed());
  Assert (sparsity.is_compressed() == true, ExcMatrixNotClosed());
  Assert (sparsity.n_rows() == sparsity.n_cols(),
	  ExcNotQuadratic());
  Assert (sparsity.n_block_rows() == sparsity.n_block_cols(),
	  ExcNotQuadratic());
  Assert (sparsity.n_block_rows() == sparsity.n_block_cols(),
	  ExcNotQuadratic());
  Assert (sparsity.get_column_indices() == sparsity.get_row_indices(),
	  ExcNotQuadratic());

  if (use_vectors == true)
    {
      Assert (vec.size() == sparsity.n_rows(), 
	      ExcDimensionMismatch(vec.size(), sparsity.n_rows()));
      Assert (vec.n_blocks() == sparsity.n_block_rows(),
	      ExcDimensionMismatch(vec.n_blocks(), sparsity.n_block_rows()));
    }

  double average_diagonal = 0;
  for (unsigned int b=0; b<uncondensed.n_block_rows(); ++b)
    for (unsigned int i=0; i<uncondensed.block(b,b).m(); ++i)
      average_diagonal += std::fabs (uncondensed.block(b,b).diag_element(i));
  average_diagonal /= uncondensed.m();

  const BlockIndices &
    index_mapping = sparsity.get_column_indices();
  
				   // store for each index whether it must be
				   // distributed or not. If entry is
				   // numbers::invalid_unsigned_int,
				   // no distribution is necessary.
				   // otherwise, the number states which line
				   // in the constraint matrix handles this
				   // index
  std::vector<unsigned int> distribute (sparsity.n_rows(),
                                        numbers::invalid_unsigned_int);
  
  for (unsigned int c=0; c<lines.size(); ++c)
    distribute[lines[c].line] = c;

  const unsigned int n_rows = sparsity.n_rows();
  for (unsigned int row=0; row<n_rows; ++row)
    {
				       // get index of this row
				       // within the blocks
      const std::pair<unsigned int,unsigned int>
	block_index = index_mapping.global_to_local(row);
      const unsigned int block_row = block_index.first;
      
      if (distribute[row] == numbers::invalid_unsigned_int)
					 // regular line. loop over
					 // all columns and see
					 // whether this column must
					 // be distributed
	{

					   // to loop over all entries
					   // in this row, we have to
					   // loop over all blocks in
					   // this blockrow and the
					   // corresponding row
					   // therein
	  for (unsigned int block_col=0; block_col<blocks; ++block_col)
	    {
              for (typename SparseMatrix<number>::iterator
                     entry = uncondensed.block(block_row, block_col).begin(block_index.second);
                   entry != uncondensed.block(block_row, block_col).end(block_index.second);
                   ++entry)
                {
                  const unsigned int global_col
                    = index_mapping.local_to_global(block_col,entry->column());
		    
                  if (distribute[global_col] != numbers::invalid_unsigned_int)
                                                     // distribute entry at
                                                     // regular row @p row
                                                     // and irregular column
                                                     // global_col; set old
                                                     // entry to zero
                    {
                      const double old_value = entry->value ();
			
                      for (unsigned int q=0;
                           q!=lines[distribute[global_col]].entries.size(); ++q)
                        uncondensed.add (row,
                                         lines[distribute[global_col]].entries[q].first,
                                         old_value *
                                         lines[distribute[global_col]].entries[q].second);

				   // need to subtract this element from the
				   // vector. this corresponds to an
				   // explicit elimination in the respective
				   // row of the inhomogeneous constraint in
				   // the matrix with Gauss elimination
		      if (use_vectors == true)
			vec(global_col) -= entry->value() * 
			                   lines[distribute[global_col]].inhomogeneity;

                      entry->value() = 0.;
                    }
                }
	    }
	}
      else
	{
					   // row must be
					   // distributed. split the
					   // whole row into the
					   // chunks defined by the
					   // blocks
	  for (unsigned int block_col=0; block_col<blocks; ++block_col)
	    {
              for (typename SparseMatrix<number>::iterator
                     entry = uncondensed.block(block_row, block_col).begin(block_index.second);
                   entry != uncondensed.block(block_row, block_col).end(block_index.second);
                   ++entry)
                {
                  const unsigned int global_col
                    = index_mapping.local_to_global (block_col, entry->column());
		    
                  if (distribute[global_col] ==
                      numbers::invalid_unsigned_int)
                                                     // distribute
                                                     // entry at
                                                     // irregular
                                                     // row @p row
                                                     // and regular
                                                     // column
                                                     // global_col. set
                                                     // old entry to
                                                     // zero
                    {
                      const double old_value = entry->value();
			  
                      for (unsigned int q=0;
                           q!=lines[distribute[row]].entries.size(); ++q) 
                        uncondensed.add (lines[distribute[row]].entries[q].first,
                                         global_col,
                                         old_value *
                                         lines[distribute[row]].entries[q].second);

                      entry->value() = 0.;
                    }
                  else
                                                     // distribute entry at
                                                     // irregular row @p row
                                                     // and irregular column
                                                     // @p global_col set old
                                                     // entry to one if on
                                                     // main diagonal, zero
                                                     // otherwise
                    {
                      const double old_value = entry->value ();
			  
                      for (unsigned int p=0; p!=lines[distribute[row]].entries.size(); ++p)
			{
			  for (unsigned int q=0; q!=lines[distribute[global_col]].entries.size(); ++q)
			    uncondensed.add (lines[distribute[row]].entries[p].first,
					     lines[distribute[global_col]].entries[q].first,
					     old_value *
					     lines[distribute[row]].entries[p].second *
					     lines[distribute[global_col]].entries[q].second);

			  if (use_vectors == true)
			    vec(lines[distribute[row]].entries[p].first) -= 
			      old_value * lines[distribute[row]].entries[p].second *
			      lines[distribute[global_col]].inhomogeneity;
			}

                      entry->value() = (row == global_col ? average_diagonal : 0. );
                    }
                }
	    }

	  				   // take care of vector
	  if (use_vectors == true)
	    {
	      for (unsigned int q=0; q!=lines[distribute[row]].entries.size(); ++q) 
		vec(lines[distribute[row]].entries[q].first)
		  += (vec(row) * lines[distribute[row]].entries[q].second);

	      vec(lines[distribute[row]].line) = 0.;
	    }
	}
    }
}



template <class VectorType>
void
ConstraintMatrix::set_zero (VectorType &vec) const
{
  Assert (sorted == true, ExcMatrixNotClosed());

  std::vector<ConstraintLine>::const_iterator constraint_line = lines.begin();
  for (; constraint_line!=lines.end(); ++constraint_line)
    vec(constraint_line->line) = 0.;
}



template <typename VectorType>
void
ConstraintMatrix::
distribute_local_to_global (const Vector<double>            &local_vector,
                            const std::vector<unsigned int> &local_dof_indices,
                            VectorType                      &global_vector) const
{
  Assert (local_vector.size() == local_dof_indices.size(),
          ExcDimensionMismatch(local_vector.size(), local_dof_indices.size()));
  Assert (sorted == true, ExcMatrixNotClosed());

  const unsigned int n_local_dofs = local_vector.size();
  
                                   // have a special case where there are no
                                   // constraints at all, since then we can be
                                   // a lot faster
  if (lines.size() == 0)
    {
      for (unsigned int i=0; i<n_local_dofs; ++i)
        global_vector(local_dof_indices[i]) += local_vector(i);
    }
  else
    {
      for (unsigned int i=0; i<n_local_dofs; ++i)
        {
                                           // first figure out whether this
                                           // dof is constrained
          ConstraintLine index_comparison;
          index_comparison.line = local_dof_indices[i];

          const std::vector<ConstraintLine>::const_iterator
            position = std::lower_bound (lines.begin(),
                                         lines.end(),
                                         index_comparison);

                                           // if the line is not
                                           // constrained, then simply
                                           // copy the data. otherwise
                                           // distribute it, but make
                                           // sure we don't touch the
                                           // entries of fixed dofs
					   //
					   // there is one critical
					   // point: sometimes a dof
					   // may be both constrained
					   // and fixed, for example
					   // hanging nodes in 3d at
					   // the boundary. in that
					   // case, we don't quite
					   // know what to do --
					   // handle the constraint or
					   // the fixed
					   // value. however, this
					   // isn't so hard if all the
					   // nodes that this node is
					   // constrained to are also
					   // fixed nodes, in which
					   // case we could do both
					   // but opt to copy the
					   // element. however, we
					   // have to check that all
					   // the nodes to which it is
					   // constrained are also
					   // fixed
          if ((position == lines.end())
	      ||
	      (position->line != local_dof_indices[i]))
            global_vector(local_dof_indices[i]) += local_vector(i);
          else
	    {
              for (unsigned int j=0; j<position->entries.size(); ++j)
                global_vector(position->entries[j].first)
                  += local_vector(i) * position->entries[j].second;
	    }
        }
    }
}



template <typename MatrixType>
void
ConstraintMatrix::
distribute_local_to_global (const FullMatrix<double>        &local_matrix,
                            const std::vector<unsigned int> &local_dof_indices,
                            MatrixType                      &global_matrix) const
{
  Vector<double> local_dummy(0), global_dummy (0);
  distribute_local_to_global (local_matrix, local_dummy, local_dof_indices,
			      global_matrix, global_dummy);
}



template <typename MatrixType, class VectorType>
void
ConstraintMatrix::
distribute_local_to_global (const FullMatrix<double>        &local_matrix,
			    const Vector<double>            &local_vector,
                            const std::vector<unsigned int> &local_dof_indices,
                            MatrixType                      &global_matrix,
			    VectorType                      &global_vector) const
{
				   // check whether we work on real vectors
				   // or we just used a dummy when calling
				   // the other function above.
  const bool use_vectors = (local_vector.size() == 0 && 
			    global_vector.size() == 0) ? false : true;

  Assert (local_matrix.n() == local_dof_indices.size(),
          ExcDimensionMismatch(local_matrix.n(), local_dof_indices.size()));
  Assert (local_matrix.m() == local_dof_indices.size(),
          ExcDimensionMismatch(local_matrix.m(), local_dof_indices.size()));
  Assert (global_matrix.m() == global_matrix.n(), ExcNotQuadratic());
  if (use_vectors == true)
    {
      Assert (local_matrix.m() == local_vector.size(),
	      ExcDimensionMismatch(local_matrix.m(), local_vector.size()));
      Assert (global_matrix.m() == global_vector.size(),
	      ExcDimensionMismatch(global_matrix.m(), global_vector.size()));
    }
  Assert (sorted == true, ExcMatrixNotClosed());

  const unsigned int n_local_dofs = local_dof_indices.size();

                                   // A lock that allows only one thread at
				   // time to go on in this function.
  Threads::ThreadMutex::ScopedLock lock(mutex);
  
                                   // have a special case where there are no
                                   // constraints at all, since then we can be
                                   // a lot faster
  if (lines.size() == 0)
    {
      global_matrix.add(local_dof_indices, local_matrix);
      if (use_vectors == true)
	for (unsigned int i=0; i<local_dof_indices.size(); ++i)
	  global_vector(local_dof_indices[i]) += local_vector(i);
    }
  else
    {
                                       // here we have to do something a
                                       // little nastier than in the
                                       // respective function for
                                       // vectors. the reason is that we
                                       // have two nested loops and we don't
                                       // want to repeatedly check whether a
                                       // certain dof is constrained or not
                                       // by searching over all the
                                       // constrained dofs. so we have to
                                       // cache this knowledge, by storing
                                       // for each dof index whether and
                                       // where the line of the constraint
                                       // matrix is located. Moreover, we
                                       // store how many entries there are
                                       // at most in one constrained row in
                                       // order to set the scratch array for
                                       // column data to a sufficient size.
      std::vector<const ConstraintLine *>
        constraint_lines (n_local_dofs,
                          static_cast<const ConstraintLine *>(0));
      unsigned int n_max_entries_per_row = 0;
      for (unsigned int i=0; i<n_local_dofs; ++i)
        {
          ConstraintLine index_comparison;
          index_comparison.line = local_dof_indices[i];

          const std::vector<ConstraintLine>::const_iterator
            position = std::lower_bound (lines.begin(),
                                         lines.end(),
                                         index_comparison);
          
                                           // if this dof is constrained,
                                           // then set the respective entry
                                           // in the array. otherwise leave
                                           // it at the invalid position
          if ((position != lines.end()) &&
              (position->line == local_dof_indices[i]))
	    {
	      constraint_lines[i] = &*position;
	      n_max_entries_per_row += position->entries.size();
	    }
        }

				       // We need to add the number of
				       // entries in the local matrix in
				       // order to obtain a sufficient size
				       // for the scratch array.
      n_max_entries_per_row += n_local_dofs;
      if (column_indices.size() < n_max_entries_per_row)
        {
	  column_indices.resize(n_max_entries_per_row);
	  column_values.resize(n_max_entries_per_row);
	}

                                       // now distribute entries row by row
      for (unsigned int i=0; i<n_local_dofs; ++i)
        {
          const ConstraintLine *position_i = constraint_lines[i];
          const bool is_constrained_i = (position_i != 0);

	  unsigned int col_counter = 0;
          
          for (unsigned int j=0; j<n_local_dofs; ++j)
            {
				       // we don't need to proceed when the
				       // matrix element is zero
	      if (local_matrix(i,j) == 0)
		continue;

              const ConstraintLine *position_j = constraint_lines[j];
              const bool is_constrained_j = (position_j != 0);

              if ((is_constrained_i == false) &&
                  (is_constrained_j == false))
                {
                                                   // neither row nor column
                                                   // is constrained, so
                                                   // write the value into
                                                   // the scratch array
		  column_indices[col_counter] = local_dof_indices[j];
		  column_values[col_counter] = local_matrix(i,j);
		  col_counter++;
                }
              else if ((is_constrained_i == true) &&
                       (is_constrained_j == false))
                {
                                                   // ok, row is
                                                   // constrained, but
                                                   // column is not. This
                                                   // creates entries in
                                                   // several rows to the
                                                   // same column, which is
                                                   // not covered by the
                                                   // scratch array. Write
                                                   // the values directly
                                                   // into the matrix
                  for (unsigned int q=0; q<position_i->entries.size(); ++q)
                    global_matrix.add (position_i->entries[q].first,
                                       local_dof_indices[j],
                                       local_matrix(i,j) *
                                       position_i->entries[q].second);
                }
              else if ((is_constrained_i == false) &&
                       (is_constrained_j == true))
                {
                                                   // simply the other way
                                                   // round: row ok, column
                                                   // is constrained. This
                                                   // time, we can put
                                                   // everything into the
                                                   // scratch array, since
                                                   // we are in the correct
                                                   // row.
                  for (unsigned int q=0; q<position_j->entries.size(); ++q)
		    {
		      column_indices[col_counter] = position_j->entries[q].first;
		      column_values[col_counter] = local_matrix(i,j) *
			                           position_j->entries[q].second;
		      col_counter++;
		    }

				   // need to subtract this element from the
				   // vector. this corresponds to an
				   // explicit elimination in the respective
				   // row of the inhomogeneous constraint in
				   // the matrix with Gauss elimination
		  if (use_vectors == true)
		    global_vector(local_dof_indices[i]) -= local_matrix(j,i) * 
		                                           position_j->inhomogeneity;
                }
              else if ((is_constrained_i == true) &&
                       (is_constrained_j == true))
                {
                                                   // last case: both row
                                                   // and column are
                                                   // constrained. Again,
                                                   // this creates entries
                                                   // in other rows than the
                                                   // current one, so write
                                                   // the values again in
                                                   // the matrix directly
                  for (unsigned int p=0; p<position_i->entries.size(); ++p)
		    {
		      for (unsigned int q=0; q<position_j->entries.size(); ++q)
			global_matrix.add (position_i->entries[p].first,
					   position_j->entries[q].first,
					   local_matrix(i,j) *
					   position_i->entries[p].second *
					   position_j->entries[q].second);

		      if (use_vectors == true)
			global_vector (position_i->entries[p].first) -=
			  local_matrix(j,i) * position_i->entries[p].second *
			  position_j->inhomogeneity;
		    }

                                                   // to make sure that the
                                                   // global matrix remains
                                                   // invertible, we need to
                                                   // do something with the
                                                   // diagonal elements. add
                                                   // the absolute value of
                                                   // the local matrix, so
                                                   // the resulting entry
                                                   // will always be
                                                   // positive and
                                                   // furthermore be in the
                                                   // same order of
                                                   // magnitude as the other
                                                   // elements of the matrix
						   //
						   // note that this also
						   // captures the special
						   // case that a dof is
						   // both constrained and
						   // fixed (this can happen
						   // for hanging nodes in
						   // 3d that also happen to
						   // be on the
						   // boundary). in that
						   // case, following the
						   // above program flow, it
						   // is realized that when
						   // distributing the row
						   // and column no elements
						   // of the matrix are
						   // actually touched if
						   // all the degrees of
						   // freedom to which this
						   // dof is constrained are
						   // also constrained (the
						   // usual case with
						   // hanging nodes in
						   // 3d). however, in the
						   // line below, we do
						   // actually do something
						   // with this dof
                  if (i == j)
		    {
		      column_indices[col_counter] = local_dof_indices[j];
		      column_values[col_counter] = 
			std::max(std::fabs(local_matrix(i,j)), 1e-10);
		      col_counter++;
		    }
                }
              else
                Assert (false, ExcInternalError());
            }

				   // Check whether we did remain within the
				   // arrays when adding elements into the
				   // scratch arrays. Moreover, there should
				   // be at least one element in the scratch
				   // array (the element diagonal).
	  Assert (col_counter <= n_max_entries_per_row, ExcInternalError());

				   // Finally, write the scratch array into
				   // the sparse matrix.
	  if (col_counter > 0)
	    global_matrix.add(local_dof_indices[i], col_counter, 
			      &column_indices[0], &column_values[0], 
			      false);

				   // And we take care of the vector
	  if (use_vectors == true)
	    {
	      if (is_constrained_i == true)
		for (unsigned int q=0; q<position_i->entries.size(); ++q)
		  global_vector(position_i->entries[q].first)
		    += local_vector(i) * position_i->entries[q].second;
	      else
		global_vector(local_dof_indices[i]) += local_vector(i);
	    }
	}
    }
}



template <typename SparsityType>
void
ConstraintMatrix::
add_entries_local_to_global (const std::vector<unsigned int> &local_dof_indices,
			     SparsityType                    &sparsity_pattern,
			     const bool                       keep_constrained_entries,
			     const Table<2,bool>             &dof_mask) const
{
				   // similar to the function for distributing
				   // matrix entries; see there for comments.
  const unsigned int n_local_dofs = local_dof_indices.size();
  bool dof_mask_is_active = false;
  if (dof_mask.n_rows() == n_local_dofs)
    {
      dof_mask_is_active = true;
      Assert (dof_mask.n_cols() == n_local_dofs,
	      ExcDimensionMismatch(dof_mask.n_cols(), n_local_dofs));
    }

  if (lines.size() == 0)
    {
      bool add_these_indices;
      for (unsigned int i=0; i<n_local_dofs; ++i)
        for (unsigned int j=0; j<n_local_dofs; ++j)
	  {
				        // There is one complication when 
				        // we call this function with
				        // dof_mask argument: When the
				        // mask is empty, we cannot
				        // access the respective
				        // position, and it would even
				        // be inefficient to create
				        // such a mask. Hence, we need
				        // to first check whether
				        // there is some information
				        // in the mask at all.
	    if (dof_mask_is_active)
	      {
		if (dof_mask[i][j] == true)
		  add_these_indices = true;
		else
		  add_these_indices = false;
	      }
	    else
	      add_these_indices = true;
	    if (add_these_indices == true)
	      sparsity_pattern.add(local_dof_indices[i],
				   local_dof_indices[j]);
	  }
    }
  else
    {
				       // if there are constraints, then they
				       // need to be sorted to allow for
				       // faster sorting (it doesn't matter
				       // whether the constraint matrix is
				       // closed or not if there are no
				       // constraints, as above)
      Assert (sorted == true, ExcMatrixNotClosed());

      std::vector<const ConstraintLine *>
        constraint_lines (n_local_dofs,
                          static_cast<const ConstraintLine *>(0));
      for (unsigned int i=0; i<n_local_dofs; ++i)
        {
          ConstraintLine index_comparison;
          index_comparison.line = local_dof_indices[i];

          const std::vector<ConstraintLine>::const_iterator
            position = std::lower_bound (lines.begin(),
                                         lines.end(),
                                         index_comparison);

          if ((position != lines.end()) &&
              (position->line == local_dof_indices[i]))
            constraint_lines[i] = &*position;
        }


      bool add_these_indices;
      for (unsigned int i=0; i<n_local_dofs; ++i)
        {
          const ConstraintLine *position_i = constraint_lines[i];
          const bool is_constrained_i = (position_i != 0);

          for (unsigned int j=0; j<n_local_dofs; ++j)
	    {
				        // Again, first check whether
				        // the mask contains any
				        // information, and decide
				        // then whether the current
				        // index should be added.			
	      if (dof_mask_is_active)
		{
		  if (dof_mask[i][j] == true)
		    add_these_indices = true;
		  else
		    add_these_indices = false;
		}
	      else
		add_these_indices = true;

	      if (add_these_indices == true)
	      {
		const ConstraintLine *position_j = constraint_lines[j];
		const bool is_constrained_j = (position_j != 0);

					       // if so requested, add the
					       // entry unconditionally, even
					       // if it is going to be
					       // constrained away
		if (keep_constrained_entries == true)
		  sparsity_pattern.add (local_dof_indices[i],
					local_dof_indices[j]);


		if ((is_constrained_i == false) &&
		    (is_constrained_j == false) &&
		    (keep_constrained_entries == false))
		  {
		    sparsity_pattern.add (local_dof_indices[i],
					  local_dof_indices[j]);
		  }
		else if ((is_constrained_i == true) &&
			 (is_constrained_j == false))
		  {
		    for (unsigned int q=0; q<position_i->entries.size(); ++q)
		      sparsity_pattern.add (position_i->entries[q].first,
					    local_dof_indices[j]);
		  }
		else if ((is_constrained_i == false) &&
			 (is_constrained_j == true))
		  {
		    for (unsigned int q=0; q<position_j->entries.size(); ++q)
		      sparsity_pattern.add (local_dof_indices[i],
					    position_j->entries[q].first);
		  }
		else if ((is_constrained_i == true) &&
			 (is_constrained_j == true))
		  {
		    for (unsigned int p=0; p<position_i->entries.size(); ++p)
		      for (unsigned int q=0; q<position_j->entries.size(); ++q)
			sparsity_pattern.add (position_i->entries[p].first,
					      position_j->entries[q].first);

		    if (i == j)
		      sparsity_pattern.add (local_dof_indices[i],
					    local_dof_indices[i]);
		  }
		else
						 // the only case that can
						 // happen here is one that we
						 // have already taken care of
		  Assert ((is_constrained_i == false) &&
			  (is_constrained_j == false) &&
			  (keep_constrained_entries == true),
			  ExcInternalError());
	      }
	    }
        }
    }
}



template<class VectorType>
void
ConstraintMatrix::distribute (const VectorType &condensed,
			      VectorType       &uncondensed) const
{
  Assert (sorted == true, ExcMatrixNotClosed());
  Assert (condensed.size()+n_constraints() == uncondensed.size(),
	  ExcDimensionMismatch(condensed.size()+n_constraints(),
			       uncondensed.size()));

				   // store for each line of the new vector
				   // its old line number before
				   // distribution. If the shift is
				   // -1, this line was condensed away
  std::vector<int> old_line;

  old_line.reserve (uncondensed.size());

  std::vector<ConstraintLine>::const_iterator next_constraint = lines.begin();
  unsigned int                                shift           = 0;
  unsigned int n_rows = uncondensed.size();

  if (next_constraint == lines.end()) 
				     // if no constraint is to be handled
    for (unsigned int row=0; row!=n_rows; ++row)
      old_line.push_back (row);
  else
    for (unsigned int row=0; row!=n_rows; ++row)
      if (row == next_constraint->line)
	{
					   // this line is constrained
	  old_line.push_back (-1);
					   // note that @p lines is ordered
	  ++shift;
	  ++next_constraint;
	  if (next_constraint == lines.end())
					     // nothing more to do; finish rest
					     // of loop
	    {
	      for (unsigned int i=row+1; i<n_rows; ++i)
		old_line.push_back (i-shift);
	      break;
	    };
	}
      else
	old_line.push_back (row-shift);


  next_constraint = lines.begin();
				   // note: in this loop we need not check
				   // whether @p next_constraint is a valid
				   // iterator, since @p next_constraint is
				   // only evaluated so often as there are
				   // entries in new_line[*] which tells us
				   // which constraints exist
  for (unsigned int line=0; line<uncondensed.size(); ++line) 
    if (old_line[line] != -1)
				       // line was not condensed away
      uncondensed(line) = condensed(old_line[line]);
    else
      {
					 // line was condensed away,
					 // create it newly. first set
					 // it to zero
	uncondensed(line) = next_constraint->inhomogeneity;
					 // then add the different
					 // contributions
	for (unsigned int i=0; i<next_constraint->entries.size(); ++i)
	  uncondensed(line) += (condensed(old_line[next_constraint->entries[i].first]) *
				next_constraint->entries[i].second);
	++next_constraint;
      };
}



template<class VectorType>
void
ConstraintMatrix::distribute (VectorType &vec) const
{
  Assert (sorted == true, ExcMatrixNotClosed());

  std::vector<ConstraintLine>::const_iterator next_constraint = lines.begin();
  for (; next_constraint != lines.end(); ++next_constraint) 
    {
				       // fill entry in line
				       // next_constraint.line by adding the
				       // different contributions
      double new_value = next_constraint->inhomogeneity;
      for (unsigned int i=0; i<next_constraint->entries.size(); ++i)
	new_value += (vec(next_constraint->entries[i].first) *
                      next_constraint->entries[i].second);
      vec(next_constraint->line) = new_value;
    }
}


DEAL_II_NAMESPACE_CLOSE

#endif
