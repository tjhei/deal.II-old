//----------------------------  sparsity_pattern_01.cc  ---------------------------
//    $Id: sparsity_pattern_01.cc 15674 2008-01-24 17:40:56Z kanschat $
//    Version: $Name$ 
//
//    Copyright (C) 2008 by the deal.II authors
//
//    This file is subject to QPL and may not be  distributed
//    without copyright and license information. Please refer
//    to the file deal.II/doc/license.html for the  text  and
//    further information on this license.
//
//----------------------------  sparsity_pattern_01.cc  ---------------------------


// check

#include "../tests.h"
#include <base/logstream.h>
#include <lac/sparsity_pattern.h>
#include <lac/compressed_sparsity_pattern.h>
#include <lac/compressed_set_sparsity_pattern.h>
#include <lac/full_matrix.h>
#include "testmatrix.h"
#include <fstream>
#include <sstream>
#include <iomanip>
#include <list>
#include <set>
#include <cstdio>


const unsigned int N = 15;


template <typename SP>
void build_sparsity (SP &sparsity_pattern)
{
				   // generate usual 5-point sparsity pattern
  sparsity_pattern.reinit((N-1)*(N-1), (N-1)*(N-1), 5);
  FDMatrix(N,N).five_point_structure (sparsity_pattern);
  sparsity_pattern.compress ();

  deallog << sparsity_pattern.n_rows() << " "
	  << sparsity_pattern.n_cols() << " "
	  << sparsity_pattern.bandwidth() << " "
	  << sparsity_pattern.n_nonzero_elements()
	  << std::endl;
}


template <typename SP>
void row_length ()
{
  SP sparsity_pattern;
  build_sparsity (sparsity_pattern);
  
  for (unsigned int i=0; i<sparsity_pattern.n_rows(); ++i)
    deallog << sparsity_pattern.row_length(i) << std::endl;
}


template <typename SP>
void print_gnuplot ()
{
  SP sparsity_pattern;
  build_sparsity (sparsity_pattern);

  sparsity_pattern.print_gnuplot(deallog.get_file_stream());
}



template <typename SP>
void print ()
{
  SP sparsity_pattern;
  build_sparsity (sparsity_pattern);

  sparsity_pattern.print(deallog.get_file_stream());
}



template <typename SP>
void copy_with_offdiagonals_1 ()
{
  SparsityPattern sparsity_pattern;
  build_sparsity (sparsity_pattern);

				   // generate copy of sp1 with extra
				   // off-diagonals
  SP sp2(sparsity_pattern, 10, 2);
  sp2.compress ();
  deallog << sp2.n_rows() << " " << sp2.n_cols() << " "
	  << sp2.bandwidth() << " " << sp2.n_nonzero_elements()
	  << std::endl;
  for (unsigned int i=0; i<sp2.n_rows(); ++i)
    deallog << sp2.row_length(i) << std::endl;
  sp2.print_gnuplot(deallog.get_file_stream());
}



template <typename SP>
void copy_with_offdiagonals_2 ()
{
  SparsityPattern sparsity_pattern;
  build_sparsity (sparsity_pattern);

				   // generate copy of sp1 with
				   // extra off-diagonals, add some
				   // non-symmetric elements and symmetrize
				   // again
  SP sp3(sparsity_pattern, (N-1)*(N-1), 2);
  for (unsigned int i=0; i<(N-1)*(N-1); ++i)
    sp3.add (0,i);
  sp3.symmetrize ();
  sp3.compress ();
  deallog << sp3.n_rows() << " " << sp3.n_cols() << " "
	  << sp3.bandwidth() << " " << sp3.n_nonzero_elements()
	  << std::endl;
  for (unsigned int i=0; i<sp3.n_rows(); ++i)
    deallog << sp3.row_length(i) << std::endl;
  sp3.print_gnuplot(deallog.get_file_stream());
}



void
do_copy_from (const std::list<std::set<unsigned int,std::greater<unsigned int> > > &sparsity,
	      SparsityPattern &sp4)
{
  sp4.copy_from ((N-1)*(N-1), (N-1)*(N-1),
		 sparsity.begin(), sparsity.end());
}


template <typename SP>
void
do_copy_from (const CompressedSparsityPattern &sparsity,
	      SP &sp4)
{
  std::list<std::set<unsigned int,std::greater<unsigned int> > > sparsity_x;
  for (unsigned int i=0; i<sparsity.n_rows(); ++i)
    {
      sparsity_x.push_back
	(std::set<unsigned int,std::greater<unsigned int> >());
      
      for (unsigned int j=0; j<sparsity.n_cols(); ++j)
	if (sparsity.exists(i,j))
	  sparsity_x.back().insert (j);
    }
  
  do_copy_from (sparsity_x, sp4);
}


void
do_copy_from (const CompressedSetSparsityPattern &sparsity,
	      SparsityPattern &sp4)
{
  std::list<std::set<unsigned int,std::greater<unsigned int> > > sparsity_x;
  for (unsigned int i=0; i<sparsity.n_rows(); ++i)
    {
      sparsity_x.push_back
	(std::set<unsigned int,std::greater<unsigned int> >());
      
      for (unsigned int j=0; j<sparsity.n_cols(); ++j)
	if (sparsity.exists(i,j))
	  sparsity_x.back().insert (j);
    }
  
  do_copy_from (sparsity_x, sp4);
}



void
do_copy_from (const FullMatrix<double> &sparsity,
	      SparsityPattern &sp4)
{
  sp4.copy_from (sparsity);
}



template <typename SP>
void copy_from_1 ()
{
  SparsityPattern sparsity_pattern;
  build_sparsity (sparsity_pattern);

				   // now test the copy_from function. for
				   // this copy over the column indices, but
				   // in different order as the order should
				   // not matter to that function
  std::list<std::set<unsigned int,std::greater<unsigned int> > > sparsity;
  for (unsigned int row=0; row<sparsity_pattern.n_rows(); ++row)
    {
      sparsity.push_back
	(std::set<unsigned int,std::greater<unsigned int> >());
      for (const unsigned int
	     *p=(sparsity_pattern.get_column_numbers()
		 +sparsity_pattern.get_rowstart_indices()[row]);
	   p != (sparsity_pattern.get_column_numbers()
		 +sparsity_pattern.get_rowstart_indices()[row+1]);
	   ++p)
	sparsity.back().insert (*p);
    }
  SP sp4;
  do_copy_from (sparsity, sp4);

				   // now check for equivalence of original
				   // and copy
  Assert (sparsity_pattern.n_nonzero_elements() ==
	  sp4.n_nonzero_elements(),
	  ExcInternalError());
  for (unsigned int i=0; i<sparsity_pattern.n_nonzero_elements(); ++i)
    Assert (sp4.exists (sparsity_pattern.matrix_position(i).first,
			sparsity_pattern.matrix_position(i).second)
	    == true,
	    ExcInternalError());
}



template <typename SP>
void copy_from_2 ()
{
  CompressedSparsityPattern sparsity_pattern;
  build_sparsity (sparsity_pattern);

  SP sp4;
  do_copy_from (sparsity_pattern, sp4);

				   // now check for equivalence of original
				   // and copy
  Assert (sparsity_pattern.n_nonzero_elements() ==
	  sp4.n_nonzero_elements(),
	  ExcInternalError());
  for (unsigned int i=0; i<sparsity_pattern.n_rows(); ++i)
    for (unsigned int j=0; j<sparsity_pattern.n_cols(); ++j)
      Assert (sparsity_pattern.exists(i,j) == sp4.exists (i,j),
	      ExcInternalError());
}



template <typename SP>
void copy_from_3 ()
{
  CompressedSetSparsityPattern sparsity_pattern;
  build_sparsity (sparsity_pattern);

  SP sp4;
  do_copy_from (sparsity_pattern, sp4);

				   // now check for equivalence of original
				   // and copy
  Assert (sparsity_pattern.n_nonzero_elements() ==
	  sp4.n_nonzero_elements(),
	  ExcInternalError());
  for (unsigned int i=0; i<sparsity_pattern.n_rows(); ++i)
    for (unsigned int j=0; j<sparsity_pattern.n_cols(); ++j)
      Assert (sparsity_pattern.exists(i,j) == sp4.exists (i,j),
	      ExcInternalError());
}



template <typename SP>
void copy_from_4 ()
{
  const unsigned int M = 25;
  FullMatrix<double> sparsity_pattern(M,M);
  for (unsigned int i=0; i<M; ++i)
    for (unsigned int j=0; j<M; ++j)
      if (std::abs((int)(i-j)) == 3)
	sparsity_pattern (i,j) = 1;

  SP sp4;
  do_copy_from (sparsity_pattern, sp4);

				   // now check for equivalence of original
				   // and copy
  Assert (sp4.n_nonzero_elements() ==
	  static_cast<unsigned int>(sparsity_pattern.frobenius_norm()),
	  ExcInternalError());
  
  for (unsigned int i=0; i<M; ++i)
    for (unsigned int j=0; j<M; ++j)
      if (std::abs((int)(i-j)) == 3)
	Assert (sp4.exists (i,j)
		== true,
		ExcInternalError());
}



template <typename SP>
void matrix_position ()
{
  SP sparsity_pattern;
  build_sparsity (sparsity_pattern);

				   // check the matrix_position
				   // function. the checked
				   // function should be the inverse
				   // of operator()
  for (unsigned int i=0; i<sparsity_pattern.n_nonzero_elements(); ++i)
    Assert (sparsity_pattern(sparsity_pattern.matrix_position(i).first,
			     sparsity_pattern.matrix_position(i).second) == i,
	    ExcInternalError());
  for (unsigned int row=0; row<sparsity_pattern.n_rows(); ++row)
    for (unsigned int col=0; col<sparsity_pattern.n_cols(); ++col)
      if (sparsity_pattern(row,col) != SparsityPattern::invalid_entry)
	Assert (sparsity_pattern.matrix_position(sparsity_pattern(row,col)) ==
		std::make_pair(row,col),
		ExcInternalError());
}

  
template <typename SP>
void block_read_write ()
{
  SP sparsity_pattern;
  build_sparsity (sparsity_pattern);

				 // check block_write/block_read by
				 // dumping a sparsity pattern and
				 // checking whether the
				 // read-back-in pattern is the same
  std::ostringstream tmp_write;
  sparsity_pattern.block_write (tmp_write);

  SP sp5;
  
  std::istringstream tmp_read(tmp_write.str());
  sp5.block_read (tmp_read);

				   // now check for equivalence of
				   // sparsity_pattern and sp5
  deallog << sparsity_pattern.n_rows() - sp5.n_rows() << ' '
	  << sparsity_pattern.n_cols() - sp5.n_cols() << ' '
	  << (sparsity_pattern.is_compressed() ^ sp5.is_compressed()) << ' '
	  << (sparsity_pattern.is_compressed() ^ sp5.is_compressed()) << ' '
	  << std::endl;
  
  for (unsigned int row=0; row<sparsity_pattern.n_rows(); ++row)
    {
      const unsigned int
        *sparsity_pattern_p=sparsity_pattern.get_column_numbers()+sparsity_pattern.get_rowstart_indices()[row];
      const unsigned int
        *sp5_p=sp5.get_column_numbers()+sp5.get_rowstart_indices()[row];
      for (; sparsity_pattern_p != (sparsity_pattern.get_column_numbers() +
				    sparsity_pattern.get_rowstart_indices()[row+1]);
           ++sparsity_pattern_p, ++sp5_p)
	Assert (*sparsity_pattern_p == *sp5_p, ExcInternalError());
    }
}

  
  
