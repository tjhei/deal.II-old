//----------------------------  block_matrices.cc  ---------------------------
//    block_matrices.cc,v 1.15 2003/04/30 23:08:40 wolf Exp
//    Version: 
//
//    Copyright (C) 2000, 2001, 2002, 2003 by the deal.II authors
//
//    This file is subject to QPL and may not be  distributed
//    without copyright and license information. Please refer
//    to the file deal.II/doc/license.html for the  text  and
//    further information on this license.
//
//----------------------------  block_matrices.cc  ---------------------------


#include "../tests.h"
#include <base/logstream.h>
#include <lac/block_sparsity_pattern.h>
#include <lac/block_sparse_matrix.h>
#include <lac/block_vector.h>
#include <fstream>
#include <iostream>
#include <algorithm>




void test () 
{
  std::ofstream logfile("block_matrices.output");
  logfile.setf(std::ios::fixed);
  logfile.precision(2);
  deallog.attach(logfile);
  deallog.depth_console(0);

  BlockSparsityPattern bsp(3,2);
				   // set sizes
  bsp.block(0,0).reinit ( 2, 10, 7);
  bsp.block(0,1).reinit ( 2, 19, 8);
  bsp.block(1,0).reinit ( 7, 10, 6);
  bsp.block(1,1).reinit ( 7, 19, 7);
  bsp.block(2,0).reinit (10, 10, 6);
  bsp.block(2,1).reinit (10, 19, 12);
  bsp.collect_sizes ();

				   // add this pseudo-random sparsity
				   // pattern:
				   // 0   1    1 1 1    1 1 1    1 1 1 
				   // 1    1 1 1    1 1 1    1 1 1    1
				   // 2       1 1 1 1    1 1 1    1 1 1
				   // 3   1 1 1    1 1 1 1    1 1 1    
				   // 4   1    1 1 1    1 1 1 1    1 1 
				   // 5    1 1 1    1 1 1    1 1 1 1   
				   // 6    1    1 1 1    1 1 1    1 1 1
				   // 7   1 1 1 1    1 1 1    1 1 1    
				   // 8   1    1 1 1 1    1 1 1    1 1 
				   // 9   11 1 1    1 1 1 1    1 1 1   
				   // 1    1    1 1 1    1 1 1 1    1 1
				   // 11    1 1 1    1 1 1    1 1 1 1  
				   // 12  1 11   1 1 1    1 1 1    1 1 
				   // 13   1 111 1    1 1 1    1 1 1   
				   // 14   1   11 1 1 1    1 1 1    1 1
				   // 15    1 1 1    1 1 1 1    1 1 1  
				   // 16  1 1    1 1 1    1 1 1 1    1 
				   // 17     1 1 11   1 1 1    1 1 1 1 
				   // 18   1 1    111 1    1 1 1    1 1
  for (unsigned int row=0; row<19; ++row)
    for (unsigned int i=0; i<10; ++i)
      bsp.add (row, (row*5+i*9)%29);
  bsp.compress ();

				   // now check whether the elements
				   // we inserted are indeed those
				   // that are in there. for now, we
				   // only check their number, but
				   // their places are checked later
				   // with the matrix-vector
				   // operations.
  unsigned int total_nonzero_elements = 0;
  for (unsigned int row=0; row<19; ++row)
    {
				       // first count the number of
				       // elements in each row
      std::vector<bool> t(29, false);
      for (unsigned int i=0; i<10; ++i)
	t[(row*5+i*9)%29] = true;
				       // if we are in the third block
				       // row, then the first matrix
				       // is square, so there may be
				       // an additional element
      if (row>=9)
	t[row-9] = true;

      deallog << "Row " << row << " sparsity:  ";
      for (unsigned int i=0; i<29; ++i)
	deallog << t[i];
      deallog << std::endl;
      
      const unsigned int c=count(t.begin(), t.end(), true);
      
				       // now see how many elements
				       // there really are:
      unsigned int ac=0;
      for (unsigned int col=0; col<2; ++col)
	if (row<2)
	  ac += bsp.block(0,col).row_length(row-0);
	else
	  if (row<9)
	    ac += bsp.block(1,col).row_length(row-2);
	  else
	    ac += bsp.block(2,col).row_length(row-9);
      deallog << "Row=" << row
	      << ": expected length=" << c
	      << ", actual length=" << ac
	      << std::endl;
      total_nonzero_elements += ac;
      AssertThrow (c==ac, ExcInternalError());
    };
  deallog << total_nonzero_elements << "=="
	  << bsp.n_nonzero_elements()
	  << std::endl;
  AssertThrow (total_nonzero_elements == bsp.n_nonzero_elements(),
	       ExcInternalError());



  
				   // now make a matrix with this
				   // sparsity pattern
  BlockSparseMatrix<double> bsm (bsp);
  deallog << total_nonzero_elements << "=="
	  << bsm.n_nonzero_elements()
	  << std::endl;
  AssertThrow (total_nonzero_elements == bsm.n_nonzero_elements(),
	       ExcInternalError());
  
				   // try to write something into it,
				   // set entry (i,j) to i*j
  for (unsigned int row=0; row<19; ++row)
    for (unsigned int i=0; i<10; ++i)
      bsm.set (row, (row*5+i*9)%29, row*((row*5+i*9)%29));
				   // and add .5 to each value
  for (unsigned int row=0; row<19; ++row)
    for (unsigned int i=0; i<10; ++i)
      bsm.add (row, (row*5+i*9)%29, 0.5);


				   // Check the iterator
  deallog.push("Iterator");
  BlockSparseMatrix<double>::const_iterator iter = bsm.begin();
  const BlockSparseMatrix<double>::const_iterator end_iter = bsm.end();
  for (;iter != end_iter;++iter)
    deallog << iter->row() << '\t' << iter->column()
	    << '\t' << iter->value() << std::endl;
  deallog.pop();
  
				   // now allocate two block vectors
				   // and see what we can get after
				   // vmults:
  BlockVector<double> src;
  std::vector<unsigned int> src_sizes (2);
  src_sizes[0] = 10;
  src_sizes[1] = 19;
  src.reinit (src_sizes);

  BlockVector<double> dst;
  std::vector<unsigned int> dst_sizes (3);
  dst_sizes[0] = 2;
  dst_sizes[1] = 7;
  dst_sizes[2] = 10;
  dst.reinit (dst_sizes);

  for (unsigned int i=0; i<29; ++i)
    src(i) = i;

  bsm.vmult (dst, src);
				   // now check what came out
  for (unsigned int row=0; row<19; ++row)
    {
      std::vector<double> t(29, 0.);
				       // first check which elements
				       // in this row exist
      for (unsigned int i=0; i<10; ++i)
	t[(row*5+i*9)%29] = row*((row*5+i*9)%29);
      
      for (unsigned int i=0; i<10; ++i)
	t[(row*5+i*9)%29] += 0.5;

				       // compute the exact result
      double row_sum = 0;
      for (unsigned int i=0; i<29; ++i)
	row_sum += t[i]*i;

				       // compare to vmult result
      Assert (row_sum == dst(row), ExcInternalError());
      deallog << "vmult " << row << ' ' << row_sum << ' ' << dst(row) << std::endl;
    };


				   // test matrix_scalar_product. note that dst=M*src
  const double msp1 = dst.norm_sqr ();
  const double msp2 = bsm.matrix_scalar_product (dst, src);
  Assert (msp1 == msp2, ExcInternalError());
  deallog << "matrix_scalar_product " << msp1 << ' ' << msp2 << std::endl;
}




int main ()
{
  try
    {
      test ();
    }
  catch (std::exception &e)
    {
      std::cerr << std::endl << std::endl
	   << "----------------------------------------------------"
	   << std::endl;
      std::cerr << "Exception on processing: " << e.what() << std::endl
	   << "Aborting!" << std::endl
	   << "----------------------------------------------------"
	   << std::endl;
				       // abort
      return 2;
    }
  catch (...) 
    {
      std::cerr << std::endl << std::endl
	   << "----------------------------------------------------"
	   << std::endl;
      std::cerr << "Unknown exception!" << std::endl
	   << "Aborting!" << std::endl
	   << "----------------------------------------------------"
	   << std::endl;
				       // abort
      return 3;
    };
  
  
  return 0;
}
