//----------------------------  block_sparse_matrix_iterator_04.cc  ---------------------------
//    $Id$
//    Version: $Name$ 
//
//    Copyright (C) 2004 by the deal.II authors
//
//    This file is subject to QPL and may not be  distributed
//    without copyright and license information. Please refer
//    to the file deal.II/doc/license.html for the  text  and
//    further information on this license.
//
//----------------------------  block_sparse_matrix_iterator_04.cc  ---------------------------


// this test a failure in design of the block sparse matrix iterators: falling
// off the end of the matrix does not yield the iterator provided by the end()
// function

#include "../tests.h"
#include <lac/block_sparsity_pattern.h>
#include <lac/block_sparse_matrix.h>
#include <fstream>
#include <iostream>


void test ()
{
  BlockSparsityPattern bsp (2,2);
  for (unsigned int i=0; i<2; ++i)
    for (unsigned int j=0; j<2; ++j)
      bsp.block(i,j).reinit (1,1,1);
  bsp.collect_sizes ();
  bsp.compress ();

  BlockSparseMatrix<double> m(bsp);

                                   // advance it to the end of the matrix
  BlockSparseMatrix<double>::const_iterator it = m.begin();
  for (unsigned int i=0; i<4; ++i)
    ++it;

  deallog << it->row() << ' ' << it->index() << ' '
          << it->column() << ' ' << it->block_row() << ' '
          << it->block_column()
          << std::endl;  

                                   // now also get an end iterator
  BlockSparseMatrix<double>::const_iterator it2 = m.end();
  deallog << it2->row() << ' ' << it2->index() << ' '
          << it2->column() << ' ' << it2->block_row() << ' '
          << it2->block_column()
          << std::endl;  

                                   // make sure that the two of them match
  Assert (it == it2, ExcInternalError());

                                   // interestingly, at the time of writing
                                   // this test, above assertion is ok, but an
                                   // elementwise one is not (we fail in the
                                   // first line)
  Assert (it.row() == it2.row(), ExcInternalError());
  Assert (it.block_row() == it2.block_row(), ExcInternalError());
  Assert (it.column() == it2.column(), ExcInternalError());
  Assert (it.block_column() == it2.block_column(), ExcInternalError());
  Assert (it.index() == it2.index(), ExcInternalError());
   
  deallog << "OK" << std::endl;
}



int main ()
{
  std::ofstream logfile("block_sparse_matrix_iterator_04.output");
  deallog.attach(logfile);
  deallog.depth_console(0);

  try
    {
      test ();
    }
  catch (std::exception &exc)
    {
      std::cerr << std::endl << std::endl
		<< "----------------------------------------------------"
		<< std::endl;
      std::cerr << "Exception on processing: " << std::endl
		<< exc.what() << std::endl
		<< "Aborting!" << std::endl
		<< "----------------------------------------------------"
		<< std::endl;
      
      return 1;
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
      return 1;
    };
}
