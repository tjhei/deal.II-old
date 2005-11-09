//----------------------------  sparse_matrix_iterator_02.cc  ---------------------------
//    $Id$
//    Version: $Name$ 
//
//    Copyright (C) 2004, 2005 by the deal.II authors
//
//    This file is subject to QPL and may not be  distributed
//    without copyright and license information. Please refer
//    to the file deal.II/doc/license.html for the  text  and
//    further information on this license.
//
//----------------------------  sparse_matrix_iterator_02.cc  ---------------------------


// test setting some elements and reading them back from a const matrix
// iterator

#include "../tests.h"
#include <lac/sparse_matrix.h>    
#include <fstream>
#include <iostream>


void test ()
{
  SparsityPattern sp (5,5,3);
  for (unsigned int i=0; i<5; ++i)
    for (unsigned int j=0; j<5; ++j)
      if (((i+2*j+1) % 3 == 0)
          ||
          (i==j))
        sp.add (i,j);
  sp.compress ();

  SparseMatrix<double> m(sp);
  for (unsigned int i=0; i<5; ++i)
    for (unsigned int j=0; j<5; ++j)
      if (((i+2*j+1) % 3 == 0)
          ||
          (i==j))
        m.set (i,j, i*j);
  
  SparseMatrix<double>::const_iterator i = m.begin();
  for (; i!=m.end(); ++i)
    {
      deallog << i->row() << ' ' << i->column() << ' '
              << i->value() << std::endl;
      Assert (std::fabs(i->value() - i->row()*i->column()) < 1e-14,
              ExcInternalError());
    }

  deallog << "OK" << std::endl;
}



int main ()
{
  std::ofstream logfile("sparse_matrix_iterator_02/output");
  deallog.attach(logfile);
  deallog.depth_console(0);
  deallog.threshold_double(1.e-10);

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
