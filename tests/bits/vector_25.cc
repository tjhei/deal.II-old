//----------------------------  vector_25.cc  ---------------------------
//    vector_11.cc,v 1.4 2003/07/03 10:31:46 guido Exp
//    Version: 
//
//    Copyright (C) 2004 by the deal.II authors
//
//    This file is subject to QPL and may not be  distributed
//    without copyright and license information. Please refer
//    to the file deal.II/doc/license.html for the  text  and
//    further information on this license.
//
//----------------------------  vector_25.cc  ---------------------------

// check Vector::operator = (Scalar) with setting to zero

#include "../tests.h"
#include <lac/vector.h>    
#include <fstream>
#include <iostream>
#include <vector>


void test (Vector<double> &v)
{
                                   // set some entries of the vector
  for (unsigned int i=0; i<v.size(); ++i)
    if (i%3 == 0)
      v(i) = i+1.;
  v.compress ();

                                   // then clear it again and make sure the
                                   // vector is really empty
  const unsigned int sz = v.size();
  v = 0;
  Assert (v.size() == sz, ExcInternalError());
  Assert (v.l2_norm() == 0, ExcInternalError());

  deallog << "OK" << std::endl;
}


int main () 
{
  std::ofstream logfile("vector_25.output");
  deallog.attach(logfile);
  deallog.depth_console(0);

  try
    {
      Vector<double> v (100);
      test (v);
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
