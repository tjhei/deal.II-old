//----------------------------  petsc_32.cc  ---------------------------
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
//----------------------------  petsc_32.cc  ---------------------------


// check PETScWrappers::Vector::l2_norm()

#include "../tests.h"
#include <lac/petsc_vector.h>    
#include <fstream>
#include <iostream>
#include <vector>


void test (PETScWrappers::Vector &v)
{
                                   // set some elements of the vector
  double sum = 0;
  for (unsigned int i=0; i<v.size(); i+=1+i)
    {
      v(i) = i;
      sum += i;
    }
  v.compress ();

                                   // then check the norm
  Assert (std::fabs(v.mean_value() - sum/v.size()) < 1e-14*sum/v.size(),
          ExcInternalError());

  deallog << "OK" << std::endl;
}



int main (int argc,char **argv) 
{
  std::ofstream logfile("petsc_32.output");
  deallog.attach(logfile);
  deallog.depth_console(0);
  deallog.threshold_double(1.e-10);

  try
    {
      PetscInitialize(&argc,&argv,0,0);
      {
        PETScWrappers::Vector v (100);
        test (v);
      }
      PetscFinalize();
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
