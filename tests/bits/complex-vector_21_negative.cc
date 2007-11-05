//----------------------------  vector_21_negative.cc  ---------------------------
//    $Id$
//    Version: $Name$ 
//
//    Copyright (C) 2004, 2005, 2007 by the deal.II authors
//
//    This file is subject to QPL and may not be  distributed
//    without copyright and license information. Please refer
//    to the file deal.II/doc/license.html for the  text  and
//    further information on this license.
//
//----------------------------  vector_21_negative.cc  ---------------------------


// check Vector<std::complex<double> >::operator /=. the original check was that the factor
// by which we divide must be positive. this is of course nonsensical, it
// should have been that the factor is != 0...

#include "../tests.h"
#include <lac/vector.h>    
#include <fstream>
#include <iostream>
#include <vector>


void test (Vector<std::complex<double> > &v)
{
                                   // set only certain elements of the
                                   // vector. have a bit pattern of where we
                                   // actually wrote elements to
  std::vector<bool> pattern (v.size(), false);
  for (unsigned int i=0; i<v.size(); i+=1+i)
    {
      v(i) = 1.*i*std::complex<double> (i+1., i+2.);
      pattern[i] = true;
    }
  v.compress ();

                                   // multiply v with 3/4
  v /= -4./3.;

                                   // check that the entries are ok
  for (unsigned int i=0; i<v.size(); ++i)
    Assert (((pattern[i] == true) && (v(i)-(-1.*i*std::complex<double> (i+1., i+2.)*3./4.) == std::complex<double>(0)))
	    ||
	    ((pattern[i] == false) && (v(i) == std::complex<double>(0))),
	    ExcInternalError());

  deallog << "OK" << std::endl;
}



int main () 
{
  std::ofstream logfile("complex-vector_21_negative/output");
  deallog.attach(logfile);
  deallog.depth_console(0);
  deallog.threshold_double(1.e-10);

  try
    {
      Vector<std::complex<double> > v (100);
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
