//----------------------------  vector_48.cc  ---------------------------
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
//----------------------------  vector_48.cc  ---------------------------


// check Vector<std::complex<double> >::ratio

#include "../tests.h"
#include <lac/vector.h>    
#include <fstream>
#include <iostream>
#include <vector>


void test (Vector<std::complex<double> > &v,
           Vector<std::complex<double> > &w,
           Vector<std::complex<double> > &x)
{
  for (unsigned int i=0; i<v.size(); ++i)
    {
      v(i) = std::complex<double> (i+1., i+2.);
      w(i) = std::complex<double> (i+2., i+3.);
      x(i) = std::complex<double> (i+3., i+4.);
    }
  
  v.compress ();
  w.compress ();
  x.compress ();

  v.ratio (w, x);

                                   // make sure we get the expected result
  for (unsigned int i=0; i<v.size(); ++i)
    {
      Assert (w(i) == std::complex<double> (i+2., i+3.),
	      ExcInternalError());
      Assert (x(i) == std::complex<double> (i+3., i+4.),
	      ExcInternalError());
      Assert (std::abs(v(i) -
		       std::complex<double> (i+2., i+3.) /
		       std::complex<double> (i+3., i+4.)) < 1e-14*std::abs(v(i)),
              ExcInternalError());
    }

  deallog << "OK" << std::endl;
}



int main () 
{
  std::ofstream logfile("complex-vector_48/output");
  deallog.attach(logfile);
  deallog.depth_console(0);
  deallog.threshold_double(1.e-10);

  try
    {
      Vector<std::complex<double> > v (100);
      Vector<std::complex<double> > w (100);
      Vector<std::complex<double> > x (100);
      test (v,w,x);
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
