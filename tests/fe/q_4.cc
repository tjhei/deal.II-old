//----------------------------  q_4.cc  ---------------------------
//    $Id$
//    Version: $Name$
//
//    Copyright (C) 2003 by the deal.II authors
//
//    This file is subject to QPL and may not be  distributed
//    without copyright and license information. Please refer
//    to the file deal.II/doc/license.html for the  text  and
//    fuqher information on this license.
//
//----------------------------  q_4.cc  ---------------------------


// previously, the FETools::get_interpolation_matrix function would
// compute its result itself by interpolation. now, the different
// finite elements do that themselves, if they can. make sure the
// result doesn't change

#include <base/logstream.h>
#include <fe/fe_q.h>
#include <fe/fe_tools.h>

#include <fstream>
#include <string>

#define PRECISION 5



template<int dim>
void
test(const unsigned int degree1,
     const unsigned int degree2)
{
  deallog << "FE_Q<" << dim << "> (" << degree1 << ")"
          << " to FE_Q<" << dim << "> (" << degree2 << ")"
	  << std::endl;
  
  FE_Q<dim> fe1(degree1);
  FE_Q<dim> fe2(degree2);

  FullMatrix<float> m (fe2.dofs_per_cell,
                       fe1.dofs_per_cell);
  FETools::get_interpolation_matrix (fe1, fe2, m);

  for (unsigned int i=0; i<m.m(); ++i)
    {
      for (unsigned int j=0; j<m.n(); ++j)
        deallog << m(i,j) << ' ';
      
      deallog << std::endl;
    }
  
  deallog << std::endl;
}


int
main()
{
  std::ofstream logfile ("q_4.output");
  logfile.precision (PRECISION);
  logfile.setf(std::ios::fixed);  
  deallog.attach(logfile);
  deallog.depth_console(0);

  for (unsigned int degree1=1; degree1<=4; ++degree1)
    for (unsigned int degree2=1; degree2<=4; ++degree2)
      test<1>(degree1, degree2);
  for (unsigned int degree1=1; degree1<=3; ++degree1)
    for (unsigned int degree2=1; degree2<=3; ++degree2)
      test<2>(degree1, degree2);
  for (unsigned int degree1=1; degree1<=2; ++degree1)
    for (unsigned int degree2=1; degree2<=2; ++degree2)
      test<3>(degree1, degree2);
  
  return 0;
}



