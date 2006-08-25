//----------------------------  project_rt_04.cc  ---------------------------
//    $Id: project_rt_04.cc 12732 2006-03-28 23:15:45Z wolf $
//    Version: $Name$ 
//
//    Copyright (C) 2006 by the deal.II authors
//
//    This file is subject to QPL and may not be  distributed
//    without copyright and license information. Please refer
//    to the file deal.II/doc/license.html for the  text  and
//    further information on this license.
//
//----------------------------  project_rt_04.cc  ---------------------------


// check that VectorTools::project works for RT elements correctly

char logname[] = "project_rt_04/output";


#include "project_common.cc"


template <int dim>
void test ()
{
  if (dim != 1)
				     // this is interesting also in 3d, but is
				     // exceedingly slow there. limit to the
				     // case of RT(0) elements in 3d
    for (unsigned int p=0; p<(dim == 2 ? 3 : 1); ++p)
      test_with_2d_deformed_mesh (FE_RaviartThomas<dim>(p), p+1, 1);
}
