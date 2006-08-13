//----------------------------  project_abf_02.cc  ---------------------------
//    $Id: project_abf_02.cc 12732 2006-03-28 23:15:45Z wolf $
//    Version: $Name$ 
//
//    Copyright (C) 2006 by the deal.II authors
//
//    This file is subject to QPL and may not be  distributed
//    without copyright and license information. Please refer
//    to the file deal.II/doc/license.html for the  text  and
//    further information on this license.
//
//----------------------------  project_abf_02.cc  ---------------------------


// check that VectorTools::project works for ABF elements correctly on
// a uniformly refined mesh for functions of degree q

char logname[] = "project_abf_02/output";


#include "project_common.cc"


template <int dim>
void test ()
{
  if (dim > 1)
    for (unsigned int p=0; p<6-dim; ++p)
      test_with_hanging_nodes (FE_ABF<dim>(p), p);
}
