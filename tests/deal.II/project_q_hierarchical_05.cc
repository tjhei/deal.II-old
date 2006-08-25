//----------------------------  project_q_hierarchical_05.cc  ---------------------------
//    $Id: project_q_hierarchical_05.cc 12732 2006-03-28 23:15:45Z wolf $
//    Version: $Name$ 
//
//    Copyright (C) 2006 by the deal.II authors
//
//    This file is subject to QPL and may not be  distributed
//    without copyright and license information. Please refer
//    to the file deal.II/doc/license.html for the  text  and
//    further information on this license.
//
//----------------------------  project_q_hierarchical_05.cc  ---------------------------


// check that VectorTools::project works for QHierarchical elements correctly

char logname[] = "project_q_hierarchical_05/output";


#include "project_common.cc"


template <int dim>
void test ()
{
  for (unsigned int p=1; p<6-dim; ++p)
    test_with_2d_deformed_refined_mesh (FE_Q_Hierarchical<dim>(p), p);
}
