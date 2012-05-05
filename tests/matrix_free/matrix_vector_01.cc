//------------------  matrix_vector_01.cc  ------------------------
//    $Id$
//    Version: $Name$
//
//------------------  matrix_vector_01.cc  ------------------------


// this function tests the correctness of the implementation of matrix free
// matrix-vector products by comparing with the result of deal.II sparse
// matrix. The mesh uses a hypercube mesh with no hanging nodes and no other
// constraints

#include "../tests.h"

std::ofstream logfile("matrix_vector_01/output");

#include "matrix_vector_common.h"


template <int dim, int fe_degree>
void test ()
{
  Triangulation<dim> tria;
  GridGenerator::hyper_cube (tria);
  tria.refine_global(5-dim);

  FE_Q<dim> fe (fe_degree);
  DoFHandler<dim> dof (tria);
  dof.distribute_dofs(fe);
  ConstraintMatrix constraints;
  constraints.close();

  do_test<dim, fe_degree, double> (dof, constraints);
}

