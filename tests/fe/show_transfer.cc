//----------------------------  show_transfer.cc  ---------------------------
//    $Id$
//    Version: $Name$
//
//    Copyright (C) 1998, 1999, 2000, 2001, 2002 by the deal.II authors
//
//    This file is subject to QPL and may not be  distributed
//    without copyright and license information. Please refer
//    to the file deal.II/doc/license.html for the  text  and
//    further information on this license.
//
//----------------------------  show_transfer.cc  ---------------------------
//
// Print multigrid transfer matrices between one and four cells.
//
//----------------------------  show_transfer.cc  ---------------------------

#include <base/quadrature_lib.h>
#include <lac/vector.h>
#include <grid/tria.h>
#include <grid/tria_iterator.h>
#include <dofs/dof_accessor.h>
#include <grid/grid_generator.h>
#include <fe/fe_values.h>
#include <multigrid/multigrid.h>
#include <multigrid/mg_dof_handler.h>

#include <vector>
#include <fstream>
#include <string>

char fname[50];

#define TEST(l,el) { el fe; print_matrix(of, tr, l, fe, #el); }

template<int dim>
inline void
print_matrix(std::ostream& of,
	     Triangulation<dim>& tr,
	     unsigned int level,
	     const FiniteElement<dim>& finel,
	     const char* name)
{
  MGDoFHandler<dim> dof(tr);
  dof.distribute_dofs(finel);

  MGTransferPrebuilt transfer;
  transfer.build_matrices(dof);

  unsigned int n_coarse = dof.n_dofs(level-1);
  unsigned int n_fine = dof.n_dofs(level);
  Vector<double> in(n_fine);
  Vector<double> out(n_coarse);

  of << name << std::endl;
  for (unsigned int i=0;i<n_fine;++i)
    {
      in = 0.;
      out = 0.;
      in(i) = 1.;
      transfer.restrict_and_add(level,out,in);
      out.print(of, 3, false);
    }
  of << std::endl;
}


int
main()
{
  Triangulation<2> tr;

  GridGenerator::hyper_cube(tr, -1., 1.);
  tr.refine_global(2);

  std::ofstream of("transfer.dat");
  
  TEST(1,FEQ1<2>);
  TEST(1,FEQ2<2>);
  TEST(1,FEQ3<2>);
  TEST(1,FEQ4<2>);

  TEST(1,FEDG_Q0<2>);
  TEST(1,FEDG_Q1<2>);
  TEST(1,FEDG_Q2<2>);
  TEST(1,FEDG_Q3<2>);
  TEST(1,FEDG_Q4<2>);

  TEST(2,FEQ1<2>);
  TEST(2,FEQ2<2>);
  TEST(2,FEQ3<2>);
  TEST(2,FEQ4<2>);

  TEST(2,FEDG_Q0<2>);
  TEST(2,FEDG_Q1<2>);
  TEST(2,FEDG_Q2<2>);
  TEST(2,FEDG_Q3<2>);
  TEST(2,FEDG_Q4<2>);
  return 0;
}
