//----------------------------  subdomain_ids_04.cc  ---------------------------
//    $Id$
//    Version: $Name$ 
//
//    Copyright (C) 2001, 2002, 2003, 2004 by the deal.II authors
//
//    This file is subject to QPL and may not be  distributed
//    without copyright and license information. Please refer
//    to the file deal.II/doc/license.html for the  text  and
//    further information on this license.
//
//----------------------------  subdomain_ids_04.cc  ---------------------------

// check DoFRenumbering::subdomain_wise


#include "../tests.h"
#include <base/logstream.h>
#include <grid/tria.h>
#include <grid/grid_generator.h>
#include <grid/tria_iterator.h>
#include <dofs/dof_handler.h>
#include <dofs/dof_accessor.h>
#include <dofs/dof_tools.h>
#include <fe/fe_dgq.h>
#include <fe/fe_q.h>
#include <fe/fe_system.h>
#include <dofs/dof_tools.h>
#include <dofs/dof_renumbering.h>

#include <fstream>
#include <algorithm>
#include <cmath>


std::ofstream logfile("subdomain_ids_04.output");


template <int dim>
void test ()
{
  deallog << dim << 'D' << std::endl;
  Triangulation<dim> tria;
  GridGenerator::hyper_cube(tria, -1, 1);
  tria.refine_global (2);

				   // we now have a number of cells,
				   // flag them with some subdomain
				   // ids based on their position, in
				   // particular we take the quadrant
				   // (octant)
  typename Triangulation<dim>::active_cell_iterator
    cell = tria.begin_active (),
    endc = tria.end ();
  for (; cell!=endc; ++cell)
    {
      unsigned int subdomain = 0;
      for (unsigned int d=0; d<dim; ++d)
	if (cell->center()(d) > 0)
	  subdomain |= (1<<d);
      Assert (subdomain < (1<<dim), ExcInternalError());

      cell->set_subdomain_id (subdomain);
    };

				   // distribute some degrees of freedom and
				   // output some information on them
  FESystem<dim> fe(FE_Q<dim>(2),dim, FE_DGQ<dim>(1), 1);
  DoFHandler<dim> dof_handler (tria);
  dof_handler.distribute_dofs (fe);
  deallog << dof_handler.n_dofs() << std::endl;

                                   // renumber by subdomain
  DoFRenumbering::subdomain_wise (dof_handler);

                                   // then get the subdomain association of
                                   // all dofs. this should yield consecutive
                                   // regions of dofs with increasing
                                   // subdomain numbers. first output these
                                   // numbers, then also check that this is
                                   // indeed so
  std::vector<unsigned int> subdomain_association (dof_handler.n_dofs());
  DoFTools::get_subdomain_association (dof_handler, subdomain_association);

  for (unsigned int i=0; i<dof_handler.n_dofs(); ++i)
    deallog << i << ' ' << subdomain_association[i] << std::endl;

  unsigned int present_subdomain = 0;
  for (unsigned int i=0; i<dof_handler.n_dofs(); ++i)
    if (subdomain_association[i] != present_subdomain)
      {
                                         // we must just have crossed the
                                         // boundary to another subdomain
        Assert (subdomain_association[i] == present_subdomain+1,
                ExcInternalError());
        ++present_subdomain;
      }
  Assert (present_subdomain == (1<<dim)-1, ExcInternalError());
}


int main ()
{
  logfile.precision(4);
  deallog.attach(logfile);
  deallog.depth_console(0);

  test<1> ();
  test<2> ();
  test<3> ();
  
  return 0;
}
