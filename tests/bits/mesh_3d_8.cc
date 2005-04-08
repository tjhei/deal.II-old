//----------------------------  mesh_3d_8.cc  ---------------------------
//    $Id$
//    Version: $Name$ 
//
//    Copyright (C) 2003, 2004, 2005 by the deal.II authors
//
//    This file is subject to QPL and may not be  distributed
//    without copyright and license information. Please refer
//    to the file deal.II/doc/license.html for the  text  and
//    further information on this license.
//
//----------------------------  mesh_3d_8.cc  ---------------------------


// check that face orientation flags work. at one point in time we
// mixed up the generation of edges associated with faces for the
// non-standard case when refining the mesh. thus make sure that each
// edge only appears once in the list of edges of every cell.

#include "../tests.h"
#include "mesh_3d.h"

#include <base/logstream.h>
#include <base/quadrature_lib.h>
#include <grid/tria.h>
#include <grid/tria_accessor.h>
#include <grid/tria_iterator.h>
#include <grid/grid_reordering.h>
#include <grid/grid_generator.h>
#include <dofs/dof_handler.h>
#include <fe/fe_q.h>
#include <fe/fe_values.h>

#include <fstream>
#include <set>

void check_this (Triangulation<3> &tria)
{
  for (Triangulation<3>::cell_iterator cell=tria.begin();
       cell != tria.end(); ++cell)
    {
      std::set<Triangulation<3>::line_iterator> lines;
      for (unsigned int l=0; l<GeometryInfo<3>::lines_per_cell; ++l)
        {
          Assert (lines.find (cell->line(l)) == lines.end(),
                  ExcInternalError());
          lines.insert (cell->line(l));
        }
    }
  deallog << "    ok." << std::endl;
}


void check (Triangulation<3> &tria)
{
  deallog << "Initial check" << std::endl;
  check_this (tria);
  
  for (unsigned int r=0; r<3; ++r)
    {
      tria.refine_global (1);
      deallog << "Check " << r << std::endl;
      check_this (tria);
    }

  coarsen_global (tria);
  deallog << "Check " << 1 << std::endl;
  check_this (tria);
  
  tria.refine_global (1);
  deallog << "Check " << 2 << std::endl;
  check_this (tria);
}


int main () 
{
  std::ofstream logfile("mesh_3d_8.output");
  deallog.attach(logfile);
  deallog.depth_console(0);
  deallog.threshold_double(1.e-10);

  {  
    Triangulation<3> coarse_grid;
    create_two_cubes (coarse_grid);
    check (coarse_grid);
  }
  
  {  
    Triangulation<3> coarse_grid;
    create_L_shape (coarse_grid);
    check (coarse_grid);
  }
  
  {  
    Triangulation<3> coarse_grid;
    GridGenerator::hyper_ball (coarse_grid);
    check (coarse_grid);
  }
  
}

  
  
