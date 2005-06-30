//----------------------------  point_inside_2.cc  ---------------------------
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
//----------------------------  point_inside_2.cc  ---------------------------


// check TriaAccessor<3>::point_inside for a cell that is _not_
// aligned with the coordinate axes
//
// this program is a modified version of one by Joerg Weimar,
// TU Braunschweig

#include "../tests.h"
#include <base/logstream.h>
#include <grid/tria.h>
#include <grid/tria_accessor.h>
#include <grid/tria_iterator.h>
#include <grid/grid_generator.h>
#include <fstream>


template <int dim>
void check ()
{
				   // use a rather complicated way to
				   // generate a new mesh with one
				   // moved vertex -- one could move
				   // it by hand in the original
				   // triangulation, but this is how
				   // the original testcase came in,
				   // and it tests some more stuff, so
				   // why change things
  Triangulation<dim> triangulation;
  
				   // we generate a hyper_cube and
				   // modify one vertex.
  GridGenerator::hyper_cube (triangulation);
  
				   // Now get the cell
  Triangulation<dim>::cell_iterator cell = triangulation.begin();
  cell->vertex(0)(0)=-1.;
                                      
				   // and test it.
  double testcoord[14][3] = {{0.5,0.5,0.5},
			     {2,0.5,0.5},
			     {0.5,2,0.5},
			     {0.5,0.5,2},
			     {-2,0.5,0.5},
			     {0.5,-2,0.5},
			     {0.5,0.5,-2},
			     {0.9,0.9,0.9},
                            {1.0,0.5,0.5},
                            {0.9999999,0.5,0.5},
                            {1.0000001,0.5,0.5},
                            {-0.1,0.1,0.1},
                            {-0.24,0.5,0.5},
                            {-0.26,0.5,0.5}
                            };
    
  const bool expected2d[] = {1,0,0,1,0,0,1,1,1,1,0,1,1,1};
  const bool expected3d[] = {1,0,0,0,0,0,0,1,1,1,0,1,1,0};
  const bool *expected=dim==2 ? expected2d : expected3d;
  for (int i=0; i<14; i++)
    {
      Point<dim> testpoint;
      testpoint(0)=testcoord[i][0];
      testpoint(1)=testcoord[i][1];
      if (dim==3)
	testpoint(2)=testcoord[i][2];

      bool res = cell->point_inside(testpoint);
      deallog << testpoint << "  \t inside " << res 
	      << " expected " << expected[i] << std::endl;
      Assert (res == expected[i], ExcInternalError());
    }    
}


int main () 
{
  std::ofstream logfile("point_inside_2.output");
  deallog.attach(logfile);
  deallog.depth_console(0);
  deallog.threshold_double(1.e-10);

  check<2> ();
  check<3> ();
}
