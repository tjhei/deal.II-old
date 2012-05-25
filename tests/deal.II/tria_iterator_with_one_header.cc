//----------------------------  tria_iterator_with_one_header.cc  ---------------------------
//    $Id$
//    Version: $Name$
//
//    Copyright (C) 2001, 2002, 2003, 2004, 2005, 2006, 2008, 2012 by the deal.II authors
//
//    This file is subject to QPL and may not be  distributed
//    without copyright and license information. Please refer
//    to the file deal.II/doc/license.html for the  text  and
//    further information on this license.
//
//----------------------------  tria_iterator_with_one_header.cc  ---------------------------

// It used to be that if you wanted to use
// Triangulation::active_cell_iterator in a context where that type
// was actually used, you also had to #include
// <deal.II/grid/tria_iterator.h> and <deal.II/grid/tria_accessor.h>.
// This was changed in r25531 in such a way that these types are now
// no longer only forward declared. test that this continues to work

#include "../tests.h"
#include <deal.II/base/logstream.h>
#include <deal.II/grid/tria.h>
#include <deal.II/grid/grid_generator.h>

#include <fstream>
#include <iomanip>

std::ofstream logfile("tria_iterator_with_one_header/output");


template <int dim>
void test ()
{
  Triangulation<dim> tria;
  GridGenerator::hyper_cube(tria);
  deallog << tria.begin_active()->center() << std::endl;
}


int main ()
{
  deallog << std::setprecision(4);
  logfile << std::setprecision(4);
  deallog.attach(logfile);
  deallog.depth_console(0);
  deallog.threshold_double(1.e-10);

  test<1> ();
  test<2> ();
  test<3> ();

  return 0;
}
