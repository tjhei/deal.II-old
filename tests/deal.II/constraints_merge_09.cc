//----------------------------  constraints_merge_09.cc  ---------------------------
//    $Id$
//    Version: $Name$
//
//    Copyright (C) 1998, 1999, 2000, 2001, 2003, 2004, 2005, 2008, 2010, 2011 by the deal.II authors
//
//    This file is subject to QPL and may not be  distributed
//    without copyright and license information. Please refer
//    to the file deal.II/doc/license.html for the  text  and
//    further information on this license.
//
//----------------------------  constraints_merge_09.cc  ---------------------------


// merge and print a bunch of ConstrainMatrices. test the case where we have
// conflicting constraints and the right object wins (winning constraint
// introduces a chain)
//
// like _06, but using localized lines

#include "../tests.h"
#include <deal.II/lac/constraint_matrix.h>
#include <deal.II/base/logstream.h>

#include <fstream>
#include <iomanip>


std::ofstream logfile("constraints_merge_09/output");


void merge_check ()
{
  deallog << "Checking ConstraintMatrix::merge with localized lines" << std::endl;

				// set local lines to a very large range that
				// surely triggers an error if the
				// implementation is wrong
  IndexSet local_lines (100000000);
  local_lines.add_range (99999890, 99999900);
  local_lines.add_range (99999990,100000000);
  local_lines.compress();

				// the test is the same as
				// constraints_merge_02, but we add very large
				// indices here
  const unsigned int index_0  = local_lines.nth_index_in_set(0);
  const unsigned int index_2  = local_lines.nth_index_in_set(2);
  const unsigned int index_11 = local_lines.nth_index_in_set(11);
  const unsigned int index_13 = local_lines.nth_index_in_set(13);

  deallog << "Number of local lines: "
	  << local_lines.n_elements() << std::endl;

				   // check twice, once with closed
				   // objects, once with open ones
  for (unsigned int run=0; run<2; ++run)
    {
      deallog << "Checking with " << (run == 0 ? "open" : "closed")
	      << " objects" << std::endl;

				       // check that the `merge' function
				       // works correctly
      ConstraintMatrix c1 (local_lines), c2 (local_lines);

				       // enter simple line
      c1.add_line          (index_0);
      c1.add_entry         (index_0, index_11, 1.);
      c1.set_inhomogeneity (index_0, 42);

      c1.add_line          (index_13);
      c1.add_entry         (index_13, index_2, 0.5);
      c1.set_inhomogeneity (index_13, 2);

				       // fill second constraints
				       // object that has a conflict
      c2.add_line          (index_0);
      c2.add_entry         (index_0, index_13, 2.);
      c2.set_inhomogeneity (index_0, 142);
				       // in one of the two runs,
				       // close the objects
      if (run == 1)
	{
	  c1.close ();
	  c2.close ();
	};

				       // now merge the two and print the
				       // results
      try
	{
	  c1.merge (c2, ConstraintMatrix::right_object_wins);
	}
      catch (...)
	{
	  Assert (false, ExcInternalError());
	}

      c1.print (logfile);
    }
}


int main ()
{
  deallog << std::setprecision (2);
  logfile << std::setprecision (2);
  deallog.attach(logfile);
  deallog.depth_console(0);
  deallog.threshold_double(1.e-10);

  merge_check ();
}

