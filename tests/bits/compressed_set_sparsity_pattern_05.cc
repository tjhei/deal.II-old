//----------------------------  compressed_set_sparsity_pattern_05.cc  ---------------------------
//    $Id$
//    Version: $Name$ 
//
//    Copyright (C) 2004, 2005, 2007 by the deal.II authors
//
//    This file is subject to QPL and may not be  distributed
//    without copyright and license information. Please refer
//    to the file deal.II/doc/license.html for the  text  and
//    further information on this license.
//
//----------------------------  compressed_set_sparsity_pattern_05.cc  ---------------------------


// check CompressedSetSparsityPattern::symmetrize. since we create quite some
// output here, choose smaller number of rows and entries than in the other
// tests

#include "../tests.h"
#include <base/logstream.h>
#include <lac/compressed_set_sparsity_pattern.h>
#include <iostream>
#include <fstream>


void test ()
{
  const unsigned int N = 100;
  CompressedSetSparsityPattern csp (N,N);
  for (unsigned int i=0; i<N; ++i)
    for (unsigned int j=0; j<10; ++j)
      csp.add (i, (i+(i+1)*(j*j+i))%N);
  csp.symmetrize ();

  for (unsigned int i=0; i<N; ++i)
    {
      unsigned int index = 0;
      for (CompressedSetSparsityPattern::row_iterator
	     j = csp.row_begin(i); j != csp.row_end(i); ++j, ++index)
	deallog << i << ' ' << index << ' ' << *j
		<< std::endl;
    }
}



int main () 
{
  std::ofstream logfile("compressed_set_sparsity_pattern_05/output");
  deallog.attach(logfile);
  deallog.depth_console(0);
  deallog.threshold_double(1.e-10);

  test ();
  return 0;
}
