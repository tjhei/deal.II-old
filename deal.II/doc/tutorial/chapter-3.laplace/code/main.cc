// $Id$

#include "laplace.h"
#include "functions.h"

#include <iostream.h>
#include <fstream.h>
#include <stdlib.h>

char fname[30];

main(int argc, char** argv)
{
  if (argc==1)
    cerr << "Usage: " << argv[0] << "firstgrid\nUsing 3"
	 << endl;
  
  int firstgrid = 3;

  if (argc>=2) firstgrid = atoi(argv[1]);

  Laplace lap;
  
  for (unsigned step = 0; step < 3 ; ++step)
  {
    // Generate a refined grid.
    if (!step)
      lap.remesh(firstgrid);
    else
    {
      lap.remesh(1);
    }

    
    // Assemble and solve the problem.
    lap.assemble();
    lap.solve();
       
    // Data output.
    lap.write_data(fname);
  }
}
