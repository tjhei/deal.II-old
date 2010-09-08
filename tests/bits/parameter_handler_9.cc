//----------------------------  parameter_handler_9.cc  ---------------------------
//    $Id$
//    Version: $Name$
//
//    Copyright (C) 2003, 2004, 2005, 2010 by the deal.II authors
//
//    This file is subject to QPL and may not be  distributed
//    without copyright and license information. Please refer
//    to the file deal.II/doc/license.html for the  text  and
//    further information on this license.
//
//----------------------------  parameter_handler_9.cc  ---------------------------


// test ParameterHandler::log_parameters(Text)

#include "../tests.h"
#include <base/logstream.h>
#include <base/parameter_handler.h>
#include <fstream>
#include <iomanip>


int main ()
{
  try
    {
      std::ofstream logfile("parameter_handler_9/output");
      deallog.attach(logfile);
      deallog.depth_console(0);
      deallog.threshold_double(1.e-10);

      ParameterHandler prm;
      prm.enter_subsection ("Testing testing");
      {
	prm.declare_entry ("string list",
			   "a",
			   Patterns::List(Patterns::Selection("a|b|c|d|e|f|g|h")),
			   "docs 1");
	prm.declare_entry ("int/int",
			   "1",
			   Patterns::Integer());
	prm.declare_entry ("double_double",
			   "3.1415926",
			   Patterns::Double(),
			   "docs 3");

	prm.enter_subsection ("Testing%testing");
	{
	  prm.declare_entry ("string&list",
			     "a,b,c",
			     Patterns::List(Patterns::Selection("a|b|c|d|e|f|g|h")),
			     "docs 1");
	  prm.declare_entry ("int*int",
			     "2",
			     Patterns::Integer());
	  prm.declare_entry ("double+double",
			     "6.1415926",
			     Patterns::Double(),
			     "docs 3");
	}
	prm.leave_subsection ();
      }
      prm.leave_subsection ();

                                       // read and then write
                                       // parameters. take same input file
                                       // as for parameter_handler_3, but
                                       // use different output format
      prm.read_input("parameter_handler_8/prm");
      prm.log_parameters (deallog);
    }
  catch (std::exception &exc)
    {
      deallog << std::endl << std::endl
		<< "----------------------------------------------------"
		<< std::endl;
      deallog << "Exception on processing: " << std::endl
		<< exc.what() << std::endl
		<< "Aborting!" << std::endl
		<< "----------------------------------------------------"
		<< std::endl;

      return 1;
    }
  catch (...)
    {
      deallog << std::endl << std::endl
		<< "----------------------------------------------------"
		<< std::endl;
      deallog << "Unknown exception!" << std::endl
		<< "Aborting!" << std::endl
		<< "----------------------------------------------------"
		<< std::endl;
      return 1;
    };

  return 0;
}
