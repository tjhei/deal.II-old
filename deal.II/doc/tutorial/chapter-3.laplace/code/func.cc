// $Id$

// JS. 
const char* funcversion = "Functions: $Revision$";

#include "functions.h"

#include <cmath>


double 
BoundaryFct::operator ()(const Point<2> &p) const
{ 
  return cos(2*M_PI*p(0))*cos(2*M_PI*p(1));
}
