// $Id$

// JS. 
const char* funcversion = "Functions: $Revision$";

#include "functions.h"

#include <cmath>

double
WeightFunction::operator() (const Point<2>& p) const
{
  //  double r = p(0)*p(0) + p(1) * p(1);
  //if (r>=.8) return 0.;
  //return 1.-r*(2.-r);
  return 1.;

}

double 
BoundaryFct::operator ()(const Point<2> &p) const
{ 
  return sin(4*M_PI*p(0))*sin(4*M_PI*p(1));
}
