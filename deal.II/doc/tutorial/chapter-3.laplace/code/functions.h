// $Id$

// JS.Wird das File ueberhaupt gebraucht ?

#include <base/function.h>

class WeightFunction
  : public Function<2>
{
public:
  WeightFunction()
      {}
  virtual double operator()(const Point<2> &p) const;
};

class BoundaryFct
  : public Function<2>
{
 public:
  virtual double operator()(const Point<2> &p) const;
};

