// $Id$

#include <base/exceptions.h>
#include <base/point.h>
#include <grid/tria.h>
#include <grid/dof.h>
#include <grid/dof_constraints.h>
#include <grid/tria_boundary.h>
#include <lac/vector_memory.h>
#include <lac/dvector.h>
#include <lac/dsmatrix.h>

class AdvMatrix :
  public dSMatrix
{
public:
  void precondition(dVector& dst, const dVector& src) const
      {
	dSMatrix::precondition_SSOR(dst, src);
      }
};


class Laplace
{
protected:
  Point<2> direction;
  Triangulation<2> tr;
  DoFHandler<2> dof_primal;

  dSMatrixStruct matrix_structure;
  AdvMatrix A;
  
  dVector u;
  dVector z;
  dVector f;

  PrimitiveVectorMemory<dVector> mem;
  
  ConstraintMatrix hanging_nodes;

  StraightBoundary<2> boundary;
  
public:
  Laplace();
  ~Laplace();

  void remesh(unsigned int global_refine = 0);
  void assemble_primal();
  void solve_primal();

  void write_data(const char* name);
};

