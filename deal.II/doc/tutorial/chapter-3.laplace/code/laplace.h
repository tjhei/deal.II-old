// $Id$

#include <base/exceptions.h>
#include <base/point.h>
#include <grid/tria.h>
#include <grid/dof.h>
#include <grid/dof_constraints.h>
#include <grid/tria_boundary.h>
#include <lac/vector_memory.h>
#include <lac/vector.h>
#include <lac/sparsematrix.h>

class AdvMatrix : 
  public SparseMatrix<double>
{
public:
  void precondition(Vector<double>& dst, const Vector<double>& src) const
      {
	SparseMatrix::precondition_SSOR(dst, src);
      }
};


class Laplace
{
protected:
  Point<2> direction;
  Triangulation<2> tr;
  DoFHandler<2> dof;

  SparseMatrixStruct matrix_structure;
  AdvMatrix A;
  
  Vector<float> u;
  Vector<float> z;
  Vector<float> f;

  PrimitiveVectorMemory<float> mem;
  
  ConstraintMatrix hanging_nodes;

  StraightBoundary<2> boundary;
  
public:
  Laplace();
  ~Laplace();

  void remesh(unsigned int global_refine = 0);
  void assemble();
  void solve();

  void write_data(const char* name);
};

