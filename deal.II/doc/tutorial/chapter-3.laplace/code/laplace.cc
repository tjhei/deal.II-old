// $Id$

const char* laplaceversion = "Laplace: $Revision$";

#include "laplace.h"
#include "functions.h"

#include <lac/solver_cg.h>
#include <grid/tria_accessor.h>
#include <grid/dof_accessor.h>
#include <grid/tria_iterator.h>
#include <grid/tria_boundary.h>
#include <grid/dof_constraints.h>
#include <basic/data_io.h>
#include <base/function.h>
#include <base/parameter_handler.h>
#include <fe/fe_lib.lagrange.h>
#include <fe/fe_lib.criss_cross.h>
#include <fe/fe_values.h>
#include <base/quadrature_lib.h>
#include <numerics/matrices.h>
#include <numerics/vectors.h>
#include <lac/vector.h>
#include <lac/fullmatrix.h>

#include <cmath>
#include <fstream>
#include <iomanip>

#define PRIMEL FELinear<2>

// Finite Elements

static PRIMEL fe;

// Quadrature formulae

static QGauss2<2> qc;
static QGauss2<1> qf;
static QGauss5<2> qc_integrate;
static QGauss5<1> qf_integrate;

StraightBoundary<2> stb;

Laplace::Laplace()
	     : dof(&tr)		
{
  // Generate the initial triangulation
  tr.create_hypercube(-1.,1.);

  // Distribute the degrees of freedom
  dof.distribute_dofs(fe);
 }

Laplace::~Laplace()
{}


// Remesh the grid
void
Laplace::remesh(unsigned int steps)
{
  if (tr.n_levels() <= 1)
  {
    tr.refine_global(1); // refine globally
  }

  if (steps)
    tr.refine_global(steps);
  else
    tr.execute_coarsening_and_refinement();  // refine locally

  // redistribute the degrees of freedom...
  dof.distribute_dofs(fe);
  // ...and renumber them so they can be used more efficiently
  dof.renumber_dofs(Cuthill_McKee);
}

// JS.Primales Problem zusammenstellen.
void
Laplace::assemble()
{
  // Initialize the problem matrix, i.e. reserve storage space
  matrix_structure.reinit(dof.n_dofs(),dof.n_dofs(),
			  dof.max_couplings_between_dofs());
  // Generate the matrix structure
  dof.make_sparsity_pattern(matrix_structure);
  hanging_nodes.clear();
  dof.make_constraint_matrix(hanging_nodes);
  hanging_nodes.condense(matrix_structure);
  
  // The problem has the form Au=f.
  A.reinit(matrix_structure);
  f.reinit(dof.n_dofs());

  // Calculate the trial functions on the cell faces.
  FEValues<2> fevalues(fe, qc, UpdateFlags(update_gradients |
						   update_JxW_values));
  FEFaceValues<2> ffvalues(fe, qf,
			   UpdateFlags(update_JxW_values | update_q_points));

  // Integrate the problem locally...
  vector<int> indices(fe.total_dofs);
  Vector<float> elvec(fe.total_dofs);
  
  FullMatrix<float> elmat(fe.total_dofs);
  
  for (DoFHandler<2>::active_cell_iterator c = dof.begin_active()
					; c != dof.end() ; ++c)
  {
    fevalues.reinit(c, stb);
    elmat.clear();
    elvec.clear();
    c->get_dof_indices(indices);
    
    for (unsigned k=0;k<qc.n_quadrature_points;++k)
    {
      for (unsigned i=0;i<fe.total_dofs;++i)
      {
	const Point<2> dv = fevalues.shape_grad(i,k);

	
	for (unsigned j=0;j<fe.total_dofs;++j)
	{
	  const Point<2> du = fevalues.shape_grad(j,k);
	  
	  elmat(i,j) += fevalues.JxW(k)
			* du * dv
			;
	  
	}
      }
    }
    // ...and insert the local matrix into the global one.
    for (unsigned i=0;i<fe.total_dofs;++i)
    {
      f(indices[i]) += elvec(i);
            
      for (unsigned j=0;j<fe.total_dofs;++j)
      {
	A.add(indices[i], indices[j], elmat(i,j));
      }
    }
  }    

  // Insert the boundary conditions into the matrix.
  map<int,double> boundary_values;
  DoFHandler<2>::FunctionMap dirichlet_bc;
  BoundaryFct bfkt;
  dirichlet_bc[0]=&bfkt;
  VectorTools<2>::interpolate_boundary_values(dof,dirichlet_bc,
					      fe,boundary,
					      boundary_values);
  u.reinit(f);
  MatrixTools<2>::apply_boundary_values(boundary_values,A,u,f);
}

// Solve the primal problem.
void
Laplace::solve()
{
  // Solver control: max 1000 iterations, threshold 1e-10
  SolverControl control(1000, 1.e-10);
  // Define the solver.
  SolverCG<AdvMatrix, Vector<float>> solver(control, mem);
  
  solver.solve(A,u,f);
  
  hanging_nodes.distribute(u);
}


// Data output for gnuplot.
void Laplace::write_data(const char* name)
{

  DataOut<2> out;
  char fname[100];
  {
    ofstream gnuplot(fname);

    out.clear_data_vectors();
    out.attach_dof_handler(dof);
    out.add_data_vector(u,"solution","kg");
    
    out.write_gnuplot (gnuplot, 1);
    gnuplot.close ();
  }
     
}




