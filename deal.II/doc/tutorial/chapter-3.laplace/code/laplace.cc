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

#include <base/logstream.h>

#include <cmath>
#include <fstream>
#include <iomanip>

#define PRIMEL FELinear<2>
#define DUEL FEQuadraticSub<2>

// Finite Elements

static PRIMEL fe_primal;
// JS.static DUEL fe_dual;

// Quadrature formulae

static QGauss2<2> qc_primal;
static QGauss2<1> qf_primal;
// static QGauss3<2> qc_dual;
// static QGauss3<1> qf_dual;
static QGauss5<2> qc_integrate;
static QGauss5<1> qf_integrate;

StraightBoundary<2> stb;

// JS.ist im Moment noch PureTransportSolution...
Laplace::Laplace()
	     : dof_primal(&tr)		
{
  // JS.Triangulation generieren. Zellränder werden numeriert.
  tr.create_hypercube(-1.,1.);

  // JS.Freiheitsgrade verteilen. D.h. Zellen numerieren.
  dof_primal.distribute_dofs(fe_primal);
 }

Laplace::~Laplace()
{}


// JS.Gitter verfeinern.
void
Laplace::remesh(unsigned int steps)
{
  if (tr.n_levels() <= 1)
  {
    tr.refine_global(1); //JS.Lokal ist execute_coarsening_etc...
  }

  if (steps)
    tr.refine_global(steps);
  else
    tr.execute_coarsening_and_refinement();

  // JS. Freiheitsgrade neu verteilen.
  dof_primal.distribute_dofs(fe_primal);
  // JS. und dem Problem angemessener nochmal numerieren.
  dof_primal.renumber_dofs(Cuthill_McKee);
  deallog << "Cells " << tr.n_active_cells()
	  << "  PrimalDoFs " << dof_primal.n_dofs()
	  << endl;
}

// JS.Primales Problem zusammenstellen.
void
Laplace::assemble_primal(const Function<2>&,const Function<2>&)
{
  deallog << "Assembling primal problem" << endl;
  // JS. Platz für neue Matrix mit (?) (2x = quadratisch) Anzahl der Zellen, 
  // Anzahl der Kopplungen (Matrix dünn besetzt, für effizientes Speichern)
  matrix_structure.reinit(dof_primal.n_dofs(),dof_primal.n_dofs(),
			  dof_primal.max_couplings_between_dofs());
  // JS.Hängende Noden in die Matrix einbauen; d.h. Matrix generieren.
  dof_primal.make_sparsity_pattern(matrix_structure);
  hanging_nodes.clear();
  dof_primal.make_constraint_matrix(hanging_nodes);
  hanging_nodes.condense(matrix_structure);
  
  //JS.Problem der Form Au=f.
  A.reinit(matrix_structure);
  f.reinit(dof_primal.n_dofs());

  // JS.Ansatzfunktionen auf Zellrändern im Voraus berechnen aus
  // Effizienzgründen.
  FEValues<2> fevalues(fe_primal, qc_primal, UpdateFlags(update_gradients |
						   update_JxW_values));
  FEFaceValues<2> ffvalues(fe_primal, qf_primal,
			   UpdateFlags(update_JxW_values | update_q_points));
  //JS.Ab hier lokales Problem el... = Finites Element...
  //JS. Index für eine Zelle, für späteren Einbau in globale Matrix.
  vector<int> indices(fe_primal.total_dofs);
  dVector elvec(fe_primal.total_dofs);
  
  dFMatrix elmat(fe_primal.total_dofs);
  
  // JS.Einmal alle Zellen durchlaufen
  for (DoFHandler<2>::active_cell_iterator c = dof_primal.begin_active()
					; c != dof_primal.end() ; ++c)
  {
    fevalues.reinit(c, stb);
    elmat.clear();
    elvec.clear();
    c->get_dof_indices(indices);
    
    // JS.Integration des Problems. Diese Schleifenfolge für Effizienz.
    for (unsigned k=0;k<qc_primal.n_quadrature_points;++k)
    {
      for (unsigned i=0;i<fe_primal.total_dofs;++i)
      {
	const Point<2> dv = fevalues.shape_grad(i,k);

	
	for (unsigned j=0;j<fe_primal.total_dofs;++j)
	{
	  const Point<2> du = fevalues.shape_grad(j,k);
	  
	  elmat(i,j) += fevalues.JxW(k)
			* du * dv
			;
	  
	}
      }
    }
    // JS.Lokale Matrix in globale einbauen.
    for (unsigned i=0;i<fe_primal.total_dofs;++i)
    {
      //      f(indices[i]) += elvec(i);
      f(indices[i]) = 0;
      
      for (unsigned j=0;j<fe_primal.total_dofs;++j)
      {
	A.add(indices[i], indices[j], elmat(i,j));
      }
    }
  }    

    // JS. Randwerte.
  map<int,double> boundary_values;
  DoFHandler<2>::FunctionMap dirichlet_bc;
  BoundaryFct bfkt;
  dirichlet_bc[0]=&bfkt;
  VectorTools<2>::interpolate_boundary_values(dof_primal,dirichlet_bc,
					      fe_primal,boundary,
					      boundary_values);
  u.reinit(f);
  MatrixTools<2>::apply_boundary_values(boundary_values,A,u,f);
    
  
  // JS.Hängende Noden einbauen.
  //  hanging_nodes.condense(A);
  //hanging_nodes.condense(f);
}

// JS. Primales Problem lösen.
void
Laplace::solve_primal()
{
  deallog.push("Solve");

  // JS.Empfindlichkeit des Lösers einstellen.
  SolverControl control(1000, 1.e-10);
  // JS. Löser definieren. modifiziertes cg-Verfahren, 
  SolverCG<AdvMatrix, dVector> solver(control, mem);
  
  // JS.???
  //  u.reinit(f);
  
  // JS.lösen.
  solver.solve(A,u,f);
  
  // JS.???
  hanging_nodes.distribute(u);

  deallog.pop();
}


// JS. Datenausgabe im Gnuplot-Format.
void Laplace::write_data(const char* name)
{
  deallog << "Writing gnuplot" << endl;

  DataOut<2> out;
  char fname[100];
  sprintf(fname,"P_%s",name);
  
  {
    ofstream gnuplot(fname);

    out.clear_data_vectors();
    out.attach_dof_handler(dof_primal);
    out.add_data_vector(u,"solution","kg");
    
    out.write_gnuplot (gnuplot, 1);
    gnuplot.close ();
  }
     
}


// JS. Ergebnis zurückgeben. Wie funktioniert das ?
double
Laplace::result(const Function<2>& interior, const Function<2>& boundary)
{
  double erg = 0., ergex = 0.;
  FEValues<2> fevalues(fe_primal, qc_integrate,
		       UpdateFlags(update_q_points | update_JxW_values));
  FEFaceValues<2> ffvalues(fe_primal, qf_integrate,
			   UpdateFlags(update_q_points | update_JxW_values));
  vector<double> uh(qc_integrate.n_quadrature_points);
  vector<double> uf(qf_integrate.n_quadrature_points);

  // JS.Alle Zellen durch.
  for (DoFHandler<2>::active_cell_iterator c = dof_primal.begin_active()
					; c != dof_primal.end() ; ++c)
  {
    double s = 0.;
    fevalues.reinit(c, stb);
    
    fevalues.get_function_values(u, uh);
    
    for (unsigned int k=0;k<qc_integrate.n_quadrature_points;++k)
    {
      s += fevalues.JxW(k)
	   * uh[k] * interior(fevalues.get_quadrature_points()[k]);
      ergex += fevalues.JxW(k)
	//  * exact(fevalues.get_quadrature_points()[k])
	       * interior(fevalues.get_quadrature_points()[k]);
    }
    // JS.Alle Zellränder.
    for (unsigned fi=0;fi<GeometryInfo<2>::faces_per_cell;++fi)
    {
      DoFHandler<2>::face_iterator f = c->face(fi);
      unsigned char bi = f->boundary_indicator();
      if (bi == 0xFF) continue;
      ffvalues.reinit(c, fi, stb);
      ffvalues.get_function_values(u, uf);	
      
      // JS.??? Integrationspunkte ???
      for (unsigned k=0;k<qf_primal.n_quadrature_points;++k)
      {
	s += ffvalues.JxW(k)
	     * uf[k] * boundary(ffvalues.get_quadrature_points()[k]);
      }
    }
    erg += s;
  }
  deallog << "Results " << setw(18) << setprecision(15) << erg << " " << ergex << endl;
  return erg;
}

// JS. Gitter anpassen
void
Laplace::adapt()
{
  tr.refine_and_coarsen_fixed_fraction(f, .5, 0);
}



