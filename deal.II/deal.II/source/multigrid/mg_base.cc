//----------------------------  mg_base.cc  ---------------------------
//    $Id$
//    Version: $Name$
//
//    Copyright (C) 1998, 1999, 2000 by the deal.II authors
//
//    This file is subject to QPL and may not be  distributed
//    without copyright and license information. Please refer
//    to the file deal.II/doc/license.html for the  text  and
//    further information on this license.
//
//----------------------------  mg_base.cc  ---------------------------


#include <multigrid/mg_base.h>
#include <multigrid/mg_smoother.h>
#include <iostream>
#include <cmath>


MGBase::~MGBase ()
{}


MGBase::MGBase(const MGTransferBase &transfer,
	       const unsigned        minlevel,
	       const unsigned        maxlevel)
		:
		maxlevel(maxlevel),
		minlevel(minlevel),
		defect(minlevel,maxlevel),
		solution(minlevel,maxlevel),
		transfer(&transfer)
{
  Assert(minlevel <= maxlevel,
	 ExcSwitchedLevels(minlevel, maxlevel));
}


void
MGBase::vcycle(const MGSmootherBase     &pre_smooth,
	       const MGSmootherBase     &post_smooth,
	       const MGCoarseGridSolver &coarse_grid_solver)
{
  level_mgstep (maxlevel, pre_smooth, post_smooth, coarse_grid_solver);
//  abort ();
}


void
MGBase::level_mgstep(const unsigned int        level,
		     const MGSmootherBase     &pre_smooth,
		     const MGSmootherBase     &post_smooth,
		     const MGCoarseGridSolver &coarse_grid_solver)
{
#ifdef MG_DEBUG
  char *name = new char[100];
  sprintf(name, "MG%d-defect",level);
  print_vector(level, defect[level], name);
#endif

  solution[level] = 0.;
  
  if (level == minlevel)
    {
      coarse_grid_solver(level, solution[level], defect[level]);
#ifdef MG_DEBUG
      sprintf(name, "MG%d-solution",level);
      print_vector(level, solution[level], name);
#endif
      return;
    }

			   // smoothing of the residual by modifying s
  pre_smooth.smooth(level, solution[level], defect[level]);
				   // t = d-As

#ifdef MG_DEBUG
  sprintf(name, "MG%d-pre",level);
  print_vector(level, solution[level], name);
#endif
  
  t.reinit(solution[level].size());
  level_vmult(level, t, solution[level], defect[level]);
  
				   // make t rhs of lower level
				   // The non-refined parts of the
				   // coarse-level defect already contain
				   // the global defect.
  transfer->restrict_and_add (level, defect[level-1], t);

				   // add additional DG contribution
  edge_vmult(level, defect[level-1], defect[level]);
  
				   // do recursion
  level_mgstep(level-1, pre_smooth, post_smooth, coarse_grid_solver);

				   // reset size of the auxiliary
				   // vector, since it has been
				   // resized in the recursive call to
				   // level_mgstep directly above
  t.reinit(solution[level].size());

				   // do coarse grid correction

  transfer->prolongate(level, t, solution[level-1]);

#ifdef MG_DEBUG
  sprintf(name, "MG%d-cgc",level);
  print_vector(level, t, name);
#endif

  solution[level] += t;
  
				   // post-smoothing

  post_smooth.smooth(level, solution[level], defect[level]);

#ifdef MG_DEBUG
  sprintf(name, "MG%d-post",level);
  print_vector(level, solution[level], name);

  delete[] name;
#endif
}


void
MGBase::edge_vmult (const unsigned int,
		    Vector<double>&,
		    const Vector<double>&)
{}

//////////////////////////////////////////////////////////////////////

MGCoarseGridSolver::~MGCoarseGridSolver()
{};


//////////////////////////////////////////////////////////////////////

MGTransferBase::~MGTransferBase()
{};


