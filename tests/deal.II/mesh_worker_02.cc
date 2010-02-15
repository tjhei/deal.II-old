//----------------------------------------------------------------------
//    $Id$
//    Version: $Name$ 
//
//    Copyright (C) 2000, 2001, 2003, 2004, 2007, 2008, 2009, 2010 by the deal.II authors
//
//    This file is subject to QPL and may not be  distributed
//    without copyright and license information. Please refer
//    to the file deal.II/doc/license.html for the  text  and
//    further information on this license.
//
//----------------------------------------------------------------------

// Test consistency of assemblers MatrixSimple and MGMatrixSimple

#include "../tests.h"
#include <numerics/mesh_worker.h>
#include <numerics/mesh_worker_assembler.h>
#include <numerics/mesh_worker_loop.h>

#include <base/logstream.h>
#include <lac/compressed_sparsity_pattern.h>
#include <lac/sparsity_pattern.h>
#include <lac/sparse_matrix.h>
#include <grid/grid_generator.h>
#include <dofs/dof_tools.h>
#include <fe/mapping_q1.h>
#include <fe/fe_q.h>
#include <fe/fe_dgp.h>
#include <multigrid/mg_tools.h>

#include <fstream>
#include <iomanip>

using namespace dealii;


// Use local matrices for Laplacian / interior penalty DG
template <int dim>
class MatrixIntegrator : public Subscriptor
{
  public:
    static void cell(MeshWorker::DoFInfo<dim>& dinfo,
		     typename MeshWorker::IntegrationWorker<dim>::CellInfo& info);
    static void bdry(MeshWorker::DoFInfo<dim>& dinfo,
		     typename MeshWorker::IntegrationWorker<dim>::FaceInfo& info);
    static void face(MeshWorker::DoFInfo<dim>& dinfo1,
		     MeshWorker::DoFInfo<dim>& dinfo2,
		     typename MeshWorker::IntegrationWorker<dim>::FaceInfo& info1,
		     typename MeshWorker::IntegrationWorker<dim>::FaceInfo& info2);
};


template <int dim>
void MatrixIntegrator<dim>::cell(MeshWorker::DoFInfo<dim>& dinfo,
				 typename MeshWorker::IntegrationWorker<dim>::CellInfo& info)
{
  const FEValuesBase<dim>& fe = info.fe_values();
  FullMatrix<double>& local_matrix = dinfo.matrix(0).matrix;
  
  for (unsigned int k=0; k<fe.n_quadrature_points; ++k)
    for (unsigned int i=0; i<fe.dofs_per_cell; ++i)
      for (unsigned int j=0; j<fe.dofs_per_cell; ++j)
	local_matrix(i,j) += (fe.shape_grad(i,k) * fe.shape_grad(j,k))
			     * fe.JxW(k);
}


template <int dim>
void MatrixIntegrator<dim>::bdry(MeshWorker::DoFInfo<dim>& dinfo,
				 typename MeshWorker::IntegrationWorker<dim>::FaceInfo& info)
{
  const FEFaceValuesBase<dim>& fe = info.fe_values();
  FullMatrix<double>& local_matrix = dinfo.matrix(0).matrix;
  
  const unsigned int deg = fe.get_fe().tensor_degree();
  const double penalty = 2. * deg * (deg+1) * dinfo.face->measure() / dinfo.cell->measure();
  
  for (unsigned k=0;k<fe.n_quadrature_points;++k)
    for (unsigned int i=0; i<fe.dofs_per_cell; ++i)
      for (unsigned int j=0; j<fe.dofs_per_cell; ++j)
	local_matrix(i,j) += (fe.shape_value(i,k) * penalty * fe.shape_value(j,k)
			      - (fe.normal_vector(k) * fe.shape_grad(i,k)) * fe.shape_value(j,k)
			      - (fe.normal_vector(k) * fe.shape_grad(j,k)) * fe.shape_value(i,k))
			     * fe.JxW(k);
}


template <int dim>
void MatrixIntegrator<dim>::face(MeshWorker::DoFInfo<dim>& dinfo1,
				 MeshWorker::DoFInfo<dim>& dinfo2,
				 typename MeshWorker::IntegrationWorker<dim>::FaceInfo& info1,
				 typename MeshWorker::IntegrationWorker<dim>::FaceInfo& info2)
{
  const FEFaceValuesBase<dim>& fe1 = info1.fe_values();
  const FEFaceValuesBase<dim>& fe2 = info2.fe_values();
  FullMatrix<double>& matrix_v1u1 = dinfo1.matrix(0, false).matrix;
  FullMatrix<double>& matrix_v1u2 = dinfo1.matrix(0, true).matrix;
  FullMatrix<double>& matrix_v2u1 = dinfo2.matrix(0, true).matrix;
  FullMatrix<double>& matrix_v2u2 = dinfo2.matrix(0, false).matrix;
  
  const unsigned int deg = fe1.get_fe().tensor_degree();
  const double penalty1 = deg * (deg+1) * dinfo1.face->measure() / dinfo1.cell->measure();
  const double penalty2 = deg * (deg+1) * dinfo2.face->measure() / dinfo2.cell->measure();
  const double penalty = penalty1 + penalty2;
  
  for (unsigned k=0;k<fe1.n_quadrature_points;++k)
    for (unsigned int i=0; i<fe1.dofs_per_cell; ++i)
      for (unsigned int j=0; j<fe1.dofs_per_cell; ++j)
	{
	  matrix_v1u1(i,j) += (fe1.shape_value(i,k) * penalty * fe1.shape_value(j,k)
			       - (fe1.normal_vector(k) * fe1.shape_grad(i,k)) * fe1.shape_value(j,k)
			       - (fe1.normal_vector(k) * fe1.shape_grad(j,k)) * fe1.shape_value(i,k)
	  ) * .5 * fe1.JxW(k);
	  matrix_v1u2(i,j) += (-fe1.shape_value(i,k) * penalty * fe2.shape_value(j,k)
			       + (fe1.normal_vector(k) * fe1.shape_grad(i,k)) * fe2.shape_value(j,k)
			       - (fe1.normal_vector(k) * fe2.shape_grad(j,k)) * fe1.shape_value(i,k)
	  ) * .5 * fe1.JxW(k);
	  matrix_v2u1(i,j) += (-fe2.shape_value(i,k) * penalty * fe1.shape_value(j,k)
			       - (fe1.normal_vector(k) * fe2.shape_grad(i,k)) * fe1.shape_value(j,k)
			       + (fe1.normal_vector(k) * fe1.shape_grad(j,k)) * fe2.shape_value(i,k)
	  ) * .5 * fe1.JxW(k);
	  matrix_v2u2(i,j) += (fe2.shape_value(i,k) * penalty * fe2.shape_value(j,k)
			       + (fe1.normal_vector(k) * fe2.shape_grad(i,k)) * fe2.shape_value(j,k)
			       + (fe1.normal_vector(k) * fe2.shape_grad(j,k)) * fe2.shape_value(i,k)
	  ) * .5 * fe1.JxW(k);
	}
}


template <int dim>
void
assemble(const DoFHandler<dim>& dof_handler, SparseMatrix<double>& matrix)
{
  const FiniteElement<dim>& fe = dof_handler.get_fe();
  MappingQ1<dim> mapping;
  
  MeshWorker::IntegrationWorker<dim> integration_worker;
  MeshWorker::Assembler::MatrixSimple<SparseMatrix<double> > assembler;
  
  const unsigned int n_gauss_points = dof_handler.get_fe().tensor_degree()+1;
  integration_worker.initialize_gauss_quadrature(n_gauss_points, n_gauss_points, n_gauss_points);
  UpdateFlags update_flags = update_values | update_gradients;
  integration_worker.add_update_flags(update_flags, true, true, true, true);

  assembler.initialize(matrix);
  MeshWorker::IntegrationInfoBox<dim> info_box;
  info_box.initialize(integration_worker, fe, mapping);
  MeshWorker::DoFInfo<dim> dof_info(dof_handler);
  
  info_box.initialize(integration_worker, fe, mapping);
  MeshWorker::loop
    <MeshWorker::DoFInfo<dim>, MeshWorker::IntegrationInfoBox<dim> >
    (dof_handler.begin_active(),
     dof_handler.end(),
     dof_info,
     info_box,
     &MatrixIntegrator<dim>::cell,
     &MatrixIntegrator<dim>::bdry,
     &MatrixIntegrator<dim>::face,
     assembler);
}


template <int dim>
void
assemble(const MGDoFHandler<dim>& dof_handler,
	 MGLevelObject<SparseMatrix<double> > matrix,
	 MGLevelObject<SparseMatrix<double> > dg_up,
	 MGLevelObject<SparseMatrix<double> > dg_down)
{
  const FiniteElement<dim>& fe = dof_handler.get_fe();
  MappingQ1<dim> mapping;
  
  MeshWorker::IntegrationWorker<dim> integration_worker;
  MeshWorker::Assembler::MGMatrixSimple<SparseMatrix<double> > assembler;

  const unsigned int n_gauss_points = dof_handler.get_fe().tensor_degree()+1;
  integration_worker.initialize_gauss_quadrature(n_gauss_points, n_gauss_points, n_gauss_points);
  UpdateFlags update_flags = update_values | update_gradients;
  integration_worker.add_update_flags(update_flags, true, true, true, true);

  assembler.initialize(matrix);
  MeshWorker::IntegrationInfoBox<dim> info_box;
  info_box.initialize(integration_worker, fe, mapping);
  MeshWorker::DoFInfo<dim> dof_info(dof_handler);

  MeshWorker::loop<MeshWorker::DoFInfo<dim>, MeshWorker::IntegrationInfoBox<dim> >
    (dof_handler.begin(),
     dof_handler.end(),
     dof_info,
     info_box,
     &MatrixIntegrator<dim>::cell,
     &MatrixIntegrator<dim>::bdry,
     &MatrixIntegrator<dim>::face,
     assembler);
}


template <int dim>
void
test_simple(MGDoFHandler<dim>& mgdofs)
{
  SparsityPattern pattern;
  SparseMatrix<double> matrix;
  Vector<double> v;

  const DoFHandler<dim>& dofs = mgdofs;
  const FiniteElement<dim>& fe = dofs.get_fe();
  pattern.reinit (dofs.n_dofs(), dofs.n_dofs(),
		  (GeometryInfo<dim>::faces_per_cell
		   *GeometryInfo<dim>::max_children_per_face+1)*fe.dofs_per_cell);
  DoFTools::make_flux_sparsity_pattern (dofs, pattern);
  pattern.compress();
  matrix.reinit (pattern);
  
  assemble(dofs, matrix);
  deallog << std::setprecision(3);
  matrix.print(deallog);

  MGLevelObject<SparsityPattern> mg_sparsity;
  MGLevelObject<SparsityPattern> mg_sparsity_dg_interface;
  MGLevelObject<SparseMatrix<double> > mg_matrix;
  MGLevelObject<SparseMatrix<double> > mg_matrix_dg_up;
  MGLevelObject<SparseMatrix<double> > mg_matrix_dg_down;

  const unsigned int n_levels = mgdofs.get_tria().n_levels();
  
  mg_sparsity.resize(0, n_levels-1);
  mg_sparsity_dg_interface.resize(0, n_levels-1);
  mg_matrix.resize(0, n_levels-1);
  mg_matrix_dg_up.resize(0, n_levels-1);
  mg_matrix_dg_down.resize(0, n_levels-1);
  
  for (unsigned int level=mg_sparsity.get_minlevel();
       level<=mg_sparsity.get_maxlevel();++level)
    {
      CompressedSparsityPattern c_sparsity(mgdofs.n_dofs(level));
      CompressedSparsityPattern ci_sparsity;
      if (level>0)
	ci_sparsity.reinit(mgdofs.n_dofs(level-1), mgdofs.n_dofs(level));
      
      MGTools::make_flux_sparsity_pattern(mgdofs, c_sparsity, level);
      if (level>0)
	MGTools::make_flux_sparsity_pattern_edge(mgdofs, ci_sparsity, level);
      
      mg_sparsity[level].copy_from(c_sparsity);
      mg_matrix[level].reinit(mg_sparsity[level]);
      if (level>0)
	{
	  mg_sparsity_dg_interface[level].copy_from(ci_sparsity);
	  mg_matrix_dg_up[level].reinit(mg_sparsity_dg_interface[level]);
	  mg_matrix_dg_down[level].reinit(mg_sparsity_dg_interface[level]);
	}
    }
  
}


template<int dim>
void
test(const FiniteElement<dim>& fe)
{
  Triangulation<dim> tr;
  GridGenerator::hyper_L(tr);
  tr.begin()->set_refine_flag();
  tr.execute_coarsening_and_refinement();
//  tr.begin(2)->set_refine_flag();
//  tr.execute_coarsening_and_refinement();
//  tr.refine_global(1);
  deallog << "Triangulation levels";
  for (unsigned int l=0;l<tr.n_levels();++l)
    deallog << ' ' << l << ':' << tr.n_cells(l);
  deallog << std::endl;
  
  unsigned int cn = 0;
  for (typename Triangulation<dim>::cell_iterator cell = tr.begin();
       cell != tr.end(); ++cell, ++cn)
    cell->set_user_index(cn);

  MGDoFHandler<dim> dofs(tr);
  dofs.distribute_dofs(fe);
  deallog << "DoFHandler " << dofs.n_dofs() << " levels";
  for (unsigned int l=0;l<tr.n_levels();++l)
    deallog << ' ' << l << ':' << dofs.n_dofs(l);
  deallog << std::endl;
  
  test_simple(dofs);
}


int main ()
{
  std::ofstream logfile ("mesh_worker_02/output");
  logfile << std::setprecision (2);
  logfile << std::fixed;  
  deallog << std::setprecision (2);
  deallog << std::fixed;  
  deallog.attach(logfile);
//  deallog.depth_console (0);

  std::vector<boost::shared_ptr<FiniteElement<2> > > fe2;
  fe2.push_back(boost::shared_ptr<FiniteElement<2> >(new  FE_DGP<2>(1)));
//  fe2.push_back(boost::shared_ptr<FiniteElement<2> >(new  FE_Q<2>(1)));
  
  for (unsigned int i=0;i<fe2.size();++i)
    test(*fe2[i]);
}
