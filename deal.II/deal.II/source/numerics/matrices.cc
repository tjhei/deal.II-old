//----------------------------  matrices.cc  ---------------------------
//    $Id$
//    Version: $Name$
//
//    Copyright (C) 1998, 1999, 2000, 2001 by the deal.II authors
//
//    This file is subject to QPL and may not be  distributed
//    without copyright and license information. Please refer
//    to the file deal.II/doc/license.html for the  text  and
//    further information on this license.
//
//----------------------------  matrices.cc  ---------------------------


#include <base/function.h>
#include <dofs/dof_handler.h>
#include <dofs/dof_accessor.h>
#include <grid/tria_iterator.h>
#include <grid/geometry_info.h>
#include <base/quadrature.h>
#include <fe/fe.h>
#include <fe/fe_values.h>
#include <numerics/matrices.h>
#include <lac/vector.h>
#include <lac/block_vector.h>
#include <lac/sparse_matrix.h>
#include <lac/block_sparse_matrix.h>
#include <fe/mapping_q1.h>

#include <algorithm>
#include <set>
#include <cmath>


// if necessary try to work around a bug in the IBM xlC compiler
#ifdef XLC_WORK_AROUND_STD_BUG
using namespace std;
#endif




//TODO:[RH,GK] Replace global by local object; better: have two functions, or by default arg
static const MappingQ1<deal_II_dimension> mapping_q1;



//TODO:[RH,GK] maybe re-create the create_mass_matrix function with 2 args

template <int dim>
void MatrixCreator<dim>::create_mass_matrix (const DoFHandler<dim>    &dof,
					     const Quadrature<dim>    &q,
					     SparseMatrix<double>     &matrix,
					     const Function<dim> * const coefficient)
{
  UpdateFlags update_flags = UpdateFlags(update_values | update_JxW_values);
  if (coefficient != 0)
    update_flags = UpdateFlags (update_flags | update_q_points);

  FEValues<dim> fe_values (dof.get_fe(), q, update_flags);
    
  const unsigned int dofs_per_cell = fe_values.dofs_per_cell,
		     n_q_points    = fe_values.n_quadrature_points;
  const FiniteElement<dim>    &fe  = fe_values.get_fe();
  const unsigned int n_components  = fe.n_components();

  FullMatrix<double>  cell_matrix (dofs_per_cell, dofs_per_cell);
  std::vector<double> coefficient_values (n_q_points);
  
  std::vector<unsigned int> dof_indices (dofs_per_cell);
  
  typename DoFHandler<dim>::active_cell_iterator cell = dof.begin_active();
  for (; cell!=dof.end(); ++cell)
    {
      fe_values.reinit (cell);
      
      cell_matrix.clear ();
      cell->get_dof_indices (dof_indices);
      
      const FullMatrix<double>  &values    = fe_values.get_shape_values ();
      const std::vector<double> &weights   = fe_values.get_JxW_values ();
      
      if (coefficient != 0)
	{
	  coefficient->value_list (fe_values.get_quadrature_points(),
				   coefficient_values);
	  for (unsigned int point=0; point<n_q_points; ++point)
	    for (unsigned int i=0; i<dofs_per_cell; ++i) 
	      for (unsigned int j=0; j<dofs_per_cell; ++j)
		if ((n_components==1) ||
		    (fe.system_to_component_index(i).first ==
		     fe.system_to_component_index(j).first))
		  cell_matrix(i,j) += (values(i,point) *
				       values(j,point) *
				       weights[point] *
				       coefficient_values[point]);
	}
      else
	for (unsigned int point=0; point<n_q_points; ++point)
	  for (unsigned int i=0; i<dofs_per_cell; ++i) 
	    for (unsigned int j=0; j<dofs_per_cell; ++j)
	      if ((n_components==1) ||
		  (fe.system_to_component_index(i).first ==
		   fe.system_to_component_index(j).first))
		cell_matrix(i,j) += (values(i,point) *
				     values(j,point) *
				     weights[point]);

				       // transfer everything into the
				       // global object
      for (unsigned int i=0; i<dofs_per_cell; ++i)
	for (unsigned int j=0; j<dofs_per_cell; ++j)
	  matrix.add (dof_indices[i], dof_indices[j],
		      cell_matrix(i,j));
    };
};



template <int dim>
void MatrixCreator<dim>::create_mass_matrix (const DoFHandler<dim>    &dof,
					     const Quadrature<dim>    &q,
					     SparseMatrix<double>     &matrix,
					     const Function<dim>      &rhs,
					     Vector<double>           &rhs_vector,
					     const Function<dim> * const coefficient)
{
  UpdateFlags update_flags = UpdateFlags(update_values |
					 update_q_points |
					 update_JxW_values);
  if (coefficient != 0)
    update_flags = UpdateFlags (update_flags | update_q_points);

  FEValues<dim> fe_values (dof.get_fe(), q, update_flags);
    
  const unsigned int dofs_per_cell = fe_values.dofs_per_cell,
		     n_q_points    = fe_values.n_quadrature_points;
  const FiniteElement<dim>    &fe  = fe_values.get_fe();
  const unsigned int n_components  = fe.n_components();

  FullMatrix<double>  cell_matrix (dofs_per_cell, dofs_per_cell);
  Vector<double>      local_rhs (dofs_per_cell);
  std::vector<double> rhs_values (fe_values.n_quadrature_points);
  std::vector<double> coefficient_values (n_q_points);
  
  std::vector<unsigned int> dof_indices (dofs_per_cell);
  
  typename DoFHandler<dim>::active_cell_iterator cell = dof.begin_active();
  for (; cell!=dof.end(); ++cell)
    {
      fe_values.reinit (cell);
      
      cell_matrix.clear ();
      local_rhs.clear ();
      cell->get_dof_indices (dof_indices);
      
      const FullMatrix<double>  &values    = fe_values.get_shape_values ();
      const std::vector<double> &weights   = fe_values.get_JxW_values ();
      rhs.value_list (fe_values.get_quadrature_points(), rhs_values);
      
      if (coefficient != 0)
	{
	  coefficient->value_list (fe_values.get_quadrature_points(),
				   coefficient_values);
	  for (unsigned int point=0; point<n_q_points; ++point)
	    for (unsigned int i=0; i<dofs_per_cell; ++i) 
	      {
		for (unsigned int j=0; j<dofs_per_cell; ++j)
		  if ((n_components==1) ||
		      (fe.system_to_component_index(i).first ==
		       fe.system_to_component_index(j).first))
		    cell_matrix(i,j) += (values(i,point) *
					 values(j,point) *
					 weights[point] *
					 coefficient_values[point]);
		local_rhs(i) += values(i,point) *
				rhs_values[point] *
				weights[point];
	      };
	}
      else
	for (unsigned int point=0; point<n_q_points; ++point)
	  for (unsigned int i=0; i<dofs_per_cell; ++i) 
	    {
	      for (unsigned int j=0; j<dofs_per_cell; ++j)
		if ((n_components==1) ||
		    (fe.system_to_component_index(i).first ==
		     fe.system_to_component_index(j).first))
		  cell_matrix(i,j) += (values(i,point) *
				       values(j,point) *
				       weights[point]);
	      local_rhs(i) += values(i,point) *
			      rhs_values[point] *
			      weights[point];
	    };

				       // transfer everything into the
				       // global object
      for (unsigned int i=0; i<dofs_per_cell; ++i)
	for (unsigned int j=0; j<dofs_per_cell; ++j)
	  matrix.add (dof_indices[i], dof_indices[j],
		      cell_matrix(i,j));
      for (unsigned int i=0; i<dofs_per_cell; ++i)
	rhs_vector(dof_indices[i]) += local_rhs(i);
    };
};



#if deal_II_dimension == 1

template <>
void MatrixCreator<1>::create_boundary_mass_matrix (const DoFHandler<1>       &,
						    const Quadrature<0>       &,
						    SparseMatrix<double>      &,
						    const FunctionMap         &,
						    Vector<double>            &,
						    std::vector<unsigned int> &,
						    const Function<1>         *)
{
  Assert (false, ExcNotImplemented());
};

#endif


template <int dim>
void MatrixCreator<dim>::create_boundary_mass_matrix (const DoFHandler<dim>     &dof,
						      const Quadrature<dim-1>   &q,
						      SparseMatrix<double>      &matrix,
						      const FunctionMap         &rhs,
						      Vector<double>            &rhs_vector,
						      std::vector<unsigned int> &dof_to_boundary_mapping,
						      const Function<dim>       *a)
{
  const FiniteElement<dim> &fe = dof.get_fe();
  const unsigned int n_components  = fe.n_components();
  const bool         fe_is_system  = (n_components != 1);
  
  Assert (matrix.n() == dof.n_boundary_dofs(rhs), ExcInternalError());
  Assert (matrix.n() == matrix.m(), ExcInternalError());
  Assert (matrix.n() == rhs_vector.size(), ExcInternalError());
  Assert (rhs.size() != 0, ExcInternalError());
  Assert (dof.get_fe() == fe, ExcInternalError());
  Assert (dof_to_boundary_mapping.size() == dof.n_dofs(),
	  ExcInternalError());
  Assert (n_components == rhs.begin()->second->n_components,
	  ExcComponentMismatch());
#ifdef DEBUG
  if (true)
    {
      unsigned int max_element = 0;
      for (std::vector<unsigned int>::const_iterator i=dof_to_boundary_mapping.begin();
	   i!=dof_to_boundary_mapping.end(); ++i)
	if ((*i != DoFHandler<dim>::invalid_dof_index) &&
	    (*i > max_element))
	  max_element = *i;
      Assert (max_element  == matrix.n()-1, ExcInternalError());
    };
#endif
  
  const unsigned int dofs_per_cell = fe.dofs_per_cell,
		     dofs_per_face = fe.dofs_per_face;
  
  FullMatrix<double> cell_matrix(dofs_per_cell, dofs_per_cell);
  Vector<double>     cell_vector(dofs_per_cell);


  UpdateFlags update_flags = UpdateFlags (update_values     |
					  update_JxW_values |
					  update_q_points);
  FEFaceValues<dim> fe_values (mapping_q1, fe, q, update_flags);

				   // two variables for the coefficient,
				   // one for the two cases indicated in
				   // the name
  std::vector<double>          coefficient_values_scalar (fe_values.n_quadrature_points);
  std::vector<Vector<double> > coefficient_values_system (fe_values.n_quadrature_points,
						     Vector<double>(n_components));

  std::vector<double>          rhs_values_scalar (fe_values.n_quadrature_points);
  std::vector<Vector<double> > rhs_values_system (fe_values.n_quadrature_points,
					     Vector<double>(n_components));

  std::vector<unsigned int> dofs (dofs_per_cell);
  std::vector<unsigned int> dofs_on_face_vector (dofs_per_face);
  std::set<int> dofs_on_face;

  DoFHandler<dim>::active_cell_iterator cell = dof.begin_active (),
					endc = dof.end ();
  for (; cell!=endc; ++cell)
    for (unsigned int face=0; face<GeometryInfo<dim>::faces_per_cell; ++face)
				       // check if this face is on that part of
				       // the boundary we are interested in
      if (rhs.find(cell->face(face)->boundary_indicator()) != rhs.end())
	{
	  cell_matrix.clear ();
	  cell_vector.clear ();
	  
	  fe_values.reinit (cell, face);

	  const FullMatrix<double> &values    = fe_values.get_shape_values ();
	  const std::vector<double>     &weights   = fe_values.get_JxW_values ();

	  if (fe_is_system)
					     // FE has several components
	    {
	      rhs.find(cell->face(face)->boundary_indicator())
		->second->vector_value_list (fe_values.get_quadrature_points(),
					     rhs_values_system);

	      if (a != 0)
		{
		  a->vector_value_list (fe_values.get_quadrature_points(),
					coefficient_values_system);
		  for (unsigned int point=0; point<fe_values.n_quadrature_points; ++point)
		    for (unsigned int i=0; i<fe_values.dofs_per_cell; ++i) 
		      {
			for (unsigned int j=0; j<fe_values.dofs_per_cell; ++j)
			  if (fe.system_to_component_index(i).first ==
			      fe.system_to_component_index(j).first)
			    {
			      cell_matrix(i,j)
				+= (values(i,point) *
				    values(j,point) *
				    weights[point] *
				    coefficient_values_system[point](
				      fe.system_to_component_index(i).first));
			    };
			
			cell_vector(i) += values(i,point) *
					  rhs_values_system[point](
					    fe.system_to_component_index(i).first) *
					  weights[point];
		      };
		}
	      else
		for (unsigned int point=0; point<fe_values.n_quadrature_points; ++point)
		  for (unsigned int i=0; i<fe_values.dofs_per_cell; ++i) 
		    {
		      for (unsigned int j=0; j<fe_values.dofs_per_cell; ++j)
			if (fe.system_to_component_index(i).first ==
			    fe.system_to_component_index(j).first)
			  {
			    cell_matrix(i,j) += (values(i,point) *
						 values(j,point) *
						 weights[point]);
			  };
		      
		      cell_vector(i) += values(i,point) *
					rhs_values_system[point](
					  fe.system_to_component_index(i).first) *
					weights[point];
		    };
	    }
	  else
					     // FE is a scalar one
	    {
	      rhs.find(cell->face(face)->boundary_indicator())
		->second->value_list (fe_values.get_quadrature_points(), rhs_values_scalar);

	      if (a != 0)
		{
		  a->value_list (fe_values.get_quadrature_points(),
				 coefficient_values_scalar);
		  for (unsigned int point=0; point<fe_values.n_quadrature_points; ++point)
		    for (unsigned int i=0; i<fe_values.dofs_per_cell; ++i) 
		      {
			for (unsigned int j=0; j<fe_values.dofs_per_cell; ++j)
			  cell_matrix(i,j) += (values(i,point) *
					       values(j,point) *
					       weights[point] *
					       coefficient_values_scalar[point]);
			cell_vector(i) += values(i,point) *
					  rhs_values_scalar[point] *
					  weights[point];
		      };
		}
	      else
		for (unsigned int point=0; point<fe_values.n_quadrature_points; ++point)
		  for (unsigned int i=0; i<fe_values.dofs_per_cell; ++i) 
		    {
		      for (unsigned int j=0; j<fe_values.dofs_per_cell; ++j)
			cell_matrix(i,j) += (values(i,point) *
					     values(j,point) *
					     weights[point]);
		      cell_vector(i) += values(i,point) *
					rhs_values_scalar[point] *
					weights[point];
		    };
	    };


					   // now transfer cell matrix and vector
					   // to the whole boundary matrix
					   //
					   // in the following: dof[i] holds the
					   // global index of the i-th degree of
					   // freedom on the present cell. If it
					   // is also a dof on the boundary, it
					   // must be a nonzero entry in the
					   // dof_to_boundary_mapping and then
					   // the boundary index of this dof is
					   // dof_to_boundary_mapping[dof[i]].
					   //
					   // if dof[i] is not on the boundary,
					   // it should be zero on the boundary
					   // therefore on all quadrature
					   // points and finally all of its
					   // entries in the cell matrix and
					   // vector should be zero. If not, we
					   // throw an error (note: because of
					   // the evaluation of the shape
					   // functions only up to machine
					   // precision, the term "must be zero"
					   // really should mean: "should be
					   // very small". since this is only an
					   // assertion and not part of the
					   // code, we may choose "very small"
					   // quite arbitrarily)
					   //
					   // the main problem here is that the
					   // matrix or vector entry should also
					   // be zero if the degree of freedom
					   // dof[i] is on the boundary, but not
					   // on the present face, i.e. on
					   // another face of the same cell also
					   // on the boundary. We can therefore
					   // not rely on the
					   // dof_to_boundary_mapping[dof[i]]
					   // being !=-1, we really have to
					   // determine whether dof[i] is a
					   // dof on the present face. We do so
					   // by getting the dofs on the
					   // face into @p{dofs_on_face_vector},
					   // a vector as always. Usually,
					   // searching in a vector is
					   // inefficient, so we copy the dofs
					   // into a set, which enables binary
					   // searches.
	  cell->get_dof_indices (dofs);
	  cell->face(face)->get_dof_indices (dofs_on_face_vector);

  	  dofs_on_face.clear ();
	  dofs_on_face.insert (dofs_on_face_vector.begin(),
			       dofs_on_face_vector.end());
	  
#ifdef DEBUG
					   // in debug mode: compute an element
					   // in the matrix which is
					   // guaranteed to belong to a boundary
					   // dof. We do this to check that the
					   // entries in the cell matrix are
					   // guaranteed to be zero if the
					   // respective dof is not on the
					   // boundary. Since because of
					   // round-off, the actual
					   // value of the matrix entry may be
					   // only close to zero, we assert that
					   // it is small relative to an element
					   // which is guaranteed to be nonzero.
					   // (absolute smallness does not
					   // suffice since the size of the
					   // domain scales in here)
					   //
					   // for this purpose we seek the
					   // diagonal of the matrix, where there
					   // must be an element belonging to
					   // the boundary. we take the maximum
					   // diagonal entry.
	  double max_diag_entry = 0;
	  for (unsigned int i=0; i<dofs_per_cell; ++i)
	    if (fabs(cell_matrix(i,i)) > max_diag_entry)
	      max_diag_entry = fabs(cell_matrix(i,i));
#endif  
	  
	  for (unsigned int i=0; i<dofs_per_cell; ++i)
	    for (unsigned int j=0; j<dofs_per_cell; ++j)
	      if ((dofs_on_face.find(dofs[i]) != dofs_on_face.end()) &&
		  (dofs_on_face.find(dofs[j]) != dofs_on_face.end()))
		matrix.add(dof_to_boundary_mapping[dofs[i]],
			   dof_to_boundary_mapping[dofs[j]],
			   cell_matrix(i,j));
	      else
		{
						   // compare here for relative
						   // smallness
		  Assert (fabs(cell_matrix(i,j)) <= 1e-10 * max_diag_entry,
			  ExcInternalError ());
		};
	  
	  for (unsigned int j=0; j<dofs_per_cell; ++j)
	    if (dofs_on_face.find(dofs[j]) != dofs_on_face.end())
	      rhs_vector(dof_to_boundary_mapping[dofs[j]]) += cell_vector(j);
	    else
	      {
						   // compare here for relative
						   // smallness
		Assert (fabs(cell_vector(j)) <= 1e-10 * max_diag_entry,
			ExcInternalError());
	      };
	};
};



template <int dim>
void MatrixCreator<dim>::create_laplace_matrix (const DoFHandler<dim>    &dof,
						const Quadrature<dim>    &q,
						SparseMatrix<double>     &matrix,
						const Function<dim> * const coefficient)
{
  UpdateFlags update_flags = UpdateFlags(update_JxW_values |
					 update_gradients);
  if (coefficient != 0)
    update_flags = UpdateFlags (update_flags | update_q_points);

  FEValues<dim> fe_values (dof.get_fe(), q, update_flags);
    
  const unsigned int dofs_per_cell = fe_values.dofs_per_cell,
		     n_q_points    = fe_values.n_quadrature_points;
  const FiniteElement<dim>    &fe  = fe_values.get_fe();
  const unsigned int n_components  = fe.n_components();

  FullMatrix<double>  cell_matrix (dofs_per_cell, dofs_per_cell);
  std::vector<double> coefficient_values (n_q_points);
  
  std::vector<unsigned int> dof_indices (dofs_per_cell);
  
  typename DoFHandler<dim>::active_cell_iterator cell = dof.begin_active();
  for (; cell!=dof.end(); ++cell)
    {
      fe_values.reinit (cell);
      
      cell_matrix.clear ();
      cell->get_dof_indices (dof_indices);
      
      const std::vector<std::vector<Tensor<1,dim> > >
	&grads   = fe_values.get_shape_grads ();
      const std::vector<double> &weights = fe_values.get_JxW_values ();
      
      if (coefficient != 0)
	{
	  coefficient->value_list (fe_values.get_quadrature_points(),
				   coefficient_values);
	  for (unsigned int point=0; point<n_q_points; ++point)
	    for (unsigned int i=0; i<dofs_per_cell; ++i) 
	      for (unsigned int j=0; j<dofs_per_cell; ++j)
		if ((n_components==1) ||
		    (fe.system_to_component_index(i).first ==
		     fe.system_to_component_index(j).first))
		  cell_matrix(i,j) += (grads[i][point] *
				       grads[j][point] *
				       weights[point] *
				       coefficient_values[point]);
	}
      else
	for (unsigned int point=0; point<n_q_points; ++point)
	  for (unsigned int i=0; i<dofs_per_cell; ++i) 
	    for (unsigned int j=0; j<dofs_per_cell; ++j)
	      if ((n_components==1) ||
		  (fe.system_to_component_index(i).first ==
		   fe.system_to_component_index(j).first))
		cell_matrix(i,j) += (grads[i][point] *
				     grads[j][point] *
				     weights[point]);

				       // transfer everything into the
				       // global object
      for (unsigned int i=0; i<dofs_per_cell; ++i)
	for (unsigned int j=0; j<dofs_per_cell; ++j)
	  matrix.add (dof_indices[i], dof_indices[j],
		      cell_matrix(i,j));
    };
};




//TODO:[GK,RH] maybe recreate this function
/*

template <int dim>
void MatrixCreator<dim>::create_level_laplace_matrix (unsigned int level,
						      const MGDoFHandler<dim>    &dof,
						      const Quadrature<dim>    &q,
						      SparseMatrix<float>     &matrix,
						      const Function<dim> * const a)
{
  Vector<double> dummy;   // no entries, should give an error if accessed
  UpdateFlags update_flags = UpdateFlags(update_gradients |
					 update_JxW_values);
  if (a != 0)
    update_flags = UpdateFlags(update_flags | update_q_points);
  const Assembler<dim>::AssemblerData data (dof,
					    true, false,  // assemble matrix but not rhs
					    matrix, dummy,
					    q, update_flags);
  TriaIterator<dim, Assembler<dim> >
    assembler (const_cast<Triangulation<dim>*>(&dof.get_tria()),
	       dof.get_tria().begin(level)->level(),
	       dof.get_tria().begin(level)->index(),
	       &data);
  LaplaceMatrix<dim> equation (0, a);
  do 
    {
      assembler->assemble (equation);
      ++assembler
    }
  while ( (assembler.state()==valid) && (assembler->level() == level) );
};

*/





template <int dim>
void MatrixCreator<dim>::create_laplace_matrix (const DoFHandler<dim>    &dof,
						const Quadrature<dim>    &q,
						SparseMatrix<double>     &matrix,
						const Function<dim>      &rhs,
						Vector<double>           &rhs_vector,
						const Function<dim> * const coefficient)
{
  UpdateFlags update_flags = UpdateFlags(update_values    |
					 update_gradients |
					 update_q_points  |
					 update_JxW_values);
  if (coefficient != 0)
    update_flags = UpdateFlags (update_flags | update_q_points);

  FEValues<dim> fe_values (dof.get_fe(), q, update_flags);
    
  const unsigned int dofs_per_cell = fe_values.dofs_per_cell,
		     n_q_points    = fe_values.n_quadrature_points;
  const FiniteElement<dim>    &fe  = fe_values.get_fe();
  const unsigned int n_components  = fe.n_components();

  FullMatrix<double>  cell_matrix (dofs_per_cell, dofs_per_cell);
  Vector<double>      local_rhs (dofs_per_cell);
  std::vector<double> rhs_values (fe_values.n_quadrature_points);
  std::vector<double> coefficient_values (n_q_points);
  
  std::vector<unsigned int> dof_indices (dofs_per_cell);
  
  typename DoFHandler<dim>::active_cell_iterator cell = dof.begin_active();
  for (; cell!=dof.end(); ++cell)
    {
      fe_values.reinit (cell);
      
      cell_matrix.clear ();
      local_rhs.clear ();
      cell->get_dof_indices (dof_indices);
      
      const FullMatrix<double>  &values    = fe_values.get_shape_values ();
      const std::vector<std::vector<Tensor<1,dim> > >
	&grads   = fe_values.get_shape_grads ();
      const std::vector<double> &weights   = fe_values.get_JxW_values ();
      rhs.value_list (fe_values.get_quadrature_points(), rhs_values);
      
      if (coefficient != 0)
	{
	  coefficient->value_list (fe_values.get_quadrature_points(),
				   coefficient_values);
	  for (unsigned int point=0; point<n_q_points; ++point)
	    for (unsigned int i=0; i<dofs_per_cell; ++i) 
	      {
		for (unsigned int j=0; j<dofs_per_cell; ++j)
		  if ((n_components==1) ||
		      (fe.system_to_component_index(i).first ==
		       fe.system_to_component_index(j).first))
		    cell_matrix(i,j) += (grads[i][point] *
					 grads[j][point] *
					 weights[point] *
					 coefficient_values[point]);
		local_rhs(i) += values(i,point) *
				rhs_values[point] *
				weights[point];
	      };
	}
      else
	for (unsigned int point=0; point<n_q_points; ++point)
	  for (unsigned int i=0; i<dofs_per_cell; ++i) 
	    {
	      for (unsigned int j=0; j<dofs_per_cell; ++j)
		if ((n_components==1) ||
		    (fe.system_to_component_index(i).first ==
		     fe.system_to_component_index(j).first))
		  cell_matrix(i,j) += (grads[i][point] *
				       grads[j][point] *
				       weights[point]);
	      local_rhs(i) += values(i,point) *
			      rhs_values[point] *
			      weights[point];
	    };

				       // transfer everything into the
				       // global object
      for (unsigned int i=0; i<dofs_per_cell; ++i)
	for (unsigned int j=0; j<dofs_per_cell; ++j)
	  matrix.add (dof_indices[i], dof_indices[j],
		      cell_matrix(i,j));
      for (unsigned int i=0; i<dofs_per_cell; ++i)
	rhs_vector(dof_indices[i]) += local_rhs(i);
    };
};




template <int dim>
template <typename number>
void
MatrixTools<dim>::apply_boundary_values (const std::map<unsigned int,double> &boundary_values,
					 SparseMatrix<number>  &matrix,
					 Vector<number>   &solution,
					 Vector<number>   &right_hand_side,
					 const bool        preserve_symmetry)
{
  Assert (matrix.n() == matrix.m(),
	  ExcDimensionsDontMatch(matrix.n(), matrix.m()));
  Assert (matrix.n() == right_hand_side.size(),
	  ExcDimensionsDontMatch(matrix.n(), right_hand_side.size()));
  Assert (matrix.n() == solution.size(),
	  ExcDimensionsDontMatch(matrix.n(), solution.size()));
				   // if no boundary values are to be applied
				   // simply return
  if (boundary_values.size() == 0)
    return;


  std::map<unsigned int,double>::const_iterator dof  = boundary_values.begin(),
						endd = boundary_values.end();
  const unsigned int n_dofs             = matrix.m();
  const SparsityPattern    &sparsity    = matrix.get_sparsity_pattern();
  const unsigned int *sparsity_rowstart = sparsity.get_rowstart_indices();
  const unsigned int *sparsity_colnums  = sparsity.get_column_numbers();

				   // if a diagonal entry is zero
				   // later, then we use another
				   // number instead. take it to be
				   // the first nonzero diagonal
				   // element of the matrix, or 1 if
				   // there is no such thing
  number first_nonzero_diagonal_entry = 1;
  for (unsigned int i=0; i<n_dofs; ++i)
    if (matrix.diag_element(i) != 0)
      {
	first_nonzero_diagonal_entry = matrix.diag_element(i);
	break;
      };

  
  for (; dof != endd; ++dof)
    {
      Assert (dof->first < n_dofs, ExcInternalError());
      
      const unsigned int dof_number = dof->first;
				       // for each boundary dof:
      
				       // set entries of this line
				       // to zero except for the diagonal
				       // entry. Note that the diagonal
				       // entry is always the first one
				       // for square matrices, i.e.
				       // we shall not set
				       // matrix.global_entry(
				       //     sparsity_rowstart[dof.first])
      const unsigned int last = sparsity_rowstart[dof_number+1];
      for (unsigned int j=sparsity_rowstart[dof_number]+1; j<last; ++j)
	matrix.global_entry(j) = 0.;


				       // set right hand side to
				       // wanted value: if main diagonal
				       // entry nonzero, don't touch it
				       // and scale rhs accordingly. If
				       // zero, take the first main
				       // diagonal entry we can find, or
				       // one if no nonzero main diagonal
				       // element exists. Normally, however,
				       // the main diagonal entry should
				       // not be zero.
				       //
				       // store the new rhs entry to make
				       // the gauss step more efficient
      number new_rhs;
      if (matrix.diag_element(dof_number) != 0.0)
	new_rhs = right_hand_side(dof_number)
		= dof->second * matrix.diag_element(dof_number);
      else
	{
					   // use the SparseMatrix::
					   // to work around a bug in
					   // egcs
	  matrix.SparseMatrix<number>::set (dof_number, dof_number,
					    first_nonzero_diagonal_entry);
	  new_rhs = right_hand_side(dof_number)
		  = dof->second * first_nonzero_diagonal_entry;
	};


				       // if the user wants to have
				       // the symmetry of the matrix
				       // preserved, and if the
				       // sparsity pattern is
				       // symmetric, then do a Gauss
				       // elimination step with the
				       // present row
      if (preserve_symmetry)
	{
					   // store the only nonzero entry
					   // of this line for the Gauss
					   // elimination step
	  const number diagonal_entry = matrix.diag_element(dof_number);
	  
					   // we have to loop over all
					   // rows of the matrix which
					   // have a nonzero entry in
					   // the column which we work
					   // in presently. if the
					   // sparsity pattern is
					   // symmetric, then we can
					   // get the positions of
					   // these rows cheaply by
					   // looking at the nonzero
					   // column numbers of the
					   // present row. we need not
					   // look at the first entry,
					   // since that is the
					   // diagonal element and
					   // thus the present row
	  for (unsigned int j=sparsity_rowstart[dof_number]+1; j<last; ++j)
	    {
	      const unsigned int row = sparsity_colnums[j];

					       // find the position of
					       // element
					       // (row,dof_number)
	      const unsigned int *
		p = std::lower_bound(&sparsity_colnums[sparsity_rowstart[row]+1],
				     &sparsity_colnums[sparsity_rowstart[row+1]],
				     dof_number);

					       // check whether this line has
					       // an entry in the regarding column
					       // (check for ==dof_number and
					       // != next_row, since if
					       // row==dof_number-1, *p is a
					       // past-the-end pointer but points
					       // to dof_number anyway...)
					       //
					       // there should be such an entry!
	      Assert ((*p == dof_number) &&
		      (p != &sparsity_colnums[sparsity_rowstart[row+1]]),
		      ExcInternalError());

	      const unsigned int global_entry
		= (p - &sparsity_colnums[sparsity_rowstart[0]]);
	      
					       // correct right hand side
	      right_hand_side(row) -= matrix.global_entry(global_entry) /
				      diagonal_entry * new_rhs;
	      
					       // set matrix entry to zero
	      matrix.global_entry(global_entry) = 0.;
	    };
	};

				       // preset solution vector
      solution(dof_number) = dof->second;
    };
};





template <int dim>
void
MatrixTools<dim>::apply_boundary_values (const std::map<unsigned int,double> &boundary_values,
					 BlockSparseMatrix<double>  &matrix,
					 BlockVector<double>   &solution,
					 BlockVector<double>   &right_hand_side,
					 const bool             preserve_symmetry)
{
  const unsigned int blocks = matrix.n_block_rows();
  
  Assert (matrix.n() == matrix.m(),
	  ExcDimensionsDontMatch(matrix.n(), matrix.m()));
  Assert (matrix.n() == right_hand_side.size(),
	  ExcDimensionsDontMatch(matrix.n(), right_hand_side.size()));
  Assert (matrix.n() == solution.size(),
	  ExcDimensionsDontMatch(matrix.n(), solution.size()));
  Assert (matrix.n_block_rows() == matrix.n_block_cols(),
	  ExcMatrixNotBlockSquare());
  Assert (matrix.get_sparsity_pattern().get_row_indices() == 
	  matrix.get_sparsity_pattern().get_column_indices(),
	  ExcMatrixNotBlockSquare());
  Assert (matrix.get_sparsity_pattern().get_column_indices() ==
	  solution.get_block_indices (),
	  ExcBlocksDontMatch ());
  Assert (matrix.get_sparsity_pattern().get_row_indices() ==
	  right_hand_side.get_block_indices (),
	  ExcBlocksDontMatch ());
  
  
				   // if no boundary values are to be applied
				   // simply return
  if (boundary_values.size() == 0)
    return;


  std::map<unsigned int,double>::const_iterator dof  = boundary_values.begin(),
						endd = boundary_values.end();
  const unsigned int n_dofs = matrix.m();
  const BlockSparsityPattern &
    sparsity_pattern = matrix.get_sparsity_pattern();

				   // if a diagonal entry is zero
				   // later, then we use another
				   // number instead. take it to be
				   // the first nonzero diagonal
				   // element of the matrix, or 1 if
				   // there is no such thing
  double first_nonzero_diagonal_entry = 0;
  for (unsigned int diag_block=0; diag_block<blocks; ++diag_block)
    {
      for (unsigned int i=0; i<matrix.block(diag_block,diag_block).n(); ++i)
	if (matrix.block(diag_block,diag_block).diag_element(i) != 0)
	  {
	    first_nonzero_diagonal_entry 
	      = matrix.block(diag_block,diag_block).diag_element(i);
	    break;
	  };
				       // check whether we have found
				       // something in the present
				       // block
      if (first_nonzero_diagonal_entry != 0)
	break;
    };
				   // nothing found on all diagonal
				   // blocks? if so, use 1.0 instead
  if (first_nonzero_diagonal_entry == 0)
    first_nonzero_diagonal_entry = 1;
  
  
				   // pointer to the mapping between
				   // global and block indices. since
				   // the row and column mappings are
				   // equal, store a pointer on only
				   // one of them
  const BlockIndices &
    index_mapping = sparsity_pattern.get_column_indices();
  
				   // now loop over all boundary dofs
  for (; dof != endd; ++dof)
    {
      Assert (dof->first < n_dofs, ExcInternalError());

				       // get global index and index
				       // in the block in which this
				       // dof is located
      const unsigned int dof_number = dof->first;
      const std::pair<unsigned int,unsigned int>
	block_index = index_mapping.global_to_local (dof_number);

				       // for each boundary dof:
      
				       // set entries of this line
				       // to zero except for the diagonal
				       // entry. Note that the diagonal
				       // entry is always the first one
				       // for square matrices, i.e.
				       // we shall not set
				       // matrix.global_entry(
				       //     sparsity_rowstart[dof.first])
				       // of the diagonal block
      for (unsigned int block_col=0; block_col<blocks; ++block_col)
	{
	  const SparsityPattern &
	    local_sparsity = sparsity_pattern.block(block_index.first,
						    block_col);

					   // find first and last
					   // entry in the present row
					   // of the present
					   // block. exclude the main
					   // diagonal element, which
					   // is the diagonal element
					   // of a diagonal block,
					   // which must be a square
					   // matrix so the diagonal
					   // element is the first of
					   // this row.
	  const unsigned int 
	    last  = local_sparsity.get_rowstart_indices()[block_index.second+1],
	    first = (block_col == block_index.first ?
		     local_sparsity.get_rowstart_indices()[block_index.second]+1 :
		     local_sparsity.get_rowstart_indices()[block_index.second]);
	  
	  for (unsigned int j=first; j<last; ++j)
	    matrix.block(block_index.first,block_col).global_entry(j) = 0.;
	};
      

				       // set right hand side to
				       // wanted value: if main diagonal
				       // entry nonzero, don't touch it
				       // and scale rhs accordingly. If
				       // zero, take the first main
				       // diagonal entry we can find, or
				       // one if no nonzero main diagonal
				       // element exists. Normally, however,
				       // the main diagonal entry should
				       // not be zero.
				       //
				       // store the new rhs entry to make
				       // the gauss step more efficient
      double new_rhs;
      if (matrix.block(block_index.first, block_index.first)
	  .diag_element(block_index.second) != 0.0)
	new_rhs = dof->second * 
		  matrix.block(block_index.first, block_index.first)
		  .diag_element(block_index.second);
      else
	{
	  matrix.block(block_index.first, block_index.first)
	    .diag_element(block_index.second)
	    = first_nonzero_diagonal_entry;
	  new_rhs = dof->second * first_nonzero_diagonal_entry;
	};
      right_hand_side.block(block_index.first)(block_index.second)
	= new_rhs;


				       // if the user wants to have
				       // the symmetry of the matrix
				       // preserved, and if the
				       // sparsity pattern is
				       // symmetric, then do a Gauss
				       // elimination step with the
				       // present row. this is a
				       // little more complicated for
				       // block matrices.
      if (preserve_symmetry)
	{
					   // store the only nonzero entry
					   // of this line for the Gauss
					   // elimination step
	  const double diagonal_entry 
	    = matrix.block(block_index.first,block_index.first)
	    .diag_element(block_index.second);
	  
					   // we have to loop over all
					   // rows of the matrix which
					   // have a nonzero entry in
					   // the column which we work
					   // in presently. if the
					   // sparsity pattern is
					   // symmetric, then we can
					   // get the positions of
					   // these rows cheaply by
					   // looking at the nonzero
					   // column numbers of the
					   // present row.
					   //
					   // note that if we check
					   // whether row @p{row} in
					   // block (r,c) is non-zero,
					   // then we have to check
					   // for the existence of
					   // column @p{row} in block
					   // (c,r), i.e. of the
					   // transpose block
	  for (unsigned int block_row=0; block_row<blocks; ++block_row)
	    {
					       // get pointers to the
					       // sparsity patterns of
					       // this block and of
					       // the transpose one
	      const SparsityPattern &this_sparsity
		= sparsity_pattern.block (block_row, block_index.first);
	      const SparsityPattern &transpose_sparsity
		= sparsity_pattern.block (block_index.first, block_row);
	      
					       // traverse the row of
					       // the transpose block
					       // to find the
					       // interesting rows in
					       // the present block.
					       // don't use the
					       // diagonal element of
					       // the diagonal block
	      const unsigned int
		first = (block_index.first == block_row ?
			 transpose_sparsity.get_rowstart_indices()[block_index.second]+1 :
			 transpose_sparsity.get_rowstart_indices()[block_index.second]),
		last  = transpose_sparsity.get_rowstart_indices()[block_index.second+1];
	      
	      for (unsigned int j=first; j<last; ++j)
		{
						   // get the number
						   // of the column in
						   // this row in
						   // which a nonzero
						   // entry is. this
						   // is also the row
						   // of the transpose
						   // block which has
						   // an entry in the
						   // interesting row
		  const unsigned int row = transpose_sparsity.get_column_numbers()[j];

						   // find the
						   // position of
						   // element
						   // (row,dof_number)
						   // in this block
						   // (not in the
						   // transpose
						   // one). note that
						   // we have to take
						   // care of special
						   // cases with
						   // square
						   // sub-matrices
		  const unsigned int *p = 0;
		  if (this_sparsity.n_rows() == this_sparsity.n_cols())
		    {
		      if (this_sparsity.get_column_numbers()
			  [this_sparsity.get_rowstart_indices()[row]]
			  ==
			  block_index.second)
			p = &this_sparsity.get_column_numbers()
			    [this_sparsity.get_rowstart_indices()[row]];
		      else
			p = std::lower_bound(&this_sparsity.get_column_numbers()
					     [this_sparsity.get_rowstart_indices()[row]+1],
					     &this_sparsity.get_column_numbers()
					     [this_sparsity.get_rowstart_indices()[row+1]],
					     block_index.second);
		    }
		  else
		    p = std::lower_bound(&this_sparsity.get_column_numbers()
					 [this_sparsity.get_rowstart_indices()[row]],
					 &this_sparsity.get_column_numbers()
					 [this_sparsity.get_rowstart_indices()[row+1]],
					 block_index.second);

						   // check whether this line has
						   // an entry in the regarding column
						   // (check for ==dof_number and
						   // != next_row, since if
						   // row==dof_number-1, *p is a
						   // past-the-end pointer but points
						   // to dof_number anyway...)
						   //
						   // there should be
						   // such an entry!
						   // note, however,
						   // that this
						   // assertion will
						   // fail sometimes
						   // if the sparsity
						   // pattern is not
						   // symmetric!
		  Assert ((*p == block_index.second) &&
			  (p != &this_sparsity.get_column_numbers()
			   [this_sparsity.get_rowstart_indices()[row+1]]),
			  ExcInternalError());
		  
		  const unsigned int global_entry
		    = (p
		       -
		       &this_sparsity.get_column_numbers()
		       [this_sparsity.get_rowstart_indices()[0]]);

						   // correct right hand side
		  right_hand_side.block(block_row)(row)
		    -= matrix.block(block_row,block_index.first).global_entry(global_entry) /
		    diagonal_entry * new_rhs;
		  
						   // set matrix entry to zero
		  matrix.block(block_row,block_index.first).global_entry(global_entry) = 0.;
		};
	    };
	};

				       // preset solution vector
      solution.block(block_index.first)(block_index.second) = dof->second;
    };
};





// explicit instantiations

template class MatrixCreator<deal_II_dimension>;
template class MatrixTools<deal_II_dimension>;


template
void
MatrixTools<deal_II_dimension>::
apply_boundary_values (const std::map<unsigned int,double> &boundary_values,
		       SparseMatrix<double>  &matrix,
		       Vector<double>   &solution,
		       Vector<double>   &right_hand_side,
		       const bool        preserve_symmetry);

template
void
MatrixTools<deal_II_dimension>::
apply_boundary_values (const std::map<unsigned int,double> &boundary_values,
		       SparseMatrix<float>  &matrix,
		       Vector<float>   &solution,
		       Vector<float>   &right_hand_side,
		       const bool       preserve_symmetry);

template
void
MatrixTools<deal_II_dimension>::
apply_boundary_values (const std::map<unsigned int,double> &,
		       BlockSparseMatrix<double>      &,
		       BlockVector<double>            &,
		       BlockVector<double>            &,
		       const bool);



