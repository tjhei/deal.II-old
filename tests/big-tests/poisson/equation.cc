/* $Id$ */

#include "poisson.h"






void PoissonEquation<1>::assemble (dFMatrix            &cell_matrix,
				   dVector             &rhs,
				   const FEValues<1>   &fe_values,
				   const Triangulation<1>::cell_iterator &) const {
  for (unsigned int point=0; point<fe_values.n_quadrature_points; ++point)
    for (unsigned int i=0; i<fe_values.total_dofs; ++i) 
      {
	for (unsigned int j=0; j<fe_values.total_dofs; ++j)
	  cell_matrix(i,j) += (fe_values.shape_grad(i,point) *
			       fe_values.shape_grad(j,point)) *
			      fe_values.JxW(point);
	rhs(i) += fe_values.shape_value(i,point) *
		  right_hand_side(fe_values.quadrature_point(point)) *
		  fe_values.JxW(point);
      };
};



void PoissonEquation<2>::assemble (dFMatrix            &cell_matrix,
				   dVector             &rhs,
				   const FEValues<2>   &fe_values,
				   const Triangulation<2>::cell_iterator &) const {
  for (unsigned int point=0; point<fe_values.n_quadrature_points; ++point)
    for (unsigned int i=0; i<fe_values.total_dofs; ++i) 
      {
	for (unsigned int j=0; j<fe_values.total_dofs; ++j)
	  cell_matrix(i,j) += (fe_values.shape_grad(i,point) *
			       fe_values.shape_grad(j,point)) *
			      fe_values.JxW(point);
	rhs(i) += fe_values.shape_value(i,point) *
		  right_hand_side(fe_values.quadrature_point(point)) *
		  fe_values.JxW(point);
      };
};



template <int dim>
void PoissonEquation<dim>::assemble (dFMatrix            &,
				     const FEValues<dim> &,
				     const Triangulation<dim>::cell_iterator &) const {
  Assert (false, ExcPureVirtualFunctionCalled());
};



template <int dim>
void PoissonEquation<dim>::assemble (dVector             &,
				     const FEValues<dim> &,
				     const Triangulation<dim>::cell_iterator &) const {
  Assert (false, ExcPureVirtualFunctionCalled());
};






template class PoissonEquation<1>;
template class PoissonEquation<2>;
