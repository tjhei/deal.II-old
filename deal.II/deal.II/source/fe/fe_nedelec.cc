//----------------------------------------------------------------
//    $Id$
//    Version: $Name$
//
//    Copyright (C) 2002 by the deal.II authors
//
//    This file is subject to QPL and may not be  distributed
//    without copyright and license information. Please refer
//    to the file deal.II/doc/license.html for the  text  and
//    further information on this license.
//
//----------------------------------------------------------------

#include <base/quadrature.h>
#include <base/polynomial.h>
#include <base/tensor_product_polynomials.h>
#include <grid/tria.h>
#include <grid/tria_iterator.h>
#include <dofs/dof_accessor.h>
#include <fe/fe.h>
#include <fe/mapping.h>
#include <fe/fe_nedelec.h>
#include <fe/fe_values.h>


template <int dim>
FE_Nedelec<dim>::FE_Nedelec (const unsigned int degree)
		:
		FiniteElement<dim> (FiniteElementData<dim>(get_dpo_vector(degree),
							   dim),
//TODO: I'd think this element is actually additive in the restriction
				    std::vector<bool> (dim,false),
				    std::vector<std::vector<bool> >(FiniteElementData<dim>(get_dpo_vector(degree),dim).dofs_per_cell,
								    std::vector<bool>(dim,true))),
		degree(degree)
{
  Assert (dim >= 2, ExcNotUsefulInThisDimension());
  
				   // copy constraint matrices if they
				   // are defined. otherwise set them
				   // to invalid size
  if (degree<Matrices::n_constraint_matrices+1)
    interface_constraints.fill (Matrices::constraint_matrices[degree-1]);
  else
    interface_constraints.reinit(0,0);

				   // next copy over embedding
				   // matrices if they are defined
  if ((degree < Matrices::n_embedding_matrices+1) &&
      (Matrices::embedding[degree-1][0] != 0))
    for (unsigned int c=0; c<GeometryInfo<dim>::children_per_cell; ++c)
      prolongation[c].fill (Matrices::embedding[degree-1][c]);
  else
    for (unsigned int i=0; i<GeometryInfo<dim>::children_per_cell;++i)
      prolongation[i].reinit(0,0);

				   // then fill restriction
				   // matrices. they are hardcoded for
				   // the first few elements
  switch (dim)
    {
      case 2:   // 2d
      {
	switch (degree)
	  {
	    case 1:
	    {
					       // DoF on bottom line
					       // of coarse cell will
					       // be mean value of
					       // bottom DoFs on the
					       // two adjacent child
					       // cells
	      restriction[0](0,0) = 0.5;
	      restriction[1](0,0) = 0.5;
					       // same for other DoFs
	      restriction[1](1,1) = 0.5;
	      restriction[2](1,1) = 0.5;

	      restriction[2](2,2) = 0.5;
	      restriction[3](2,2) = 0.5;

	      restriction[3](3,3) = 0.5;
	      restriction[0](3,3) = 0.5;

	      break;
	    };
	    
	    default:
	    {
					       // in case we don't
					       // have the matrices
					       // (yet), set them to
					       // impossible
					       // values. this does
					       // not prevent the use
					       // of this FE, but will
					       // prevent the use of
					       // these matrices
	      for (unsigned int i=0;
		   i<GeometryInfo<dim>::children_per_cell;
		   ++i)
		restriction[i].reinit(0,0);
	    };
	  };
	
	break;
      };


      case 3:   // 3d
      {
	switch (degree)
	  {
	    case 1:
	    {
					       // same principle as in
					       // 2d
	      restriction[0](0,0) = 0.5;
	      restriction[1](0,0) = 0.5;

	      restriction[1](1,1) = 0.5;
	      restriction[2](1,1) = 0.5;

	      restriction[2](2,2) = 0.5;
	      restriction[3](2,2) = 0.5;

	      restriction[3](3,3) = 0.5;
	      restriction[0](3,3) = 0.5;

	      restriction[4](4,4) = 0.5;
	      restriction[5](4,4) = 0.5;

	      restriction[5](5,5) = 0.5;
	      restriction[6](5,5) = 0.5;

	      restriction[6](6,6) = 0.5;
	      restriction[7](6,6) = 0.5;

	      restriction[7](7,7) = 0.5;
	      restriction[4](7,7) = 0.5;


	      restriction[1](8,8) = 0.5;
	      restriction[5](8,8) = 0.5;

	      restriction[2](9,9) = 0.5;
	      restriction[6](9,9) = 0.5;

	      restriction[3](10,10) = 0.5;
	      restriction[7](10,10) = 0.5;

	      restriction[0](11,11) = 0.5;
	      restriction[5](11,11) = 0.5;
	      
	      break;
	    };
	    
	    default:
	    {
					       // in case we don't
					       // have the matrices
					       // (yet), set them to
					       // impossible
					       // values. this does
					       // not prevent the use
					       // of this FE, but will
					       // prevent the use of
					       // these matrices
	      for (unsigned int i=0;
		   i<GeometryInfo<dim>::children_per_cell;
		   ++i)
		restriction[i].reinit(0,0);
	    };
	  };
	
	break;
      };
      
      default:
	    Assert (false,ExcNotImplemented());
    }

				   // finally fill in support points
				   // on cell and face
  initialize_unit_support_points ();
  initialize_unit_face_support_points ();
};



template <int dim>
FiniteElement<dim> *
FE_Nedelec<dim>::clone() const
{
  return new FE_Nedelec<dim>(degree);
}



template <int dim>
double
FE_Nedelec<dim>::shape_value_component (const unsigned int i,
					const Point<dim> &p,
					const unsigned int component) const
{
  Assert (i<dofs_per_cell, ExcIndexRange(i,0,dofs_per_cell));
  Assert (component < dim, ExcIndexRange (component, 0, dim));
  
  switch (dim)
    {
      case 2:    // 2D
      {
	switch (degree)
	  {
					     // first order Nedelec
					     // elements
	    case 1:
	    {
	      switch (i)
		{
							 // (1-y, 0)
		  case 0: return (component == 0 ? 1-p(1) : 0);
							 // (0,x)
		  case 1: return (component == 0 ? 0 : p(0));
							 // (y, 0)
		  case 2: return (component == 0 ? p(1) : 0);
							 // (0, 1-x)
		  case 3: return (component == 0 ? 0 : 1-p(0));

							 // there are
							 // only four
							 // shape
							 // functions!?
		  default:
			Assert (false, ExcInternalError());
			return 0;
		};
	    };

					     // no other degrees
					     // implemented
	    default:
		  Assert (false, ExcNotImplemented());
	  };
      };

      case 3:    // 3D
      {
	switch (degree)
	  {
					     // first order Nedelec
					     // elements
	    case 1:
	    {
					       // note that the
					       // degrees of freedom
					       // on opposite faces
					       // have a common vector
					       // direction, so simply
					       // that a little. these
					       // directions are:
					       //
					       // for lines 0, 2, 4, 6:
					       //    (1,0,0)
					       // for lines 1, 3, 5, 7:
					       //    (0,0,1)
					       // for lines 8, 9, 10, 11:
					       //    (0,1,0)
					       //
					       // thus, sort out all
					       // those cases where
					       // the component is
					       // zero anyway, and
					       // only otherwise
					       // compute the
					       // spatially dependent
					       // part which is then
					       // also the return
					       // value
	      if (((i<8) && (((i%2==0) && (component!=0)) ||
			     ((i%2==1) && (component!=2)))) ||
		  ((i>=8) && (component != 1)))
		return 0;

					       // now we know that the
					       // only non-zero
					       // component is
					       // requested:
//TODO[Anna]: check	      
	      const double x = p(0),
			   y = p(1),
			   z = p(2);
	      switch (i)
		{
		  case  0: return (1-y)*(1-z);
		  case  2: return (1-y)*z;
		  case  1: return x*(1-y);
		  case  3: return (1-x)*(1-y);

		  case  4: return y*(1-z);
		  case  6: return y*z;
		  case  5: return x*y;
		  case  7: return (1-x)*y;
			
		  case  8: return (1-x)*(1-z);
		  case  9: return x*(1-z);
		  case 10: return x*z;
		  case 11: return (1-x)*z;
		  default:
			Assert (false, ExcInternalError());
			return 0;
		};
	    };

					     // no other degrees
					     // implemented
	    default:
		  Assert (false, ExcNotImplemented());
	  };
      };
      
				       // presently no other space
				       // dimension implemented
      default:
	    Assert (false, ExcNotImplemented());
    };
  
  return 0;
}



template <int dim>
Tensor<1,dim>
FE_Nedelec<dim>::shape_grad_component (const unsigned int i,
				       const Point<dim> &/*p*/,
				       const unsigned int component) const
{
  Assert (i<dofs_per_cell, ExcIndexRange(i,0,dofs_per_cell));
  Assert (component < dim, ExcIndexRange (component, 0, dim));

  switch (dim)
    {
      case 2:    // 2D
      {
	switch (degree)
	  {
					     // first order Nedelec
					     // elements
	    case 1:
	    {
					       // on the unit cell,
					       // the gradients of
					       // these shape
					       // functions are
					       // constant, so we pack
					       // them into a table
					       // for simpler lookup
					       //
					       // the format is: first
					       // index=shape function
					       // number; second
					       // index=vector
					       // component, thrid
					       // index=component
					       // within gradient
//TODO[Anna]: check	      
	      static const double unit_gradients[4][2][2]
		= { { {0.,-1.}, {0.,0.} },
		    { {0.,0.},  {1.,0.} },
		    { {0.,+1.}, {0.,0.} },
		    { {0.,0.},  {-1.,0.} } };
	      return Tensor<1,dim>(unit_gradients[i][component]);
	    };

					     // no other degrees
					     // implemented
	    default:
		  Assert (false, ExcNotImplemented());
	  };
      };

				       // presently no other space
				       // dimension implemented
      default:
	    Assert (false, ExcNotImplemented());
    };
  
  return Tensor<1,dim>();
}



template <int dim>
Tensor<2,dim>
FE_Nedelec<dim>::shape_grad_grad_component (const unsigned int i,
					    const Point<dim> &/*p*/,
					    const unsigned int component) const
{
  Assert (i<dofs_per_cell, ExcIndexRange(i,0,dofs_per_cell));
  Assert (component < dim, ExcIndexRange (component, 0, dim));

  switch (dim)
    {
      case 2:    // 2D
      {
	switch (degree)
	  {
					     // first order Nedelec
					     // elements. their second
					     // derivatives on the
					     // unit cell are zero
	    case 1:
	    {
	      return Tensor<2,dim>();
	    };

					     // no other degrees
					     // implemented
	    default:
		  Assert (false, ExcNotImplemented());
	  };
      };

      case 3:    // 3D
      {
	switch (degree)
	  {
					     // first order Nedelec
					     // elements. their second
					     // derivatives on the
					     // unit cell are zero
	    case 1:
	    {
	      return Tensor<2,dim>();
	    };

					     // no other degrees
					     // implemented
	    default:
		  Assert (false, ExcNotImplemented());
	  };
      };
	    
      
				       // presently no other space
				       // dimension implemented
      default:
	    Assert (false, ExcNotImplemented());
    };

  return Tensor<2,dim>();
}


//----------------------------------------------------------------------
// Auxiliary functions
//----------------------------------------------------------------------



template <int dim>
void FE_Nedelec<dim>::initialize_unit_support_points ()
{
//TODO: fix for higher orders. correct now for lowest order, all dimensions  
// is this correct? all DoFs on lines, none on faces or bubbles?

				   // all degrees of freedom are on
				   // edges, and their order is the
				   // same as the edges themselves
  unit_support_points.resize(GeometryInfo<dim>::lines_per_cell * degree);
  unsigned int index = 0;
  for (unsigned int line=0; line<GeometryInfo<dim>::lines_per_cell; ++line)
    {
      const unsigned int
	vertex_index_0 = GeometryInfo<dim>::vertices_adjacent_to_line(line,0),
	vertex_index_1 = GeometryInfo<dim>::vertices_adjacent_to_line(line,1);
      
      const Point<dim>
	vertex_0 = GeometryInfo<dim>::unit_cell_vertex(vertex_index_0),
	vertex_1 = GeometryInfo<dim>::unit_cell_vertex(vertex_index_1);

				       // place dofs equispaced
				       // between the vertices of each
				       // line
      for (unsigned int d=0; d<degree; ++d, ++index)
	unit_support_points[index]
	  = (vertex_0*(d+1) + vertex_1*(degree-d)) / (degree+1);
    };
};


#if deal_II_dimension == 1

template <>
void FE_Nedelec<1>::initialize_unit_face_support_points ()
{
				   // no faces in 1d, so nothing to do
};

#endif


template <int dim>
void FE_Nedelec<dim>::initialize_unit_face_support_points ()
{
//TODO: fix for higher orders. correct now for lowest order, all dimensions  
// is this correct? all DoFs on lines, none on faces or bubbles?
				   // do this the same as above, but
				   // for one dimension less
  unit_face_support_points.resize(GeometryInfo<dim-1>::lines_per_cell * degree);
  unsigned int index = 0;
  for (unsigned int line=0; line<GeometryInfo<dim-1>::lines_per_cell; ++line)
    {
      const unsigned int
	vertex_index_0 = GeometryInfo<dim-1>::vertices_adjacent_to_line(line,0),
	vertex_index_1 = GeometryInfo<dim-1>::vertices_adjacent_to_line(line,1);
      
      const Point<dim-1>
	vertex_0 = GeometryInfo<dim-1>::unit_cell_vertex(vertex_index_0),
	vertex_1 = GeometryInfo<dim-1>::unit_cell_vertex(vertex_index_1);

				       // place dofs equispaced
				       // between the vertices of each
				       // line
      for (unsigned int d=0; d<degree; ++d, ++index)
	unit_face_support_points[index]
	  = (vertex_0*(d+1) + vertex_1*(degree-d)) / (degree+1);
    };
};



template <int dim>
std::vector<unsigned int>
FE_Nedelec<dim>::get_dpo_vector(const unsigned int degree)
{
//TODO: fix for higher orders. correct now for lowest order, all dimensions  
  std::vector<unsigned int> dpo(dim+1, 0);
// can this be done in a dimension independent and degree independent way?  
// if DoFs are located only on lines, the the following is the correct way

				   // put all degrees of freedom on
				   // the lines, and in particular
				   // @p{degree} DoFs per line:
  dpo[1] = degree;
  return dpo;
}



template <int dim>
UpdateFlags
FE_Nedelec<dim>::update_once (const UpdateFlags flags) const
{
//TODO: think about what this actually means here???  
				   // for this kind of elements, only
				   // the values can be precomputed
				   // once and for all. set this flag
				   // if the values are requested at
				   // all
  return (update_default | (flags & update_values));
}



template <int dim>
UpdateFlags
FE_Nedelec<dim>::update_each (const UpdateFlags flags) const
{
//TODO: think about what this actually means here???  

  UpdateFlags out = update_default;

  if (flags & update_gradients)
    out |= update_gradients | update_covariant_transformation;
  if (flags & update_second_derivatives)
    out |= update_second_derivatives | update_covariant_transformation;

  return out;
}



//----------------------------------------------------------------------
// Data field initialization
//----------------------------------------------------------------------

template <int dim>
typename Mapping<dim>::InternalDataBase *
FE_Nedelec<dim>::get_data (const UpdateFlags      /*update_flags*/,
			   const Mapping<dim>    &/*mapping*/,
			   const Quadrature<dim> &/*quadrature*/) const
{
  return 0;
//TODO
//  				   // generate a new data object and
//  				   // initialize some fields
//    InternalData* data = new InternalData;

//  				   // check what needs to be
//  				   // initialized only once and what
//  				   // on every cell/face/subface we
//  				   // visit
//    data->update_once = update_once(update_flags);
//    data->update_each = update_each(update_flags);
//    data->update_flags = data->update_once | data->update_each;

//    const UpdateFlags flags(data->update_flags);
//    const unsigned int n_q_points = quadrature.n_quadrature_points;

//  				   // some scratch arrays
//    std::vector<double> values(0);
//    std::vector<Tensor<1,dim> > grads(0);
//    std::vector<Tensor<2,dim> > grad_grads(0);

//  				   // initialize fields only if really
//  				   // necessary. otherwise, don't
//  				   // allocate memory
//    if (flags & update_values)
//      {
//        values.resize (dofs_per_cell);
//        data->shape_values.resize(dofs_per_cell,
//  				std::vector<double>(n_q_points));
//      }

//    if (flags & update_gradients)
//      {
//        grads.resize (dofs_per_cell);
//        data->shape_gradients.resize(dofs_per_cell,
//  				   std::vector<Tensor<1,dim> >(n_q_points));
//      }

//  				   // if second derivatives through
//  				   // finite differencing is required,
//  				   // then initialize some objects for
//  				   // that
//    if (flags & update_second_derivatives)
//      data->initialize_2nd (this, mapping, quadrature);

//  				   // next already fill those fields
//  				   // of which we have information by
//  				   // now. note that the shape
//  				   // gradients are only those on the
//  				   // unit cell, and need to be
//  				   // transformed when visiting an
//  				   // actual cell
//    if (flags & (update_values | update_gradients))
//      for (unsigned int i=0; i<n_q_points; ++i)
//        {
//  	polynomial_space.compute(quadrature.point(i),
//  				 values, grads, grad_grads);
	
//  	if (flags & update_values)
//  	  for (unsigned int k=0; k<dofs_per_cell; ++k)
//  	    data->shape_values[renumber[k]][i] = values[k];
	
//  	if (flags & update_gradients)
//  	  for (unsigned int k=0; k<dofs_per_cell; ++k)
//  	    data->shape_gradients[renumber[k]][i] = grads[k];
//        }
//    return data;
}




//----------------------------------------------------------------------
// Fill data of FEValues
//----------------------------------------------------------------------

template <int dim>
void
FE_Nedelec<dim>::fill_fe_values (const Mapping<dim>                   &/*mapping*/,
				 const typename DoFHandler<dim>::cell_iterator &/*cell*/,
				 const Quadrature<dim>                &/*quadrature*/,
				 typename Mapping<dim>::InternalDataBase &/*mapping_data*/,
				 typename Mapping<dim>::InternalDataBase &/*fedata*/,
				 FEValuesData<dim>                    &/*data*/) const
{
//TODO!!  
//  				   // convert data object to internal
//  				   // data for this class. fails with
//  				   // an exception if that is not
//  				   // possible
//    InternalData &fe_data = dynamic_cast<InternalData &> (fedata);
  
//    const UpdateFlags flags(fe_data.current_update_flags());

//    for (unsigned int k=0; k<dofs_per_cell; ++k)
//      {
//        if (flags & update_values)
//  	for (unsigned int i=0; i<quadrature.n_quadrature_points; ++i)
//  	  data.shape_values(k,i) = fe_data.shape_values[k][i];
      
//        if (flags & update_gradients)
//  	mapping.transform_covariant(data.shape_gradients[k],
//  				    fe_data.shape_gradients[k],
//  				    mapping_data, 0);
//      }

//    if (flags & update_second_derivatives)
//      compute_2nd (mapping, cell, 0, mapping_data, fe_data, data);
  
//    fe_data.first_cell = false;
}



template <int dim>
void
FE_Nedelec<dim>::fill_fe_face_values (const Mapping<dim>                   &/*mapping*/,
				      const typename DoFHandler<dim>::cell_iterator &/*cell*/,
				      const unsigned int                    /*face*/,
				      const Quadrature<dim-1>              &/*quadrature*/,
				      typename Mapping<dim>::InternalDataBase       &/*mapping_data*/,
				      typename Mapping<dim>::InternalDataBase       &/*fedata*/,
				      FEValuesData<dim>                    &/*data*/) const
{
//TODO!!  
//  				   // convert data object to internal
//  				   // data for this class. fails with
//  				   // an exception if that is not
//  				   // possible
//    InternalData &fe_data = dynamic_cast<InternalData &> (fedata);

//  				   // offset determines which data set
//  				   // to take (all data sets for all
//  				   // faces are stored contiguously)
//    const unsigned int offset = face * quadrature.n_quadrature_points;
  
//    const UpdateFlags flags(fe_data.update_once | fe_data.update_each);

//    for (unsigned int k=0; k<dofs_per_cell; ++k)
//      {
//        for (unsigned int i=0;i<quadrature.n_quadrature_points;++i)
//  	if (flags & update_values)
//  	  data.shape_values(k,i) = fe_data.shape_values[k][i+offset];
      
//        if (flags & update_gradients)
//  	mapping.transform_covariant(data.shape_gradients[k],
//  				    fe_data.shape_gradients[k],
//  				    mapping_data, offset);
//      }

//    if (flags & update_second_derivatives)
//      compute_2nd (mapping, cell, offset, mapping_data, fe_data, data);
  
//    fe_data.first_cell = false;
}



template <int dim>
void
FE_Nedelec<dim>::fill_fe_subface_values (const Mapping<dim>                   &/*mapping*/,
					 const typename DoFHandler<dim>::cell_iterator &/*cell*/,
					 const unsigned int                    /*face*/,
					 const unsigned int                    /*subface*/,
					 const Quadrature<dim-1>              &/*quadrature*/,
					 typename Mapping<dim>::InternalDataBase       &/*mapping_data*/,
					 typename Mapping<dim>::InternalDataBase       &/*fedata*/,
					 FEValuesData<dim>                    &/*data*/) const
{
//TODO!!  
//  				   // convert data object to internal
//  				   // data for this class. fails with
//  				   // an exception if that is not
//  				   // possible
//    InternalData &fe_data = dynamic_cast<InternalData &> (fedata);

//  				   // offset determines which data set
//  				   // to take (all data sets for all
//  				   // sub-faces are stored contiguously)
//    const unsigned int offset = (face * GeometryInfo<dim>::subfaces_per_face + subface)
//  			      * quadrature.n_quadrature_points;

//    const UpdateFlags flags(fe_data.update_once | fe_data.update_each);

//    for (unsigned int k=0; k<dofs_per_cell; ++k)
//      {
//        for (unsigned int i=0;i<quadrature.n_quadrature_points;++i)
//  	if (flags & update_values)
//  	  data.shape_values(k,i) = fe_data.shape_values[k][i+offset];
      
//        if (flags & update_gradients)
//  	mapping.transform_covariant(data.shape_gradients[k],
//  				    fe_data.shape_gradients[k],
//  				    mapping_data, offset);
//      }
  
//    if (flags & update_second_derivatives)
//      compute_2nd (mapping, cell, offset, mapping_data, fe_data, data);
  
//    fe_data.first_cell = false;
}



template <int dim>
unsigned int
FE_Nedelec<dim>::n_base_elements () const
{
  return 1;
};



template <int dim>
const FiniteElement<dim> &
FE_Nedelec<dim>::base_element (const unsigned int index) const
{
  Assert (index==0, ExcIndexRange(index, 0, 1));
  return *this;
};



template <int dim>
bool
FE_Nedelec<dim>::has_support_on_face (const unsigned int shape_index,
				      const unsigned int face_index) const
{
  Assert (shape_index < dofs_per_cell,
	  ExcIndexRange (shape_index, 0, dofs_per_cell));
  Assert (face_index < GeometryInfo<dim>::faces_per_cell,
	  ExcIndexRange (face_index, 0, GeometryInfo<dim>::faces_per_cell));
	  
//TODO: fix for higher orders. correct now for lowest order, all dimensions  
//TODO!!
// can this be done in a way that is dimension and degree independent?
  
				   // all degrees of freedom are on
				   // lines, so also on a face. the
				   // question is whether it has
				   // support on this particular face
  Assert (false, ExcNotImplemented());
  return true;
}



template <int dim>
unsigned int
FE_Nedelec<dim>::memory_consumption () const
{
  Assert (false, ExcNotImplemented ());
  return 0;
}



template <int dim>
unsigned int
FE_Nedelec<dim>::get_degree () const
{
  return degree;
};



template class FE_Nedelec<deal_II_dimension>;
