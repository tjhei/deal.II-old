//---------------------------------------------------------------------------
//    $Id$
//    Version: $Name$
//
//    Copyright (C) 2009, 2010 by the deal.II authors
//
//    This file is subject to QPL and may not be  distributed
//    without copyright and license information. Please refer
//    to the file deal.II/doc/license.html for the  text  and
//    further information on this license.
//
//---------------------------------------------------------------------------


#include <numerics/mesh_worker_info.h>
#include <base/quadrature_lib.h>

DEAL_II_NAMESPACE_OPEN


namespace MeshWorker
{
  template <int dim, int spacedim, typename number>
  DoFInfo<dim,spacedim,number>::DoFInfo(const BlockInfo& info)
		  : block_info(&info, typeid(*this).name())
  {}


  template <int dim, int spacedim, typename number>
  DoFInfo<dim,spacedim,number>::DoFInfo()
  {}


  template <int dim, int spacedim, typename number>
  void
  DoFInfo<dim,spacedim,number>::get_indices(const typename DoFHandler<dim, spacedim>::cell_iterator& c)
  {
    if (!c->has_children())
      {
	indices.resize(c->get_fe().dofs_per_cell);

	if (block_info == 0 || block_info->local().size() == 0)
	  c->get_dof_indices(indices);
	else
	  {
	    indices_org.resize(c->get_fe().dofs_per_cell);
	    c->get_dof_indices(indices_org);
	    for (unsigned int i=0;i<indices.size();++i)
	      indices[this->block_info->renumber(i)] = indices_org[i];
	  }
      }
    else
      indices.resize(0);

    level_cell = false;
  }


  template <int dim, int spacedim, typename number>
  void
  DoFInfo<dim,spacedim,number>::get_indices(const typename MGDoFHandler<dim, spacedim>::cell_iterator& c)
  {
    indices.resize(c->get_fe().dofs_per_cell);

    if (block_info == 0 || block_info->local().size() == 0)
      c->get_mg_dof_indices(indices);
    else
      {
	indices_org.resize(c->get_fe().dofs_per_cell);
	c->get_mg_dof_indices(indices_org);
	for (unsigned int i=0;i<indices.size();++i)
	  indices[this->block_info->renumber(i)] = indices_org[i];
      }
    level_cell = true;
  }

//----------------------------------------------------------------------//

  template<int dim, int sdim>
  void
  IntegrationInfo<dim,sdim>::initialize_data(
    const boost::shared_ptr<VectorDataBase<dim,sdim> > &data)
  {
    global_data = data;
    const unsigned int nqp = fevalv[0]->n_quadrature_points;

    values.resize(global_data->n_values());
//    deallog << "values: " << values.size() << " [";
				     // For all selected finite
				     // element functions
    for (unsigned int i=0;i<values.size();++i)
      {
	values[i].resize(n_components);
//	deallog << ' ' << values[i].size() << " {";
					 // For all components
	for (unsigned int j=0;j<values[i].size();++j)
	  {
	    values[i][j].resize(nqp);
//	    deallog << ' ' << values[i][j].size();
	  }
//	deallog << " }";
      }
//    deallog << " ]" << std::endl;

    gradients.resize(global_data->n_gradients());
				     // For all selected finite
				     // element functions
    for (unsigned int i=0;i<gradients.size();++i)
      {
	gradients[i].resize(n_components);
					 // For all components
	for (unsigned int j=0;j<gradients[i].size();++j)
	  {
	    gradients[i][j].resize(nqp);
	  }
      }

    hessians.resize(global_data->n_hessians());
				     // For all selected finite
				     // element functions
    for (unsigned int i=0;i<hessians.size();++i)
      {
	hessians[i].resize(n_components);
					 // For all components
	for (unsigned int j=0;j<hessians[i].size();++j)
	  {
	    hessians[i][j].resize(nqp);
	  }
      }
  }


  template<int dim, int sdim>
  void
  IntegrationInfo<dim,sdim>::clear()
  {
    fevalv.resize(0);
  }



  template<int dim, int sdim>
  template <typename number>
  void
  IntegrationInfo<dim,sdim>::fill_local_data(const DoFInfo<dim, sdim, number>& info, bool split_fevalues)
  {
     if (split_fevalues)
       {
	unsigned int comp = 0;
					 // Loop over all blocks
	for (unsigned int b=0;b<info.block_info->local().size();++b)
	  {
	    const unsigned int fe_no = info.block_info->base_element(b);
	    const FEValuesBase<dim,sdim>& fe = this->fe_values(fe_no);
	    const unsigned int n_comp = fe.get_fe().n_components();
	    const unsigned int block_start = info.block_info->local().block_start(b);
	    const unsigned int block_size = info.block_info->local().block_size(b);

            if(info.level_cell)
	    this->global_data->mg_fill(values, gradients, hessians, fe, info.cell->level(), info.indices,
				    comp, n_comp, block_start, block_size);
            else
	    this->global_data->fill(values, gradients, hessians, fe, info.indices,
				    comp, n_comp, block_start, block_size);
 	    comp += n_comp;
	  }
       }
     else
       {
	 const FEValuesBase<dim,sdim>& fe = this->fe_values(0);
	 const unsigned int n_comp = fe.get_fe().n_components();
         if(info.level_cell)
	 this->global_data->mg_fill(values, gradients, hessians, fe, info.cell->level(), info.indices,
				 0, n_comp, 0, info.indices.size());
         else
	 this->global_data->fill(values, gradients, hessians, fe, info.indices,
				 0, n_comp, 0, info.indices.size());
       }
  }


//----------------------------------------------------------------------//


  template<int dim, int sdim>
  void
  IntegrationInfoBox<dim,sdim>::initialize_update_flags ()
  {
    cell_flags = update_JxW_values;
    boundary_flags = UpdateFlags(update_JxW_values | update_normal_vectors);
    face_flags = boundary_flags;
    neighbor_flags = update_default;

    if (cell_selector.has_values() != 0) cell_flags |= update_values;
    if (cell_selector.has_gradients() != 0) cell_flags |= update_gradients;
    if (cell_selector.has_hessians() != 0) cell_flags |= update_hessians;

    if (boundary_selector.has_values() != 0) boundary_flags |= update_values;
    if (boundary_selector.has_gradients() != 0) boundary_flags |= update_gradients;
    if (boundary_selector.has_hessians() != 0) boundary_flags |= update_hessians;

    if (face_selector.has_values() != 0) face_flags |= update_values;
    if (face_selector.has_gradients() != 0) face_flags |= update_gradients;
    if (face_selector.has_hessians() != 0) face_flags |= update_hessians;

    if (face_selector.has_values() != 0) neighbor_flags |= update_values;
    if (face_selector.has_gradients() != 0) neighbor_flags |= update_gradients;
    if (face_selector.has_hessians() != 0) neighbor_flags |= update_hessians;
  }


  template <int dim, int sdim>
  void
  IntegrationInfoBox<dim,sdim>::add_update_flags(
    const UpdateFlags flags,
    bool cell,
    bool boundary,
    bool face,
    bool neighbor)
  {
    if (cell) cell_flags |= flags;
    if (boundary) boundary_flags |= flags;
    if (face) face_flags |= flags;
    if (neighbor) neighbor_flags |= flags;
  }
}


DEAL_II_NAMESPACE_CLOSE

