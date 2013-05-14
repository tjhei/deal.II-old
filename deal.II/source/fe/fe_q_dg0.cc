//---------------------------------------------------------------------------
//    $Id: fe_q.cc 28798 2013-03-07 17:00:02Z maier $
//    Version: $Name$
//
//    Copyright (C) 2012, 2013 by the deal.II authors
//
//    This file is subject to QPL and may not be  distributed
//    without copyright and license information. Please refer
//    to the file deal.II/doc/license.html for the  text  and
//    further information on this license.
//
//---------------------------------------------------------------------------

#include <deal.II/base/quadrature.h>
#include <deal.II/base/qprojector.h>
#include <deal.II/base/template_constraints.h>
#include <deal.II/fe/fe_q_dg0.h>
#include <deal.II/fe/fe_nothing.h>
#include <deal.II/base/quadrature_lib.h>
#include <deal.II/dofs/dof_accessor.h>


#include <vector>
#include <sstream>

DEAL_II_NAMESPACE_OPEN


template <int dim, int spacedim>
FE_Q_DG0<dim,spacedim>::FE_Q_DG0 (const unsigned int degree)
  :
  FE_Q_Base<TensorProductPolynomialsConst<dim>, dim, spacedim> (
    TensorProductPolynomialsConst<dim>(Polynomials::LagrangeEquidistant::generate_complete_basis(degree)),
    FiniteElementData<dim>(get_dpo_vector(degree),
                           1, degree,
                           FiniteElementData<dim>::L2),
    get_riaf_vector(degree))
{
  Assert (degree > 0,
          ExcMessage ("This element can only be used for polynomial degrees "
                      "greater than zero"));

  std::vector<Point<1> > support_points_1d(degree+1);
  for (unsigned int i=0; i<=degree; ++i)
    support_points_1d[i][0] = static_cast<double>(i)/degree;

  this->initialize(support_points_1d);

  // adjust unit support point for discontinuous node
  Point<dim> point;
  for (unsigned int d=0; d<dim; ++d)
    point[d] = 0.5;
  this->unit_support_points.push_back(point);
  AssertDimension(this->dofs_per_cell, this->unit_support_points.size());
}



template <int dim, int spacedim>
FE_Q_DG0<dim,spacedim>::FE_Q_DG0 (const Quadrature<1> &points)
  :
  FE_Q_Base<TensorProductPolynomialsConst<dim>, dim, spacedim> (
    TensorProductPolynomialsConst<dim>(Polynomials::generate_complete_Lagrange_basis(points.get_points())),
    FiniteElementData<dim>(get_dpo_vector(points.size()-1),
                           1, points.size()-1,
                           FiniteElementData<dim>::L2),
    get_riaf_vector(points.size()-1))
{
  const int degree = points.size()-1;

  Assert (degree > 0,
          ExcMessage ("This element can only be used for polynomial degrees "
                      "at least zero"));

  this->initialize(points.get_points());

  // adjust unit support point for discontinuous node
  Point<dim> point;
  for (unsigned int d=0; d<dim; ++d)
    point[d] = 0.5;
  this->unit_support_points.push_back(point);
  AssertDimension(this->dofs_per_cell, this->unit_support_points.size());
}



template <int dim, int spacedim>
std::string
FE_Q_DG0<dim,spacedim>::get_name () const
{
  // note that the FETools::get_fe_from_name function depends on the
  // particular format of the string this function returns, so they have to be
  // kept in synch

  std::ostringstream namebuf;
  bool type = true;
  const unsigned int n_points = this->degree +1;
  std::vector<double> points(n_points);
  const unsigned int dofs_per_cell = this->dofs_per_cell;
  const std::vector<Point<dim> > &unit_support_points = this->unit_support_points;
  unsigned int index = 0;

  // Decode the support points in one coordinate direction.
  for (unsigned int j=0; j<dofs_per_cell; j++)
    {
      if ((dim>1) ? (unit_support_points[j](1)==0 &&
                     ((dim>2) ? unit_support_points[j](2)==0: true)) : true)
        {
          if (index == 0)
            points[index] = unit_support_points[j](0);
          else if (index == 1)
            points[n_points-1] = unit_support_points[j](0);
          else
            points[index-1] = unit_support_points[j](0);

          index++;
        }
    }
  // Do not consider the discontinuous node for dimension 1
  Assert (index == n_points || (dim==1 && index == n_points+1),
          ExcMessage ("Could not decode support points in one coordinate direction."));

  // Check whether the support points are equidistant.
  for (unsigned int j=0; j<n_points; j++)
    if (std::fabs(points[j] - (double)j/this->degree) > 1e-15)
      {
        type = false;
        break;
      }

  if (type == true)
    namebuf << "FE_Q_DG0<" << dim << ">(" << this->degree << ")";
  else
    {

      // Check whether the support points come from QGaussLobatto.
      const QGaussLobatto<1> points_gl(n_points);
      type = true;
      for (unsigned int j=0; j<n_points; j++)
        if (points[j] != points_gl.point(j)(0))
          {
            type = false;
            break;
          }
      if (type == true)
        namebuf << "FE_Q_DG0<" << dim << ">(QGaussLobatto(" << this->degree+1 << "))";
      else
        namebuf << "FE_Q_DG0<" << dim << ">(QUnknownNodes(" << this->degree << "))";
    }
  return namebuf.str();
}



template <int dim, int spacedim>
FiniteElement<dim,spacedim> *
FE_Q_DG0<dim,spacedim>::clone() const
{
  return new FE_Q_DG0<dim,spacedim>(*this);
}



template <int dim, int spacedim>
void
FE_Q_DG0<dim,spacedim>::interpolate(std::vector<double>       &local_dofs,
                                    const std::vector<double> &values) const
{
  Assert (values.size() == this->unit_support_points.size(),
          ExcDimensionMismatch(values.size(),
                               this->unit_support_points.size()));
  Assert (local_dofs.size() == this->dofs_per_cell,
          ExcDimensionMismatch(local_dofs.size(),this->dofs_per_cell));
  Assert (this->n_components() == 1,
          ExcDimensionMismatch(this->n_components(), 1));

  std::copy(values.begin(), values.end(), local_dofs.begin());

  // We don't need the discontinuous function for local interpolation
  local_dofs[local_dofs.size()-1] = 0.;
}



template <int dim, int spacedim>
void
FE_Q_DG0<dim,spacedim>::interpolate(std::vector<double>    &local_dofs,
                                    const std::vector<Vector<double> > &values,
                                    unsigned int offset) const
{
  Assert (values.size() == this->unit_support_points.size(),
          ExcDimensionMismatch(values.size(),
                               this->unit_support_points.size()));
  Assert (local_dofs.size() == this->dofs_per_cell,
          ExcDimensionMismatch(local_dofs.size(),this->dofs_per_cell));
  Assert (values[0].size() >= offset+this->n_components(),
          ExcDimensionMismatch(values[0].size(),offset+this->n_components()));

  for (unsigned int i=0; i<this->dofs_per_cell-1; ++i)
    {
      const std::pair<unsigned int, unsigned int> index
        = this->system_to_component_index(i);
      local_dofs[i] = values[i](offset+index.first);
    }

  // We don't need the discontinuous function for local interpolation
  local_dofs[local_dofs.size()-1] = 0.;
}



template <int dim, int spacedim>
void
FE_Q_DG0<dim,spacedim>::interpolate(
  std::vector<double> &local_dofs,
  const VectorSlice<const std::vector<std::vector<double> > > &values) const
{
  Assert (values[0].size() == this->unit_support_points.size(),
          ExcDimensionMismatch(values.size(),
                               this->unit_support_points.size()));
  Assert (local_dofs.size() == this->dofs_per_cell,
          ExcDimensionMismatch(local_dofs.size(),this->dofs_per_cell));
  Assert (values.size() == this->n_components(),
          ExcDimensionMismatch(values.size(), this->n_components()));

  for (unsigned int i=0; i<this->dofs_per_cell-1; ++i)
    {
      const std::pair<unsigned int, unsigned int> index
        = this->system_to_component_index(i);
      local_dofs[i] = values[index.first][i];
    }

  // We don't need the discontinuous function for local interpolation
  local_dofs[local_dofs.size()-1] = 0.;
}



template <int dim, int spacedim>
void
FE_Q_DG0<dim,spacedim>::
get_interpolation_matrix (const FiniteElement<dim,spacedim> &x_source_fe,
                          FullMatrix<double>       &interpolation_matrix) const
{
  // this is only implemented, if the source FE is also a Q_DG0 element
  typedef FE_Q_DG0<dim,spacedim> FEQDG0;
  typedef FiniteElement<dim,spacedim> FEL;

  AssertThrow ((x_source_fe.get_name().find ("FE_Q_DG0<") == 0)
               ||
               (dynamic_cast<const FEQDG0 *>(&x_source_fe) != 0),
               typename FEL::
               ExcInterpolationNotImplemented());

  Assert (interpolation_matrix.m() == this->dofs_per_cell,
          ExcDimensionMismatch (interpolation_matrix.m(),
                                this->dofs_per_cell));
  Assert (interpolation_matrix.n() == x_source_fe.dofs_per_cell,
          ExcDimensionMismatch (interpolation_matrix.m(),
                                x_source_fe.dofs_per_cell));

  this->FE_Q_Base<TensorProductPolynomialsConst<dim>,dim,spacedim>::
    get_interpolation_matrix(x_source_fe, interpolation_matrix);
}



template <int dim, int spacedim>
std::vector<bool>
FE_Q_DG0<dim,spacedim>::get_riaf_vector(const unsigned int deg)
{
  std::vector<bool> riaf(Utilities::fixed_power<dim>(deg+1)+1,false);
  riaf[riaf.size()-1]=true;
  return riaf;
}



template <int dim, int spacedim>
std::vector<unsigned int>
FE_Q_DG0<dim,spacedim>::get_dpo_vector(const unsigned int deg)
{
  std::vector<unsigned int> dpo(dim+1, 1U);
  for (unsigned int i=1; i<dpo.size(); ++i)
    dpo[i]=dpo[i-1]*(deg-1);

  dpo[dim]++;//we need an additional DG0-node for a dim-dimensional object
  return dpo;
}



template <int dim, int spacedim>
bool
FE_Q_DG0<dim,spacedim>::has_support_on_face (const unsigned int shape_index,
                                             const unsigned int face_index) const
{
  // discontinuous function has support on all faces
  if (shape_index == this->dofs_per_cell-1)
    return true;
  else
    return FE_Q_Base<TensorProductPolynomialsConst<dim>,dim,spacedim>::has_support_on_face(shape_index, face_index);
}


// explicit instantiations
#include "fe_q_dg0.inst"

DEAL_II_NAMESPACE_CLOSE
