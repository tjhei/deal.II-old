//----------------------------  tria_boundary_lib.cc  ---------------------------
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
//----------------------------  tria_boundary_lib.cc  ---------------------------


#include <grid/tria_boundary_lib.h>
#include <grid/tria.h>
#include <grid/tria_iterator.h>
#include <grid/tria_accessor.h>
#include <base/tensor.h>
#include <cmath>


template <int dim>
HyperBallBoundary<dim>::HyperBallBoundary (const Point<dim> p,
					   const double     radius) :
		center(p), radius(radius)
{};



template <int dim>
Point<dim>
HyperBallBoundary<dim>::get_new_point_on_line (const typename Triangulation<dim>::line_iterator &line) const
{
  Point<dim> middle = StraightBoundary<dim>::get_new_point_on_line (line);
  
  middle -= center;
				   // project to boundary
  middle *= radius / sqrt(middle.square());
  
  middle += center;
  return middle;
};



#if deal_II_dimension == 1

template <>
Point<1>
HyperBallBoundary<1>::
get_new_point_on_quad (const Triangulation<1>::quad_iterator &) const
{
  Assert (false, ExcInternalError());
  return Point<1>();
};

#endif



template <int dim>
Point<dim>
HyperBallBoundary<dim>::
get_new_point_on_quad (const typename Triangulation<dim>::quad_iterator &quad) const
{
  Point<dim> middle = StraightBoundary<dim>::get_new_point_on_quad (quad);
  
  middle -= center;
				   // project to boundary
  middle *= radius / sqrt(middle.square());
  
  middle += center;
  return middle;
};


#if deal_II_dimension == 2

template <>
void
HyperBallBoundary<2>::get_intermediate_points_on_line (
  const Triangulation<2>::line_iterator &line,
  std::vector<Point<2> > &points) const
{
  const unsigned int n=points.size();
  Assert(n>0, ExcInternalError());
  if (n==1)
    points[0]=get_new_point_on_line(line);
  else
    {
      Point<2> v0=line->vertex(0)-center,
	       v1=line->vertex(1)-center;
      double eps=1e-14;
      Assert(fabs(v0.square()-radius*radius)<eps, ExcInternalError());
      Assert(fabs(v1.square()-radius*radius)<eps, ExcInternalError());
      
      double alpha=acos((v0*v1)/sqrt(v0.square()*v1.square()))/(n+1);

      Tensor<2,2> S;
      S[0][0]=cos(alpha);
      S[1][0]=sin(alpha);
      S[0][1]=-S[1][0];
      S[1][1]=S[0][0];

      contract(points[0], S, v0);
      for (unsigned int i=1; i<n; ++i)
	contract(points[i], S, points[i-1]);

      for (unsigned int i=0; i<n; ++i)
	points[i]+=center;
    }
}

#endif


template <int dim>
void
HyperBallBoundary<dim>::get_intermediate_points_on_line (
  const typename Triangulation<dim>::line_iterator &,
  typename std::vector<Point<dim> > &) const
{
  Assert(false, ExcNotImplemented());
}


#if deal_II_dimension == 3

template <>
void
HyperBallBoundary<3>::get_intermediate_points_on_quad (
  const Triangulation<3>::quad_iterator &quad,
  std::vector<Point<3> > &points) const
{
  Assert(points.size()==1, ExcNotImplemented());
  StraightBoundary<3>::get_intermediate_points_on_quad (quad, points);
  Point<3> &middle=points[0];
  
  middle -= center;
				   // project to boundary
  middle *= radius / sqrt(middle.square());
  
  middle += center;
}

#endif


template <int dim>
void
HyperBallBoundary<dim>::get_intermediate_points_on_quad (
  const Triangulation<dim>::quad_iterator &,
  typename std::vector<Point<dim> > &) const
{
  Assert(false,ExcNotImplemented());
}


template <int dim>
Point<dim>
HyperBallBoundary<dim>::get_center () const 
{
  return center;
};



template <int dim>
double
HyperBallBoundary<dim>::get_radius () const 
{
  return radius;
};


/* ---------------------------------------------------------------------- */


template <int dim>
HalfHyperBallBoundary<dim>::HalfHyperBallBoundary (const Point<dim> center,
						   const double     radius) :
		HyperBallBoundary<dim> (center, radius)
{};



template <int dim>
Point<dim>
HalfHyperBallBoundary<dim>::
get_new_point_on_line (const typename Triangulation<dim>::line_iterator &line) const 
{
  const Point<dim> line_center = line->center();
  if (line_center(0) == center(0))
    return line_center;
  else
    return HyperBallBoundary<dim>::get_new_point_on_line (line);
};



#if deal_II_dimension == 1

template <>
Point<1>
HalfHyperBallBoundary<1>::
get_new_point_on_quad (const Triangulation<1>::quad_iterator &) const 
{
  Assert (false, ExcInternalError());
  return Point<1>();
};

#endif



template <int dim>
Point<dim>
HalfHyperBallBoundary<dim>::
get_new_point_on_quad (const typename Triangulation<dim>::quad_iterator &quad) const 
{
  const Point<dim> quad_center = quad->center();
  if (quad_center(0) == center(0))
    return quad_center;
  else
    return HyperBallBoundary<dim>::get_new_point_on_quad (quad);
};



/* ---------------------------------------------------------------------- */



template <int dim>
HyperShellBoundary<dim>::HyperShellBoundary (const Point<dim> &center) :
		center (center) 
{};



template <int dim>
Point<dim>
HyperShellBoundary<dim>::
get_new_point_on_line (const typename Triangulation<dim>::line_iterator &line) const 
{
  const Point<dim> middle = StraightBoundary<dim>::get_new_point_on_line (line);
				   // compute the position of the points relative to the origin
  const Point<dim> middle_relative = middle - center,
		   vertex_relative = line->vertex(0) - center;
  
				   // take vertex(0) to gauge the
				   // radius corresponding to the line
				   // under consideration
  const double radius = sqrt(vertex_relative.square());

				   // scale and shift back to the
				   // original coordinate system
  return (middle_relative * (radius / sqrt(middle_relative.square()))) + center;
};



#if deal_II_dimension == 1

template <>
Point<1>
HyperShellBoundary<1>::
get_new_point_on_quad (const Triangulation<1>::quad_iterator &) const
{
  Assert (false, ExcInternalError());
  return Point<1>();
};

#endif



template <int dim>
Point<dim>
HyperShellBoundary<dim>::
get_new_point_on_quad (const typename Triangulation<dim>::quad_iterator &quad) const
{
  const Point<dim> middle = StraightBoundary<dim>::get_new_point_on_quad (quad);
				   // compute the position of the points relative to the origin
  const Point<dim> middle_relative = middle - center,
		   vertex_relative = quad->vertex(0) - center;
  
				   // take vertex(0) to gauge the
				   // radius corresponding to the line
				   // under consideration
  const double radius = sqrt(vertex_relative.square());

				   // scale and shift back to the
				   // original coordinate system
  return (middle_relative * (radius / sqrt(middle_relative.square()))) + center;
};


/* ---------------------------------------------------------------------- */




template <int dim>
HalfHyperShellBoundary<dim>::HalfHyperShellBoundary (const Point<dim> &center) :
		HyperShellBoundary<dim> (center) 
{};



template <int dim>
Point<dim>
HalfHyperShellBoundary<dim>::
get_new_point_on_line (const typename Triangulation<dim>::line_iterator &line) const 
{
				   // first check whether the two end
				   // points of the line are on the
				   // axis of symmetry. if so, then
				   // return the mid point
  if ((line->vertex(0)(0) == center(0)) &&
      (line->vertex(1)(0) == center(0)))
    return (line->vertex(0) + line->vertex(1))/2;
  

				   // otherwise we are on the outer or
				   // inner part of the shell. proceed
				   // as in the base class
  return HyperShellBoundary<dim>::get_new_point_on_line (line);
};



#if deal_II_dimension == 1

template <>
Point<1>
HalfHyperShellBoundary<1>::
get_new_point_on_quad (const Triangulation<1>::quad_iterator &) const
{
  Assert (false, ExcInternalError());
  return Point<1>();
};

#endif



template <int dim>
Point<dim>
HalfHyperShellBoundary<dim>::
get_new_point_on_quad (const typename Triangulation<dim>::quad_iterator &quad) const
{
				   // same thing as for the new point
				   // on the line
  if ((quad->vertex(0)(0) == center(0)) &&
      (quad->vertex(1)(0) == center(0)) &&
      (quad->vertex(2)(0) == center(0)) &&
      (quad->vertex(3)(0) == center(0)))
    return (quad->vertex(0) + quad->vertex(1) +
	    quad->vertex(2) + quad->vertex(3)   )/4;
  

				   // otherwise we are on the outer or
				   // inner part of the shell. proceed
				   // as in the base class
  return HyperShellBoundary<dim>::get_new_point_on_quad (quad);
};



// explicit instantiations
template class HyperBallBoundary<deal_II_dimension>;
template class HalfHyperBallBoundary<deal_II_dimension>;
template class HyperShellBoundary<deal_II_dimension>;
template class HalfHyperShellBoundary<deal_II_dimension>;
