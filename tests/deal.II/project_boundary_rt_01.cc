//----------------------------------------------------------------------
//    $Id$
//    Version: $Name$ 
//
//    Copyright (C) 2007 by the deal.II authors
//
//    This file is subject to QPL and may not be  distributed
//    without copyright and license information. Please refer
//    to the file deal.II/doc/license.html for the  text  and
//    further information on this license.
//
//----------------------------------------------------------------------

#include <base/logstream.h>
#include <base/function.h>
#include <base/quadrature_lib.h>
#include <lac/vector.h>
#include <grid/grid_generator.h>
#include <dofs/dof_handler.h>
#include <fe/fe_raviart_thomas.h>
#include <fe/mapping_q1.h>
#include <fe/fe_values.h>
#include <numerics/vectors.h>
#include <numerics/data_out.h>

#include <fstream>

using namespace dealii;

/**
 * A vector-valued polynomial for testing RT elements.
 */
template<int dim>
class TestFunction : public Function<dim>
{
  public:
    
				     /// Construct a polynomial of degree p
    TestFunction(unsigned int degree);

    virtual void vector_value_list (const std::vector<Point<dim> > &points,
				    std::vector<Vector<double> >   &values) const;
  private:
    unsigned int degree;
};


template<int dim>
TestFunction<dim>::TestFunction (unsigned int p)
		:
		Function<dim>(dim), degree(p)
{}


template<int dim>
void
TestFunction<dim>::vector_value_list (const std::vector<Point<dim> > &points,
				      std::vector<Vector<double> >   &values) const
{
  if (degree < 2)
    {
      Assert(false, ExcNotImplemented());
    }
  else
    {
      for (unsigned int k=0;k<points.size();++k)
	{
					   // Base of the function is
					   // the distance to a
					   // different point in each
					   // component
	  for (unsigned int d=0;d<dim;++d)
	    {
	      Point<dim> p = points[k];
	      for (unsigned int dd=0;dd<dim;++dd)
		p(dd) -= d;
	      const double r2 = p.square();
	      values[k](d) = std::pow(r2, (int) degree/2);
	    }
	}
    }
}



template<int dim>
double integrate_error(const DoFHandler<dim>& dof,
		       FEFaceValues<dim>& fe,
		       const Vector<double>& u,
		       const Function<dim>& f)
{
  double result = 0.;
  std::vector<Vector<double> > f_values(fe.n_quadrature_points, Vector<double>(dim));
  std::vector<Vector<double> > fe_values(fe.n_quadrature_points, Vector<double>(dim));  
  
  for (typename DoFHandler<dim>::active_cell_iterator cell = dof.begin_active();
       cell != dof.end(); ++cell)
    {
      for (unsigned int face=0 ; face != GeometryInfo<dim>::faces_per_cell; ++face)
	{
	  if (!cell->at_boundary(face)) continue;
	  
	  fe.reinit(cell, face);
	  f.vector_value_list(fe.get_quadrature_points(), f_values);
	  fe.get_function_values(u, fe_values);
	  for (unsigned int k=0;k<fe.n_quadrature_points;++k)
	    {
	      double diff = 0.;
	      for (unsigned int d=0;d<dim;++d)
		diff += fe.normal_vector(k)(d) * (f_values[k](d) - fe_values[k](d));
	      result += fe.JxW(k) * diff * diff;
	    }
	}
    }
  return result;
}


template<int dim>
void test_projection (const Triangulation<dim>& tr,
		      const FiniteElement<dim>& fe)
{
  deallog << fe.get_name() << std::endl;

  const unsigned int degree = fe.tensor_degree();
  
  DoFHandler<dim> dof(tr);
  dof.distribute_dofs(fe);
  
  QGauss<dim-1> quadrature(degree+2);
  MappingQ1<dim> mapping;

  TestFunction<dim> f(degree-1);
  std::map<unsigned int, double> boundary_constraints;
  typename FunctionMap<dim>::type boundary_map;
  for (unsigned char i=0;i<255;++i)
    boundary_map[i] = &f;
  VectorTools::project_boundary_values(mapping, dof, boundary_map, quadrature,
				       boundary_constraints);

				   // Fill a vector with the projected
				   // boundary values
  Vector<double> u(dof.n_dofs());
  for (typename std::map<unsigned int, double>::const_iterator
	 i = boundary_constraints.begin(); i != boundary_constraints.end(); ++i)
    u(i->first) = i->second;
  
  FEFaceValues<dim> feval(mapping, fe, quadrature,
			  update_quadrature_points
			  | update_normal_vectors
			  | update_JxW_values
			  | update_values);
  double err = integrate_error(dof, feval, u, f);
  deallog << err << std::endl;

  DataOut<dim> dout;
  dout.attach_dof_handler(dof);
  std::ofstream of("T.gnuplot");
  dout.add_data_vector(u, "u");
  dout.build_patches(3);
  dout.write_gnuplot(of);
}


template<int dim>
void test_hyper_cube(const FiniteElement<dim>& fe)
{
  Triangulation<dim> tr;
  GridGenerator::hyper_cube(tr);
  tr.refine_global(2);
  test_projection(tr, fe);
}


int main()
{
  std::ofstream logfile ("project_boundary_rt_01/output");
  logfile.precision (2);
  logfile.setf(std::ios::fixed);  
  deallog.attach(logfile);
  deallog.depth_console (0);
  deallog.threshold_double(1.e-12);

  FE_RaviartThomasNodal<2> rt22(2);
  test_hyper_cube(rt22);
  FE_RaviartThomasNodal<2> rt23(3);
  test_hyper_cube(rt23);
  FE_RaviartThomasNodal<2> rt24(4);
  test_hyper_cube(rt24);
  
  
}
