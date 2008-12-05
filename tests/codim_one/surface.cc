//----------------------------  template.cc  ---------------------------
//    $Id: testsuite.html 13373 2006-07-13 13:12:08Z manigrasso $
//    Version: $Name$ 
//
//    Copyright (C) 2005 by the deal.II authors 
//
//    This file is subject to QPL and may not be  distributed
//    without copyright and license information. Please refer
//    to the file deal.II/doc/license.html for the  text  and
//    further information on this license.
//
//----------------------------  template.cc  ---------------------------


// calculates the measure of the surface of a hypersphere

#include "../tests.h"
#include <fstream>
#include <base/logstream.h>

// all include files needed for the program

#include <grid/tria.h>
#include <grid/grid_in.h>
#include <grid/grid_out.h>
#include <fe/mapping.h>
#include <fe/mapping_q1.h>
#include <fe/fe_q.h>
#include <fe/fe_values.h>
#include <base/quadrature_lib.h>
#include <dofs/dof_handler.h>
#include <dofs/dof_accessor.h>

#include <fstream>
#include <string>

std::ofstream logfile("surface/output");

// Computes the area and the outer normals of circles and spheres
// which are more and more refined, and prints the error on the
// output.
template <int dim, int spacedim>
void test(std::string filename) {

    Triangulation<dim, spacedim> triangulation;
    GridIn<dim, spacedim> gi;

    gi.attach_triangulation (triangulation);
    std::ifstream in (filename.c_str());
    gi.read_ucd (in);

    const QGauss<dim> quadrature(2);
    const FE_Q<dim,spacedim> dummy_fe (1);
    DoFHandler<dim,spacedim> dof_handler (triangulation);

    FEValues<dim,spacedim> fe_values (dummy_fe, quadrature,
				      update_JxW_values | 
				      update_cell_normal_vectors | 
				      update_quadrature_points);
  
    dof_handler.distribute_dofs (dummy_fe);

    double area = 0;
    double normals = 0;

    typename DoFHandler<dim,spacedim>::active_cell_iterator
	cell = dof_handler.begin_active(),
	endc = dof_handler.end();

    std::vector<Point<spacedim> > expectedcellnormals(fe_values.n_quadrature_points);

    for (; cell!=endc; ++cell)
    {
	fe_values.reinit (cell);    
	const std::vector<Point<spacedim> > & cellnormals = fe_values.get_cell_normal_vectors();
	const std::vector<Point<spacedim> > & quad_points = fe_values.get_quadrature_points();
      
	for (unsigned int i=0; i<fe_values.n_quadrature_points; ++i)
	{
	    expectedcellnormals[i] = quad_points[i]/quad_points[i].norm();
	    area += fe_values.JxW (i);
	    normals += (expectedcellnormals[i]-cellnormals[i]).norm();
	}
    };

    deallog<<"Approximate measure of hyper sphere = "<<area<<std::endl;
    deallog<<"Error = "<<std::fabs(dim*2*numbers::PI-area)<<std::endl;
    deallog << "Average error in norms: " 
	    << ( normals/dof_handler.get_tria().n_active_cells()
		 /fe_values.n_quadrature_points) 
	    << std::endl;

}



int main () 
{
    deallog.attach(logfile);
    deallog.depth_console(0);
  
    deallog<<"Test <1,2>"<<std::endl;
    test<1,2>("grids/circle_1.inp");
    test<1,2>("grids/circle_2.inp");
    test<1,2>("grids/circle_3.inp");
    test<1,2>("grids/circle_4.inp");

    deallog<<"Test <2,3>"<<std::endl;
    test<2,3>("grids/sphere_1.inp"); 
    test<2,3>("grids/sphere_2.inp");
    test<2,3>("grids/sphere_3.inp");
    test<2,3>("grids/sphere_4.inp");
  
    return 0;
}
