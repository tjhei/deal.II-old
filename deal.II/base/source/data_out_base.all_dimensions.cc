/* $Id$ */


#include <base/data_out_base.h>
#include <base/parameter_handler.h>


DataOutBase::UcdFlags::UcdFlags (const bool write_preamble) :
		write_preamble (write_preamble)
{};



DataOutBase::PovrayFlags::PovrayFlags (const bool smooth,
				       const bool bicubic_patch,
				       const bool external_data) :
		smooth (smooth),
		bicubic_patch(bicubic_patch),
		external_data(external_data)
{};



void DataOutBase::UcdFlags::declare_parameters (ParameterHandler &prm)
{
  prm.declare_entry ("Write preamble", "true", Patterns::Bool());
};



void DataOutBase::UcdFlags::parse_parameters (ParameterHandler &prm)
{
  write_preamble = prm.get_bool ("Write preamble");
};



void DataOutBase::GnuplotFlags::declare_parameters (ParameterHandler &/*prm*/)
{
};



void DataOutBase::GnuplotFlags::parse_parameters (ParameterHandler &/*prm*/)
{
};



void DataOutBase::PovrayFlags::declare_parameters (ParameterHandler &prm)
{
  prm.declare_entry ("Use smooth triangles", "false",
		     Patterns::Bool());
  prm.declare_entry ("Use bicubic patches", "false",
		     Patterns::Bool());
  prm.declare_entry ("Include external file", "true",
		     Patterns::Bool ());
};



void DataOutBase::PovrayFlags::parse_parameters (ParameterHandler &prm)
{
  smooth        = prm.get_bool ("Use smooth triangles");
  bicubic_patch = prm.get_bool ("Use bicubic patches");
  external_data = prm.get_bool ("Include external file");
};



DataOutBase::EpsFlags::EpsFlags (const unsigned int  height_vector,
				 const unsigned int  color_vector,
				 const SizeType      size_type,
				 const unsigned int  size,
				 const double        line_width,
				 const double        azimut_angle,
				 const double        turn_angle,
				 const double        z_scaling,
				 const bool          draw_mesh,
				 const bool          draw_cells,
				 const bool          shade_cells,
				 const ColorFunction color_function) :
		height_vector(height_vector),
		color_vector(color_vector),
		size_type(size_type),
		size(size),
		line_width(line_width),
		azimut_angle(azimut_angle),
		turn_angle(turn_angle),
		z_scaling(z_scaling),
		draw_mesh(draw_mesh),
		draw_cells(draw_cells),
		shade_cells(shade_cells),
		color_function(color_function)
{};



DataOutBase::EpsFlags::RgbValues
DataOutBase::EpsFlags::default_color_function (const double x,
					       const double xmin,
					       const double xmax)
{
  RgbValues rgb_values;
  
// A difficult color scale:
//     xmin          = black  (1)
// 3/4*xmin+1/4*xmax = blue   (2)
// 1/2*xmin+1/2*xmax = green  (3)
// 1/4*xmin+3/4*xmax = red    (4)
//              xmax = white  (5)
// Makes the following color functions:
//
// red      green    blue
//       __
//      /      /\  /  /\    /
// ____/    __/  \/  /  \__/

//     { 0                                (1) - (3)
// r = { ( 4*x-2*xmin+2*xmax)/(xmax-xmin) (3) - (4)
//     { 1                                (4) - (5)
//
//     { 0                                (1) - (2)
// g = { ( 4*x-3*xmin-  xmax)/(xmax-xmin) (2) - (3)
//     { (-4*x+  xmin+3*xmax)/(xmax-xmin) (3) - (4)
//     { ( 4*x-  xmin-3*xmax)/(xmax-xmin) (4) - (5)
//
//     { ( 4*x-4*xmin       )/(xmax-xmin) (1) - (2)
// b = { (-4*x+2*xmin+2*xmax)/(xmax-xmin) (2) - (3)
//     { 0                                (3) - (4)
//     { ( 4*x-  xmin-3*xmax)/(xmax-xmin) (4) - (5)

  double sum   =   xmax+  xmin;
  double sum13 =   xmin+3*xmax;
  double sum22 = 2*xmin+2*xmax;
  double sum31 = 3*xmin+  xmax;
  double dif = xmax-xmin;
  double rezdif = 1.0/dif;

  int where;

  if (x<(sum31)/4)
    where = 0;
  else if (x<(sum22)/4)
    where = 1;
  else if (x<(sum13)/4)
    where = 2;
  else
    where = 3;

  if (dif!=0)
    {
      switch (where)
	{
	  case 0:
		rgb_values.red   = 0;
		rgb_values.green = 0;
		rgb_values.blue  = (x-xmin)*4.*rezdif;
		break;
	  case 1:
		rgb_values.red   = 0;
		rgb_values.green = (4*x-3*xmin-xmax)*rezdif;
		rgb_values.blue  = (sum22-4.*x)*rezdif;
		break;
	  case 2:
		rgb_values.red   = (4*x-2*sum)*rezdif;
		rgb_values.green = (xmin+3*xmax-4*x)*rezdif;
		rgb_values.blue  = 0;
		break;
	  case 3:
		rgb_values.red   = 1;
		rgb_values.green = (4*x-xmin-3*xmax)*rezdif;
		rgb_values.blue  = (4.*x-sum13)*rezdif;
	  default:
		break;
	};
    }
  else // White 
    rgb_values.red = rgb_values.green = rgb_values.blue = 1;

  return rgb_values;
};



DataOutBase::EpsFlags::RgbValues
DataOutBase::EpsFlags::grey_scale_color_function (const double x,
						  const double xmin,
						  const double xmax)
{
  DataOutBase::EpsFlags::RgbValues rgb_values;
  rgb_values.red = rgb_values.blue = rgb_values.green
		 = (x-xmin)/(xmax-xmin);
  return rgb_values;
};



bool DataOutBase::EpsCell2d::operator < (const EpsCell2d &e) const
{
				   // note the "wrong" order in
				   // which we sort the elements
  return depth > e.depth;
};



void DataOutBase::EpsFlags::declare_parameters (ParameterHandler &prm)
{
  prm.declare_entry ("Index of vector for height", "0",
		     Patterns::Integer());
  prm.declare_entry ("Index of vector for color", "0",
		     Patterns::Integer());
  prm.declare_entry ("Scale to width or height", "width",
		     Patterns::Selection ("width|height"));
  prm.declare_entry ("Size (width or height) in eps units", "300",
		     Patterns::Integer());
  prm.declare_entry ("Line widths in eps units", "0.5",
		     Patterns::Double());
  prm.declare_entry ("Azimut angle", "60",
		     Patterns::Double());
  prm.declare_entry ("Turn angle", "30",
		     Patterns::Double());
  prm.declare_entry ("Scaling for z-axis", "1",
		     Patterns::Double ());
  prm.declare_entry ("Draw mesh lines", "true",
		     Patterns::Bool());
  prm.declare_entry ("Fill interior of cells", "true",
		     Patterns::Bool());
  prm.declare_entry ("Color shading of interior of cells", "true",
		     Patterns::Bool());
  prm.declare_entry ("Color function", "default",
		     Patterns::Selection ("default|grey scale"));
};



void DataOutBase::EpsFlags::parse_parameters (ParameterHandler &prm)
{
  height_vector = prm.get_integer ("Index of vector for height");
  color_vector  = prm.get_integer ("Index of vector for color");
  if (prm.get ("Scale to width or height") == "width")
    size_type   = width;
  else
    size_type   = height;
  size          = prm.get_integer ("Size (width or height) in eps units");
  line_width    = prm.get_double ("Line widths in eps units");
  azimut_angle  = prm.get_double ("Azimut angle");
  turn_angle    = prm.get_double ("Turn angle");
  z_scaling     = prm.get_double ("Scaling for z-axis");
  draw_mesh     = prm.get_bool ("Draw mesh lines");
  draw_cells    = prm.get_bool ("Fill interior of cells");
  shade_cells   = prm.get_bool ("Color shading of interior of cells");
  if (prm.get("Color function") == "default")
    color_function = &default_color_function;
  else
    color_function = &grey_scale_color_function;
};



void DataOutBase::GmvFlags::declare_parameters (ParameterHandler &/*prm*/)
{
};



void DataOutBase::GmvFlags::parse_parameters (ParameterHandler &/*prm*/)
{
};
