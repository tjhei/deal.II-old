/* $Id$ */


#include <base/point.h>
#include <basic/grid_out.h>
#include <grid/tria.h>
#include <grid/tria_accessor.h>
#include <grid/tria_iterator.h>

#include <iomanip>
#include <algorithm>
#include <list>
#include <ctime>
#include <cmath>



template <int dim>
void GridOut::write_ucd (const Triangulation<dim> &tria,
			 ostream                  &out) 
{
  AssertThrow (out, ExcIO());

  typename Triangulation<dim>::active_cell_iterator        cell=tria.begin_active();
  const typename Triangulation<dim>::active_cell_iterator  endc=tria.end();

				   // first loop over all cells to
				   // find out the vertices which
				   // are in use. copy them for fast
				   // output
  vector<Point<dim> > vertices (tria.n_vertices());
  vector<bool> vertex_used (tria.n_vertices(), false);
  unsigned int        n_vertices = 0;

  for (cell=tria.begin_active(); cell!=endc; ++cell)
    for (unsigned int vertex=0; vertex<GeometryInfo<dim>::vertices_per_cell;
	 ++vertex)
				       // if not yet copied
      if (vertex_used[cell->vertex_index(vertex)] == false)
	{
	  vertex_used[cell->vertex_index(vertex)] = true;
	  vertices[cell->vertex_index(vertex)] = cell->vertex(vertex);
	  ++n_vertices;
	};
  

				   // write preamble
  if (ucd_flags.write_preamble)
    {
				       // block this to have local
				       // variables destroyed after
				       // use
      time_t  time1= time (0);
      tm     *time = localtime(&time1); 
      out << "# This file was generated by the deal.II library." << endl
	  << "# Date =  "
	  << time->tm_year+1900 << "/"
	  << time->tm_mon+1 << "/"
	  << time->tm_mday << endl
	  << "# Time =  "
	  << time->tm_hour << ":"
	  << setw(2) << time->tm_min << ":"
	  << setw(2) << time->tm_sec << endl
	  << "#" << endl
	  << "# For a description of the UCD format see the AVS Developer's guide."
	  << endl
	  << "#" << endl;
    };

				   // start with ucd data
  out << n_vertices << ' '
      << tria.n_active_cells() + (ucd_flags.write_faces ?
				  n_boundary_faces(tria) :
				  0)
      << " 0 0 0"                  // no data
      << endl;

				   // actually write the vertices.
				   // note that we shall number them
				   // with first index 1 instead of 0
  for (unsigned int i=0; i<vertices.size(); ++i)
    if (vertex_used[i])
      {
	out << i+1                 // vertex index
	    << "  "
	    << vertices[i];
	for (unsigned int d=dim+1; d<=3; ++d)
	  out << " 0";             // fill with zeroes
	out << endl;
      };
	
				   // write cells. Enumerate cells
				   // consecutively, starting with 1
  unsigned int cell_index=1;
  for (cell=tria.begin_active();
       cell!=endc; ++cell, ++cell_index)
    {
      out << cell_index << ' '
	  << static_cast<unsigned int>(cell->material_id())
	  << " ";
      switch (dim) 
	{
	  case 1:  out << "line    "; break;
	  case 2:  out << "quad    "; break;
	  case 3:  out << "hex     "; break;
	  default:
		Assert (false, ExcNotImplemented());
	};

				       // it follows a list of the
				       // vertices of each cell. in 1d
				       // this is simply a list of the
				       // two vertices, in 2d its counter
				       // clockwise, as usual in this
				       // library. in 3d, the same applies
				       // (special thanks to AVS for
				       // numbering their vertices in a
				       // way compatible to deal.II!)
				       //
				       // technical reference:
				       // AVS Developer's Guide, Release 4,
				       // May, 1992, p. E6
				       //
				       // note: vertex numbers are 1-base
      for (unsigned int vertex=0; vertex<GeometryInfo<dim>::vertices_per_cell;
	   ++vertex)
	out << cell->vertex_index(vertex)+1 << ' ';
      out << endl;
    };

				   // write faces with non-zero boundary
				   // indicator
  if (ucd_flags.write_faces)
    write_ucd_faces (tria, cell_index, out);
    
  AssertThrow (out, ExcIO());
};



#if deal_II_dimension == 1

template <>
unsigned int GridOut::n_boundary_faces (const Triangulation<1> &) const
{
  return 0;
};

#endif



template <int dim>
unsigned int GridOut::n_boundary_faces (const Triangulation<dim> &tria) const
{
  typename Triangulation<dim>::active_face_iterator face, endf;
  unsigned long int n_faces = 0;

  for (face=tria.begin_active_face(), endf=tria.end_face();
       face != endf; ++face)
    if ((face->at_boundary()) &&
	(face->boundary_indicator() != 0))
      n_faces++;

  return n_faces;
};



#if deal_II_dimension == 1

template <>
void GridOut::write_ucd_faces (const Triangulation<1> &,
			       const unsigned int,
			       ostream &) const
{
  return;
};

#endif



template <int dim>
void GridOut::write_ucd_faces (const Triangulation<dim> &tria,
			       const unsigned int        starting_index,
			       ostream                  &out) const
{
  typename Triangulation<dim>::active_face_iterator face, endf;
  unsigned int index=starting_index;

  for (face=tria.begin_active_face(), endf=tria.end_face();
       face != endf; ++face)
    if (face->at_boundary() &&
	(face->boundary_indicator() != 0)) 
      {
	out << index << "  "
	    << static_cast<unsigned int>(face->boundary_indicator())
	    << "  ";
	switch (dim) 
	  {
	    case 2: out << "line    ";  break;
	    case 3: out << "quad    ";  break;
	    default:
		  Assert (false, ExcNotImplemented());
	  };
	for (unsigned int vertex=0; vertex<GeometryInfo<dim>::vertices_per_face; ++vertex)
	  out << face->vertex_index(vertex) << ' ';
	out << endl;

	++index;
      };	  
};



template <int dim>
void GridOut::write_gnuplot (const Triangulation<dim> &tria,
			     ostream                  &out) 
{
  AssertThrow (out, ExcIO());

  typename Triangulation<dim>::active_cell_iterator        cell=tria.begin_active();
  const typename Triangulation<dim>::active_cell_iterator  endc=tria.end();
  for (; cell!=endc; ++cell)
    {
      if (gnuplot_flags.write_cell_numbers)
	out << "# cell " << cell << endl;
      
      switch (dim)
	{
	  case 1:
		out << cell->vertex(0) << ' ' << cell->level() << endl
		    << cell->vertex(1) << ' ' << cell->level() << endl
		    << endl;
		break;

	  case 2:
		out << cell->vertex(0) << ' ' << cell->level() << endl
		    << cell->vertex(1) << ' ' << cell->level() << endl
		    << cell->vertex(2) << ' ' << cell->level() << endl
		    << cell->vertex(3) << ' ' << cell->level() << endl
		    << cell->vertex(0) << ' ' << cell->level() << endl
		    << endl  // double new line for gnuplot 3d plots
		    << endl;
		break;

	  case 3:
						 // front face
		out << cell->vertex(0) << ' ' << cell->level() << endl
		    << cell->vertex(1) << ' ' << cell->level() << endl
		    << cell->vertex(2) << ' ' << cell->level() << endl
		    << cell->vertex(3) << ' ' << cell->level() << endl
		    << cell->vertex(0) << ' ' << cell->level() << endl
		    << endl;
						 // back face
		out << cell->vertex(4) << ' ' << cell->level() << endl
		    << cell->vertex(5) << ' ' << cell->level() << endl
		    << cell->vertex(6) << ' ' << cell->level() << endl
		    << cell->vertex(7) << ' ' << cell->level() << endl
		    << cell->vertex(4) << ' ' << cell->level() << endl
		    << endl;

						 // now for the four connecting lines
		out << cell->vertex(0) << ' ' << cell->level() << endl
		    << cell->vertex(4) << ' ' << cell->level() << endl
		    << endl;
		out << cell->vertex(1) << ' ' << cell->level() << endl
		    << cell->vertex(5) << ' ' << cell->level() << endl
		    << endl;
		out << cell->vertex(2) << ' ' << cell->level() << endl
		    << cell->vertex(6) << ' ' << cell->level() << endl
		    << endl;
		out << cell->vertex(3) << ' ' << cell->level() << endl
		    << cell->vertex(7) << ' ' << cell->level() << endl
		    << endl;
		break;
	};
    };
  
  AssertThrow (out, ExcIO());
};



template <int dim>
void GridOut::write_eps (const Triangulation<dim> &tria,
			 ostream                  &out) 
{
  typedef list<pair<Point<2>,Point<2> > > LineList;

				   // get a pointer to the flags
				   // common to all dimensions,
				   // in order to avoid the recurring
				   // distinctions between
				   // eps_flags_1, eps_flags_2, ...
  const EpsFlagsBase &eps_flags_base = (dim==1 ?
					(EpsFlagsBase&)eps_flags_1 :
					(dim==2 ?
					 (EpsFlagsBase&)eps_flags_2 :
					 (dim==3 ?
					  (EpsFlagsBase&)eps_flags_3 :
					  *(EpsFlagsBase*)0)));
  
  AssertThrow (out, ExcIO());

				   // make up a list of lines by which
				   // we will construct the triangulation
				   //
				   // this part unfortunately is a bit
				   // dimension dependent, so we have to
				   // treat every dimension different.
				   // however, by directly producing
				   // the lines to be printed, i.e. their
				   // 2d images, we can later do the
				   // actual output dimension independent
				   // again
  LineList line_list;

  switch (dim)
    {
      case 2:
      {
	Triangulation<dim>::active_line_iterator line   =tria.begin_active_line ();
	Triangulation<dim>::active_line_iterator endline=tria.end_line ();
	
	for (; line!=endline; ++line)
					   // one would expect
					   // make_pair(line->vertex(0),
					   //           line->vertex(1))
					   // here, but that is not
					   // dimension independent, since
					   // vertex(i) is Point<dim>,
					   // but we want a Point<2>.
					   // in fact, whenever we're here,
					   // the vertex is a Point<dim>,
					   // but the compiler does not
					   // know this. hopefully, the
					   // compiler will optimize away
					   // this little kludge
	  line_list.push_back (make_pair(Point<2>(line->vertex(0)(0),
						  line->vertex(0)(1)),
					 Point<2>(line->vertex(1)(0),
						  line->vertex(1)(1))));
	break;
      };
       
      case 3:
      {
	Triangulation<dim>::active_line_iterator line   =tria.begin_active_line ();
	Triangulation<dim>::active_line_iterator endline=tria.end_line ();
	
					 // loop over all lines and compute their
					 // projection on the plane perpendicular
					 // to the direction of sight

					 // direction of view equals the unit 
					 // vector of the position of the
					 // spectator to the origin.
					 //
					 // we chose here the viewpoint as in
					 // gnuplot as default.
					 //
					 // note: the following might be wrong
					 // if one of the base vectors below
					 // is in direction of the viewer, but
					 // I am too tired at present to fix
					 // this
	const double z_angle    = eps_flags_3.azimut_angle;
	const double turn_angle = eps_flags_3.turn_angle;
	const double pi = 3.1415926536;
	const Point<dim> view_direction(-sin(z_angle * 2.*pi / 360.) * sin(turn_angle * 2.*pi / 360.),
					+sin(z_angle * 2.*pi / 360.) * cos(turn_angle * 2.*pi / 360.),
					-cos(z_angle * 2.*pi / 360.));
	
					 // decide about the two unit vectors
					 // in this plane. we chose the first one
					 // to be the projection of the z-axis
					 // to this plane
	const Point<dim> vector1
	  = Point<dim>(0,0,1) - ((Point<dim>(0,0,1) * view_direction) * view_direction);
	const Point<dim> unit_vector1 = vector1 / sqrt(vector1.square());
	
					 // now the third vector is fixed. we
					 // chose the projection of a more or
					 // less arbitrary vector to the plane
					 // perpendicular to the first one
	const Point<dim> vector2
	  = (Point<dim>(1,0,0)
	     - ((Point<dim>(1,0,0) * view_direction) * view_direction)
	     - ((Point<dim>(1,0,0) * unit_vector1)   * unit_vector1));
	const Point<dim> unit_vector2 = vector2 / sqrt(vector2.square());
	
	for (; line!=endline; ++line) 
	  line_list.push_back (make_pair(Point<2>(line->vertex(0) * unit_vector2,
						  line->vertex(0) * unit_vector1),
					 Point<2>(line->vertex(1) * unit_vector2,
						  line->vertex(1) * unit_vector1)));

	break;
      };

      default:
	    Assert (false, ExcNotImplemented());
    };
  
  

				   // find out minimum and maximum x and
				   // y coordinates to compute offsets
				   // and scaling factors
  double x_min = tria.begin_active_line()->vertex(0)(0);
  double x_max = x_min;
  double y_min = tria.begin_active_line()->vertex(0)(1);
  double y_max = y_min;

  for (LineList::const_iterator line=line_list.begin();
       line!=line_list.end(); ++line)
    {
      x_min = min (x_min, line->first(0));
      x_min = min (x_min, line->second(0));

      x_max = max (x_max, line->first(0));
      x_max = max (x_max, line->second(0));

      y_min = min (y_min, line->first(1));
      y_min = min (y_min, line->second(1));

      y_max = max (y_max, line->first(1));
      y_max = max (y_max, line->second(1));
    };

				   // scale in x-direction such that
				   // in the output 0 <= x <= 300.
				   // don't scale in y-direction to
				   // preserve the shape of the
				   // triangulation
  const double scale = (eps_flags_base.size /
			(eps_flags_base.size_type==EpsFlagsBase::width ?
			 x_max - x_min :
			 y_min - y_max));
  
  
				   // now write preamble
  if (true) 
    {
				       // block this to have local
				       // variables destroyed after
				       // use
      time_t  time1= time (0);
      tm     *time = localtime(&time1); 
      out << "%!PS-Adobe-2.0 EPSF-1.2" << endl
	  << "%%Title: deal.II Output" << endl
	  << "%%Creator: the deal.II library" << endl
	  << "%%Creation Date: " 
	  << time->tm_year+1900 << "/"
	  << time->tm_mon+1 << "/"
	  << time->tm_mday << " - "
	  << time->tm_hour << ":"
	  << setw(2) << time->tm_min << ":"
	  << setw(2) << time->tm_sec << endl
	  << "%%BoundingBox: "
					 // lower left corner
	  << "0 0 "
					 // upper right corner
	  << static_cast<unsigned int>( (x_max-x_min) * scale )
	  << ' '
	  << static_cast<unsigned int>( (y_max-y_min) * scale )
	  << endl;

				       // define some abbreviations to keep
				       // the output small:
				       // m=move turtle to
				       // x=execute line stroke
      out << "/m {moveto} bind def" << endl
	  << "/x {lineto stroke} bind def" << endl;
      
      out << "%%EndProlog" << endl
	  << endl;

				       // set fine lines
      out << eps_flags_base.line_width << " setlinewidth" << endl;
    };

				   // now write the lines
  const Point<2> offset(x_min, y_min);
  
  for (LineList::const_iterator line=line_list.begin();
       line!=line_list.end(); ++line)
    out << (line->first  - offset) * scale << " m "
	<< (line->second - offset) * scale << " x\n";

  out << "showpage" << endl;
  
  AssertThrow (out, ExcIO());
};




template <int dim>
void GridOut::write (const Triangulation<dim> &tria,
		     ostream                  &out,
		     OutputFormat              output_format)
{
  switch (output_format)
    {
      case ucd:
	    write_ucd (tria, out);
	    return;

      case gnuplot:
	    write_gnuplot (tria, out);
	    return;

      case eps:
	    write_eps (tria, out);
	    return;
    };

  Assert (false, ExcInternalError());
};




// explicit instantiations. note that write instantiates all the other
// functions as needed
template void GridOut::write (const Triangulation<deal_II_dimension> &, ostream &, OutputFormat);
