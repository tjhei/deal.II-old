//----------------------------  grid_out.h  ---------------------------
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
//----------------------------  grid_out.h  ---------------------------
#ifndef __deal2__grid_out_h
#define __deal2__grid_out_h



#include <base/config.h>
#include <base/exceptions.h>
#include <string>

template <int dim> class Triangulation;
template <int dim> class Mapping;


/**
 * Within this namespace, we define several structures that are used
 * to describe flags that can be given to grid output routines to
 * modify the default outfit of the grids written into a file. See the
 * different subclasses and the documentation of the @ref{GridOut}
 * class for more details.
 *
 * @author Wolfgang Bangerth, 1998, 2001
 */
namespace GridOutFlags
{
				   /**
				    * Flags describing the details of
				    * output in UCD format.
				    */
  struct Ucd 
  {
				       /**
					* Write a comment at the beginning
					* of the file stating the date of
					* creation and some other data.
					* While this is supported by the
					* UCD format (and the AVS program),
					* some other programs get confused
					* by this, so you can switch it off
					* this way.
					*
					* Default: @p{true}.
					*/
      bool write_preamble;
      
				       /**
					* When writing a mesh, write boundary
					* faces explicitly if their boundary
					* indicator is not the default
					* boundary indicator, which is zero.
					* This is necessary if you later
					* want to re-read the grid and want
					* to get the same boundary indicators
					* for the different parts of the
					* boundary of the triangulation.
					*
					* It is not necessary if you only want
					* to write the triangulation to
					* view or print it.
					*
					* Default: @p{false}.
					*/
      bool write_faces;
      
				       /**
					* Constructor.
					*/
      Ucd (const bool write_preamble = true,
	   const bool write_faces    = false);
  };
  
  
				   /**
				    * Flags describing the details of
				    * output in GNUPLOT format.
				    */
  struct Gnuplot
  {
				       /**
					* Write the number of each cell into
					* the output file before starting
					* with the lines it is composed of,
					* as a comment. This might be useful
					* if you want to find out details
					* about the grid, for example the
					* position of cells of which you
					* know the number. It enlarges
					* the size of the output
					* significantly, however.
					*
					* Default: @p{false}.
					*/
      bool write_cell_numbers;
      
				       /**
					* This is the number of
					* points on a boundary face,
					* that are ploted
					* additionally to the
					* vertices of the face.
					*/
      unsigned int n_boundary_face_points;
      
				       /**
					* Constructor.
					*/
      Gnuplot (const bool         write_cell_number = false,
	       const unsigned int n_boundary_face_points = 2);
  };

				   /**
				    * Flags describing the details of
				    * output for encapsulated postscript.
				    * In this structure, the flags common
				    * to all dimensions are listed. Flags
				    * which are specific to one space
				    * dimension only are listed in derived
				    * classes.
				    *
				    * By default, the size of the picture
				    * is scaled such that the width equals
				    * 300 units.
				    */
  struct EpsFlagsBase
  {
				       /**
					* Enum denoting the possibilities
					* whether the scaling should be done
					* such that the given @p{size} equals
					* the width or the height of
					* the resulting picture.
					*/
      enum SizeType {
	    width, height
      };
      
				       /**
					* See above. Default is @p{width}.
					*/
      SizeType size_type;
      
				       /**
					* Width or height of the output
					* as given in postscript units
					* This usually is given by the
					* strange unit 1/72 inch. Whether
					* this is height or width is
					* specified by the flag
					* @p{size_type}.
					*
					* Default is 300.
					*/
      unsigned int size;
      
				       /**
					* Width of a line in postscript
					* units. Default is 0.5.
					*/
      double line_width;
				       /**
					* Should lines with a set @p{user_flag}
					* be drawn in a different color?
					*/
      bool color_lines_on_user_flag;
      
				       /**
					* This is the number of
					* points on a boundary face,
					* that are ploted
					* additionally to the
					* vertices of the face.
					*
					* This is used if the
					* mapping used is not the
					* standard @p{MappingQ1}
					* mapping.
					*/
      unsigned int n_boundary_face_points;
      
				       /**
					* Constructor.
					*/
      EpsFlagsBase (const SizeType     size_type  = width,
		    const unsigned int size       = 300,
		    const double       line_width = 0.5,
		    const bool color_lines_on_user_flag = false,
		    const unsigned int n_boundary_face_points = 2);
  };
  
  
				   /**
				    * Flags describing the details of
				    * output for encapsulated postscript
				    * for all dimensions not explicitly
				    * specialized below. Some flags that
				    * are common to all dimensions are
				    * listed in the base class.
				    *
				    * This class does not actually
				    * exist, we only here declare the
				    * general template and declare
				    * explicit specializations below.
				    */
  template <int dim>
  struct Eps
  {};
    
				   /**
				    * Flags specific to the output of
				    * grids in one space dimensions.
				    */
  template <>
  struct Eps<1> : public EpsFlagsBase 
  {
				       /**
					* Constructor.
					*/
      Eps (const SizeType     size_type  = width,
	   const unsigned int size       = 300,
	   const double       line_width = 0.5,
	   const bool         color_lines_on_user_flag = false,
	   const unsigned int n_boundary_face_points = 2);
  };

    
				   /**
				    * Flags specific to the output of
				    * grids in two space dimensions.
				    */
  template <>
  struct Eps<2> : public EpsFlagsBase 
  {
				       /**
					* Constructor.
					*/
      Eps (const SizeType     size_type  = width,
	   const unsigned int size       = 300,
	   const double       line_width = 0.5,
	   const bool         color_lines_on_user_flag = false,
	   const unsigned int n_boundary_face_points = 2);
  };
  
				   /**
				    * Flags specific to the output of
				    * grids in three space dimensions.
				    */
  template <>
  struct Eps<3> : public EpsFlagsBase
  {
				       /**
					* Angle of the line origin-viewer
					* against the z-axis in degrees.
					*
					* Default is the Gnuplot-default
					* of 60.
					*/
      double azimut_angle;
      
				       /**
					* Angle by which the viewers
					* position projected onto the
					* x-y-plane is rotated around
					* the z-axis, in positive sense
					* when viewed from above. The
					* unit are degrees, and zero
					* equals a position above or below
					* the negative y-axis.
					*
					* Default is the Gnuplot-default
					* of 30.
					*/
      double turn_angle;
      
				       /**
					* Constructor.
					*/
      Eps (const SizeType     size_type  = width,
	   const unsigned int size       = 300,
	   const double       line_width = 0.5,
	   const bool         color_lines_on_user_flag = false,
	   const unsigned int n_boundary_face_points = 2,
	   const double       azimut_angle    = 60,
	   const double       turn_angle      = 30);
  };
};



/**
 * This class provides a means to output a triangulation to a file in different
 * formats. Presently provided are functions to write it in GNUPLOT and
 * encapsulated postscript format, as well as in UCD format.
 * There are also function to dispatch to
 * the different output functions based on a parameter given, e.g., through
 * a parameter file, thus making user programs invariant of the number and
 * names of the file formats implemented in this class. The main advantage of
 * this class over the @ref{DataOut} class is that it does not have to mess around
 * with actual data vectors and that no @ref{DoFHandler} object is needed to
 * write the pure geometrical information involved here.
 *
 * Available output formats can be found in the functions with names @p{write_...}
 *
 * @sect3{Usage}
 * Usage is simple: either you use the direct form
 * @begin{verbatim}
 *   ofstream output_file("some_filename");
 *   GridOut().write_gnuplot (tria, output_file);
 * @end{verbatim}
 * if you know which format you want to have, or if you want the format to be
 * a runtime parameter, you can write
 * @begin{verbatim}
 *   GridOut::OutputFormat grid_format =
 *                   GridOut::parse_output_format(get_format_name_from_somewhere());
 *   ofstream output_file("some_filename" + GridOut::default_suffix(output_format));
 *   GridOut().write (tria, output_file, output_format);
 * @end{verbatim}
 * The function @p{get_output_format_names()} provides a list of possible names of
 * output formats in a string that is understandable by the @ref{ParameterHandler} class.
 *
 * Note that here, we have created an unnamed object of type @ref{GridOut} and called
 * one of its @p{write_*} functions. This looks like as if the respective function
 * could really be made @p{static}. This was not done in order to allow for
 * parameters to be passed to the different output functions in a way compatible
 * with the scheme of allowing the right output format to be selected at run-time
 * through the generic @p{write} function.
 *
 * In order to explain this, consider each function had one or more additional
 * parameters giving the details of output, for example position of the spectator
 * for 3d meshed, line thicknesses, etc. While this would allow each output
 * function any flexibility it needs, it would not allow us to use the generic
 * function @p{write} which is given a parameter determining the output format,
 * since it is impractical to give it a list of parameters for each and every
 * output format supported which it may then pass on to the respective output
 * function.
 *
 * Rather, we have chosen to let each object of this class
 * @ref{GridOut} have a set of parameters for each supported output
 * format. These are collected in structures @ref{GridOutFlags::Eps},
 * @ref{GridOutFlags::Gnuplot}, etc declared in the @ref{GridOutFlags}
 * namespace, and you can set your preferred flags like this:
 * @begin{verbatim}
 *   GridOut grid_out;
 *   GridOutFlags::Ucd ucd_flags;
 *   ...    // set some fields in ucd_flags
 *   grid_out.set_flags (ucd_flags);
 *   ...
 *   ...    // write some file with data_out
 * @end{verbatim}
 * The respective output function then use the so-set flags. By default, they
 * are set to reasonable values as described above and in the documentation
 * of the different flags structures. Resetting the flags can
 * be done by calling @p{grid_out.set_flags (GridOutFlags::Ucd());}, since the
 * default constructor of each of the flags structures sets the parameters
 * to their initial values.
 *
 * The advantage of this approach is that it is possible to change the flags
 * of one or more output formats according to your needs and later use the
 * generic @p{write} function; the actual output function then called will
 * use the flags as set before.
 *
 * Note that some of the structures describing the flags of the different
 * output formats are empty since the respective format does not support
 * any flags. The structure and the @p{set_flags} function are provided
 * anyway. Note also that some of the structures may differ between the
 * dimensions supported by this class; they then have a template parameter,
 * as usual.
 *
 *
 * @author Wolfgang Bangerth, 1999; postscript format based on an implementation by Stefan Nauber, 1999
 */
class GridOut 
{
  public:
				     /**
				      * Declaration of a name for each of the
				      * different output formats.
				      */
    enum OutputFormat { gnuplot, eps, ucd };

				     /**
				      * Write the triangulation in the
				      * gnuplot format.
				      *
				      * In GNUPLOT format, each cell
				      * is written as a sequence of
				      * its confining lines. Apart
				      * from the coordinates of the
				      * line's end points, the level
				      * and the material of the cell
				      * are appended to each line of
				      * output. Therefore, if you let
				      * GNUPLOT draw a 2d grid as a 3d
				      * plot, you will see more
				      * refined cells being raised
				      * against cells with less
				      * refinement.  Also, if you draw
				      * a cut through a 3d grid, you
				      * can extrude the refinement
				      * level in the direction
				      * orthogonal to the cut
				      * plane. The same can be done
				      * with the material id, which is
				      * plotted after the level.
				      *
				      * A more useful application of
				      * this feature is the following:
				      * if you use the GNUPLOT
				      * command (for a 2d grid here)
				      * @begin{verbatim}
				      * splot [:][:][2.5:3.5] "grid_file.gnuplot" *
				      * @end{verbatim}
				      * then the
				      * whole x- and y-range will be
				      * plotted, i.e. the whole grid,
				      * but only those lines with a
				      * z-value between 2.5 and
				      * 3.5. Since the z-values were
				      * chosen to be the level to
				      * which a cell belongs, this
				      * results in a plot of those
				      * cells only that belong to
				      * level 3 in this example. This
				      * way, it is easy to produce
				      * plots of the different levels
				      * of grid.
				      *
				      * @p{mapping} is a pointer to a
				      * mapping used for the
				      * transformation of cells at the
				      * boundary. If zero, then use
				      * standard Q1 mapping.
				      *
				      * Names and values of additional
				      * flags controlling the output
				      * can be found in the
				      * documentation of the
				      * @ref{GridOutFlags::Gnuplot} class.
				      */
    template <int dim>
    void write_gnuplot (const Triangulation<dim> &tria,
			std::ostream             &out,
			const Mapping<dim>       *mapping=0);

				     /**
				      * Declaration of the specialization
				      * of above function for 1d.
				      */
    void write_gnuplot (const Triangulation<1> &tria,
			std::ostream           &out,
			const Mapping<1>       *mapping=0);
    
				     /**
				      * Write the triangulation in the
				      * ucd format.
				      *
				      * UCD (unstructured cell data)
				      * is the format used by AVS and
				      * some other programs. It is
				      * described in the AVS
				      * developer's guide. Besides the
				      * usual output of the grid
				      * only, you can decide through
				      * additional flags (see below,
				      * and the documentation of the
				      * @ref{GridOutFlags::Ucd} class)
				      * whether boundary faces with
				      * non-zero boundary indicator
				      * shall be written to the file
				      * explicitly. This is useful,
				      * if you want to re-read the
				      * grid later on, since
				      * @p{deal.II} sets the boundary
				      * indicator to zero by default;
				      * therefore, to obtain the
				      * same triangulation as before,
				      * you have to specify faces
				      * with differing boundary
				      * indicators explicitly, which
				      * is done by this flag.
				      *
				      * Names and values of further
				      * flags controlling the output
				      * can be found in the
				      * documentation of the
				      * @ref{GridOut::Ucd} class.
				      */
    template <int dim>
    void write_ucd (const Triangulation<dim> &tria,
		    std::ostream             &out);

				     /**
				      * Write the triangulation in the
				      * encapsulated postscript format.
				      *
				      * In this format, each line of
				      * the triangulation is written
				      * separately. We scale the
				      * picture such that either
				      * x-values or y-values range
				      * between zero and a fixed
				      * size. The other axis is scaled
				      * by the same factor. Which axis
				      * is taken to compute the scale
				      * and the size of the box it
				      * shall fit into is determined
				      * by the output flags (see
				      * below, and the documentation
				      * of the @ref{GridOutFlags::Eps}
				      * class).
				      *
				      * The bounding box is close to
				      * the triangulation on all four
				      * sides, without an extra
				      * frame. The line width is
				      * chosen to be 0.5 by default,
				      * but can be changed. The line
				      * width is to be compared with
				      * the extension of the picture,
				      * of which the default is 300.
				      *
				      * The flag 
				      * @p{color_lines_on_user_flag}
				      * allows to draw lines with the
				      * @p{user_flag} set to be drawn in
				      * red. The colors black and red
				      * are defined as @p{b} and @p{r} in
				      * the preamble of the output
				      * file and can be changed there
				      * according to need.
				      *
				      * @p{mapping} is a pointer to a
				      * mapping used for the
				      * transformation of cells at the
				      * boundary. If zero, then use
				      * standard Q1 mapping.
				      *
				      * Names and values of additional
				      * flags controlling the output
				      * can be found in the
				      * documentation of the
				      * @ref{GridOutFlags::Eps}
				      * class. Especially the
				      * viewpoint for three
				      * dimensional grids is of
				      * importance here.
				      */
    template <int dim>
    void write_eps (const Triangulation<dim> &tria,
		    std::ostream             &out,
		    const Mapping<dim>       *mapping=0);

				     /**
				      * Declaration of the
				      * specialization of above
				      * function for 1d. This function
				      * is presently not implemented.
				      */
    void write_eps (const Triangulation<1> &tria,
		    std::ostream           &out,
		    const Mapping<1>       *mapping=0);
    
				     /**
				      * Write data and grid to @p{out} according
				      * to the given data format. This function
				      * simply calls the appropriate
				      * @p{write_*} function.
				      */
    template <int dim>
    void write (const Triangulation<dim> &tria,
		std::ostream             &out,
		const OutputFormat        output_format,
		const Mapping<dim>       *mapping=0);

				     /**
				      * Set the flags to be used for output
				      * in UCD format.
				      */
    void set_flags (const GridOutFlags::Ucd &ucd_flags);

    				     /**
				      * Set the flags to be used for output
				      * in GNUPLOT format.
				      */
    void set_flags (const GridOutFlags::Gnuplot &gnuplot_flags);

    				     /**
				      * Set the flags to be used for output
				      * in 1d EPS output.
				      */
    void set_flags (const GridOutFlags::Eps<1> &eps_flags);

				     /**
				      * Set the flags to be used for output
				      * in 2d EPS output.
				      */
    void set_flags (const GridOutFlags::Eps<2> &eps_flags);

				     /**
				      * Set the flags to be used for output
				      * in 3d EPS output.
				      */
    void set_flags (const GridOutFlags::Eps<3> &eps_flags);

				     /**
				      * Provide a function which tells us which
				      * suffix with a given output format
				      * usually has. At present the following
				      * formats are defined:
				      * @begin{itemize}
				      * @item @p{gnuplot}: @p{.gnuplot}
				      * @item @p{ucd}: @p{.inp}
				      * @item @p{eps}: @p{.eps}.
				      * @end{itemize}
				      *
				      * Since this function does not need data
				      * from this object, it is static and can
				      * thus be called without creating an
				      * object of this class.
				      */
    static std::string default_suffix (const OutputFormat output_format);

				     /**
				      * Return the @p{OutputFormat} value
				      * corresponding to the given string. If
				      * the string does not match any known
				      * format, an exception is thrown.
				      *
				      * Since this function does not need data
				      * from this object, it is static and can
				      * thus be called without creating an
				      * object of this class. Its main purpose
				      * is to allow a program to use any
				      * implemented output format without the
				      * need to extend the program's parser
				      * each time a new format is implemented.
				      *
				      * To get a list of presently available
				      * format names, e.g. to give it to the
				      * @ref{ParameterHandler} class, use the
				      * function @p{get_output_format_names ()}.
				      */
    static OutputFormat parse_output_format (const std::string &format_name);

				     /**
				      * Return a list of implemented output
				      * formats. The different names are
				      * separated by vertical bar signs (@p{`|'})
				      * as used by the @ref{ParameterHandler}
				      * classes.
				      */
    static std::string get_output_format_names ();

				     /**
				      * Determine an estimate for the
				      * memory consumption (in bytes)
				      * of this object.
				      */
    unsigned int memory_consumption () const;

				     /**
				      * Exception
				      */
    DeclException0 (ExcInvalidState);

  private:

				     /**
				      * Flags to be used upon output of UCD
				      * data. Can be changed by using the
				      * @p{set_flags} function.
				      */
    GridOutFlags::Ucd     ucd_flags;

				     /**
				      * Flags to be used upon output of GNUPLOT
				      * data. Can be changed by using the
				      * @p{set_flags} function.
				      */
    GridOutFlags::Gnuplot gnuplot_flags;

				     /**
				      * Flags to be used upon output of EPS
				      * data in one space dimension. Can be
				      * changed by using the @p{set_flags}
				      * function.
				      */
    GridOutFlags::Eps<1>  eps_flags_1;

				     /**
				      * Flags to be used upon output of EPS
				      * data in two space dimensions. Can be
				      * changed by using the @p{set_flags}
				      * function.
				      */
    GridOutFlags::Eps<2>  eps_flags_2;

				     /**
				      * Flags to be used upon output of EPS
				      * data in three space dimensions. Can be
				      * changed by using the @p{set_flags}
				      * function.
				      */
    GridOutFlags::Eps<3>  eps_flags_3;


				     /**
				      * Write the grid information about
				      * faces to @p{out}. Only those faces
				      * are printed which are on the boundary
				      * and which have a boundary indicator
				      * not equal to zero, since the latter
				      * is the default for boundary faces.
				      *
				      * Since cells and faces are continuously
				      * numbered, the @p{starting_index} for
				      * the numbering of the faces is passed
				      * also.
				      *
				      * This function unfortunately can not
				      * be included in the regular @p{write_ucd}
				      * function, since it needs special
				      * treatment for the case @p{dim==1}, in
				      * which case the face iterators are
				      * @p{void*}'s and lack the member functions
				      * which are called. We would not actually
				      * call these functions, but the compiler
				      * would complain anyway when compiling
				      * the function for @p{dim==1}. Bad luck.
				      */
    template <int dim>
    void write_ucd_faces (const Triangulation<dim> &tria,
			  const unsigned int        starting_index,
			  std::ostream             &out) const;

				     /**
				      * Declaration of the specialization
				      * of above function for 1d. Does
				      * nothing.
				      */
    void write_ucd_faces (const Triangulation<1> &tria,
			  const unsigned int      starting_index,
			  std::ostream           &out) const;
    
				     /**
				      * Return the number of faces in the
				      * triangulation which have a boundary
				      * indicator not equal to zero. Only
				      * these faces are explicitly printed
				      * in the @p{write_*} functions;
				      * all faces with indicator 255 are
				      * interior ones and an indicator with
				      * value zero for faces at the boundary
				      * are considered default.
				      *
				      * This function always returns an empty
				      * list in one dimension.
				      *
				      * The reason for this function is the
				      * same as for @p{write_ucd_faces}. See
				      * there for more information.
				      */
    template <int dim>
    unsigned int n_boundary_faces (const Triangulation<dim> &tria) const;

				     /**
				      * Declaration of the specialization
				      * of above function for
				      * 1d. Simply returns zero.
				      */
    unsigned int n_boundary_faces (const Triangulation<1> &tria) const;
};



#endif
