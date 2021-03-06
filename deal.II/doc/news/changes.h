 // ---------------------------------------------------------------------
// $Id$
//
// Copyright (C) 2013, 2014 by the deal.II authors
//
// This file is part of the deal.II library.
//
// The deal.II library is free software; you can use it, redistribute
// it, and/or modify it under the terms of the GNU Lesser General
// Public License as published by the Free Software Foundation; either
// version 2.1 of the License, or (at your option) any later version.
// The full text of the license can be found in the file LICENSE at
// the top level of the deal.II distribution.
//
// ---------------------------------------------------------------------

/**
@page changes_after_8_1 Changes after Version 8.1

<p>
This is the list of changes made after the release of
deal.II version 8.1.0.
All entries are signed with the names of the authors.
</p>



<!-- ----------- INCOMPATIBILITIES ----------------- -->

<a name="incompatible"></a>
<h3 style="color:red">Incompatibilities</h3>

<p style="color:red">
Following are a few modifications to the library that unfortunately
are incompatible with previous versions of the library, but which we
deem necessary for the future maintainability of the
library. Unfortunately, some of these changes will require
modifications to application programs. We apologize for the
inconvenience this causes.
</p>

<ol>
  <li> Removed: Class PointerMatrixBase (and, consequently, the various
  classes derived from it) had comparison operators that were intended to
  work generically for any kind of derived class. However, the implementation
  used a scheme that was not robust enough to handle the various situations
  that derived classes implemented and, consequently, was not always correct.
  These operators were not previously used inside the library and, likely,
  were not widely used in applications either. They have now been removed.
  <br>
  (Wolfgang Bangerth, 2014/02/15)
  </li>

  <li> The change from functionparser to muparser introduced a small number of
  incompatibilies: units, use_degress, and recursion with 'eval' are not
  longer supported. Comparing for equality is done using '==' instead of '='.
  <br>
  (Timo Heister, 2014/02/10)
  </li>  

  <li> Changed: The various classes generating graphical output, such
  as DataOut or DataOutStack, are all derived from a common interface
  class DataOutInterface which, in turn was derived from DataOutBase
  through <i>private</i> inheritance. Because we frequently also
  access the (public) members of this private base class this has tripped
  up most every compiler we know of at one point or another. Furthermore,
  because DataOutBase was a class that only defined static member functions
  and had not member variables, there was really no reason for this
  construct.
  <br>
  For these reasons, DataOutBase is now just a regular namespace and the
  inheritance is gone. For the most part, this should not lead to any
  incompatibilities except in cases where you accessed members of
  DataOutBase through their derived classes. For example, it was possible
  to write <code>DataOut@<2@>::Patch@<2,2@></code> even though the
  <code>Patch</code> class is actually declared in DataOutBase. Since
  the inheritance is now gone, this is no longer possible and one
  actually has to write DataOutBase::Patch instead. Using this form
  turns out to be compatible also with older versions of deal.II.
  <br>
  (Wolfgang Bangerth, 2014/02/01)
  </li>
</ol>


<!-- ----------- GENERAL IMPROVEMENTS ----------------- -->

<a name="general"></a>
<h3>General</h3>


<ol>
  <li> Changed: The functionparser library bundled with deal.II got replaced
  by the muparser library.  
  <br>
  (Timo Heister, 2014/02/10)
  </li>  

  <li> Changed: It was possible to call DoFAccessor::set_active_fe_index()
  on non-active cells. However, this made no sense: Since degrees of
  freedoms only exist on active cells
  for hp::DoFHandler (i.e., there is currently no implementation
  of multilevel hp::DoFHandler objects), it does not make sense
  to assign active FE indices to non-active cells since they
  do not have finite element spaces associated with them without
  having any degrees of freedom.
  <br>
  The same of course is true for asking for the finite element active
  on a non-active cell, i.e. using the functions
  DoFAccessor::active_fe_index() and
  DoFAccessor::get_fe(). All of these functions now produce exceptions on
  non-active cells.
  <br>
  (Wolfgang Bangerth, 2014/01/24)
  </li>

  <li> New: deal.II now links with the
  <a href="http://www.boost.org/doc/libs/1_55_0/libs/iostreams/doc/index.html">BOOST
  Iostreams</a> library (at least if the libz and libbz2 libraries
  can be found that are necessary for BOOST Iostreams).
  Among many other things, this allows to easily
  read files that have been compressed, as in the following code snippet:
  @code
    #include <boost/iostreams/filtering_stream.hpp>
    #include <boost/iostreams/filter/gzip.hpp>
    #include <boost/iostreams/device/file.hpp>

    ...

    boost::iostreams::filtering_istream in;
    in.push(boost::iostreams::basic_gzip_decompressor<>());
    in.push(boost::iostreams::file_source("myfile.gz"));

    int i;
    in >> i;
  @endcode
  More documentation on how to use BOOST Iostream can be found
  in the documentation link referenced above.
  <br>
  (Wolfgang Bangerth, 2013/12/21)
  </li>
</ol>


<!-- ----------- SPECIFIC IMPROVEMENTS ----------------- -->

<a name="specific"></a>
<h3>Specific improvements</h3>

<ol>
  <li> Improved: Inhomogeneous tangential and normal flow constraints can
       now be treated via VectorTools::compute_nonzero_normal_flux_constraints
       and VectorTools::compute_nonzero_tangential_flux_constraints.
  <br>
  (Daniel Arndt, 2014/03/16)
  </li>

  <li> Changed: Class TriaAccessor had a function parent_index(), but this function
  could only work for cell accessors. The function has consequently been moved
  to class CellAccessor.
  <br>
  (Wolfgang Bangerth, 2014/03/15)
  </li>

  <li> Fixed: step-32 had a piece of code where we accessed an internal
  representation of how Trilinos vectors are actually stored. This is poor
  style and has been rewritten.
  <br>
  (Wolfgang Bangerth, 2014/03/14)
  </li>

  <li> Fixed: VectorTools::project_boundary_values_curl_conforming contained
  a bug for some cases. This is now fixed.
  <br>
  (Markus B&uuml;rg, 2014/03/10)
  </li>
  
  <li> Fixed: ParameterHandler will no longer output an error if the file
  to be read ends with "end" without a newline.
  <br>
  (Timo Heister, 2014/02/28)
  </li>

  <li>Improved: DoFRenumbering::Cuthill_McKee can now run with distributed
  triangulations with the renumbering only done within each processor's
  subdomain.
  <br>
  (Martin Kronbichler, 2014/02/20)

  <li>Fixed: There was an indexing error in GridIn::read_vtk() that triggered
  for some input files. This is now fixed.
  <br>
  (Mayank Sabharwal, 2014/02/19)

  <li>New: There is a new namespace TimeStepping for the algorithms that do time
  integrations. In this new namespace, several Runge-Kutta methods have been
  implemented: explicit methods, implicit methods, and embedded explicit methods.
  <br>
  (Damien Lebrun-Grandie, Bruno Turcksin, 2014/02/17)

  <li>New: There is now a class FEEvaluationDGP that implements matrix-free
  evaluation routines by truncated tensor products for FE_DGP elements.
  <br>
  (Martin Kronbichler, 2014/02/17)

  <li>Changed: The InverseMatrixRichardson used to eat all exceptions
  that may have been produced by the underlying Richardson solver, leaving
  no trace that the underlying solver may have failed when you call functions
  such as InverseMatrixRichardson::vmult(). These exceptions are now propagated
  out to the caller.
  <br>
  (Wolfgang Bangerth, 2014/02/16)


  <li>New: FE_TraceQ implements finite elements on faces, which
  correspond to the traces of H<sup>1</sup>-conforming elements.
  <br>
  (Angela Klewinghaus, 2014/02/14)

  <li>New: FE_FaceQ and FE_FaceP now also work in 1D (with a single dof
  on each vertex).
  <br>
  (Martin Kronbichler, 2014/02/11)

  <li>Fixed: FE_DGQ::has_support_on_face returned a wrong number for element
  degree larger than 1 in 1D. This is now fixed.
  <br>
  (Martin Kronbichler, 2014/02/10)

  <li>Changed: DerivativeApproximation used to be a class that only had
  static members. It is now a namespace.
  <br>
  (Wolfgang Bangerth, 2014/02/08)

  <li>New: ThreadLocalStorage::clear() clears out all objects allocated on the
  current and all other threads.
  <br>
  (Wolfgang Bangerth, 2014/02/06)

  <li>Fixed: A configuration error on Debian Testing where accidentally a
  non-pic libSuiteSparse_config.a was picked up when building a shared
  library up resulting in a link error.
  <br>
  (Matthias Maier, 2014/02/04)

  <li> Changed: GridTools::transform() can now deal with meshes with hanging nodes.
  <br>
  (Timo Heister, 2014/02/04)
  </li>

  <li>Fixed: Calling FEValuesViews::Vector::get_function_curls() computed
  wrong results in some cases (see https://code.google.com/p/dealii/issues/detail?id=182).
  This is now fixed.
  <br>
  (Christoph Heiniger, Wolfgang Bangerth, 2014/02/03)

  <li>Added: The class LAPACKFullMatrix now implements interfaces to
  matrix-matrix multiplication. Also, LAPACKFullMatrix::apply_lu_factorization
  now also operates on multiple right hand sides in form of another
  LAPACKFullMatrix.
  <br>
  (Martin Kronbichler, 2014/02/03)

  <li>Added: A sanity check for the full link interface at configure time.
  Hopefully this prevents some people from compiling the whole library just
  to hit a link error.
  <br>
  (Matthias Maier, 2014/02/01)

  <li>Fixed: The build system does no longer record full paths to system
  libraries but uses the appropriate short names instead.
  <br>
  (Matthias Maier, 2014/02/01)

  <li>Reworked: External feature setup. Disabling a feature now cleans up
  associated internal, cached variables. A per-feature linkage test now spots
  common linking inconsistencies early in the configuration stage (and not
  just after a complete compilation).
  <br>
  (Matthias Maier, 2014/02/01)

  <li>New/fixed: The ParameterHandler::print_parameters_section
  method did not work for XML output. There is now a flag
  include_top_level_elements which prints all higher
  subsection elements, default is false.
  For XML output setting this flag to true is required
  to ensure that the output is a valid XML document,
  starting with one root element ParameterHandler and
  compatible with read_input_from_xml and the parameterGUI.
  <br>
  (Martin Steigemann, 2014/02/01)

  <li>New: There is now a method to copy the content from a
  PETScWrappers::MPI::Vector and TrilinosWrappers::MPI::Vector to
  deal.II's parallel distributed vector.
  <br>
  (Ben Thompson, Martin Kronbichler, 2014/01/31)

  <li>Fixed: The SolutionTransfer class had all sorts of problems when
  used with hp::DoFHandler that made its results at least questionable.
  Several parts of this class have been rewritten to make the results
  more predictable and, likely, more correct.
  <br>
  (Wolfgang Bangerth, 2014/01/26)

  <li>Fixed: A regression where a single whitespace accidentally added to
  DEAL_II_LINKER_FLAGS internally prevented cmake-2.8.8 from configuring
  sucessfully.
  <br>
  (Matthias Maier, Krysztof Bzowski, 2014/01/26)

  <li> Fixed: SparsityPattern::max_entries_per_row() forgot to consider
  the last row of the matrix and consequently sometimes returned
  wrong values. This is now fixed.
  <br>
  (Martin Kronbichler, 2014/01/22)
  </li>

  <li> Improved: In rare cases when the vector of error indicators
  has entries equal to zero the adjust_interesting_range method
  produces a negative lower bound. As a result the
  parallel::distributed::GridRefinement::refine_and_coarsen_fixed_*
  methods flagged the wrong number of cells for coarsening and refinement.
  This is now changed by adjusting the lower bound in adjust_interesting_range
  only, if not equal to zero.
  <br>
  (Martin Steigemann, 2014/01/22)
  </li>

  <li> Changed: It was previously possible to set the
  <code>active_fe_index</code> on non-active cells of an hp::DoFHandler.
  However, this was prone to mistakes because it may lead to the assumption
  that a finite element space out of the ones described by the hp::FECollection
  associated with this hp::DoFHandler was actually associated with such
  a cell. Since we do not actually distribute degrees of freedom for such
  hp::DoFHandler objects on non-active cells, this is not the case. Consequently,
  it no longer has any effect to assign active FE indices to non-active cells:
  these values are simply reset later on.
  <br>
  (Wolfgang Bangerth, 2014/01/20)
  </li>

  <li> Fixed: The method DoFTools::extract_constant_modes only worked for
  elements where the constant function 1 is represented by all ones. This
  is now fixed by querying the element for its constant modes on each cell.
  <br>
  (Martin Kronbichler, 2014/01/19)
  </li>

  <li> Fixed: PETScWrappers::MPI::Vector::all_zero() was broken with more than
  one processor (illegal memory access) and did not communicate between all
  processors. Documentation for many vector types of all_zero() has been extended.
  <br>
  (Timo Heister, 2014/01/17)
  </li>

  <li> Fixed/new: DoFCellAccessor::set_dof_values_by_interpolation and
  DoFCellAccessor::get_interpolated_dof_values could previously be
  called for hp::DoFHandler objects on cells that are non-active. This
  makes no sense since these cells have no associated finite element
  space. Doing so now raises an exception.
  <br>
  However, there are legitimate cases where one may want to interpolate
  from children to a parent's finite element space or the other way around.
  Since in the hp
  case no finite element space is naturally associated with an inactive
  cell, it is now possible to pass an explicit finite element index
  argument to these functions specifying which element of an hp::FECollection
  object describes the space onto which you want to interpolate.
  <br>
  (Mihai Alexe, Wolfgang Bangerth, 2014/01/18)
  </li>

  <li> Fixed: The methods IndexSet::do_compress() and
  IndexSet::add_indices(IndexSet&) had quadratic complexity in the number of
  ranges. The algorithms have been changed into linear complexity ones.
  <br>
  (Martin Kronbichler, 2014/01/15)
  </li>

  <li> Fixed: There were several bugs in functions like
  FEValues::get_function_values() where the code did not properly handle the
  case of FE_Nothing. This is now fixed.
  <br>
  (Wolfgang Bangerth, 2014/01/08)
  </li>

  <li> Fixed: DataOut got confused in some situations where one uses FE_Nothing.
  This is now fixed.
  <br>
  (Minh Do-Quang, Wolfgang Bangerth, 2014/01/08)
  </li>

  <li> Fixed: FESystem::get_interpolation_matrix, a function that is among
  other places used by SolutionTransfer, had a bug that prevented it from
  running correctly in some situations where one uses FE_Nothing.
  This is now fixed.
  <br>
  (Minh Do-Quang, Wolfgang Bangerth, 2014/01/08)
  </li>

  <li> Improved: When you call WorkStream::run with an empty function object
  for the copier, operations on individual cells are essentially all independent.
  In other words, you have a massively parallel collection of jobs. In this
  case, a parallel for loop over all elements is better suited than the
  pipeline approach currently used. This has now been implemented.
  <br>
  (Wolfgang Bangerth, 2013/12/26)
  </li>

  <li> New: The new function VectorTools::interpolate_based_on_material_id()
  can be used to interpolate several functions onto a mesh, based on the
  material id of each cell individually.
  <br>
  (Valentin Zingan, 2013/12/26)
  </li>

  <li> New: A new reinit() method has been introduced to
  TrilinosWrappers::SparsityPattern that takes all rows that are possibly
  written into as an optional argument. This allows for pre-allocating all
  possible entries right away, which makes writing into the matrix from
  several threads possible (otherwise, only one processor at a time can write
  off-processor data). Similarly, TrilinosWrappers::MPI::Vector objects can
  be initialized with hints to ghost elements for a writable vector that can
  be added into from multiple threads.
  <br>
  (Martin Kronbichler, 2013/12/23)
  </li>

  <li> New: The TableBase::fill function has become more powerful in that
  it now doesn't just take pointers to initializing elements but can deal
  with arbitrary input iterators. It now also takes a flag that denotes the
  order in which table elements are initialized, allowing to switch between
  C- and Fortran-style table layouts.
  <br>
  Along with the TableBase::fill function, the Table classes of various
  ranks have also gotten constructors that allow the in-place construction
  not only of a table of correct size, but already initialized from
  somewhere. This finally allows to mark Table objects as const by creating
  them already with the correct content.
  <br>
  (Wolfgang Bangerth, 2013/12/21)
  </li>

  <li> New: There is now a new class Functions::InterpolatedTensorProductGridData that can
  be used to (bi-/tri-)linearly interpolate data given on a tensor product
  mesh of $x$ (and $y$ and $z$) values, for example to evaluate experimentally
  determined coefficients, or to assess the accuracy of a solution by
  comparing with a solution generated by a different code and written in
  gridded data. There is also a new class Functions::InterpolatedUniformGridData that
  can perform the same task more efficiently if the data is stored on meshes
  that are uniform in each coordinate direction.
  <br>
  (Wolfgang Bangerth, 2013/12/20)
  </li>

  <li> Fixed: ParameterHandler::get_double() and ParameterHandler::get_integer()
  had bugs in that they didn't detect if they were asked to return a number
  for a parameter whose value was in fact not a number but some general
  text. This is now fixed.
  <br>
  (Wolfgang Bangerth, 2013/12/19)
  </li>

  <li> Fixed: VectorTools::project_boundary_values could not deal with
  function values close to (but not exactly equal to) zero. This is now fixed.
  <br>
  (Martin Kronbichler, 2013/12/16)
  </li>

  <li> New: It is now possible to select between different smoothers and coarse
  solvers in the Trilinos AMG preconditioners by a string to the smoother's name.
  <br>
  (Andrew Baker, 2013/12/14)
  </li>

</ol>


*/
