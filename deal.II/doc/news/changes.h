/**
 * @page changes_after_6.0 Changes after Version 6.0

<p>
This is the list of changes made after the release of 
deal.II version 6.0. It is subdivided into changes
made to the three sub-libraries <a href="#base">base</a>, 
<a href="#lac">lac</a>, and <a href="#deal.II">deal.II</a>, as well as
changes to the <a href="#general">general infrastructure,
documentation, etc</a>.
</p>

<p>
All entries are signed with the names of the author. Regular
contributor's names are abbreviated by WB (Wolfgang Bangerth), GK
(Guido Kanschat), RH (Ralf Hartmann).
</p>


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
  <li> <p>Changed: The version number of the deal.II intermediate format written by
  DataOutBase::write_deal_intermediate has been increased to 3 to accomodate the fact that
  we now support writing vector-valued data to output files in at least some output formats.
  (Previously, vector-valued date was written as a collection of scalar fields.) Since
  we can only read files written in intermediate format that has the same number as the
  files we generate, this means that files written with previous format numbers can now
  no longer be read.
  <br>
  (WB 2007/10/11)
  </p>

  <li> <p>Changed: FilteredMatrix now can be applied to any matrix having the standard
  set of <code>vmult</code> functions. In order to achieve this, its interface had to be overhauled.
  Only the <code>VECTOR</code> template argument remains. Furthermore, instead of
  PreconditionJacobi being applied to FilteredMatrix, FilteredMatrix
  can now be applied to any preconditioner.
  <br>
  (GK 2007/09/25)
  </p>

  <li> <p>Changed: The deprecated typedefs
  <code>internal::Triangulation::Line</code>, 
  <code>internal::Triangulation::Quad</code>, and
  <code>internal::Triangulation::Hexahedron</code> have been removed.
  <br>
  (WB 2007/09/07)
  </p>
</ol>



<a name="general"></a>
<h3>General</h3>

<ol>
  <li>
</ol>



<a name="base"></a>
<h3>base</h3>

<ol>

  <li> <p>New: The DataOutBase class and derived classes can now deal with data that is
  logically vector-valued, i.e. derived classes can pass down information that some of
  the components of the output should be grouped together as vectors, instead of being
  treated as a number of separate scalar fields. This allows visualization programs to
  display these components as vector-fields right away, without the need to manually group
  them together into a vector.
  <p>
  While all output format writers receive the information necessary to do this, currently
  only the VTK reader in DataOutBase::write_vtk makes use of it.
  <p>
  The use of this ability is explained in the @ref step_22 "step-22" tutorial program.
  <br>
  (WB 2007/10/11)
  </p> </li>

  <li> <p>New: Macro #AssertDimension introduced for easier handling of
  ExcDimensionMismatch.
  <br>
  (GK 2007/09/13)
  </p> </li>

</ol>



<a name="lac"></a>
<h3>lac</h3>

<ol>
  <li>
</ol>



<a name="deal.II"></a>
<h3>deal.II</h3>

<ol> 

<li> <p>Improved: The number of blocks of an FESystem was properly defined and the
constructors changed accordingly. At least non of the test programs noticed the change.
<br>
(GK 2007/10/03)
</p></li>

  <li> <p>Improved: In an effort to make names more consistent, second
  derivatives in FEValuesBase and UpdateFlags have been renamed to
  Hessians. Thus, the clash between the forms <tt>2nd</tt> and
  <tt>second</tt> has been removed. Old function and enum names are
  available for compatibility but have been marked deprecated.
  <br>
  (GK 2007/09/12)
  </p></li>

  <li> <p>Fixed+New: The GridOutFlags::Ucd and
  GridOutFlags::Msh structures now take a new parameter
  <code>write_lines</code> to output lines with boundary id different
  from 0 in three-dimensional meshes. This fixes an annoying bug for
  which meshes with ids different from zero where not written in a
  compatible way, and if re-read with the corresponding
  GridIn functions, would not yield the same mesh upon
  refinement.
  <br>
  (Luca Heltai 2007/09/10) 
  </p>
  </li>

  <li> <p>Extended: The possibilities of graphical output via the DataOut,
  DataOutFaces and DataOutRotation classes have been extended by the
  ability to perform a postprocessing step before a given data
  vector is written to the output. This way, derived variables can be
  written instead of the original data. To this end, there is a new
  version of <code>DataOut_DoFData::add_data_vector</code> taking a
  data vector and a DataPostprocessor, which performs the actual
  calculation of new data based on the values and possibly derivatives
  of the original data at each point of the patch.
  <br>
  (Tobias Leicht 2007/09/10) 
  </p> 
  </li>

</ol>


*/
