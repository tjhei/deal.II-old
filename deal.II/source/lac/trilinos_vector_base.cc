//---------------------------------------------------------------------------
//    $Id$
//    Version: $Name$
//
//    Copyright (C) 2008, 2010, 2011, 2012 by the deal.II authors
//
//    This file is subject to QPL and may not be  distributed
//    without copyright and license information. Please refer
//    to the file deal.II/doc/license.html for the  text  and
//    further information on this license.
//
//---------------------------------------------------------------------------


#include <deal.II/base/memory_consumption.h>
#include <deal.II/lac/trilinos_vector_base.h>

#ifdef DEAL_II_WITH_TRILINOS

#  include <cmath>
#  include <Epetra_Import.h>


DEAL_II_NAMESPACE_OPEN

namespace TrilinosWrappers
{
  namespace
  {
#ifndef DEAL_II_USE_LARGE_INDEX_TYPE
    // define a helper function that queries the global vector length of an
    // Epetra_FEVector object  by calling either the 32- or 64-bit 
    // function necessary.
    int global_length(const Epetra_FEVector &vector)
    {
      return vector.GlobalLength();
    }
#else
    // define a helper function that queries the global vector length of an
    // Epetra_FEVector object  by calling either the 32- or 64-bit 
    // function necessary.
    long long int global_length(const Epetra_FEVector &vector)
    {
      return vector.GlobalLength64();
    }
#endif
  }

  namespace internal
  {
    VectorReference::operator TrilinosScalar () const
    {
      Assert (index < vector.size(),
              ExcIndexRange (index, 0, vector.size()));

      // Trilinos allows for vectors
      // to be referenced by the [] or
      // () operators but only ()
      // checks index bounds. We check
      // these bounds by ourselves, so
      // we can use []. Note that we
      // can only get local values.

      const TrilinosWrappers::types::int_type local_index = 
        vector.vector->Map().LID(static_cast<TrilinosWrappers::types::int_type>(index));
      Assert (local_index >= 0,
              ExcAccessToNonLocalElement (index,
                                          vector.vector->Map().MinMyGID(),
                                          vector.vector->Map().MaxMyGID()));


      return (*(vector.vector))[0][local_index];
    }
  }



  VectorBase::VectorBase ()
    :
    last_action (Zero),
    compressed  (true),
    has_ghosts  (false),
#ifdef DEAL_II_WITH_MPI
    vector(new Epetra_FEVector(
             Epetra_Map(0,0,Epetra_MpiComm(MPI_COMM_SELF))))
#else
    vector(new Epetra_FEVector(
             Epetra_Map(0,0,Epetra_SerialComm())))
#endif
  {}



  VectorBase::VectorBase (const VectorBase &v)
    :
    Subscriptor(),
    last_action (Zero),
    compressed (true),
    has_ghosts  (v.has_ghosts),
    vector(new Epetra_FEVector(*v.vector))
  {}



  VectorBase::~VectorBase ()
  {}



  void
  VectorBase::clear ()
  {
    // When we clear the vector,
    // reset the pointer and generate
    // an empty vector.
#ifdef DEAL_II_WITH_MPI
    Epetra_Map map (0, 0, Epetra_MpiComm(MPI_COMM_SELF));
#else
    Epetra_Map map (0, 0, Epetra_SerialComm());
#endif

    has_ghosts = false;
    vector.reset (new Epetra_FEVector(map));
    last_action = Zero;
  }



  VectorBase &
  VectorBase::operator = (const VectorBase &v)
  {
    Assert (vector.get() != 0,
            ExcMessage("Vector is not constructed properly."));

    if (local_range() != v.local_range())
      {
        last_action = Zero;
        vector.reset (new Epetra_FEVector(*v.vector));
        has_ghosts = v.has_ghosts;
      }
    else
      {
        Assert (vector->Map().SameAs(v.vector->Map()) == true,
                ExcMessage ("The Epetra maps in the assignment operator ="
                            " do not match, even though the local_range "
                            " seems to be the same. Check vector setup!"));
        int ierr;
        ierr = vector->GlobalAssemble(last_action);
        AssertThrow (ierr == 0, ExcTrilinosError(ierr));

        ierr = vector->Update(1.0, *v.vector, 0.0);
        AssertThrow (ierr == 0, ExcTrilinosError(ierr));

        last_action = Zero;
      }

    return *this;
  }



  template <typename number>
  VectorBase &
  VectorBase::operator = (const ::dealii::Vector<number> &v)
  {
    Assert (size() == v.size(),
            ExcDimensionMismatch(size(), v.size()));

    // this is probably not very efficient
    // but works. in particular, we could do
    // better if we know that
    // number==TrilinosScalar because then we
    // could elide the copying of elements
    //
    // let's hope this isn't a
    // particularly frequent operation
    std::pair<size_type, size_type>
    local_range = this->local_range ();
    for (size_type i=local_range.first; i<local_range.second; ++i)
      (*vector)[0][i-local_range.first] = v(i);

    return *this;
  }



  TrilinosScalar
  VectorBase::el (const size_type index) const
  {
    // Extract local indices in
    // the vector.
    TrilinosWrappers::types::int_type trilinos_i = 
      vector->Map().LID(static_cast<TrilinosWrappers::types::int_type>(index));
    TrilinosScalar value = 0.;

    // If the element is not
    // present on the current
    // processor, we can't
    // continue. Just print out 0.

    // TODO: Is this reasonable?
    if (trilinos_i == -1 )
      {
        return 0.;
        //Assert (false, ExcAccessToNonlocalElement(index, local_range().first,
        //                                local_range().second-1));
      }
    else
      value = (*vector)[0][trilinos_i];

    return value;
  }



  TrilinosScalar
  VectorBase::operator () (const size_type index) const
  {
    // Extract local indices in
    // the vector.
    TrilinosWrappers::types::int_type trilinos_i = 
      vector->Map().LID(static_cast<TrilinosWrappers::types::int_type>(index));
    TrilinosScalar value = 0.;

    // If the element is not present
    // on the current processor, we
    // can't continue. This is the
    // main difference to the el()
    // function.
    if (trilinos_i == -1 )
      {
        Assert (false, ExcAccessToNonlocalElement(index, local_range().first,
                                                  local_range().second-1));
      }
    else
      value = (*vector)[0][trilinos_i];

    return value;
  }



  void
  VectorBase::add (const VectorBase &v,
                   const bool        allow_different_maps)
  {
    if (allow_different_maps == false)
      *this += v;
    else
      {
        AssertThrow (size() == v.size(),
                     ExcDimensionMismatch (size(), v.size()));

        Epetra_Import data_exchange (vector->Map(), v.vector->Map());

        int ierr = vector->Import(*v.vector, data_exchange, Add);
        AssertThrow (ierr == 0, ExcTrilinosError(ierr));

        last_action = Insert;
      }
  }



  bool
  VectorBase::operator == (const VectorBase &v) const
  {
    Assert (size() == v.size(),
            ExcDimensionMismatch(size(), v.size()));
    if (local_size() != v.local_size())
      return false;

    size_type i;
    for (i=0; i<local_size(); i++)
      if ((*(v.vector))[0][i]!=(*vector)[0][i]) return false;

    return true;
  }



  bool
  VectorBase::operator != (const VectorBase &v) const
  {
    Assert (size() == v.size(),
            ExcDimensionMismatch(size(), v.size()));

    return (!(*this==v));
  }



  bool
  VectorBase::all_zero () const
  {
    // get a representation of the vector and
    // loop over all the elements
    TrilinosScalar *start_ptr = (*vector)[0];
    const TrilinosScalar *ptr  = start_ptr,
                          *eptr = start_ptr + local_size();
    unsigned int flag = 0;
    while (ptr != eptr)
      {
        if (*ptr != 0)
          {
            flag = 1;
            break;
          }
        ++ptr;
      }

#ifdef DEAL_II_WITH_MPI
    // in parallel, check that the vector
    // is zero on _all_ processors.
    const Epetra_MpiComm *mpi_comm
      = dynamic_cast<const Epetra_MpiComm *>(&vector->Map().Comm());
    unsigned int num_nonzero = Utilities::MPI::sum(flag, mpi_comm->Comm());
    return num_nonzero == 0;
#else
    return flag == 0;
#endif

  }



  bool
  VectorBase::is_non_negative () const
  {
#ifdef DEAL_II_WITH_MPI
    // if this vector is a parallel one, then
    // we need to communicate to determine
    // the answer to the current
    // function. this still has to be
    // implemented
    AssertThrow(local_size() == size(), ExcNotImplemented());
#endif
    // get a representation of the vector and
    // loop over all the elements
    TrilinosScalar *start_ptr;
    int leading_dimension;
    int ierr = vector->ExtractView (&start_ptr, &leading_dimension);
    AssertThrow (ierr == 0, ExcTrilinosError(ierr));

    // TODO: This
    // won't work in parallel like
    // this. Find out a better way to
    // this in that case.
    const TrilinosScalar *ptr  = start_ptr,
                          *eptr = start_ptr + size();
    bool flag = true;
    while (ptr != eptr)
      {
        if (*ptr < 0.0)
          {
            flag = false;
            break;
          }
        ++ptr;
      }

    return flag;
  }



  // TODO: up to now only local
  // data printed out! Find a
  // way to neatly output
  // distributed data...
  void
  VectorBase::print (const char *format) const
  {
    Assert (global_length(*vector)!=0, ExcEmptyObject());

    for (size_type j=0; j<size(); ++j)
      {
        double t = (*vector)[0][j];

        if (format != 0)
          std::printf (format, t);
        else
          std::printf (" %5.2f", double(t));
      }
    std::printf ("\n");
  }



  void
  VectorBase::print (std::ostream      &out,
                     const unsigned int precision,
                     const bool         scientific,
                     const bool         across) const
  {
    AssertThrow (out, ExcIO());

    // get a representation of the
    // vector and loop over all
    // the elements TODO: up to
    // now only local data printed
    // out! Find a way to neatly
    // output distributed data...
    TrilinosScalar *val;
    int leading_dimension;
    int ierr = vector->ExtractView (&val, &leading_dimension);

    AssertThrow (ierr == 0, ExcTrilinosError(ierr));
    out.precision (precision);
    if (scientific)
      out.setf (std::ios::scientific, std::ios::floatfield);
    else
      out.setf (std::ios::fixed, std::ios::floatfield);

    if (across)
      for (size_type i=0; i<size(); ++i)
        out << static_cast<double>(val[i]) << ' ';
    else
      for (size_type i=0; i<size(); ++i)
        out << static_cast<double>(val[i]) << std::endl;
    out << std::endl;

    // restore the representation
    // of the vector
    AssertThrow (out, ExcIO());
  }



  void
  VectorBase::swap (VectorBase &v)
  {
    std::swap(last_action, v.last_action);
    std::swap(compressed, v.compressed);
    std::swap(vector, v.vector);
  }



  std::size_t
  VectorBase::memory_consumption () const
  {
    //TODO[TH]: No accurate memory
    //consumption for Trilinos vectors
    //yet. This is a rough approximation with
    //one index and the value per local
    //entry.
    return sizeof(*this)
           + this->local_size()*( sizeof(double)+
               sizeof(TrilinosWrappers::types::int_type) );
  }

  void
  VectorBase::update_ghost_values() const
  {
  }

} /* end of namespace TrilinosWrappers */


namespace TrilinosWrappers
{
#include "trilinos_vector_base.inst"
}

DEAL_II_NAMESPACE_CLOSE

#endif // DEAL_II_WITH_TRILINOS
