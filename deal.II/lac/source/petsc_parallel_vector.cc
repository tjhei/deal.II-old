//---------------------------------------------------------------------------
//    $Id$
//    Version: $Name$
//
//    Copyright (C) 2004 by the deal.II authors
//
//    This file is subject to QPL and may not be  distributed
//    without copyright and license information. Please refer
//    to the file deal.II/doc/license.html for the  text  and
//    further information on this license.
//
//---------------------------------------------------------------------------


#include <lac/petsc_parallel_vector.h>

#include <cmath>

#ifdef DEAL_II_USE_PETSC


namespace PETScWrappers
{


  ParallelVector::ParallelVector ()
  {
                                     // this is an invalid empty vector, so we
                                     // can just as well create a sequential
                                     // one to avoid all the overhead incurred
                                     // by parallelism
    const int n = 0;
    const int ierr
      = VecCreateSeq (PETSC_COMM_SELF, n, &vector);
    AssertThrow (ierr == 0, ExcPETScError(ierr));
  }



  ParallelVector::ParallelVector (const unsigned int n,
                                  const unsigned int local_size,
                                  const MPI_Comm    &communicator)
                  :
                  communicator (communicator)
  {
    ParallelVector::create_vector (n, local_size);
  }

  

  ParallelVector::ParallelVector (const VectorBase &v,
                                  const unsigned int local_size,
                                  const MPI_Comm    &communicator)
                  :
                  communicator (communicator)
  {
    ParallelVector::create_vector (v.size(), local_size);

    VectorBase::operator = (v);
  }

  
  void
  ParallelVector::create_vector (const unsigned int  n,
                                 const unsigned int  local_size)
  {
    Assert (local_size < n, ExcIndexRange (local_size, 0, n));

    const int ierr
      = VecCreateMPI (PETSC_COMM_SELF, local_size, n, &vector);
    AssertThrow (ierr == 0, ExcPETScError(ierr));
  }
}

#else
// On gcc2.95 on Alpha OSF1, the native assembler does not like empty
// files, so provide some dummy code
namespace { void dummy () {} }
#endif // DEAL_II_USE_PETSC
