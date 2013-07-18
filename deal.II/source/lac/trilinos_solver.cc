// ---------------------------------------------------------------------
// $Id$
//
// Copyright (C) 2008 - 2013 by the deal.II authors
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

#include <deal.II/lac/trilinos_solver.h>

#ifdef DEAL_II_WITH_TRILINOS

#  include <deal.II/base/conditional_ostream.h>
#  include <deal.II/lac/trilinos_sparse_matrix.h>
#  include <deal.II/lac/trilinos_vector_base.h>
#  include <deal.II/lac/trilinos_precondition.h>

#  include <cmath>

DEAL_II_NAMESPACE_OPEN

namespace TrilinosWrappers
{

  SolverBase::AdditionalData::AdditionalData (const bool         output_solver_details,
                                              const unsigned int gmres_restart_parameter)
    :
    output_solver_details (output_solver_details),
    gmres_restart_parameter (gmres_restart_parameter)
  {}



  SolverBase::SolverBase (SolverControl  &cn)
    :
    solver_name    (gmres),
    solver_control (cn)
  {}



  SolverBase::SolverBase (const enum SolverBase::SolverName  solver_name,
                          SolverControl                     &cn)
    :
    solver_name    (solver_name),
    solver_control (cn)
  {}



  SolverBase::~SolverBase ()
  {}



  SolverControl &
  SolverBase::control() const
  {
    return solver_control;
  }



  void
  SolverBase::solve (const SparseMatrix     &A,
                     VectorBase             &x,
                     const VectorBase       &b,
                     const PreconditionBase &preconditioner)
  {
    linear_problem.reset();

    // We need an
    // Epetra_LinearProblem object
    // to let the AztecOO solver
    // know about the matrix and
    // vectors.
    linear_problem.reset
    (new Epetra_LinearProblem(const_cast<Epetra_CrsMatrix *>(&A.trilinos_matrix()),
                              &x.trilinos_vector(),
                              const_cast<Epetra_MultiVector *>(&b.trilinos_vector())));

    execute_solve(preconditioner);
  }



  void
  SolverBase::solve (Epetra_Operator        &A,
                     VectorBase             &x,
                     const VectorBase       &b,
                     const PreconditionBase &preconditioner)
  {
    linear_problem.reset();

    // We need an
    // Epetra_LinearProblem object
    // to let the AztecOO solver
    // know about the matrix and
    // vectors.
    linear_problem.reset
    (new Epetra_LinearProblem(&A,
                              &x.trilinos_vector(),
                              const_cast<Epetra_MultiVector *>(&b.trilinos_vector())));

    execute_solve(preconditioner);
  }



  void
  SolverBase::solve (const SparseMatrix           &A,
                     dealii::Vector<double>       &x,
                     const dealii::Vector<double> &b,
                     const PreconditionBase       &preconditioner)
  {
    linear_problem.reset();

    // In case we call the solver with
    // deal.II vectors, we create views
    // of the vectors in Epetra format.
    Assert (x.size() == A.n(),
            ExcDimensionMismatch(x.size(), A.n()));
    Assert (b.size() == A.m(),
            ExcDimensionMismatch(b.size(), A.m()));
    Assert (A.local_range ().second == A.m(),
            ExcMessage ("Can only work in serial when using deal.II vectors."));
    Assert (A.trilinos_matrix().Filled(),
            ExcMessage ("Matrix is not compressed. Call compress() method."));

    Epetra_Vector ep_x (View, A.domain_partitioner(), x.begin());
    Epetra_Vector ep_b (View, A.range_partitioner(), const_cast<double *>(b.begin()));

    // We need an
    // Epetra_LinearProblem object
    // to let the AztecOO solver
    // know about the matrix and
    // vectors.
    linear_problem.reset (new Epetra_LinearProblem
                          (const_cast<Epetra_CrsMatrix *>(&A.trilinos_matrix()),
                           &ep_x, &ep_b));

    execute_solve(preconditioner);
  }



  void
  SolverBase::solve (Epetra_Operator              &A,
                     dealii::Vector<double>       &x,
                     const dealii::Vector<double> &b,
                     const PreconditionBase       &preconditioner)
  {
    linear_problem.reset();

    Epetra_Vector ep_x (View, A.OperatorDomainMap(), x.begin());
    Epetra_Vector ep_b (View, A.OperatorRangeMap(), const_cast<double *>(b.begin()));

    // We need an
    // Epetra_LinearProblem object
    // to let the AztecOO solver
    // know about the matrix and
    // vectors.
    linear_problem.reset (new Epetra_LinearProblem(&A,&ep_x, &ep_b));

    execute_solve(preconditioner);
  }



  void
  SolverBase::execute_solve(const PreconditionBase &preconditioner)
  {
    int ierr;

    // Next we can allocate the
    // AztecOO solver...
    solver.SetProblem(*linear_problem);

    // ... and we can specify the
    // solver to be used.
    switch (solver_name)
      {
      case cg:
        solver.SetAztecOption(AZ_solver, AZ_cg);
        break;
      case cgs:
        solver.SetAztecOption(AZ_solver, AZ_cgs);
        break;
      case gmres:
        solver.SetAztecOption(AZ_solver, AZ_gmres);
        solver.SetAztecOption(AZ_kspace, additional_data.gmres_restart_parameter);
        break;
      case bicgstab:
        solver.SetAztecOption(AZ_solver, AZ_bicgstab);
        break;
      case tfqmr:
        solver.SetAztecOption(AZ_solver, AZ_tfqmr);
        break;
      default:
        Assert (false, ExcNotImplemented());
      }

    // Introduce the
    // preconditioner, ...
    ierr = solver.SetPrecOperator (const_cast<Epetra_Operator *>
                                   (preconditioner.preconditioner.get()));
    AssertThrow (ierr == 0, ExcTrilinosError(ierr));

    // ... set some options, ...
    solver.SetAztecOption (AZ_output, additional_data.output_solver_details ?
                           AZ_all : AZ_none);
    solver.SetAztecOption (AZ_conv, AZ_noscaled);

    // ... and then solve!
    ierr = solver.Iterate (solver_control.max_steps(),
                           solver_control.tolerance());

    // report errors in more detail
    // than just by checking whether
    // the return status is zero or
    // greater. the error strings are
    // taken from the implementation
    // of the AztecOO::Iterate
    // function
    switch (ierr)
      {
      case -1:
        AssertThrow (false, ExcMessage("AztecOO::Iterate error code -1: "
                                       "option not implemented"));
      case -2:
        AssertThrow (false, ExcMessage("AztecOO::Iterate error code -2: "
                                       "numerical breakdown"));
      case -3:
        AssertThrow (false, ExcMessage("AztecOO::Iterate error code -3: "
                                       "loss of precision"));
      case -4:
        AssertThrow (false, ExcMessage("AztecOO::Iterate error code -4: "
                                       "GMRES hessenberg ill-conditioned"));
      default:
        AssertThrow (ierr >= 0, ExcTrilinosError(ierr));
      }

    // Finally, let the deal.II
    // SolverControl object know
    // what has happened. If the
    // solve succeeded, the status
    // of the solver control will
    // turn into
    // SolverControl::success.
    solver_control.check (solver.NumIters(), solver.TrueResidual());

    if (solver_control.last_check() != SolverControl::success)
      throw SolverControl::NoConvergence (solver_control.last_step(),
                                          solver_control.last_value());
  }





  /* ---------------------- SolverCG ------------------------ */

  SolverCG::AdditionalData::
  AdditionalData (const bool output_solver_details)
    :
    output_solver_details (output_solver_details)
  {}



  SolverCG::SolverCG (SolverControl        &cn,
                      const AdditionalData &data)
    :
    SolverBase (cn),
    additional_data (data.output_solver_details)
  {
    solver_name = cg;
  }


  /* ---------------------- SolverGMRES ------------------------ */

  SolverGMRES::AdditionalData::
  AdditionalData (const bool output_solver_details,
                  const unsigned int restart_parameter)
    :
    output_solver_details (output_solver_details),
    restart_parameter (restart_parameter)
  {}



  SolverGMRES::SolverGMRES (SolverControl        &cn,
                            const AdditionalData &data)
    :
    SolverBase (cn),
    additional_data (data.output_solver_details,
                     data.restart_parameter)
  {
    solver_name = gmres;
  }


  /* ---------------------- SolverBicgstab ------------------------ */

  SolverBicgstab::AdditionalData::
  AdditionalData (const bool output_solver_details)
    :
    output_solver_details (output_solver_details)
  {}




  SolverBicgstab::SolverBicgstab (SolverControl        &cn,
                                  const AdditionalData &data)
    :
    SolverBase (cn),
    additional_data (data.output_solver_details)
  {
    solver_name = bicgstab;
  }


  /* ---------------------- SolverCGS ------------------------ */

  SolverCGS::AdditionalData::
  AdditionalData (const bool output_solver_details)
    :
    output_solver_details (output_solver_details)
  {}




  SolverCGS::SolverCGS (SolverControl        &cn,
                        const AdditionalData &data)
    :
    SolverBase (cn),
    additional_data (data.output_solver_details)
  {
    solver_name = cgs;
  }


  /* ---------------------- SolverTFQMR ------------------------ */

  SolverTFQMR::AdditionalData::
  AdditionalData (const bool output_solver_details)
    :
    output_solver_details (output_solver_details)
  {}



  SolverTFQMR::SolverTFQMR (SolverControl        &cn,
                            const AdditionalData &data)
    :
    SolverBase (cn),
    additional_data (data.output_solver_details)
  {
    solver_name = tfqmr;
  }



  /* ---------------------- SolverDirect ------------------------ */

  SolverDirect::AdditionalData::
  AdditionalData (const bool output_solver_details)
    :
    output_solver_details (output_solver_details)
  {}




  SolverDirect::SolverDirect (SolverControl  &cn,
                              const AdditionalData &data)
    :
    solver_control (cn),
    additional_data (data.output_solver_details)
  {}



  SolverDirect::~SolverDirect ()
  {}



  SolverControl &
  SolverDirect::control() const
  {
    return solver_control;
  }



  void
  SolverDirect::solve (const SparseMatrix     &A,
                       VectorBase             &x,
                       const VectorBase       &b)
  {
    // First set whether we want to print
    // the solver information to screen
    // or not.
    ConditionalOStream  verbose_cout (std::cout,
                                      additional_data.output_solver_details);

    linear_problem.reset();
    solver.reset();

    // We need an
    // Epetra_LinearProblem object
    // to let the AztecOO solver
    // know about the matrix and
    // vectors.
    linear_problem.reset
    (new Epetra_LinearProblem(const_cast<Epetra_CrsMatrix *>(&A.trilinos_matrix()),
                              &x.trilinos_vector(),
                              const_cast<Epetra_MultiVector *>(&b.trilinos_vector())));

    // Next we can allocate the
    // AztecOO solver...
    solver.reset (Amesos().Create("Amesos_Klu", *linear_problem));

    verbose_cout << "Starting symbolic factorization" << std::endl;
    solver->SymbolicFactorization();

    verbose_cout << "Starting numeric factorization" << std::endl;
    solver->NumericFactorization();

    verbose_cout << "Starting solve" << std::endl;
    solver->Solve();

    // Finally, let the deal.II
    // SolverControl object know
    // what has happened. If the
    // solve succeeded, the status
    // of the solver control will
    // turn into
    // SolverControl::success.
    solver_control.check (0, 0);

    if (solver_control.last_check() != SolverControl::success)
      throw SolverControl::NoConvergence (solver_control.last_step(),
                                          solver_control.last_value());
  }



  void
  SolverDirect::solve (const SparseMatrix           &A,
                       dealii::Vector<double>       &x,
                       const dealii::Vector<double> &b)
  {
    // First set whether we want to print
    // the solver information to screen
    // or not.
    ConditionalOStream  verbose_cout (std::cout,
                                      additional_data.output_solver_details);

    linear_problem.reset();
    solver.reset();


    // In case we call the solver with
    // deal.II vectors, we create views
    // of the vectors in Epetra format.
    Assert (x.size() == A.n(),
            ExcDimensionMismatch(x.size(), A.n()));
    Assert (b.size() == A.m(),
            ExcDimensionMismatch(b.size(), A.m()));
    Assert (A.local_range ().second == A.m(),
            ExcMessage ("Can only work in serial when using deal.II vectors."));
    Epetra_Vector ep_x (View, A.domain_partitioner(), x.begin());
    Epetra_Vector ep_b (View, A.range_partitioner(), const_cast<double *>(b.begin()));

    // We need an
    // Epetra_LinearProblem object
    // to let the AztecOO solver
    // know about the matrix and
    // vectors.
    linear_problem.reset (new Epetra_LinearProblem
                          (const_cast<Epetra_CrsMatrix *>(&A.trilinos_matrix()),
                           &ep_x, &ep_b));

    // Next we can allocate the
    // AztecOO solver...
    solver.reset (Amesos().Create("Amesos_Klu", *linear_problem));

    verbose_cout << "Starting symbolic factorization" << std::endl;
    solver->SymbolicFactorization();

    verbose_cout << "Starting numeric factorization" << std::endl;
    solver->NumericFactorization();

    verbose_cout << "Starting solve" << std::endl;
    solver->Solve();

    // Finally, let the deal.II
    // SolverControl object know
    // what has happened. If the
    // solve succeeded, the status
    // of the solver control will
    // turn into
    // SolverControl::success.
    solver_control.check (0, 0);

    if (solver_control.last_check() != SolverControl::success)
      throw SolverControl::NoConvergence (solver_control.last_step(),
                                          solver_control.last_value());
  }




}

DEAL_II_NAMESPACE_CLOSE

#endif // DEAL_II_WITH_PETSC
