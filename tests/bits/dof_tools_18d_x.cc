//----------------------------  dof_tools_1d.cc  ---------------------------
//    $Id$
//    Version: $Name$ 
//
//    Copyright (C) 2003, 2004, 2005, 2006, 2007 by the deal.II authors
//
//    This file is subject to QPL and may not be  distributed
//    without copyright and license information. Please refer
//    to the file deal.II/doc/license.html for the  text  and
//    further information on this license.
//
//----------------------------  dof_tools_1d.cc  ---------------------------

#include "../tests.h"
#include "dof_tools_common.cc"
#include <lac/block_sparsity_pattern.h>

// check
//   DoFTools::
//   make_flux_sparsity_pattern (const DoFHandler<dim> &,
//	                         BlockCompressedSetSparsityPattern  &);

std::string output_file_name = "dof_tools_18d_x/output";

                                   // set up masks. choose X-shaped
                                   // masks with full first row and
                                   // column (well, we had to invent
                                   // something)
void
make_masks (const unsigned int n,
            Table<2, DoFTools::Coupling> &m1,
            Table<2, DoFTools::Coupling> &m2)
{
  m1.reinit (n,n);
  m2.reinit (n,n);
  for (unsigned int i=0; i<n; ++i)
    m1(i,0) = m1(0,i) = m2(i,0) = m2(0,i) = m1(i,i) = m2(i,i) = DoFTools::nonzero;
}

  
void
my_check_this (const DoFHandler<1> &) 
{}



template <int dim>
void
my_check_this (const DoFHandler<dim> &dof_handler)
{
                                   // we split up the matrix into
                                   // blocks according to the number
                                   // of dofs in each component. this
                                   // fails if the element is not
                                   // primitive, so skip this test for
                                   // such elements
  if (dof_handler.get_fe().is_primitive() != true)
    return;
  
  Table<2, DoFTools::Coupling> mask_int;
  Table<2, DoFTools::Coupling> mask_ext;
  make_masks (dof_handler.get_fe().n_components(),
              mask_int, mask_ext);
  
                                   // create sparsity pattern
  const unsigned int n_components = dof_handler.get_fe().n_components();
  BlockCompressedSetSparsityPattern sp (n_components,
                                     n_components);
  std::vector<unsigned int> dofs_per_component(n_components);
  DoFTools::count_dofs_per_component (dof_handler,
                                      dofs_per_component);
  for (unsigned int i=0; i<n_components; ++i)
    for (unsigned int j=0; j<n_components; ++j)
      sp.block(i,j).reinit(dofs_per_component[i],
                           dofs_per_component[j]);
  sp.collect_sizes ();
  
  DoFTools::make_flux_sparsity_pattern (dof_handler, sp,
                                        mask_int, mask_ext);
  sp.compress ();
  
                                   // write out 20 lines of this
                                   // pattern (if we write out the
                                   // whole pattern, the output file
                                   // would be in the range of 40 MB)
  for (unsigned int l=0; l<20; ++l)
    {
      const unsigned int line = l*(sp.n_rows()/20);
      std::pair<unsigned int,unsigned int>
        block_row = sp.get_row_indices().global_to_local(line);
      for (unsigned int col=0; col<n_components; ++col)
        {
	  for (CompressedSetSparsityPattern::row_iterator
		 c = sp.block(block_row.first,col).row_begin(block_row.second);
	       c!=sp.block(block_row.first,col).row_end(block_row.second); ++c)
            deallog << *c
                    << " ";
          deallog << std::endl;
        }
    }

                                   // write out some other indicators
  for (unsigned int r=0; r<n_components; ++r)
    for (unsigned int c=0; c<n_components; ++c)
      {
        const CompressedSetSparsityPattern &x = sp.block(r,c);
        deallog << x.bandwidth () << std::endl
                << x.max_entries_per_row () << std::endl
                << x.n_nonzero_elements () << std::endl;
        
        unsigned int hash = 0;
        for (unsigned int l=0; l<x.n_rows(); ++l)
          hash += l*x.row_length(l);
        deallog << hash << std::endl;
      }
}


template <int dim>
void
check_this (const DoFHandler<dim> &dof_handler)
{
				   // since we can't forward declare
				   // check_this in this file (it is forward
				   // declared in dof_tools_common.cc), we
				   // also can't make the driver file aware of
				   // the overload for 1d. to avoid linker
				   // errors, we can consequently not overload
				   // check_this, and need this forwarder
				   // function
  my_check_this (dof_handler);
}
