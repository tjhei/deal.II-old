//---------------------------------------------------------------------------
//    $Id$
//    Version: $Name$
//
//    Copyright (C) 1999, 2000, 2001, 2002, 2003, 2004, 2005, 2006 by the deal.II authors
//
//    This file is subject to QPL and may not be  distributed
//    without copyright and license information. Please refer
//    to the file deal.II/doc/license.html for the  text  and
//    further information on this license.
//
//---------------------------------------------------------------------------


/*
 * Single out some functions which are needed by all dimensions, but
 * which are not template. They thus have the same name and when we
 * try to link with the libraries for different dimensions at the same
 * time, we get linker errors for functions defined more than once. By
 * putting these functions in a single file, the linker is allowed to
 * use it only once and throw away all other versions of this file in
 * the other libraries.
 */


#include <base/memory_consumption.h>
#include <grid/tria.h>
#include <grid/tria_levels.h>
#include <grid/tria_boundary.h>

#include <algorithm>
#include <functional>
#include <numeric>



bool
SubCellData::check_consistency (const unsigned int dim) const
{
  switch (dim) 
    {
      case 1:
	    return ((boundary_lines.size() == 0) &&
		    (boundary_quads.size() == 0));
      case 2:
	    return (boundary_quads.size() == 0);
    };
  return true;
}


namespace internal
{
  namespace Triangulation
  {
    
    void
    TriaLevel<0>::reserve_space (const unsigned int total_cells,
                                          const unsigned int dimension)
    {
                                       // we need space for total_cells
                                       // cells. Maybe we have more already
                                       // with those cells which are unused,
                                       // so only allocate new space if needed.
                                       //
                                       // note that all arrays should have equal
                                       // sizes (checked by @p{monitor_memory}
      if (total_cells > refine_flags.size()) 
        {
          refine_flags.reserve (total_cells);
          refine_flags.insert (refine_flags.end(),
                               total_cells - refine_flags.size(),
                               false);
      
          coarsen_flags.reserve (total_cells);
          coarsen_flags.insert (coarsen_flags.end(),
                                total_cells - coarsen_flags.size(),
                                false);
      
          subdomain_ids.reserve (total_cells);
          subdomain_ids.insert (subdomain_ids.end(),
                                total_cells - subdomain_ids.size(),
                                0);
      
          neighbors.reserve (total_cells*(2*dimension));
          neighbors.insert (neighbors.end(),
                            total_cells*(2*dimension) - neighbors.size(),
                            std::make_pair(-1,-1));
        };
    }



    void
    TriaLevel<0>::monitor_memory (const unsigned int true_dimension) const
    {
                                       // check that we have not allocated
                                       // too much memory. note that bool
                                       // vectors allocate their memory in
                                       // chunks of whole integers, so
                                       // they may over-allocate by up to
                                       // as many elements as an integer
                                       // has bits
      Assert (refine_flags.size() <= refine_flags.capacity() + sizeof(int)*8 ||
              refine_flags.size()<DEAL_II_MIN_BOOL_VECTOR_CAPACITY,
              ExcMemoryWasted ("refine_flags",
                               refine_flags.size(), refine_flags.capacity()));
      Assert (coarsen_flags.size() <= coarsen_flags.capacity() + sizeof(int)*8 ||
              coarsen_flags.size()<DEAL_II_MIN_BOOL_VECTOR_CAPACITY,
              ExcMemoryWasted ("coarsen_flags",
                               coarsen_flags.size(), coarsen_flags.capacity()));
      Assert (neighbors.size() ==  neighbors.capacity() ||
              neighbors.size()<DEAL_II_MIN_VECTOR_CAPACITY,
              ExcMemoryWasted ("neighbors",
                               neighbors.size(), neighbors.capacity()));
      Assert (subdomain_ids.size() ==  subdomain_ids.capacity() ||
              subdomain_ids.size()<DEAL_II_MIN_VECTOR_CAPACITY,
              ExcMemoryWasted ("subdomain_ids",
                               subdomain_ids.size(), subdomain_ids.capacity()));
      Assert (2*true_dimension*refine_flags.size() == neighbors.size(),
              ExcMemoryInexact (refine_flags.size(), neighbors.size()));
      Assert (2*true_dimension*coarsen_flags.size() == neighbors.size(),
              ExcMemoryInexact (coarsen_flags.size(), neighbors.size()));
    }



    unsigned int
    TriaLevel<0>::memory_consumption () const
    {
      return (MemoryConsumption::memory_consumption (refine_flags) +
              MemoryConsumption::memory_consumption (coarsen_flags) +
              MemoryConsumption::memory_consumption (neighbors));
    }



    void
    TriaLevel<1>::reserve_space (const unsigned int new_lines)
    {
      const unsigned int new_size = new_lines +
                                    std::count_if (lines.used.begin(),
                                                   lines.used.end(),
                                                   std::bind2nd (std::equal_to<bool>(), true));

                                       // same as in @p{reserve_space<0>}: only
                                       // allocate space if necessary
      if (new_size>lines.lines.size()) 
        {
          lines.lines.reserve (new_size);
          lines.lines.insert (lines.lines.end(), new_size-lines.lines.size(), Line());
  
          lines.used.reserve (new_size);
          lines.used.insert (lines.used.end(), new_size-lines.used.size(), false);
  
          lines.user_flags.reserve (new_size);
          lines.user_flags.insert (lines.user_flags.end(),
                                   new_size-lines.user_flags.size(), false);
      
          lines.children.reserve (new_size);
          lines.children.insert (lines.children.end(), new_size-lines.children.size(),
                                 -1);

          lines.material_id.reserve (new_size);
          lines.material_id.insert (lines.material_id.end(),
                                    new_size-lines.material_id.size(),
                                    255);

          lines.user_pointers.reserve (new_size);
          lines.user_pointers.insert (lines.user_pointers.end(),
                                      new_size-lines.user_pointers.size(), 0);
        };
    }



    void
    TriaLevel<1>::monitor_memory (const unsigned int true_dimension) const
    {
                                       // check that we have not allocated
                                       // too much memory. note that bool
                                       // vectors allocate their memory in
                                       // chunks of whole integers, so
                                       // they may over-allocate by up to
                                       // as many elements as an integer
                                       // has bits
      Assert (lines.lines.size() == lines.lines.capacity() ||
              lines.lines.size()<DEAL_II_MIN_VECTOR_CAPACITY,
              ExcMemoryWasted ("lines",
                               lines.lines.size(), lines.lines.capacity()));
      Assert (lines.children.size() == lines.children.capacity() ||
              lines.children.size()<DEAL_II_MIN_VECTOR_CAPACITY,
              ExcMemoryWasted ("children",
                               lines.children.size(), lines.children.capacity()));
      Assert (lines.used.size() <= lines.used.capacity() + sizeof(int)*8 ||
              lines.used.size()<DEAL_II_MIN_BOOL_VECTOR_CAPACITY,
              ExcMemoryWasted ("used",
                               lines.used.size(), lines.used.capacity()));
      Assert (lines.user_flags.size() <= lines.user_flags.capacity() + sizeof(int)*8 ||
              lines.user_flags.size()<DEAL_II_MIN_BOOL_VECTOR_CAPACITY,
              ExcMemoryWasted ("user_flags",
                               lines.user_flags.size(), lines.user_flags.capacity()));
      Assert (lines.lines.size() == lines.used.size(),
              ExcMemoryInexact (lines.lines.size(), lines.used.size()));
      Assert (lines.lines.size() == lines.user_flags.size(),
              ExcMemoryInexact (lines.lines.size(), lines.user_flags.size()));
      Assert (lines.lines.size() == lines.children.size(),
              ExcMemoryInexact (lines.lines.size(), lines.children.size()));
      Assert (lines.lines.size() == lines.material_id.size(),
              ExcMemoryInexact (lines.lines.size(), lines.material_id.size()));
      Assert (lines.lines.size() == lines.user_pointers.size(),
              ExcMemoryInexact (lines.lines.size(), lines.user_pointers.size()));

      TriaLevel<0>::monitor_memory (true_dimension);
    }



    unsigned int
    TriaLevel<1>::memory_consumption () const
    {
      return (TriaLevel<0>::memory_consumption() +
              MemoryConsumption::memory_consumption (lines.lines) +
              MemoryConsumption::memory_consumption (lines.children) +
              MemoryConsumption::memory_consumption (lines.used) +
              MemoryConsumption::memory_consumption (lines.user_flags) +
              MemoryConsumption::memory_consumption (lines.material_id) +
              MemoryConsumption::memory_consumption (lines.user_pointers));
    }
  


    void
    TriaLevel<2>::reserve_space (const unsigned int new_quads)
    {
      const unsigned int new_size = new_quads +
                                    std::count_if (quads.used.begin(),
                                                   quads.used.end(),
                                                   std::bind2nd (std::equal_to<bool>(), true));

                                       // see above...
      if (new_size>quads.quads.size())
        {
          quads.quads.reserve (new_size);
          quads.quads.insert (quads.quads.end(), new_size-quads.quads.size(), Quad());
      
          quads.used.reserve (new_size);
          quads.used.insert (quads.used.end(), new_size-quads.used.size(), false);
  
          quads.user_flags.reserve (new_size);
          quads.user_flags.insert (quads.user_flags.end(),
                                   new_size-quads.user_flags.size(), false);
  
          quads.children.reserve (new_size);
          quads.children.insert (quads.children.end(), new_size-quads.children.size(),
                                 -1);

          quads.material_id.reserve (new_size);
          quads.material_id.insert (quads.material_id.end(),
                                    new_size-quads.material_id.size(),
                                    255);

          quads.user_pointers.reserve (new_size);
          quads.user_pointers.insert (quads.user_pointers.end(),
                                      new_size-quads.user_pointers.size(), 0);
        };
    }



    void
    TriaLevel<2>::monitor_memory (const unsigned int true_dimension) const
    {
                                       // check that we have not allocated
                                       // too much memory. note that bool
                                       // vectors allocate their memory in
                                       // chunks of whole integers, so
                                       // they may over-allocate by up to
                                       // as many elements as an integer
                                       // has bits
      Assert (quads.quads.size() == quads.quads.capacity() ||
              quads.quads.size()<DEAL_II_MIN_VECTOR_CAPACITY,
              ExcMemoryWasted ("quads",
                               quads.quads.size(), quads.quads.capacity()));
      Assert (quads.children.size() == quads.children.capacity() ||
              quads.children.size()<DEAL_II_MIN_VECTOR_CAPACITY,
              ExcMemoryWasted ("children",
                               quads.children.size(), quads.children.capacity()));
      Assert (quads.used.size() <= quads.used.capacity() + sizeof(int)*8 ||
              quads.used.size()<DEAL_II_MIN_BOOL_VECTOR_CAPACITY,
              ExcMemoryWasted ("used",
                               quads.used.size(), quads.used.capacity()));
      Assert (quads.user_flags.size() <= quads.user_flags.capacity() + sizeof(int)*8 ||
              quads.user_flags.size()<DEAL_II_MIN_BOOL_VECTOR_CAPACITY,
              ExcMemoryWasted ("user_flags",
                               quads.user_flags.size(), quads.user_flags.capacity()));
      Assert (quads.quads.size() == quads.used.size(),
              ExcMemoryInexact (quads.quads.size(), quads.used.size()));
      Assert (quads.quads.size() == quads.user_flags.size(),
              ExcMemoryInexact (quads.quads.size(), quads.user_flags.size()));
      Assert (quads.quads.size() == quads.children.size(),
              ExcMemoryInexact (quads.quads.size(), quads.children.size()));
      Assert (quads.quads.size() == quads.material_id.size(),
              ExcMemoryInexact (quads.quads.size(), quads.material_id.size()));
      Assert (quads.quads.size() == quads.user_pointers.size(),
              ExcMemoryInexact (quads.quads.size(), quads.user_pointers.size()));

      TriaLevel<1>::monitor_memory (true_dimension);
    }



    unsigned int
    TriaLevel<2>::memory_consumption () const
    {
      return (TriaLevel<1>::memory_consumption() +
              MemoryConsumption::memory_consumption (quads.quads) +
              MemoryConsumption::memory_consumption (quads.children) +
              MemoryConsumption::memory_consumption (quads.used) +
              MemoryConsumption::memory_consumption (quads.user_flags) +
              MemoryConsumption::memory_consumption (quads.material_id) +
              MemoryConsumption::memory_consumption (quads.user_pointers));
    }



    void
    TriaLevel<3>::reserve_space (const unsigned int new_hexes)
    {
      const unsigned int new_size = new_hexes +
                                    std::count_if (hexes.used.begin(),
                                                   hexes.used.end(),
                                                   std::bind2nd (std::equal_to<bool>(), true));

                                       // see above...
      if (new_size>hexes.hexes.size())
        {
          hexes.hexes.reserve (new_size);
          hexes.hexes.insert (hexes.hexes.end(), new_size-hexes.hexes.size(), Hexahedron());
      
          hexes.used.reserve (new_size);
          hexes.used.insert (hexes.used.end(), new_size-hexes.used.size(), false);
  
          hexes.user_flags.reserve (new_size);
          hexes.user_flags.insert (hexes.user_flags.end(),
                                   new_size-hexes.user_flags.size(), false);
  
          hexes.children.reserve (new_size);
          hexes.children.insert (hexes.children.end(), new_size-hexes.children.size(),
                                 -1);

          hexes.material_id.reserve (new_size);
          hexes.material_id.insert (hexes.material_id.end(),
                                    new_size-hexes.material_id.size(),
                                    255);

          hexes.user_pointers.reserve (new_size);
          hexes.user_pointers.insert (hexes.user_pointers.end(),
                                      new_size-hexes.user_pointers.size(), 0);

          hexes.face_orientations.reserve (new_size * GeometryInfo<3>::faces_per_cell);
          hexes.face_orientations.insert (hexes.face_orientations.end(),
                                          new_size * GeometryInfo<3>::faces_per_cell
                                          - hexes.face_orientations.size(),
                                          true);
        };
    }



    void
    TriaLevel<3>::monitor_memory (const unsigned int true_dimension) const
    {
                                       // check that we have not allocated
                                       // too much memory. note that bool
                                       // vectors allocate their memory in
                                       // chunks of whole integers, so
                                       // they may over-allocate by up to
                                       // as many elements as an integer
                                       // has bits
      Assert (hexes.hexes.size() == hexes.hexes.capacity() ||
              hexes.hexes.size()<DEAL_II_MIN_VECTOR_CAPACITY,
              ExcMemoryWasted ("hexes",
                               hexes.hexes.size(), hexes.hexes.capacity()));
      Assert (hexes.children.size() == hexes.children.capacity() ||
              hexes.children.size()<DEAL_II_MIN_VECTOR_CAPACITY,
              ExcMemoryWasted ("children",
                               hexes.children.size(), hexes.children.capacity()));
      Assert (hexes.used.size() <= hexes.used.capacity() + sizeof(int)*8 ||
              hexes.used.size()<DEAL_II_MIN_BOOL_VECTOR_CAPACITY,
              ExcMemoryWasted ("used",
                               hexes.used.size(), hexes.used.capacity()));
      Assert (hexes.user_flags.size() <= hexes.user_flags.capacity() + sizeof(int)*8 ||
              hexes.user_flags.size()<DEAL_II_MIN_BOOL_VECTOR_CAPACITY,
              ExcMemoryWasted ("user_flags",
                               hexes.user_flags.size(), hexes.user_flags.capacity()));
      Assert (hexes.hexes.size() == hexes.used.size(),
              ExcMemoryInexact (hexes.hexes.size(), hexes.used.size()));
      Assert (hexes.hexes.size() == hexes.user_flags.size(),
              ExcMemoryInexact (hexes.hexes.size(), hexes.user_flags.size()));
      Assert (hexes.hexes.size() == hexes.children.size(),
              ExcMemoryInexact (hexes.hexes.size(), hexes.children.size()));
      Assert (hexes.hexes.size() == hexes.material_id.size(),
              ExcMemoryInexact (hexes.hexes.size(), hexes.material_id.size()));
      Assert (hexes.hexes.size() == hexes.user_pointers.size(),
              ExcMemoryInexact (hexes.hexes.size(), hexes.user_pointers.size()));
      Assert (hexes.hexes.size() * GeometryInfo<3>::faces_per_cell
              == hexes.face_orientations.size(),
              ExcMemoryInexact (hexes.hexes.size() * GeometryInfo<3>::faces_per_cell,
                                hexes.face_orientations.size()));

      TriaLevel<2>::monitor_memory (true_dimension);
    }



    unsigned int
    TriaLevel<3>::memory_consumption () const
    {
      return (TriaLevel<2>::memory_consumption() +
              MemoryConsumption::memory_consumption (hexes.hexes) +
              MemoryConsumption::memory_consumption (hexes.children) +
              MemoryConsumption::memory_consumption (hexes.used) +
              MemoryConsumption::memory_consumption (hexes.user_flags) +
              MemoryConsumption::memory_consumption (hexes.material_id) +
              MemoryConsumption::memory_consumption (hexes.user_pointers) +
              MemoryConsumption::memory_consumption (hexes.face_orientations));
    }



    NumberCache<1>::NumberCache () :
                    n_lines (0),
                    n_active_lines (0) 
                                       // all other fields are
                                       // default constructed
    {}



    unsigned int
    NumberCache<1>::memory_consumption () const
    {
      return (MemoryConsumption::memory_consumption (n_lines) +
              MemoryConsumption::memory_consumption (n_lines_level) +
              MemoryConsumption::memory_consumption (n_active_lines) +
              MemoryConsumption::memory_consumption (n_active_lines_level));
    }


    NumberCache<2>::NumberCache () :
                    n_quads (0),
                    n_active_quads (0) 
                                       // all other fields are
                                       // default constructed
    {}



    unsigned int
    NumberCache<2>::memory_consumption () const
    {
      return (NumberCache<1>::memory_consumption () +
              MemoryConsumption::memory_consumption (n_quads) +
              MemoryConsumption::memory_consumption (n_quads_level) +
              MemoryConsumption::memory_consumption (n_active_quads) +
              MemoryConsumption::memory_consumption (n_active_quads_level));
    }



    NumberCache<3>::NumberCache () :
                    n_hexes (0),
                    n_active_hexes (0) 
                                       // all other fields are
                                       // default constructed
    {}



    unsigned int
    NumberCache<3>::memory_consumption () const
    {
      return (NumberCache<2>::memory_consumption () +
              MemoryConsumption::memory_consumption (n_hexes) +
              MemoryConsumption::memory_consumption (n_hexes_level) +
              MemoryConsumption::memory_consumption (n_active_hexes) +
              MemoryConsumption::memory_consumption (n_active_hexes_level));
    }
  }
}
