// $Id$
// Copyright Guido Kanschat, Universitaet Heidelberg, 1999

#include <numerics/multigrid.h>
#include <lac/dvector.h>

void
MultiGrid::vmult(dVector& dst, const dVector& src) const
{
  dst = 0.;
  
  copy_to_mg(s,src);
  
  for (unsigned l=0;l<maxlevel;++l)
  {
    level_active_vmult(l,d[l],s[l]);
  }
  copy_from_mg(dst,d);
}


void
MultiGrid::precondition(dVector& dst, const dVector& src) const
{
  copy_to_mg(s,src);
  copy_to_mg(d,dst);
  level_mgstep(maxlevel);
}

void
MultiGrid::level_mgstep(unsigned level) const
{
  if (level == minlevel)
  {
    coarse_grid_solution(level, d[level], s[level]);
    return;
  }
  
  for (unsigned i=0; i< n_presmooth; ++i)
  {
    smooth(level, d[level], s[level]);
  }
}

