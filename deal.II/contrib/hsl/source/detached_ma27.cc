//----------------------------  detached_ma27.cc  ---------------------------
//    $Id$
//    Version: $Name$
//
//    Copyright (C) 2002 by the deal.II authors
//
//    This file is subject to QPL and may not be  distributed
//    without copyright and license information. Please refer
//    to the file deal.II/doc/license.html for the  text  and
//    further information on this license.
//
//----------------------------  detached_ma27.cc  ---------------------------

#include <cstdio>
#include <unistd.h>
#include <hsl/hsl.h>
#include <vector>
#include <iostream>


template <typename T>
void put (const T *t, const size_t N, const char */*debug_info*/)
{
  write (1,
         reinterpret_cast<const char *> (t),
         sizeof(T) * N);
  fflush (NULL);
};


template <typename T>
void get (T *t, const size_t N, const char */*debug_info*/)
{
  unsigned int count = 0;
  while (count < sizeof(T)*N)
    {
      int ret = read (0,
                      reinterpret_cast<char *> (t) + count,
                      sizeof(T) * N - count);
      if (ret < 0)
        {
          std::cerr << "------ error " << ret << " on client side!"
                    << std::endl;
          abort ();
        }
      else
        count += ret;
    };
};



int main () 
{
  unsigned int N, NZ, NSTEPS, LA, MAXFRT, LIW;
  int IFLAG;
  std::vector<unsigned int> IRN, ICN, IW, IKEEP, IW1;
  std::vector<double> A;

  while (true)
    {
      char action;
      get(&action, 1, "ACTION");
      
      switch (action)
        {
          case '1':
          {
            get (&N, 1, "N");
            get (&NZ, 1, "NZ");

            IRN.resize (NZ);
            ICN.resize (NZ);
            
            get (&IRN[0], NZ, "IRN");
            get (&ICN[0], NZ, "ICN");
            get (&LIW, 1, "LIW");
            get (&IFLAG, 1, "IFLAG");

            IW.resize (LIW);
            IKEEP.resize (3*N);
            IW1.resize (2*N);
            
                                             // next call function
            HSL::MA27::ma27ad_ (&N, &NZ, &IRN[0], &ICN[0], &IW[0], &LIW,
                                &IKEEP[0], &IW1[0], &NSTEPS, &IFLAG);

                                             // then return IFLAG
            put (&IFLAG, 1, "IFLAG");

            sleep (1);
            break;
          };

          case '2':
          {
            get (&LA, 1, "LA");
            A.resize (LA);
            get (&A[0], LA, "A");
            
            HSL::MA27::ma27bd_ (&N, &NZ, &IRN[0], &ICN[0], &A[0], &LA,
                                &IW[0], &LIW, &IKEEP[0],
                                &NSTEPS, &MAXFRT, &IW1[0], &IFLAG);

                                             // if IFLAG==0, then the
                                             // call succeeded and we
                                             // won't need ICN, IRN,
                                             // and IKEEP any more
            if (IFLAG==0)
              {
                std::vector<unsigned int> tmp1, tmp2, tmp3;
                ICN.swap (tmp1);
                IRN.swap (tmp2);
                IKEEP.swap (tmp3);
              };
            
                                             // finally return IFLAG
            put (&IFLAG, 1, "IFLAG");

            break;
          };
          

          case '3':
          {
            std::vector<double> W(MAXFRT);
            std::vector<double> rhs(N);
            get (&rhs[0], N, "RHS");

            HSL::MA27::ma27cd_ (&N, &A[0], &LA, &IW[0],
                                &LIW, &W[0], &MAXFRT, &rhs[0],
                                &IW1[0], &NSTEPS);

            put (&rhs[0], N, "RHS");
            
            break;
          };
          
          
          case '4':
          {
            unsigned int NRLNEC;
            HSL::MA27::ma27x1_ (&NRLNEC);
            put (&NRLNEC, 1, "NRLNEC");
            break;
          };

          case '5':
          {
            unsigned int NIRNEC;
            HSL::MA27::ma27x2_ (&NIRNEC);
            put (&NIRNEC, 1, "NIRNEC");
            break;
          };

          case '6':
          {
            unsigned int LP;
            get (&LP, 1, "LP");
            HSL::MA27::ma27x3_ (&LP);
            break;
          };

          case '7':
          {
            exit (0);
            break;
          };
          
          default:
          {
            std::cerr << "Invalid action key ('" << action
                      << "'=" << static_cast<unsigned int>(action) << ")!!"
                      << std::endl;
            abort();
          };
        };
    };
};

                
