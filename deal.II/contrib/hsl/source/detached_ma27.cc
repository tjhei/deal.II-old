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

#include <base/config.h>
#include <hsl/hsl.h>

#include <vector>
#include <iostream>
#include <cstdio>
#include <cstdlib>

#include <unistd.h>
#include <pthread.h>
#include <signal.h>

#ifdef HAVE_STD_STRINGSTREAM
#  include <sstream>
#else
#  include <strstream>
#endif

#ifndef DEAL_II_USE_DIRECT_ERRNO_H
#  include <errno.h>
#else
#  include </usr/include/errno.h>
#endif

#include <sys/errno.h>


pid_t master_pid;


extern "C"
void * monitor_thread (void *) 
{
                                   // loop and check every once in a
                                   // while whether the mother process
                                   // is still existing or has died
                                   // without giving us due notice. if
                                   // the latter is the case, then
                                   // also exit this process
                                   //
                                   // check by calling "ps -p PID",
                                   // where PID is the pid of the
                                   // parent. if the return value is
                                   // non-null, then ps couldn't find
                                   // out about the parent process, so
                                   // it is apparently gone
#ifdef HAVE_STD_STRINGSTREAM
  std::ostringstream s;
  s << "ps -p " << master_pid;
  const char * const command = s.str().c_str();
#else
  std::ostrstream s;
  s << "ps -p " << master_pid << std::ends;
  const char * const command = s.str();
#endif
  
  while (true)
    {
      int ret = std::system (command);
      if (ret < 0)
        {
          std::cerr << "---- detached_ma27: Monitor process couldn't start 'ps'!"
                    << std::endl;
          std::abort ();
        }
      else
        if (ret != 0)
          {
            std::cerr << "---- detached_ma27: Master process seems to have died!"
                      << std::endl;
            std::abort ();
          };

                                       // ok, master still running,
                                       // take a little rest and then
                                       // ask again
      sleep (20);
    };
};



template <typename T>
void put (const T *t, const size_t N, const char */*debug_info*/)
{
  try_write:
  int ret = write (1,
                   reinterpret_cast<const char *> (t),
                   sizeof(T) * N);
                                   // if write call was
                                   // interrupted, just
                                   // retry
  if ((ret<0) && (errno==EINTR))
    goto try_write;
  
  fflush (NULL);
};



template <typename T>
void get (T *t, const size_t N, const char */*debug_info*/)
{
  unsigned int count = 0;
  while (count < sizeof(T)*N)
    {
      try_read:
      int ret = read (0,
                      reinterpret_cast<char *> (t) + count,
                      sizeof(T) * N - count);
                                       // if read call was
                                       // interrupted, just
                                       // retry
      if ((ret<0) && (errno==EINTR))
        goto try_read;
      
      if (ret < 0)
        {
          std::cerr << "------ error " << ret << " on client side!"
                    << " errno=" << errno
                    << std::endl;
          abort ();
        }
      else
        count += ret;
    };
};



int main () 
{
                                   // first action is to get the pid
                                   // of the master process, so that
                                   // we can check whether it is still
                                   // alive or not...
  get (&master_pid, 1, "master_pid");
                                   // ...and start off a thread that
                                   // actually checks that
  pthread_t monitor_thread_id;
  pthread_create (&monitor_thread_id, 0, &monitor_thread, 0);
  
                                   // then go into the action loop...
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
                                             // ok, this is the stop
                                             // signal. for this, kill
                                             // the monitor thread,
                                             // and exit gracefully
            pthread_kill (monitor_thread_id, SIGKILL);
            exit (0);
            break;
          };
          
          default:
          {
            std::cerr << "------ error " << ret << " on client side!"
                      << " Invalid action key ('" << action
                      << "'=" << static_cast<unsigned int>(action) << ")!"
                      << std::endl;
            abort();
          };
        };
    };
};

                
