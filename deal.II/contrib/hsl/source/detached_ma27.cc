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
#include <base/thread_management.h>
#include <hsl/hsl.h>

#include <vector>
#include <list>
#include <iostream>
#include <cstdio>
#include <cstdlib>
#include <typeinfo>

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



namespace CommunicationsLog
{
  enum Direction { put, get };
  
  struct Record 
  {
      Direction             direction;
      const std::type_info* type;
      unsigned int          count;
      unsigned int          scheduled_bytes;
      unsigned int          completed_bytes;
      std::string           description;
  };

  std::list<Record> communication_log;

  
  template <typename T>
  void record_communication (const Direction    direction,
                             const unsigned int count,
                             const unsigned int completed_bytes,
                             const std::string &descr)
  {
    Record record = {direction, &typeid(T), count,
                     sizeof(T)*count, completed_bytes, descr};
    communication_log.push_back (record);
  };


  void list_communication () 
  {
    std::cerr << "------------------------------" << std::endl
              << "Communiction log history:" << std::endl;
    
    for (std::list<Record>::const_iterator i=communication_log.begin();
         i!=communication_log.end(); ++i)
      std::cerr << "-- "
                << (i->direction == put ? "put" : "get")
                << " "
                << i->count << " objects of type "
                << i->type->name()
                << ", " << i->completed_bytes
                << " of " << i->scheduled_bytes
                << " bytes completed, description="
                << i->description
                << std::endl;
    std::cerr << "------------------------------" << std::endl;
  };  
};



/**
 * Output an error message and terminate the program.
 */
void die (const std::string &text)
{
  std::cerr << "----- detached_ma27: " << text
            << std::endl;
  CommunicationsLog::list_communication ();
  abort ();
};


/**
 * Output an error message and terminate the program. Write two error
 * codes.
 */
template <typename T1, typename T2>
void die (const std::string &text, const T1 t1, const T2 t2)
{
  std::cerr << "----- detached_ma27: " << text
            << " code1=" << t1 << ", code2=" << t2
            << std::endl;
  CommunicationsLog::list_communication ();
  abort ();
};



/**
 * loop and check every once in a while whether the mother process is
 * still existing or has died without giving us due notice. if the
 * latter is the case, then also exit this process
 *
 * check by calling "ps -p PID", where PID is the pid of the
 * parent. if the return value is non-null, then ps couldn't find out
 * about the parent process, so it is apparently gone
 */
extern "C"
void monitor_parent_liveness (const pid_t master_pid) 
{
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
        die ("detached_ma27: Monitor process couldn't start 'ps'!");
      else
        if (ret != 0)
          die ("---- detached_ma27: Master process seems to have died!");

                                       // ok, master still running,
                                       // take a little rest and then
                                       // ask again
      sleep (20);
    };
};




/**
 * Put a certain number of objects to the output stream.
 */
template <typename T>
void put (const T *t, const size_t N, const char *debug_info)
{
                                   // repeat writing until syscall is
                                   // not interrupted
  int ret;
  do
    ret = write (1, reinterpret_cast<const char *> (t),
                 sizeof(T) * N);
  while ((ret<0) && (errno==EINTR));
  if (ret < 0)
    die ("error on client side in 'put'", ret, errno);
  if (ret < static_cast<signed int>(sizeof(T)*N))
    die ("not everything was written", ret, sizeof(T)*N);

  fflush (NULL);
  CommunicationsLog::
    record_communication<T> (CommunicationsLog::put, N, ret, debug_info);
};



/**
 * Read a certain number of objects from the input stream.
 */
template <typename T>
void get (T *t, const size_t N, const char *debug_info)
{
  unsigned int count = 0;
  while (count < sizeof(T)*N)
    {
      int ret;
      do
        ret = read (0, reinterpret_cast<char *> (t) + count,
                    sizeof(T) * N - count);
      while ((ret<0) && (errno==EINTR));
      
      if (ret < 0)
        die ("error on client side in 'get'", ret, errno);
      else
        count += ret;
    };
  
  CommunicationsLog::
    record_communication<T> (CommunicationsLog::get, N, count, debug_info);
};



int main () 
{
                                   // first action is to get the pid
                                   // of the master process, so that
                                   // we can check whether it is still
                                   // alive or not...
  pid_t master_pid;
  get (&master_pid, 1, "master_pid");
                                   // ...and start off a thread that
                                   // actually checks that
  Threads::ThreadManager thread_manager;
  Threads::spawn (thread_manager,
                  Threads::encapsulate (&monitor_parent_liveness)
                  .collect_args(master_pid));
  
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
                                             // signal. exit gracefully
            exit (0);
            break;
          };
          
          default:
                die ("Invalid action key", action,
                     static_cast<unsigned short int>(action));
        };
    };
                                   // exit here explicitly, without
                                   // giving the thread manager a
                                   // chance to wait for the child
                                   // thread, since that will loop
                                   // forever. however, we should
                                   // never be able to get to this
                                   // point anyway...
  exit (1);
};

                
