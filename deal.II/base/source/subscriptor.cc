//---------------------------------------------------------------------------
//    $Id$
//    Version: $Name$
//
//    Copyright (C) 1998, 1999, 2000, 2001, 2002, 2003, 2005, 2006, 2007, 2008 by the deal.II authors
//
//    This file is subject to QPL and may not be  distributed
//    without copyright and license information. Please refer
//    to the file deal.II/doc/license.html for the  text  and
//    further information on this license.
//
//---------------------------------------------------------------------------


#include <base/thread_management.h>
#include <base/subscriptor.h>
#include <base/logstream.h>

#include <typeinfo>
#include <string>
#include <iostream>

DEAL_II_NAMESPACE_OPEN


namespace 
{
// create a lock that might be used to control subscription to and
// unsubscription from objects, as that might happen in parallel.
// since it should happen rather seldom that several threads try to
// operate on different objects at the same time (the usual case is
// that they subscribe to the same object right after thread
// creation), a global lock should be sufficient, rather than one that
// operates on a per-object base (in which case we would have to
// include the huge <thread_management.h> file into the
// <subscriptor.h> file).
  Threads::ThreadMutex subscription_lock;
}


static const char* unknown_subscriber = "unknown subscriber";


Subscriptor::Subscriptor ()
                :
		counter (0),
		object_info (0)
{}


Subscriptor::Subscriptor (const Subscriptor &)
                :
		counter (0),
		object_info (0)
{}


Subscriptor::~Subscriptor ()
{
				   // check whether there are still
				   // subscriptions to this object. if
				   // so, output the actual name of
				   // the class to which this object
				   // belongs, i.e. the most derived
				   // class. note that the name may be
				   // mangled, so it need not be the
				   // clear-text class name. however,
				   // you can obtain the latter by
				   // running the c++filt program over
				   // the output.
#ifdef DEBUG
  std::string infostring;
  for (map_iterator it = counter_map.begin(); it != counter_map.end(); ++it)
    {
      if (it->second > 0)
	infostring += std::string("\n  from Subscriber ")
		      + std::string(it->first);
    }

				   // if there are still active pointers, show
				   // a message and kill the program. However,
				   // under some circumstances, this is not so
				   // desirable. For example, in code like this
				   //
				   // Triangulation tria;
				   // DoFHandler *dh = new DoFHandler(tria);
				   // ...some function that throws an exception
				   //
				   // the exception will lead to the
				   // destruction of the triangulation, but
				   // since the dof_handler is on the heap it
				   // will not be destroyed. This will trigger
				   // an assertion in the triangulation. If we
				   // kill the program at this point, we will
				   // never be able to learn what caused the
				   // problem. In this situation, just display
				   // a message and continue the program.
  if (counter != 0)
    {
      if (std::uncaught_exception() == false)
	{
	  Assert (counter == 0,
		  ExcInUse (counter, object_info->name(), infostring));
	}
      else
	{
	  std::cerr << "---------------------------------------------------------"
		    << std::endl
		    << "An object pointed to by a SmartPointer is being destroyed."
		    << std::endl
		    << "Under normal circumstances, this would abort the program."
		    << std::endl
		    << "However, another exception is being processed at the"
		    << std::endl
		    << "moment, so the program will continue to run to allow"
		    << std::endl
		    << "this exception to be processed."
		    << std::endl
		    << "---------------------------------------------------------"
		    << std::endl;
	}
    }
#endif
}



Subscriptor & Subscriptor::operator = (const Subscriptor &s)
{
  object_info = s.object_info;
  return *this;
}

// These are the implementations for debug mode. The optimized
// versions are inlined in the header file.

void Subscriptor::do_subscribe (const char* id) const
{
#ifdef DEBUG
  if (object_info == 0)
    object_info = &typeid(*this);
  Threads::ThreadMutex::ScopedLock lock (subscription_lock);
  ++counter;
  
#if DEAL_USE_MT == 0
  const char* const name = (id != 0) ? id : unknown_subscriber;
  
  map_iterator it = counter_map.find(name);
  if (it == counter_map.end())
    counter_map.insert(map_value_type(name, 1U));
  
  else
    it->second++;
#endif
#endif
}


void Subscriptor::do_unsubscribe (const char* id) const
{
#ifdef DEBUG
  Assert (counter>0, ExcNotUsed());
  Threads::ThreadMutex::ScopedLock lock (subscription_lock);
  --counter;
  
#if DEAL_USE_MT == 0
  const char* name = (id != 0) ? id : unknown_subscriber;

  map_iterator it = counter_map.find(name);
  Assert (it != counter_map.end(), ExcNoSubscriber(object_info->name(), name));
  Assert (it->second > 0, ExcNoSubscriber(object_info->name(), name));
  
  it->second--;
#endif
#endif
}


unsigned int Subscriptor::n_subscriptions () const
{
  return counter;
}


void Subscriptor::list_subscribers () const
{
#if DEAL_USE_MT == 0
  for (map_iterator it = counter_map.begin();
  it != counter_map.end(); ++it)
    deallog << it->second << '/'
	    << counter << " subscriptions from \""
	    << it->first << '\"' << std::endl;
#else
  deallog << "No subscriber listing with multithreading" << std::endl;
#endif
}

DEAL_II_NAMESPACE_CLOSE
