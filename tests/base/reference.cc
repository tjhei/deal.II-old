
#include <fstream>
#include <base/subscriptor.h>
#include <base/smartpointer.h>
#include <base/logstream.h>

class Test : public Subscriptor
{
  const char* name;
public:
  Test(const char* n) :
		  name(n)
      {
	deallog << "Construct " << name << endl;
      }
  ~Test()
      {
	deallog << "Destruct " << name << endl;
      }	  
  void f()
  {
    deallog << "mutable" << endl;
  }
  void f() const
  {
    deallog << "const" << endl;
  }
};



main()
{
  ofstream logfile("reference.output");
  deallog.attach(logfile);
  deallog.depth_console(0);
  cerr = logfile;
  Test a("A");
  const Test b("B");
  SmartPointer<Test>       r=&a;
  SmartPointer<const Test> s=&a;
//  SmartPointer<Test>       t=&b;    // this one should not work
  SmartPointer<Test>       t=const_cast<Test*>(&b);
  SmartPointer<const Test> u=&b;

  
  deallog << "a ";
  a.f();            // should print "mutable", since #a# is not const
  deallog << "b ";
  b.f();            // should print "const", since #b# is const
  deallog << "r ";
  r->f();           // should print "mutable", since it points to the non-const #a#
  deallog << "s ";
  s->f();           // should print "const", since it points to the const #b#
				   // but we made it const
  deallog << "t ";
  t->f();           // should print "mutable", since #b# is const, but
				   // we casted the constness away
  deallog << "u ";
  u->f();           // should print "const" since #b# is const
				   // Now try if subscriptor works
  {
    Test c("C");
    r = &c;
    Test d("D");
    r = &d;
  }
}

void abort()
{}
