// $Id$

#include "laplace.h"
#include "functions.h"

#include <iostream.h>
#include <fstream.h>
#include <stdlib.h>

#include <base/logstream.h>
#include <base/jobidentifier.h>

ZeroFunction<2> zero;

char fname[30];

main(int argc, char** argv)
{
  // JS.Logfile erzeugen, als Stream konzipiert 

  ofstream logfile("T");
  deallog.attach(logfile);
  
  if (argc==1)
    cerr << "Usage: " << argv[0] << "firstgrid\nUsing 3"
	 << endl;
  
  int firstgrid = 3;
  
  if (argc>=2) firstgrid = atoi(argv[1]);

  deallog << "Firstgrid " << firstgrid << endl;

  // JS.Benötigte Funktionen zur Lösung des Problems   
  WeightFunction weight;
  BoundaryFct boundary;
  Laplace lap;
  
  // JS.Logstream ist ein stack, ab hier wird "Adaptive:" vor jede Zeile 
  // gestellt
  deallog.push("Adaptive");
  deallog.depth_console(2);
  
  for (unsigned step = 0; step < 3 ; ++step)
  {
    deallog << "Step " << step << endl;
    // JS.Beim ersten Mal ein verfeinertes Grid erzeugen, firstgrid=Anzahl
    // der Verfeinerungen
    if (!step)
      lap.remesh(firstgrid);
    else
    {
//JS.      lap.adapt();
      lap.remesh(1);
    }

    deallog.push("Primal");

    // JS.exakte Lösung mit 0 auf der rechten Seite
    lap.assemble_primal(boundary,zero);
    lap.solve_primal();
    
    // JS.ab hier kein "Adaptive:" mehr
    deallog.pop();
    
    sprintf(fname,"T%02d",step);
    // JS.Daten zurückschreiben. Aber welche ?
    lap.result(weight,boundary);
    lap.write_data(fname);
  }
  // JS.Logfile sauber abschließen und Schluß
  deallog.pop();
  deallog.detach();
}
