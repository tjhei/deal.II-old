#ifndef MESH_CONVERSION_H
#define MESH_CONVERSION_H

//--------------------------------------------------------------------------------

#include <string>
#include <sstream>
#include <fstream>
#include <iostream>
#include <iomanip>
#include <cmath>
#include <vector>

using namespace std;

class MeshConversion
{
	public:
		MeshConversion (const unsigned int dimension);
		~MeshConversion ();
		bool read_in_abaqus_inp (const std::string file_name);
		bool write_out_avs_ucd (const std::string file_name);

	private:
		void greeting ();
		std::vector <int> get_global_node_numbers(const int face_cell_no, const int face_cell_face_no);
		
		const double 				tolerance;
		const unsigned int 			dimension;
		
		unsigned int 				node_per_face;
		unsigned int 				node_per_cell;
		unsigned int 				face_per_cell;
		
		std::string 				input_file_name;
		std::string 				output_file_name;
		
		std::vector< std::vector<double> > 	node_list; 	// Stored as [ global node-id (int), x-coord, y-coord, z-coord ]
		std::vector< std::vector<int> > 	cell_list; 	// Stored as [ material-id (int), node1, node2, node3, node4, node5, node6, node7, node8 ]
		std::vector< std::vector<int> > 	face_list; 	// Stored as [ sideset-id (int), node1, node2, node3, node4 ]
};

#endif // MESH_CONVERSION_H
