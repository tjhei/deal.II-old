/* $Id$ */
/* Copyright W. Bangerth, University of Heidelberg, 1998 */


#include <fe/fe_linear_mapping.h>
#include <fe/quadrature.h>
#include <grid/tria_iterator.h>
#include <grid/dof_accessor.h>
#include <grid/tria_boundary.h>





/*---------------------------- FELinearMapping ----------------------------------*/

#if deal_II_dimension == 1

template <>
inline
double
FELinearMapping<1>::shape_value_transform (const unsigned int i,
					   const Point<1>     &p) const
{
  Assert((i<2), ExcInvalidIndex(i));
  const double xi = p(0);
  switch (i)
    {
      case 0: return 1.-xi;
      case 1: return xi;
    }
  return 0.;
};



template <>
inline
Point<1>
FELinearMapping<1>::shape_grad_transform(const unsigned int i,
					 const Point<1>&) const
{
  Assert((i<2), ExcInvalidIndex(i));
  switch (i)
    {
    case 0: return Point<1>(-1.);
    case 1: return Point<1>(1.);
    }
  return Point<1>();
};



template <>
void FELinearMapping<1>::get_face_jacobians (const DoFHandler<1>::face_iterator &,
					     const Boundary<1>         &,
					     const vector<Point<0> > &,
					     vector<double>      &) const {
  Assert (false, ExcInternalError());
};



template <>
void FELinearMapping<1>::get_subface_jacobians (const DoFHandler<1>::face_iterator &,
						const unsigned int           ,
						const vector<Point<0> > &,
						vector<double>      &) const {
  Assert (false, ExcInternalError());
};



template <>
void FELinearMapping<1>::get_normal_vectors (const DoFHandler<1>::cell_iterator &,
					     const unsigned int,
					     const Boundary<1> &,
					     const vector<Point<0> > &,
					     vector<Point<1> > &) const {
  Assert (false, ExcInternalError());
};



template <>
void FELinearMapping<1>::get_normal_vectors (const DoFHandler<1>::cell_iterator &,
					    const unsigned int,
					    const unsigned int,
					    const vector<Point<0> > &,
					    vector<Point<1> > &) const {
  Assert (false, ExcInternalError());
};


template <>
void FELinearMapping<1>::fill_fe_values (const DoFHandler<1>::cell_iterator &cell,
					 const vector<Point<1> >            &unit_points,
					 vector<dFMatrix>  &jacobians,
					 const bool         compute_jacobians,
					 vector<Point<1> > &support_points,
					 const bool         compute_support_points,
					 vector<Point<1> > &q_points,
					 const bool         compute_q_points,
					 const dFMatrix      &shape_values_transform,
					 const vector<vector<Point<1> > > &shape_gradients_transform,
					 const Boundary<1> &boundary) const {
				   // simply pass down
  FiniteElement<1>::fill_fe_values (cell, unit_points,
				    jacobians, compute_jacobians,
				    support_points, compute_support_points,
				    q_points, compute_q_points,
				    shape_values_transform, shape_gradients_transform,
				    boundary);
};



#endif



#if deal_II_dimension == 2

template <>
inline
double
FELinearMapping<2>::shape_value_transform (const unsigned int i,
					   const Point<2>& p) const
{
  Assert((i<4), ExcInvalidIndex(i));
  switch (i)
    {
    case 0: return (1.-p(0)) * (1.-p(1));
    case 1: return p(0) * (1.-p(1));
    case 2: return p(0) * p(1);
    case 3: return (1.-p(0)) * p(1);
    }
  return 0.;
};



template <>
inline
Point<2>
FELinearMapping<2>::shape_grad_transform (const unsigned int i,
					  const Point<2>& p) const
{
  Assert((i<4), ExcInvalidIndex(i));
  switch (i)
    {
    case 0: return Point<2> (p(1)-1., p(0)-1.);
    case 1: return Point<2> (1.-p(1), -p(0));
    case 2: return Point<2> (p(1), p(0));
    case 3: return Point<2> (-p(1), 1.-p(0));
    }
  return Point<2> ();
};



template <>
void FELinearMapping<2>::get_face_jacobians (const DoFHandler<2>::face_iterator &face,
					     const Boundary<2>         &,
					     const vector<Point<1> > &unit_points,
					     vector<double> &face_jacobians) const {
				   // more or less copied from the linear
				   // finite element
  Assert (unit_points.size() == face_jacobians.size(),
	  ExcWrongFieldDimension (unit_points.size(), face_jacobians.size()));

				   // a linear mapping for a single line
				   // produces particularly simple
				   // expressions for the jacobi
				   // determinant :-)
  const double h = sqrt((face->vertex(1) - face->vertex(0)).square());
  fill_n (face_jacobians.begin(),
	  unit_points.size(),
	  h);  
};



template <>
void FELinearMapping<2>::get_subface_jacobians (const DoFHandler<2>::face_iterator &face,
						const unsigned int           ,
						const vector<Point<1> > &unit_points,
						vector<double> &face_jacobians) const {
				   // more or less copied from the linear
				   // finite element
  Assert (unit_points.size() == face_jacobians.size(),
	  ExcWrongFieldDimension (unit_points.size(), face_jacobians.size()));
  Assert (face->at_boundary() == false,
	  ExcBoundaryFaceUsed ());

				   // a linear mapping for a single line
				   // produces particularly simple
				   // expressions for the jacobi
				   // determinant :-)
  const double h = sqrt((face->vertex(1) - face->vertex(0)).square());
  fill_n (face_jacobians.begin(),
	  unit_points.size(),
	  h/2);
};



template <>
void FELinearMapping<2>::get_normal_vectors (const DoFHandler<2>::cell_iterator &cell,
					     const unsigned int       face_no,
					     const Boundary<2>       &,
					     const vector<Point<1> > &unit_points,
					     vector<Point<2> > &normal_vectors) const {
				   // more or less copied from the linear
				   // finite element
  Assert (unit_points.size() == normal_vectors.size(),
	  ExcWrongFieldDimension (unit_points.size(), normal_vectors.size()));

  const DoFHandler<2>::face_iterator face = cell->face(face_no);
				   // compute direction of line
  const Point<2> line_direction = (face->vertex(1) - face->vertex(0));
				   // rotate to the right by 90 degrees
  const Point<2> normal_direction(line_direction(1),
				  -line_direction(0));

  if (face_no <= 1)
				     // for sides 0 and 1: return the correctly
				     // scaled vector
    fill (normal_vectors.begin(), normal_vectors.end(),
	  normal_direction / sqrt(normal_direction.square()));
  else
				     // for sides 2 and 3: scale and invert
				     // vector
    fill (normal_vectors.begin(), normal_vectors.end(),
	  normal_direction / (-sqrt(normal_direction.square())));
};



template <>
void FELinearMapping<2>::get_normal_vectors (const DoFHandler<2>::cell_iterator &cell,
					     const unsigned int       face_no,
					     const unsigned int,
					     const vector<Point<1> > &unit_points,
					     vector<Point<2> > &normal_vectors) const {
				   // more or less copied from the linear
				   // finite element
				   // note, that in 2D the normal vectors to the
				   // subface have the same direction as that
				   // for the face
  Assert (unit_points.size() == normal_vectors.size(),
	  ExcWrongFieldDimension (unit_points.size(), normal_vectors.size()));
  Assert (cell->face(face_no)->at_boundary() == false,
	  ExcBoundaryFaceUsed ());

  const DoFHandler<2>::face_iterator face = cell->face(face_no);
				   // compute direction of line
  const Point<2> line_direction = (face->vertex(1) - face->vertex(0));
				   // rotate to the right by 90 degrees
  const Point<2> normal_direction(line_direction(1),
				  -line_direction(0));

  if (face_no <= 1)
				     // for sides 0 and 1: return the correctly
				     // scaled vector
    fill (normal_vectors.begin(), normal_vectors.end(),
	  normal_direction / sqrt(normal_direction.square()));
  else
				     // for sides 2 and 3: scale and invert
				     // vector
    fill (normal_vectors.begin(), normal_vectors.end(),
	  normal_direction / (-sqrt(normal_direction.square())));
};

#endif



template <int dim>
void FELinearMapping<dim>::fill_fe_values (const DoFHandler<dim>::cell_iterator &cell,
					   const vector<Point<dim> >            &unit_points,
					   vector<dFMatrix>    &jacobians,
					   const bool           compute_jacobians,
					   vector<Point<dim> > &support_points,
					   const bool           compute_support_points,
					   vector<Point<dim> > &q_points,
					   const bool           compute_q_points,
					   const dFMatrix      &shape_values_transform,
					   const vector<vector<Point<dim> > > &/*shape_grad_transform*/,
					   const Boundary<dim> &boundary) const {
  Assert (jacobians.size() == unit_points.size(),
	  ExcWrongFieldDimension(jacobians.size(), unit_points.size()));
  Assert (q_points.size() == unit_points.size(),
	  ExcWrongFieldDimension(q_points.size(), unit_points.size()));
  Assert (support_points.size() == total_dofs,
	  ExcWrongFieldDimension(support_points.size(), total_dofs));

  
  unsigned int n_points=unit_points.size();

  Point<dim> vertices[GeometryInfo<dim>::vertices_per_cell];
  for (unsigned int l=0; l<GeometryInfo<dim>::vertices_per_cell; ++l)
    vertices[l] = cell->vertex(l);
  

  if (compute_q_points) 
    {
				       // initialize points to zero
      for (unsigned int i=0; i<n_points; ++i)
	q_points[i] = Point<dim> ();
      
				       // note: let x_l be the vector of the
				       // lth quadrature point in real space and
				       // xi_l that on the unit cell, let further
				       // p_j be the vector of the jth vertex
				       // of the cell in real space and
				       // N_j(xi_l) be the value of the associated
				       // basis function at xi_l, then
				       // x_l(xi_l) = sum_j p_j N_j(xi_l)
				       //
				       // Here, N_j is the *linear* basis function,
				       // not that of the finite element, since we
				       // use a subparametric mapping
      for (unsigned int j=0; j<GeometryInfo<dim>::vertices_per_cell; ++j) 
	for (unsigned int l=0; l<n_points; ++l) 
	  q_points[l] += vertices[j] * shape_values_transform(j, l);
    };
  

/* jacobi matrices: compute d(x)/d(xi) and invert this
   Let M(l) be the inverse of J at the quadrature point l, then
     M_{ij}(l) = sum_s p_i(s) d(N_s(l))/d(xi_j)
   where p_i(s) is the i-th coordinate of the s-th vertex vector,
   N_s(l) is the value of the s-th vertex shape function at the
   quadrature point l.

   We could therefore write:
   l=0..n_points-1
     i=0..dim-1
       j=0..dim-1
         M_{ij}(l) = 0
	 s=0..n_vertices
	   M_{ij}(l) += p_i(s) d(N_s(l))/d(xi_j)

  However, we rewrite the loops to only compute the gradient once for
  each integration point and basis function.

  Indeed, this was the old way we did it; the code is below. However, there
  is a more efficient way, namely setting up M analytically, doing the
  inversion analyically and only then doing the evaluation; a small Maple
  script does this (it is part of the <lagrange> script in the <scripts>
  subdirectory).
  
  if (compute_jacobians) 
    {
      dFMatrix M(dim,dim);
      for (unsigned int l=0; l<n_points; ++l) 
	{
	  M.clear ();
	  for (unsigned int s=0; s<GeometryInfo<dim>::vertices_per_cell; ++s)
	    {
					       // we want the linear transform,
					       // so use that function
	      const Point<dim> gradient = shape_grad_transform[s][l];
	      for (unsigned int i=0; i<dim; ++i)
		for (unsigned int j=0; j<dim; ++j)
		  M(i,j) += vertices[s](i) * gradient(j);
	    };
	  jacobians[l].invert(M);
	};
    };

    
  One last note regarding whether we have to invert M or M transposed: it is
  easy to try out, by computing the gradients of a function on a distorted
  cell (just move one vertex) where the nodal values for linear elements
  are one for the moved vertex and zero otherwise. Please also note that
  when computing the gradients on the real cell, the jacobian matrix
  is multiplied to the unit cell gradient *from the right*! be very careful
  with these things.

  The following little program tests the correct behaviour (you have to find
  out the right include files, I tested it within a whole project with far
  more include files than necessary):

  -------------------------------------------
  int main () {
    Triangulation<2> tria;
    tria.create_hypercube (0,1);
    tria.begin_active()->vertex(2)(0) = 2;

    DoFHandler<2> dof(&tria);
    FELinear<2> fe;
    dof.distribute_dofs(fe);

    StraightBoundary<2> b;
    QTrapez<2> q;
    FEValues<2> fevalues(fe,q,update_gradients);
    fevalues.reinit (dof.begin_active(),b);
  
  
    dVector val(4);
    val(2) = 1;

    vector<Point<2> > grads(4);
    fevalues.get_function_grads (val, grads);

    for (unsigned int i=0; i<4; ++i)
      cout << "Vertex " << i
	   << "   grad=" << grads[i] << endl;
  };
  ---------------------------------------------
  
  The correct output should be
  --------------------------------
  Vertex 0   grad=0 0
  Vertex 1   grad=0.5 0
  Vertex 2   grad=0 1
  Vertex 3   grad=0.5 0.5
  --------------------------------
  and the wrong would be
  --------------------------------
  Vertex 0   grad=0 0
  Vertex 1   grad=0.5 0
  Vertex 2   grad=-1 1
  Vertex 3   grad=0 1
  --------------------------------  
*/

  if (compute_jacobians)
    for (unsigned int point=0; point<n_points; ++point)
      {
	const double xi = unit_points[point](0);
	const double eta= unit_points[point](1);
	
	const double t6 = vertices[0](0)*vertices[3](1);
	const double t8 = vertices[2](0)*xi;
	const double t10 = vertices[1](0)*eta;
	const double t12 = vertices[3](0)*vertices[1](1);
	const double t16 = vertices[3](0)*xi;
	const double t20 = vertices[0](0)*vertices[1](1);
	const double t22 = vertices[0](0)*vertices[2](1);
	const double t24 = t6*xi-t8*vertices[1](1)-t10*vertices[3](1)+
			   t12*eta-vertices[3](0)*vertices[2](1)*eta-
			   t16*vertices[0](1)+t16*vertices[1](1)-t12+
			   vertices[3](0)*vertices[0](1)-t20*eta+t22*eta;
	const double t28 = vertices[1](0)*vertices[3](1);
	const double t31 = vertices[2](0)*eta;
	const double t36 = t8*vertices[0](1)+vertices[1](0)*vertices[2](1)*xi-
			   t28*xi+t10*vertices[0](1)-t31*vertices[0](1)+
			   t31*vertices[3](1)+t20-t6-vertices[1](0)*
			   vertices[0](1)+t28-t22*xi;
	const double t38 = 1/(t24+t36);

	jacobians[point](0,0) = (-vertices[0](1)+vertices[0](1)*xi-
				 vertices[1](1)*xi+vertices[2](1)*xi+
				 vertices[3](1)-vertices[3](1)*xi)*t38;
	jacobians[point](0,1) = -(-vertices[0](0)+vertices[0](0)*xi-
				  vertices[1](0)*xi+t8+vertices[3](0)-t16)*t38;
	jacobians[point](1,0) = -(-vertices[0](1)+vertices[0](1)*eta+
				  vertices[1](1)-vertices[1](1)*eta+
				  vertices[2](1)*eta-vertices[3](1)*eta)*t38;
	jacobians[point](1,1) = (-vertices[0](0)+vertices[0](0)*eta+
				 vertices[1](0)-t10+t31-vertices[3](0)*eta)*t38;
      };
  
    
  if (compute_support_points)
    get_support_points (cell, boundary, support_points);
};




/*------------------------------- Explicit Instantiations -------------*/

template class FELinearMapping<deal_II_dimension>;
