/*----------------------------   tensor.h     ---------------------------*/
/*      $Id$                 */
#ifndef __tensor_H
#define __tensor_H
/*----------------------------   tensor.h     ---------------------------*/


#include <base/tensor_base.h>



/**
 * Provide a general tensor class with an arbitrary rank, i.e. with
 * an arbitrary number of indices. The Tensor class provides an
 * indexing operator and a bit of infrastructure, but most
 * functionality is recursively handed down to tensors of rank 1 or
 * put into external templated functions, e.g. the #contract# family.
 *
 * Using this tensor class for objects of rank 2 has advantages over
 * matrices in many cases since the dimension is known to the compiler
 * as well as the location of the data. It is therefore possible to
 * produce far more efficient code than for matrices with
 * runtime-dependent dimension.
 */
template <int rank_, int dim>
class Tensor {
  public:
				     /**
				      * Provide a way to get the
				      * dimension of an object without
				      * explicit knowledge of it's
				      * data type. Implementation is
				      * this way instead of providing
				      * a function #dimension()#
				      * because now it is possible to
				      * get the dimension at compile
				      * time without the expansion and
				      * preevaluation of an inlined
				      * function; the compiler may
				      * therefore produce more
				      * efficient code and you may use
				      * this value to declare other
				      * data types.
				      */
    static const unsigned int dimension = dim;

				     /**
				      * Publish the rank of this tensor to
				      * the outside world.
				      */
    static const unsigned int rank      = rank_;
    
				     /**
				      * Declare an array type which can
				      * be used to initialize statically
				      * an object of this type.
				      */
    typedef typename Tensor<rank_-1,dim>::array_type array_type[dim];

				     /**
				      * Constructor. Initialize all entries
				      * to zero.
				      */
    Tensor ();
    
				     /**
				      * Copy constructor, where the data is
				      * copied from a C-style array.
				      */
    Tensor (const array_type &initializer);
    
				     /**
				      * Read-Write access operator.
				      */
    Tensor<rank_-1,dim> &operator [] (const unsigned int i);

				     /**
				      * Read-only access operator.
				      */
    const Tensor<rank_-1,dim> &operator [] (const unsigned int i) const;

				     /**
				      *  Assignment operator.
				      */
    Tensor & operator = (const Tensor<rank_,dim> &);

				     /**
				      *  Test for equality of two tensors.
				      */
    bool operator == (const Tensor<rank_,dim> &) const;

    				     /**
				      *  Test for inequality of two tensors.
				      */
    bool operator != (const Tensor<rank_,dim> &) const;

				     /**
				      *  Add another vector, i.e. move this tensor by
				      *  the given offset.
				      */
    Tensor<rank_,dim> & operator += (const Tensor<rank_,dim> &);
    
				     /**
				      *  Subtract another vector.
				      */
    Tensor<rank_,dim> & operator -= (const Tensor<rank_,dim> &);

				     /**
				      *  Scale the tensor by #factor#, i.e. multiply
				      *  all coordinates by #factor#.
				      */
    Tensor<rank_,dim> & operator *= (const double &factor);

				     /**
				      *  Scale the vector by #1/factor#.
				      */
    Tensor<rank_,dim> & operator /= (const double &factor);

				     /**
				      *  Add two tensors. If possible, use
				      *  #operator +=# instead since this does not
				      *  need to copy a point at least once.
				      */
    Tensor<rank_,dim>   operator + (const Tensor<rank_,dim> &) const;

				     /**
				      *  Subtract two tensors. If possible, use
				      *  #operator +=# instead since this does not
				      *  need to copy a point at least once.
				      */
    Tensor<rank_,dim>   operator - (const Tensor<rank_,dim> &) const;

				     /**
				      * Reset all values to zero.
				      */
    void clear ();

    
				     /**
				      *  Exception
				      */
    DeclException1 (ExcInvalidIndex,
		    int,
		    << "Invalid index " << arg1);
  private:
				     /**
				      * Array of tensors holding the
				      * subelements.
				      */
    Tensor<rank_-1,dim> subtensor[dim];
};




/*--------------------------- Inline functions -----------------------------*/


template <int rank_, int dim>
inline
Tensor<rank_,dim>::Tensor () {
// default constructor. not specifying an initializer list calls
// the default constructor of the subobjects, which initialize them
// selves. therefore, the tensor is set to zero this way
};



template <int rank_, int dim>
inline
Tensor<rank_,dim>::Tensor (const array_type &initializer) {
  for (unsigned int i=0; i<dim; ++i)
    subtensor[i] =  Tensor<rank_-1,dim>(initializer[i]);
};

      


template <int rank_, int dim>
inline
Tensor<rank_-1,dim> &
Tensor<rank_,dim>::operator[] (const unsigned int i) {
  Assert (i<dim, ExcInvalidIndex(i));
  
  return subtensor[i];
};



template <int rank_, int dim>
inline
const Tensor<rank_-1,dim> &
Tensor<rank_,dim>::operator[] (const unsigned int i) const {
  Assert (i<dim, ExcInvalidIndex(i));
  
  return subtensor[i];
};



template <int rank_, int dim>
inline
Tensor<rank_,dim> & Tensor<rank_,dim>::operator = (const Tensor<rank_,dim> &t) {
  for (unsigned int i=0; i<dim; ++i)
    subtensor[i] = t.subtensor[i];
  return *this;
};



template <int rank_, int dim>
inline
bool Tensor<rank_,dim>::operator == (const Tensor<rank_,dim> &p) const {
  for (unsigned int i=0; i<dim; ++i)
    if (subtensor[i] != p.subtensor[i]) return false;
  return true;
};



template <int rank_, int dim>
inline
bool Tensor<rank_,dim>::operator != (const Tensor<rank_,dim> &p) const {
  return !((*this) == p);
};



template <int rank_, int dim>
inline
Tensor<rank_,dim> & Tensor<rank_,dim>::operator += (const Tensor<rank_,dim> &p) {
  for (unsigned int i=0; i<dim; ++i)
    subtensor[i] += p.subtensor[i];
  return *this;
};



template <int rank_, int dim>
inline
Tensor<rank_,dim> & Tensor<rank_,dim>::operator -= (const Tensor<rank_,dim> &p) {
  for (unsigned int i=0; i<dim; ++i)
    subtensor[i] -= p.subtensor[i];
  return *this;
};



template <int rank_, int dim>
inline
Tensor<rank_,dim> & Tensor<rank_,dim>::operator *= (const double &s) {
  for (unsigned int i=0; i<dim; ++i)
    subtensor[i] *= s;
  return *this;
};



template <int rank_, int dim>
inline
Tensor<rank_,dim> & Tensor<rank_,dim>::operator /= (const double &s) {
  for (unsigned int i=0; i<dim; ++i)
    subtensor[i] /= s;
  return *this;
};



template <int rank_, int dim>
inline
Tensor<rank_,dim>
Tensor<rank_,dim>::operator + (const Tensor<rank_,dim> &t) const {
  Tensor<rank_,dim> tmp(*this);
  
  for (unsigned int i=0; i<dim; ++i)
    tmp.subtensor[i] += t.subtensor[i];

  return tmp;
};



template <int rank_, int dim>
inline
Tensor<rank_,dim>
Tensor<rank_,dim>::operator - (const Tensor<rank_,dim> &t) const {
  Tensor<rank_,dim> tmp(*this);
  
  for (unsigned int i=0; i<dim; ++i)
    tmp.subtensor[i] -= t.subtensor[i];

  return tmp;
};



template <int rank_, int dim>
inline
void Tensor<rank_,dim>::clear () {
  for (unsigned int i=0; i<dim; ++i)
    subtensor[i].clear();
};





/* ----------------- Non-member functions operating on tensors. ------------ */



/*  Exception class. This is certainly not the best possible place for its
    declaration, but at present, local classes to any of Tensor<> can't
    be properly accessed (haven't investigated why). If anyone has a better
    idea, realize it!
*/
DeclException1 (ExcInvalidTensorIndex,
		int,
		<< "Invalid tensor index " << arg1);



template <int dim>
inline
void contract (Tensor<1,dim>       &dest,
	       const Tensor<2,dim> &src1,
	       const Tensor<1,dim> &src2) {
  dest.clear ();
  for (unsigned int i=0; i<dim; ++i)
    for (unsigned int j=0; j<dim; ++j)
      dest[i] += src1[i][j] * src2[j];
};



template <int dim>
inline
void contract (Tensor<2,dim>       &dest,
	       const Tensor<2,dim> &src1,
	       const Tensor<2,dim> &src2) {
  dest.clear ();
  for (unsigned int i=0; i<dim; ++i)
    for (unsigned int j=0; j<dim; ++j)
      for (unsigned int k=0; k<dim; ++k)
	dest[i][j] += src1[i][k] * src2[k][j];
};



template <int dim>
inline
void contract (Tensor<2,dim>       &dest,
	       const Tensor<2,dim> &src1,   const unsigned int index1,
	       const Tensor<2,dim> &src2,   const unsigned int index2) {
  dest.clear ();

  switch (index1)
    {
      case 1:
	    switch (index2)
	      {
		case 1:
		      for (unsigned int i=0; i<dim; ++i)
			for (unsigned int j=0; j<dim; ++j)
			  for (unsigned int k=0; k<dim; ++k)
			    dest[i][j] += src1[k][i] * src2[k][j];
		      break;
		case 2:
		      for (unsigned int i=0; i<dim; ++i)
			for (unsigned int j=0; j<dim; ++j)
			  for (unsigned int k=0; k<dim; ++k)
			    dest[i][j] += src1[k][i] * src2[j][k];
		      break;

		default:
		      Assert (false, ExcInvalidTensorIndex (index2));
	      };
	    break;
      case 2:
	    switch (index2)
	      {
		case 1:
		      for (unsigned int i=0; i<dim; ++i)
			for (unsigned int j=0; j<dim; ++j)
			  for (unsigned int k=0; k<dim; ++k)
			    dest[i][j] += src1[i][k] * src2[k][j];
		      break;
		case 2:
		      for (unsigned int i=0; i<dim; ++i)
			for (unsigned int j=0; j<dim; ++j)
			  for (unsigned int k=0; k<dim; ++k)
			    dest[i][j] += src1[i][k] * src2[j][k];
		      break;

		default:
		      Assert (false, ExcInvalidTensorIndex (index2));
	      };
	    break;

      default:
	    Assert (false, ExcInvalidTensorIndex (index1));
    };
};



template <int dim>
inline
void contract (Tensor<2,dim>       &dest,
	       const Tensor<3,dim> &src1,   const unsigned int index1,
	       const Tensor<1,dim> &src2) {
  dest.clear ();

  switch (index1)
    {
      case 1:
	    for (unsigned int i=0; i<dim; ++i)
	      for (unsigned int j=0; j<dim; ++j)
		for (unsigned int k=0; k<dim; ++k)
		  dest[i][j] += src1[k][i][j] * src2[k];
	    break;

      case 2:
	    for (unsigned int i=0; i<dim; ++i)
	      for (unsigned int j=0; j<dim; ++j)
		for (unsigned int k=0; k<dim; ++k)
		  dest[i][j] += src1[i][k][j] * src2[k];
	    break;

      case 3:
	    for (unsigned int i=0; i<dim; ++i)
	      for (unsigned int j=0; j<dim; ++j)
		for (unsigned int k=0; k<dim; ++k)
		  dest[i][j] += src1[i][j][k] * src2[k];
	    break;

      default:
	    Assert (false, ExcInvalidTensorIndex (index1));
    };
};



template <int dim>
inline
void contract (Tensor<3,dim>       &dest,
	       const Tensor<3,dim> &src1,
	       const Tensor<2,dim> &src2) {
  dest.clear ();
  for (unsigned int i=0; i<dim; ++i)
    for (unsigned int j=0; j<dim; ++j)
      for (unsigned int k=0; k<dim; ++k)
	for (unsigned int l=0; l<dim; ++l)
	  dest[i][j][k] += src1[i][j][l] * src2[l][k];
};



template <int dim>
inline
void contract (Tensor<3,dim>       &dest,
	       const Tensor<2,dim> &src1,
	       const Tensor<3,dim> &src2) {
  dest.clear ();
  for (unsigned int i=0; i<dim; ++i)
    for (unsigned int j=0; j<dim; ++j)
      for (unsigned int k=0; k<dim; ++k)
	for (unsigned int l=0; l<dim; ++l)
	  dest[i][j][k] += src1[i][l] * src2[l][j][k];
};



template <int dim>
inline
void contract (Tensor<4,dim>       &dest,
	       const Tensor<3,dim> &src1,
	       const Tensor<3,dim> &src2) {
  dest.clear ();
  for (unsigned int i=0; i<dim; ++i)
    for (unsigned int j=0; j<dim; ++j)
      for (unsigned int k=0; k<dim; ++k)
	for (unsigned int l=0; l<dim; ++l)
	  for (unsigned int m=0; m<dim; ++m)
	    dest[i][j][k][l] += src1[i][j][m] * src2[m][k][l];
};



template <int rank>
inline
double determinant (const Tensor<rank,1> &t) {
				   // determinant of tensors of
				   // dimension one and arbitrary rank can
				   // be computed by recursion
  return determinant(t[0]);
};


inline
double determinant (const Tensor<1,1> &t) {
  return t[0];
};




inline
double determinant (const Tensor<2,2> &t) {
  return ((t[0][0] * t[1][1]) -
	  (t[1][0] * t[0][1]));
};



inline
double determinant (const Tensor<2,3> &t) {
				   // get this using Maple:
				   // with(linalg);
				   // a := matrix(3,3);
				   // x := det(a);
				   // readlib(C);
				   // C(x, optimized);
  return ( t[0][0]*t[1][1]*t[2][2]
	   -t[0][0]*t[1][2]*t[2][1]
	   -t[1][0]*t[0][1]*t[2][2]
	   +t[1][0]*t[0][2]*t[2][1]
	   +t[2][0]*t[0][1]*t[1][2]
	   -t[2][0]*t[0][2]*t[1][1] );
};

    
    



/*----------------------------   tensor.h     ---------------------------*/
/* end of #ifndef __tensor_H */
#endif
/*----------------------------   tensor.h     ---------------------------*/
