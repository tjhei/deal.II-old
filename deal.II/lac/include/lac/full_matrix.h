/*----------------------------   fmatrix.h     ---------------------------*/
//      $Id$
#ifndef __lac_fullmatrix_H
#define __lac_fullmatrix_H
/*----------------------------   fmatrix.h     ---------------------------*/

// This file is part of the DEAL Library
// DEAL is Copyright(1995) by
// Roland Becker, Guido Kanschat, Franz-Theo Suttmeier
// Revised by Wolfgang Bangerth


#include <base/exceptions.h>


// forward declarations

template<typename number> class Vector;

class iVector;



/**
 *  Rectangular/quadratic full matrix.
 *
 *  Memory for Components is supplied explicitly <p>
 *  ( ! Amount of memory needs not to comply with actual dimension due to reinitializations ! ) <p>
 *  - all necessary methods for matrices are supplied <p>
 *  - operators available are '=' and '( )' <p>
 *  CONVENTIONS for used 'equations' : <p>
 *  - THIS matrix is always named 'A' <p>
 *  - matrices are always uppercase , vectors and scalars are lowercase <p>
 *  - Transp(A) used for transpose of matrix A
 *
 */
template<typename number>
class FullMatrix
{
  private:
				     /**
				      * Component-array.
				      */
    number* val;
				     /** 
				      * Dimension. Actual number of Columns
				      */
    unsigned int dim_range;
				     /**
				      * Dimension. Actual number of Rows
				      */
    unsigned int dim_image;
				     /**
				      * Dimension. Determines amount of reserved memory
				      */
    unsigned int val_size;
    
				     /**
				      * Initialization   . initialize memory for Matrix <p>
				      * ( m rows , n columns )
				      */
    void init (const unsigned int m, const unsigned int n);
    
				     /**
				      * Return a read-write reference to the
				      * element #(i,j)#.
				      *
				      * This function does no bounds checking.
				      */
    number& el (const unsigned int i, const unsigned int j);
    
				     /**
				      * Return the value of the element #(i,j)#.
				      *
				      * This function does no bounds checking.
				      */
    number el (const unsigned int i, const unsigned int j) const;
    
    
  public:
				     /**
				      * Constructor. Initialize the matrix as
				      * a square matrix with dimension #n#.
				      */
    explicit FullMatrix (const unsigned int n = 1);
    
				     /**
				      * Constructor. Initialize the matrix as
				      * a rectangular #m# times #n# matrix.
				      */
    FullMatrix (const unsigned int m, const unsigned int n);
    
				     /** 
				      * Copy constructor. Be very careful with
				      * this constructor, since it may take a
				      * huge amount of computing time for large
				      * matrices!!
				      */
    explicit FullMatrix (const FullMatrix&);

				     /**
				      * Destructor. Release all memory.
				      */
    ~FullMatrix();
    
				     /**
				      * Comparison operator. Be careful with
				      * this thing, it may eat up huge amounts
				      * of computing time! It is most commonly
				      * used for internal consistency checks
				      * of programs.
				      */
    bool operator == (const FullMatrix<number> &) const;

				     /**
				      *  A = B           . Copy all elements
				      */
    template<typename number2>
    FullMatrix<number>& operator = (const FullMatrix<number2>& B);
    
    
				     /**
				      *  U(0-m,0-n) = s  . Fill all elements
				      */
    template<typename number2>
    void fill (const FullMatrix<number2>& src,
	       const unsigned int i=0, const unsigned int j=0);
    
				     /**
				      * Change  Dimension.
				      * Set dimension to (m,n) <p>
				      * ( reinit rectangular matrix )
				      */
    void reinit (const unsigned int m, const unsigned int n);
    
				     /**
				      * Change  Dimension.
				      * Set dimension to (n,n) <p>
				      * ( reinit quadratic matrix )
				      */
    void reinit (const unsigned int n);
    
				     /**
				      * Adjust  Dimension.
				      * Set dimension to ( m(B),n(B) ) <p>
				      * ( adjust to dimensions of another matrix B )
				      */
    template<typename number2>
    void reinit (const FullMatrix<number2> &B);
    
				     /**
				      * Return number of rows of this matrix.
				      * To remember: this matrix is an
				      * $m \times n$-matrix.
				      */
    unsigned int m () const;
    
				     /**
				      * Return number of columns of this matrix.
				      * To remember: this matrix is an
				      * $m \times n$-matrix.
				      */
    unsigned int n () const;

    				     /**
				      * Return whether the matrix contains only
				      * elements with value zero. This function
				      * is mainly for internal consistency
				      * check and should seldomly be used when
				      * not in debug mode since it uses quite
				      * some time.
				      */
    bool all_zero () const;

				     //@}
    
    
				     /**@name 2: Data-Access
				      */
				     //@{
				     /**
				      *   Access Elements. returns element at relative 'address' i <p>
				      *   ( -> access to A(i/n , i mod n) )
				      */
    number el (const unsigned int i) const;
    
				     /**
				      * Return the value of the element #(i,j)#.
				      * Does the same as the #el(i,j)# function
				      * but does bounds checking.
				      */
    number operator() (const unsigned int i, const unsigned int j) const;
    
				     /**
				      * Return a read-write reference to
				      * the element #(i,j)#.
				      * Does the same as the #el(i,j)# function
				      * but does bounds checking.
				      */
    number& operator() (const unsigned int i, const unsigned int j);
    
				     /**
				      * Set all entries in the matrix to
				      * zero.
				      */
    void clear ();
				     //@}
    
    
				     /**@name 3: Basic applications on matrices
				      */
				     //@{
				     /**
				      *  A+=B            . Simple addition
				      */
    template<typename number2>
    void add (const number s, const FullMatrix<number2>& B);

				     /**
				      * A+=Transp(B).
				      * Simple addition of the transpose of B to this
				      */
    template<typename number2>
    void Tadd (const number s, const FullMatrix<number2>& B);
    
				     /**
				      * C=A*B.
				      * Matrix-matrix-multiplication 
				      */
 
    template<typename number2>
    void mmult (FullMatrix<number2>& C, const FullMatrix<number2>& B) const;
    
				     /**
				      * C=Transp(A)*B.
				      * Matrix-matrix-multiplication using
				      * transpose of this
				      */
    template<typename number2>
    void Tmmult (FullMatrix<number2>& C, const FullMatrix<number2>& B) const;
    
				     /**
				      *  w (+)= A*v.
				      *  Matrix-vector-multiplication ; <p>
				      *  ( application of this to a vector v )
				      *  flag adding=true : w+=A*v
				      */
    template<typename number2>
    void vmult (Vector<number2>& w, const Vector<number2>& v, const bool adding=false) const;
    
				     /**
				      *  w (+)= Transp(A)*v.
				      *  Matrix-vector-multiplication ; <p>
				      *  (application of transpose of this to a vector v)
				      *  flag adding=true : w+=A*v
				      */
    template<typename number2>
    void Tvmult (Vector<number2>& w, const Vector<number2>& v, const bool adding=false) const;

				     /**
				      * Return the norm of the vector #v# with
				      * respect to the norm induced by this
				      * matrix, i.e. $\left<v,Mv\right>$. This
				      * is useful, e.g. in the finite element
				      * context, where the $L_2$ norm of a
				      * function equals the matrix norm with
				      * respect to the mass matrix of the vector
				      * representing the nodal values of the
				      * finite element function.
				      *
				      * Note the order in which the matrix
				      * appears. For non-symmetric matrices
				      * there is a difference whether the
				      * matrix operates on the first
				      * or on the second operand of the
				      * scalar product.
				      *
				      * Obviously, the matrix needs to be square
				      * for this operation.
				      */
    template<typename number2>
    double matrix_norm (const Vector<number2> &v) const;

				     /**
				      * Build the matrix scalar product
				      * #u^T M v#. This function is mostly
				      * useful when building the cellwise
				      * scalar product of two functions in
				      * the finite element context.
				      */
    template<typename number2>
    double matrix_scalar_product (const Vector<number2> &u, const Vector<number2> &v) const;
    
				     /**
				      * A=Inverse(A). Inversion of this by
				      * Gauss-Jordan-algorithm
				      */
    void gauss_jordan ();

				     /**
                                      * Computes the determinant of a matrix.
                                      * This is only implemented for one two and
                                      * three dimensions, since for higher
                                      * dimensions the numerical work explodes.
                                      * Obviously, the matrix needs to be square
                                      * for this function.
                                      */
    double determinant () const;

				     /**
				      * Compute the quadratic matrix norm.
				      * Return value is the root of the square
				      * sum of all matrix entries.
				      */
    double norm2 () const;
				     /**
				      * Assign the inverse of the given
				      * matrix to #*this#. This function is
				      * only implemented (hardcoded) for
				      * square matrices of dimension one,
				      * two and three.
				      */
    void invert (const FullMatrix<number> &M);
				     //@}


				     /**@name 4: Basic applications on Rows or Columns
				      */
				     //@{
				     /**
				      *  A(i,1-n)+=s*A(j,1-n).
				      * Simple addition of rows of this
				      */
    void add_row (const unsigned int i, const number s, const unsigned int j);

				     /**
				      *  A(i,1-n)+=s*A(j,1-n)+t*A(k,1-n).
				      *  Multiple addition of rows of this
				      */
    void add_row (const unsigned int i,
		  const number s, const unsigned int j,
		  const number t, const unsigned int k);

				     /**
				      *  A(1-n,i)+=s*A(1-n,j).
				      *  Simple addition of columns of this
				      */
    void add_col (const unsigned int i, const number s, const unsigned int j);

				     /**
				      *  A(1-n,i)+=s*A(1-n,j)+t*A(1-n,k).
				      *  Multiple addition of columns of this
				      */
    void add_col (const unsigned int i,
		  const number s, const unsigned int j,
		  const number t, const unsigned int k);

				     /**
				      * Swap  A(i,1-n) <-> A(j,1-n).
				      * Swap rows i and j of this
				      */
    void swap_row (const unsigned int i, const unsigned int j);

				     /**
				      *  Swap  A(1-n,i) <-> A(1-n,j).
				      *  Swap columns i and j of this
				      */
    void swap_col (const unsigned int i, const unsigned int j);
				     //@}


				     /**@name 5: Mixed stuff. Including more
				      *  applications on matrices
				      */
				     //@{
				     /**
				      *  w=b-A*v.
				      *  Residual calculation , returns |w|
				      */
    template<typename number2, typename number3>
    double residual (Vector<number2>& w, const Vector<number2>& v, const Vector<number3>& b) const;

				     /**
				      *  Inversion of lower triangle .
				      */
    template<typename number2>
    void forward (Vector<number2>& dst, const Vector<number2>& src) const;

				     /**
				      *  Inversion of upper triangle .
				      */
    template<typename number2>
    void backward (Vector<number2>& dst, const Vector<number2>& src) const;

				     /**
				      * QR - factorization of a matrix.
				      * The orthogonal transformation Q is
				      * applied to the vector y and this matrix. <p>
				      * After execution of householder, the upper
				      *  triangle contains the resulting matrix R, <p>
				      * the lower the incomplete factorization matrices.
				      */
    template<typename number2>
    void householder (Vector<number2>& y);

				     /**
				      * Least - Squares - Approximation by QR-factorization.
				      */
    template<typename number2>
    double least_squares (Vector<number2>& dst, Vector<number2>& src);

				     /**
				      *  A(i,i)+=B(i,1-n). Addition of complete
				      *  rows of B to diagonal-elements of this ; <p>
				      *  ( i = 1 ... m )
				      */
    template<typename number2>
    void add_diag (const number s, const FullMatrix<number2>& B);

				     /**
				      *  A(i,i)+=s  i=1-m.
				      * Add constant to diagonal elements of this
				      */
    void diagadd (const number s);

				     /**
				      *  w+=part(A)*v. Conditional partial
				      *  Matrix-vector-multiplication <p>
				      *  (used elements of v determined by x)
				      */
    template<typename number2>
    void gsmult (Vector<number2>& w, const Vector<number2>& v, const iVector& x) const;


				     /**
				      * Output of the matrix in user-defined format.
				      */
    void print (ostream& s, int width=5, int precision=2) const;

				     /**
				      * Print the matrix in the usual format,
				      * i.e. as a matrix and not as a list of
				      * nonzero elements. For better
				      * readability, zero elements
				      * are displayed as empty space.
				      *
				      * Each entry is printed in scientific
				      * format, with one pre-comma digit and
				      * the number of digits given by
				      * #precision# after the comma, with one
				      * space following.
				      * The precision defaults to four, which
				      * suffices for most cases. The precision
				      * and output format are {\it not}
				      * properly reset to the old values
				      * when the function exits.
				      *
				      * You should be aware that this function
				      * may produce {\bf large} amounts of
				      * output if applied to a large matrix!
				      * Be careful with it.
				      */
    void print_formatted (ostream &out,
			  const unsigned int presicion=3) const;
				     //@}

				     /**
				      * Exception
				      */
    DeclException2 (ExcInvalidIndex,
		    int, int,
		    << "The given index " << arg1
		    << " should be less than " << arg2 << ".");
				     /**
				      * Exception
				      */
    DeclException2 (ExcDimensionMismatch,
		    int, int,
		    << "The two dimensions " << arg1 << " and " << arg2
		    << " do not match here.");
				     /**
				      * Exception
				      */
    DeclException0 (ExcNotQuadratic);
				     /**
				      * Exception
				      */
    DeclException0 (ExcNotRegular);
				     /**
				      * Exception
				      */
    DeclException3 (ExcInvalidDestination,
		    int, int, int,
		    << "Target region not in matrix: size in this direction="
		    << arg1 << ", size of new matrix=" << arg2
		    << ", offset=" << arg3);
				     /**
				      * Exception
				      */
    DeclException1 (ExcNotImplemented,
		    int,
		    << "This function is not implemented for the given"
		    << " matrix dimension " << arg1);
				     /**
				      * Exception
				      */
    DeclException0 (ExcIO);
};





/*-------------------------Inline functions -------------------------------*/

template <typename number>
inline number &
FullMatrix<number>::el (const unsigned int i, const unsigned int j)
{
  return val[i*dim_range+j];
};


template <typename number>
inline number
FullMatrix<number>::el (const unsigned int i, const unsigned int j) const
{
  return val[i*dim_range+j];
};


template <typename number>
inline unsigned int
FullMatrix<number>::m() const
{
  return dim_image;
};


template <typename number>
inline unsigned int
FullMatrix<number>::n() const
{
  return dim_range;
};


template <typename number>
inline number
FullMatrix<number>::el (const unsigned int i) const
{
  return val[i];
};


template <typename number>
inline number
FullMatrix<number>::operator() (const unsigned int i, const unsigned int j) const
{  
  Assert (i<dim_image, ExcInvalidIndex (i, dim_image));
  Assert (j<dim_range, ExcInvalidIndex (i, dim_range));
  return el(i,j);
};


template <typename number>
inline number &
FullMatrix<number>::operator() (const unsigned int i, const unsigned int j)
{
  Assert (i<dim_image, ExcInvalidIndex (i, dim_image));
  Assert (j<dim_range, ExcInvalidIndex (j, dim_range));
  return el(i,j);
}




/*----------------------------   fullmatrix.h     ---------------------------*/
/* end of #ifndef __lac_fullmatrix_H */
#endif
/*----------------------------   fullmatrix.h     ---------------------------*/
