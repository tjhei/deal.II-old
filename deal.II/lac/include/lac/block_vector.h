//----------------------------  block_vector.h  ---------------------------
//    $Id$
//    Version: $Name$
//
//    Copyright (C) 1998, 1999, 2000 by the deal.II authors
//
//    This file is subject to QPL and may not be  distributed
//    without copyright and license information. Please refer
//    to the file deal.II/doc/license.html for the  text  and
//    further information on this license.
//
//----------------------------  block_vector.h  ---------------------------
#ifndef __deal2__block_vector_h
#define __deal2__block_vector_h


#include <lac/vector.h>
#include <base/exceptions.h>
#include <cstdio>
#include <vector>

/**
 * Several vectors of data. 
 *
 * The BlockVector is a collection of normal LAC-#Vector#s. Each of
 * the vectors inside can have a different size.
 *
 * The functionality of #BlockVector# includes everything, a #Vector#
 * can do, plus the access to a single #Vector# inside the
 * #BlockVector# by #block(i)#.
 *
 * @author Guido Kanschat, 1999
 */
template <int n_blocks, typename Number>
class BlockVector
{
  public:
				     /**
				      *  Dummy-Constructor. Dimension=0
				      */
    BlockVector ();
    
				     /**
				      * Copy-Constructor. Dimension set to
				      * that of V, all components are copied
				      * from V
				      */
    BlockVector (const BlockVector<n_blocks,Number>& V);


// note: I disabled this function for the time being, since egcs1.1.2
// does not respect the "explicit" keyword for template constructors.
// this leads to unwanted conversions and in some places to automatically
// generated temporaries, where this is not a good idea. [WB]
// 				     /**
// 				      * Copy constructor taking a BlockVector of
// 				      * another data type. This will fail if
// 				      * there is no conversion path from
// 				      * #OtherNumber# to #Number#. Note that
// 				      * you may lose accuracy when copying
// 				      * to a BlockVector with data elements with
// 				      * less accuracy.
// 				      */
//     template <typename OtherNumber>
//     explicit
//     BlockVector (const BlockVector<OtherNumber> &v);
    
				     /**
				      * Constructor. Set dimension to #n# and
				      * initialize all elements with zero.
				      */
    BlockVector (const vector<unsigned int>& n);

                                     /**
				      * Destructor. Clears memory
				      */
    ~BlockVector ();

				     /**
				      * Access to a single block.
				      */
    Vector<Number>& block(unsigned int i);
    
				     /**
				      * Read-only access to a single block.
				      */
    const Vector<Number>& block(unsigned int i) const;
    
				     /**
				      * Set all entries to zero. Equivalent to
				      * #v = 0#, but more obvious and faster.
				      * Note that this function does not change
				      * the size of the vector, unlike the
				      * STL's #vector<>::clear# function.
				      */
    void clear ();
    
				     /**
				      * $U(0-N) = s$: fill all components.
				      */
    BlockVector<n_blocks,Number> & operator= (const Number s);
    
				     /**
				      *  $U = V$: copy all components.
				      */
    BlockVector<n_blocks,Number> &
    operator= (const BlockVector<n_blocks,Number>& V);

				     /**
				      * $U = V$ for different types.
				      */
    template<typename Number2>
    BlockVector<n_blocks,Number> &
    operator= (const BlockVector<n_blocks, Number2>& V);
    
				     /**
				      * $U = U * V$: scalar product.
				      */
    Number operator* (const BlockVector<n_blocks,Number>& V) const;

				     /**
				      * Return square of the $l_2$-norm.
				      */
    Number norm_sqr () const;

				     /**
				      * Return the mean value of the elements of
				      * this vector.
				      */
    Number mean_value () const;

				     /**
				      * Return the $l_1$-norm of the vector, i.e.
				      * the sum of the absolute values.
				      */
    Number l1_norm () const;

				     /**
				      * Return the $l_2$-norm of the vector, i.e.
				      * the square root of the sum of the
				      * squares of the elements.
				      */
    Number l2_norm () const;

				     /**
				      * Return the maximum absolute value of the
				      * elements of this vector, which is the
				      * $l_\infty$-norm of a vector.
				      */
    Number linfty_norm () const;


/**
				      * Change the dimension of the vector to
				      * #N#. The reserved memory for this vector
				      * remains unchanged if possible, to make
				      * things faster, but this may waste some
				      * memory, so take this in the back of your
				      * head.
				      * However, if #N==0# all memory is freed,
				      * i.e. if you want to resize the vector
				      * and release the memory not needed, you
				      * have to first call #reinit(0)# and then
				      * #reinit(N)#. This cited behaviour is
				      * analogous to that of the STL containers.
				      *
				      * On #fast==false#, the vector is filled by
				      * zeros.
				      */ 
    void reinit (const vector<unsigned int>& N,
		 const bool         fast=false);
    
				     /**
				      * Change the dimension to that of the
				      * vector #V#. The same applies as for
				      * the other #reinit# function.
				      *
				      * The elements of #V# are not copied, i.e.
				      * this function is the same as calling
				      * #reinit (V.size(), fast)#.
				      */
    void reinit (const BlockVector<n_blocks,Number> &V,
		 const bool            fast=false);
    
  				     /**
  				      * Return dimension of the vector. This is the
				      * sum of the dimensions of all components.
  				      */
    unsigned int size () const;

				     /**
				      * Return whether the vector contains only
				      * elements with value zero. This function
				      * is mainly for internal consistency
				      * check and should seldomly be used when
				      * not in debug mode since it uses quite
				      * some time.
				      */
    bool all_zero () const;

// See above.    
//  				     /**
//  				      * Make the #Vector# class a bit like the
//  				      * #vector<># class of the C++ standard
//  				      * library by returning iterators to
//  				      * the start and end of the elements of this
//  				      * vector.
//  				      */
//      iterator begin ();

//  				     /**
//  				      * Return constant iterator to the start of
//  				      * the vectors.
//  				      */
//      const_iterator begin () const;

//  				     /**
//  				      * Return an iterator pointing to the
//  				      * element past the end of the array.
//  				      */
//      iterator end ();

//      				     /**
//  				      * Return a constant iterator pointing to
//  				      * the element past the end of the array.
//  				      */
//      const_iterator end () const;  


/**
				      * @name 2: Data-Access
				      */
				     //@{
				     /**
				      * Access components, returns U(i).
				      */
    Number operator() (const unsigned int i) const;
    
				     /**
				      * Access components, returns U(i)
				      * as a writeable reference.
				      */
    Number& operator() (const unsigned int i);
				     //@}


/**
				      * @name 3: Modification of vectors
				      */
				     //@{
				     /**
				      * Addition operator.
				      * Fast equivalent to #U.add(1, V)#.
				      */
    BlockVector<n_blocks,Number> &
    operator += (const BlockVector<n_blocks,Number> &V);

    				     /**
				      * Subtraction operator.
				      * Fast equivalent to #U.add(-1, V)#.
				      */
    BlockVector<n_blocks,Number> &
    operator -= (const BlockVector<n_blocks,Number> &V);

				     /**
				      * $U(0-DIM)+=s$.
				      * Addition of #s# to all components. Note
				      * that #s# is a scalar and not a vector.
				      */
    void add (const Number s);
    
				     /**
				      * U+=V.
				      * Simple vector addition, equal to the
				      * #operator +=#.
				      */
    void add (const BlockVector<n_blocks,Number>& V);
    
				     /**
				      * U+=a*V.
				      * Simple addition of a scaled vector.
				      */
    void add (const Number a, const BlockVector<n_blocks,Number>& V);
    
				     /**
				      * U+=a*V+b*W.
				      * Multiple addition of scaled vectors.
				      */
    void add (const Number a, const BlockVector<n_blocks,Number>& V,
	      const Number b, const BlockVector<n_blocks,Number>& W);
    
				     /**
				      * U=s*U+V.
				      * Scaling and simple vector addition.
				      */
    void sadd (const Number s, const BlockVector<n_blocks,Number>& V);
    
				     /**
				      * U=s*U+a*V.
				      * Scaling and simple addition.
				      */
    void sadd (const Number s, const Number a, const BlockVector<n_blocks,Number>& V);
    
				     /**
				      * U=s*U+a*V+b*W.
				      * Scaling and multiple addition.
				      */
    void sadd (const Number s, const Number a,
	       const BlockVector<n_blocks,Number>& V,
	       const Number b, const BlockVector<n_blocks,Number>& W);
    
				     /**
				      * U=s*U+a*V+b*W+c*X.
				      * Scaling and multiple addition.
				      */
    void sadd (const Number s, const Number a,
	       const BlockVector<n_blocks,Number>& V,
	       const Number b, const BlockVector<n_blocks,Number>& W, 
	       const Number c, const BlockVector<n_blocks,Number>& X);
    
				     /**
				      * Scale each element of the vector by the
				      * given factor. This function was
				      * previously called #equ(Number)#, which
				      * in my eyes is an extremely unintuitive
				      * naming and was thus replaced.
				      */
    void scale (const Number factor);
    
				     /**
				      *  U=a*V. Replacing.
				      */
    void equ (const Number a, const BlockVector<n_blocks,Number>& V);
    
				     /**
				      * U=a*V+b*W.
				      * Replacing by sum.
				      */
    void equ (const Number a, const BlockVector<n_blocks,Number>& V,
	      const Number b, const BlockVector<n_blocks,Number>& W);

				     //@}


/**
				      * @name 5: Mixed stuff
				      */
				     //@{
				     /**
				      *  Output of vector in user-defined format.
				      */
    void print (FILE* fp, const char* format = 0) const;
    
				     /**
				      *  Output of vector in user-defined format.
				      */
    void print (const char* format = 0) const;

				     /**
				      * Print to a stream.
				      * 
				      */
    void print (ostream &, unsigned int precision = 3,
		bool scientific = true,
		bool across = true) const;

				     /**
				      * Write the vector en bloc to a file. This
				      * is done in a binary mode, so the output
				      * is neither readable by humans nor 
				      * (probably) by other computers using
				      * a different operating system of number
				      * format.
				      */
    void block_write (ostream &out) const;

				     /**
				      * Read a vector en block from a file. This
				      * is done using the inverse operations to
				      * the above function, so it is reasonably
				      * fast because the bitstream is not
				      * interpreted.
				      *
				      * The vector is resized if necessary.
				      *
				      * A primitive form of error checking is
				      * performed which will recognize the
				      * bluntest attempts to interpret some
				      * data as a vector stored bitwise to a
				      * file, but not more.
				      */
    void block_read (istream &in);
				     //@}

				     /**
				      * Exception
				      */
    DeclException2 (ExcDimensionsDontMatch,
		    int, int,
		    << "The dimensions " << arg1 << " and " << arg2
		    << " do not match here.");
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
    DeclException1 (ExcInvalidNumber,
		    int,
		    << "The provided number is invalid here: " << arg1);
				     /**
				      * Exception
				      */
    DeclException0 (ExcOutOfMemory);
				     /**
				      * Exception
				      */
    DeclException0 (ExcEmptyVector);
				     /**
				      * Exception
				      */
    DeclException0 (ExcIO);

  protected:
				     /**
				      * Pointer to the array of components.
				      */
    Vector<Number> components[n_blocks];

                                     /**
				      * Global starting index of each vector.
				      * The last andredundant value is the total
				      * number of entries.
				      */
    unsigned int start[n_blocks+1];
};


/*----------------------- Inline functions ----------------------------------*/


template <int n_blocks, typename Number>
inline
unsigned int BlockVector<n_blocks,Number>::size () const
{
  return start[n_blocks];
}


//  template <int n_blocks, typename Number>
//  inline
//  BlockVector<n_blocks,Number>::iterator BlockVector<n_blocks,Number>::begin () {
//    return &val[0];
//  };


//  template <int n_blocks, typename Number>
//  inline
//  BlockVector<n_blocks,Number>::const_iterator BlockVector<n_blocks,Number>::begin () const {
//    return &val[0];
//  };


//  template <int n_blocks, typename Number>
//  inline
//  BlockVector<n_blocks,Number>::iterator BlockVector<n_blocks,Number>::end () {
//    return &val[dim];
//  };


//  template <typename Number>
//  inline
//  BlockVector<n_blocks,Number>::const_iterator BlockVector<n_blocks,Number>::end () const {
//    return &val[dim];
//  };


template <int n_blocks, typename Number>
inline
Number BlockVector<n_blocks,Number>::operator() (const unsigned int i) const
{
  //  Assert (i<start[n_blocks], ExcInvalidIndex(i,dim));

  int j=n_blocks-1;
    while ((start[j]>i) && (j!=0)) --j;

  return components[j](i-start[j]);
}


template <int n_blocks, typename Number>
inline
Number& BlockVector<n_blocks,Number>::operator() (const unsigned int i)
{
  //  Assert (i<dim, ExcInvalidIndex(i,dim));

  int j=n_blocks-1;
    while ((start[j]>i) && (j!=0)) --j;

  return components[j](i-start[j]);
}

template <int n_blocks, typename Number>
inline
Vector<Number>&
BlockVector<n_blocks,Number>::block(unsigned int i)
{
  Assert(i<n_blocks, ExcIndexRange(i,0,n_blocks));

  return components[i];
}

template <int n_blocks, typename Number>
inline
const Vector<Number>&
BlockVector<n_blocks,Number>::block(unsigned int i) const
{
  Assert(i<n_blocks, ExcIndexRange(i,0,n_blocks));

  return components[i];
}


#endif
