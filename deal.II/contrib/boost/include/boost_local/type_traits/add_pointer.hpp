
// (C) Copyright Steve Cleary, Beman Dawes, Howard Hinnant & John Maddock 2000.
// Permission to copy, use, modify, sell and distribute this software is 
// granted provided this copyright notice appears in all copies. This software 
// is provided "as is" without express or implied warranty, and with no claim 
// as to its suitability for any purpose.
//
// See http://www.boost.org for most recent version including documentation.

#ifndef BOOST_TT_ADD_POINTER_HPP_INCLUDED
#define BOOST_TT_ADD_POINTER_HPP_INCLUDED

#include <boost_local/type_traits/remove_reference.hpp>

// should be the last #include
#include <boost_local/type_traits/detail/type_trait_def.hpp>

namespace boost {

namespace detail {

template <typename T>
struct add_pointer_impl
{
    typedef typename remove_reference<T>::type no_ref_type;
    typedef no_ref_type* type;
};

} // namespace detail

BOOST_TT_AUX_TYPE_TRAIT_DEF1(add_pointer,T,typename detail::add_pointer_impl<T>::type)

} // namespace boost

#include <boost_local/type_traits/detail/type_trait_undef.hpp>

#endif // BOOST_TT_ADD_POINTER_HPP_INCLUDED
