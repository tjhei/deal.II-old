
// (C) Copyright Dave Abrahams, Steve Cleary, Beman Dawes, Howard
// Hinnant & John Maddock 2000.  Permission to copy, use, modify,
// sell and distribute this software is granted provided this
// copyright notice appears in all copies. This software is provided
// "as is" without express or implied warranty, and with no claim as
// to its suitability for any purpose.
//
// See http://www.boost.org for most recent version including documentation.

#ifndef BOOST_TT_IS_ARRAY_HPP_INCLUDED
#define BOOST_TT_IS_ARRAY_HPP_INCLUDED

#include <boost_local/type_traits/config.hpp>

#ifdef BOOST_NO_TEMPLATE_PARTIAL_SPECIALIZATION
#   include <boost_local/type_traits/detail/yes_no_type.hpp>
#   include <boost_local/type_traits/detail/wrap.hpp>
#endif

#include <cstddef>

// should be the last #include
#include <boost_local/type_traits/detail/bool_trait_def.hpp>

namespace boost {

#ifndef BOOST_NO_TEMPLATE_PARTIAL_SPECIALIZATION

BOOST_TT_AUX_BOOL_TRAIT_DEF1(is_array,T,false)
BOOST_TT_AUX_BOOL_TRAIT_PARTIAL_SPEC1_2(typename T,std::size_t N,is_array,T[N],true)
BOOST_TT_AUX_BOOL_TRAIT_PARTIAL_SPEC1_2(typename T,std::size_t N,is_array,T const[N],true)
BOOST_TT_AUX_BOOL_TRAIT_PARTIAL_SPEC1_2(typename T,std::size_t N,is_array,T volatile[N],true)
BOOST_TT_AUX_BOOL_TRAIT_PARTIAL_SPEC1_2(typename T,std::size_t N,is_array,T const volatile[N],true)

#else // BOOST_NO_TEMPLATE_PARTIAL_SPECIALIZATION

namespace detail {

using ::boost::type_traits::yes_type;
using ::boost::type_traits::no_type;
using ::boost::type_traits::wrap;

template< typename T > T(* is_array_tester1(wrap<T>) )(wrap<T>);
char BOOST_TT_DECL is_array_tester1(...);

template< typename T> no_type is_array_tester2(T(*)(wrap<T>));
yes_type BOOST_TT_DECL is_array_tester2(...);

template< typename T >
struct is_array_impl
{ 
    BOOST_STATIC_CONSTANT(bool, value = 
        sizeof(::boost::detail::is_array_tester2(
            ::boost::detail::is_array_tester1(
                ::boost::type_traits::wrap<T>()
                )
        )) == 1
    );
};

} // namespace detail

BOOST_TT_AUX_BOOL_TRAIT_DEF1(is_array,T,::boost::detail::is_array_impl<T>::value)
#ifndef BOOST_NO_CV_VOID_SPECIALIZATIONS
BOOST_TT_AUX_BOOL_TRAIT_SPEC1(is_array,void,false)
BOOST_TT_AUX_BOOL_TRAIT_SPEC1(is_array,void const,false)
BOOST_TT_AUX_BOOL_TRAIT_SPEC1(is_array,void volatile,false)
BOOST_TT_AUX_BOOL_TRAIT_SPEC1(is_array,void const volatile,false)
#endif

#endif // BOOST_NO_TEMPLATE_PARTIAL_SPECIALIZATION

} // namespace boost

#include <boost_local/type_traits/detail/bool_trait_undef.hpp>

#endif // BOOST_TT_IS_ARRAY_HPP_INCLUDED
