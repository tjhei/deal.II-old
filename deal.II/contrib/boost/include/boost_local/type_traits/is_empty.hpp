
// (C) Copyright Steve Cleary, Beman Dawes, Howard Hinnant & John Maddock 2000.
// Permission to copy, use, modify, sell and distribute this software is 
// granted provided this copyright notice appears in all copies. This software 
// is provided "as is" without express or implied warranty, and with no claim 
// as to its suitability for any purpose.
//
// See http://www.boost.org for most recent version including documentation.

#ifndef BOOST_TT_IS_EMPTY_HPP_INCLUDED
#define BOOST_TT_IS_EMPTY_HPP_INCLUDED

#include <boost_local/type_traits/is_convertible.hpp>
#include <boost_local/type_traits/detail/ice_or.hpp>
#include <boost_local/type_traits/config.hpp>

#ifndef BOOST_NO_TEMPLATE_PARTIAL_SPECIALIZATION
#   include <boost_local/type_traits/remove_cv.hpp>
#   include <boost_local/type_traits/is_class.hpp>
#   include <boost_local/type_traits/add_reference.hpp>
#else
#   include <boost_local/type_traits/is_reference.hpp>
#   include <boost_local/type_traits/is_pointer.hpp>
#   include <boost_local/type_traits/is_member_pointer.hpp>
#   include <boost_local/type_traits/is_array.hpp>
#   include <boost_local/type_traits/is_void.hpp>
#   include <boost_local/type_traits/detail/ice_and.hpp>
#   include <boost_local/type_traits/detail/ice_not.hpp>
#endif

// should be always the last #include directive
#include <boost_local/type_traits/detail/bool_trait_def.hpp>

namespace boost {

namespace detail {

#ifndef BOOST_NO_TEMPLATE_PARTIAL_SPECIALIZATION
template <typename T>
struct empty_helper_t1 : public T
{
    empty_helper_t1();  // hh compiler bug workaround
    int i[256];
};

struct empty_helper_t2 { int i[256]; };

#ifndef __BORLANDC__

template <typename T, bool is_a_class = false>
struct empty_helper
{
    BOOST_STATIC_CONSTANT(bool, value = false);
};

template <typename T>
struct empty_helper<T, true>
{
    BOOST_STATIC_CONSTANT(
        bool, value = (sizeof(empty_helper_t1<T>) == sizeof(empty_helper_t2))
        );
};

template <typename T>
struct is_empty_impl
{
    typedef typename remove_cv<T>::type cvt;
    BOOST_STATIC_CONSTANT(
        bool, value = (
            ::boost::type_traits::ice_or<
              ::boost::detail::empty_helper<cvt,::boost::is_class<T>::value>::value
              , BOOST_IS_EMPTY(cvt)
            >::value
            ));
};

#else // __BORLANDC__

template <typename T, bool is_a_class, bool convertible_to_int>
struct empty_helper
{
    BOOST_STATIC_CONSTANT(bool, value = false);
};

template <typename T>
struct empty_helper<T, true, false>
{
    BOOST_STATIC_CONSTANT(bool, value = (
        sizeof(empty_helper_t1<T>) == sizeof(empty_helper_t2)
        ));
};

template <typename T>
struct is_empty_impl
{
   typedef typename remove_cv<T>::type cvt;
   typedef typename add_reference<T>::type r_type;

   BOOST_STATIC_CONSTANT(
       bool, value = (
           ::boost::type_traits::ice_or<
              ::boost::detail::empty_helper<
                  cvt
                , ::boost::is_class<T>::value
                , ::boost::is_convertible< r_type,int>::value
              >::value
              , BOOST_IS_EMPTY(cvt)
           >::value));
};

#endif // __BORLANDC__

#else // BOOST_NO_TEMPLATE_PARTIAL_SPECIALIZATION

#ifdef BOOST_MSVC6_MEMBER_TEMPLATES

template <typename T>
struct empty_helper_t1 : public T
{
   empty_helper_t1();
   int i[256];
};

struct empty_helper_t2 { int i[256]; };

template <typename T>
struct empty_helper_base
{
   enum { value = (sizeof(empty_helper_t1<T>) == sizeof(empty_helper_t2)) };
};

template <typename T>
struct empty_helper_nonbase
{
   enum { value = false };
};

template <bool base>
struct empty_helper_chooser
{
   template <typename T> struct result_
   {
      typedef empty_helper_nonbase<T> type;
   };
};

template <>
struct empty_helper_chooser<true>
{
   template <typename T> struct result_
   {
      typedef empty_helper_base<T> type;
   };
};

template <typename T> 
struct is_empty_impl
{ 
   typedef ::boost::detail::empty_helper_chooser<
      ::boost::type_traits::ice_and<
         ::boost::type_traits::ice_not< ::boost::is_reference<T>::value >::value,
         ::boost::type_traits::ice_not< ::boost::is_convertible<T,double>::value >::value,
         ::boost::type_traits::ice_not< ::boost::is_pointer<T>::value >::value,
         ::boost::type_traits::ice_not< ::boost::is_member_pointer<T>::value >::value,
         ::boost::type_traits::ice_not< ::boost::is_array<T>::value >::value,
         ::boost::type_traits::ice_not< ::boost::is_void<T>::value >::value,
         ::boost::type_traits::ice_not<
            ::boost::is_convertible<T,void const volatile*>::value
            >::value
      >::value > chooser;

   typedef typename chooser::template result_<T> result;
   typedef typename result::type eh_type;

   BOOST_STATIC_CONSTANT(bool, value =
      (::boost::type_traits::ice_or<eh_type::value, BOOST_IS_EMPTY(T)>::value)); 
};

#else

template <typename T> struct is_empty_impl
{
    BOOST_STATIC_CONSTANT(bool, value = BOOST_IS_EMPTY(T));
};

#endif  // BOOST_MSVC6_MEMBER_TEMPLATES

#endif  // BOOST_NO_TEMPLATE_PARTIAL_SPECIALIZATION

} // namespace detail

BOOST_TT_AUX_BOOL_TRAIT_DEF1(is_empty,T,::boost::detail::is_empty_impl<T>::value)

} // namespace boost

#include <boost_local/type_traits/detail/bool_trait_undef.hpp>

#endif // BOOST_TT_IS_EMPTY_HPP_INCLUDED
