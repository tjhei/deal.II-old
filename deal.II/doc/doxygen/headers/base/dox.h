//-------------------------------------------------------------------------
//    $Id$
//    Version: $Name$
//
//    Copyright (C) 2005 by the deal.II authors
//
//    This file is subject to QPL and may not be  distributed
//    without copyright and license information. Please refer
//    to the file deal.II/doc/license.html for the  text  and
//    further information on this license.
//
//-------------------------------------------------------------------------

/**
 * @defgroup Quadrature Quadrature formul�
 *
 * This module contains the base class Quadrature as well as the
 * quadrature formul� provided by deal.II.
 *
 * The class QIterated is used, to construct an iterated quadrature
 * formula out of an existing one, thereby increasing the accuracy of
 * the formula without increasing the order.
 *
 * While the usual quadrature formul� of higher dimensions
 * generate tensor products which are equal in each direction, the
 * class QAnisotropic generates tensor products of possibly different
 * formul� in each direction.
 *
 * The class QProjector is not actually a quadrature rule by itself,
 * but it provides functions for computing the quadrature on the
 * surfaces of higher dimensional cells.
 *
 * All other classes in this module actually implement quadrature
 * rules of different order and other characteristics.
 */
