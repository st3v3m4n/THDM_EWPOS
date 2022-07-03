* T134.F
* the T134 two-loop integral
* this file is part of FeynHiggs
* last modified 27 Jan 15 th

c This file is used here for the program THDM_EWPOS.
c The original file can be found in the program FeynHiggs (https://wwwth.mpp.mpg.de/members/heinemey/feynhiggs/cFeynHiggs.html).

c#include "externals.h"
#include "types.h"

C> @brief Implementation of the two-loop integral T134.
C>
C> The function @ref t134 just calls the function @ref t134q,
C> which takes also the renormalization scale **mudim** a input.
C>
C> @ingroup group_loopint
	RealType function T134(m1, m2, m3)
	implicit none
C> @param[in] Inputparameter for the different masses
	RealType m1, m2, m3

#include "lt.h"

	RealType T134q
	external T134q

	T134 = T134q(m1, m2, m3, mudim)
	end

************************************************************************

C> @brief This function gives the difference between the two-loop integral \f$T_{134}\f$
C> and the (D-4)-dimensional part of the one-loop integral \f$A_0\f$.
C>
C>
C> The function @ref t134sub caculates the difference
C> \f[T_{134}\left(m_1^2,m_2^2,m_3^2\right)-A_0^{(\delta)}\left(m_1^2\right)-A_0^{(\delta)}\left(m_2^2\right)-A_0^{(\delta)}\left(m_3^2\right),\f]
C> where \f$A_0^{(\delta)}\f$ is the part in the one-loop function \f$A_0\f$, which is proportional to \f$\delta=(D-4)/2\f$.
C> The parts with \f$A_0^{(\delta)}\f$ are calculated by calling the function @ref a0delq.
C> In the subroutines for the two-loop corrections to the \f$\rho\f$ parameter, the two-loop integrals are expressed in terms of
C> the functions @ref t134sub, @ref a0fh and @ref b0fh, since then all the terms with \f$A_0^{(\delta)}\f$ cancel.
C> This cancelation was checked explicitly for all the implemented two-loop corrections to \f$\Delta\rho\f$.
C>
C> @hidecallergraph
C> @ingroup group_loopint
	RealType function T134sub(m1, m2, m3)
	implicit none
C> @param[in] Inputparameter for the different masses
	RealType m1, m2, m3

#include "lt.h"

	RealType T134q, A0delq
	external T134q, A0delq

	T134sub = T134q(m1, m2, m3, mudim) -
     &  A0delq(m1, mudim) - A0delq(m2, mudim) - A0delq(m3, mudim)
	end

************************************************************************

C> @brief Implementation of the two-loop integral T134
C> with the regularization scale as input.
C>
C> @ingroup group_loopint
	RealType function T134q(m1, m2, m3, q)
	implicit none
C> @param[in] Inputparameter for the squared masses
	RealType m1, m2, m3
C> @param[in] Inputparameter for the regularization scale \f$\mu_D^2\f$
C> as input.
	RealType q

#include "lt.h"

	ComplexType Dilog
	external Dilog

	RealType x1, x2, x3, tmp, x13, x23
	ComplexType lam, a1, a2

	RealType m, mlm
	mlm(m) = m*log(m/q)*(log(m/q) - 3)

	x1 = m1
	x2 = m2
	if( x1 .gt. x2 ) then
	  x2 = x1
	  x1 = m2
	endif
	x3 = m3
	if( x2 .gt. x3 ) then
	  x3 = x2
	  x2 = m3
	endif
	if( x1 .gt. x2 ) then
	  tmp = x2
	  x2 = x1
	  x1 = tmp
	endif

	T134q = .5D0*(7 + zeta2)*(x1 + x2 + x3) + mlm(x3)

	if( x2 .eq. 0 ) then
	  T134q = T134q + x3*zeta2
	else
	  x23 = x2/x3
	  if( x1 .eq. 0 ) then
	    T134q = T134q + mlm(x2) - .5D0*x2*log(x23)**2 +
     &        (x3 - x2)*Re(Dilog(ToComplex(1 - x23)))
	  else
	    x13 = x1/x3
	    lam = Sqrtc((x13 - x23)**2 - 2*(x13 + x23) + 1)
	    a1 = .5D0*(1 + x13 - x23 - lam)
	    a2 = .5D0*(1 - x13 + x23 - lam)
	    lam = lam*(log(a1)*log(a2) -
     &        Dilog(a1) - Dilog(a2) -
     &        .5D0*log(x13)*log(x23) + zeta2)
	    T134q = T134q + mlm(x1) + mlm(x2) +
     &        .25D0*( (x1 - x2 - x3)*log(x23)**2 +
     &                (x2 - x1 - x3)*log(x13)**2 +
     &                (x3 - x1 - x2)*log(x1/x2)**2 ) +
     &        x3*Re(lam)
	  endif
	endif
	end

************************************************************************
* Spira's Delta b needs this

	RealType function T134s(m1, m2, m3, q)
	implicit none
	RealType m1, m2, m3, q

#include "lt.h"

	RealType T134q
	external T134q

	T134s = (T134q(m1, m2, m3, q) - .5D0*zeta2*(m1 + m2 + m3))/q
	end
