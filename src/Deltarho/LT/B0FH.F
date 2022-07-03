c B0FH.F
c the scalar two-point function
c this function is adapted from Ansgar Denners bcanew.f
c to the conventions of LoopTools
c this file is part of FeynHiggs
c last modified 1 Jan 15 th

c This file is used here for the program THDM_EWPOS.
c The original file can be found in the program FeynHiggs (https://wwwth.mpp.mpg.de/members/heinemey/feynhiggs/cFeynHiggs.html).

c#include "externals.h"
#include "types.h"

C> @brief Implementation of the one-loop integral \f$B_0\left(p^2,m_1^2,m_2^2\right)\f$ (taken from FeynHiggs).
C>
C> Internally the function @ref b0q is called with the
C> regularization scale **mudim** as input.
C> The regularization scale **mudim** is set by the subroutine @ref set2lregpara,
C> which has to be called before evaluating @ref b0fh.
C>
C> @hidecallergraph
C> @ingroup group_loopint
	ComplexType function B0FH(p, m1, m2)
	implicit none
C> @param[in] Inputparameter for the squared momentum \f$p^2\f$
	RealType p
C> @param[in] Inputparameter for the squared mass \f$m_1^2\f$
	RealType m1
C> @param[in] Inputparameter for the squared mass \f$m_2^2\f$
	RealType m2

#include "lt.h"

	ComplexType B0q
	external B0q

	B0FH = B0q(p, m1, m2, mudim)
	end

************************************************************************

C> @brief Implementation of the one-loop integral \f$B_0\left(p^2,m_1^2,m_2^2\right)\f$
C>  with the regularization scale as input
C> (taken from FeynHiggs).
C>
C> This implementation contains also the UV-divergent part
C> **delta**.
C> Note that only the finite part of \f$B_0\left(p^2,m_1^2,m_2^2\right)\f$ has to be used in the subroutines for
C> the two-loop corrections to \f$\Delta\rho\f$.
C> Therefore, the parameter **delta** is set to zero in these subroutines by calling the subroutine @ref set2lregpara.
C>
C>
C> @ingroup group_loopint
	ComplexType function B0q(p, m1, m2, q)
	implicit none
C> @param[in] Inputparameter for the squared momentum \f$p^2\f$
	RealType p
C> @param[in] Inputparameter for the squared mass \f$m_1^2\f$
	RealType m1
C> @param[in] Inputparameter for the squared mass \f$m_2^2\f$
	RealType m2
C> @param[in] Inputparameter for the regularization scale \f$\mu_D^2\f$
	RealType q

#include "lt.h"

	ComplexType fpv, xlogx
	external fpv, xlogx

	ComplexType x1, x2, y1, y2, r
	RealType minacc

	minacc = diffeps*(m1 + m2)
* general case
	if( abs(p) .gt. minacc ) then
	  call roots(p, m1, m2, x1, x2, y1, y2, r)
	  if( abs(y1) .gt. .5D0 .and. abs(y2) .gt. .5D0 ) then
	    B0q = -log(m2/q) - fpv(1, x1, y1) - fpv(1, x2, y2)
	  else if( abs(x1) .lt. 10 .and. abs(x2) .lt. 10 ) then
	    B0q = 2 - log(p*oneMeps/q) +
     &        xlogx(-x1) + xlogx(-x2) - xlogx(y1) - xlogx(y2)
	  else if( abs(x1) .gt. .5D0 .and. abs(x2) .gt. .5D0 ) then
	    B0q = -log(m1/q) - fpv(1, y1, x1) - fpv(1, y2, x2)
	  else
	    print *, "B0(", p, ",", m1, ",", m2, ") not defined"
	    B0q = nan
	  endif
* zero momentum
	else if( abs(m1 - m2) .gt. minacc ) then
	  x2 = oneMeps*m1/(m1 - m2)
	  y2 = oneMeps*m2/(m2 - m1)
	  if( abs(y2) .gt. .5D0 ) then
	    B0q = -log(m2/q) - fpv(1, x2, y2)
	  else
	    B0q = -log(m1/q) - fpv(1, y2, x2)
	  endif
	else
	  B0q = -log(m2/q)
	endif
        B0q = B0q + delta
	end
