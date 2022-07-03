* A0del.F
* the (D-4)-dimensional part of A0
* this file is part of FeynHiggs
* last modified 27 Jan 15 th

c This file is used here for the program THDM_EWPOS.
c The original file can be found in the program FeynHiggs (https://wwwth.mpp.mpg.de/members/heinemey/feynhiggs/cFeynHiggs.html).


c#include "externals.h"
#include "types.h"

C> @brief Implementation of the (D-4)-dimensional part of \f$A_0\left(m^2\right)\f$.
C>
C> This function caculates the part of the scalar one-point function \f$A_0\f$,
C> which is proportional to \f$\delta=(D-4)/2\f$.
C> Internally the function @ref a0delq is called with the
C> regularization scale **mudim** as input.
C> The regularization scale **mudim** is set by the subroutine @ref set2lregpara,
C> which has to be called before evaluating @ref a0del.
C>
C> @ingroup group_loopint
	RealType function A0del(m)
	implicit none
C> @param[in] Inputparameter for the squared mass.
	RealType m

#include "lt.h"

	RealType A0delq
	external A0delq

	A0del = A0delq(m, mudim)
	end

************************************************************************

C> @brief The (D-4)-dimensional part of \f$A_0\left(m^2\right)\f$ with
C> the regularization scale as input.
C>
C> @ingroup group_loopint
	RealType function A0delq(m, q)
	implicit none
C> @param[in] Inputparameter for the squared mass.
	RealType m
C> @param[in] Inputparameter for the regularization scale \f$\mu_D^2\f$
	RealType q

#include "lt.h"

	if( m .ne. 0 ) then
	  A0delq = .5D0*m*((1 - log(m/q))**2 + 1 + zeta2)
	else
	  A0delq = 0
	endif
	end
