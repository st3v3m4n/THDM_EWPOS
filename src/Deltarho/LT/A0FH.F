* A0FH.F
* the scalar one-point function
* this function is adapted from Ansgar Denners bcanew.f
* to the conventions of LoopTools
* this file is part of FeynHiggs
* last modified 30 Nov 11 th

c This file is used here for the program THDM_EWPOS.
c The original file can be found in the program FeynHiggs (https://wwwth.mpp.mpg.de/members/heinemey/feynhiggs/cFeynHiggs.html).

c#include "externals.h"
#include "types.h"

C> @brief Implementation of the one-loop integral \f$A_0\left(m^2\right)\f$ (taken from FeynHiggs).
C>
C> Internally the function @ref a0q is called with the
C> regularization scale **mudim** as input.
C> The regularization scale **mudim** is set by the subroutine @ref set2lregpara,
C> which has to be called before evaluating @ref a0fh.
C>
C> @hidecallergraph
C> @ingroup group_loopint
      ComplexType function A0FH(m)
      implicit none
C> @param[in]  Inputparameter for the squared mass.
      RealType m

#include "lt.h"

      ComplexType A0q
      external A0q

      A0FH = A0q(m, mudim)
      end

************************************************************************

C> @brief Implementation of the one-loop integral \f$A_0\left(m^2\right)\f$
C>  with the regularization scale as input
C> (taken from FeynHiggs).
C>
C> This implementation contains also the UV-divergent part
C> **delta**.
C> Note that only the finite part of \f$A_0\left(m^2\right)\f$ has to be used in the subroutines for
C> the two-loop corrections to \f$\Delta\rho\f$.
C> Therefore, the parameter **delta** is set to zero in these subroutines by calling the subroutine @ref set2lregpara.
C>
C>
C> @ingroup group_loopint
      ComplexType function A0q(m, q)
      implicit none
C> @param[in] Inputparameter for the squared mass.
      RealType m
C> @param[in] Inputparameter for the regularization scale \f$\mu_D^2\f$
      RealType q

#include "lt.h"

      if( m .eq. 0 ) then
        A0q = 0
      else
        A0q = m*(1 - log(m/q) + delta)
      endif
      end
