c This file is part of THDM_EWPOS.

c THDM_EWPOS is free software: you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.

c THDM_EWPOS is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License for more details.

c   You should have received a copy of the GNU General Public License along with THDM_EWPOS. If not, see <https://www.gnu.org/licenses/>.

c Copyright 2013 - 2018 Stephan Hessenberger for the Max-Planck-Institute for Physics in Munich


#include "types.h"

C> @brief This subroutine sets the regularization parameters for the two-loop contributions to \f$\Delta\rho\f$
C>
C> The regularization parameters **delta** and **mudim** are defined in the header-file lt.h.
C>
C> @ingroup group_loopint
      subroutine Set2Lregpara()
      implicit none

#include "looptools.h"
#include "lt.h"

C> In difference to the implementation of the finite part of the two-loop integral @ref t134,
C> the implementations of the one-loop integrals \f$A_0\f$ and \f$B_0\f$ in @ref a0fh and @ref b0fh contain also
C> the divergent part in form of the parameter **delta**.
C> Since only the finite parts of \f$A_0\f$ and \f$B_0\f$ have to be used in the subroutines for
C> the two-loop corrections to \f$\Delta\rho\f$, the parameter **delta** is set to zero.
      delta=0D0
C> In order to be consistent, the regularization parameter **mudim** is set equal to
C> the value used in the LoopTools functions.
      Mudim=Getmudim()

      end
