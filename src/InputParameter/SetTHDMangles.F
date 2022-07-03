c This file is part of THDM_EWPOS.

c THDM_EWPOS is free software: you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.

c THDM_EWPOS is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License for more details.

c   You should have received a copy of the GNU General Public License along with THDM_EWPOS. If not, see <https://www.gnu.org/licenses/>.

c Copyright 2013 - 2018 Stephan Hessenberger for the Max-Planck-Institute for Physics in Munich



c> @brief This subroutine sets the different trigonometric functions of the mixing angles in the THDM.
c>
c> @ingroup group_Input

C> The different trigonometric functions of the THDM mixing angles \f$\alpha\f$ and \f$\beta\f$
C> are defined in the header-file paraTHDM.h.
C> They are calculated in terms of \f$\tan\beta\f$ and
C> \f$\sin\alpha\f$, which have to be given as input.
      subroutine SetTHDMangles

#include "paraTHDM.h"

C> The trigonometric functions are given by the following parameters
C> (with the abbreviations \f$\sin x=s_x\f$, \f$\cos x=c_x\f$ and \f$\tan x=t_x\f$):
C> - TB2: \f$t_\beta^2\f$
      TB2=TB**2
C> - SB: \f$s_\beta\f$
      SB=TB/SQRT(1+TB2)
C> - CB: \f$c_\beta\f$
      CB=1D0/Sqrt(1+TB2)
C> - SB2: \f$s_\beta^2\f$
      SB2=SB**2
C> - CB2: \f$c_\beta^2\f$
      CB2=CB**2
C> - SA2: \f$s_\alpha^2\f$
      SA2=SA**2
C> - CA2: \f$c_\alpha^2\f$
      CA2=1D0-SA2
C> - CA: \f$c_\alpha\f$
      CA=Sqrt(CA2)
C> - SBA: \f$s_{\beta-\alpha}\f$
      SBA=SB*CA-CB*SA
C> - SBA2: \f$s_{\beta-\alpha}^2\f$
      SBA2=SBA**2
C> - CBA: \f$c_{\beta-\alpha}\f$
      CBA=CB*CA+SB*SA
C> - CBA2: \f$c_{\beta-\alpha}^2\f$
      CBA2=CBA**2
C> - SAB: \f$s_{\alpha+\beta}\f$
      SAB=SA*CB+CA*SB
C> - CAB: \f$c_{\alpha+\beta}\f$
      CAB=CA*CB-SA*SB
C> - S2A: \f$s_{2\alpha}\f$
      S2A=2*SA*CA
C> - S2B: \f$s_{2\beta}\f$
      S2B=2*SB*CB
C> - C2A: \f$c_{2\alpha}\f$
      C2A=CA2-SA2
C> - C2B: \f$c_{2\beta}\f$
      C2B=CB2-SB2

      end
