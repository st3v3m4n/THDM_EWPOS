c This file is part of THDM_EWPOS.

c THDM_EWPOS is free software: you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.

c THDM_EWPOS is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License for more details.

c   You should have received a copy of the GNU General Public License along with THDM_EWPOS. If not, see <https://www.gnu.org/licenses/>.

c Copyright 2013 - 2018 Stephan Hessenberger for the Max-Planck-Institute for Physics in Munich


#include "types.h"


C> @brief This subroutine calculates the leptonic mixing angle in the SM at 1Loop.
C>
C> @ingroup group_mixangles
      subroutine CalcslepSM1Loop(slepSM1Loop)
      RealType gVeSM, gAeSM
C> @param[out] The parameter slepSM1Loop gives the leptonic mixing angle in the SM
C> at the one-loop order.
      RealType slepSM1Loop
      RealType DelKSM1Loop
#include "CTdeclarationSM.h"
#include "paraSM.h"


      call CalcDelKSM1Loop(DelKSM1Loop,1)

      slepSM1Loop=SW2*(1+DelKSM1Loop)


      end
