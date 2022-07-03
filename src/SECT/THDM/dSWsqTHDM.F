c This file is part of THDM_EWPOS.

c THDM_EWPOS is free software: you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.

c THDM_EWPOS is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License for more details.

c   You should have received a copy of the GNU General Public License along with THDM_EWPOS. If not, see <https://www.gnu.org/licenses/>.

c Copyright 2013 - 2018 Stephan Hessenberger for the Max-Planck-Institute for Physics in Munich


C> @brief Non-standard one-loop contribution to the counterterm of the electroweak mixing angle.
C>
C> @ingroup group_SECT
C>
C> The counterterm is renormalized on-shell, following the conventions from [arXiv:0709.1075](https://arxiv.org/abs/0709.1075).
      subroutine CalcdSWsqTHDM
      implicit none

#include "CTdeclarationTHDM.h"

#include "paraTHDM.h"
#include "paraSM.h"

      call CalcdMW2THDM
      call CalcdMZ2THDM

      dSWsqTHDM=CW2*(dMZ2THDM/MZ2-dMW2THDM/MW2)

      end
