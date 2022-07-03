c This file is part of THDM_EWPOS.

c THDM_EWPOS is free software: you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.

c THDM_EWPOS is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License for more details.

c   You should have received a copy of the GNU General Public License along with THDM_EWPOS. If not, see <https://www.gnu.org/licenses/>.

c Copyright 2013 - 2018 Stephan Hessenberger for the Max-Planck-Institute for Physics in Munich

C> @brief One-loop contribution to the counterterm of the electroweak mixing angle in the SM.
C>
C> @ingroup group_SECT
C>
C> The counterterm is renormalized on-shell, following the conventions from [arXiv:0709.1075](https://arxiv.org/abs/0709.1075).
      subroutine CalcdSWsqSM
      implicit none

#include "CTdeclarationSM.h"

#include "paraSM.h"

      call CalcdMW2SM
      call CalcdMZ2SM

      dSWsqSM=CW2*(dMZ2SM/MZ2-dMW2SM/MW2)

      end
