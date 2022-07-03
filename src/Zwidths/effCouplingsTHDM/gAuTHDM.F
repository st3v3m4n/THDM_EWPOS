c This file is part of THDM_EWPOS.

c THDM_EWPOS is free software: you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.

c THDM_EWPOS is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License for more details.

c   You should have received a copy of the GNU General Public License along with THDM_EWPOS. If not, see <https://www.gnu.org/licenses/>.

c Copyright 2013 - 2018 Stephan Hessenberger for the Max-Planck-Institute for Physics in Munich


c This subroutine calculates the contribution to the effective up type axial-coupling from the THDM


      subroutine CalcgAuTHDM(gAuTHDM,drns)
      double precision gAuTHDM
      double precision  drns, au
#include "CTdeclarationTHDM.h"
#include "paraTHDM.h"
#include "paraSM.h"

      au=1D0/2D0

      gAuTHDM=1D0*(au*(-1D0/2D0*drns
     & +1D0/2D0*dZZZTHDM
     & +1D0/2D0*PiAA0THDM
     & -1D0/2D0*(SW2-CW2)/SW2*
     & (dMW2THDM/MW2-dMZ2THDM/MZ2)))


      end
