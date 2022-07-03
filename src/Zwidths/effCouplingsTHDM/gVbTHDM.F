c This file is part of THDM_EWPOS.

c THDM_EWPOS is free software: you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.

c THDM_EWPOS is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License for more details.

c   You should have received a copy of the GNU General Public License along with THDM_EWPOS. If not, see <https://www.gnu.org/licenses/>.

c Copyright 2013 - 2018 Stephan Hessenberger for the Max-Planck-Institute for Physics in Munich



C This Subroutine calculates the contribution to the effective b-vector-coupling from the THDM
#include "FVbTHDM.F"
#include "FVbSMHiggs.F"


      subroutine CalcgVbTHDM(gVbTHDM,drns)
      double precision gVbTHDM
      double precision  drns, Qb, vb

#include "CTdeclarationTHDM.h"

      double complex FVbTHDM, FVbSMHiggs

#include "paraTHDM.h"
#include "paraSM.h"


      Qb=-1D0/3D0
      vb=-1D0/2D0-2D0*SW2*Qb


      call CalcFVbTHDM(FVbTHDM)
      call CalcFVbSMHiggs(FVbSMHiggs)
      gVbTHDM=1D0*(vb*(-1D0/2D0*drns
     & +1D0/2D0*dZZZTHDM+1D0/2D0*PiAA0THDM
     & -1D0/2D0*(SW2-CW2)/SW2*
     & (dMW2THDM/MW2-dMZ2THDM/MZ2))+2D0*SW*CW*Qb
     & *(1D0/2D0*dZAZTHDM+CW/SW
     & *(dMW2THDM/MW2-dMZ2THDM/MZ2))
     & +DBLE(FVbTHDM-FVbSMHiggs))


      end
