c This file is part of THDM_EWPOS.

c THDM_EWPOS is free software: you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.

c THDM_EWPOS is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License for more details.

c   You should have received a copy of the GNU General Public License along with THDM_EWPOS. If not, see <https://www.gnu.org/licenses/>.

c Copyright 2013 - 2018 Stephan Hessenberger for the Max-Planck-Institute for Physics in Munich
#include "FVmuSM.F"


      subroutine CalcgVmuSM(gVmuSM,drSM)

      double precision gVmuSM
      double complex FVmuSM
      double precision vmu, Qmu, drSM

#include "CTdeclarationSM.h"
#include "paraSM.h"

      Qmu=-1D0
      vmu=-1D0/2D0-2D0*SW2*Qmu


      Call CalcFVmuSM(FVmuSM)

      gVmuSM=1D0*(vmu*(1D0-1D0/2D0*drSM+1D0/2D0*dZZZSM
     & +1D0/2D0*PiAA0SM+1D0/2D0*SW/CW*dZZASM
     & -1D0/2D0*(SW2-CW2)/SW2*(dMW2SM/MW2-dMZ2SM/MZ2))
     & +2D0*SW*CW*Qmu*(1D0/2D0*dZAZSM
     & +CW/SW*(dMW2SM/MW2-dMZ2SM/MZ2))
     & +DBLE(FVmuSM))


      end
