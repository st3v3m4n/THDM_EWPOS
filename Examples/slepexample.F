c This file is part of THDM_EWPOS.

c THDM_EWPOS is free software: you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.

c THDM_EWPOS is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License for more details.

c   You should have received a copy of the GNU General Public License along with THDM_EWPOS. If not, see <https://www.gnu.org/licenses/>.

c Copyright 2013 - 2018 Stephan Hessenberger for the Max-Planck-Institute for Physics in Munich


      program slepexample
      double precision slep
      double precision drSM, drns
#include "paraTHDM.h"
#include "paraSM.h"

      Print*,"------------------------------------------------------------------------------------"
      Print*,"Results for the effective leptonic mixing angle in the THDM with the alignment limit"
      Print*,"------------------------------------------------------------------------------------"


      call SetSMParameter

      Mh0=125D0
      Mh02=Mh0**2
      MHH=500D0
      MHH2=MHH**2
      MA0=550D0
      MA02=MA0**2
      MHp=570D0
      MHp2=MHp**2
      TB=1D0
      TB2=TB**2
      SBA=1D0
      SBA2=1D0
      CBA=0D0
      CBA2=0D0
      Lambda5=0D0

      call printSMpara
      call printTHDMpara(1)
      call ltini
      Print*,"Mudim:", Getmudim()

      Print*,"**********************************************"
      Print*, "Result with non-standard one-loop correction:"
      call CalcMWTHDM(drSM,drns,1,0)
      Print*,"MW:",MW
      call CalcslepTHDM(slep,0)
      Print*,"effective leptonic mixing angle:",slep
      Print*,"**********************************************"
      Print*, "Result with non-standard one- and two-loop corrections:"
      call CalcMWTHDM(drSM,drns,1,6)
      Print*,"MW:",MW
      call CalcslepTHDM(slep,5)
      Print*,"effective leptonic mixing angle:",slep



      end
