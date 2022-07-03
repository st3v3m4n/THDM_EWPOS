c This file is part of THDM_EWPOS.

c THDM_EWPOS is free software: you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.

c THDM_EWPOS is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License for more details.

c   You should have received a copy of the GNU General Public License along with THDM_EWPOS. If not, see <https://www.gnu.org/licenses/>.

c Copyright 2013 - 2018 Stephan Hessenberger for the Max-Planck-Institute for Physics in Munich


      program NoAlignexample
      double precision drSM, drns, drhoAllH2L
      Integer J


#include "paraTHDM.h"
#include "paraSM.h"

      Print*,"-----------------------------------------------------------------------"
      Print*,"Results for precision observables in the general THDM without alignment"
      Print*,"-----------------------------------------------------------------------"

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
      SA=0.6D0
      m12sq=0D0
      Lambda6=0D0
      Lambda7=0D0
      call SetTHDMangles

      call printSMpara
      call printTHDMpara(2)
      call ltini
      Print*,"mudim:",GetMudim()

      Print*,"**********************************************"
      Print*,"Results with MS-Bar renormalization of off-diagonal mass-CT:"
      Print*,"**********************************************"
      call CalcMWTHDM(drSM,drns,1,8)
      Print*, "Result for MW:", MW

      call CalcAllH2Loop(drhoAllH2L,1)
      Print*,"Non-standard two-loop correction to the rho parameter:"
     &  , drhoAllH2L

      Print*,"**********************************************"
      Print*,"Results with on-shell renormalization of off-diagonal mass-CT:"
      Print*,"**********************************************"
      MW=0D0
      call CalcMWTHDM(drSM,drns,1,9)
      Print*, "Result for MW:", MW

      call CalcIHDM2Loop(drhoAllH2L,2)
      Print*,"Non-standard two-loop correction to the rho parameter:"
     &  , drhoAllH2L


      end
