c This file is part of THDM_EWPOS.

c THDM_EWPOS is free software: you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.

c THDM_EWPOS is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License for more details.

c   You should have received a copy of the GNU General Public License along with THDM_EWPOS. If not, see <https://www.gnu.org/licenses/>.

c Copyright 2013 - 2018 Stephan Hessenberger for the Max-Planck-Institute for Physics in Munich


#include "types.h"


      program Deltarhoexample
      implicit none
      RealType drhoNS1L, drhoNSt2L, drhoNStb2L, drhoNSH2L,
     &  drhoMixH2L


#include "paraTHDM.h"
#include "paraSM.h"

       call SetSMParameter

       MW=80.385D0
       MW2=MW**2
       CW2=MW2/MZ2
       SW2=1D0-CW2

      Mh0=125D0
      Mh02=Mh0**2
      MHH=500D0
      MHH2=MHH**2
      MA0=550D0
      MA02=MA0**2
      MHp=570D0
      MHp2=MHp**2
      TB=1.5D0
      TB2=TB**2
      SBA=1D0
      SBA2=1D0
      CBA=0D0
      CBA2=0D0
      Lambda5=0D0

      Print*,"-------------------------------------------------------------"
      Print*,"Corrections to Delta rho in the THDM with the alignment limit"
      Print*,"-------------------------------------------------------------"

      call printSMpara
      Print*,"MW =",MW
      call printTHDMpara(1)

      call ltini


      call CalcdrhoNS1L(drhoNS1L)
      call CalcNStop2Loop(drhoNSt2L)
      call CalcNStb2Loop(drhoNStb2L)
      call CalcNSHiggs2Loop(drhoNSH2L)
      call CalcMixHiggs2Loop(drhoMixH2L)


      Print*,"Mudim:",Mudim
      Print*,"Non-standard one-loop correction:",drhoNS1L
      Print*,"Non-standard two-loop top-Yukawa correction:"
     &  ,drhoNSt2L
      Print*,"Non-standard two-loop correction from top- and bottom-Yukawa coupling:"
     &  ,drhoNStb2L
      Print*,"Non-standard scalar two-loop correction"
     &  ,drhoNSH2L
      Print*,"Mixed scalar two-loop correction:"
     &  ,drhoMixH2L




      end
