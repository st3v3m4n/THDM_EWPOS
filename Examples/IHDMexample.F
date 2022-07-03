
c This file is part of THDM_EWPOS.

c THDM_EWPOS is free software: you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.

c THDM_EWPOS is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License for more details.

c   You should have received a copy of the GNU General Public License along with THDM_EWPOS. If not, see <https://www.gnu.org/licenses/>.

c Copyright 2013 - 2018 Stephan Hessenberger for the Max-Planck-Institute for Physics in Munich


      program IHDMexample
      double precision GZTHDM(9)
      double precision drSM, drns, GZtot, slep, drhoIHDM2L
      Integer J


#include "paraTHDM.h"
#include "paraSM.h"

      Print*,"-------------------------------------------------"
      Print*,"Results for precision observables in the IHDM"
      Print*,"-------------------------------------------------"

      call SetSMParameter

      Mh0=125D0
      Mh02=Mh0**2
      MHH=500D0
      MHH2=MHH**2
      MA0=550D0
      MA02=MA0**2
      MHp=570D0
      MHp2=MHp**2
      SBA=1D0
      SBA2=1D0
      CBA=0D0
      CBA2=0D0
      L345=0D0

      call printSMpara
      call printTHDMpara(3)
      call ltini
      Print*,"mudim:",GetMudim()

      Print*,"**********************************************"
      call CalcMWTHDM(drSM,drns,7)
      Print*, "Result for MW:", MW

      call CalcIHDM2Loop(drhoIHDM2L)
      Print*,"Non-standard two-loop correction to the rho parameter:"
     &  , drhoIHDM2L

      call CalcslepTHDM(slep,6)
      Print*,"Result for effective leptonic mixing angle:",slep


      call CalcZWidthsTHDM(GZTHDM,7)
      Do 100 J=1,9

      Write(*,3000)"Result for  GZTHDM(",J,"):",GZTHDM(J)

 100  CONTINUE
      call CalcGZtot(GZtot,GZTHDM)
      Print*,"Result for total Z Width:", GZtot
      call ltexi

 3000 FORMAT(A,I1,A,D20.14)


      end
