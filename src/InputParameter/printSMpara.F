c This file is part of THDM_EWPOS.

c THDM_EWPOS is free software: you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.

c THDM_EWPOS is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License for more details.

c   You should have received a copy of the GNU General Public License along with THDM_EWPOS. If not, see <https://www.gnu.org/licenses/>.

c Copyright 2013 - 2018 Stephan Hessenberger for the Max-Planck-Institute for Physics in Munich



C> @brief This subroutine prints the values for the input-parameters of the SM.
C>
C> @ingroup group_Input

      subroutine printSMpara
      implicit none
#include "paraSM.h"



      write(*,*)"SM parameter:"
      Write(*,*)"(Masses in GeV)"
      write(*,*)""
      Write(*,*)"MZ =",MZ
      Write(*,*)"Mh0 =",Mh0
      write(*,*)""
      Write(*,*)"ME =", ME
      Write(*,*)"MM =", MM
      Write(*,*)"ML =", ML
      Write(*,*)"MU =", MU
      Write(*,*)"MC =", MC
      Write(*,*)"MT =", MT
      Write(*,*)"MD =", MD
      Write(*,*)"MS =", MS
      Write(*,*)"MB =", MB
      write(*,*)""
      Write(*,*)"GF =",GF
      Write(*,*)"1/Alfa =",1D0/Alfa
      Write(*,*)"DelAlfaMZ:",DelAlfaMZ
      Write(*,*)"AlfaMZ:",AlfaMZ
      Write(*,*)"AlfasMZ:",AlfasMZ
      write(*,*)""
        Write(*,*)'CKM(1,1) =',CKM(1,1)
        Write(*,*)'CKM(1,2) =',CKM(1,2)
        Write(*,*)'CKM(1,3) =',CKM(1,3)
        Write(*,*)'CKM(2,1) =',CKM(2,1)
        Write(*,*)'CKM(2,2) =',CKM(2,2)
        Write(*,*)'CKM(2,3) =',CKM(2,3)
        Write(*,*)'CKM(3,1) =',CKM(3,1)
        Write(*,*)'CKM(3,2) =',CKM(3,2)
        Write(*,*)'CKM(3,3) =',CKM(3,3)



      end
