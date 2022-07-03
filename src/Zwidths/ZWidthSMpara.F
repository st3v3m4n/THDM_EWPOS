c This file is part of THDM_EWPOS.

c THDM_EWPOS is free software: you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.

c THDM_EWPOS is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License for more details.

c   You should have received a copy of the GNU General Public License along with THDM_EWPOS. If not, see <https://www.gnu.org/licenses/>.

c Copyright 2013 - 2018 Stephan Hessenberger for the Max-Planck-Institute for Physics in Munich


#include "types.h"


C> @brief Parameterizations for the partial and total width of the Z boson in the SM
C>
C> The parameterizations are taken from [arXiv:1401.2447](https://arxiv.org/abs/1401.2447) \cite Freitas:2014hra,
C> where also information about the included higher-order corrections can be found.
C>
C> @ingroup group_Zwidths
      RealType Function ZWidthSM(WidthIndex)
      implicit none
C>  @param[in] The index WidthIndex specifies which width is obtained:
C>	- WidthIndex=1:	partial width of the neutrino
C>	- WidthIndex=2:	partial width of the electron
C>	- WidthIndex=3:	partial width of the muon
C>	- WidthIndex=4:	partial width of the tau lepton
C>	- WidthIndex=5:	partial width of the up quark
C>	- WidthIndex=6:	partial width of the down quark
C>	- WidthIndex=7:	partial width of the charm quark
C>	- WidthIndex=8:	partial width of the strange quark
C>	- WidthIndex=9:	partial width of the bottom quark
C>	- WidthIndex=10:	total Z width
      Integer WidthIndex
      RealType X0(1:10), c1(1:10), c2(1:10), c3(1:10),
     & c4(1:10), c5(1:10), c6(1:10), c7(1:10)
      RealType Lhiggs, dalpha, dtop, dalphas, dz
#include "paraSM.h"


      X0(1)=167.157D0
      X0(2)=83.966D0
      X0(3)=83.966D0
      X0(4)=83.776D0
      X0(5)=299.936D0
      X0(6)=382.770D0
      X0(7)=299.860D0
      X0(8)=382.770D0
      X0(9)=375.724D0
      X0(10)=2494.254D0

      c1(1)=-0.055D0
      c1(2)=-0.047D0
      c1(3)=-0.047D0
      c1(4)=-0.047D0
      c1(5)=-0.34D0
      c1(6)=-0.34D0
      c1(7)=-0.34D0
      c1(8)=-0.34D0
      c1(9)=-0.30D0
      c1(10)=-2.0D0

      c2(1)=1.26D0
      c2(2)=0.807D0
      c2(3)=0.807D0
      c2(4)=0.806D0
      c2(5)=4.07D0
      c2(6)=3.83D0
      c2(7)=4.07D0
      c2(8)=3.83D0
      c2(9)=-2.28D0
      c2(10)=19.7D0

      c3(1)=-0.19D0
      c3(2)=-0.095D0
      c3(3)=-0.095D0
      c3(4)=-0.095D0
      c3(5)=14.27D0
      c3(6)=10.20D0
      c3(7)=14.27D0
      c3(8)=10.20D0
      c3(9)=10.53D0
      c3(10)=58.60D0

      c4(1)=-0.02D0
      c4(2)=-0.01D0
      c4(3)=-0.01D0
      c4(4)=-0.01D0
      c4(5)=1.8D0
      C4(6)=-2.4D0
      c4(7)=1.8D0
      C4(8)=-2.4D0
      C4(9)=-2.4D0
      C4(10)=-4.0D0

      C5(1)=0.36D0
      C5(2)=0.25D0
      C5(3)=0.25D0
      C5(4)=0.25D0
      C5(5)=1.8D0
      C5(6)=0.67D0
      C5(7)=1.8D0
      C5(8)=0.67D0
      C5(9)=1.2D0
      C5(10)=8.0D0

      C6(1)=-0.1D0
      C6(2)=-1.1D0
      C6(3)=-1.1D0
      C6(4)=-1.1D0
      C6(5)=-11.1D0
      C6(6)=-10.1D0
      C6(7)=-11.1D0
      C6(8)=-10.1D0
      C6(9)=-10.0D0
      C6(10)=-55.9D0

      C7(1)=503D0
      C7(2)=285D0
      C7(3)=285D0
      C7(4)=285D0
      C7(5)=1253D0
      C7(6)=1469D0
      C7(7)=1253D0
      C7(8)=1469D0
      C7(9)=1458D0
      C7(10)=9267D0




      Lhiggs=DLOG(Mh0/125.7D0)
      dalpha=DelAlfaMZ/0.059D0-1D0
      dtop=(MT/173.2D0)**2-1D0
      dalphas=AlfasMZ/0.1184D0-1
      dz=MZ/91.1876D0-1



      ZWidthSM= X0(WidthIndex) + c1(WidthIndex)*Lhiggs
     & + c2(WidthIndex)*dtop
     & + c3(WidthIndex)*dalphas + c4(WidthIndex)*dalphas**2
     & + c5(WidthIndex)*dalphas*dtop
     & + c6(WidthIndex)*dalpha + c7(WidthIndex)*dz

c Change units to GeV


      ZWidthSM=ZWidthSM*1D-3



      END
