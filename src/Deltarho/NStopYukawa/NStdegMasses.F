c This file is part of THDM_EWPOS.

c THDM_EWPOS is free software: you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.

c THDM_EWPOS is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License for more details.

c   You should have received a copy of the GNU General Public License along with THDM_EWPOS. If not, see <https://www.gnu.org/licenses/>.

c Copyright 2013 - 2018 Stephan Hessenberger for the Max-Planck-Institute for Physics in Munich



* NStdegMasses.F
* generated 14-Dec-2015 15:21

C> @brief This subroutine calculates \f$\delta\rho^{(2)}_\mathrm{t,NS}\f$
C> with degenerate masses of the non-standard scalars.
C>
C> @ingroup group_rho
	subroutine calcNStdegMasses
	implicit none


#include "NStdegMasses.h"


	RealType T134sub

	external T134sub

	ComplexType A0FH, B0FH

	external A0FH, B0FH

#include "paraTHDM.h"
#include "paraSM.h"
	NStdl(1) = A0FH(MHH2)

	NStdl(2) = A0FH(MT2)

	NStdl(3) = B0FH(MT2,0.D0,MHH2)

	NStdl(4) = B0FH(MT2,MHH2,MT2)

	NStdl(5) = T134sub(MT2,MT2,MHH2)

	NStdl(6) = T134sub(MHH2,0.D0,0.D0)

	NStdl(7) = T134sub(MT2,MHH2,0.D0)

	NStdx(9) = 7*MHH2**2 + 8*MT2**2 - 24*MT2*MHH2

        NStdx(10) = 11*MHH2**2 + 24*MT2**2 - 44*MT2*MHH2 +
     &    1/MT2*(NStdl(2)*NStdx(9))

	NStdx(5) = 3*MHH2**2 + 8*MT2**2 - 11*MT2*MHH2

	NStdx(1) = MHH2 - 2*MT2*(1 - 1/MT2*NStdl(2))

	NStdx(12) = 5*MHH2**2 + 6*MT2**2 - 14*MT2*MHH2

	NStdx(18) = Re(NStdl(2)) + (MHH2 - 2*MT2)*Re(NStdl(4))

        NStdx(4) = 23*MHH2**3 + 40*MT2**3 - 102*MT2**2*MHH2 -
     &    33*MT2*MHH2**2

        NStdx(6) = 1/MT2*(MHH2*(7*MHH2 - 16*MT2)*NStdl(2)) -
     &    2/MT2**2*(NStdl(2)**2*NStdx(5))

        NStdx(7) = 3*MHH2**3 - 12*MT2**3 + 26*MT2**2*MHH2 -
     &    17*MT2*MHH2**2

        NStdx(15) = -3*Re(NStdl(1)) +
     &    (MHH2 - MT2)*Re(NStdl(3)) +
     &    2*(MT2*(3 + 1/MT2*Re(NStdl(2))) +
     &       (MHH2 - 2*MT2)*Re(NStdl(4)))

        NStdx(17) = MT2*(9 + 4/MT2*Re(NStdl(2))) -
     &    6*Re(NStdl(1)) + 2*(MHH2 - MT2)*Re(NStdl(3)) +
     &    4*(MHH2 - 2*MT2)*Re(NStdl(4))

        NStdx(19) = MT2 + 4*NStdx(18) - 6*Re(NStdl(1)) +
     &    2*(MHH2 - MT2)*Re(NStdl(3))

        NStdx(2) = -4*(5*MHH2 - 32*MT2)*NStdl(2) -
     &    24*NStdl(2)**2 +
     &    (MHH2 - MT2)*(5*MHH2 + 28*MT2 - 8*NStdl(5)) +
     &    12*NStdl(1)*NStdx(1)

	NStdx(3) = MHH2**2 + 4*MT2**2 - 5*MT2*MHH2

        NStdx(8) = MT2*NStdx(4) + 4*MT2**2*NStdx(6) -
     &    8*NStdl(5)*NStdx(7)

        NStdx(16) = MT2*(3 + 4/MT2*Re(NStdl(2))) -
     &    1/MT2*(NStdl(2)*NStdx(15)) - 6*Re(NStdl(1)) +
     &    2*(MHH2 - MT2)*Re(NStdl(3)) +
     &    4*(MHH2 - 2*MT2)*Re(NStdl(4))

	NStdx(20) = 2/MT2*(NStdl(2)*NStdx(17)) - 3*NStdx(19)

        NStdx(11) = MT2*(NStdl(1)*NStdx(10))/(MHH2*NStdx(3)) +
     &    NStdl(6)

	NStdx(13) = MHH2*NStdx(11) + (NStdl(7)*NStdx(12))/(MHH2 - MT2)

        NStdx(14) = MT2*NStdx(2)/(MHH2 - 4*MT2) +
     &    NStdx(8)/NStdx(3) + 4*NStdx(13)

        NStdegMasses =
     &   Alfa2/(MW2**2*Pi**2*SW2**2)*
     &    (3/512.D0*NStdx(14) -
     &       (3*MT2)/512.D0*(4*NStdx(16) + NStdx(20)))/TB2

	end
