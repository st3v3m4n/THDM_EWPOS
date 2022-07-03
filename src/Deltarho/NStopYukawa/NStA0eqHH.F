c This file is part of THDM_EWPOS.

c THDM_EWPOS is free software: you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.

c THDM_EWPOS is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License for more details.

c   You should have received a copy of the GNU General Public License along with THDM_EWPOS. If not, see <https://www.gnu.org/licenses/>.

c Copyright 2013 - 2018 Stephan Hessenberger for the Max-Planck-Institute for Physics in Munich



* NStA0eqHH.F
* generated 14-Dec-2015 14:39

C> @brief This subroutine calculates \f$\delta\rho^{(2)}_\mathrm{t,NS}\f$ with \f$m_{A^0}=m_{H^0}\f$.
C>
C> @ingroup group_rho
	subroutine calcNStA0eqHH
	implicit none


#include "NStA0eqHH.h"


	RealType T134sub

	external T134sub

	ComplexType A0FH, B0FH

	external A0FH, B0FH

#include "paraTHDM.h"
#include "paraSM.h"
	NStahl(1) = A0FH(MT2)

	NStahl(10) = T134sub(MT2,MHp2,0.D0)

	NStahl(11) = T134sub(MT2,MT2,MHp2)

	NStahl(12) = T134sub(MHp2,0.D0,0.D0)

	NStahl(2) = B0FH(MHH2,MT2,MT2)

	NStahl(3) = A0FH(MHH2)

	NStahl(4) = B0FH(MHp2,0.D0,MT2)

	NStahl(5) = A0FH(MHp2)

	NStahl(6) = B0FH(MT2,0.D0,MHp2)

	NStahl(7) = B0FH(MT2,MHH2,MT2)

	NStahl(8) = T134sub(MT2,MHH2,0.D0)

	NStahl(9) = T134sub(MT2,MT2,MHH2)

	NStahx(37) = 2*MHH2**2 - 3*MHH2*(MHp2 + 2*MT2) + 10*MT2*MHp2

	NStahx(13) = MHH2**2 - MHp2*(MHH2 - MT2)

	NStahx(15) = MHp2*(MHp2 + MT2) - MHH2*(MHp2 + 2*MT2)

	NStahx(17) = MHp2**2 + MT2**2 - 4*MT2*MHp2

	NStahx(18) = MHp2**2 + 2*MT2**2 - 5*MT2*MHp2

	NStahx(19) = MHH2**2 - MHH2*(MHp2 + 2*MT2) + MT2*MHp2

	NStahx(29) = 3*MHH2**2 + 52*MT2**2 - 26*MT2*MHH2

	NStahx(34) = 3*MHp2**2 - 16*MT2**2 - 4*MT2*MHp2

	NStahx(35) = 3*MHp2**2 + 16*MT2**2 - 16*MT2*MHp2

        NStahx(38) = MHH2*(MHp2 + 6*MT2) +
     &    1/MT2*(NStahl(1)*NStahx(37))

	NStahx(4) = 6*MHp2 - 1/MT2*((3*MHp2 - 4*MT2)*NStahl(1))

        NStahx(14) = MHH2*(MHH2 - 2*MHp2 + 2*MT2) +
     &    2/MT2*(NStahl(1)*NStahx(13))

        NStahx(16) = MHp2*(3*MHH2 - 4*MHp2 + 2*MT2) -
     &    2/MT2*(NStahl(1)*NStahx(15))

        NStahx(20) = NStahl(10)*
     &     (MHp2*NStahx(17) - MHH2*NStahx(18)) +
     &    (MHH2 - MT2)*NStahl(8)*NStahx(19)

        NStahx(26) = 5*MHH2**3 + 120*MT2**2*MHH2 -
     &    48*(MT2**3 + MT2*MHH2**2)

	NStahx(30) = 21*MHH2**2 - 1/MT2*(NStahl(1)*NStahx(29))

        NStahx(36) = 6*MHH2**3 + MHH2**2*(3*MHp2 - 20*MT2) +
     &    64*MT2**2*NStahl(1) - 4*MT2*NStahx(34) -
     &    16*(MHH2 - 2*MT2)*NStahl(1)**2 + MHH2*NStahx(35)

        NStahx(39) = 2*MHH2**3 - MHH2**2*(MHp2 + 12*MT2) -
     &    8*MT2**2*MHp2 + 4*MT2*NStahx(38)

        NStahx(41) = MHH2**2 +
     &    ((MHH2 - MHp2)*(MHH2 - 2*MT2)**2)/(MHH2 - 4*MT2) -
     &    2*MHp2*(MHH2 - MT2)

	NStahx(5) = MHp2**2 + 4*MT2**2 - MT2*NStahx(4)

	NStahx(2) = 19*MHp2 + 4/MT2*((MHp2 + MT2)*NStahl(1))

        NStahx(21) = MT2*
     &     (NStahl(3)*NStahx(14) - NStahl(5)*NStahx(16)) +
     &    2*NStahx(20)

        NStahx(23) = MHH2**2 + 3*MHH2*MHp2 + 8*MHp2**2 -
     &    13*MT2*(MHH2 - MHp2)

	NStahx(27) = 63*MHH2**3 - 4/MT2*(NStahl(1)*NStahx(26))

	NStahx(28) = 9*MHH2 + 2/MT2**2*((MHH2 - 3*MT2)*NStahl(1)**2)

	NStahx(3) = 2*MHp2**2 + 5*MT2**2 - 6*MT2*MHp2

        NStahx(31) = 2*MHH2**3 - 84*MT2**3 + 72*MT2**2*MHH2 -
     &    MT2*NStahx(30)

        NStahx(32) = 2*MHH2**3 - 12*MT2**3 + 38*MT2**2*MHH2 -
     &    17*MT2*MHH2**2

	NStahx(40) = (MHH2 - MHp2)*NStahx(36) + 4*NStahl(3)*NStahx(39)

        NStahx(42) = MHp2*(MHp2 - 2*MT2)*NStahl(11) +
     &    NStahl(9)*NStahx(41)

	NStahx(50) = MHp2 - 1/MT2*((2*MHH2 - 3*MHp2)*Re(NStahl(1)))

	NStahx(51) = MHp2**2 + MT2*(2*MHH2 - MHp2)

        NStahx(52) = MHH2 + 2*(MHH2 - 2*MT2)*Re(NStahl(2)) -
     &    4*MHH2*Re(NStahl(4))

        NStahx(54) = MHH2 - MHp2 -
     &    2*(MT2 + (MHp2 - MT2)*Re(NStahl(4)))

	NStahx(6) = 3*MHp2**2 + (4*NStahl(5)*NStahx(5))/MHp2

	NStahx(60) = Re(NStahl(1)) + (MHH2 - 2*MT2)*Re(NStahl(7))

        NStahx(1) = NStahl(1)**2 + (MHp2 - 2*MT2)*NStahl(11) +
     &    MHp2*NStahl(12)

	NStahx(10) = -16*MT2 + MHH2*(5 + 1/MT2*NStahl(1))

	NStahx(11) = MHH2**2 - 4*MT2**2 - 2*MT2*MHH2

        NStahx(22) = MT2*((1 - 2/MT2*NStahl(1))*NStahl(1)) +
     &    NStahx(21)/(MHH2 - MHp2)**2

        NStahx(24) = MT2*NStahx(23) +
     &    8*MHH2*(MHH2 - 3*MT2)*NStahl(9) -
     &    8*MHp2*(MHp2 - 3*MT2)*NStahl(11)

        NStahx(33) = -192*MT2**4 + 88*MT2**2*MHH2**2 -
     &    MT2*NStahx(27) + 16*MT2**3*NStahx(28) +
     &    8*(MHH2**4 + NStahl(3)*NStahx(31)) -
     &    8*NStahl(9)*NStahx(32)

        NStahx(43) = 4*MHp2*(MHp2 - 4*(MT2 + NStahl(1)))*
     &     NStahl(5) - NStahx(40)/(MHH2 - 4*MT2) + 8*NStahx(42)

        NStahx(46) = MHH2 - 2*MT2*(2 - 1/MT2*Re(NStahl(1))) -
     &    (MHH2 - 2*MT2)*Re(NStahl(2))

        NStahx(47) = MHp2 - MT2*(2 - 1/MT2*Re(NStahl(1))) -
     &    (MHp2 - MT2)*Re(NStahl(4))

        NStahx(49) = MHH2 - MHp2 +
     &    2*(MHH2 - 2*MT2)*Re(NStahl(2)) -
     &    2*(MT2 + Re(NStahl(1)) + (MHp2 - MT2)*Re(NStahl(4)))

        NStahx(53) = MHp2**2 + 2*MT2*NStahx(50) -
     &    MHp2*NStahx(52) - 2*NStahx(51)*Re(NStahl(4))

        NStahx(55) = MHH2*NStahx(54) +
     &    2*(3*MHH2 - 4*MHp2)*Re(NStahl(1)) -
     &    2*(MHH2 - 2*MHp2)*(MHH2 - 2*MT2)*Re(NStahl(2))

        NStahx(57) = -2*Re(NStahl(3)) - Re(NStahl(5)) +
     &    (MHp2 - MT2)*Re(NStahl(6)) +
     &    2*(MT2*(3 + 1/MT2*Re(NStahl(1))) +
     &       (MHH2 - 2*MT2)*Re(NStahl(7)))

        NStahx(59) = MT2*(9 + 4/MT2*Re(NStahl(1))) -
     &    4*Re(NStahl(3)) - 2*Re(NStahl(5)) +
     &    2*(MHp2 - MT2)*Re(NStahl(6)) +
     &    4*(MHH2 - 2*MT2)*Re(NStahl(7))

        NStahx(61) = MT2 + 4*NStahx(60) - 4*Re(NStahl(3)) -
     &    2*Re(NStahl(5)) + 2*(MHp2 - MT2)*Re(NStahl(6))

        NStahx(7) = 3*MT2**3 + MT2**2*NStahx(2) -
     &    2*MT2*NStahx(6) - 8*NStahl(10)*NStahx(3)

        NStahx(9) = 4/MT2*((11*MHH2 - 28*MT2)*NStahl(1)) -
     &    MHH2*(3 + 8/MT2**2*NStahl(1)**2)

        NStahx(12) = -84*MT2**3 + 18*MT2*MHH2**2 +
     &    MT2**2*NStahx(9) +
     &    8*(MT2*(NStahl(3)*NStahx(10)) + MHH2**2*NStahl(8)) -
     &    8*((-4*MT2**2 + 5*MT2*MHH2)*NStahl(8) +
     &       NStahl(9)*NStahx(11))

	NStahx(25) = 4*NStahx(22) - NStahx(24)/(MHH2 - MHp2)

        NStahx(44) = NStahx(33)/(MHH2*(MHH2 - 4*MT2)**2) +
     &    NStahx(43)/(MHH2 - MHp2)**2

        NStahx(48) = (NStahl(3)*NStahx(46))/MHH2 +
     &    (NStahl(5)*NStahx(47))/MHp2 - 3*Re(NStahl(1)) +
     &    (MHH2 - 2*MT2)*Re(NStahl(2)) + (MHp2 - MT2)*Re(NStahl(4))

        NStahx(56) = (MHH2 - MHp2)*(MHH2 + MHp2)*NStahx(49) +
     &    2*(NStahl(5)*NStahx(53) + NStahl(3)*NStahx(55))

        NStahx(58) = MT2*(3 + 4/MT2*Re(NStahl(1))) -
     &    1/MT2*(NStahl(1)*NStahx(57)) - 4*Re(NStahl(3)) -
     &    2*Re(NStahl(5)) + 2*(MHp2 - MT2)*Re(NStahl(6)) +
     &    4*(MHH2 - 2*MT2)*Re(NStahl(7))

	NStahx(62) = 2/MT2*(NStahl(1)*NStahx(59)) - 3*NStahx(61)

	NStahx(8) = 8*NStahx(1) + NStahx(7)/(MHp2 - MT2)

        NStahx(45) = 2*MT2*NStahx(44) - NStahx(8) -
     &    NStahx(12)/(MHH2 - 4*MT2) - 2*NStahx(25)

        NStahx(63) = 8*NStahx(48) -
     &    (2*NStahx(56))/(MHH2 - MHp2)**2 - 4*NStahx(58) -
     &    NStahx(62)

        NStA0eqHH = -(Alfa2/(MW2**2*Pi**2*SW2**2)*
     &      (3/1024.D0*NStahx(45) - (3*MT2)/512.D0*NStahx(63))/TB2)

	end
