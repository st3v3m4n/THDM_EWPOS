c This file is part of THDM_EWPOS.

c THDM_EWPOS is free software: you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.

c THDM_EWPOS is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License for more details.

c   You should have received a copy of the GNU General Public License along with THDM_EWPOS. If not, see <https://www.gnu.org/licenses/>.

c Copyright 2013 - 2018 Stephan Hessenberger for the Max-Planck-Institute for Physics in Munich


* MixHA0eqHH.F
* generated 15-Dec-2015 18:28

C> @brief This subroutine calculates the two-loop correction \f$\delta\rho^{(2)}_\mathrm{H,Mix}\f$
C> for \f$m_{A^0}=m_{H^0}\f$
C>
C> @ingroup group_rho

	subroutine calcMixHA0eqHH
	implicit none


#include "MixHA0eqHH.h"


	RealType T134sub

	external T134sub

	ComplexType A0FH, B0FH

	external A0FH, B0FH

#include "paraTHDM.h"
#include "paraSM.h"
	MixHahl(1) = B0FH(MHH2,Mh02,MHH2)

	MixHahl(10) = T134sub(MHH2,MHH2,0.D0)

	MixHahl(11) = T134sub(MHp2,MHp2,0.D0)

	MixHahl(12) = T134sub(MHp2,MHp2,Mh02)

	MixHahl(13) = T134sub(MHp2,MHH2,Mh02)

	MixHahl(2) = A0FH(MHH2)

	MixHahl(3) = B0FH(MHH2,0.D0,MHp2)

	MixHahl(4) = B0FH(MHp2,0.D0,MHH2)

	MixHahl(5) = B0FH(MHp2,Mh02,MHp2)

	MixHahl(6) = A0FH(MHp2)

	MixHahl(7) = A0FH(Mh02)

	MixHahl(8) = T134sub(MHH2,MHH2,Mh02)

	MixHahl(9) = T134sub(MHp2,MHH2,0.D0)

	MixHahx(44) = -2*MHp2 + (MW2*SW2)/(Alfa*Pi)*Lambda5

	MixHahx(45) = 3*MHp2 + (MW2*SW2)/(Alfa*Pi)*Lambda5

	MixHahx(17) = MHH2 + MHp2 - (MW2*SW2)/(Alfa*Pi)*Lambda5

	MixHahx(26) = -5*MHp2 + (4*MW2*SW2)/(Alfa*Pi)*Lambda5

	MixHahx(27) = -17*MHp2 + (4*MW2*SW2)/(Alfa*Pi)*Lambda5

	MixHahx(28) = 17*MHp2 + (4*MW2*SW2)/(Alfa*Pi)*Lambda5

	MixHahx(36) = -7 - 6*CW2*(1 - 2*SW2) - 18*SW2 + 12*SW2**2

	MixHahx(38) = 7 - 12*SW2**2 + 6*(CW2 + (3 - 2*CW2)*SW2)

	MixHahx(39) = -1 - 4*SW2**2 + 2*(CW2 + (3 - 2*CW2)*SW2)

	MixHahx(43) = -2*(MHH2 + MHp2) + (MW2*SW2)/(Alfa*Pi)*Lambda5

	MixHahx(46) = 4*MHH2**2 + MixHahx(44)**2 - 4*MHH2*MixHahx(45)

	MixHahx(59) = -2*MHp2 + (5*MW2*SW2)/(Alfa*Pi)*Lambda5

        MixHahx(60) = (9*MHH2 - 5*MHp2)*MHp2 +
     &    (MW2*SW2)/(Alfa*Pi)*(Lambda5*(MHH2 - 3*MHp2))

	MixHahx(71) = Mh02 + MHH2 + MHp2 - (MW2*SW2)/(Alfa*Pi)*Lambda5

	MixHahx(10) = -MHH2 + (2*MW2*SW2)/(Alfa*Pi)*Lambda5

	MixHahx(11) = MHH2 - 7*MHp2 + (MW2*SW2)/(Alfa*Pi)*Lambda5

	MixHahx(13) = 2*MHp2 + (MW2*SW2)/(Alfa*Pi)*Lambda5

        MixHahx(18) = -3*(MHH2 - MHp2)**2 +
     &    2*(Mh02**2 + Mh02*MixHahx(17))

	MixHahx(21) = 2*MHH2 + (MW2*SW2)/(Alfa*Pi)*Lambda5

	MixHahx(23) = -4*(MHH2 + MHp2) + (3*MW2*SW2)/(Alfa*Pi)*Lambda5

        MixHahx(24) = -6*MHH2*MHp2 - 5*(MHH2**2 + MHp2**2) +
     &    (4*MW2*SW2)/(Alfa*Pi)*(Lambda5*(MHH2 + MHp2))

        MixHahx(25) = -10*MHH2*MHp2 + 23*(MHH2**2 + MHp2**2) +
     &    (3*MW2**2*SW2**2)/(Alfa2*Pi**2)*Lambda5**2 -
     &    (8*MW2*SW2)/(Alfa*Pi)*(Lambda5*(MHH2 + MHp2))

        MixHahx(29) = 5*MHH2**3 - MHp2**2*MixHahx(26) +
     &    MHH2*MHp2*MixHahx(27) - MHH2**2*MixHahx(28)

	MixHahx(33) = -1 - 6*CW2*(1 - 2*SW2) - 18*SW2 + 12*SW2**2

	MixHahx(34) = -1 - 2*CW2*(1 - 2*SW2) - 6*SW2 + 4*SW2**2

        MixHahx(40) = -4*MHp2**2 +
     &    (4*(MHH2**2 + MHH2*MHp2 + MHp2**2)*MixHahl(6))/MHp2 +
     &    MHH2*MHp2*MixHahx(38) + 3*MHH2**2*MixHahx(39)

        MixHahx(47) = Mh02**3 - 16*MHH2*MHp2*MixHahx(17) -
     &    2*Mh02**2*MixHahx(43) + Mh02*MixHahx(46)

	MixHahx(51) = MHH2**2 - 4*MHH2*MHp2 + MHp2**2

	MixHahx(54) = -2*MHH2 + (5*MW2*SW2)/(Alfa*Pi)*Lambda5

	MixHahx(55) = 6*MHH2 - 4*MHp2 + (MW2*SW2)/(Alfa*Pi)*Lambda5

        MixHahx(56) = MHH2*(5*MHH2 - 9*MHp2) +
     &    (MW2*SW2)/(Alfa*Pi)*(Lambda5*(3*MHH2 - MHp2))

	MixHahx(58) = -4*MHH2 + 6*MHp2 + (MW2*SW2)/(Alfa*Pi)*Lambda5

	MixHahx(6) = -MHp2 + (2*MW2*SW2)/(Alfa*Pi)*Lambda5

        MixHahx(61) = -((MHH2 - MHp2)*MHp2*MixHahx(59)) +
     &    Mh02*MixHahx(60)

        MixHahx(68) = Mh02*MHH2 + MHH2**2 +
     &    (MW2*SW2)/(Alfa*Pi)*(Lambda5*MHp2) +
     &    (MHH2 - MHp2)**2*Re(MixHahl(3))

        MixHahx(69) = Mh02*MHp2 + MHp2**2 +
     &    (MW2*SW2)/(Alfa*Pi)*(Lambda5*MHH2) +
     &    (MHH2 - MHp2)**2*Re(MixHahl(4))

	MixHahx(7) = -7*MHH2 + MHp2 + (MW2*SW2)/(Alfa*Pi)*Lambda5

        MixHahx(72) = (MHH2 - MHp2)*MixHahx(71) +
     &    (MHH2 - MHp2)**2*Re(MixHahl(3))

        MixHahx(12) = 8*MHp2**2 +
     &    (MW2**2*SW2**2)/(Alfa2*Pi**2)*Lambda5**2 -
     &    4*MHp2*MixHahx(10) - Mh02*MixHahx(11)

        MixHahx(14) = Mh02**2 - 4*(3*MHH2 - MHp2)*MHp2 +
     &    (2*MW2*SW2)/(Alfa*Pi)*(Lambda5*(MHH2 + MHp2)) -
     &    Mh02*MixHahx(13)

	MixHahx(16) = MHH2 - MHp2 - 4*MixHahl(2) + 4*MixHahl(6)

        MixHahx(19) = 3*(MHH2 - MHp2)**2*MixHahl(9) +
     &    MixHahl(13)*MixHahx(18)

        MixHahx(22) = Mh02**2 + 4*MHH2*(MHH2 - 3*MHp2) +
     &    (2*MW2*SW2)/(Alfa*Pi)*(Lambda5*(MHH2 + MHp2)) -
     &    Mh02*MixHahx(21)

        MixHahx(30) = 3*Mh02**4 - 2*Mh02**3*MixHahx(23) +
     &    16*MHH2*MHp2*MixHahx(24) + Mh02**2*MixHahx(25) +
     &    4*Mh02*MixHahx(29)

	MixHahx(32) = 1 - 2*CW2*(1 - 2*SW2) - 6*SW2 + 4*SW2**2

        MixHahx(35) = (MHH2**2 + MHp2**2)*MixHahx(33) +
     &    6*MHH2*MHp2*MixHahx(34)

	MixHahx(50) = Mh02**2 - (MHH2 - MHp2)**2 + Mh02*(MHH2 + MHp2)

        MixHahx(52) = Mh02**2 + 2*Mh02*(MHH2 + MHp2) +
     &    (MW2**2*SW2**2)/(Alfa2*Pi**2)*Lambda5**2 - 2*MixHahx(51)

        MixHahx(57) = Mh02**3 -
     &    2*MHH2*(MHH2 - MHp2)*MixHahx(54) - Mh02**2*MixHahx(55) +
     &    2*Mh02*MixHahx(56)

	MixHahx(62) = Mh02**3 - Mh02**2*MixHahx(58) - 2*MixHahx(61)

        MixHahx(8) = 8*MHH2**2 +
     &    (MW2**2*SW2**2)/(Alfa2*Pi**2)*Lambda5**2 -
     &    4*MHH2*MixHahx(6) - Mh02*MixHahx(7)

        MixHahx(1) = MHH2*(1 + (2*MixHahl(2))/MHH2) -
     &    MHp2*(1 + (2*MixHahl(6))/MHp2)

        MixHahx(2) = Mh02 + 2*MHH2 -
     &    ((Mh02 - 6*MHH2)*MixHahl(2) + MixHahl(2)**2)/MHH2 +
     &    (1 + MixHahl(2)/MHH2)*MixHahl(7) - MixHahl(8)

	MixHahx(3) = Mh02 + 2*MHH2 - (MW2*SW2)/(Alfa*Pi)*Lambda5

        MixHahx(31) = (MixHahl(2)**2*MixHahx(3)*MixHahx(22))/
     &     (MHH2 - MHp2)**2 - MixHahx(30)/(Mh02 - 4*MHp2)

        MixHahx(4) = Mh02 + 2*MHp2 -
     &    ((Mh02 - 6*MHp2)*MixHahl(6) + MixHahl(6)**2)/MHp2 +
     &    (1 + MixHahl(6)/MHp2)*MixHahl(7) - MixHahl(12)

	MixHahx(5) = Mh02 + 2*MHp2 - (MW2*SW2)/(Alfa*Pi)*Lambda5

        MixHahx(53) = (2*MW2*SW2)/(Alfa*Pi)*
     &     (Lambda5*MixHahx(50)) - Mh02*MixHahx(52)

        MixHahx(63) = (MixHahl(8)*MixHahx(3)*MixHahx(57))/
     &     (Mh02 - 4*MHH2) +
     &    (MixHahl(12)*MixHahx(5)*MixHahx(62))/(Mh02 - 4*MHp2)

        MixHahx(9) = (MHH2*MixHahl(6)*MixHahx(3)*MixHahx(5))/
     &     (MHH2 - MHp2)**2 +
     &    (2*MHH2**2*MixHahx(8))/((Mh02 - 4*MHH2)*(MHH2 - MHp2))

        MixHahx(37) = 4*MHH2**2 + 3*MHp2**2*MixHahx(32) +
     &    MHH2*MHp2*MixHahx(36)

        MixHahx(73) = 2*(MHH2 - MHp2)**2*Re(MixHahl(4)) +
     &    MixHahx(5)**2*Re(MixHahl(5))

        MixHahx(75) = MixHahx(3)**2*Re(MixHahl(1)) +
     &    2*(MHH2 - MHp2)**2*Re(MixHahl(3))

        MixHahx(76) = MixHahx(3)**2 -
     &    4*(MHH2 - MHp2)**2*Re(MixHahl(4)) -
     &    MixHahx(5)**2*(1 + 2*Re(MixHahl(5)))

        MixHahx(41) = (MixHahl(6)*MixHahx(37))/MHp2 -
     &    (MixHahl(2)*MixHahx(40))/MHH2

        MixHahx(48) = (MixHahl(2)*MixHahx(3)**2)/
     &     (MHH2 - MHp2) + MixHahx(47)/(Mh02 - 4*MHp2)

        MixHahx(65) = MixHahx(3)**2 -
     &    MixHahx(3)**2*Re(MixHahl(1)) +
     &    2*(MHH2 - MHp2)**2*(1 - Re(MixHahl(3)))

        MixHahx(66) = MixHahx(5)**2 +
     &    2*(MHH2 - MHp2)**2*(1 - Re(MixHahl(4))) -
     &    MixHahx(5)**2*Re(MixHahl(5))

        MixHahx(70) = 2*MixHahx(68) - 2*MixHahx(69) +
     &    MixHahx(3)**2*Re(MixHahl(1)) -
     &    MixHahx(5)**2*Re(MixHahl(5))

        MixHahx(74) = -((2*MHH2 - MHp2)*MixHahx(73)) +
     &    MHp2*(2*MixHahx(72) + MixHahx(3)**2*Re(MixHahl(1)))

        MixHahx(77) = -2*(MHH2 - 2*MHp2)*MixHahx(75) +
     &    MHH2*MixHahx(76)

        MixHahx(15) = (4*MixHahl(6)*MixHahx(12))/
     &     ((MHH2 - MHp2)*MHp2) +
     &    (MixHahl(6)**2*MixHahx(5)*MixHahx(14))/
     &     ((MHH2 - MHp2)**2*MHp2**2)

        MixHahx(20) = 11*(MHH2 - MHp2)**2 +
     &    8*(MixHahl(8)*MixHahx(3) + MixHahl(12)*MixHahx(5)) +
     &    (4*(MHH2 - MHp2)*MixHahl(7)*MixHahx(16) - 8*MixHahx(19))/
     &     Mh02

        MixHahx(42) = -16*
     &     (MHH2*MixHahl(10) + MHp2*MixHahl(11)) +
     &    8*(MHH2 + MHp2)*MixHahl(9)*MixHahx(32) - 3*MixHahx(35) -
     &    4*MixHahx(41)

        MixHahx(49) = (MixHahl(6)*MixHahx(5)**2)/
     &     ((Mh02 - 4*MHp2)*(MHH2 - MHp2)) -
     &    MixHahx(48)/(Mh02 - 4*MHH2)

        MixHahx(67) = (MixHahl(2)*MixHahx(65))/MHH2 +
     &    (MixHahl(6)*MixHahx(66))/MHp2 +
     &    MixHahx(3)**2*Re(MixHahl(1)) +
     &    2*(MHH2 - MHp2)**2*(Re(MixHahl(3)) + Re(MixHahl(4))) +
     &    MixHahx(5)**2*Re(MixHahl(5))

        MixHahx(78) = (MHH2 - MHp2)*(MHH2 + MHp2)*
     &     MixHahx(70) - 2*MixHahl(6)*MixHahx(74) +
     &    MixHahl(2)*MixHahx(77)

        MixHahx(64) = -2*(MHH2 - MHp2)*
     &     (1 + (2*MixHahl(7))/Mh02)*MixHahx(1) +
     &    (8*MixHahl(2)*MixHahx(9))/MHH2 + MixHahx(20) -
     &    MixHahx(42) - 8*((MixHahx(2)*MixHahx(3)**2)/
     &        (Mh02 - 4*MHH2) +
     &       (MixHahx(4)*MixHahx(5)**2)/(Mh02 - 4*MHp2) +
     &       MixHahl(7)*MixHahx(49)) -
     &    4*((MHp2**2*MixHahx(15))/(Mh02 - 4*MHp2) +
     &       MixHahx(31)/(Mh02 - 4*MHH2) +
     &       (2*MixHahl(13)*MixHahx(53) + MixHahx(63))/
     &        (MHH2 - MHp2)**2)

	MixHahx(79) = 2*MixHahx(67) - MixHahx(78)/(MHH2 - MHp2)**2

        MixHA0eqHH = -(Alfa2/(MW2**2*Pi**2*SW2**2)*
     &      (1/1024.D0*MixHahx(64) - 1/256.D0*MixHahx(79)))

	end
