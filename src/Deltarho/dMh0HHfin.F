c This file is part of THDM_EWPOS.

c THDM_EWPOS is free software: you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.

c THDM_EWPOS is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License for more details.

c   You should have received a copy of the GNU General Public License along with THDM_EWPOS. If not, see <https://www.gnu.org/licenses/>.

c Copyright 2013 - 2018 Stephan Hessenberger for the Max-Planck-Institute for Physics in Munich


#include "types.h"

C> @brief This subroutine calculates the additional finite part of the sub-loop
C> renormalization, which is induced by changing the renormalization of \f$\delta m^2_{h^0H^0}\f$
C> from a \f$\overline{MS}\f$-renormalization to an on-shell renormalization.
C>
C> @ingroup group_rho
      subroutine calcdMh0HHfin(dMh0HHfin)
      implicit none

      ComplexType dup1, dup2
C> @param[out] The parameter dmh0hhfin gives the result for the additional finite correction.
      RealType dMh0HHfin

      RealType T134sub

      external T134sub

      ComplexType A0FH, B0FH

      external A0FH, B0FH

#include "paraTHDM.h"
#include "paraSM.h"

      dup1 = Lambda6*S2B**2 - 2*Lambda7*SB2**2

      dup2 = (Mh02 + 2*MHH2)*S2A*S2B - 2*m12sq*(3*S2A + S2B)

        dMh0HHfin = -(Alfa2/(MW2**2*Pi**2*SW2**2)*
     &      (CBA*SBA*(1/256.D0*
     &            (Mh02*(A0FH(Mh02)/Mh02 - B0FH(0.D0,MA02,Mh02)) -
     &              MHH2*(A0FH(MHH2)/MHH2 - B0FH(0.D0,MA02,MHH2)) +
     &              MHp2*B0FH(0.D0,Mh02,MHp2)) -
     &           1/256.D0*(MHp2*B0FH(0.D0,MHH2,MHp2)))*
     &         (CBA*SBA*(3*Mh02*MHH2*Re(B0FH(Mh02,0.D0,0.D0)) -
     &              2*(MA02 - Mh02)*(MA02 - MHH2)*
     &               Re(B0FH(Mh02,0.D0,MA02)) -
     &              4*(Mh02 - MHp2)*(MHH2 - MHp2)*
     &               Re(B0FH(Mh02,0.D0,MHp2))) -
     &           (((CBA*(2*MA02 - MHH2)*S2B**2 -
     &                    (4*m12sq - 2*MHH2*S2B)*SAB)*
     &                  (CAB*(4*m12sq - 2*Mh02*S2B) -
     &                    (2*MA02 - Mh02)*S2B**2*SBA) +
     &                 2*((MW2**2*SW2**2)/(Alfa2*Pi**2)*
     &                     (CBA*
     &                       (2*CB2**2*Lambda6**2 - dup1*Lambda7)*
     &                       SBA) +
     &                    (MW2*SW2)/(Alfa*Pi)*
     &                     ((CB2*Lambda6 - Lambda7*SB2)*
     &                       (4*m12sq*(CBA*SAB - CAB*SBA) -
     &                       S2B*
     &                       (2*CBA*MHH2*SAB +
     &                       S2B*
     &                       (CBA2*(2*MA02 - MHH2) + Mh02*SBA2) -
     &                       2*(CAB*Mh02*SBA + MA02*S2B*SBA2))))))*
     &               Re(B0FH(Mh02,MA02,MA02)) -
     &              3*((CBA*
     &                     (6*m12sq*S2A -
     &                       (2*m12sq + (2*Mh02 + MHH2)*S2A)*S2B)*
     &                     (CAB*(4*CBA2*m12sq - 2*Mh02*S2B) -
     &                       Mh02*S2A*S2B*SBA) +
     &                    (2*MW2*SW2)/(Alfa*Pi)*
     &                     (CBA2*
     &                       ((CB2*Lambda6 - Lambda7*SB2)*
     &                       (CBA2*
     &                       ((2*Mh02 + MHH2)*S2A*S2B -
     &                       2*m12sq*(3*S2A - S2B - 6*CAB*SBA)) -
     &                       3*Mh02*S2B*(2*CAB*SBA + S2A*SBA2)) -
     &                       (3*MW2*SW2)/(Alfa*Pi)*
     &                       (CBA*CBA2*
     &                       (2*CB2**2*Lambda6**2 - dup1*Lambda7)*
     &                       SBA))))*Re(B0FH(Mh02,Mh02,Mh02)) +
     &                 (dup2*SBA*
     &                     (MHH2*S2B*(CBA*S2A - 2*SAB) +
     &                       4*m12sq*SAB*SBA2) -
     &                    (2*MW2*SW2)/(Alfa*Pi)*
     &                     (SBA2*
     &                       ((CB2*Lambda6 - Lambda7*SB2)*
     &                       (MHH2*S2B*(3*CBA2*S2A - 6*CBA*SAB) -
     &                       ((Mh02 + 2*MHH2)*S2A*S2B -
     &                       2*m12sq*(3*S2A + S2B + 6*CBA*SAB))*
     &                       SBA2) +
     &                       (3*MW2*SW2)/(Alfa*Pi)*
     &                       (CBA*
     &                       (2*CB2**2*Lambda6**2 - dup1*Lambda7)*
     &                       SBA*SBA2))))*Re(B0FH(Mh02,MHH2,MHH2)))
     &                - 2*((CBA*dup2*
     &                     (2*m12sq*(3*S2A - S2B) -
     &                       (2*Mh02 + MHH2)*S2A*S2B)*SBA -
     &                    (18*MW2*SW2)/(Alfa*Pi)*
     &                     (CBA2*SBA2*
     &                       (S2A*(4*m12sq - (Mh02 + MHH2)*S2B)*
     &                       (CB2*Lambda6 - Lambda7*SB2) +
     &                       (MW2*SW2)/(Alfa*Pi)*
     &                       (CBA*
     &                       (2*CB2**2*Lambda6**2 - dup1*Lambda7)*
     &                       SBA))))*Re(B0FH(Mh02,Mh02,MHH2)) +
     &                 ((CBA*(MHH2 - 2*MHp2)*S2B**2 +
     &                       (4*m12sq - 2*MHH2*S2B)*SAB)*
     &                     (CAB*(4*m12sq - 2*Mh02*S2B) +
     &                       (Mh02 - 2*MHp2)*S2B**2*SBA) -
     &                    (2*MW2*SW2)/(Alfa*Pi)*
     &                     ((CB2*Lambda6 - Lambda7*SB2)*
     &                       (CBA*(4*m12sq - 2*MHH2*S2B)*SAB -
     &                       2*CAB*(2*m12sq - Mh02*S2B)*SBA +
     &                       S2B**2*
     &                       (CBA2*(MHH2 - 2*MHp2) -
     &                       (Mh02 - 2*MHp2)*SBA2)) +
     &                       (MW2*SW2)/(Alfa*Pi)*
     &                       (CBA*
     &                       (2*CB2**2*Lambda6**2 - dup1*Lambda7)*
     &                       SBA)))*Re(B0FH(Mh02,MHp2,MHp2))))/
     &            S2B**4))/(Mh02 - MHH2))

      end
