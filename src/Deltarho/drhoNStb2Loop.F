c This file is part of THDM_EWPOS.

c THDM_EWPOS is free software: you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.

c THDM_EWPOS is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License for more details.

c   You should have received a copy of the GNU General Public License along with THDM_EWPOS. If not, see <https://www.gnu.org/licenses/>.

c Copyright 2013 - 2018 Stephan Hessenberger for the Max-Planck-Institute for Physics in Munich


#include "types.h"
#include "NStopbottom/NStbindMasses.F"
#include "NStopbottom/NStbA0eqHH.F"
#include "NStopbottom/NStbA0eqHp.F"
#include "NStopbottom/NStbHHeqHp.F"
#include "NStopbottom/NStbdegMasses.F"

C> @brief This subroutine calculates the two-loop corrections to the \f$\rho\f$ parameter
C> from the top- and bottom-Yukawa interaction in the alignment limit.
C>
C> @ingroup group_rho
C>
C>
C> In order to avoid numerical instabilities for \f$m_{S}=2 m_{t}\f$ or \f$m_{S}=2 m_{b}\f$
C> (where \f$S\f$ is either \f$A^0\f$ or \f$H^0\f$) the mass \f$m_{S}\f$
C>  is shifted by an internally defined
C> variable **tshift**, if one of the differences \f$m_{S}-2 m_{t}\f$ or\f$m_{S}-2 m_{b}\f$ is smaller than **tshift**.
C> Numerical instabilities for \f$m_{H^\pm}=m_t+m_b\f$ are avoided
C> by shifting \f$m_{H^\pm}\f$ by **tshift**, if the difference \f$m_{H^\pm}-(m_t+m_b)\f$
C> is smaller than **tshift**.
C>
C> The implementation of the general result of \f$\delta\rho^{(2)}_\mathrm{tb,NS}\f$ with
C> independent masses of the non-standard scalars leads also to numerical instabilities,
C> if two masses of the non-standard scalars are set equal.
C> In order to avoid these instabilities the subroutine \ref calcnstb2loop
C>  checks first if the mass difference between the non-standard scalars is smaller
C> than the internal variable **shift** and uses then different implementations for \f$\delta\rho^{(2)}_\mathrm{tb,NS}\f$,
C> depending on the specific mass settings:
C>
C> - If both \f$m_{A^0}-m_{H^0}\f$ and \f$m_{A^0}-m_{H^\pm}\f$ are smaller than **shift**,
C> the subroutine uses the explicit result with degenerate masses of the non-standard scalars,
C> which is implemented in the subroutine \ref calcnstbdegmasses.
C>
C> - If only the difference between \f$m_{H^0}\f$ and \f$m_{H^\pm}\f$ is smaller than **shift**,
C> the subroutine uses the explicit result with \f$m_{H^0}=m_{H^\pm}\f$,
C> which is implemented in the subroutine \ref calcnstbhheqhp.
C>
C> - If the difference between \f$m_{A^0}\f$ and \f$m_{H^0}\f$ is smaller than **shift**,
C> the subroutine uses the explicit result with \f$m_{A^0}=m_{H^0}\f$,
C> which is implemented in the subroutine \ref calcnstba0eqhh.
C>
C> - If the difference between \f$m_{A^0}\f$ and \f$m_{H^\pm}\f$ is smaller than **shift**,
C> the subroutine uses the explicit result with \f$m_{A^0}=m_{H^\pm}\f$,
C> which is implemented in the subroutine \ref calcnstba0eqhp.
C>
C> - If the difference between \f$m_{H^0}\f$ and \f$m_{H^\pm}\f$ is smaller than **shift**,
C> the subroutine uses the explicit result with \f$m_{H^0}=m_{H^\pm}\f$,
C> which is implemented in the subroutine \ref calcnstbhheqhp.
C>
C> - The general result for \f$\delta \rho^{(2)}_\mathrm{tb,NS}\f$,
C> which is implemented in \ref calcnstbindmasses,
C> is used only if the mass differences between all of the non-standard scalars
C> is larger than **shift**.
C>
      subroutine CalcNStb2Loop(drhoNStb2L)
      implicit none
C> @param[out] The parameter drhonstb2l gives the two-loop correction \f$\delta\rho^{(2)}_\mathrm{tb,NS}\f$.
      RealType drhoNStb2L
      RealType shift, tshift

#include "drhoNStb2Loop.h"

#include "paraTHDM.h"
#include "paraSM.h"

      shift=1.0D0
      tshift=1.0D-5

      call Set2Lregpara()

      IF (DABS(MA0-2D0*MT).LT. tshift) THEN
      MA0=MA0+tshift
      MA02=MA0**2
      ENDIF

      IF (DABS(MHH-2D0*MT).LT. tshift ) THEN
      MHH=MHH+tshift
      MHH2=MHH**2
      ENDIF

      IF (DABS(MA0-2D0*MB).LT. tshift) THEN
      MA0=MA0+tshift
      MA02=MA0**2
      ENDIF

      IF (DABS(MHH-2D0*MB).LT. tshift ) THEN
      MHH=MHH+tshift
      MHH2=MHH**2
      ENDIF


      IF (DABS(MHp-(MT+MB)).LT. tshift ) THEN
      MHp=MHp+tshift
      MHp2=MHp**2
      ENDIF

      IF (DABS(MA0-MHH).LT. shift .AND. DABS(MA0-MHp).LT. shift) THEN

      call CalcNStbdegMasses
      drhoNStb2L=NStbdegMasses

      ELSE IF (DABS(MA0-MHH).LT. shift ) THEN

      call CalcNStbA0eqHH
      drhoNStb2L=NStbA0eqHH

      ELSE IF (DABS(MA0-MHp).LT. shift ) THEN

      call CalcNStbA0eqHp
      drhoNStb2L=NStbA0eqHp

      ELSE IF (DABS(MHH-MHp).LT. shift ) THEN

      call CalcNStbHHeqHp
      drhoNStb2L=NStbHHeqHp
      ELSE

      call CalcNStbindMasses

      drhoNStb2L=NStbindMasses

      ENDIF

      end
