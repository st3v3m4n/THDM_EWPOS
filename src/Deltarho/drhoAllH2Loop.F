c This file is part of THDM_EWPOS.

c THDM_EWPOS is free software: you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.

c THDM_EWPOS is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License for more details.

c   You should have received a copy of the GNU General Public License along with THDM_EWPOS. If not, see <https://www.gnu.org/licenses/>.

c Copyright 2013 - 2018 Stephan Hessenberger for the Max-Planck-Institute for Physics in Munich



#include "types.h"
#include "AllHZ2Violation/AllHindMasses.F"
#include "AllHZ2Violation/AllHA0eqHH.F"
#include "AllHZ2Violation/AllHHHeqHp.F"


C> @brief Subroutine for the two-loop corrections to the \f$\rho\f$ parameter
C> from the scalar self-interaction between all scalars without the alignment limit.
C>
C> In order to avoid numerical instabilities the mass \f$m_S\f$ of a non-standard scalar \f$S\f$
C> (where \f$S\f$ is either \f$H^0\f$, \f$A^0\f$ or \f$H^\pm\f$) is shifted by an internally defined
C> variable **tshift**, if the difference between \f$m_{h^0}\f$ and \f$2m_{S}\f$ is smaller than **tshift**.
C>
C> The implementation of the general result for \f$\delta\rho^{(2)}_\mathrm{H,All}\f$ with
C> independent masses of the non-standard scalars leads also to numerical instabilities,
C> if two of the masses \f$m_{H^0}\f$, \f$m_{A^0}\f$ and \f$m_{H^\pm}\f$ are equal.
C> In order to avoid these instabilities the subroutine \ref calcallh2loop
C>  checks first if the mass difference between these scalars is smaller
C> than the internal variable **shift** and uses then different implementations for \f$\delta\rho^{(2)}_\mathrm{H,All}\f$,
C> depending on the specific mass settings:
C>
C> - If the difference between
C> \f$m_{A^0}\f$ and \f$m_{H^\pm}\f$
C> is smaller than **shift**, than the result for \f$\delta\rho^{(2)}_\mathrm{H,All}\f$ is
C> set equal to zero, as it is required due to a restoration of a custodial symmetry
C> for \f$m_{A^0}=m_{H^\pm}\f$.
C> An explicit calculation with \f$m_{A^0}=m_{H^\pm}\f$
C> showed that \f$\delta\rho^{(2)}_\mathrm{H,All}\f$ is indeed zero for this mass settings.
C>
C>
C> - If the difference between \f$m_{H^0}\f$ and \f$m_{H^\pm}\f$ is smaller than **shift**,
C> the subroutine uses the explicit result with \f$m_{H^0}=m_{H^\pm}\f$,
C> which is implemented in the subroutine \ref calcallhhheqhp.
C>
C> - If the difference between \f$m_{H^0}\f$ and \f$m_{A^0}\f$ is smaller than **shift**,
C> the subroutine uses the explicit result with \f$m_{A^0}=m_{H^0}\f$,
C> which is implemented in the subroutine \ref calcallha0eqhh.
C>
C> - The general result for \f$\delta \rho^{(2)}_\mathrm{H,All}\f$,
C> which is implemented in \ref calcallhindmasses,
C> is used only if the mass differences between all of the non-standard scalars
C> is larger than **shift**.
C>
C>
C> @ingroup group_rho
      subroutine CalcAllH2Loop(drhoAllH2L,renflag)
      implicit none
C> @param[out] The parameter drhoallh2l gives the two-loop contribution \f$\delta\rho^{(2)}_{H}\f$.
      RealType drhoAllH2L
      RealType dMh0HHfin
      RealType shift, tshift
C> @param[in] The flag renflag selects which renormalization should be used for the off-diagonal
C> mass counterterm \f$\delta m^2_{h^0H^0}\f$
C> - renflag=1: \f$\overline{\mathrm{MS}}\f$-renormalization;
C> - renflag=2: on-shell renormalization
C> (the additional finite terms which are present in the sub-loop renormalization are calculated by the subroutine
C> @ref calcdmh0hhfin and then added to the result in the \f$\overline{\mathrm{MS}}\f$-renormalization)
      Integer renflag

#include "drhoAllH2Loop.h"

#include "paraTHDM.h"
#include "paraSM.h"

      call Set2Lregpara()

      shift=1.0D-2
      tshift=1.0D-4

      IF (DABS(Mh0-2D0*MA0).LT. tshift ) THEN
      MA0=MA0+tshift
      MA02=MA0**2
      ENDIF

      IF (DABS(Mh0-2D0*MHp).LT. tshift ) THEN
      MHp=MHp+tshift
      MHp2=MHp**2
      ENDIF

      IF (DABS(Mh0-2D0*MHH).LT. tshift ) THEN
      MHH=MHH+tshift
      MHH2=MHH**2
      ENDIF

      IF (DABS(MHH-2D0*MHp).LT. tshift ) THEN
      MHp=MHp+tshift
      MHp2=MHp**2
      ENDIF

      IF (DABS(MA0-MHp).LT. shift ) THEN

      drhoAllH2L=0D0


      ELSE IF (DABS(MHH-MHp).LT. shift ) THEN

      call calcAllHHHeqHp
      drhoAllH2L=AllHHHeqHp
      ELSE IF (DABS(MA0-MHH).LT. shift ) THEN

      call CalcAllHA0eqHH
      drhoAllH2L=AllHA0eqHH
      ELSE

      call CalcAllHindMasses

      drhoAllH2L=AllHindMasses


      ENDIF

      IF (renflag.eq.1) THEN

      RETURN

      ELSE

      call calcdMh0HHfin(dMh0HHfin)

      drhoAllH2L=drhoAllH2L+dMh0HHfin

      ENDIF



      end
