c This file is part of THDM_EWPOS.

c THDM_EWPOS is free software: you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.

c THDM_EWPOS is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License for more details.

c   You should have received a copy of the GNU General Public License along with THDM_EWPOS. If not, see <https://www.gnu.org/licenses/>.

c Copyright 2013 - 2018 Stephan Hessenberger for the Max-Planck-Institute for Physics in Munich


#include "types.h"
#include "MixedHiggsStates/MixHindMasses.F"
#include "MixedHiggsStates/MixHA0eqHH.F"

C> @brief This subroutine calculates the two-loop corrections to the \f$\rho\f$ parameter
C> from the scalar self-interaction between SM-like and non-standard scalars.
C>
C> In order to avoid numerical instabilities the mass \f$m_S\f$ of a non-standard scalar \f$S\f$
C> (where \f$S\f$ is either \f$H^0\f$, \f$A^0\f$ or \f$H^\pm\f$) is shifted by an internally defined
C> variable **tshift**, if the difference between \f$m_{h^0}\f$ and \f$2m_{S}\f$ is smaller than **tshift**.
C>
C> The implementation of the general result for \f$\delta\rho^{(2)}_\mathrm{H,Mix}\f$ with
C> independent masses of the non-standard scalars leads also to numerical instabilities,
C> if two masses of the non-standard scalars are set equal.
C> In order to avoid these instabilities the subroutine \ref calcmixhiggs2loop
C>  checks first if the mass difference between the non-standard scalars is smaller
C> than the internal variable **shift** and uses then different implementations for \f$\delta\rho^{(2)}_\mathrm{H,Mix}\f$,
C> depending on the specific mass settings:
C>
C> - If the difference between
C> \f$m_{S^0}\f$ and \f$m_{H^\pm}\f$ (where \f$S^0\f$ is either \f$H^0\f$ or \f$A^0\f$)
C> is smaller than **shift**, than the result for \f$\delta\rho^{(2)}_\mathrm{H,Mix}\f$ is
C> set equal to zero, as it is required due to a restoration of a custodial symmetry
C> for \f$m_{H^0}=m_{H^\pm}\f$ or \f$m_{A^0}=m_{H^\pm}\f$.
C> An explicit calculation with \f$m_{H^0}=m_{H^\pm}\f$ or \f$m_{A^0}=m_{H^\pm}\f$
C> showed that \f$\delta\rho^{(2)}_\mathrm{H,Mix}\f$ is indeed zero for these mass settings.
C> As an additional test, the limit \f$m_{S^0}\rightarrow m_{H^\pm}\f$
C> was taken in Mathematica for the general analytical result with independent masses, which
C> led also to \f$\delta\rho^{(2)}_\mathrm{H,Mix}=0\f$.
C>
C> - If the difference between \f$m_{H^0}\f$ and \f$m_{A^0}\f$ is smaller than **shift**,
C> the subroutine uses the explicit result with \f$m_{A^0}=m_{H^0}\f$,
C> which is implemented in the subroutine \ref calcmixha0eqhh.
C>
C> - The general result for \f$\delta \rho^{(2)}_\mathrm{H,Mix}\f$,
C> which is implemented in \ref calcmixhindmasses,
C> is used only if the mass differences between all of the non-standard scalars
C> is larger than **shift**.
C>
C>
C> @ingroup group_rho
      subroutine CalcMixHiggs2Loop(drhoMixH2L)
      implicit none
C> @param[out] The parameter drhomixh2l gives the two-loop correction \f$\delta\rho^{(2)}_\mathrm{H,Mix}\f$.
      RealType drhoMixH2L
      RealType shift, tshift

#include "drhoMixHiggs2Loop.h"

#include "paraTHDM.h"
#include "paraSM.h"

      shift=1.0D-2
      tshift=1.0D-6



      call Set2Lregpara()

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


      IF (DABS(MA0-MHp).LT. shift ) THEN

      drhoMixH2L=0D0


      ELSE IF (DABS(MHH-MHp).LT. shift ) THEN

      drhoMixH2L= 0D0
      ELSE IF (DABS(MA0-MHH).LT. shift ) THEN

      call CalcMixHA0eqHH
      drhoMixH2L=MixHA0eqHH
      ELSE

      call CalcMixHindMasses

      drhoMixH2L=MixHindMasses

      ENDIF

      end
