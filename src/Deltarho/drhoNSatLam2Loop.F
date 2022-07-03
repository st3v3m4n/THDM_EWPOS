c This file is part of THDM_EWPOS.

c THDM_EWPOS is free software: you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.

c THDM_EWPOS is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License for more details.

c   You should have received a copy of the GNU General Public License along with THDM_EWPOS. If not, see <https://www.gnu.org/licenses/>.

c Copyright 2013 - 2018 Stephan Hessenberger for the Max-Planck-Institute for Physics in Munich


#include "types.h"
#include "NSatLambda/NSatSindMasses.F"
#include "NSatLambda/NSatSA0eqHH.F"
#include "NSatLambda/NSatSA0eqHp.F"
#include "NSatLambda/NSatSHHeqHp.F"

C> @brief This subroutine calculates the two-loop corrections
C> at \f$\mathcal{O}(\alpha_t \lambda_i)\f$
C> to the \f$\rho\f$ parameter.
C>
C> @ingroup group_rho
C>
C> @anchor Goldself_atlambda
C> @image latex Goldself_atlambda.eps "Generic diagrams for the contribution to the Goldstone self-energies" width=0.5\textwidth
C> @image html Goldself_atlambda.png "Generic diagrams for the contribution to the Goldstone self-energies"
C>
C> This subroutine calculates \f$\delta\rho^{\alpha_t \lambda_i}_\mathrm{NS}\f$,
C> the non-standard two-loop corrections at \f$\mathcal{O}{(\alpha_t \lambda_i)}\f$
C> (where \f$\alpha_t=y_t/4\pi\f$, with the top-Yukawa coupling \f$y_t\f$ and \f$\lambda_i\f$ corresponds to a scalar self-coupling).
C> This contribution has to be calculated
C> from the self-energies of the Goldstone-bosons shown \link Goldself_atlambda above\endlink,
C>  since in the calculation in terms of the gauge-boson self-energies a clear seperation
C> between the contributions of \f$\mathcal{O}(\alpha_t^2)\f$
C> and \f$\mathcal{O}(\alpha_t\lambda_i)\f$ is not possible.
C> The calculation in terms of the Goldstone-boson self-energies is possible
C> due to a Ward identity in the gauge-less limit (for details see @cite Barbieri:1992dq and @cite Fleischer:1993ub).
C> The diagrams of type (c) are shown here just for completeness but do not contribute to \f$\delta\rho^{\alpha_t \lambda_i}_\mathrm{NS}\f$,
C> since their result is independent
C> of the external momentum and only the derivative of the
C> Goldstone-boson self-energies with respect to the external momentum
C> enters in the calculation.
C>
C>
C> Note that the bottom mass has been neglected in the calculation.
C>
C>
      subroutine CalcNSatS2Loop(drhoNSatS2L)
      implicit none
C> @param[out] The parameter drhoNSats2L gives the contribution \f$\delta\rho^{\alpha_t\lambda_i}_\mathrm{NS}\f$.
      RealType drhoNSatS2L
      RealType shift, tshift

#include "drhoNSatLam2Loop.h"

#include "paraTHDM.h"
#include "paraSM.h"

      shift=1.0D0
      tshift=1.0D-5

      call Set2Lregpara()

C> In order to avoid numerical instabilities for \f$m_{S}=2 m_{t}\f$ or \f$m_{S}=m_{t}\f$
C> (where \f$S\f$ is either \f$A^0\f$ or \f$H^0\f$),
C> the mass \f$m_{S}\f$
C>  is shifted by an internally defined
C> variable **tshift**, if the difference \f$m_{S}-2 m_{t}\f$ is smaller than **tshift**.
      IF (DABS(MA0-2D0*MT).LT. tshift .OR. DABS(MA0-MT).LT. tshift) THEN
      MA0=MA0+tshift
      MA02=MA0**2
      ENDIF

      IF (DABS(MHH-2D0*MT).LT. tshift .OR. DABS(MHH-MT).LT. tshift ) THEN
      MHH=MHH+tshift
      MHH2=MHH**2
      ENDIF

C> Numerical instabilities for \f$m_{H^\pm}=2 m_t\f$ are avoided
C> by shifting \f$m_{H^\pm}\f$ by **tshift**, if the difference \f$m_{H^\pm}-2 m_t\f$
C> is smaller than **tshift**.
      IF (DABS(MHp-2D0*MT).LT. tshift ) THEN
      MHp=MHp+tshift
      MHp2=MHp**2
      ENDIF

C> The implementation of the general result of \f$\delta\rho^{\alpha_t \lambda_i}_\mathrm{NS}\f$ with
C> independent masses of the non-standard scalars leads also to numerical instabilities,
C> if two masses of the non-standard scalars are set equal.
C> In order to avoid these instabilities the subroutine \ref calcnsats2loop
C>  checks first if the mass difference between the non-standard scalars is smaller
C> than the internal variable **shift** and uses then different implementations for \f$\delta\rho^{\alpha_t \lambda_i}_\mathrm{NS}\f$,
C> depending on the specific mass settings:
C>
C> - If both \f$m_{A^0}-m_{H^0}\f$ and \f$m_{A^0}-m_{H^\pm}\f$ are smaller than **shift**,
C>   the result is set to zero, since for degenerate masses of the non-standard scalars
C>   all the couplings between one Goldstone-boson and two non-standard scalars vanish.
C>
C> - If only the difference between \f$m_{H^0}\f$ and \f$m_{H^\pm}\f$ is smaller than **shift**,
C>  the subroutine uses the explicit result with \f$m_{H^0}=m_{H^\pm}\f$,
C>  which is implemented in the subroutine \ref calcnsatshheqhp.
C>
C> - If the difference between \f$m_{A^0}\f$ and \f$m_{H^0}\f$ is smaller than **shift**,
C>   the subroutine uses the explicit result with \f$m_{A^0}=m_{H^0}\f$,
C>   which is implemented in the subroutine \ref calcnsatsa0eqhh.
C>
C> - If the difference between \f$m_{A^0}\f$ and \f$m_{H^\pm}\f$ is smaller than **shift**,
C>   the subroutine uses the explicit result with \f$m_{A^0}=m_{H^\pm}\f$,
C>   which is implemented in the subroutine \ref calcnsatsa0eqhp.
C>
C> - If the difference between \f$m_{H^0}\f$ and \f$m_{H^\pm}\f$ is smaller than **shift**,
C>   the subroutine uses the explicit result with \f$m_{H^0}=m_{H^\pm}\f$,
C>   which is implemented in the subroutine \ref calcnsatshheqhp.
C>
C> - The general result for \f$\delta\rho^{\alpha_t \lambda_i}_\mathrm{NS}\f$,
C>   which is implemented in \ref calcnsatsindmasses,
C>   is used only if the mass differences between all of the non-standard scalars
C>   is larger than **shift**.
C>
      IF (DABS(MA0-MHH).LT. shift .AND. DABS(MA0-MHp).LT. shift) THEN

      drhoNSatS2L=0D0

      ELSE IF (DABS(MA0-MHH).LT. shift ) THEN

      call calcNSatSA0eqHH
      drhoNSatS2L=NSatSA0eqHH

      ELSE IF (DABS(MA0-MHp).LT. shift ) THEN

      call calcNSatSA0eqHp
      drhoNSatS2L=NSatSA0eqHp

      ELSE IF (DABS(MHH-MHp).LT. shift ) THEN

      call calcNSatSHHeqHp
      drhoNSatS2L=NSatSHHeqHp
      ELSE

      call calcNSatSindMasses

      drhoNSatS2L=NSatSindMasses

      ENDIF

      end
