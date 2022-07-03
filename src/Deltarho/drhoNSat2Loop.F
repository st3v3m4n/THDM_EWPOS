c This file is part of THDM_EWPOS.

c THDM_EWPOS is free software: you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.

c THDM_EWPOS is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License for more details.

c   You should have received a copy of the GNU General Public License along with THDM_EWPOS. If not, see <https://www.gnu.org/licenses/>.

c Copyright 2013 - 2018 Stephan Hessenberger for the Max-Planck-Institute for Physics in Munich


#include "types.h"
#include "NSatsquared/NSat2indMasses.F"

C> @brief This subroutine calculates the two-loop corrections
C> at \f$\mathcal{O}(\alpha_t^2)\f$
C> to the \f$\rho\f$ parameter.
C>
C> @ingroup group_rho
C>
C> @anchor Goldself_atsq
C> @image latex Goldself_atsq.eps "Generic diagrams for the contribution to the Goldstone self-energies" width=0.5\textwidth
C> @image html Goldself_atsq.png "Generic diagrams for the contribution to the Goldstone self-energies"
C>
C> This subroutine calculates \f$\delta\rho^{\alpha_t^2}_\mathrm{NS}\f$,
C> the non-standard two-loop corrections at \f$\mathcal{O}{(\alpha_t^2)}\f$
C> (where \f$\alpha_t=y_t/4\pi\f$, with the top-Yukawa coupling \f$y_t\f$).
C> This contribution has to be calculated
C> from the self-energies of the Goldstone-bosons shown \link Goldself_atsq above \endlink.
C> This is possible due to a Ward identity in the gauge-less limit (for details see @cite Barbieri:1992dq and @cite Fleischer:1993ub).
C> In the calculation in terms of the gauge-boson self-energies a clear seperation
C> between the contributions of \f$\mathcal{O}(\alpha_t^2)\f$
C> and \f$\mathcal{O}(\alpha_t\lambda_i)\f$ (where \f$\lambda_i\f$ corresponds to a scalar self-coupling) is not possible.
C>
C>
C> Note that the bottom mass has been neglected in the calculation.
C>
C> In order to avoid numerical instabilities for \f$m_{H^0}=2 m_{t}\f$ or \f$m_{A^0}=2 m_{t}\f$ the mass \f$m_{S}\f$
C> (where \f$S\f$ is either \f$A^0\f$ or \f$H^0\f$)
C>  is shifted by an internally defined
C> variable **tshift**, if the difference \f$m_{S}-2 m_{t}\f$ is smaller than **tshift**.
C> Numerical instabilities for \f$m_{H^\pm}=m_t\f$ are avoided
C> by shifting \f$m_{H^\pm}\f$ by **tshift**, if the difference \f$m_{H^\pm}-m_t\f$
C> is smaller than **tshift**.

      subroutine CalcNSatsq2Loop(drhoNSat2L)
      implicit none
C> @param[out] The parameter drhoNSat2L gives the contribution \f$\delta\rho^{\alpha_t^2}_\mathrm{NS}\f$.
      RealType drhoNSat2L
      RealType shift, tshift

#include "NSatsquared/NSat2indMasses.h"

#include "paraTHDM.h"
#include "paraSM.h"

      shift=1.0D-2
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

      IF (DABS(MHp-MT).LT. tshift ) THEN
      MHp=MHp+tshift
      MHp2=MHp**2
      ENDIF

      call calcNSat2indMasses

      drhoNSat2L=NSat2indMasses


      end
