c This file is part of THDM_EWPOS.

c THDM_EWPOS is free software: you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.

c THDM_EWPOS is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License for more details.

c   You should have received a copy of the GNU General Public License along with THDM_EWPOS. If not, see <https://www.gnu.org/licenses/>.

c Copyright 2013 - 2018 Stephan Hessenberger for the Max-Planck-Institute for Physics in Munich


#include "types.h"
#include "NSHiggsStates/NSHindMasses.F"
#include "NSHiggsStates/NSHA0eqHH.F"
#include "NSHiggsStates/NSHHHeqHp.F"

C> @brief This subroutine calculates the two-loop corrections to the \f$\rho\f$ parameter
C> from the scalar self-interaction between the non-standard scalars.
C>
C> @ingroup group_rho
C>
C> In order to avoid numerical instabilities for \f$m_{H^0}=2 m_{A^0}\f$ or \f$m_{H^0}=2 m_{H^\pm}\f$ the mass \f$m_{H^0}\f$
C>  is shifted by an internally defined
C> variable **tshift**, if one of the differences \f$m_{H^0}-2 m_{A^0}\f$ or \f$m_{H^0}-2 m_{H^\pm}\f$ is smaller than **tshift**.
C>
C> The implementation of the general result of \f$\delta\rho^{(2)}_\mathrm{H,NS}\f$ with
C> independent masses of the non-standard scalars leads also to numerical instabilities,
C> if two masses of the non-standard scalars are set equal.
C> In order to avoid these instabilities the subroutine \ref calcnshiggs2loop
C>  checks first if the mass difference between the non-standard scalars is smaller
C> than the internal variable **shift** and uses then different implementations for \f$\delta\rho^{(2)}_\mathrm{H,NS}\f$,
C> depending on the specific mass settings:
C>
C> - If the difference between
C> \f$m_{A^0}\f$ and \f$m_{H^\pm}\f$
C> is smaller than **shift**, than the result for \f$\delta\rho^{(2)}_\mathrm{H,NS}\f$ is
C> set equal to zero, as it is required due to a restoration of a custodial symmetry
C> for  \f$m_{A^0}=m_{H^\pm}\f$.
C> An explicit calculation with \f$m_{A^0}=m_{H^\pm}\f$
C> showed that \f$\delta\rho^{(2)}_\mathrm{H,NS}\f$ is indeed zero for this mass settings.
C> As an additional test, the limit \f$m_{A^0}\rightarrow m_{H^\pm}\f$
C> was taken in Mathematica for the general analytical result with independent masses, which
C> led also to \f$\delta\rho^{(2)}_\mathrm{H,NS}=0\f$.
C>
C> - If the difference between \f$m_{H^0}\f$ and \f$m_{A^0}\f$ is smaller than **shift**,
C> the subroutine uses the explicit result with \f$m_{A^0}=m_{H^0}\f$,
C> which is implemented in the subroutine \ref calcnsha0eqhh.
C>
C> - If the difference between \f$m_{H^0}\f$ and \f$m_{H^\pm}\f$ is smaller than **shift**,
C> the subroutine uses the explicit result with \f$m_{H^0}=m_{H^\pm}\f$,
C> which is implemented in the subroutine \ref calcnshhheqhp.
C>
C> - The general result for \f$\delta \rho^{(2)}_\mathrm{H,NS}\f$,
C> which is implemented in \ref calcnshindmasses,
C> is used only if the mass differences between all of the non-standard scalars
C> is larger than **shift**.
C>
      subroutine CalcNSHiggs2Loop(drhoNSH2L)
      implicit none
C> @param[out] The parameter drhonsh2l gives the two-loop correction \f$\delta\rho^{(2)}_\mathrm{H,NS}\f$.
      RealType drhoNSH2L
      RealType shift, tshift

#include "drhoNSHiggs2Loop.h"

#include "paraTHDM.h"
#include "paraSM.h"

      shift=1.0D-2
      tshift=1.0D-6


      call Set2Lregpara()

      IF (DABS(MHH-2D0*MA0).LT. tshift ) THEN
      MHH=MHH+tshift
      MHH2=MHH**2
      ENDIF

      IF (DABS(MHH-2D0*MHp).LT. tshift ) THEN
      MHH=MHH+tshift
      MHH2=MHH**2
      ENDIF



      IF (DABS(MA0-MHp).LT. shift ) THEN

      drhoNSH2L=0D0

      ELSE IF (DABS(MA0-MHH).LT. shift ) THEN

      call CalcNSHA0eqHH
      drhoNSH2L=NSHA0eqHH

      ELSE IF (DABS(MHH-MHp).LT. shift ) THEN

      call CalcNSHHHeqHp
      drhoNSH2L=NSHHHeqHp
      ELSE

      call CalcNSHindMasses

      drhoNSH2L=NSHindMasses

      ENDIF

      end
