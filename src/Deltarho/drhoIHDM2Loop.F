c This file is part of THDM_EWPOS.

c THDM_EWPOS is free software: you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.

c THDM_EWPOS is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License for more details.

c   You should have received a copy of the GNU General Public License along with THDM_EWPOS. If not, see <https://www.gnu.org/licenses/>.

c Copyright 2013 - 2018 Stephan Hessenberger for the Max-Planck-Institute for Physics in Munich


#include "types.h"
#include "IHDM/IHDMindMasses.F"
#include "IHDM/IHDMA0eqHH.F"

C> @brief This subroutine calculates the two-loop corrections to the \f$\rho\f$ parameter
C> from the scalar self-interaction in the IHDM.
C>
C> In order to avoid numerical instabilities the mass \f$m_S\f$ of a non-standard scalar \f$S\f$
C> (where \f$S\f$ is either \f$H^0\f$, \f$A^0\f$ or \f$H^\pm\f$) is shifted by an internally defined
C> variable **tshift**, if the difference between \f$m_{h^0}\f$ and \f$2m_{S}\f$ is smaller than **tshift**.
C>
C> The implementation of the general result of \f$\delta\rho^{(2)}_\mathrm{IHDM}\f$ with
C> independent masses of the non-standard scalars leads also to numerical instabilities,
C> if two masses of the non-standard scalars are set equal.
C> In order to avoid these instabilities the subroutine \ref calcihdm2loop
C>  checks first if the mass difference between the non-standard scalars is smaller
C> than the internal variable **shift** and uses then different implementations for \f$\delta\rho^{(2)}_\mathrm{IHDM}\f$,
C> depending on the specific mass settings:
C>
C> - If the difference between
C> \f$m_{S^0}\f$ and \f$m_{H^\pm}\f$ (where \f$S^0\f$ is either \f$H^0\f$ or \f$A^0\f$)
C> is smaller than **shift**, than the result for \f$\delta\rho^{(2)}_\mathrm{IHDM}\f$ is
C> set equal to zero, as it is required due to a restoration of a custodial symmetry
C> for \f$m_{H^0}=m_{H^\pm}\f$ or \f$m_{A^0}=m_{H^\pm}\f$.
C> An explicit calculation with \f$m_{H^0}=m_{H^\pm}\f$ or \f$m_{A^0}=m_{H^\pm}\f$
C> showed that \f$\delta\rho^{(2)}_\mathrm{IHDM}\f$ is indeed zero for these mass settings.
C> As an additional test, the limit \f$m_{S^0}\rightarrow m_{H^\pm}\f$
C> was taken in Mathematica for the general analytical result with independent masses, which
C> led also to \f$\delta\rho^{(2)}_\mathrm{IHDM}=0\f$.
C>
C> - If the difference between \f$m_{H^0}\f$ and \f$m_{A^0}\f$ is smaller than **shift**,
C> the subroutine uses the explicit result with \f$m_{A^0}=m_{H^0}\f$,
C> which is implemented in the subroutine \ref calcihdma0eqhh.
C>
C> - The general result for \f$\delta \rho^{(2)}_\mathrm{IHDM}\f$,
C> which is implemented in \ref calcihdmindmasses,
C> is used only if the mass differences between all of the non-standard scalars
C> is larger than **shift**.
C>
C>
C> @ingroup group_rho

      subroutine CalcIHDM2Loop(drhoIHDM2L)
      implicit none
C> @param[out] The parameter drhoihdm2l gives the two-loop correction \f$\delta\rho^{(2)}_\mathrm{IHDM}\f$.
      RealType drhoIHDM2L
      RealType shift, tshift

#include "drhoIHDM2Loop.h"

#include "paraTHDM.h"
#include "paraSM.h"

      shift=1.0D-2
      tshift=1.0D-6


C> If \f$\Lambda_{345}\f$ should be used as an input-parameter
C> the preprocessor variable L345INPUT needs to be defined in paraTHDM.h.
C> The value of \f$\mu_2^2\f$, which is used internally in the subroutine,
C> is then calculated from the input-parameters \f$m_{H^0}\f$ and \f$\Lambda_{345}\f$.
#ifdef L345INPUT
      mu2sq=MHH2-1D0/2D0*L345*MW2*SW2/(Alfa*pi)
#endif

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

      drhoIHDM2L=0D0


      ELSE IF (DABS(MHH-MHp).LT. shift ) THEN

      drhoIHDM2L= 0D0
      ELSE IF (DABS(MA0-MHH).LT. shift ) THEN

      call CalcIHDMA0eqHH
      drhoIHDM2L=IHDMA0eqHH
      ELSE

      call CalcIHDMindMasses

      drhoIHDM2L=IHDMindMasses

      ENDIF

      end
