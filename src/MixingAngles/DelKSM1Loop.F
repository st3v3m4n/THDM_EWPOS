c This file is part of THDM_EWPOS.

c THDM_EWPOS is free software: you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.

c THDM_EWPOS is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License for more details.

c   You should have received a copy of the GNU General Public License along with THDM_EWPOS. If not, see <https://www.gnu.org/licenses/>.

c Copyright 2013 - 2018 Stephan Hessenberger for the Max-Planck-Institute for Physics in Munich


#include "types.h"

C> @brief Subroutine for the calculation of \f$\Delta\kappa^{(1)}_\mathrm{SM}\f$,
C> the one-loop-correction to \f$\kappa\f$ in the SM.
C>
C> The one-loop SM correction is given by
C> \f[
C> \Delta\kappa^{(1)}_\mathrm{SM}=-\frac{1}{2}\frac{s_W}{c_W}\delta^{(1)} Z_{AZ}^\mathrm{SM}
C>+\frac{\delta^{(1)} s_{W,\mathrm{SM}}^2}{s_W^2}
C>+\frac{v_l}{v_l-a_l}\left(\frac{F_{V,\mathrm{SM}}^{l}}{v_l}-\frac{F_{A,\mathrm{SM}}^{l}}{a_l}\right),
C> \f]
C> where the index on the counter-terms indicates that only the standard contributions
C> are conisdered in their calculation. For the leptonic form-factors we use \f$l=e\f$.
C>
C> @ingroup group_mixangles
      subroutine CalcDelKSM1Loop(DelKSM1Loop,FFlag)
      implicit none
C> @param[out] The parameter DelKSM1L gives \f$\Delta\kappa^{(1)}_\mathrm{SM}\f$.
      RealType DelKSM1Loop
C> @param[in] The flag FFlag selects which lepton should be used for the calculation
C> of the vertex correction form factors \f$F_{V,\mathrm{SM}}\f$ and \f$F_{A,\mathrm{SM}}^{l}\f$:
C> - FFlag=1: electron form factor
C> - FFlag=2: muon form factor
C> - FFlag=3: tau form factor
      integer FFLag


      double precision FVlep, FAlep

      double precision vlep, alep, Qlep
      double complex FVeSM, FAeSM, FVmuSM, FAmuSM,
     & FVtauSM, FAtauSM

#include "CTdeclarationSM.h"
#include "paraSM.h"

      Qlep=-1D0
      vlep=-1D0/2D0-2D0*SW2*Qlep
      alep=-1D0/2D0

      IF (FFlag .eq. 1) THEN
      call CalcFVeSM(FVeSM)
      call CalcFAeSM(FAeSM)
      FVlep=DBLE(FVeSM)
      FAlep=DBLE(FAeSM)
      ELSE IF (FFLag .eq. 2) THEN
      call CalcFVmuSM(FVmuSM)
      call CalcFAmuSM(FAmuSM)
      FVlep=DBLE(FVmuSM)
      FAlep=DBLE(FAmuSM)
      ELSE
      call CalcFVtauSM(FVtauSM)
      call CalcFAtauSM(FAtauSM)
      FVlep=DBLE(FVtauSM)
      FAlep=DBLE(FAtauSM)
      END IF

      call CalcdSWsqSM
      call CalcdZAZSM


      DelKSM1Loop=-1D0/2D0*CW/SW*dZAZSM + dSWsqSM/SW2 -
     &  vlep/(alep-vlep)*(FVlep/vlep-FAlep/alep)



      end
