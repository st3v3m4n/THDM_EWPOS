c This file is part of THDM_EWPOS.

c THDM_EWPOS is free software: you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.

c THDM_EWPOS is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License for more details.

c   You should have received a copy of the GNU General Public License along with THDM_EWPOS. If not, see <https://www.gnu.org/licenses/>.

c Copyright 2013 - 2018 Stephan Hessenberger for the Max-Planck-Institute for Physics in Munich


C> @brief This subroutine calculates the one-loop SM contribution to \f$\Delta r\f$.
C>
C>
C> The one-loop contribution in the SM is given by
C>\f[
C>\Delta r_\mathrm{SM}^\mathrm{(1)}=\Pi^{AA}_\mathrm{SM}\left(0\right)+\frac{c_W^2}{s_W^2}\left(\frac{\delta M_{W,\mathrm{SM}}^2}{M_W^2}-\frac{\delta M_{Z,\mathrm{SM}}^2}{M_W^2}\right)
C>+\frac{\Sigma^W_\mathrm{SM}\left(0\right)-\delta M_{W,\mathrm{SM}}^2}{M_W^2}-2\frac{c_W}{s_W}\frac{\Sigma^{AZ}_\mathrm{SM}\left(0\right)}{M_Z^2}
C>+\frac{\alpha}{4\pi s_W^2}\left(6+\frac{7-4s_W^2}{2s_W^2}\log c_W^2\right),
C>\f]
C> with the following SM contributions to the one-loop counterterms or self-energies:
C> - the photon-vacuum polarization \f$\Pi^{AA}_\mathrm{SM}\left(0\right)\f$ (calculated by the subroutine \ref calcpiaa0sm),
C> - the W boson mass counterterm \f$\delta M_{W,\mathrm{SM}}^2\f$ (calculated by the subroutine \ref calcdmw2sm),
C> - the Z boson mass counterterm \f$\delta M_{Z,\mathrm{SM}}^2\f$ (calculated by the subroutine \ref calcdmz2sm),
C> - the self-energy of the W boson \f$\Sigma^W_\mathrm{SM}\left(0\right)\f$ (calculated by the function \ref sigmawsm),
C> - the mixing self-energy between photon  and Z boson \f$\Sigma^{AZ}_\mathrm{SM}\left(0\right)\f$ (calculated by the function \ref sigmaazsm).
C>
C> @ingroup group_MW
      subroutine CalcdrSM1Loop(drSM1Loop)
C> @param[out] The parameter drSM1Loop gives the one-loop contribution \f$\Delta r^{(1)}_\mathrm{SM}\f$.
      double precision drSM1Loop

      double complex SigmaWSM, SigmaAZSM
      external SigmaWSM, SigmaAZSM


#include "CTdeclarationSM.h"
#include "paraSM.h"

      call CalcPiAA0SM
      call CalcdMZ2SM
      call CalcdMW2SM


      drSM1Loop= PiAA0SM-CW2/SW2*(dMZ2SM/MZ2-dMW2SM/MW2)
     & +(SigmaWSM(0D0)-dMW2SM)/MW2-2D0*CW/SW*SigmaAZSM(0D0)/MZ2
     & +Alfa/(4D0*Pi*SW2)
     & *(6.D0+(7.D0-4.D0*SW2)/(2*SW2)*DLOG(CW2))

      end
