c This file is part of THDM_EWPOS.

c THDM_EWPOS is free software: you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.

c THDM_EWPOS is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License for more details.

c   You should have received a copy of the GNU General Public License along with THDM_EWPOS. If not, see <https://www.gnu.org/licenses/>.

c Copyright 2013 - 2018 Stephan Hessenberger for the Max-Planck-Institute for Physics in Munich




C> @brief This subroutine calculates the SM prediction of the W-boson mass using the parametrisation given in
C>  [hep-ph/0311148](https://arxiv.org/abs/hep-ph/0311148).
C>
C>The parametrisation requires the following SM input parameters:
C> - the mass of the Higgs boson in the SM, \f$M_H\f$,
C> - the mass of the top quark, \f$m_t\f$,
C> - the mass of the Z boson, \f$M_Z\f$,
C> - the strong coupling constant, \f$\alpha_s\left(M_Z\right)\f$,
C> - the shift in the electromagnetic fine structure constant, \f$\Delta\alpha\f$.
C>
C> Currently, the lighter of the CP-even Higgs bosons in the THDM is identified with the Higgs boson in the SM.
C> Therefore \f$m_{h^0}=M_H\f$ is used for the SM Higgs boson mass in the parametrisation.
C> The remaining input parameters are defined in the file paraSM.h and set by the subroutine @ref setsmparameter.
C> @ingroup group_MW
      subroutine CalcMWSMho(paraflag)
      double precision MW0, C1, C2, C3, C4, C5, C6, C7, C8, C9,
     & C10, C11, dalp, delH, dh, dT, dZ, dals,
     & DALPHA, ALSTR
C> @param[in] The input-parameter paraflag is used to choose between the two possible parametrisations given in
C>  [hep-ph/0311148](https://arxiv.org/abs/hep-ph/0311148):
C> - paraflag=1: the parametrisation approximates the SM result of \f$M_W\f$ to better than 0.5 MeV
C> over the whole range of \f$10\;\mathrm{ GeV}\leq M_H \leq 1\;\mathrm{ TeV}\f$,
C> - paraflag=2: the parametrisation approximates the SM result of \f$M_W\f$ to better than 0.25 MeV
C> over the whole range of \f$100\;\mathrm{ GeV}\leq M_H \leq 1\;\mathrm{ TeV}\f$.
      Integer paraflag

#include "paraTHDM.h"
#include "paraSM.h"



        dalp=DelAlfaMZ/0.05907D0-1D0
        dals=AlfasMZ/0.119D0-1D0
        delH=LOG(Mh0/100D0)
	dh=(Mh0/100D0)**2
        dT=(MT/174.3D0)**2-1D0
        dZ=(MZ/91.1875D0)-1D0

        IF (paraflag .eq. 1) THEN

        MW0=80.3779D0
        C1  = 0.05427D0
        C2  = 0.008931D0
	C3  = 0.0000882D0
	C4  = 0.000161D0
	C5  = 1.070D0
	C6  = 0.5237D0
	C7  = 0.0679D0
	C8  = 0.00179D0
	C9  = 0.0000664D0
	C10 = 0.0795D0
	C11 = 114.9D0

        ElSE

        MW0=80.3779D0
        C1  = 0.05263D0
        C2  = 0.010239D0
	C3  = 0.000954D0
	C4  = -0.000054D0
	C5  = 1.077D0
	C6  = 0.5252D0
	C7  = 0.0700D0
	C8  = 0.004102D0
	C9  = 0.000111D0
	C10 = 0.0774D0
	C11 = 115D0

        ENDIF

        MWSMho=MW0-C1*delH-C2*delH**2+C3*delH**4+C4*(dh-1D0)
     &       +C6*dT-C7*dT**2-C8*delH*dT+C9*dh*dT
     &             -C5*dalp-C10*dals+C11*dZ

c      Print*, "Input fuer MWSMho:"
c      Print*, "----------------"
c      Write(*,*) "MT=",MT
c      Write(*,*) "Mh0=",Mh0
c      Write(*,*) "MZ=",MZ
c      Write(*,*) "alphastr=", ALSTR
c      Write(*,*) "Delta alpha=", DALPHA



      end
