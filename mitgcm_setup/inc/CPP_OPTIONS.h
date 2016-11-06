! $Header: /u/gcmpack/MITgcm/verification/global_ocean.cs32x15/code/CPP_OPTIONS.h,v 1.9 2006/07/16 01:56:37 jmc Exp $
! $Name:  $

#ifndef CPP_OPTIONS_H
#define CPP_OPTIONS_H

! CPP flags controlling particular source code features

! o Shortwave heating as extra term in external_forcing.F
! Note: this should be a run-time option
#undef SHORTWAVE_HEATING

! o Include/exclude phi_hyd calculation code
#define INCLUDE_PHIHYD_CALCULATION_CODE

! o Include/exclude call to S/R CONVECT
#define INCLUDE_CONVECT_CALL

! o Include/exclude call to S/R CALC_DIFFUSIVITY
#define INCLUDE_CALC_DIFFUSIVITY_CALL

! o Include/exclude Implicit vertical advection code
#define INCLUDE_IMPLVERTADV_CODE

! o Include/exclude AdamsBashforth-3rd-Order code
#undef ALLOW_ADAMSBASHFORTH_3

! o Include/exclude nonHydrostatic code
#define ALLOW_NONHYDROSTATIC

! o Include pressure loading code
#define ATMOSPHERIC_LOADING

! o Use "Exact Convervation" of fluid in Free-Surface formulation
!   so that d/dt(eta) is exactly equal to - Div.Transport
#define EXACT_CONSERV

! o Allow the use of Non-Linear Free-Surface formulation
!   this implies that surface thickness (hFactors) vary with time
#define NONLIN_FRSURF

! o ALLOW isotropic scaling of harmonic and bi-harmonic terms when
!   using an locally isotropic spherical grid with (dlambda) x (dphi*cos(phi))
! *only for use on a lat-lon grid*
!   Setting this flag here affects both momentum and tracer equation unless
!   it is set/unset again in other header fields (e.g., GAD_OPTIONS.h).
!   The definition of the flag is commented to avoid interference with
!   such other header files.
!   The preferred method is specifying a value for viscAhGrid or viscA4Grid
!   in data which is then automatically scaled by the grid size;
!   the old method of specifying viscAh/viscA4 and this flag is provided
!   for completeness only (and for use with the adjoint).
!#define ISOTROPIC_COS_SCALING

! o This flag selects the form of COSINE(lat) scaling of bi-harmonic term.
! *only for use on a lat-lon grid*
!   Has no effect if ISOTROPIC_COS_SCALING is undefined.
!   Has no effect on vector invariant momentum equations.
!   Setting this flag here affects both momentum and tracer equation unless
!   it is set/unset again in other header fields (e.g., GAD_OPTIONS.h).
!   The definition of the flag is commented to avoid interference with
!   such other header files.
!#define COSINEMETH_III

! o Use "OLD" UV discretisation near boundaries (*not* recommended)
!   Note - only works with  #undef NO_SLIP_LATERAL  in calc_mom_rhs.F
!          because the old code did not have no-slip BCs
#undef  OLD_ADV_BCS

! o Execution environment support options
#include "CPP_EEOPTIONS.h"

! o Include/exclude code specific to the ECCO/SEALION version.
!   AUTODIFF or EXF package.
!   Currently controled by a single header file
!   For this to work, PACKAGES_CONFIG.h needs to be included!
!ph#if (defined (ALLOW_AUTODIFF) || \
!ph     defined (ALLOW_ECCO) || \
!ph     defined (ALLOW_EXF))
!ph# include "ECCO_CPPOPTIONS.h"
!ph#endif

#endif /* CPP_OPTIONS_H */

