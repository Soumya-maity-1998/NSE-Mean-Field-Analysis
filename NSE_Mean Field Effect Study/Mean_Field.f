cccc  Subroutine for calculating mean field due to free proton and neutron gas
cccc	Date-17.06.2019
cccc  ----------------------------------------------------------------- 
      subroutine dmeanfield(rhop,rhon,etap,etan,temp,dkin_p,dkin_n,pot_p
	1, pot_n, poten_dens, emassp, emassn)

	use SLy5_Model_parameters
      implicit real *8 (a-h, o-z)

	dmass = 938.0d0	  ! mass of nucleons
      pi = 3.14159265d0
      univ = (2.0d0*pi*temp)/(1240.0d0*1240.0d0)
	univ3by2 = univ**1.5d0	
	univ5by2 = univ**2.5d0

	rho = rhon + rhop	  ! Total free particle density
	rho_0 = 0.1604d0	  ! Saturation Density
	x = (rho - rho_0)/(3.0d0*rho_0)		 ! Dimensionless parameter
	delta = (rhon - rhop)/(rhon + rhop)	 ! asymmetry parameter
	b = 6.93147d0 ! It was mistakenly written as 3.0103d0 

	v0_is = para_Esat - para_Kinetic*(1.0d0 + para_kinetic_K0)
	v0_iv=para_Esym-(5.0d0/9.0d0)*para_Kinetic*(1.0d0+para_kinetic_K0+
	13.0d0*para_kinetic_Ksym)

	v1_is = -para_Kinetic*(2.0d0 + 5.0d0*para_kinetic_K0)
	v1_iv=para_Lsym-(5.0d0/9.0d0)*para_Kinetic*(2.0d0+5.0d0*
	1para_kinetic_K0+15.0d0*para_kinetic_Ksym)

	v2_is=para_Ksat-2.0d0*para_Kinetic*(-1.0d0+5.0d0*para_kinetic_K0)
	v2_iv = para_Ksym-(10.0d0/9.0d0)*para_Kinetic*(-1.0d0+5.0d0*
	1para_kinetic_K0 + 15.0d0*para_kinetic_Ksym)

	v3_is=para_Qsat-2.0d0*para_Kinetic*(4.0d0-5.0d0*para_kinetic_K0)
	v3_iv = para_Qsym - (10.0d0/9.0d0)*para_Kinetic*(4.0d0 - 5.0d0*
	1para_kinetic_K0 - 15.0d0*para_kinetic_Ksym)

	v4_is=para_Zsat-8.0d0*para_Kinetic*(-7.0d0+5.0d0*para_kinetic_K0)
	v4_iv = para_Zsym - (40.0d0/9.0d0)*para_Kinetic*(-7.0d0+5.0d0*
	1para_kinetic_K0 + 15.0d0*para_kinetic_Ksym)


	a4_is = (243.0d0*v0_is) - (81.0d0*v1_is)+((27.0d0*v2_is)/2.0d0)
	1-((3.0d0*v3_is)/2.0d0) + ((1.0d0*v4_is)/8.0d0)
	a4_iv = (243.0d0*v0_iv) - (81.0d0*v1_iv) + ((27.0d0*v2_iv)/2.0d0)
	1-((3.0d0*v3_iv)/2.0d0) + ((1.0d0*v4_iv)/8.0d0)

cccc  Calculating the Nuclear Potential

	pot1 = v0_is + v0_iv*(delta**2.0d0)

	pot21 = 2.0d0*(v1_is + v1_iv*(delta**2.0d0))*x
	pot22 = (3.0d0/2.0d0)*(v2_is + v2_iv*(delta**2.0d0))*x*x
	pot23 = (2.0d0/3.0d0)*(v3_is + v3_iv*(delta**2.0d0))*x*x*x
	pot24 = (5.0d0/24.0d0)*(v4_is + v4_iv*(delta**2.0d0))*x*x*x*x

	pot2 = pot21 + pot22 + pot23 + pot24

	pot31 = (v1_is + v1_iv*(delta**2.0d0))
	pot32 = (v2_is + v2_iv*(delta**2.0d0))*x
	pot33 = (1.0d0/2.0d0)*(v3_is + v3_iv*(delta**2.0d0))*x*x
	pot34 = (1.0d0/6.0d0)*(v4_is + v4_iv*(delta**2.0d0))*x*x*x

	pot3 = (1.0d0/3.0d0)*(pot31 + pot32 + pot33 + pot34)

	pot40 = v0_iv
	pot41 = v1_iv*x
	pot42 = (1.0d0/2.0d0)*v2_iv*x*x
	pot43 = (1.0d0/6.0d0)*v3_iv*x*x*x
	pot44 = (1.0d0/24.0d0)*v4_iv*x*x*x*x

	pot4n = 2.0d0*delta*(1.0d0-delta)*(pot40+pot41+pot42+pot43 +pot44)
	pot4p = -2.0d0*delta*(1.0d0+delta)*(pot40+pot41+pot42+pot43+pot44)

      pot511 = (5.0d0/3.0d0)*(x**4.0d0)
	pot512 = (6.0d0-b)*(x**5.0d0)
	pot513 = -(3.0d0*b)*(x**6.0d0)
	pot51  = (a4_is + a4_iv*(delta**2.0d0))*(pot511 + pot512 + pot513)
	pot52n = 2.0d0*delta*(1.0d0 - delta)*a4_iv*(x**5.0d0)
	pot52p = -2.0d0*delta*(1.0d0 + delta)*a4_iv*(x**5.0d0)
	pot53  = dexp(-b*(1.0d0 + (3.0d0*x)))

	pot5n = (pot51 + pot52n)*pot53
	pot5p = (pot51 + pot52p)*pot53


	eff_factor_p1 = (para_kinetic_K0 - (para_kinetic_Ksym*delta))
	eff_factor_p  = 1.0d0 + (eff_factor_p1*(rho/rho_0))

	emassp = dmass/eff_factor_p	 ! Effective proton mass

	eff_factor_n1 = (para_kinetic_K0 + (para_kinetic_Ksym*delta))
	eff_factor_n = 1.0d0 + (eff_factor_n1*(rho/rho_0))

	emassn = dmass/eff_factor_n	 ! Effective neutron mass


	order3by2 = 1.5d0
	call Fermi(order3by2, etan, feta_kineticn)
	call Fermi(order3by2, etap, feta_kineticp)

	const_kinetic = 12.0d0*pi*univ5by2
	tau_n = const_kinetic*feta_kineticn*(emassn**2.5d0)
	tau_p = const_kinetic*feta_kineticp*(emassp**2.5d0)

	dkin_p = (1240.0d0*1240.0d0*tau_p)/(8.0d0*pi*pi*emassp)
	dkin_n = (1240.0d0*1240.0d0*tau_n)/(8.0d0*pi*pi*emassn)

	potp_eff1 = (tau_p*(para_kinetic_K0 + para_kinetic_Ksym))/rho_0
	potp_eff2 = (tau_n*(para_kinetic_K0 - para_kinetic_Ksym))/rho_0

	potp_eff = potp_eff1 + potp_eff2

	potn_eff1 = (tau_n*(para_kinetic_K0 + para_kinetic_Ksym))/rho_0
	potn_eff2 = (tau_p*(para_kinetic_K0 - para_kinetic_Ksym))/rho_0

	potn_eff = potn_eff1 + potn_eff2

	pot_p = pot1 + pot2 + pot3 + pot4p + pot5p + potp_eff
	pot_n = pot1 + pot2 + pot3 + pot4n + pot5n + potn_eff

c	--------------------------------------
c     Calculating potential energy density 
c	--------------------------------------
	vpot0 = v0_is + v0_iv*(delta**2.0d0)
	vpot1 = (v1_is + v1_iv*(delta**2.0d0))*x
	vpot2 = (1.0d0/2.0d0)*(v2_is + v2_iv*(delta**2.0d0))*x*x
	vpot3 = (1.0d0/6.0d0)*(v3_is + v3_iv*(delta**2.0d0))*x*x*x
	vpot4 = (1.0d0/24.0d0)*(v4_is + v4_iv*(delta**2.0d0))*x*x*x*x

	a4_is = (243.0d0*v0_is) - (81.0d0*v1_is)+((27.0d0*v2_is)/2.0d0)
	1-((3.0d0*v3_is)/2.0d0) + ((1.0d0*v4_is)/8.0d0)
	a4_iv = (243.0d0*v0_iv) - (81.0d0*v1_iv)+((27.0d0*v2_iv)/2.0d0)
	1-((3.0d0*v3_iv)/2.0d0) + ((1.0d0*v4_iv)/8.0d0)

	vpot_added = (a4_is + a4_iv*(delta**2.0d0))*(x**5.0d0)
	1*(dexp(-b*(1.0d0 + (3.0d0*x))))

	vpot = vpot0 + vpot1 + vpot2 + vpot3 + vpot4 + vpot_added
c	poten_dens = rho*vpot
	poten_dens = 0.0d0 !Changed it to zero to study effect. 

      return
	end