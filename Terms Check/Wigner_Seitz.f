	subroutine seitz_lightnuclei(da,dz,rho_electron,seitz_correction)
      implicit real *8 (a-h, o-z)

	coul_const = 0.864d0	
	rho_0 = 0.1604d0
	para_Lsym = 48.30d0
	para_Ksat = 230.0d0
	para_Ksym = -112.0d0

	delta = (da-2.0d0*dz)/da
      rho = rho_0*(1.0d0-(3.0d0*para_Lsym*delta*delta)/
	1(para_Ksat+(para_Ksym*delta*delta)))

	seitz_factor1 = 1.5d0*(((2.0d0*rho_electron)/((1.0d0-delta)*rho))
	1**(1.0d0/3.0d0))
	seitz_factor2 = 0.5d0*((2.0d0*rho_electron)/((1.0d0-delta)*rho))
	seitz_correction =(coul_const/1.2d0)*(seitz_factor1-seitz_factor2)
	1*(dz/(da**(1.0d0/3.0d0)))
c	seitz_correction=0.0d0
	return
	end