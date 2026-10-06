cccc  Grand canonical model for neutron and proton
cccc	Date-02.04.2019
      implicit real *8 (a-h,o-z)
	dimension yy(0:1000,0:1000),fee(0:1000,0:1000)
	dimension ndriplow(0:1000),ndriphigh(0:1000)
	dimension yyrho(0:1000,0:1000)
	dimension etap_c(0:1000,0:1000),etan_c(0:1000,0:1000)
c      open(unit=4,file='limz.out',status='unknown')



	open(unit = 15, file = 'Mass_Fraction_T=5_rhoB=0.1_Yp_dep.out',
	1status = 'unknown')
	open(unit = 16, file = 'Chemical_Potentials_0.1.out', status = 
	1'unknown')
	open(unit = 17, file = 'Bound_Observables_0.1.out', status = 
	1'unknown')

      open(unit=11,file='A0=2000_T=5_rhoB=0.1_Yp_dependence_Other_pro
	1perties_complete.out',status='unknown')
      open(unit=24,file='A0=2000_T=5_rhoB=0.1_Yp_dependence_excitatio
	1n_cluster.out',status='unknown')
      open(unit=23,file='A0=2000_T=5_rhoB=0.1_Yp_dependence_excitatio
	1n_gas.out',status='unknown')
      open(unit=25,file='A0=2000_T=5_rhoB=0.1_Yp_dependence_Mass_frac
	1tion.out',status='unknown')
      open(unit=26,file='A0=2000_T=5_rhoB=0.1_Yp_dependence_heavy.ou
	1t',status='unknown')
      open(unit=12,file='A0=2000_T=5_rhoB=0.1_Yp_dependence_isotopic
	1.out',status='unknown')
c      open(unit=21,file='A0=2000_yp=0.2_Vf=3.33V0_T=16MeV_free_energy1.o
c	1ut',status='unknown')
c      open(unit=22,file='A0=2000_yp=0.2_Vf=3.33V0_T=16MeV_free_energy2.o
cj	1ut',status='unknown')
c      open(unit=15,file='Others.out',status='unknown')
c      open(unit=12,file='A0=300_Z0=92_Vf=2V0_to_50V0_T=10.0_MeV_mu_eta_U
c	1_conv.out',status='unknown')
c      open(unit=13,file='A0=300_Z0=92_Vf=2V0_to_50V0_T=10.0_MeV_effectiv
c	1e_mass.out',status='unknown')

      pi=3.14159265
	dmass=938.0d0

	rho_0=0.1604d0
	para_Kinetic=22.1d0
	para_kinetic_K0=0.4286d0
	para_kinetic_Ksym=0.1786d0
	para_Esat=-15.98d0
	para_Esym=32.03d0
	para_Lsym=48.30d0
	para_Ksat=230.0d0
	para_Ksym=-112.0d0
	para_Qsat=-364.0d0
	para_Qsym=501.0d0
	para_Zsat=1592.0d0
	para_Zsym=-3087.0d0

c	numn=28.0d0
c	numz=28.0d0



cccc ------------------------------------------------------------------ 
cccc  Maximum possible proton and neutron in a cluster are 100 and 300
cccc ------------------------------------------------------------------
      numzmax=100
	numnmax=300 
cccc ------------------------------------------------------------------
cccc Calculating the driplines
cccc ------------------------------------------------------------------
      ndriplow(0)=1
      ndriphigh(0)=1
      ndriplow(1)=0
      ndriphigh(1)=2
      ndriplow(2)=1
      ndriphigh(2)=4

cccc	Temperature and electron density are during dripline calculation only
	temp0=0.0d0
	rho_electron0=0.0d0

      do 33 iz=3,numzmax
      diz=dfloat(iz)
      iamin=nint(diz*1.2)
 31   amin=dfloat(iamin)
      call binding(amin,diz,temp0,rho_electron0,res1,etapc,etanc)
      daless=amin-1.0d0
      dzless=diz-1.0d0
      call binding(daless,dzless,temp0,rho_electron0,res2,etapc,etanc)
       if(res1.le.res2)go to 32
      iamin=iamin+1
      go to 31
 32   ndriplow(iz)=iamin-iz
 
      iamax=iamin
 34   continue
      amax=dfloat(iamax)
      call binding(amax,diz,temp0,rho_electron0,res1,etapc,etanc)
      dmore=amax+1.0
      
      call binding(dmore,diz,temp0,rho_electron0,res2,etapc,etanc)
      if(res1.le.res2)go to 35
      iamax=iamax+1
	inmax=iamax-iz
      if(inmax.ge.numnmax)then
      iamax=iz+numnmax
      go to 35
      end if
      go to 34
 35   ndriphigh(iz)=iamax-iz

 33   continue
      
      do 51 i=0,numzmax
c      write(4,*)i,ndriplow(i),ndriphigh(i)
 51   continue

	if(numnmax.gt.ndriphigh(numzmax)) then
	numnmax=ndriphigh(numzmax)
	end if
c	write(*,*)numnmax
	numamax=numzmax+numnmax

      do iz=3,numzmax
	ndriphigh(iz)=ndriphigh(iz)
      end do

c	goto 999

cccc ------------------------------------------------------------------
ccc   Guesss value of BetaMu
cccc ------------------------------------------------------------------
      betamuz=-0.2d0
      betamun=-0.2d0


cccc ------------------------------------------------------------------
cccc Input Density, temperature and proton fraction
cccc ------------------------------------------------------------------









	tempi=5.0d0
      do itemp=1,1
	temp=tempi-dfloat(itemp-1)*0.50d0

	do idens=10,10
c      dens_ratio=0.3d0
	dens_ratio=0.01d0*dfloat(idens)
	volf_ratio=1.0d0/dens_ratio

	if (itemp.eq.1) then
	numa=2000
	do ifrac=60,2,-1
	proton_frac=0.01d0*dfloat(ifrac)
	numz=nint(dfloat(numa)*proton_frac)
	numn=numa-numz



cccc-------------------------------------------------------------------
	vol=volf_ratio*dfloat(numa)/0.1604d0
      vol_normal=dfloat(numa)/0.1604d0
	densbyrho0=1.0d0/volf_ratio
	dens=0.1604d0/volf_ratio
      rho_electron=dens*proton_frac
      univ=(2.0d0*pi*temp)/(1240.0d0*1240.0d0)
	univ3by2=univ**1.5d0
	univ5by2=univ**2.5d0


	numnmax3=numnmax*3
      do i=1,numzmax
	do j=1,numnmax3
	  fee(i,j)=0.0d0
	  etap_c(i,j)=0.0d0
	  etan_c(i,j)=0.0d0
	end do
	end do


	dz_d=1.0d0
      dn_d=1.0d0
	da_d=dz_d+dn_d
      spin_d=3.0
	call seitz_lightnuclei(da_d,dz_d,rho_electron,seitz_corr_d)
      bind_d=-2.225-seitz_corr_d
      fee(1,1)=-bind_d/temp+dlog(spin_d)

	dz_tr=1.0d0
      dn_tr=2.0d0
	da_tr=dz_tr+dn_tr
      spin_tr=2.0
	call seitz_lightnuclei(da_tr,dz_tr,rho_electron,seitz_corr_tr)
      bind_tr=-8.482-seitz_corr_tr
      fee(1,2)=-bind_tr/temp+dlog(spin_tr)

	dz_H4=1.0d0
      dn_H4=3.0d0
	da_H4=dz_H4+dn_H4
      spin_H4=5.0
	call seitz_lightnuclei(da_H4,dz_H4,rho_electron,seitz_corr_H4)
      bind_H4=-6.881-seitz_corr_H4
      fee(1,3)=-bind_H4/temp+dlog(spin_H4)

	dz_H5=1.0d0
      dn_H5=4.0d0
	da_H5=dz_H5+dn_H5
      spin_H5=2.0
	call seitz_lightnuclei(da_H5,dz_H5,rho_electron,seitz_corr_H5)
      bind_H5=-6.682-seitz_corr_H5
      fee(1,4)=-bind_H5/temp+dlog(spin_H5)

	dz_H6=1.0d0
      dn_H6=5.0d0
	da_H6=dz_H6+dn_H6
      spin_H6=5.0
	call seitz_lightnuclei(da_H6,dz_H6,rho_electron,seitz_corr_H6)
      bind_H6=-5.769-seitz_corr_H6
      fee(1,5)=-bind_H6/temp+dlog(spin_H6)

	dz_H7=1.0d0
      dn_H7=5.0d0
	da_H7=dz_H7+dn_H7
      spin_H7=2.0
	call seitz_lightnuclei(da_H7,dz_H7,rho_electron,seitz_corr_H7)
      bind_H7=-6.580-seitz_corr_H7
      fee(1,6)=-bind_H7/temp+dlog(spin_H7)

	dz_he3=2.0d0
      dn_he3=1.0d0
	da_he3=dz_he3+dn_he3
      spin_he3=2.0
	call seitz_lightnuclei(da_he3,dz_he3,rho_electron,seitz_corr_he3)
      bind_he3=-7.718-seitz_corr_he3
      fee(2,1)=-bind_he3/temp+dlog(spin_he3)

	dz_he4=2.0d0
      dn_he4=2.0d0
	da_he4=dz_he4+dn_he4
	call seitz_lightnuclei(da_he4,dz_he4,rho_electron,seitz_corr_he4)
      bind_he4=-28.296-seitz_corr_he4
      fee(2,2)=-bind_he4/temp 

	dz_he5=2.0d0
      dn_he5=3.0d0
	da_he5=dz_he5+dn_he5
      spin_he5=4.0
	call seitz_lightnuclei(da_he5,dz_he5,rho_electron,seitz_corr_he5)
      bind_he5=-27.561-seitz_corr_he5
      fee(2,3)=-bind_he5/temp+dlog(spin_he5)

	dz_he6=2.0d0
      dn_he6=4.0d0
	da_he6=dz_he6+dn_he6
      spin_he6=1.0
	call seitz_lightnuclei(da_he6,dz_he6,rho_electron,seitz_corr_he6)
      bind_he6=-29.271-seitz_corr_he6
      fee(2,4)=-bind_he6/temp+dlog(spin_he6)

	dz_he7=2.0d0
      dn_he7=5.0d0
	da_he7=dz_he7+dn_he7
      spin_he7=4.0
	call seitz_lightnuclei(da_he7,dz_he7,rho_electron,seitz_corr_he7)
      bind_he7=-28.861-seitz_corr_he7
      fee(2,5)=-bind_he7/temp+dlog(spin_he7)

	dz_he8=2.0d0
      dn_he8=6.0d0
	da_he8=dz_he8+dn_he8
      spin_he8=1.0
	call seitz_lightnuclei(da_he8,dz_he8,rho_electron,seitz_corr_he8)
      bind_he8=-31.396-seitz_corr_he8
      fee(2,6)=-bind_he8/temp+dlog(spin_he8)

	dz_he9=2.0d0
      dn_he9=7.0d0
	da_he9=dz_he9+dn_he9
      spin_he9=2.0
	call seitz_lightnuclei(da_he9,dz_he9,rho_electron,seitz_corr_he9)
      bind_he9=-30.141-seitz_corr_he9
      fee(2,7)=-bind_he9/temp+dlog(spin_he9)

	dz_he10=2.0d0
      dn_he10=8.0d0
	da_he10=dz_he10+dn_he10
      spin_he10=2.0
	call seitz_lightnuclei(da_he10,dz_he10,rho_electron
	1,seitz_corr_he10)
      bind_he10=-29.951-seitz_corr_he10
      fee(2,8)=-bind_he10/temp+dlog(spin_he10)

c      tc=18.0
c      tcsq=tc*tc
c      tempsq=temp*temp
c      tens=18.0*((tcsq-tempsq)/(tcsq+tempsq))**(5./4.0)
c       xxx=tcsq-tempsq
c      yyy=tcsq+tempsq
c      aaa=xxx**0.25/(yyy**1.25)+xxx**1.25/(yyy**2.25)
c      sens=2.5*temp*temp*aaa*18+tens
c	seitz=1.0d0
c	zc=0.72d0

      do iz=3,numzmax
      do in=ndriplow(iz),ndriphigh(iz)
      if(in.gt.numnmax3)goto 100
	ia=iz+in     
      da=dfloat(ia)
      dz=dfloat(iz)
c      seitz=1.0
c      xx_gr1=15.8*da-tens*(da**.666667)
c     1-zc*dz*dz*seitz/(da**.33333)
c     1-23.5d0*((da-2.*dz)**2.)/da



	call binding(da,dz,temp,rho_electron,bind,etapc,etanc)
c	call entropying(da,dz,temp,entropy)
	fee(iz,in)=-(bind)/temp
	etap_c(iz,in)=etapc
	etan_c(iz,in)=etanc
c	if(iz.eq.8) then
c	write(*,*)
c	write(*,221)iz,ia,fee(iz,in)
c	end if

100   continue
      end do
	end do


c221   format(2i5,f9.3)

c	goto 999

	iter=0
	emassp=dmass
	emassn=dmass
	potfp=0.0d0
	potfn=0.0d0
c	goto 554
ccc   Iterative technique for finding BetaMun and BetaMuz
ccc----------------------------------------------------
200   continue
      iter=iter+1
c	write(*,*)iter
	sumn=0.0d0
	sumz=0.0d0
	derivnn=0.0d0
	derivnz=0.0d0
	derivzn=0.0d0
	derivzz=0.0d0
	sum_clust=0.0d0
	etap=betamuz-(potfp/temp)
	etan=betamun-(potfn/temp)
	do iz=0,numzmax
	do in=ndriplow(iz),ndriphigh(iz)
	if(in.gt.numnmax3)goto 199
	  dz=dfloat(iz)
	  dn=dfloat(in)
	  da=dz+dn
	if(iz.eq.0.and.in.eq.1) then
	rhofn=univ3by2*(emassn**1.5d0)*exp(etan+(potfn/temp))
	dlhsn=rhofn/(2.0d0*univ3by2*(emassn**1.5d0))
      call Etainv(dlhsn,etan)
	else if(iz.eq.1.and.in.eq.0) then
	rhofp=univ3by2*(emassp**1.5d0)*exp(etap+(potfp/temp))
	dlhsp=rhofp/(2.0d0*univ3by2*(emassp**1.5d0))
      call Etainv(dlhsp,etap)
	call dmeanfield(rhofp,rhofn,etap,etan,temp,dkin_p,dkin_n,potfp
	1,potfn,poten_dens,emassp,emassn)
	else
c	  if(iz.eq.in) then
	  if(iz.eq.1.and.in.eq.1) then
	  fee_eff=fee(iz,in)
	  else if(iz.eq.1.and.in.eq.2) then	   
	  fee_eff=fee(iz,in)
	  else if(iz.eq.1.and.in.eq.3) then	   
	  fee_eff=fee(iz,in)
	  else if(iz.eq.1.and.in.eq.4) then	   
	  fee_eff=fee(iz,in)
	  else if(iz.eq.1.and.in.eq.5) then	   
	  fee_eff=fee(iz,in)
	  else if(iz.eq.1.and.in.eq.6) then	   
	  fee_eff=fee(iz,in)
	  else if(iz.eq.2.and.in.eq.1) then	   
	  fee_eff=fee(iz,in)
	  else if(iz.eq.2.and.in.eq.2) then	   
	  fee_eff=fee(iz,in)
	  else if(iz.eq.2.and.in.eq.3) then	   
	  fee_eff=fee(iz,in)
	  else if(iz.eq.2.and.in.eq.4) then	   
	  fee_eff=fee(iz,in)
	  else if(iz.eq.2.and.in.eq.5) then	   
	  fee_eff=fee(iz,in)
	  else if(iz.eq.2.and.in.eq.6) then	   
	  fee_eff=fee(iz,in)
	  else if(iz.eq.2.and.in.eq.7) then	   
	  fee_eff=fee(iz,in)
	  else if(iz.eq.2.and.in.eq.8) then	   
	  fee_eff=fee(iz,in)
	  else	   
  	  delta=(da-(2.0d0*dz))/da
        rho=rho_0*(1.0d0-(3.0d0*para_Lsym*delta*delta)/
	1(para_Ksat+(para_Ksym*delta*delta)))
	  rhopc=(rho*dz)/da
	  rhonc=rho-rhopc
	  fee_gas_cor1=-((2.0d0/3.0d0)*(dkin_n+dkin_p))/temp
	  fee_gas_cor2=poten_dens/temp
	  fee_gas_cor3n=rhofn*etan
	  fee_gas_cor3p=rhofp*etap
	  fee_gas_cor3=fee_gas_cor3p+fee_gas_cor3n
	  fee_gas_cor=((fee_gas_cor1+fee_gas_cor2+fee_gas_cor3)*da)/rho	
	  fee_eff=fee(iz,in)+fee_gas_cor
	  end if
	  termsumn=univ3by2*(dmass**1.5d0)*dn*((dz+dn)**1.5d0)
	1*exp((dn*(etan+(potfn/temp)))+(dz*(etap+(potfp/temp)))+fee_eff)
	  termsumz=univ3by2*(dmass**1.5d0)*dz*((dz+dn)**1.5d0)
	1*exp((dn*(etan+(potfn/temp)))+(dz*(etap+(potfp/temp)))+fee_eff)
	  sumn=sumn+termsumn
	  sumz=sumz+termsumz
	  termderivnn=univ3by2*(dmass**1.5d0)*(dn**2.0d0)*((dz+dn)**1.5d0)
	1*exp((dn*(etan+(potfn/temp)))+(dz*(etap+(potfp/temp)))+fee_eff)
	  termderivzz=univ3by2*(dmass**1.5d0)*(dz**2.0d0)*((dz+dn)**1.5d0)
     1*exp((dn*(etan+(potfn/temp)))+(dz*(etap+(potfp/temp)))+fee_eff)
	  termderivnz=univ3by2*(dmass**1.5d0)*(dn*dz)*((dz+dn)**1.5d0)
	1*exp((dn*(etan+(potfn/temp)))+(dz*(etap+(potfp/temp)))+fee_eff)
	  termderivzn=univ3by2*(dmass**1.5d0)*(dn*dz)*((dz+dn)**1.5d0)
	1*exp((dn*(etan+(potfn/temp)))+(dz*(etap+(potfp/temp)))+fee_eff)
	  derivnn=derivnn+termderivnn
	  derivnz=derivnz+termderivnz
	  derivzn=derivzn+termderivzn
	  derivzz=derivzz+termderivzz
	  termsum_clust=univ3by2*(dmass**1.5d0)*((dz+dn)**2.5d0)
	1*exp((dn*(etan+(potfn/temp)))+(dz*(etap+(potfp/temp)))+fee_eff)
	  sum_clust=sum_clust+termsum_clust
c	  end if
	end if
199   continue
	end do
	end do
	frac_clust=sum_clust/(sum_clust+rhofp+rhofn)
c	frac_clust=1.0d0
   	vol_av=vol-(vol_normal*frac_clust)
	  funcn=dfloat(numn)-(sumn*vol_av+rhofn*vol)
	  funcz=dfloat(numz)-(sumz*vol_av+rhofp*vol)
	  derivnn=-(derivnn*vol_av+rhofn*vol)
	  derivnz=-derivnz*vol_av
	  derivzn=-derivzn*vol_av
	  derivzz=-(derivzz*vol_av+rhofp*vol)
	dlower=(derivnn*derivzz)-(derivnz*derivnz)
	dupper_n=(funcn*derivzz)-(funcz*derivnz)
	dupper_z=-(funcn*derivzn)+(funcz*derivnn)
c	write(*,*)
c	write(*,35)iter,betamuz,betamun,funcn,funcz
c35    format(i10,4f20.5)
c      write(*,*)
c	write(*,*)iter,funcz,funcn
      if(dabs(funcn).gt.1.0e-10.and.dabs(funcz).gt.1.0e-10) then
	betamun=betamun-(dupper_n/dlower)
	betamuz=betamuz-(dupper_z/dlower)
	goto 200
	end if
c	write(*,*)"Here"
554	continue
      vol_av_req=vol_av
	goto 555
	iter2=0
300	continue
	iter2=iter2+1
	betamun1=betamun
	betamuz1=betamuz
	sumn=0.0d0
	sumz=0.0d0
	sum_clust=0.0d0
	potfp_req=potfp
	potfn_req=potfn
	etap=betamuz1-(potfp/temp)
	etan=betamun1-(potfn/temp)
	do iz=0,numzmax
	do in=ndriplow(iz),ndriphigh(iz)
	if(in.gt.numnmax3)goto 299
	  dz=dfloat(iz)
	  dn=dfloat(in)
	  da=dz+dn
	if(iz.eq.0.and.in.eq.1) then
	rhofn=univ3by2*(emassn**1.5d0)*exp(etan+(potfn/temp))
	dlhsn=rhofn/(2.0d0*univ3by2*(emassn**1.5d0))
      call Etainv(dlhsn,etan)
	else if(iz.eq.1.and.in.eq.0) then
	rhofp=univ3by2*(emassp**1.5d0)*exp(etap+(potfp/temp))
	dlhsp=rhofp/(2.0d0*univ3by2*(emassp**1.5d0))
      call Etainv(dlhsp,etap)
	call dmeanfield(rhofp,rhofn,etap,etan,temp,dkin_p,dkin_n,potfp
	1,potfn,poten_dens,emassp,emassn)
	else
c        if(iz.eq.in) then
	  if(iz.eq.1.and.in.eq.1) then
	  fee_eff=fee(iz,in)
	  else if(iz.eq.1.and.in.eq.2) then	   
	  fee_eff=fee(iz,in)
	  else if(iz.eq.1.and.in.eq.3) then	   
	  fee_eff=fee(iz,in)
	  else if(iz.eq.1.and.in.eq.4) then	   
	  fee_eff=fee(iz,in)
	  else if(iz.eq.1.and.in.eq.5) then	   
	  fee_eff=fee(iz,in)
	  else if(iz.eq.1.and.in.eq.6) then	   
	  fee_eff=fee(iz,in)
	  else if(iz.eq.2.and.in.eq.1) then	   
	  fee_eff=fee(iz,in)
	  else if(iz.eq.2.and.in.eq.2) then	   
	  fee_eff=fee(iz,in)
	  else if(iz.eq.2.and.in.eq.3) then	   
	  fee_eff=fee(iz,in)
	  else if(iz.eq.2.and.in.eq.4) then	   
	  fee_eff=fee(iz,in)
	  else if(iz.eq.2.and.in.eq.5) then	   
	  fee_eff=fee(iz,in)
	  else if(iz.eq.2.and.in.eq.6) then	   
	  fee_eff=fee(iz,in)
	  else if(iz.eq.2.and.in.eq.7) then	   
	  fee_eff=fee(iz,in)
	  else if(iz.eq.2.and.in.eq.8) then	   
	  fee_eff=fee(iz,in)
	  else	   
  	  delta=(da-(2.0d0*dz))/da
        rho=rho_0*(1.0d0-(3.0d0*para_Lsym*delta*delta)/
	1(para_Ksat+(para_Ksym*delta*delta)))
	  rhopc=(rho*dz)/da
	  rhonc=rho-rhopc
	  fee_gas_cor1=-((2.0d0/3.0d0)*(dkin_n+dkin_p))/temp
	  fee_gas_cor2=poten_dens/temp
	  fee_gas_cor3n=rhofn*etan
	  fee_gas_cor3p=rhofp*etap
	  fee_gas_cor3=fee_gas_cor3p+fee_gas_cor3n
	  fee_gas_cor=((fee_gas_cor1+fee_gas_cor2+fee_gas_cor3)*da)/rho	
	  fee_eff=fee(iz,in)+fee_gas_cor
	  end if
	  termsumn=univ3by2*(dmass**1.5d0)*dn*((dz+dn)**1.5d0)
	1*exp((dn*(etan+(potfn/temp)))+(dz*(etap+(potfp/temp)))+fee_eff)
	  termsumz=univ3by2*(dmass**1.5d0)*dz*((dz+dn)**1.5d0)
	1*exp((dn*(etan+(potfn/temp)))+(dz*(etap+(potfp/temp)))+fee_eff)
	  sumn=sumn+termsumn
	  sumz=sumz+termsumz
	  termsum_clust=univ3by2*(dmass**1.5d0)*((dz+dn)**2.5d0)
	1*exp((dn*(etan+(potfn/temp)))+(dz*(etap+(potfp/temp)))+fee_eff)
	  sum_clust=sum_clust+termsum_clust
c	  end if
	end if
299   continue
	end do
	end do
	frac_clust=sum_clust/(sum_clust+rhofp+rhofn)
   	vol_av=vol-(vol_normal*frac_clust)
	vol_av_req=vol_av
	emassp_req=emassp
	emassn_req=emassn
	dnumbn11=(sumn*vol_av+rhofn*vol)
	dnumbz11=(sumz*vol_av+rhofp*vol)
c	write(*,*)
c	write(*,*)dnumbn11,dnumbz11
c      if(temp.ge.4.5) then
	dbetamun=-0.1d0
	dbetamuz=-0.1d0
c	else
c	dbetamun=-0.05d0
c	dbetamuz=-0.05d0
c	end if


	betamuz2=betamuz1+dbetamuz
	betamun2=betamun1+dbetamun
	sumn=0.0d0
	sumz=0.0d0
	sum_clust=0.0d0
	potfp=potfp_req
	potfn=potfn_req
	etap=betamuz2-(potfp/temp)
	etan=betamun1-(potfn/temp)

	do iz=0,numzmax
	do in=ndriplow(iz),ndriphigh(iz)
	if(in.gt.numnmax3)goto 399
	  dz=dfloat(iz)
	  dn=dfloat(in)
	  da=dz+dn
	if(iz.eq.0.and.in.eq.1) then
	rhofn=univ3by2*(emassn**1.5d0)*exp(etan+(potfn/temp))
	dlhsn=rhofn/(2.0d0*univ3by2*(emassn**1.5d0))
      call Etainv(dlhsn,etan)
	else if(iz.eq.1.and.in.eq.0) then
	rhofp=univ3by2*(emassp**1.5d0)*exp(etap+(potfp/temp))
	dlhsp=rhofp/(2.0d0*univ3by2*(emassp**1.5d0))
      call Etainv(dlhsp,etap)
	call dmeanfield(rhofp,rhofn,etap,etan,temp,dkin_p,dkin_n,potfp
	1,potfn,poten_dens,emassp,emassn)
	else
c	  if(iz.eq.in) then
	  if(iz.eq.1.and.in.eq.1) then
	  fee_eff=fee(iz,in)
	  else if(iz.eq.1.and.in.eq.2) then	   
	  fee_eff=fee(iz,in)
	  else if(iz.eq.1.and.in.eq.3) then	   
	  fee_eff=fee(iz,in)
	  else if(iz.eq.1.and.in.eq.4) then	   
	  fee_eff=fee(iz,in)
	  else if(iz.eq.1.and.in.eq.5) then	   
	  fee_eff=fee(iz,in)
	  else if(iz.eq.1.and.in.eq.6) then	   
	  fee_eff=fee(iz,in)
	  else if(iz.eq.2.and.in.eq.1) then	   
	  fee_eff=fee(iz,in)
	  else if(iz.eq.2.and.in.eq.2) then	   
	  fee_eff=fee(iz,in)
	  else if(iz.eq.2.and.in.eq.3) then	   
	  fee_eff=fee(iz,in)
	  else if(iz.eq.2.and.in.eq.4) then	   
	  fee_eff=fee(iz,in)
	  else if(iz.eq.2.and.in.eq.5) then	   
	  fee_eff=fee(iz,in)
	  else if(iz.eq.2.and.in.eq.6) then	   
	  fee_eff=fee(iz,in)
	  else if(iz.eq.2.and.in.eq.7) then	   
	  fee_eff=fee(iz,in)
	  else if(iz.eq.2.and.in.eq.8) then	   
	  fee_eff=fee(iz,in)
	  else	   
  	  delta=(da-(2.0d0*dz))/da
        rho=rho_0*(1.0d0-(3.0d0*para_Lsym*delta*delta)/
	1(para_Ksat+(para_Ksym*delta*delta)))
	  rhopc=(rho*dz)/da
	  rhonc=rho-rhopc
	  fee_gas_cor1=-((2.0d0/3.0d0)*(dkin_n+dkin_p))/temp
	  fee_gas_cor2=poten_dens/temp
	  fee_gas_cor3n=rhofn*etan
	  fee_gas_cor3p=rhofp*etap
	  fee_gas_cor3=fee_gas_cor3p+fee_gas_cor3n
	  fee_gas_cor=((fee_gas_cor1+fee_gas_cor2+fee_gas_cor3)*da)/rho	
	  fee_eff=fee(iz,in)+fee_gas_cor
	  end if
	  termsumn=univ3by2*(dmass**1.5d0)*dn*((dz+dn)**1.5d0)
	1*exp((dn*(etan+(potfn/temp)))+(dz*(etap+(potfp/temp)))+fee_eff)
	  termsumz=univ3by2*(dmass**1.5d0)*dz*((dz+dn)**1.5d0)
	1*exp((dn*(etan+(potfn/temp)))+(dz*(etap+(potfp/temp)))+fee_eff)
	  sumn=sumn+termsumn
	  sumz=sumz+termsumz
	  termsum_clust=univ3by2*(dmass**1.5d0)*((dz+dn)**2.5d0)
	1*exp((dn*(etan+(potfn/temp)))+(dz*(etap+(potfp/temp)))+fee_eff)
	  sum_clust=sum_clust+termsum_clust
c	  end if
	end if
399   continue
	end do
	end do
	frac_clust=sum_clust/(sum_clust+rhofp+rhofn)
   	vol_av=vol-(vol_normal*frac_clust)
	dnumbn12=(sumn*vol_av+rhofn*vol)
	dnumbz12=(sumz*vol_av+rhofp*vol)

	sumn=0.0d0
	sumz=0.0d0
	sum_clust=0.0d0
	potfp=potfp_req
	potfn=potfn_req
	etap=betamuz1-(potfp/temp)
	etan=betamun2-(potfn/temp)
	do iz=0,numzmax
	do in=ndriplow(iz),ndriphigh(iz)
	if(in.gt.numnmax3)goto 499
	  dz=dfloat(iz)
	  dn=dfloat(in)
	  da=dz+dn
	if(iz.eq.0.and.in.eq.1) then
	rhofn=univ3by2*(emassn**1.5d0)*exp(etan+(potfn/temp))
	dlhsn=rhofn/(2.0d0*univ3by2*(emassn**1.5d0))
      call Etainv(dlhsn,etan)
	else if(iz.eq.1.and.in.eq.0) then
	rhofp=univ3by2*(emassp**1.5d0)*exp(etap+(potfp/temp))
	dlhsp=rhofp/(2.0d0*univ3by2*(emassp**1.5d0))
      call Etainv(dlhsp,etap)
	call dmeanfield(rhofp,rhofn,etap,etan,temp,dkin_p,dkin_n,potfp
	1,potfn,poten_dens,emassp,emassn)
	else
c	  if(iz.eq.in) then
	  if(iz.eq.1.and.in.eq.1) then
	  fee_eff=fee(iz,in)
	  else if(iz.eq.1.and.in.eq.2) then	   
	  fee_eff=fee(iz,in)
	  else if(iz.eq.1.and.in.eq.3) then	   
	  fee_eff=fee(iz,in)
	  else if(iz.eq.1.and.in.eq.4) then	   
	  fee_eff=fee(iz,in)
	  else if(iz.eq.1.and.in.eq.5) then	   
	  fee_eff=fee(iz,in)
	  else if(iz.eq.1.and.in.eq.6) then	   
	  fee_eff=fee(iz,in)
	  else if(iz.eq.2.and.in.eq.1) then	   
	  fee_eff=fee(iz,in)
	  else if(iz.eq.2.and.in.eq.2) then	   
	  fee_eff=fee(iz,in)
	  else if(iz.eq.2.and.in.eq.3) then	   
	  fee_eff=fee(iz,in)
	  else if(iz.eq.2.and.in.eq.4) then	   
	  fee_eff=fee(iz,in)
	  else if(iz.eq.2.and.in.eq.5) then	   
	  fee_eff=fee(iz,in)
	  else if(iz.eq.2.and.in.eq.6) then	   
	  fee_eff=fee(iz,in)
	  else if(iz.eq.2.and.in.eq.7) then	   
	  fee_eff=fee(iz,in)
	  else if(iz.eq.2.and.in.eq.8) then	   
	  fee_eff=fee(iz,in)
	  else	   
  	  delta=(da-(2.0d0*dz))/da
        rho=rho_0*(1.0d0-(3.0d0*para_Lsym*delta*delta)/
	1(para_Ksat+(para_Ksym*delta*delta)))
	  rhopc=(rho*dz)/da
	  rhonc=rho-rhopc
	  fee_gas_cor1=-((2.0d0/3.0d0)*(dkin_n+dkin_p))/temp
	  fee_gas_cor2=poten_dens/temp
	  fee_gas_cor3n=rhofn*etan
	  fee_gas_cor3p=rhofp*etap
	  fee_gas_cor3=fee_gas_cor3p+fee_gas_cor3n
	  fee_gas_cor=((fee_gas_cor1+fee_gas_cor2+fee_gas_cor3)*da)/rho	
	  fee_eff=fee(iz,in)+fee_gas_cor
	  end if
	  termsumn=univ3by2*(dmass**1.5d0)*dn*((dz+dn)**1.5d0)
	1*exp((dn*(etan+(potfn/temp)))+(dz*(etap+(potfp/temp)))+fee_eff)
	  termsumz=univ3by2*(dmass**1.5d0)*dz*((dz+dn)**1.5d0)
	1*exp((dn*(etan+(potfn/temp)))+(dz*(etap+(potfp/temp)))+fee_eff)
	  sumn=sumn+termsumn
	  sumz=sumz+termsumz
	  termsum_clust=univ3by2*(dmass**1.5d0)*((dz+dn)**2.5d0)
	1*exp((dn*(etan+(potfn/temp)))+(dz*(etap+(potfp/temp)))+fee_eff)
	  sum_clust=sum_clust+termsum_clust
c        end if 
	end if
499   continue
	end do
	end do
	frac_clust=sum_clust/(sum_clust+rhofp+rhofn)
   	vol_av=vol-(vol_normal*frac_clust)
	dnumbn21=(sumn*vol_av+rhofn*vol)
	dnumbz21=(sumz*vol_av+rhofp*vol)


	sumn=0.0d0
	sumz=0.0d0
	sum_clust=0.0d0
	potfp=potfp_req
	potfn=potfn_req
	etap=betamuz2-(potfp/temp)
	etan=betamun2-(potfn/temp)
	do iz=0,numzmax
	do in=ndriplow(iz),ndriphigh(iz)
	if(in.gt.numnmax3)goto 599
	  dz=dfloat(iz)
	  dn=dfloat(in)
	  da=dz+dn
	if(iz.eq.0.and.in.eq.1) then
	rhofn=univ3by2*(emassn**1.5d0)*exp(etan+(potfn/temp))
	dlhsn=rhofn/(2.0d0*univ3by2*(emassn**1.5d0))
      call Etainv(dlhsn,etan)
	else if(iz.eq.1.and.in.eq.0) then
	rhofp=univ3by2*(emassp**1.5d0)*exp(etap+(potfp/temp))
	dlhsp=rhofp/(2.0d0*univ3by2*(emassp**1.5d0))
      call Etainv(dlhsp,etap)
	call dmeanfield(rhofp,rhofn,etap,etan,temp,dkin_p,dkin_n,potfp
	1,potfn,poten_dens,emassp,emassn)
	else
c	  if(iz.eq.in) then
	  if(iz.eq.1.and.in.eq.1) then
	  fee_eff=fee(iz,in)
	  else if(iz.eq.1.and.in.eq.2) then	   
	  fee_eff=fee(iz,in)
	  else if(iz.eq.1.and.in.eq.3) then	   
	  fee_eff=fee(iz,in)
	  else if(iz.eq.1.and.in.eq.4) then	   
	  fee_eff=fee(iz,in)
	  else if(iz.eq.1.and.in.eq.5) then	   
	  fee_eff=fee(iz,in)
	  else if(iz.eq.1.and.in.eq.6) then	   
	  fee_eff=fee(iz,in)
	  else if(iz.eq.2.and.in.eq.1) then	   
	  fee_eff=fee(iz,in)
	  else if(iz.eq.2.and.in.eq.2) then	   
	  fee_eff=fee(iz,in)
	  else if(iz.eq.2.and.in.eq.3) then	   
	  fee_eff=fee(iz,in)
	  else if(iz.eq.2.and.in.eq.4) then	   
	  fee_eff=fee(iz,in)
	  else if(iz.eq.2.and.in.eq.5) then	   
	  fee_eff=fee(iz,in)
	  else if(iz.eq.2.and.in.eq.6) then	   
	  fee_eff=fee(iz,in)
	  else if(iz.eq.2.and.in.eq.7) then	   
	  fee_eff=fee(iz,in)
	  else if(iz.eq.2.and.in.eq.8) then	   
	  fee_eff=fee(iz,in)
	  else	   
  	  delta=(da-(2.0d0*dz))/da
        rho=rho_0*(1.0d0-(3.0d0*para_Lsym*delta*delta)/
	1(para_Ksat+(para_Ksym*delta*delta)))
	  rhopc=(rho*dz)/da
	  rhonc=rho-rhopc
	  fee_gas_cor1=-((2.0d0/3.0d0)*(dkin_n+dkin_p))/temp
	  fee_gas_cor2=poten_dens/temp
	  fee_gas_cor3n=rhofn*etan
	  fee_gas_cor3p=rhofp*etap
	  fee_gas_cor3=fee_gas_cor3p+fee_gas_cor3n
	  fee_gas_cor=((fee_gas_cor1+fee_gas_cor2+fee_gas_cor3)*da)/rho	
	  fee_eff=fee(iz,in)+fee_gas_cor
	  end if
	  termsumn=univ3by2*(dmass**1.5d0)*dn*((dz+dn)**1.5d0)
	1*exp((dn*(etan+(potfn/temp)))+(dz*(etap+(potfp/temp)))+fee_eff)
	  termsumz=univ3by2*(dmass**1.5d0)*dz*((dz+dn)**1.5d0)
	1*exp((dn*(etan+(potfn/temp)))+(dz*(etap+(potfp/temp)))+fee_eff)
	  sumn=sumn+termsumn
	  sumz=sumz+termsumz
	  termsum_clust=univ3by2*(dmass**1.5d0)*((dz+dn)**2.5d0)
	1*exp((dn*(etan+(potfn/temp)))+(dz*(etap+(potfp/temp)))+fee_eff)
	  sum_clust=sum_clust+termsum_clust
c        end if
	end if
599   continue
	end do
	end do
	frac_clust=sum_clust/(sum_clust+rhofp+rhofn)
   	vol_av=vol-(vol_normal*frac_clust)
	dnumbn22=(sumn*vol_av+rhofn*vol)
	dnumbz22=(sumz*vol_av+rhofp*vol)

	dndbetan_betaz1=(dnumbn21-dnumbn11)/(betamun2-betamun1)
	dndbetaz_betan1=(dnumbn12-dnumbn11)/(betamuz2-betamuz1)
	dzdbetan_betaz1=(dnumbz21-dnumbz11)/(betamun2-betamun1)
	dzdbetaz_betan1=(dnumbz12-dnumbz11)/(betamuz2-betamuz1)

	dndbetan_betaz2=(dnumbn22-dnumbn12)/(betamun2-betamun1)
	dndbetaz_betan2=(dnumbn22-dnumbn21)/(betamuz2-betamuz1)
	dzdbetan_betaz2=(dnumbz22-dnumbz12)/(betamun2-betamun1)
	dzdbetaz_betan2=(dnumbz22-dnumbz21)/(betamuz2-betamuz1)

	dndbetan_betaz=(dndbetan_betaz1+dndbetan_betaz2)/2.0d0
	dndbetaz_betan=(dndbetaz_betan1+dndbetaz_betan2)/2.0d0
	dzdbetan_betaz=(dzdbetan_betaz1+dzdbetan_betaz2)/2.0d0
	dzdbetaz_betan=(dzdbetaz_betan1+dzdbetaz_betan2)/2.0d0
	deno_c=(dndbetan_betaz*dzdbetaz_betan)-(dndbetaz_betan
	1*dzdbetan_betaz)
	if(deno_c.eq.0.0d0) then
	write(*,*)"Sorry, change dbetamu and repeat the calculation"
	goto 999
	end if
	dneu_cn=(dnumbn11-dfloat(numn))*dzdbetaz_betan
	1-(dnumbz11-dfloat(numz))*dndbetaz_betan
	dneu_cz=-(dnumbn11-dfloat(numn))*dzdbetan_betaz
	1+(dnumbz11-dfloat(numz))*dndbetan_betaz


	diffn=dnumbn11-dfloat(numn)
	diffz=dnumbz11-dfloat(numz)
      if(dabs(diffn).ge.1.0e-10.and.dabs(diffz).ge.1.0e-10) then
	betamun=betamun1-(dneu_cn/deno_c)
	betamuz=betamuz1-(dneu_cz/deno_c)
 	write(*,*)
c	write(*,567)iter2,betamun1,betamuz1,betamun,betamuz,dnumbn11
c	1,dnumbz11
567   format(i5,6f15.10)
 	goto 300
	end if



555   continue





	vol_av=vol_av_req
c	emassp=emassp_req
c	emassn=emassn_req


	iprint=1
ccc----------------------------------------------------
      sum_frag=0.0d0
      sum_prot=0.0d0
	sum_neut=0.0d0
	vol_av_ratio=vol_av/vol_normal
c      write(*,31)vol_av_ratio
c31    format(1h ,'Avalable Volume to Normal Volume ratio',f7.3)
      univ_v=univ3by2*vol_av
      y=univ_v*(emassp**1.5d0)*dexp(betamuz)
      do iz=0,numzmax
	do in=ndriplow(iz),ndriphigh(iz)
	if(in.gt.numnmax3)goto 1299
      ia=iz+in
	diz=dfloat(iz)
	din=dfloat(in)
	dia=diz+din
	if(iz.eq.0.and.in.eq.1) then
	rhofn=univ3by2*(emassn**1.5d0)*exp(betamun)
	dlhsn=rhofn/(2.0d0*univ3by2*(emassn**1.5d0))
      call Etainv(dlhsn,etan)
	yy(0,1)=rhofn*vol
        sum_frag=sum_frag+yy(0,1)
        sum_prot=sum_prot+(diz*yy(0,1))
	  sum_neut=sum_neut+(din*yy(0,1))
	else if(iz.eq.1.and.in.eq.0) then
	rhofp=univ3by2*(emassp**1.5d0)*exp(betamuz)
	dlhsp=rhofp/(2.0d0*univ3by2*(emassp**1.5d0))
      call Etainv(dlhsp,etap)
	yy(1,0)=rhofp*vol
        sum_frag=sum_frag+yy(1,0)
        sum_prot=sum_prot+(diz*yy(1,0))
	  sum_neut=sum_neut+(din*yy(1,0))
	call dmeanfield(rhofp,rhofn,etap,etan,temp,dkin_p,dkin_n,potfp
	1,potfn,poten_dens,emassp,emassn)
	else
c	  if(iz.eq.in) then
	  if(iz.eq.1.and.in.eq.1) then
	  fee_eff=fee(iz,in)
	  else if(iz.eq.1.and.in.eq.2) then	   
	  fee_eff=fee(iz,in)
	  else if(iz.eq.1.and.in.eq.3) then	   
	  fee_eff=fee(iz,in)
	  else if(iz.eq.1.and.in.eq.4) then	   
	  fee_eff=fee(iz,in)
	  else if(iz.eq.1.and.in.eq.5) then	   
	  fee_eff=fee(iz,in)
	  else if(iz.eq.1.and.in.eq.6) then	   
	  fee_eff=fee(iz,in)
	  else if(iz.eq.2.and.in.eq.1) then	   
	  fee_eff=fee(iz,in)
	  else if(iz.eq.2.and.in.eq.2) then	   
	  fee_eff=fee(iz,in)
	  else if(iz.eq.2.and.in.eq.3) then	   
	  fee_eff=fee(iz,in)
	  else if(iz.eq.2.and.in.eq.4) then	   
	  fee_eff=fee(iz,in)
	  else if(iz.eq.2.and.in.eq.5) then	   
	  fee_eff=fee(iz,in)
	  else if(iz.eq.2.and.in.eq.6) then	   
	  fee_eff=fee(iz,in)
	  else if(iz.eq.2.and.in.eq.7) then	   
	  fee_eff=fee(iz,in)
	  else if(iz.eq.2.and.in.eq.8) then	   
	  fee_eff=fee(iz,in)
	  else	   
  	  delta=(dia-(2.0d0*diz))/dia
        rho=rho_0*(1.0d0-(3.0d0*para_Lsym*delta*delta)/
	1(para_Ksat+(para_Ksym*delta*delta)))
	  rhopc=(rho*diz)/dia
	  rhonc=rho-rhopc
	  fee_gas_cor1=-((2.0d0/3.0d0)*(dkin_n+dkin_p))/temp
	  fee_gas_cor2=poten_dens/temp
	  fee_gas_cor3n=rhofn*etan
	  fee_gas_cor3p=rhofp*etap
	  fee_gas_cor3=fee_gas_cor3p+fee_gas_cor3n
	  fee_gas_cor=((fee_gas_cor1+fee_gas_cor2+fee_gas_cor3)*dia)/rho	
	  fee_eff=fee(iz,in)+fee_gas_cor
	  if(diz.eq.28.0.and.dia.eq.56.0)then
	factor_extra=temp*((rhofp*etap)+(rhofn*etan))*(dia/rho)
	  write(23,223)temp,dens_ratio,proton_frac,fee_gas_cor
	1,fee_gas_cor1,fee_gas_cor2,fee_gas_cor3
	  end if
223     format(7f9.3)
c	  fee_eff=fee(iz,in)+fee_gas_cor
 	  end if
c	  if(temp.eq.16.0d0) then
c	  write(21,221)iz,in,fee(iz,in),fee_gas_cor,dkin_p,dkin_n
c221     format(2i5,4f9.4)
c	  end if 
        yy(iz,in)=univ3by2*(dmass**1.5d0)*vol_av*((diz+din)**1.5d0)*dexp
	1((diz*(etap+(potfp/temp)))+(din*(etan+(potfn/temp)))+fee_eff)
        sum_frag=sum_frag+yy(iz,in)
        sum_prot=sum_prot+(diz*yy(iz,in))
	  sum_neut=sum_neut+(din*yy(iz,in))
c	  end if
      end if
1299   continue
      end do
	end do




	sum1 = 0.0; sum2 = 0.0	
	sumH = 0.0; sumHe = 0.0 
	sumn = 0.0; sump = 0.0

	do in = ndriplow(1), ndriphigh(1)
	a = dfloat(1 + in) 
	sumH = sumH + a*yy(1, in)/vol 
	end do 

	do in = ndriplow(2), ndriphigh(2) 
	a = dfloat(2 + in) 
	sumHe = sumHe + a*yy(2, in)/vol 
	end do 

	do iz = 3, numzmax
	do in = ndriplow(iz), ndriphigh(iz) 
	a = dfloat(iz + in) 
	sum1 = sum1 + a*yy(iz, in)/vol 
	end do 
	end do 

	do iz = 0, numzmax 
	do in = ndriplow(iz), ndriphigh(iz) 
	if (in.gt.numnmax3) goto 456 
	a = dfloat(iz + in) 
	sum2 = sum2 + a*yy(iz, in)/vol
456   continue
      end do	
	end do

	xp_heavy = yy(1, 0)/(sum2*vol) 
	xn_heavy = yy(0, 1)/(sum2*vol) 
	xlight_heavy = (sumH + sumHe)/sum2 
	x_heavy = sum1/sum2 

	write(15, 5678) Proton_frac, xp_heavy, xn_heavy, xlight_heavy, 
	1x_heavy
5678	format(1f8.2, 4e12.4)


	do iz = 0, numzmax
	 do in = ndriplow(iz), ndriphigh(iz)
	ia = iz + in
	yyrho(iz, in) = yy(iz, in)/vol
C	write(12, 112) dens_ratio, proton_frac, temp, iz, ia, yy(iz, in)
	end do
	end do


	 ! To Calculate bound oservables 
	   
	sum0 = 0.0d0 
	sum1 = 0.0d0; sum2 = 0.0d0; sum3 = 0.0d0 

	do iz = 1, numzmax
	do in = ndriplow(iz), ndriphigh(iz) 
	if (in.ge.1) then 
	sum0 = sum0 + yyrho(iz, in) 
	end if
	end do 
	end do 

	do iz = 1, numzmax
	do in = ndriplow(iz), ndriphigh(iz) 
	if (in.ge.1) then
	ia = iz + in  
	sum1 = sum1 + dfloat(iz)*yyrho(iz, in)
	sum2 = sum2 + dfloat(ia)*yyrho(iz, in) 
	sum3 = sum3 + (dfloat(ia - 2.0d0*iz)/dfloat(ia))*yyrho(iz, in)
	end if   
	end do 
	end do

	z_bound = sum1/sum0 
	a_bound = sum2/sum0
	aI_bound = sum3/sum0 

	write(17, 123) Proton_frac, z_bound, a_bound, aI_bound 
123	format(1F8.2, 3f12.6)








	sum_fragz=0.0d0
      do iz=0,numzmax
	dmulz=0.0d0
	do in=ndriplow(iz),ndriphigh(iz)
	ia=iz+in
	sum_fragz=sum_fragz+dmulz
	end do
	end do

	sum_fraga=0.0d0
	do ia=1,numamax
	dmula=0.0d0
	do iz=0,numzmax
	do in=ndriplow(iz),ndriphigh(iz)
	iia=iz+in
	if(ia.eq.iia) then
	dmula=dmula+yy(iz,in)
	end if
	end do
	 end do
 	sum_fraga=sum_fraga+dmula
	 end do
      write(*,*)

       do iz=0,numzmax
	  do in=ndriplow(iz),ndriphigh(iz)
	ia=iz+in
	yyrho(iz,in)=yy(iz,in)/vol
	write(12,112)dens_ratio,proton_frac,temp,iz,ia,yyrho(iz,in)
	end do
	end do
112   format(3f8.3,2i5,e12.5)

	rhosum=0.0d0
	aboundrhosum=0.0d0
      do iz=0,numzmax
	do in=ndriplow(iz),ndriphigh(iz)
	dia=dfloat(iz)+dfloat(in)
	rhosum=rhosum+yyrho(iz,in)
	if(iz.eq.1.and.in.eq.0) then
	aboundrhosum=aboundrhosum 
	else if(iz.eq.0.and.in.eq.1) then
	aboundrhosum=aboundrhosum
	else
	aboundrhosum=aboundrhosum+(dia*yyrho(iz,in))
	end if
	end do
	end do
	abound=aboundrhosum/rhosum
	dmass_frac_Alpha4=(4.0d0*yy(2,2))/2000.0d0
	dmass_frac_Alpha8=(8.0d0*yy(2,6))/2000.0d0
	dmass_frac_C12=(12.0d0*yy(6,6))/2000.0d0
	dmass_frac_C15=(15.0d0*yy(6,9))/2000.0d0
	dmass_frac_C18=(18.0d0*yy(6,12))/2000.0d0
	dmass_frac_O16=(16.0d0*yy(8,8))/2000.0d0
	dmass_frac_O20=(20.0d0*yy(8,12))/2000.0d0
	dmass_frac_O24=(24.0d0*yy(8,16))/2000.0d0

	rhopsum=0.0d0
	zboundrhosum=0.0d0
      do iz=0,numzmax
	do in=ndriplow(iz),ndriphigh(iz)
	diz=dfloat(iz)
	rhopsum=rhopsum+yyrho(iz,in)
	if(iz.eq.1.and.in.eq.0) then
	zboundrhosum=zboundrhosum 
	else if(iz.eq.0.and.in.eq.1) then
	zboundrhosum=zboundrhosum
	else
	zboundrhosum=zboundrhosum+(diz*yyrho(iz,in))
	end if
	end do
	end do
	zbound=zboundrhosum/rhopsum

	zheavy_neu=0.0d0
	zheavy_deno=0.0d0
	xheavy_neu=0.0d0
	xheavy_deno=0.0d0
      do iz=0,numzmax
	do in=ndriplow(iz),ndriphigh(iz)
	  if(iz.eq.0) then
        diz=dfloat(iz)
	  xheavy_deno=xheavy_deno+(diz*yyrho(iz,in))
	  else if(iz.eq.1) then
	  diz=dfloat(iz)
	  xheavy_deno=xheavy_deno+(diz*yyrho(iz,in))
	  else if(iz.eq.2) then	   
	  diz=dfloat(iz)
	  xheavy_deno=xheavy_deno+(diz*yyrho(iz,in))
	  else
	  diz=dfloat(iz)
	  zheavy_neu=zheavy_neu+(diz*yyrho(iz,in))
	  zheavy_deno=zheavy_deno+yyrho(iz,in)	
	  xheavy_neu=xheavy_neu+(diz*yyrho(iz,in))
	  xheavy_deno=xheavy_deno+(diz*yyrho(iz,in))
	  end if
	end do
	end do
	if(zheavy_deno.gt.0.0d0) then
	zheavy=zheavy_neu/zheavy_deno
	else
	zheavy=0.0d0
	end if

	if(xheavy_deno.gt.0.0d0) then
	xheavy=xheavy_neu/xheavy_deno
	else
	xheavy=0.0d0
	end if


c	write(*,*)sum_frag,sum_fragz,sum_fraga
c	write(*,*)sum_prot,sum_neut

	betamuz_print=etap+(potfp/temp)
	betamun_print=etan+(potfn/temp)
	write(*,122)densbyrho0,proton_frac,temp,iter,iter2
	1,sum_prot,sum_neut
	write(11,121)densbyrho0,proton_frac,temp,betamuz_print
	1,betamun_print,rhofp,rhofn,abound,potfp,potfn,vol_av_ratio
	write(25,125)densbyrho0,proton_frac,temp,dmass_frac_Alpha4
	1,dmass_frac_Alpha8,dmass_frac_C12,dmass_frac_C15,dmass_frac_C18
     2,dmass_frac_O16,dmass_frac_O20,dmass_frac_O24
	write(26,121)densbyrho0,proton_frac,temp,abound,zbound,xheavy
	1,zheavy
c	write(12,121)temp,betamuz_print,etap,potfpbytemp
c	1,betamun_print,etan,potfnbytemp

	write(16, 6789) proton_frac, betamuz_print*temp,
	1betamun_print*temp
6789	format(1f8.2, 2f15.5)	



122   format(3f9.4,2i7,2f11.5)
121   format(11f11.6)
125   format(3f11.6,8e12.5)
	iprint=0
	end do
	end if
	end do
	end do
999   continue
      stop
      end


      subroutine binding(da,dz,temp,rho_electron,energy_tot,etap,etan)
cccc  Subroutine for calculating Wigner-Seitz correction for A>4
cccc	Date-03.07.2019
      implicit real *8 (a-h,o-z)
	pi=22.0d0/7.0d0
	coul_const=0.864d0	
	num_charge=nint(dz)
	num_mass=nint(da)
	const_p=3.0d0
	dmass=938.0d0
	const_bs=15.36563
	const_sigma0=1.09191


	rho_0=0.1604d0
	para_Kinetic=22.1d0
	para_kinetic_K0=0.4286d0
	para_kinetic_Ksym=0.1786d0
	para_Esat=-15.98d0
	para_Esym=32.03d0
	para_Lsym=48.30d0
	para_Ksat=230.0d0
	para_Ksym=-112.0d0
	para_Qsat=-364.0d0
	para_Qsym=501.0d0
	para_Zsat=1592.0d0
	para_Zsym=-3087.0d0
	b=6.93d0

	delta=(da-(2.0d0*dz))/da
	prot_frac=dz/da

      rho=rho_0*(1.0d0-(3.0d0*para_Lsym*delta*delta)/
	1(para_Ksat+(para_Ksym*delta*delta)))

	rhop=(rho*dz)/da
	rhon=rho-rhop
	x=(rho-rho_0)/(3.0d0*rho_0)


	v0_is=para_Esat-para_Kinetic*(1.0d0+para_kinetic_K0)
	v0_iv=para_Esym-(5.0d0/9.0d0)*para_Kinetic*(1.0d0+para_kinetic_K0+
	13.0d0*para_kinetic_Ksym)

	v1_is=-para_Kinetic*(2.0d0+5.0d0*para_kinetic_K0)
	v1_iv=para_Lsym-(5.0d0/9.0d0)*para_Kinetic*(2.0d0+5.0d0*
	1para_kinetic_K0+15.0d0*para_kinetic_Ksym)

	v2_is=para_Ksat-2.0d0*para_Kinetic*(-1.0d0+5.0d0*para_kinetic_K0)
	v2_iv=para_Ksym-(10.0d0/9.0d0)*para_Kinetic*(-1.0d0+5.0d0*
	1para_kinetic_K0+15.0d0*para_kinetic_Ksym)

	v3_is=para_Qsat-2.0d0*para_Kinetic*(4.0d0-5.0d0*para_kinetic_K0)
	v3_iv=para_Qsym-(10.0d0/9.0d0)*para_Kinetic*(4.0d0-5.0d0*
	1para_kinetic_K0-15.0d0*para_kinetic_Ksym)

	v4_is=para_Zsat-8.0d0*para_Kinetic*(-7.0d0+5.0d0*para_kinetic_K0)
	v4_iv=para_Zsym-(40.0d0/9.0d0)*para_Kinetic*(-7.0d0+5.0d0*
	1para_kinetic_K0+15.0d0*para_kinetic_Ksym)

	a4_is=(243.0d0*v0_is)-(81.0d0*v1_is)+((27.0d0*v2_is)/2.0d0)
	1-((3.0d0*v3_is)/2.0d0)+((1.0d0*v4_is)/8.0d0)
	a4_iv=(243.0d0*v0_iv)-(81.0d0*v1_iv)+((27.0d0*v2_iv)/2.0d0)
	1-((3.0d0*v3_iv)/2.0d0)+((1.0d0*v4_iv)/8.0d0)

	if(temp.eq.0.0d0) then
	f1_delta=((1.0d0+delta)**(5.0d0/3.0d0))
	1+((1.0d0-delta)**(5.0d0/3.0d0))
	f2_delta=delta*(((1.0d0+delta)**(5.0d0/3.0d0))
	1-((1.0d0-delta)**(5.0d0/3.0d0)))
	dkinetic1=(1.0d0+(para_kinetic_K0*rho/rho_0))*f1_delta
	dkinetic2=(para_kinetic_Ksym*rho/rho_0)*f2_delta
	dkinetic=0.5d0*para_Kinetic*((rho/rho_0)**(2.0d0/3.0d0))*
	1(dkinetic1+dkinetic2)
	else
	eff_factor_p1=(para_kinetic_K0-(para_kinetic_Ksym*delta))
	eff_factor_p=1.0d0+(eff_factor_p1*(rho/rho_0))
	emassp=dmass/eff_factor_p
	eff_factor_n1=(para_kinetic_K0+(para_kinetic_Ksym*delta))
	eff_factor_n=1.0d0+(eff_factor_n1*(rho/rho_0))
	emassn=dmass/eff_factor_n
      univ=(2.0d0*pi*temp)/(1240.0d0*1240.0d0)
	univ3by2=univ**1.5d0
	univ5by2=univ**2.5d0
	dlhsn=rhon/(2.0d0*univ3by2*(emassn**1.5d0))
      call Etainv(dlhsn,etan)
	dlhsp=rhop/(2.0d0*univ3by2*(emassp**1.5d0))
      call Etainv(dlhsp,etap)
	order3by2=1.5d0
	call Fermi(order3by2,etan,feta_kineticn)
	call Fermi(order3by2,etap,feta_kineticp)

	const_kinetic=12.0d0*pi*univ5by2
	tau_n=const_kinetic*feta_kineticn*(emassn**2.5d0)
	ekin_n=(1240.0d0*1240.0d0*tau_n)/(8.0d0*pi*pi*emassn)
	tau_p=const_kinetic*feta_kineticp*(emassp**2.5d0)
	ekin_p=(1240.0d0*1240.0d0*tau_p)/(8.0d0*pi*pi*emassp)
	dkinetic=(ekin_n+ekin_p)/rho
	end if

	if(dz.eq.28.0.and.da.eq.56.0.and.temp.gt.0.0) then
	f1_delta_0=((1.0d0+delta)**(5.0d0/3.0d0))
	1+((1.0d0-delta)**(5.0d0/3.0d0))
	f2_delta_0=delta*(((1.0d0+delta)**(5.0d0/3.0d0))
	1-((1.0d0-delta)**(5.0d0/3.0d0)))
	dkinetic1_0=(1.0d0+(para_kinetic_K0*rho/rho_0))*f1_delta_0
	dkinetic2_0=(para_kinetic_Ksym*rho/rho_0)*f2_delta_0
	dkinetic_0=0.5d0*para_Kinetic*((rho/rho_0)**(2.0d0/3.0d0))*
	1(dkinetic1_0+dkinetic2_0)
	d_ex=(dkinetic-dkinetic_0)*da
	write(24,224)temp,d_ex
	end if
224   format(2f9.3) 
	vpot0=v0_is+v0_iv*(delta**2.0d0)
	vpot1=(v1_is+v1_iv*(delta**2.0d0))*x
	vpot2=(1.0d0/2.0d0)*(v2_is+v2_iv*(delta**2.0d0))*x*x
	vpot3=(1.0d0/6.0d0)*(v3_is+v3_iv*(delta**2.0d0))*x*x*x
	vpot4=(1.0d0/24.0d0)*(v4_is+v4_iv*(delta**2.0d0))*x*x*x*x

	a4_is=(243.0d0*v0_is)-(81.0d0*v1_is)+((27.0d0*v2_is)/2.0d0)
	1-((3.0d0*v3_is)/2.0d0)+((1.0d0*v4_is)/8.0d0)
	a4_iv=(243.0d0*v0_iv)-(81.0d0*v1_iv)+((27.0d0*v2_iv)/2.0d0)
	1-((3.0d0*v3_iv)/2.0d0)+((1.0d0*v4_iv)/8.0d0)

	vpot_added=(a4_is+a4_iv*(delta**2.0d0))*(x**5.0d0)
	1*(dexp(-b*(1.0d0+(3.0d0*x))))
	vpot=vpot0+vpot1+vpot2+vpot3+vpot4+vpot_added
	if(temp.eq.0.0d0) then
	energy_bulk=(dkinetic+vpot)*da
	else
	energy_bulk_dens=-(2.0d0/3.0d0)*(ekin_n+ekin_p)+(vpot*rho)
	1+temp*((rhop*etap)+(rhon*etan))
	energy_bulk=(energy_bulk_dens*da)/rho
	end if

	sigma_neu=((2.0d0**(const_p+1.0d0))+const_bs)
	sigma_deno=(prot_frac**(-const_p))+const_bs
	1+((1.0d0-prot_frac)**(-const_p))
	sigma=const_sigma0*(sigma_neu/sigma_deno)
	radius0=(3.0d0/(4.0d0*pi*rho))**(1.0d0/3.0d0)
	temp_c=87.76d0*prot_frac*(1.0d0-prot_frac)
	1*((0.155/rho_0)**(1.0d0/3.0d0))
	energy_surface=4.0d0*pi*radius0*radius0*(da**
	1(2.0d0/3.0d0))*sigma*((1.0d0-((temp/temp_c)**2.0d0))**2.0d0)

	seitz_factor1=1.5d0*(((2.0d0*rho_electron)/((1.0d0-delta)*rho))
	1**(1.0d0/3.0d0))
	seitz_factor2=0.5d0*((2.0d0*rho_electron)/((1.0d0-delta)*rho))
	seitz=1.0d0-(seitz_factor1-seitz_factor2)
c	seitz=1.0d0
	energy_coulomb=((coul_const*seitz)/radius0)*((dz*dz)
	1/(da**(1.0d0/3.0d0)))

	energy_tot=energy_bulk+energy_surface+energy_coulomb
99    continue
      return
      end



	subroutine seitz_lightnuclei(da,dz,rho_electron,seitz_correction)
cccc  Subroutine for calculating Wigner-Seitz correction for H2,H3,He3,He4
cccc  Date-10.07.2019   
      implicit real *8 (a-h,o-z)

	coul_const=0.864d0	
	rho_0=0.1604d0
	para_Lsym=48.30d0
	para_Ksat=230.0d0
	para_Ksym=-112.0d0

	delta=(da-2.0d0*dz)/da
      rho=rho_0*(1.0d0-(3.0d0*para_Lsym*delta*delta)/
	1(para_Ksat+(para_Ksym*delta*delta)))

	seitz_factor1=1.5d0*(((2.0d0*rho_electron)/((1.0d0-delta)*rho))
	1**(1.0d0/3.0d0))
	seitz_factor2=0.5d0*((2.0d0*rho_electron)/((1.0d0-delta)*rho))
	seitz_correction=(coul_const/1.2d0)*(seitz_factor1-seitz_factor2)
	1*(dz/(da**(1.0d0/3.0d0)))
c	seitz_correction=0.0d0
	return
	end





	subroutine dmeanfield(rhop,rhon,etap,etan,temp,dkin_p,dkin_n,pot_p
	1,pot_n,poten_dens,emassp,emassn)
cccc  Subroutine for calculating mean field due to free proton and neutron gas
cccc	Date-17.06.2019
      implicit real *8 (a-h,o-z)
	dmass=938.0d0
      pi=3.14159265
      univ=(2.0d0*pi*temp)/(1240.0d0*1240.0d0)
	univ3by2=univ**1.5d0
	univ5by2=univ**2.5d0

	rho=rhon+rhop
	rho_0=0.1604d0
	x=(rho-rho_0)/(3.0d0*rho_0)
	delta=(rhon-rhop)/(rhon+rhop)
	b=6.93d0
	


	para_Kinetic=22.1d0
	para_kinetic_K0=0.4286d0
	para_kinetic_Ksym=0.1786d0
	para_Esat=-15.98d0
	para_Esym=32.03d0
	para_Lsym=48.30d0
	para_Ksat=230.0d0
	para_Ksym=-112.0d0
	para_Qsat=-364.0d0
	para_Qsym=501.0d0
	para_Zsat=1592.0d0
	para_Zsym=-3087.0d0

	v0_is=para_Esat-para_Kinetic*(1.0d0+para_kinetic_K0)
	v0_iv=para_Esym-(5.0d0/9.0d0)*para_Kinetic*(1.0d0+para_kinetic_K0+
	13.0d0*para_kinetic_Ksym)

	v1_is=-para_Kinetic*(2.0d0+5.0d0*para_kinetic_K0)
	v1_iv=para_Lsym-(5.0d0/9.0d0)*para_Kinetic*(2.0d0+5.0d0*
	1para_kinetic_K0+15.0d0*para_kinetic_Ksym)

	v2_is=para_Ksat-2.0d0*para_Kinetic*(-1.0d0+5.0d0*para_kinetic_K0)
	v2_iv=para_Ksym-(10.0d0/9.0d0)*para_Kinetic*(-1.0d0+5.0d0*
	1para_kinetic_K0+15.0d0*para_kinetic_Ksym)

	v3_is=para_Qsat-2.0d0*para_Kinetic*(4.0d0-5.0d0*para_kinetic_K0)
	v3_iv=para_Qsym-(10.0d0/9.0d0)*para_Kinetic*(4.0d0-5.0d0*
	1para_kinetic_K0-15.0d0*para_kinetic_Ksym)

	v4_is=para_Zsat-8.0d0*para_Kinetic*(-7.0d0+5.0d0*para_kinetic_K0)
	v4_iv=para_Zsym-(40.0d0/9.0d0)*para_Kinetic*(-7.0d0+5.0d0*
	1para_kinetic_K0+15.0d0*para_kinetic_Ksym)


	a4_is=(243.0d0*v0_is)-(81.0d0*v1_is)+((27.0d0*v2_is)/2.0d0)
	1-((3.0d0*v3_is)/2.0d0)+((1.0d0*v4_is)/8.0d0)
	a4_iv=(243.0d0*v0_iv)-(81.0d0*v1_iv)+((27.0d0*v2_iv)/2.0d0)
	1-((3.0d0*v3_iv)/2.0d0)+((1.0d0*v4_iv)/8.0d0)

cccc  Calculating the Nuclear Potential

	pot1=v0_is+v0_iv*(delta**2.0d0)

	pot21=2.0d0*(v1_is+v1_iv*(delta**2.0d0))*x
	pot22=(3.0d0/2.0d0)*(v2_is+v2_iv*(delta**2.0d0))*x*x
	pot23=(2.0d0/3.0d0)*(v3_is+v3_iv*(delta**2.0d0))*x*x*x
	pot24=(5.0d0/24.0d0)*(v4_is+v4_iv*(delta**2.0d0))*x*x*x*x
	pot2=pot21+pot22+pot23+pot24

	pot31=(v1_is+v1_iv*(delta**2.0d0))
	pot32=(v2_is+v2_iv*(delta**2.0d0))*x
	pot33=(1.0d0/2.0d0)*(v3_is+v3_iv*(delta**2.0d0))*x*x
	pot34=(1.0d0/6.0d0)*(v4_is+v4_iv*(delta**2.0d0))*x*x*x
	pot3=(1.0d0/3.0d0)*(pot31+pot32+pot33+pot34)

	pot40=v0_iv
	pot41=v1_iv*x
	pot42=(1.0d0/2.0d0)*v2_iv*x*x
	pot43=(1.0d0/6.0d0)*v3_iv*x*x*x
	pot44=(1.0d0/24.0d0)*v4_iv*x*x*x*x
	pot4n=2.0d0*delta*(1.0d0-delta)*(pot40+pot41+pot42+pot43+pot44)
	pot4p=-2.0d0*delta*(1.0d0+delta)*(pot40+pot41+pot42+pot43+pot44)

      pot511=(5.0d0/3.0d0)*(x**4.0d0)
	pot512=(6.0d0-b)*(x**5.0d0)
	pot513=-(3.0d0*b)*(x**6.0d0)
	pot51=(a4_is+a4_iv*(delta**2.0d0))*(pot511+pot512+pot513)
	pot52n=2.0d0*delta*(1.0d0-delta)*a4_iv*(x**5.0d0)
	pot52p=-2.0d0*delta*(1.0d0+delta)*a4_iv*(x**5.0d0)
	pot53=dexp(-b*(1.0d0+(3.0d0*x)))
	pot5n=(pot51+pot52n)*pot53
	pot5p=(pot51+pot52p)*pot53


	eff_factor_p1=(para_kinetic_K0-(para_kinetic_Ksym*delta))
	eff_factor_p=1.0d0+(eff_factor_p1*(rho/rho_0))
	emassp=dmass/eff_factor_p

	eff_factor_n1=(para_kinetic_K0+(para_kinetic_Ksym*delta))
	eff_factor_n=1.0d0+(eff_factor_n1*(rho/rho_0))
	emassn=dmass/eff_factor_n


	order3by2=1.5d0
	call Fermi(order3by2,etan,feta_kineticn)
	call Fermi(order3by2,etap,feta_kineticp)
	const_kinetic=12.0d0*pi*univ5by2
	tau_n=const_kinetic*feta_kineticn*(emassn**2.5d0)
	tau_p=const_kinetic*feta_kineticp*(emassp**2.5d0)
	dkin_p=(1240.0d0*1240.0d0*tau_p)/(8.0d0*pi*pi*emassp)
	dkin_n=(1240.0d0*1240.0d0*tau_n)/(8.0d0*pi*pi*emassn)

	potp_eff1=(tau_p*(para_kinetic_K0+para_kinetic_Ksym))/rho_0
	potp_eff2=(tau_n*(para_kinetic_K0-para_kinetic_Ksym))/rho_0
	potp_eff=potp_eff1+potp_eff2

	potn_eff1=(tau_n*(para_kinetic_K0+para_kinetic_Ksym))/rho_0
	potn_eff2=(tau_p*(para_kinetic_K0-para_kinetic_Ksym))/rho_0
	potn_eff=potn_eff1+potn_eff2


	pot_p=pot1+pot2+pot3+pot4p+pot5p+potp_eff
	pot_n=pot1+pot2+pot3+pot4n+pot5n+potn_eff

ccc   Calculating potential energy density
	vpot0=v0_is+v0_iv*(delta**2.0d0)
	vpot1=(v1_is+v1_iv*(delta**2.0d0))*x
	vpot2=(1.0d0/2.0d0)*(v2_is+v2_iv*(delta**2.0d0))*x*x
	vpot3=(1.0d0/6.0d0)*(v3_is+v3_iv*(delta**2.0d0))*x*x*x
	 vpot4=(1.0d0/24.0d0)*(v4_is+v4_iv*(delta**2.0d0))*x*x*x*x

	a4_is=(243.0d0*v0_is)-(81.0d0*v1_is)+((27.0d0*v2_is)/2.0d0)
	1-((3.0d0*v3_is)/2.0d0)+((1.0d0*v4_is)/8.0d0)
	a4_iv=(243.0d0*v0_iv)-(81.0d0*v1_iv)+((27.0d0*v2_iv)/2.0d0)
	1-((3.0d0*v3_iv)/2.0d0)+((1.0d0*v4_iv)/8.0d0)

	vpot_added=(a4_is+a4_iv*(delta**2.0d0))*(x**5.0d0)
	1*(dexp(-b*(1.0d0+(3.0d0*x))))
	vpot=vpot0+vpot1+vpot2+vpot3+vpot4+vpot_added
	poten_dens=rho*vpot


      return
	end



	subroutine Etainv(dlhs,eta)
      implicit real *8 (a-h,o-z)
      order=0.5d0
	eta1=-5.0d0
	ieter=1
10    continue
	call Fermi(order,eta1,fd1)
	eta2=eta1+1.0d0
	call Fermi(order,eta2,fd2)
	factor=(eta2-eta1)/(fd2-fd1)
	eta=(factor*(dlhs-fd1))+eta1
	call Fermi(order,eta,fd)
	dif=dabs(fd-dlhs)
c	write(*,*)
c	write(*,11)ieter,dif
11    format(i5,f9.5)
	if(dif.ge.0.00000001) then
	eta1=eta
	ieter=ieter+1
	goto 10
	end if
c	write(*,*)
c	write(*,*)"fd=",fd
c	write(*,12)ieter,fd,eta
12    format(i5,2f9.5)
	return
	end











      
      subroutine Fermi(ord,x,fd)
      DOUBLE PRECISION RELERR
      PARAMETER (RELERR=1.0D-14)
C     ..
C     .. Local Scalars ..
      DOUBLE PRECISION FD,ORD,X
      INTEGER IERR,IINP,IOUT
C     ..
C     .. External Functions ..
      INTEGER I1MACH
      EXTERNAL I1MACH
C     ..
C     .. External Subroutines ..
      EXTERNAL FERINC,FERMID
C     ..
      IINP = I1MACH(1)
      IOUT = I1MACH(2)
          CALL FERMID(ORD,X,RELERR,FD,IERR)
      CLOSE (UNIT=IINP)
      CLOSE (UNIT=IOUT)
      RETURN

   30 CONTINUE
      WRITE (IOUT,FMT=*) 'Unexpected end of input file'
      END


      DOUBLE PRECISION FUNCTION D1MACH(I)
C
C  DOUBLE-PRECISION MACHINE CONSTANTS
C
C  D1MACH( 1) = B**(EMIN-1), THE SMALLEST POSITIVE MAGNITUDE.
C
C  D1MACH( 2) = B**EMAX*(1 - B**(-T)), THE LARGEST MAGNITUDE.
C
C  D1MACH( 3) = B**(-T), THE SMALLEST RELATIVE SPACING.
C
C  D1MACH( 4) = B**(1-T), THE LARGEST RELATIVE SPACING.
C
C  D1MACH( 5) = LOG10(B)
C
C  TO ALTER THIS FUNCTION FOR A PARTICULAR ENVIRONMENT,
C  THE DESIRED SET OF DATA STATEMENTS SHOULD BE ACTIVATED BY
C  REMOVING THE C FROM COLUMN 1.
C  ON RARE MACHINES A STATIC STATEMENT MAY NEED TO BE ADDED.
C  (BUT PROBABLY MORE SYSTEMS PROHIBIT IT THAN REQUIRE IT.)
C
C  FOR IEEE-ARITHMETIC MACHINES (BINARY STANDARD), ONE OF THE FIRST
C  TWO SETS OF CONSTANTS BELOW SHOULD BE APPROPRIATE.
C
C  WHERE POSSIBLE, OCTAL OR HEXADECIMAL CONSTANTS HAVE BEEN USED
C  TO SPECIFY THE CONSTANTS EXACTLY, WHICH HAS IN SOME CASES
C  REQUIRED THE USE OF EQUIVALENT INTEGER ARRAYS.
C
      INTEGER SMALL(4)
      INTEGER LARGE(4)
      INTEGER RIGHT(4)
      INTEGER DIVER(4)
      INTEGER LOG10(4)
      INTEGER I, I1MACH
C
      DOUBLE PRECISION DMACH(5)
C
      EQUIVALENCE (DMACH(1),SMALL(1))
      EQUIVALENCE (DMACH(2),LARGE(1))
      EQUIVALENCE (DMACH(3),RIGHT(1))
      EQUIVALENCE (DMACH(4),DIVER(1))
      EQUIVALENCE (DMACH(5),LOG10(1))
C
C     MACHINE CONSTANTS FOR IEEE ARITHMETIC MACHINES, SUCH AS THE AT&T
C     3B SERIES AND MOTOROLA 68000 BASED MACHINES (E.G. SUN 3 AND AT&T
C     PC 7300), IN WHICH THE MOST SIGNIFICANT BYTE IS STORED FIRST.
C
       DATA SMALL(1),SMALL(2) /    1048576,          0 /
       DATA LARGE(1),LARGE(2) / 2146435071,         -1 /
       DATA RIGHT(1),RIGHT(2) / 1017118720,          0 /
       DATA DIVER(1),DIVER(2) / 1018167296,          0 /
       DATA LOG10(1),LOG10(2) / 1070810131, 1352628735 /
C
C     MACHINE CONSTANTS FOR IEEE ARITHMETIC MACHINES AND 8087-BASED
C     MICROS, SUCH AS THE IBM PC AND AT&T 6300, IN WHICH THE LEAST
C     SIGNIFICANT BYTE IS STORED FIRST.
C
C      DATA SMALL(1),SMALL(2) /          0,    1048576 /
C      DATA LARGE(1),LARGE(2) /         -1, 2146435071 /
C      DATA RIGHT(1),RIGHT(2) /          0, 1017118720 /
C      DATA DIVER(1),DIVER(2) /          0, 1018167296 /
C      DATA LOG10(1),LOG10(2) / 1352628735, 1070810131 /
C
C     MACHINE CONSTANTS FOR AMDAHL MACHINES.
C
C      DATA SMALL(1),SMALL(2) /    1048576,          0 /
C      DATA LARGE(1),LARGE(2) / 2147483647,         -1 /
C      DATA RIGHT(1),RIGHT(2) /  856686592,          0 /
C      DATA DIVER(1),DIVER(2) /  873463808,          0 /
C      DATA LOG10(1),LOG10(2) / 1091781651, 1352628735 /
C
C     MACHINE CONSTANTS FOR THE BURROUGHS 1700 SYSTEM.
C
C      DATA SMALL(1) / ZC00800000 /
C      DATA SMALL(2) / Z000000000 /
C
C      DATA LARGE(1) / ZDFFFFFFFF /
C      DATA LARGE(2) / ZFFFFFFFFF /
C
C      DATA RIGHT(1) / ZCC5800000 /
C      DATA RIGHT(2) / Z000000000 /
C
C      DATA DIVER(1) / ZCC6800000 /
C      DATA DIVER(2) / Z000000000 /
C
C      DATA LOG10(1) / ZD00E730E7 /
C      DATA LOG10(2) / ZC77800DC0 /
C
C     MACHINE CONSTANTS FOR THE BURROUGHS 5700 SYSTEM.
C
C      DATA SMALL(1) / O1771000000000000 /
C      DATA SMALL(2) / O0000000000000000 /
C
C      DATA LARGE(1) / O0777777777777777 /
C      DATA LARGE(2) / O0007777777777777 /
C
C      DATA RIGHT(1) / O1461000000000000 /
C      DATA RIGHT(2) / O0000000000000000 /
C
C      DATA DIVER(1) / O1451000000000000 /
C      DATA DIVER(2) / O0000000000000000 /
C
C      DATA LOG10(1) / O1157163034761674 /
C      DATA LOG10(2) / O0006677466732724 /
C
C     MACHINE CONSTANTS FOR THE BURROUGHS 6700/7700 SYSTEMS.
C
C      DATA SMALL(1) / O1771000000000000 /
C      DATA SMALL(2) / O7770000000000000 /
C
C      DATA LARGE(1) / O0777777777777777 /
C      DATA LARGE(2) / O7777777777777777 /
C
C      DATA RIGHT(1) / O1461000000000000 /
C      DATA RIGHT(2) / O0000000000000000 /
C
C      DATA DIVER(1) / O1451000000000000 /
C      DATA DIVER(2) / O0000000000000000 /
C
C      DATA LOG10(1) / O1157163034761674 /
C      DATA LOG10(2) / O0006677466732724 /
C
C     MACHINE CONSTANTS FOR THE CDC 6000/7000 SERIES.
C
C      DATA SMALL(1) / 00604000000000000000B /
C      DATA SMALL(2) / 00000000000000000000B /
C
C      DATA LARGE(1) / 37767777777777777777B /
C      DATA LARGE(2) / 37167777777777777777B /
C
C      DATA RIGHT(1) / 15604000000000000000B /
C      DATA RIGHT(2) / 15000000000000000000B /
C
C      DATA DIVER(1) / 15614000000000000000B /
C      DATA DIVER(2) / 15010000000000000000B /
C
C      DATA LOG10(1) / 17164642023241175717B /
C      DATA LOG10(2) / 16367571421742254654B /
C
C     MACHINE CONSTANTS FOR CONVEX C-1
C
C      DATA SMALL(1),SMALL(2) / '00100000'X, '00000000'X /
C      DATA LARGE(1),LARGE(2) / '7FFFFFFF'X, 'FFFFFFFF'X /
C      DATA RIGHT(1),RIGHT(2) / '3CC00000'X, '00000000'X /
C      DATA DIVER(1),DIVER(2) / '3CD00000'X, '00000000'X /
C      DATA LOG10(1),LOG10(2) / '3FF34413'X, '509F79FF'X /
C
C     MACHINE CONSTANTS FOR THE CRAY 1, XMP, 2, AND 3.
C
C      DATA SMALL(1) / 201354000000000000000B /
C      DATA SMALL(2) / 000000000000000000000B /
C
C      DATA LARGE(1) / 577767777777777777777B /
C      DATA LARGE(2) / 000007777777777777776B /
C
C      DATA RIGHT(1) / 376434000000000000000B /
C      DATA RIGHT(2) / 000000000000000000000B /
C
C      DATA DIVER(1) / 376444000000000000000B /
C      DATA DIVER(2) / 000000000000000000000B /
C
C      DATA LOG10(1) / 377774642023241175717B /
C      DATA LOG10(2) / 000007571421742254654B /
C
C     MACHINE CONSTANTS FOR THE DATA GENERAL ECLIPSE S/200
C
C     NOTE - IT MAY BE APPROPRIATE TO INCLUDE THE FOLLOWING LINE -
C     STATIC DMACH(5)
C
C      DATA SMALL/20K,3*0/,LARGE/77777K,3*177777K/
C      DATA RIGHT/31420K,3*0/,DIVER/32020K,3*0/
C      DATA LOG10/40423K,42023K,50237K,74776K/
C
C     MACHINE CONSTANTS FOR THE HARRIS SLASH 6 AND SLASH 7
C
C      DATA SMALL(1),SMALL(2) / '20000000, '00000201 /
C      DATA LARGE(1),LARGE(2) / '37777777, '37777577 /
C      DATA RIGHT(1),RIGHT(2) / '20000000, '00000333 /
C      DATA DIVER(1),DIVER(2) / '20000000, '00000334 /
C      DATA LOG10(1),LOG10(2) / '23210115, '10237777 /
C
C     MACHINE CONSTANTS FOR THE HONEYWELL DPS 8/70 SERIES.
C
C      DATA SMALL(1),SMALL(2) / O402400000000, O000000000000 /
C      DATA LARGE(1),LARGE(2) / O376777777777, O777777777777 /
C      DATA RIGHT(1),RIGHT(2) / O604400000000, O000000000000 /
C      DATA DIVER(1),DIVER(2) / O606400000000, O000000000000 /
C      DATA LOG10(1),LOG10(2) / O776464202324, O117571775714 /
C
C     MACHINE CONSTANTS FOR THE IBM 360/370 SERIES,
C     THE XEROX SIGMA 5/7/9 AND THE SEL SYSTEMS 85/86.
C
C      DATA SMALL(1),SMALL(2) / Z00100000, Z00000000 /
C      DATA LARGE(1),LARGE(2) / Z7FFFFFFF, ZFFFFFFFF /
C      DATA RIGHT(1),RIGHT(2) / Z33100000, Z00000000 /
C      DATA DIVER(1),DIVER(2) / Z34100000, Z00000000 /
C      DATA LOG10(1),LOG10(2) / Z41134413, Z509F79FF /
C
C     MACHINE CONSTANTS FOR THE INTERDATA 8/32
C     WITH THE UNIX SYSTEM FORTRAN 77 COMPILER.
C
C     FOR THE INTERDATA FORTRAN VII COMPILER REPLACE
C     THE Z'S SPECIFYING HEX CONSTANTS WITH Y'S.
C
C      DATA SMALL(1),SMALL(2) / Z'00100000', Z'00000000' /
C      DATA LARGE(1),LARGE(2) / Z'7EFFFFFF', Z'FFFFFFFF' /
C      DATA RIGHT(1),RIGHT(2) / Z'33100000', Z'00000000' /
C      DATA DIVER(1),DIVER(2) / Z'34100000', Z'00000000' /
C      DATA LOG10(1),LOG10(2) / Z'41134413', Z'509F79FF' /
C
C     MACHINE CONSTANTS FOR THE PDP-10 (KA PROCESSOR).
C
C      DATA SMALL(1),SMALL(2) / "033400000000, "000000000000 /
C      DATA LARGE(1),LARGE(2) / "377777777777, "344777777777 /
C      DATA RIGHT(1),RIGHT(2) / "113400000000, "000000000000 /
C      DATA DIVER(1),DIVER(2) / "114400000000, "000000000000 /
C      DATA LOG10(1),LOG10(2) / "177464202324, "144117571776 /
C
C     MACHINE CONSTANTS FOR THE PDP-10 (KI PROCESSOR).
C
C      DATA SMALL(1),SMALL(2) / "000400000000, "000000000000 /
C      DATA LARGE(1),LARGE(2) / "377777777777, "377777777777 /
C      DATA RIGHT(1),RIGHT(2) / "103400000000, "000000000000 /
C      DATA DIVER(1),DIVER(2) / "104400000000, "000000000000 /
C      DATA LOG10(1),LOG10(2) / "177464202324, "047674776746 /
C
C     MACHINE CONSTANTS FOR PDP-11 FORTRANS SUPPORTING
C     32-BIT INTEGERS (EXPRESSED IN INTEGER AND OCTAL).
C
C      DATA SMALL(1),SMALL(2) /    8388608,           0 /
C      DATA LARGE(1),LARGE(2) / 2147483647,          -1 /
C      DATA RIGHT(1),RIGHT(2) /  612368384,           0 /
C      DATA DIVER(1),DIVER(2) /  620756992,           0 /
C      DATA LOG10(1),LOG10(2) / 1067065498, -2063872008 /
C
C      DATA SMALL(1),SMALL(2) / O00040000000, O00000000000 /
C      DATA LARGE(1),LARGE(2) / O17777777777, O37777777777 /
C      DATA RIGHT(1),RIGHT(2) / O04440000000, O00000000000 /
C      DATA DIVER(1),DIVER(2) / O04500000000, O00000000000 /
C      DATA LOG10(1),LOG10(2) / O07746420232, O20476747770 /
C
C     MACHINE CONSTANTS FOR PDP-11 FORTRANS SUPPORTING
C     16-BIT INTEGERS (EXPRESSED IN INTEGER AND OCTAL).
C
C      DATA SMALL(1),SMALL(2) /    128,      0 /
C      DATA SMALL(3),SMALL(4) /      0,      0 /
C
C      DATA LARGE(1),LARGE(2) /  32767,     -1 /
C      DATA LARGE(3),LARGE(4) /     -1,     -1 /
C
C      DATA RIGHT(1),RIGHT(2) /   9344,      0 /
C      DATA RIGHT(3),RIGHT(4) /      0,      0 /
C
C      DATA DIVER(1),DIVER(2) /   9472,      0 /
C      DATA DIVER(3),DIVER(4) /      0,      0 /
C
C      DATA LOG10(1),LOG10(2) /  16282,   8346 /
C      DATA LOG10(3),LOG10(4) / -31493, -12296 /
C
C      DATA SMALL(1),SMALL(2) / O000200, O000000 /
C      DATA SMALL(3),SMALL(4) / O000000, O000000 /
C
C      DATA LARGE(1),LARGE(2) / O077777, O177777 /
C      DATA LARGE(3),LARGE(4) / O177777, O177777 /
C
C      DATA RIGHT(1),RIGHT(2) / O022200, O000000 /
C      DATA RIGHT(3),RIGHT(4) / O000000, O000000 /
C
C      DATA DIVER(1),DIVER(2) / O022400, O000000 /
C      DATA DIVER(3),DIVER(4) / O000000, O000000 /
C
C      DATA LOG10(1),LOG10(2) / O037632, O020232 /
C      DATA LOG10(3),LOG10(4) / O102373, O147770 /
C
C     MACHINE CONSTANTS FOR THE PRIME 50 SERIES SYSTEMS
C     WTIH 32-BIT INTEGERS AND 64V MODE INSTRUCTIONS,
C     SUPPLIED BY IGOR BRAY.
C
C      DATA SMALL(1),SMALL(2) / :10000000000, :00000100001 /
C      DATA LARGE(1),LARGE(2) / :17777777777, :37777677775 /
C      DATA RIGHT(1),RIGHT(2) / :10000000000, :00000000122 /
C      DATA DIVER(1),DIVER(2) / :10000000000, :00000000123 /
C      DATA LOG10(1),LOG10(2) / :11504046501, :07674600177 /
C
C     MACHINE CONSTANTS FOR THE SEQUENT BALANCE 8000
C
C      DATA SMALL(1),SMALL(2) / $00000000,  $00100000 /
C      DATA LARGE(1),LARGE(2) / $FFFFFFFF,  $7FEFFFFF /
C      DATA RIGHT(1),RIGHT(2) / $00000000,  $3CA00000 /
C      DATA DIVER(1),DIVER(2) / $00000000,  $3CB00000 /
C      DATA LOG10(1),LOG10(2) / $509F79FF,  $3FD34413 /
C
C     MACHINE CONSTANTS FOR THE UNIVAC 1100 SERIES.
C
C      DATA SMALL(1),SMALL(2) / O000040000000, O000000000000 /
C      DATA LARGE(1),LARGE(2) / O377777777777, O777777777777 /
C      DATA RIGHT(1),RIGHT(2) / O170540000000, O000000000000 /
C      DATA DIVER(1),DIVER(2) / O170640000000, O000000000000 /
C      DATA LOG10(1),LOG10(2) / O177746420232, O411757177572 /
C
C     MACHINE CONSTANTS FOR THE VAX UNIX F77 COMPILER
C
C      DATA SMALL(1),SMALL(2) /        128,           0 /
C      DATA LARGE(1),LARGE(2) /     -32769,          -1 /
C      DATA RIGHT(1),RIGHT(2) /       9344,           0 /
C      DATA DIVER(1),DIVER(2) /       9472,           0 /
C      DATA LOG10(1),LOG10(2) /  546979738,  -805796613 /
C
C     MACHINE CONSTANTS FOR THE VAX-11 WITH
C     FORTRAN IV-PLUS COMPILER
C
C      DATA SMALL(1),SMALL(2) / Z00000080, Z00000000 /
C      DATA LARGE(1),LARGE(2) / ZFFFF7FFF, ZFFFFFFFF /
C      DATA RIGHT(1),RIGHT(2) / Z00002480, Z00000000 /
C      DATA DIVER(1),DIVER(2) / Z00002500, Z00000000 /
C      DATA LOG10(1),LOG10(2) / Z209A3F9A, ZCFF884FB /
C
C     MACHINE CONSTANTS FOR VAX/VMS VERSION 2.2
C
C      DATA SMALL(1),SMALL(2) /       '80'X,        '0'X /
C      DATA LARGE(1),LARGE(2) / 'FFFF7FFF'X, 'FFFFFFFF'X /
C      DATA RIGHT(1),RIGHT(2) /     '2480'X,        '0'X /
C      DATA DIVER(1),DIVER(2) /     '2500'X,        '0'X /
C      DATA LOG10(1),LOG10(2) / '209A3F9A'X, 'CFF884FB'X /
C
      IF (I .LT. 1  .OR.  I .GT. 5) GOTO 999
      D1MACH = DMACH(I)
      RETURN
  999 WRITE(I1MACH(2),1999) I
 1999 FORMAT(' D1MACH - I OUT OF BOUNDS',I10)
      STOP
      END
      REAL FUNCTION R1MACH(I)
      INTEGER I
C
C  SINGLE-PRECISION MACHINE CONSTANTS
C
C  R1MACH(1) = B**(EMIN-1), THE SMALLEST POSITIVE MAGNITUDE.
C
C  R1MACH(2) = B**EMAX*(1 - B**(-T)), THE LARGEST MAGNITUDE.
C
C  R1MACH(3) = B**(-T), THE SMALLEST RELATIVE SPACING.
C
C  R1MACH(4) = B**(1-T), THE LARGEST RELATIVE SPACING.
C
C  R1MACH(5) = LOG10(B)
C
C  TO ALTER THIS FUNCTION FOR A PARTICULAR ENVIRONMENT,
C  THE DESIRED SET OF DATA STATEMENTS SHOULD BE ACTIVATED BY
C  REMOVING THE C FROM COLUMN 1.
C
C  FOR IEEE-ARITHMETIC MACHINES (BINARY STANDARD), THE FIRST
C  SET OF CONSTANTS BELOW SHOULD BE APPROPRIATE.
C
C  WHERE POSSIBLE, DECIMAL, OCTAL OR HEXADECIMAL CONSTANTS ARE USED
C  TO SPECIFY THE CONSTANTS EXACTLY.  SOMETIMES THIS REQUIRES USING
C  EQUIVALENT INTEGER ARRAYS.  IF YOUR COMPILER USES HALF-WORD
C  INTEGERS BY DEFAULT (SOMETIMES CALLED INTEGER*2), YOU MAY NEED TO
C  CHANGE INTEGER TO INTEGER*4 OR OTHERWISE INSTRUCT YOUR COMPILER
C  TO USE FULL-WORD INTEGERS IN THE NEXT 5 DECLARATIONS.
C
C  COMMENTS JUST BEFORE THE END STATEMENT (LINES STARTING WITH *)
C  GIVE C SOURCE FOR R1MACH.
C
      INTEGER SMALL(2)
      INTEGER LARGE(2)
      INTEGER RIGHT(2)
      INTEGER DIVER(2)
      INTEGER LOG10(2)
      INTEGER CRAY1, SC
      COMMON /D8MACH/ CRAY1
C/6S
C/7S
      SAVE SMALL, LARGE, RIGHT, DIVER, LOG10, SC
C/
      REAL RMACH(5)
C
      EQUIVALENCE (RMACH(1),SMALL(1))
      EQUIVALENCE (RMACH(2),LARGE(1))
      EQUIVALENCE (RMACH(3),RIGHT(1))
      EQUIVALENCE (RMACH(4),DIVER(1))
      EQUIVALENCE (RMACH(5),LOG10(1))
C
C     MACHINE CONSTANTS FOR IEEE ARITHMETIC MACHINES, SUCH AS THE AT&T
C     3B SERIES, MOTOROLA 68000 BASED MACHINES (E.G. SUN 3 AND AT&T
C     PC 7300), AND 8087 BASED MICROS (E.G. IBM PC AND AT&T 6300).
C
       DATA SMALL(1) /     8388608 /
       DATA LARGE(1) /  2139095039 /
       DATA RIGHT(1) /   864026624 /
       DATA DIVER(1) /   872415232 /
       DATA LOG10(1) /  1050288283 /, SC/987/
 
C     MACHINE CONSTANTS FOR AMDAHL MACHINES.
C
C      DATA SMALL(1) /    1048576 /
C      DATA LARGE(1) / 2147483647 /
C      DATA RIGHT(1) /  990904320 /
C      DATA DIVER(1) / 1007681536 /
C      DATA LOG10(1) / 1091781651 /, SC/987/
C
C     MACHINE CONSTANTS FOR THE BURROUGHS 1700 SYSTEM.
C
C      DATA RMACH(1) / Z400800000 /
C      DATA RMACH(2) / Z5FFFFFFFF /
C      DATA RMACH(3) / Z4E9800000 /
C      DATA RMACH(4) / Z4EA800000 /
C      DATA RMACH(5) / Z500E730E8 /, SC/987/
C
C     MACHINE CONSTANTS FOR THE BURROUGHS 5700/6700/7700 SYSTEMS.
C
C      DATA RMACH(1) / O1771000000000000 /
C      DATA RMACH(2) / O0777777777777777 /
C      DATA RMACH(3) / O1311000000000000 /
C      DATA RMACH(4) / O1301000000000000 /
C      DATA RMACH(5) / O1157163034761675 /, SC/987/
C
C     MACHINE CONSTANTS FOR FTN4 ON THE CDC 6000/7000 SERIES.
C
C      DATA RMACH(1) / 00564000000000000000B /
C      DATA RMACH(2) / 37767777777777777776B /
C      DATA RMACH(3) / 16414000000000000000B /
C      DATA RMACH(4) / 16424000000000000000B /
C      DATA RMACH(5) / 17164642023241175720B /, SC/987/
C
C     MACHINE CONSTANTS FOR FTN5 ON THE CDC 6000/7000 SERIES.
C
C      DATA RMACH(1) / O"00564000000000000000" /
C      DATA RMACH(2) / O"37767777777777777776" /
C      DATA RMACH(3) / O"16414000000000000000" /
C      DATA RMACH(4) / O"16424000000000000000" /
C      DATA RMACH(5) / O"17164642023241175720" /, SC/987/
C
C     MACHINE CONSTANTS FOR CONVEX C-1.
C
C      DATA RMACH(1) / '00800000'X /
C      DATA RMACH(2) / '7FFFFFFF'X /
C      DATA RMACH(3) / '34800000'X /
C      DATA RMACH(4) / '35000000'X /
C      DATA RMACH(5) / '3F9A209B'X /, SC/987/
C
C     MACHINE CONSTANTS FOR THE CRAY 1, XMP, 2, AND 3.
C
C      DATA RMACH(1) / 200034000000000000000B /
C      DATA RMACH(2) / 577767777777777777776B /
C      DATA RMACH(3) / 377224000000000000000B /
C      DATA RMACH(4) / 377234000000000000000B /
C      DATA RMACH(5) / 377774642023241175720B /, SC/987/
C
C     MACHINE CONSTANTS FOR THE DATA GENERAL ECLIPSE S/200.
C
C     NOTE - IT MAY BE APPROPRIATE TO INCLUDE THE FOLLOWING LINE -
C     STATIC RMACH(5)
C
C      DATA SMALL/20K,0/,LARGE/77777K,177777K/
C      DATA RIGHT/35420K,0/,DIVER/36020K,0/
C      DATA LOG10/40423K,42023K/, SC/987/
C
C     MACHINE CONSTANTS FOR THE HARRIS SLASH 6 AND SLASH 7.
C
C      DATA SMALL(1),SMALL(2) / '20000000, '00000201 /
C      DATA LARGE(1),LARGE(2) / '37777777, '00000177 /
C      DATA RIGHT(1),RIGHT(2) / '20000000, '00000352 /
C      DATA DIVER(1),DIVER(2) / '20000000, '00000353 /
C      DATA LOG10(1),LOG10(2) / '23210115, '00000377 /, SC/987/
C
C     MACHINE CONSTANTS FOR THE HONEYWELL DPS 8/70 SERIES.
C
C      DATA RMACH(1) / O402400000000 /
C      DATA RMACH(2) / O376777777777 /
C      DATA RMACH(3) / O714400000000 /
C      DATA RMACH(4) / O716400000000 /
C      DATA RMACH(5) / O776464202324 /, SC/987/
C
C     MACHINE CONSTANTS FOR THE IBM 360/370 SERIES,
C     THE XEROX SIGMA 5/7/9 AND THE SEL SYSTEMS 85/86.
C
C      DATA RMACH(1) / Z00100000 /
C      DATA RMACH(2) / Z7FFFFFFF /
C      DATA RMACH(3) / Z3B100000 /
C      DATA RMACH(4) / Z3C100000 /
C      DATA RMACH(5) / Z41134413 /, SC/987/
C
C     MACHINE CONSTANTS FOR THE INTERDATA 8/32
C     WITH THE UNIX SYSTEM FORTRAN 77 COMPILER.
C
C     FOR THE INTERDATA FORTRAN VII COMPILER REPLACE
C     THE Z'S SPECIFYING HEX CONSTANTS WITH Y'S.
C
C      DATA RMACH(1) / Z'00100000' /
C      DATA RMACH(2) / Z'7EFFFFFF' /
C      DATA RMACH(3) / Z'3B100000' /
C      DATA RMACH(4) / Z'3C100000' /
C      DATA RMACH(5) / Z'41134413' /, SC/987/
C
C     MACHINE CONSTANTS FOR THE PDP-10 (KA OR KI PROCESSOR).
C
C      DATA RMACH(1) / "000400000000 /
C      DATA RMACH(2) / "377777777777 /
C      DATA RMACH(3) / "146400000000 /
C      DATA RMACH(4) / "147400000000 /
C      DATA RMACH(5) / "177464202324 /, SC/987/
C
C     MACHINE CONSTANTS FOR PDP-11 FORTRANS SUPPORTING
C     32-BIT INTEGERS (EXPRESSED IN INTEGER AND OCTAL).
C
C      DATA SMALL(1) /    8388608 /
C      DATA LARGE(1) / 2147483647 /
C      DATA RIGHT(1) /  880803840 /
C      DATA DIVER(1) /  889192448 /
C      DATA LOG10(1) / 1067065499 /, SC/987/
C
C      DATA RMACH(1) / O00040000000 /
C      DATA RMACH(2) / O17777777777 /
C      DATA RMACH(3) / O06440000000 /
C      DATA RMACH(4) / O06500000000 /
C      DATA RMACH(5) / O07746420233 /, SC/987/
C
C     MACHINE CONSTANTS FOR PDP-11 FORTRANS SUPPORTING
C     16-BIT INTEGERS  (EXPRESSED IN INTEGER AND OCTAL).
C
C      DATA SMALL(1),SMALL(2) /   128,     0 /
C      DATA LARGE(1),LARGE(2) / 32767,    -1 /
C      DATA RIGHT(1),RIGHT(2) / 13440,     0 /
C      DATA DIVER(1),DIVER(2) / 13568,     0 /
C      DATA LOG10(1),LOG10(2) / 16282,  8347 /, SC/987/
C
C      DATA SMALL(1),SMALL(2) / O000200, O000000 /
C      DATA LARGE(1),LARGE(2) / O077777, O177777 /
C      DATA RIGHT(1),RIGHT(2) / O032200, O000000 /
C      DATA DIVER(1),DIVER(2) / O032400, O000000 /
C      DATA LOG10(1),LOG10(2) / O037632, O020233 /, SC/987/
C
C     MACHINE CONSTANTS FOR THE SEQUENT BALANCE 8000.
C
C      DATA SMALL(1) / $00800000 /
C      DATA LARGE(1) / $7F7FFFFF /
C      DATA RIGHT(1) / $33800000 /
C      DATA DIVER(1) / $34000000 /
C      DATA LOG10(1) / $3E9A209B /, SC/987/
C
C     MACHINE CONSTANTS FOR THE UNIVAC 1100 SERIES.
C
C      DATA RMACH(1) / O000400000000 /
C      DATA RMACH(2) / O377777777777 /
C      DATA RMACH(3) / O146400000000 /
C      DATA RMACH(4) / O147400000000 /
C      DATA RMACH(5) / O177464202324 /, SC/987/
C
C     MACHINE CONSTANTS FOR THE VAX UNIX F77 COMPILER.
C
C      DATA SMALL(1) /       128 /
C      DATA LARGE(1) /    -32769 /
C      DATA RIGHT(1) /     13440 /
C      DATA DIVER(1) /     13568 /
C      DATA LOG10(1) / 547045274 /, SC/987/
C
C     MACHINE CONSTANTS FOR THE VAX-11 WITH
C     FORTRAN IV-PLUS COMPILER.
C
C      DATA RMACH(1) / Z00000080 /
C      DATA RMACH(2) / ZFFFF7FFF /
C      DATA RMACH(3) / Z00003480 /
C      DATA RMACH(4) / Z00003500 /
C      DATA RMACH(5) / Z209B3F9A /, SC/987/
C
C     MACHINE CONSTANTS FOR VAX/VMS VERSION 2.2.
C
C      DATA RMACH(1) /       '80'X /
C      DATA RMACH(2) / 'FFFF7FFF'X /
C      DATA RMACH(3) /     '3480'X /
C      DATA RMACH(4) /     '3500'X /
C      DATA RMACH(5) / '209B3F9A'X /, SC/987/
C
C  ***  ISSUE STOP 777 IF ALL DATA STATEMENTS ARE COMMENTED...
      IF (SC .NE. 987) THEN
*        *** CHECK FOR AUTODOUBLE ***
         SMALL(2) = 0
         RMACH(1) = 1E13
         IF (SMALL(2) .NE. 0) THEN
*           *** AUTODOUBLED ***
            IF (      SMALL(1) .EQ. 1117925532
     *          .AND. SMALL(2) .EQ. -448790528) THEN
*              *** IEEE BIG ENDIAN ***
               SMALL(1) = 1048576
               SMALL(2) = 0
               LARGE(1) = 2146435071
               LARGE(2) = -1
               RIGHT(1) = 1017118720
               RIGHT(2) = 0
               DIVER(1) = 1018167296
               DIVER(2) = 0
               LOG10(1) = 1070810131
               LOG10(2) = 1352628735
            ELSE IF ( SMALL(2) .EQ. 1117925532
     *          .AND. SMALL(1) .EQ. -448790528) THEN
*              *** IEEE LITTLE ENDIAN ***
               SMALL(2) = 1048576
               SMALL(1) = 0
               LARGE(2) = 2146435071
               LARGE(1) = -1
               RIGHT(2) = 1017118720
               RIGHT(1) = 0
               DIVER(2) = 1018167296
               DIVER(1) = 0
               LOG10(2) = 1070810131
               LOG10(1) = 1352628735
            ELSE IF ( SMALL(1) .EQ. -2065213935
     *          .AND. SMALL(2) .EQ. 10752) THEN
*              *** VAX WITH D_FLOATING ***
               SMALL(1) = 128
               SMALL(2) = 0
               LARGE(1) = -32769
               LARGE(2) = -1
               RIGHT(1) = 9344
               RIGHT(2) = 0
               DIVER(1) = 9472
               DIVER(2) = 0
               LOG10(1) = 546979738
               LOG10(2) = -805796613
            ELSE IF ( SMALL(1) .EQ. 1267827943
     *          .AND. SMALL(2) .EQ. 704643072) THEN
*              *** IBM MAINFRAME ***
               SMALL(1) = 1048576
               SMALL(2) = 0
               LARGE(1) = 2147483647
               LARGE(2) = -1
               RIGHT(1) = 856686592
               RIGHT(2) = 0
               DIVER(1) = 873463808
               DIVER(2) = 0
               LOG10(1) = 1091781651
               LOG10(2) = 1352628735
            ELSE
               WRITE(*,9010)
               STOP 777
               END IF
         ELSE
            RMACH(1) = 1234567.
            IF (SMALL(1) .EQ. 1234613304) THEN
*              *** IEEE ***
               SMALL(1) = 8388608
               LARGE(1) = 2139095039
               RIGHT(1) = 864026624
               DIVER(1) = 872415232
               LOG10(1) = 1050288283
            ELSE IF (SMALL(1) .EQ. -1271379306) THEN
*              *** VAX ***
               SMALL(1) = 128
               LARGE(1) = -32769
               RIGHT(1) = 13440
               DIVER(1) = 13568
               LOG10(1) = 547045274
            ELSE IF (SMALL(1) .EQ. 1175639687) THEN
*              *** IBM MAINFRAME ***
               SMALL(1) = 1048576
               LARGE(1) = 2147483647
               RIGHT(1) = 990904320
               DIVER(1) = 1007681536
               LOG10(1) = 1091781651
            ELSE IF (SMALL(1) .EQ. 1251390520) THEN
*              *** CONVEX C-1 ***
               SMALL(1) = 8388608
               LARGE(1) = 2147483647
               RIGHT(1) = 880803840
               DIVER(1) = 889192448
               LOG10(1) = 1067065499
            ELSE
*              CRAY1 = 4617762693716115456
               CRAY1 = 4617762
               CRAY1 = 1000000*CRAY1 + 693716
               CRAY1 = 1000000*CRAY1 + 115456
               IF (SMALL(1) .NE. CRAY1) THEN
                  WRITE(*,9020)
                  STOP 777
                  END IF
*              *** CRAY 1, XMP, 2, AND 3 ***
*              SMALL(1) = 2306828171632181248
               SMALL(1) = 2306828
               SMALL(1) = 1000000*SMALL(1) + 171632
               SMALL(1) = 1000000*SMALL(1) + 181248
*              LARGE(1) = 6917247552664371198
               LARGE(1) = 6917247
               LARGE(1) = 1000000*LARGE(1) + 552664
               LARGE(1) = 1000000*LARGE(1) + 371198
*              RIGHT(1) = 4598878906987053056
               RIGHT(1) = 4598878
               RIGHT(1) = 1000000*RIGHT(1) + 906987
               RIGHT(1) = 1000000*RIGHT(1) + 053056
*              DIVER(1) = 4599160381963763712
               DIVER(1) = 4599160
               DIVER(1) = 1000000*DIVER(1) + 381963
               DIVER(1) = 1000000*DIVER(1) + 763712
*              LOG10(1) = 4611574008272714704
               LOG10(1) = 4611574
               LOG10(1) = 1000000*LOG10(1) + 008272
               LOG10(1) = 1000000*LOG10(1) + 714704
               END IF
            END IF
         SC = 987
         END IF
C
C  ***  ISSUE STOP 776 IF ALL DATA STATEMENTS ARE OBVIOUSLY WRONG...
      IF (RMACH(4) .GE. 1.0) STOP 776
*C/6S
*C     IF (I .LT. 1  .OR.  I .GT. 5)
*C    1   CALL SETERR(24HR1MACH - I OUT OF BOUNDS,24,1,2)
*C/7S
*      IF (I .LT. 1  .OR.  I .GT. 5)
*     1   CALL SETERR('R1MACH - I OUT OF BOUNDS',24,1,2)
*C/
C
      IF (I .LT. 1 .OR. I .GT. 5) THEN
         WRITE(*,*) 'R1MACH(I): I =',I,' is out of bounds.'
         STOP
         END IF
      R1MACH = RMACH(I)
      RETURN
C/6S
C9010 FORMAT(/42H Adjust autodoubled R1MACH by getting data/
C    *42H appropriate for your machine from D1MACH.)
C9020 FORMAT(/46H Adjust R1MACH by uncommenting data statements/
C    *30H appropriate for your machine.)
C/7S
 9010 FORMAT(/' Adjust autodoubled R1MACH by getting data'/
     *' appropriate for your machine from D1MACH.')
 9020 FORMAT(/' Adjust R1MACH by uncommenting data statements'/
     *' appropriate for your machine.')
C/
C
* /* C source for R1MACH -- remove the * in column 1 */
*#include <stdio.h>
*#include <float.h>
*#include <math.h>
*
*float r1mach_(long *i)
*{
*	switch(*i){
*	  case 1: return FLT_MIN;
*	  case 2: return FLT_MAX;
*	  case 3: return FLT_EPSILON/FLT_RADIX;
*	  case 4: return FLT_EPSILON;
*	  case 5: return log10(FLT_RADIX);
*	  }
*
*	fprintf(stderr, "invalid argument: r1mach(%ld)\n", *i);
*	exit(1);
*	return 0; /* for compilers that complain of missing return values */
*	}
      END
      INTEGER FUNCTION I1MACH(I)
C
C  I/O UNIT NUMBERS.
C
C    I1MACH( 1) = THE STANDARD INPUT UNIT.
C
C    I1MACH( 2) = THE STANDARD OUTPUT UNIT.
C
C    I1MACH( 3) = THE STANDARD PUNCH UNIT.
C
C    I1MACH( 4) = THE STANDARD ERROR MESSAGE UNIT.
C
C  WORDS.
C
C    I1MACH( 5) = THE NUMBER OF BITS PER INTEGER STORAGE UNIT.
C
C    I1MACH( 6) = THE NUMBER OF CHARACTERS PER CHARACTER STORAGE UNIT.
C                 FOR FORTRAN 77, THIS IS ALWAYS 1.  FOR FORTRAN 66,
C                 CHARACTER STORAGE UNIT = INTEGER STORAGE UNIT.
C
C  INTEGERS.
C
C    ASSUME INTEGERS ARE REPRESENTED IN THE S-DIGIT, BASE-A FORM
C
C               SIGN ( X(S-1)*A**(S-1) + ... + X(1)*A + X(0) )
C
C               WHERE 0 .LE. X(I) .LT. A FOR I=0,...,S-1.
C
C    I1MACH( 7) = A, THE BASE.
C
C    I1MACH( 8) = S, THE NUMBER OF BASE-A DIGITS.
C
C    I1MACH( 9) = A**S - 1, THE LARGEST MAGNITUDE.
C
C  FLOATING-POINT NUMBERS.
C
C    ASSUME FLOATING-POINT NUMBERS ARE REPRESENTED IN THE T-DIGIT,
C    BASE-B FORM
C
C               SIGN (B**E)*( (X(1)/B) + ... + (X(T)/B**T) )
C
C               WHERE 0 .LE. X(I) .LT. B FOR I=1,...,T,
C               0 .LT. X(1), AND EMIN .LE. E .LE. EMAX.
C
C    I1MACH(10) = B, THE BASE.
C
C  SINGLE-PRECISION
C
C    I1MACH(11) = T, THE NUMBER OF BASE-B DIGITS.
C
C    I1MACH(12) = EMIN, THE SMALLEST EXPONENT E.
C
C    I1MACH(13) = EMAX, THE LARGEST EXPONENT E.
C
C  DOUBLE-PRECISION
C
C    I1MACH(14) = T, THE NUMBER OF BASE-B DIGITS.
C
C    I1MACH(15) = EMIN, THE SMALLEST EXPONENT E.
C
C    I1MACH(16) = EMAX, THE LARGEST EXPONENT E.
C
C  TO ALTER THIS FUNCTION FOR A PARTICULAR ENVIRONMENT,
C  THE DESIRED SET OF DATA STATEMENTS SHOULD BE ACTIVATED BY
C  REMOVING THE C FROM COLUMN 1.  ALSO, THE VALUES OF
C  I1MACH(1) - I1MACH(4) SHOULD BE CHECKED FOR CONSISTENCY
C  WITH THE LOCAL OPERATING SYSTEM.  FOR FORTRAN 77, YOU MAY WISH
C  TO ADJUST THE DATA STATEMENT SO IMACH(6) IS SET TO 1, AND
C  THEN TO COMMENT OUT THE EXECUTABLE TEST ON I .EQ. 6 BELOW.
C
C  FOR IEEE-ARITHMETIC MACHINES (BINARY STANDARD), THE FIRST
C  SET OF CONSTANTS BELOW SHOULD BE APPROPRIATE, EXCEPT PERHAPS
C  FOR IMACH(1) - IMACH(4).
C
C  COMMENTS JUST BEFORE THE END STATEMENT (LINES STARTING WITH *)
C  GIVE C SOURCE FOR I1MACH.
C
      INTEGER IMACH(16),OUTPUT,SANITY,I
C
      EQUIVALENCE (IMACH(4),OUTPUT)
C
C     MACHINE CONSTANTS FOR IEEE ARITHMETIC MACHINES, SUCH AS THE AT&T
C     3B SERIES, MOTOROLA 68000 BASED MACHINES (E.G. SUN 3 AND AT&T
C     PC 7300), AND 8087 BASED MICROS (E.G. IBM PC AND AT&T 6300).
C
       DATA IMACH( 1) /    5 /
       DATA IMACH( 2) /    6 /
       DATA IMACH( 3) /    7 /
       DATA IMACH( 4) /    6 /
       DATA IMACH( 5) /   32 /
       DATA IMACH( 6) /    4 /
       DATA IMACH( 7) /    2 /
       DATA IMACH( 8) /   31 /
       DATA IMACH( 9) / 2147483647 /
       DATA IMACH(10) /    2 /
       DATA IMACH(11) /   24 /
       DATA IMACH(12) / -125 /
       DATA IMACH(13) /  128 /
       DATA IMACH(14) /   53 /
       DATA IMACH(15) / -1021 /
       DATA IMACH(16) /  1024 /, SANITY/987/
C
C     MACHINE CONSTANTS FOR AMDAHL MACHINES.
C
C      DATA IMACH( 1) /   5 /
C      DATA IMACH( 2) /   6 /
C      DATA IMACH( 3) /   7 /
C      DATA IMACH( 4) /   6 /
C      DATA IMACH( 5) /  32 /
C      DATA IMACH( 6) /   4 /
C      DATA IMACH( 7) /   2 /
C      DATA IMACH( 8) /  31 /
C      DATA IMACH( 9) / 2147483647 /
C      DATA IMACH(10) /  16 /
C      DATA IMACH(11) /   6 /
C      DATA IMACH(12) / -64 /
C      DATA IMACH(13) /  63 /
C      DATA IMACH(14) /  14 /
C      DATA IMACH(15) / -64 /
C      DATA IMACH(16) /  63 /, SANITY/987/
C
C     MACHINE CONSTANTS FOR THE BURROUGHS 1700 SYSTEM.
C
C      DATA IMACH( 1) /    7 /
C      DATA IMACH( 2) /    2 /
C      DATA IMACH( 3) /    2 /
C      DATA IMACH( 4) /    2 /
C      DATA IMACH( 5) /   36 /
C      DATA IMACH( 6) /    4 /
C      DATA IMACH( 7) /    2 /
C      DATA IMACH( 8) /   33 /
C      DATA IMACH( 9) / Z1FFFFFFFF /
C      DATA IMACH(10) /    2 /
C      DATA IMACH(11) /   24 /
C      DATA IMACH(12) / -256 /
C      DATA IMACH(13) /  255 /
C      DATA IMACH(14) /   60 /
C      DATA IMACH(15) / -256 /
C      DATA IMACH(16) /  255 /, SANITY/987/
C
C     MACHINE CONSTANTS FOR THE BURROUGHS 5700 SYSTEM.
C
C      DATA IMACH( 1) /   5 /
C      DATA IMACH( 2) /   6 /
C      DATA IMACH( 3) /   7 /
C      DATA IMACH( 4) /   6 /
C      DATA IMACH( 5) /  48 /
C      DATA IMACH( 6) /   6 /
C      DATA IMACH( 7) /   2 /
C      DATA IMACH( 8) /  39 /
C      DATA IMACH( 9) / O0007777777777777 /
C      DATA IMACH(10) /   8 /
C      DATA IMACH(11) /  13 /
C      DATA IMACH(12) / -50 /
C      DATA IMACH(13) /  76 /
C      DATA IMACH(14) /  26 /
C      DATA IMACH(15) / -50 /
C      DATA IMACH(16) /  76 /, SANITY/987/
C
C     MACHINE CONSTANTS FOR THE BURROUGHS 6700/7700 SYSTEMS.
C
C      DATA IMACH( 1) /   5 /
C      DATA IMACH( 2) /   6 /
C      DATA IMACH( 3) /   7 /
C      DATA IMACH( 4) /   6 /
C      DATA IMACH( 5) /  48 /
C      DATA IMACH( 6) /   6 /
C      DATA IMACH( 7) /   2 /
C      DATA IMACH( 8) /  39 /
C      DATA IMACH( 9) / O0007777777777777 /
C      DATA IMACH(10) /   8 /
C      DATA IMACH(11) /  13 /
C      DATA IMACH(12) / -50 /
C      DATA IMACH(13) /  76 /
C      DATA IMACH(14) /  26 /
C      DATA IMACH(15) / -32754 /
C      DATA IMACH(16) /  32780 /, SANITY/987/
C
C     MACHINE CONSTANTS FOR FTN4 ON THE CDC 6000/7000 SERIES.
C
C      DATA IMACH( 1) /    5 /
C      DATA IMACH( 2) /    6 /
C      DATA IMACH( 3) /    7 /
C      DATA IMACH( 4) /    6 /
C      DATA IMACH( 5) /   60 /
C      DATA IMACH( 6) /   10 /
C      DATA IMACH( 7) /    2 /
C      DATA IMACH( 8) /   48 /
C      DATA IMACH( 9) / 00007777777777777777B /
C      DATA IMACH(10) /    2 /
C      DATA IMACH(11) /   47 /
C      DATA IMACH(12) / -929 /
C      DATA IMACH(13) / 1070 /
C      DATA IMACH(14) /   94 /
C      DATA IMACH(15) / -929 /
C      DATA IMACH(16) / 1069 /, SANITY/987/
C
C     MACHINE CONSTANTS FOR FTN5 ON THE CDC 6000/7000 SERIES.
C
C      DATA IMACH( 1) /    5 /
C      DATA IMACH( 2) /    6 /
C      DATA IMACH( 3) /    7 /
C      DATA IMACH( 4) /    6 /
C      DATA IMACH( 5) /   60 /
C      DATA IMACH( 6) /   10 /
C      DATA IMACH( 7) /    2 /
C      DATA IMACH( 8) /   48 /
C      DATA IMACH( 9) / O"00007777777777777777" /
C      DATA IMACH(10) /    2 /
C      DATA IMACH(11) /   47 /
C      DATA IMACH(12) / -929 /
C      DATA IMACH(13) / 1070 /
C      DATA IMACH(14) /   94 /
C      DATA IMACH(15) / -929 /
C      DATA IMACH(16) / 1069 /, SANITY/987/
C
C     MACHINE CONSTANTS FOR CONVEX C-1.
C
C      DATA IMACH( 1) /    5 /
C      DATA IMACH( 2) /    6 /
C      DATA IMACH( 3) /    7 /
C      DATA IMACH( 4) /    6 /
C      DATA IMACH( 5) /   32 /
C      DATA IMACH( 6) /    4 /
C      DATA IMACH( 7) /    2 /
C      DATA IMACH( 8) /   31 /
C      DATA IMACH( 9) / 2147483647 /
C      DATA IMACH(10) /    2 /
C      DATA IMACH(11) /   24 /
C      DATA IMACH(12) / -128 /
C      DATA IMACH(13) /  127 /
C      DATA IMACH(14) /   53 /
C      DATA IMACH(15) /-1024 /
C      DATA IMACH(16) / 1023 /, SANITY/987/
C
C     MACHINE CONSTANTS FOR THE CRAY 1, XMP, 2, AND 3.
C
C      DATA IMACH( 1) /     5 /
C      DATA IMACH( 2) /     6 /
C      DATA IMACH( 3) /   102 /
C      DATA IMACH( 4) /     6 /
C      DATA IMACH( 5) /    64 /
C      DATA IMACH( 6) /     8 /
C      DATA IMACH( 7) /     2 /
C      DATA IMACH( 8) /    63 /
C      DATA IMACH( 9) /  777777777777777777777B /
C      DATA IMACH(10) /     2 /
C      DATA IMACH(11) /    47 /
C      DATA IMACH(12) / -8189 /
C      DATA IMACH(13) /  8190 /
C      DATA IMACH(14) /    94 /
C      DATA IMACH(15) / -8099 /
C      DATA IMACH(16) /  8190 /, SANITY/987/
C
C     MACHINE CONSTANTS FOR THE DATA GENERAL ECLIPSE S/200.
C
C      DATA IMACH( 1) /   11 /
C      DATA IMACH( 2) /   12 /
C      DATA IMACH( 3) /    8 /
C      DATA IMACH( 4) /   10 /
C      DATA IMACH( 5) /   16 /
C      DATA IMACH( 6) /    2 /
C      DATA IMACH( 7) /    2 /
C      DATA IMACH( 8) /   15 /
C      DATA IMACH( 9) /32767 /
C      DATA IMACH(10) /   16 /
C      DATA IMACH(11) /    6 /
C      DATA IMACH(12) /  -64 /
C      DATA IMACH(13) /   63 /
C      DATA IMACH(14) /   14 /
C      DATA IMACH(15) /  -64 /
C      DATA IMACH(16) /   63 /, SANITY/987/
C
C     MACHINE CONSTANTS FOR THE HARRIS SLASH 6 AND SLASH 7.
C
C      DATA IMACH( 1) /       5 /
C      DATA IMACH( 2) /       6 /
C      DATA IMACH( 3) /       0 /
C      DATA IMACH( 4) /       6 /
C      DATA IMACH( 5) /      24 /
C      DATA IMACH( 6) /       3 /
C      DATA IMACH( 7) /       2 /
C      DATA IMACH( 8) /      23 /
C      DATA IMACH( 9) / 8388607 /
C      DATA IMACH(10) /       2 /
C      DATA IMACH(11) /      23 /
C      DATA IMACH(12) /    -127 /
C      DATA IMACH(13) /     127 /
C      DATA IMACH(14) /      38 /
C      DATA IMACH(15) /    -127 /
C      DATA IMACH(16) /     127 /, SANITY/987/
C
C     MACHINE CONSTANTS FOR THE HONEYWELL DPS 8/70 SERIES.
C
C      DATA IMACH( 1) /    5 /
C      DATA IMACH( 2) /    6 /
C      DATA IMACH( 3) /   43 /
C      DATA IMACH( 4) /    6 /
C      DATA IMACH( 5) /   36 /
C      DATA IMACH( 6) /    4 /
C      DATA IMACH( 7) /    2 /
C      DATA IMACH( 8) /   35 /
C      DATA IMACH( 9) / O377777777777 /
C      DATA IMACH(10) /    2 /
C      DATA IMACH(11) /   27 /
C      DATA IMACH(12) / -127 /
C      DATA IMACH(13) /  127 /
C      DATA IMACH(14) /   63 /
C      DATA IMACH(15) / -127 /
C      DATA IMACH(16) /  127 /, SANITY/987/
C
C     MACHINE CONSTANTS FOR THE IBM 360/370 SERIES,
C     THE XEROX SIGMA 5/7/9 AND THE SEL SYSTEMS 85/86.
C
C      DATA IMACH( 1) /   5 /
C      DATA IMACH( 2) /   6 /
C      DATA IMACH( 3) /   7 /
C      DATA IMACH( 4) /   6 /
C      DATA IMACH( 5) /  32 /
C      DATA IMACH( 6) /   4 /
C      DATA IMACH( 7) /   2 /
C      DATA IMACH( 8) /  31 /
C      DATA IMACH( 9) / Z7FFFFFFF /
C      DATA IMACH(10) /  16 /
C      DATA IMACH(11) /   6 /
C      DATA IMACH(12) / -64 /
C      DATA IMACH(13) /  63 /
C      DATA IMACH(14) /  14 /
C      DATA IMACH(15) / -64 /
C      DATA IMACH(16) /  63 /, SANITY/987/
C
C     MACHINE CONSTANTS FOR THE INTERDATA 8/32
C     WITH THE UNIX SYSTEM FORTRAN 77 COMPILER.
C
C     FOR THE INTERDATA FORTRAN VII COMPILER REPLACE
C     THE Z'S SPECIFYING HEX CONSTANTS WITH Y'S.
C
C      DATA IMACH( 1) /   5 /
C      DATA IMACH( 2) /   6 /
C      DATA IMACH( 3) /   6 /
C      DATA IMACH( 4) /   6 /
C      DATA IMACH( 5) /  32 /
C      DATA IMACH( 6) /   4 /
C      DATA IMACH( 7) /   2 /
C      DATA IMACH( 8) /  31 /
C      DATA IMACH( 9) / Z'7FFFFFFF' /
C      DATA IMACH(10) /  16 /
C      DATA IMACH(11) /   6 /
C      DATA IMACH(12) / -64 /
C      DATA IMACH(13) /  62 /
C      DATA IMACH(14) /  14 /
C      DATA IMACH(15) / -64 /
C      DATA IMACH(16) /  62 /, SANITY/987/
C
C     MACHINE CONSTANTS FOR THE PDP-10 (KA PROCESSOR).
C
C      DATA IMACH( 1) /    5 /
C      DATA IMACH( 2) /    6 /
C      DATA IMACH( 3) /    7 /
C      DATA IMACH( 4) /    6 /
C      DATA IMACH( 5) /   36 /
C      DATA IMACH( 6) /    5 /
C      DATA IMACH( 7) /    2 /
C      DATA IMACH( 8) /   35 /
C      DATA IMACH( 9) / "377777777777 /
C      DATA IMACH(10) /    2 /
C      DATA IMACH(11) /   27 /
C      DATA IMACH(12) / -128 /
C      DATA IMACH(13) /  127 /
C      DATA IMACH(14) /   54 /
C      DATA IMACH(15) / -101 /
C      DATA IMACH(16) /  127 /, SANITY/987/
C
C     MACHINE CONSTANTS FOR THE PDP-10 (KI PROCESSOR).
C
C      DATA IMACH( 1) /    5 /
C      DATA IMACH( 2) /    6 /
C      DATA IMACH( 3) /    7 /
C      DATA IMACH( 4) /    6 /
C      DATA IMACH( 5) /   36 /
C      DATA IMACH( 6) /    5 /
C      DATA IMACH( 7) /    2 /
C      DATA IMACH( 8) /   35 /
C      DATA IMACH( 9) / "377777777777 /
C      DATA IMACH(10) /    2 /
C      DATA IMACH(11) /   27 /
C      DATA IMACH(12) / -128 /
C      DATA IMACH(13) /  127 /
C      DATA IMACH(14) /   62 /
C      DATA IMACH(15) / -128 /
C      DATA IMACH(16) /  127 /, SANITY/987/
C
C     MACHINE CONSTANTS FOR PDP-11 FORTRANS SUPPORTING
C     32-BIT INTEGER ARITHMETIC.
C
C      DATA IMACH( 1) /    5 /
C      DATA IMACH( 2) /    6 /
C      DATA IMACH( 3) /    7 /
C      DATA IMACH( 4) /    6 /
C      DATA IMACH( 5) /   32 /
C      DATA IMACH( 6) /    4 /
C      DATA IMACH( 7) /    2 /
C      DATA IMACH( 8) /   31 /
C      DATA IMACH( 9) / 2147483647 /
C      DATA IMACH(10) /    2 /
C      DATA IMACH(11) /   24 /
C      DATA IMACH(12) / -127 /
C      DATA IMACH(13) /  127 /
C      DATA IMACH(14) /   56 /
C      DATA IMACH(15) / -127 /
C      DATA IMACH(16) /  127 /, SANITY/987/
C
C     MACHINE CONSTANTS FOR PDP-11 FORTRANS SUPPORTING
C     16-BIT INTEGER ARITHMETIC.
C
C      DATA IMACH( 1) /    5 /
C      DATA IMACH( 2) /    6 /
C      DATA IMACH( 3) /    7 /
C      DATA IMACH( 4) /    6 /
C      DATA IMACH( 5) /   16 /
C      DATA IMACH( 6) /    2 /
C      DATA IMACH( 7) /    2 /
C      DATA IMACH( 8) /   15 /
C      DATA IMACH( 9) / 32767 /
C      DATA IMACH(10) /    2 /
C      DATA IMACH(11) /   24 /
C      DATA IMACH(12) / -127 /
C      DATA IMACH(13) /  127 /
C      DATA IMACH(14) /   56 /
C      DATA IMACH(15) / -127 /
C      DATA IMACH(16) /  127 /, SANITY/987/
C
C     MACHINE CONSTANTS FOR THE PRIME 50 SERIES SYSTEMS
C     WTIH 32-BIT INTEGERS AND 64V MODE INSTRUCTIONS,
C     SUPPLIED BY IGOR BRAY.
C
C      DATA IMACH( 1) /            1 /
C      DATA IMACH( 2) /            1 /
C      DATA IMACH( 3) /            2 /
C      DATA IMACH( 4) /            1 /
C      DATA IMACH( 5) /           32 /
C      DATA IMACH( 6) /            4 /
C      DATA IMACH( 7) /            2 /
C      DATA IMACH( 8) /           31 /
C      DATA IMACH( 9) / :17777777777 /
C      DATA IMACH(10) /            2 /
C      DATA IMACH(11) /           23 /
C      DATA IMACH(12) /         -127 /
C      DATA IMACH(13) /         +127 /
C      DATA IMACH(14) /           47 /
C      DATA IMACH(15) /       -32895 /
C      DATA IMACH(16) /       +32637 /, SANITY/987/
C
C     MACHINE CONSTANTS FOR THE SEQUENT BALANCE 8000.
C
C      DATA IMACH( 1) /     0 /
C      DATA IMACH( 2) /     0 /
C      DATA IMACH( 3) /     7 /
C      DATA IMACH( 4) /     0 /
C      DATA IMACH( 5) /    32 /
C      DATA IMACH( 6) /     1 /
C      DATA IMACH( 7) /     2 /
C      DATA IMACH( 8) /    31 /
C      DATA IMACH( 9) /  2147483647 /
C      DATA IMACH(10) /     2 /
C      DATA IMACH(11) /    24 /
C      DATA IMACH(12) /  -125 /
C      DATA IMACH(13) /   128 /
C      DATA IMACH(14) /    53 /
C      DATA IMACH(15) / -1021 /
C      DATA IMACH(16) /  1024 /, SANITY/987/
C
C     MACHINE CONSTANTS FOR THE UNIVAC 1100 SERIES.
C
C     NOTE THAT THE PUNCH UNIT, I1MACH(3), HAS BEEN SET TO 7
C     WHICH IS APPROPRIATE FOR THE UNIVAC-FOR SYSTEM.
C     IF YOU HAVE THE UNIVAC-FTN SYSTEM, SET IT TO 1.
C
C      DATA IMACH( 1) /    5 /
C      DATA IMACH( 2) /    6 /
C      DATA IMACH( 3) /    7 /
C      DATA IMACH( 4) /    6 /
C      DATA IMACH( 5) /   36 /
C      DATA IMACH( 6) /    6 /
C      DATA IMACH( 7) /    2 /
C      DATA IMACH( 8) /   35 /
C      DATA IMACH( 9) / O377777777777 /
C      DATA IMACH(10) /    2 /
C      DATA IMACH(11) /   27 /
C      DATA IMACH(12) / -128 /
C      DATA IMACH(13) /  127 /
C      DATA IMACH(14) /   60 /
C      DATA IMACH(15) /-1024 /
C      DATA IMACH(16) / 1023 /, SANITY/987/
C
C     MACHINE CONSTANTS FOR VAX.
C
C      DATA IMACH( 1) /    5 /
C      DATA IMACH( 2) /    6 /
C      DATA IMACH( 3) /    7 /
C      DATA IMACH( 4) /    6 /
C      DATA IMACH( 5) /   32 /
C      DATA IMACH( 6) /    4 /
C      DATA IMACH( 7) /    2 /
C      DATA IMACH( 8) /   31 /
C      DATA IMACH( 9) / 2147483647 /
C      DATA IMACH(10) /    2 /
C      DATA IMACH(11) /   24 /
C      DATA IMACH(12) / -127 /
C      DATA IMACH(13) /  127 /
C      DATA IMACH(14) /   56 /
C      DATA IMACH(15) / -127 /
C      DATA IMACH(16) /  127 /, SANITY/987/
C
C  ***  ISSUE STOP 777 IF ALL DATA STATEMENTS ARE COMMENTED...
      IF (SANITY .NE. 987) STOP 777
      IF (I .LT. 1  .OR.  I .GT. 16) GO TO 10
C
      I1MACH = IMACH(I)
C/6S
C/7S
      IF(I.EQ.6) I1MACH=1
C/
      RETURN
   10 WRITE(OUTPUT,1999) I
 1999 FORMAT(' I1MACH - I OUT OF BOUNDS',I10)
      STOP
      END


C--**--CH3753--745--C:SU--12:6:2000
C--**--CH3163--745--B:UV--2:1:2000
* **********************************************************************
*
      SUBROUTINE FERMID(ORD,X,RELERR,FD,IERR)
*
* **********************************************************************
* FERMID returns in FD the value of the Fermi-Dirac integral of real
*        order ORD and real argument X, approximated with a relative
*        error RELERR.  FERMID is a driver routine that selects FDNINT
*        for integer ORD .LE. 0, FDNEG for X .LE. 0, and FDETA, FDPOS or
*        FDASYM for X .GT. 0.  A nonzero value is assigned to the error
*        flag IERR when an error condition occurs:
*           IERR = 1:  on input, the requested relative error RELERR is
*                      smaller than the machine precision;
*           IERR = 3:  an integral of large negative integer order could
*                      not be evaluated:  increase the parameter NMAX
*                      in subroutine FDNINT.
*           IERR = 4:  an integral (probably of small argument and large
*                      negative order) could not be evaluated with the
*                      requested accuracy after the inclusion of ITMAX
*                      terms of the series expansion:  increase the
*                      parameter ITMAX in the routine which produced the
*                      error message and in its subroutines.
*        When an error occurs, a message is also printed on the standard
*        output unit by the subroutine FERERR, and the execution of the
*        program is not interrupted; to change/suppress the output unit
*        or to stop the program when an error occurs, only FERERR should
*        be modified.
*
* References:
*
*   [1] M. Goano, "Series expansion of the Fermi-Dirac integral F_j(x)
*       over the entire domain of real j and x", Solid-State
*       Electronics, vol. 36, no. 2, pp. 217-221, 1993.
*
*   [2] J. S. Blakemore, "Approximation for Fermi-Dirac integrals,
*       especially the function F_1/2(eta) used to describe electron
*       density in a semiconductor", Solid-State Electronics, vol. 25,
*       no. 11, pp. 1067-1076, 1982.
*
* If a single precision version is desired, change all occurrences of
* *SP in columns 1 to 3 to blanks and comment the corresponding double
* precision statements.
*
* Michele Goano, Politecnico di Torino  (goano@polito.it).
* Latest revision:  March 23, 1994.
* **********************************************************************
*   Parameters
*SP     REAL             ONE, TEN, THREE, TWO, ZERO
*SP     PARAMETER (ONE = 1.0E+0, TEN = 10.0E+0, THREE = 3.0E+0,
*SP  &             TWO = 2.0E+0, ZERO = 0.0E+0)
*   Scalar arguments
*SP     REAL             ORD, X, RELERR, FD
*   Local scalars
*SP     REAL             RKDIV, XASYMP
*   External subroutines
*   Intrinsic functions
*SP     INTRINSIC ABS, ANINT, LOG10, MAX, NINT, SQRT
* ----------------------------------------------------------------------
*   Parameters of the floating-point arithmetic system.  Only the values
*   for very common machines are provided:  the subroutine MACHAR [1]
*   can be used to determine the machine constants of any other system.
*
*   [1] W. J. Cody,"Algorithm 665. MACHAR: A subroutine to dynamically
*       determine machine parameters", ACM Transactions on Mathematical
*       Software, vol. 14, no. 4, pp. 303-311, 1988.
*
*   ANSI/IEEE standard 745-1985:  IBM RISC 6000, DEC Alpha (S_floating
*   and T_floating), Apple Macintosh, SunSparc, most IBM PC compilers...
*SP     PARAMETER (MACHEP = -23, MINEXP = -126, MAXEXP = 128,
*SP  &             NEGEXP = -24)
*   DEC VAX (F_floating and D_floating)
*SP     PARAMETER (MACHEP = -24, MINEXP = -128, MAXEXP = 127,
*SP  &             NEGEXP = -24)
*DP     PARAMETER (MACHEP = -56, MINEXP = -128, MAXEXP = 127,
*DP  &             NEGEXP = -56)
*   CRAY
*SP     PARAMETER (MACHEP = -47, MINEXP = -8193, MAXEXP = 8191,
*SP  &             NEGEXP = -47)
*DP     PARAMETER (MACHEP = -95, MINEXP = -8193, MAXEXP = 8191,
*DP  &             NEGEXP = -95)
*
*SP     REAL             BETA, EPS, XMIN, XMAX
C       XBIG = LOG(XMAX)
* ----------------------------------------------------------------------
C     .. Parameters ..
      DOUBLE PRECISION ONE,TEN,THREE,TWO,ZERO
      PARAMETER (ONE=1.0D+0,TEN=10.0D+0,THREE=3.0D+0,TWO=2.0D+0,
     +          ZERO=0.0D+0)
      INTEGER MACHEP,MINEXP,MAXEXP,NEGEXP
      PARAMETER (MACHEP=-52,MINEXP=-1022,MAXEXP=1024,NEGEXP=-53)
      DOUBLE PRECISION BETA,EPS,XMIN,XMAX
      PARAMETER (BETA=TWO,EPS=BETA**MACHEP,XMIN=BETA**MINEXP,
     +          XMAX= (BETA** (MAXEXP-1)-BETA** (MAXEXP+NEGEXP-1))*BETA)
C     ..
C     .. Scalar Arguments ..
      DOUBLE PRECISION FD,ORD,RELERR,X
      INTEGER IERR
C     ..
C     .. Local Scalars ..
      DOUBLE PRECISION RKDIV,XASYMP
      INTEGER NORD
      LOGICAL INTORD,TRYASY
C     ..
C     .. External Subroutines ..
      EXTERNAL FDASYM,FDETA,FDNEG,FDNINT,FDPOS,FERERR
C     ..
C     .. Intrinsic Functions ..
      INTRINSIC ABS,ANINT,LOG10,MAX,NINT,SQRT
C     ..
      IERR = 0
      FD = ZERO
      INTORD = ABS(ORD-ANINT(ORD)) .LE. ABS(ORD)*EPS
      IF (RELERR.LT.EPS) THEN
*   Test on the accuracy requested by the user
          IERR = 1
          CALL FERERR(' FERMID:  Input error: '//
     +                ' RELERR is smaller than the machine precision')

      ELSE IF (INTORD .AND. ORD.LE.ZERO) THEN
*   Analytic expression for integer ORD .le. 0
          NORD = NINT(ORD)
          CALL FDNINT(NORD,X,FD,IERR)

      ELSE IF (X.LE.ZERO) THEN
*   Series expansion for negative argument
          CALL FDNEG(ORD,X,XMIN,RELERR,FD,IERR)

      ELSE
*   Positive argument:  approximations for k_div - 1 and x_min (RKDIV
*   and XASYMP)
          RKDIV = -LOG10(RELERR)
          XASYMP = MAX(ORD-ONE,TWO*RKDIV-ORD* (TWO+RKDIV/TEN),
     +             SQRT(ABS((2*RKDIV-ONE-ORD)* (2*RKDIV-ORD))))
          IF (X.GT.XASYMP .OR. INTORD) THEN
*   Asymptotic expansion, used also for positive integer order
              TRYASY = .TRUE.
              CALL FDASYM(ORD,X,EPS,XMAX,XMIN,RELERR,FD,IERR)

          ELSE
              TRYASY = .FALSE.
          END IF

          IF (.NOT.TRYASY .OR. IERR.NE.0) THEN
              IF (ORD.GT.-TWO .AND. X.LT.TWO/THREE) THEN
*   Taylor series expansion, involving eta function
                  CALL FDETA(ORD,X,EPS,XMAX,RELERR,FD,IERR)

              ELSE
*   Series expansion for positive argument, involving confluent
*   hypergeometric functions
                  CALL FDPOS(ORD,X,EPS,XMAX,RELERR,FD,IERR)
              END IF

          END IF

      END IF

      RETURN

      END

* **********************************************************************
*
      SUBROUTINE FERINC(ORD,X,B,RELERR,FDI,IERR)
*
* **********************************************************************
* FERINC returns in FDI the value of the incomplete Fermi-Dirac integral
*        of real order ORD and real arguments X and B, approximated with
*        a relative error RELERR.  Levin's u transform [2] is used to
*        sum the alternating series (21) of [1].  A nonzero value is
*        assigned to the error flag IERR when an error condition occurs:
*           IERR = 1:  on input, the requested relative error RELERR is
*                      smaller than the machine precision;
*           IERR = 2:  on input, the lower bound B of the incomplete
*                      integral is lower than zero;
*           IERR = 3:  a complete integral of very large negative
*                      integer order could not be evaluated:  increase
*                      the parameter NMAX in subroutine FDNINT.
*           IERR = 4:  an integral (probably of small argument and large
*                      negative order) could not be evaluated with the
*                      requested accuracy after the inclusion of ITMAX
*                      terms of the series expansion:  increase the
*                      parameter ITMAX in the routine which produced the
*                      error message and in its subroutines.
*        When an error occurs, a message is also printed on the standard
*        output unit by the subroutine FERERR, and the execution of the
*        program is not interrupted; to change/suppress the output unit
*        and/or to stop the program when an error occurs, only FERERR
*        should be modified.
*
* References:
*
*   [1] M. Goano, "Series expansion of the Fermi-Dirac integral F_j(x)
*       over the entire domain of real j and x", Solid-State
*       Electronics, vol. 36, no. 2, pp. 217-221, 1993.
*
*   [2] T. Fessler, W. F. Ford, D. A. Smith, "ALGORITHM 602. HURRY: An
*       acceleration algorithm for scalar sequences and series", ACM
*       Transactions on Mathematical Software, vol. 9, no. 3,
*       pp. 355-357, September  1983.
*
* If a single precision version is desired, change all occurrences of
* *SP in columns 1 to 3 to blanks and comment the corresponding double
* precision statements.
*
* Michele Goano, Politecnico di Torino  (goano@polito.it).
* Latest revision:  March 22, 1994.
* **********************************************************************
*   Parameters
*SP     REAL             ONE, TWO, ZERO
*SP     PARAMETER (ONE = 1.0E+0, TWO = 2.0E+0, ZERO = 0.0E+0)
*   Scalar arguments
*SP     REAL             ORD, X, B, RELERR, FDI
*   Local scalars
*SP     REAL             BMX, BMXN, BN, EBMX, ENBMX, ENXMB,
*SP  &                   EXMB, FD, FDOLD, GAMMA, M, S, TERM, U,
*SP  &                   XMB, XMBN
*   Local arrays
*SP     REAL             QNUM(ITMAX), QDEN(ITMAX)
*   External subroutines
*   Intrinsic functions
* ----------------------------------------------------------------------
*   Parameters of the floating-point arithmetic system.  Only the values
*   for very common machines are provided:  the subroutine MACHAR [1]
*   can be used to determine the machine constants of any other system.
*
*   [1] W. J. Cody,"Algorithm 665. MACHAR: A subroutine to dynamically
*       determine machine parameters", ACM Transactions on Mathematical
*       Software, vol. 14, no. 4, pp. 303-311, 1988.
*
*   ANSI/IEEE standard 745-1985:  IBM RISC 6000, DEC Alpha (S_floating
*   and T_floating), Apple Macintosh, SunSparc, most IBM PC compilers...
*SP     PARAMETER (MACHEP = -23, MINEXP = -126, MAXEXP = 128,
*SP  &             NEGEXP = -24)
*   DEC VAX (F_floating and D_floating)
*SP     PARAMETER (MACHEP = -24, MINEXP = -128, MAXEXP = 127,
*SP  &             NEGEXP = -24)
*DP     PARAMETER (MACHEP = -56, MINEXP = -128, MAXEXP = 127,
*DP  &             NEGEXP = -56)
*   CRAY
*SP     PARAMETER (MACHEP = -47, MINEXP = -8193, MAXEXP = 8191,
*SP  &             NEGEXP = -47)
*DP     PARAMETER (MACHEP = -95, MINEXP = -8193, MAXEXP = 8191,
*DP  &             NEGEXP = -95)
*
*SP     REAL             EPS, XMIN, XMAX, XTINY
C     .. Parameters ..
      INTEGER ITMAX
      PARAMETER (ITMAX=100)
      DOUBLE PRECISION ONE,TWO,ZERO
      PARAMETER (ONE=1.0D+0,TWO=2.0D+0,ZERO=0.0D+0)
      INTEGER MACHEP,MINEXP,MAXEXP,NEGEXP
      PARAMETER (MACHEP=-52,MINEXP=-1022,MAXEXP=1024,NEGEXP=-53)
      DOUBLE PRECISION EPS,XMIN,XMAX
      PARAMETER (EPS=TWO**MACHEP,XMIN=TWO**MINEXP,
     +          XMAX= (TWO** (MAXEXP-1)-TWO** (MAXEXP+NEGEXP-1))*TWO)
C     ..
C     .. Scalar Arguments ..
      DOUBLE PRECISION B,FDI,ORD,RELERR,X
      INTEGER IERR
C     ..
C     .. Local Scalars ..
      DOUBLE PRECISION BMX,BMXN,BN,EBMX,ENBMX,ENXMB,EXMB,FD,FDOLD,GAMMA,
     +                 M,S,TERM,U,XMB,XMBN,XTINY
      INTEGER JTERM
      LOGICAL LOGGAM
C     ..
C     .. Local Arrays ..
      DOUBLE PRECISION QDEN(ITMAX),QNUM(ITMAX)
C     ..
C     .. External Subroutines ..
      EXTERNAL FERERR,FERMID,GAMMAC,M1KUMM,U1KUMM,WHIZ
C     ..
C     .. Intrinsic Functions ..
      INTRINSIC ABS,ANINT,EXP,LOG,NINT
C     ..
      XTINY = LOG(XMIN)
* ----------------------------------------------------------------------
      IERR = 0
      FDI = ZERO
      IF (RELERR.LT.EPS) THEN
*   Test on the accuracy requested by the user
          IERR = 1
          CALL FERERR(
     +   ' FERINC:  Input error:  RELERR smaller than machine precision'
     +                )

      ELSE IF (B.LT.ZERO) THEN
*   Error in the argument B
          IERR = 2
          CALL FERERR(' FERINC:  Input error:  B is lower than zero')

      ELSE IF (B.EQ.ZERO) THEN
*   Complete integral
          CALL FERMID(ORD,X,RELERR,FDI,IERR)

      ELSE IF (ORD.LE.ZERO .AND. ABS(ORD-ANINT(ORD)).LE.
     +         ABS(ORD)*EPS) THEN
*   Analytic expression for integer ORD .le. 0
          IF (NINT(ORD).EQ.0) THEN
              XMB = X - B
              IF (XMB.GE.ZERO) THEN
                  FDI = XMB + LOG(ONE+EXP(-XMB))

              ELSE
                  FDI = LOG(ONE+EXP(XMB))
              END IF

          ELSE
              FDI = ZERO
          END IF

      ELSE IF (B.LT.X) THEN
*   Series involving Kummer's function M
          CALL FERMID(ORD,X,RELERR,FD,IERR)
          CALL GAMMAC(ORD+TWO,EPS,XMAX,GAMMA,IERR)
          LOGGAM = .FALSE.
          IF (IERR.EQ.-1) THEN
              LOGGAM = .TRUE.
              IERR = 0
          END IF

          BMX = B - X
          BMXN = BMX
          EBMX = -EXP(BMX)
          ENBMX = -EBMX
          BN = B
          FDI = XMAX
          DO 10 JTERM = 1,ITMAX
              FDOLD = FDI
              CALL M1KUMM(ORD,BN,EPS,XMAX,RELERR,M)
              TERM = ENBMX*M
              CALL WHIZ(TERM,JTERM,QNUM,QDEN,FDI,S)
*   Check truncation error and convergence
              BMXN = BMXN + BMX
              IF (ABS(FDI-FDOLD).LE.ABS(ONE-FDI)*RELERR .OR.
     +            BMXN.LT.XTINY) GO TO 20
              ENBMX = ENBMX*EBMX
              BN = BN + B
   10     CONTINUE
          IERR = 4
          CALL FERERR(
     +        ' FERINC:  RELERR not achieved:  increase parameter ITMAX'
     +                )
   20     CONTINUE
          IF (LOGGAM) THEN
              FDI = FD - EXP((ORD+ONE)*LOG(B)-GAMMA)* (ONE-FDI)

          ELSE
              FDI = FD - B** (ORD+ONE)/GAMMA* (ONE-FDI)
          END IF

      ELSE
*   Series involving Kummer's function U
          CALL GAMMAC(ORD+ONE,EPS,XMAX,GAMMA,IERR)
          LOGGAM = .FALSE.
          IF (IERR.EQ.-1) THEN
              LOGGAM = .TRUE.
              IERR = 0
          END IF

          XMB = X - B
          XMBN = XMB
          EXMB = -EXP(XMB)
          ENXMB = -EXMB
          BN = B
          FDI = XMAX
          DO 30 JTERM = 1,ITMAX
              FDOLD = FDI
              CALL U1KUMM(ORD+ONE,BN,EPS,XMAX,RELERR,U)
              TERM = ENXMB*U
              CALL WHIZ(TERM,JTERM,QNUM,QDEN,FDI,S)
*   Check truncation error and convergence
              XMBN = XMBN + XMB
              IF (ABS(FDI-FDOLD).LE.ABS(FDI)*RELERR .OR.
     +            XMBN.LT.XTINY) GO TO 40
              ENXMB = ENXMB*EXMB
              BN = BN + B
   30     CONTINUE
          IERR = 4
          CALL FERERR(
     +        ' FERINC:  RELERR not achieved:  increase parameter ITMAX'
     +                )
   40     CONTINUE
          IF (LOGGAM) THEN
              FDI = EXP((ORD+ONE)*LOG(B)-GAMMA)*FDI

          ELSE
              FDI = B** (ORD+ONE)/GAMMA*FDI
          END IF

      END IF

      RETURN

      END

* **********************************************************************
*
      SUBROUTINE FDNINT(NORD,X,FD,IERR)
*
* **********************************************************************
* FDNINT returns in FD the value of the Fermi-Dirac integral of integer
*        order NORD (-NMAX-1 .LE. NORD .LE. 0) and argument X, for
*        which an analytical expression is available.  A nonzero value
*        is assigned to the error flag IERR when ABS(NORD).GT.NMAX+1:
*        to remedy, increase the parameter NMAX.
*
* If a single precision version is desired, change all occurrences of
* *SP in columns 1 to 3 to blanks and comment the corresponding double
* precision statements.
*
* Michele Goano, Politecnico di Torino  (goano@polito.it).
* Latest revision:  March 22, 1994.
* **********************************************************************
*   Parameters
*SP     REAL             ONE
*SP     PARAMETER (ONE = 1.0E+0, ZERO = 0.0E+0)
*   Scalar arguments
*SP     REAL             X, FD
*   Local scalars
*SP     REAL             A
*   Local arrays
*SP     REAL             QCOEF(NMAX)
*   External subroutines
*   Intrinsic functions
*SP     INTRINSIC EXP, LOG, REAL
* ----------------------------------------------------------------------
C     .. Parameters ..
      INTEGER NMAX
      PARAMETER (NMAX=100)
      DOUBLE PRECISION ONE,ZERO
      PARAMETER (ONE=1.0D+0,ZERO=0.0D+0)
C     ..
C     .. Scalar Arguments ..
      DOUBLE PRECISION FD,X
      INTEGER IERR,NORD
C     ..
C     .. Local Scalars ..
      DOUBLE PRECISION A
      INTEGER I,K,N
C     ..
C     .. Local Arrays ..
      DOUBLE PRECISION QCOEF(NMAX)
C     ..
C     .. External Subroutines ..
      EXTERNAL FERERR
C     ..
C     .. Intrinsic Functions ..
      INTRINSIC DBLE,EXP,LOG
C     ..
      IERR = 0
      FD = ZERO
*   Test on the order, whose absolute value must be lower or equal than
*   NMAX+1
      IF (NORD.LT.-NMAX-1) THEN
          IERR = 3
          CALL FERERR(
     +             ' FDNINT:  order too large:  increase parameter NMAX'
     +                )

      ELSE IF (NORD.EQ.0) THEN
*   Analytic expression for NORD .eq. 0
          IF (X.GE.ZERO) THEN
              FD = X + LOG(ONE+EXP(-X))

          ELSE
              FD = LOG(ONE+EXP(X))
          END IF

      ELSE IF (NORD.EQ.-1) THEN
*   Analytic expression for NORD .eq. -1
          IF (X.GE.ZERO) THEN
              FD = ONE/ (ONE+EXP(-X))

          ELSE
              A = EXP(X)
              FD = A/ (ONE+A)
          END IF

      ELSE
*   Evaluation of the coefficients of the polynomial P(a), having degree
*   (-NORD - 2), appearing at the numerator of the analytic expression
*   for NORD .le. -2
          N = -NORD - 1
          QCOEF(1) = ONE
          DO 20 K = 2,N
              QCOEF(K) = -QCOEF(K-1)
              DO 10 I = K - 1,2,-1
*SP           QCOEF(I) = REAL(I)*QCOEF(I) - REAL(K - (I-1))*QCOEF(I - 1)
                  QCOEF(I) = DBLE(I)*QCOEF(I) -
     +                       DBLE(K- (I-1))*QCOEF(I-1)
   10         CONTINUE
   20     CONTINUE
*   Computation of P(a)
          IF (X.GE.ZERO) THEN
              A = EXP(-X)
              FD = QCOEF(1)
              DO 30 I = 2,N
                  FD = FD*A + QCOEF(I)
   30         CONTINUE

          ELSE
              A = EXP(X)
              FD = QCOEF(N)
              DO 40 I = N - 1,1,-1
                  FD = FD*A + QCOEF(I)
   40         CONTINUE
          END IF
*   Evaluation of the Fermi-Dirac integral
          FD = FD*A* (ONE+A)**NORD
      END IF

      RETURN

      END


* **********************************************************************
*
      SUBROUTINE FDNEG(ORD,X,XMIN,RELERR,FD,IERR)
*
* **********************************************************************
* FDNEG returns in FD the value of the Fermi-Dirac integral of real
*       order ORD and negative argument X, approximated with a relative
*       error RELERR.  XMIN represent the smallest non-vanishing
*       floating-point number.  Levin's u transform [2] is used to sum
*       the alternating series (13) of [1].
*
* References:
*
*   [1] J. S. Blakemore, "Approximation for Fermi-Dirac integrals,
*       especially the function F_1/2(eta) used to describe electron
*       density in a semiconductor", Solid-State Electronics, vol. 25,
*       no. 11, pp. 1067-1076, 1982.
*
*   [2] T. Fessler, W. F. Ford, D. A. Smith, "ALGORITHM 602. HURRY: An
*       acceleration algorithm for scalar sequences and series", ACM
*       Transactions on Mathematical Software, vol. 9, no. 3,
*       pp. 355-357, September  1983.
*
* If a single precision version is desired, change all occurrences of
* *SP in columns 1 to 3 to blanks and comment the corresponding double
* precision statements.
*
* Michele Goano, Politecnico di Torino  (goano@polito.it).
* Latest revision:  February 5, 1996.
* **********************************************************************
*   Parameters
*SP     REAL             ONE, ZERO
*SP     PARAMETER (ONE = 1.0E+0, ZERO = 0.0E+0)
*   Scalar arguments
*SP     REAL             ORD, X, XMIN, RELERR, FD
*   Local scalars
*SP     REAL             EX, ENX, FDOLD, S, TERM, XN, XTINY
*   Local arrays
*SP     REAL             QNUM(ITMAX), QDEN(ITMAX)
*   External subroutines
*   Intrinsic functions
*SP     INTRINSIC ABS, EXP, LOG, REAL
* ----------------------------------------------------------------------
C     .. Parameters ..
      INTEGER ITMAX
      PARAMETER (ITMAX=100)
      DOUBLE PRECISION ONE,ZERO
      PARAMETER (ONE=1.0D+0,ZERO=0.0D+0)
C     ..
C     .. Scalar Arguments ..
      DOUBLE PRECISION FD,ORD,RELERR,X,XMIN
      INTEGER IERR
C     ..
C     .. Local Scalars ..
      DOUBLE PRECISION ENX,EX,FDOLD,S,TERM,XN,XTINY
      INTEGER JTERM
C     ..
C     .. Local Arrays ..
      DOUBLE PRECISION QDEN(ITMAX),QNUM(ITMAX)
C     ..
C     .. External Subroutines ..
      EXTERNAL FERERR,WHIZ
C     ..
C     .. Intrinsic Functions ..
      INTRINSIC ABS,DBLE,EXP,LOG
C     ..
      IERR = 0
      FD = ZERO
      XTINY = LOG(XMIN)
*
      IF (X.GT.XTINY) THEN
          XN = X
          EX = -EXP(X)
          ENX = -EX
          DO 10 JTERM = 1,ITMAX
              FDOLD = FD
*SP         TERM = ENX/REAL(JTERM)**(ORD + ONE)
              TERM = ENX/DBLE(JTERM)** (ORD+ONE)
              CALL WHIZ(TERM,JTERM,QNUM,QDEN,FD,S)
*   Check truncation error and convergence
              XN = XN + X
              IF (ABS(FD-FDOLD).LE.ABS(FD)*RELERR .OR.
     +            XN.LT.XTINY) RETURN
              ENX = ENX*EX
   10     CONTINUE
          IERR = 4
          CALL FERERR(
     +         ' FDNEG:  RELERR not achieved:  increase parameter ITMAX'
     +                )
      END IF

      RETURN

      END
* **********************************************************************
*
      SUBROUTINE FDPOS(ORD,X,EPS,XMAX,RELERR,FD,IERR)
*
* **********************************************************************
* FDPOS returns in FD the value of the Fermi-Dirac integral of real
*       order ORD and argument X .GT. 0, approximated with a relative
*       error RELERR.  EPS and XMAX represent the smallest positive
*       floating-point number such that 1.0+EPS .NE. 1.0, and the
*       largest finite floating-point number, respectively.  Levin's u
*       transform [2] is used to sum the alternating series (11) of [1].
*
* References:
*
*   [1] M. Goano, "Series expansion of the Fermi-Dirac integral F_j(x)
*       over the entire domain of real j and x", Solid-State
*       Electronics, vol. 36, no. 2, pp. 217-221, 1993.
*
*   [2] T. Fessler, W. F. Ford, D. A. Smith, "ALGORITHM 602. HURRY: An
*       acceleration algorithm for scalar sequences and series", ACM
*       Transactions on Mathematical Software, vol. 9, no. 3,
*       pp. 355-357, September  1983.
*
* If a single precision version is desired, change all occurrences of
* *SP in columns 1 to 3 to blanks and comment the corresponding double
* precision statements.
*
* Michele Goano, Politecnico di Torino  (goano@polito.it).
* Latest revision:  March 22, 1994.
* **********************************************************************
*   Parameters
*SP     REAL             ONE, TWO, ZERO
*SP     PARAMETER (ONE = 1.0E+0, TWO = 2.0E+0, ZERO = 0.0E+0)
*   Scalar arguments
*SP     REAL             ORD, X, EPS, XMAX, RELERR, FD
*   Local scalars
*SP     REAL             FDOLD, GAMMA, M, S, SEGN, TERM, U, XN
*   Local arrays
*SP     REAL             QNUM(ITMAX), QDEN(ITMAX)
*   External subroutines
*   Intrinsic functions
* ----------------------------------------------------------------------
C     .. Parameters ..
      INTEGER ITMAX
      PARAMETER (ITMAX=100)
      DOUBLE PRECISION ONE,TWO,ZERO
      PARAMETER (ONE=1.0D+0,TWO=2.0D+0,ZERO=0.0D+0)
C     ..
C     .. Scalar Arguments ..
      DOUBLE PRECISION EPS,FD,ORD,RELERR,X,XMAX
      INTEGER IERR
C     ..
C     .. Local Scalars ..
      DOUBLE PRECISION FDOLD,GAMMA,M,S,SEGN,TERM,U,XN
      INTEGER JTERM
      LOGICAL LOGGAM
C     ..
C     .. Local Arrays ..
      DOUBLE PRECISION QDEN(ITMAX),QNUM(ITMAX)
C     ..
C     .. External Subroutines ..
      EXTERNAL FERERR,GAMMAC,M1KUMM,U1KUMM,WHIZ
C     ..
C     .. Intrinsic Functions ..
      INTRINSIC ABS,EXP,LOG
C     ..
      IERR = 0
      FD = ZERO
*
      CALL GAMMAC(ORD+TWO,EPS,XMAX,GAMMA,IERR)
      LOGGAM = .FALSE.
      IF (IERR.EQ.-1) LOGGAM = .TRUE.
      SEGN = ONE
      XN = X
      FD = XMAX
      DO 10 JTERM = 1,ITMAX
          FDOLD = FD
          CALL U1KUMM(ORD+ONE,XN,EPS,XMAX,RELERR,U)
          CALL M1KUMM(ORD,XN,EPS,XMAX,RELERR,M)
          TERM = SEGN* ((ORD+ONE)*U-M)
          CALL WHIZ(TERM,JTERM,QNUM,QDEN,FD,S)
*   Check truncation error and convergence
          IF (ABS(FD-FDOLD).LE.ABS(FD+ONE)*RELERR) GO TO 20
          SEGN = -SEGN
          XN = XN + X
   10 CONTINUE
      IERR = 4
      CALL FERERR(
     +         ' FDPOS:  RELERR not achieved:  increase parameter ITMAX'
     +            )
   20 CONTINUE
      IF (LOGGAM) THEN
          FD = EXP((ORD+ONE)*LOG(X)-GAMMA)* (ONE+FD)

      ELSE
          FD = X** (ORD+ONE)/GAMMA* (ONE+FD)
      END IF

      RETURN

      END

* **********************************************************************
*
      SUBROUTINE FDETA(ORD,X,EPS,XMAX,RELERR,FD,IERR)
*
* **********************************************************************
* FDETA returns in FD the value of the Fermi-Dirac integral of real
*       order ORD and argument X such that ABS(X) .LE. PI, approximated
*       with a relative error RELERR.  EPS and XMAX represent the
*       smallest positive floating-point number such that
*       1.0+EPS .NE. 1.0, and the largest finite floating-point number,
*       respectively.  Taylor series expansion (4) of [1] is used,
*       involving eta function defined in (23.2.19) of [2].
*
*
* References:
*
*   [1] W. J. Cody and H. C. Thacher, Jr., "Rational Chebyshev
*       approximations for Fermi-Dirac integrals of orders -1/2, 1/2 and
*       3/2", Mathematics of Computation, vol. 21, no. 97, pp. 30-40,
*       1967.
*
*   [2] E. V. Haynsworth and K. Goldberg, "Bernoulli and Euler
*       Polynomials - Riemann Zeta Function", in "Handbook of
*       Mathematical Functions with Formulas, Graphs and Mathematical
*       Tables" (M. Abramowitz and I. A. Stegun, eds.), no. 55 in
*       National Bureau of Standards Applied Mathematics Series, ch. 23,
*       pp. 803-819, Washington, D.C.:  U.S. Government Printing Office,
*       1964.
*
* If a single precision version is desired, change all occurrences of
* *SP in columns 1 to 3 to blanks and comment the corresponding double
* precision statements.
*
* Michele Goano, Politecnico di Torino  (goano@polito.it).
* Latest revision:  March 22, 1994.
* **********************************************************************
*   Parameters
*SP     REAL             ONE, TWO, ZERO
*SP     PARAMETER (ONE = 1.0E+0, PI = 3.141592653589793238462643E+0,
*SP  &             TWO = 2.0E+0, ZERO = 0.0E+0)
*   Scalar arguments
*SP     REAL             ORD, X, EPS, XMAX, RELERR, FD
*   Local scalars
*SP     REAL             ETA, RJTERM, TERM, XNOFAC
*   External subroutines
*   Intrinsic functions
*SP     INTRINSIC ABS, REAL
* ----------------------------------------------------------------------
C     .. Parameters ..
      INTEGER ITMAX
      PARAMETER (ITMAX=100)
      DOUBLE PRECISION ONE,TWO,ZERO
      PARAMETER (ONE=1.0D+0,TWO=2.0D+0,ZERO=0.0D+0)
C     ..
C     .. Scalar Arguments ..
      DOUBLE PRECISION EPS,FD,ORD,RELERR,X,XMAX
      INTEGER IERR
C     ..
C     .. Local Scalars ..
      DOUBLE PRECISION ETA,RJTERM,TERM,XNOFAC
      INTEGER JTERM
      LOGICAL OKJM1,OKJM2
C     ..
C     .. External Subroutines ..
      EXTERNAL ETARIE,FERERR
C     ..
C     .. Intrinsic Functions ..
      INTRINSIC ABS,DBLE
C     ..
      IERR = 0
      FD = ZERO
*
      OKJM1 = .FALSE.
      OKJM2 = .FALSE.
      XNOFAC = ONE
      DO 10 JTERM = 1,ITMAX
*SP       RJTERM = REAL(JTERM)
          RJTERM = DBLE(JTERM)
          CALL ETARIE(ORD+TWO-RJTERM,EPS,XMAX,RELERR,ETA)
          TERM = ETA*XNOFAC
          FD = FD + TERM
*   Check truncation error and convergence.  The summation is terminated
*   when three consecutive terms of the series satisfy the bound on the
*   relative error
          IF (ABS(TERM).GT.ABS(FD)*RELERR) THEN
              OKJM1 = .FALSE.
              OKJM2 = .FALSE.

          ELSE IF (.NOT.OKJM1) THEN
              OKJM1 = .TRUE.

          ELSE IF (OKJM2) THEN
              RETURN

          ELSE
              OKJM2 = .TRUE.
          END IF

          XNOFAC = XNOFAC*X/RJTERM
   10 CONTINUE
      IERR = 4
      CALL FERERR(
     +         ' FDETA:  RELERR not achieved:  increase parameter ITMAX'
     +            )
      RETURN

      END

* **********************************************************************
*
      SUBROUTINE FDASYM(ORD,X,EPS,XMAX,XMIN,RELERR,FD,IERR)
*
* **********************************************************************
* FDASYM returns in FD the value of the Fermi-Dirac integral of real
*        order ORD and argument X .GT. 0, approximated with a relative
*        error RELERR by means of an asymptotic expansion.  EPS, XMAX
*        and XMIN represent the smallest positive floating-point number
*        such that 1.0+EPS .NE. 1.0, the largest finite floating-point
*        number, and the smallest non-vanishing floating-point number,
*        respectively.  A nonzero value is assigned to the error flag
*        IERR when the series does not converge.  The expansion always
*        terminates after a finite number of steps in case of integer
*        ORD.
*
* References:
*
*   [1] P. Rhodes, "Fermi-Dirac function of integral order", Proceedings
*       of the Royal Society of London. Series A - Mathematical and
*       Physical Sciences, vol. 204, pp. 396-405, 1950.
*
*   [2] R. B. Dingle, "Asymptotic Expansions: Their Derivation and
*       Interpretation", London and New York:  Academic Press, 1973.
*
* If a single precision version is desired, change all occurrences of
* *SP in columns 1 to 3 to blanks and comment the corresponding double
* precision statements.
*
* Michele Goano, Politecnico di Torino  (goano@polito.it).
* Latest revision:  March 22, 1994.
* **********************************************************************
*   Parameters
*SP     REAL             HALF, ONE, PI, TWO, ZERO
*SP     PARAMETER (HALF = 0.5E+0, ONE = 1.0E+0,
*SP  &             PI = 3.141592653589793238462643E+0, TWO = 2.0E+0,
*SP  &             ZERO = 0.0E+0)
*   Scalar arguments
*SP     REAL             ORD, X, EPS, XMAX, XMIN, RELERR, FD
*   Local scalars
*SP     REAL             ADD, ADDOLD, ETA, GAMMA, SEQN, XGAM, XM2
*   External subroutines
*   Intrinsic functions
*SP     INTRINSIC ABS, ANINT, COS, EXP, LOG, REAL
* ----------------------------------------------------------------------
C     .. Parameters ..
      INTEGER ITMAX
      PARAMETER (ITMAX=100)
      DOUBLE PRECISION HALF,ONE,PI,TWO,ZERO
      PARAMETER (HALF=0.5D+0,ONE=1.0D+0,
     +          PI=3.141592653589793238462643D+0,TWO=2.0D+0,ZERO=0.0D+0)
C     ..
C     .. Scalar Arguments ..
      DOUBLE PRECISION EPS,FD,ORD,RELERR,X,XMAX,XMIN
      INTEGER IERR
C     ..
C     .. Local Scalars ..
      DOUBLE PRECISION ADD,ADDOLD,ETA,GAMMA,SEQN,XGAM,XM2
      INTEGER N
      LOGICAL LOGGAM
C     ..
C     .. External Subroutines ..
      EXTERNAL ETAN,FDNEG,FERERR,GAMMAC
C     ..
C     .. Intrinsic Functions ..
      INTRINSIC ABS,ANINT,COS,DBLE,EXP,LOG
C     ..
      IERR = 0
      FD = ZERO
*
      CALL GAMMAC(ORD+TWO,EPS,XMAX,GAMMA,IERR)
      LOGGAM = .FALSE.
      IF (IERR.EQ.-1) THEN
          LOGGAM = .TRUE.
          IERR = 0
      END IF

      SEQN = HALF
      XM2 = X** (-2)
      XGAM = ONE
      ADD = XMAX
      DO 10 N = 1,ITMAX
          ADDOLD = ADD
*SP       XGAM = XGAM*XM2*(ORD + ONE - REAL(2*N-2))*
*SP  &                    (ORD + ONE - REAL(2*N-1))
          XGAM = XGAM*XM2* (ORD+ONE-DBLE(2*N-2))* (ORD+ONE-DBLE(2*N-1))
          CALL ETAN(2*N,ETA)
          ADD = ETA*XGAM
          IF (ABS(ADD).GE.ABS(ADDOLD) .AND.
     +        ABS(ORD-ANINT(ORD)).GT.ABS(ORD)*EPS) THEN
*   Asymptotic series is diverging
              IERR = 1
              RETURN

          END IF

          SEQN = SEQN + ADD
*   Check truncation error and convergence
          IF (ABS(ADD).LE.ABS(SEQN)*RELERR) GO TO 20
   10 CONTINUE
      IERR = 4
      CALL FERERR(
     +        ' FDASYM:  RELERR not achieved:  increase parameter ITMAX'
     +            )
   20 CONTINUE
      CALL FDNEG(ORD,-X,XMIN,RELERR,FD,IERR)
      IF (LOGGAM) THEN
          FD = COS(ORD*PI)*FD + TWO*SEQN*EXP((ORD+ONE)*LOG(X)-GAMMA)

      ELSE
          FD = COS(ORD*PI)*FD + X** (ORD+ONE)*TWO*SEQN/GAMMA
      END IF

      RETURN

      END

* **********************************************************************
*
      SUBROUTINE M1KUMM(A,X,EPS,XMAX,RELERR,M)
*
* **********************************************************************
* M1KUMM returns in M the value of Kummer's confluent hypergeometric
*        function M(1,2+A,-X), defined in (13.1.2) of [1], for real
*        arguments A and X, approximated with a relative error RELERR.
*        EPS and XMAX represent the smallest positive floating-point
*        number such that 1.0+EPS .NE. 1.0, and the largest finite
*        floating-point number, respectively.  Asymptotic expansion [1]
*        or continued fraction representation [2] is used.
*        Renormalization is carried out as proposed in [3].
*
* References:
*
*   [1] L. J. Slater, "Confluent Hypergeometric Functions", in "Handbook
*       of Mathematical Functions with Formulas, Graphs and Mathematical
*       Tables" (M. Abramowitz and I. A. Stegun, eds.), no. 55 in
*       National Bureau of Standards Applied Mathematics Series, ch. 13,
*       pp. 503-535, Washington, D.C.:  U.S. Government Printing Office,
*       1964.
*
*   [2] P. Henrici, "Applied and Computational Complex Analysis.
*       Volume 2.  Special Functions-Integral Transforms-Asymptotics-
*       Continued Fractions", New York:  John Wiley & Sons, 1977.
*
*   [3] W. H. Press, B. P. Flannery, S. A. Teukolsky, W. T. Vetterling,
*       "Numerical Recipes. The Art of Scientific Computing", Cambridge:
*       Cambridge University Press, 1986.
*
* If a single precision version is desired, change all occurrences of
* *SP in columns 1 to 3 to blanks and comment the corresponding double
* precision statements.
*
* Michele Goano, Politecnico di Torino  (goano@polito.it).
* Latest revision:  March 22, 1994.
* **********************************************************************
*   Parameters
*SP     REAL             ONE, PI, TEN, THREE, TWO, ZERO
*SP     PARAMETER (ONE = 1.0E+0, PI = 3.141592653589793238462643E+0,
*SP  &             TEN = 10.0E+0, THREE = 3.0E+0, TWO = 2.0E+0,
*SP  &             ZERO = 0.0E+0)
*   Scalar arguments
*SP     REAL             A, X, EPS, XMAX, RELERR, M
*   Local scalars
C       LOGICAL OKASYM
*SP     REAL             AA, ADD, ADDOLD, BB, FAC, GAMMA, GOLD, MLOG,
*SP  &                   P1, P2, Q1, Q2, RKDIV, RN, XASYMP, XBIG
*   External subroutines
*   Intrinsic functions
*SP     INTRINSIC ABS, COS, EXP, LOG, LOG10, REAL
* ----------------------------------------------------------------------
C     .. Parameters ..
      INTEGER ITMAX
      PARAMETER (ITMAX=100)
      DOUBLE PRECISION ONE,PI,TEN,THREE,TWO,ZERO
      PARAMETER (ONE=1.0D+0,PI=3.141592653589793238462643D+0,
     +          TEN=10.0D+0,THREE=3.0D+0,TWO=2.0D+0,ZERO=0.0D+0)
C     ..
C     .. Scalar Arguments ..
      DOUBLE PRECISION A,EPS,M,RELERR,X,XMAX
C     ..
C     .. Local Scalars ..
      DOUBLE PRECISION AA,ADD,ADDOLD,BB,FAC,GAMMA,GOLD,MLOG,P1,P2,Q1,Q2,
     +                 RKDIV,RN,XASYMP,XBIG
      INTEGER IERR,N
C     ..
C     .. External Subroutines ..
      EXTERNAL GAMMAC
C     ..
C     .. Intrinsic Functions ..
      INTRINSIC ABS,COS,DBLE,EXP,LOG,LOG10,MAX
C     ..
      XBIG = LOG(XMAX)
C       OKASYM = .TRUE.
      M = ZERO
*   Special cases
      IF (X.EQ.ZERO) THEN
          M = ONE

      ELSE
*   Approximations for k_div - 1 and x_min (RKDIV and XASYMP)
          RKDIV = -TWO/THREE*LOG10(RELERR)
          XASYMP = MAX(A-ONE,TWO+RKDIV-A* (TWO+RKDIV/TEN),ABS(RKDIV-A))
          IF (X.GT.XASYMP) THEN
*   Asymptotic expansion
              CALL GAMMAC(A+ONE,EPS,XMAX,GAMMA,IERR)
              IF (IERR.EQ.-1) THEN
*   Handling of the logarithm of the gamma function to avoid overflow
                  MLOG = GAMMA - X - A*LOG(X)
                  IF (MLOG.LT.XBIG) THEN
                      M = ONE - COS(PI*A)*EXP(MLOG)

                  ELSE
C               OKASYM = .FALSE.
                      GO TO 20

                  END IF

              ELSE
                  M = ONE - COS(PI*A)*GAMMA*EXP(-X)/X**A
              END IF

              ADDOLD = XMAX
              ADD = -A/X
              DO 10 N = 1,ITMAX
*   Divergence
                  IF (ABS(ADD).GE.ABS(ADDOLD)) THEN
C               OKASYM = .FALSE.
                      GO TO 20

                  END IF

                  M = M + ADD
*   Check truncation error and convergence
                  IF (ABS(ADD).LE.ABS(M)*RELERR) THEN
                      M = M* (A+ONE)/X
                      RETURN

                  END IF

                  ADDOLD = ADD
*SP           ADD = -ADD*(A - REAL(N))/X
                  ADD = -ADD* (A-DBLE(N))/X
   10         CONTINUE
          END IF
*   Continued fraction:  initial conditions
   20     CONTINUE
          GOLD = ZERO
          P1 = ONE
          Q1 = ONE
          P2 = A + TWO
          Q2 = X + A + TWO
          BB = A + TWO
*   Initial value of the normalization factor
          FAC = ONE
          DO 30 N = 1,ITMAX
*   Evaluation of a_(2N+1) and b_(2N+1)
*SP         RN = REAL(N)
              RN = DBLE(N)
              AA = -RN*X
              BB = BB + ONE
              P1 = (AA*P1+BB*P2)*FAC
              Q1 = (AA*Q1+BB*Q2)*FAC
*   Evaluation of a_(2N+2) and b_(2N+2)
              AA = (A+RN+ONE)*X
              BB = BB + ONE
              P2 = BB*P1 + AA*P2*FAC
              Q2 = BB*Q1 + AA*Q2*FAC
              IF (Q2.NE.ZERO) THEN
*   Renormalization and evaluation of w_(2N+2)
                  FAC = ONE/Q2
                  M = P2*FAC
*   Check truncation error and convergence
                  IF (ABS(M-GOLD).LT.ABS(M)*RELERR) RETURN
                  GOLD = M
              END IF

   30     CONTINUE
      END IF

      RETURN

      END

* **********************************************************************
*
      SUBROUTINE U1KUMM(A,X,EPS,XMAX,RELERR,U)
*
* **********************************************************************
* U1KUMM returns in U the value of Kummer's confluent hypergeometric
*        function U(1,1+A,X), defined in (13.1.3) of [1], for real
*        arguments A and X, approximated with a relative error RELERR.
*        EPS and XMAX represent the smallest positive floating-point
*        number such that 1.0+EPS .NE. 1.0, and the largest finite
*        floating-point number, respectively.  The relation with the
*        incomplete gamma function is exploited, by means of (13.6.28)
*        and (13.1.29) of [1].  For A .LE. 0 an expansion in terms of
*        Laguerre polynomials is used [3].  Otherwise the recipe of [4]
*        is followed: series expansion (6.5.29) of [2] if X .LT. A+1,
*        continued fraction (6.5.31) of [2] if X .GE. A+1.
*
* References:
*
*   [1] L. J. Slater, "Confluent Hypergeometric Functions", ch. 13 in
*       [5], pp. 503-535.
*
*   [2] P. J. Davis, "Gamma Function and Related Functions", ch. 6 in
*       [5], pp. 253-293.
*
*   [3] P. Henrici, "Computational Analysis with the HP-25 Pocket
*       Calculator", New York:  John Wiley & Sons, 1977.
*
*   [4] W. H. Press, B. P. Flannery, S. A. Teukolsky, W. T. Vetterling,
*       "Numerical Recipes. The Art of Scientific Computing", Cambridge:
*       Cambridge University Press, 1986.
*
*   [5] M. Abramowitz and I. A. Stegun (eds.), "Handbook of Mathematical
*       Functions with Formulas, Graphs and Mathematical Tables", no. 55
*       in National Bureau of Standards Applied Mathematics Series,
*       Washington, D.C.:  U.S. Government Printing Office, 1964.
*
* If a single precision version is desired, change all occurrences of
* *SP in columns 1 to 3 to blanks and comment the corresponding double
* precision statements.
*
* Michele Goano, Politecnico di Torino  (goano@polito.it).
* Latest revision:  March 22, 1994.
* **********************************************************************
*   Parameters
*SP     REAL             ONE, ZERO
*SP     PARAMETER (ONE = 1.0E+0, ZERO = 0.0E+0)
*   Scalar arguments
*SP     REAL             A, X, EPS, XMAX, RELERR, U
*   Local scalars
*SP     REAL             A0, A1, ANA, ANF, AP, B0, B1, DEL, FAC, G,
*SP  &                   GAMMA, GOLD, PLAGN, PLAGN1, PLAGN2, RN, T,
*SP  &                   ULOG, XBIG
*   External subroutines
*   Intrinsic functions
*SP     INTRINSIC ABS, EXP, LOG, REAL
* ----------------------------------------------------------------------
C       XBIG = LOG(XMAX)
C     .. Parameters ..
      INTEGER ITMAX
      PARAMETER (ITMAX=100)
      DOUBLE PRECISION ONE,ZERO
      PARAMETER (ONE=1.0D+0,ZERO=0.0D+0)
C     ..
C     .. Scalar Arguments ..
      DOUBLE PRECISION A,EPS,RELERR,U,X,XMAX
C     ..
C     .. Local Scalars ..
      DOUBLE PRECISION A0,A1,ANA,ANF,AP,B0,B1,DEL,FAC,G,GAMMA,GOLD,
     +                 PLAGN,PLAGN1,PLAGN2,RN,T,ULOG
      INTEGER IERR,N
      LOGICAL LOGGAM
C     ..
C     .. External Subroutines ..
      EXTERNAL GAMMAC
C     ..
C     .. Intrinsic Functions ..
      INTRINSIC ABS,DBLE,EXP,LOG
C     ..
      U = ZERO
*   Special cases
      IF (X.EQ.ZERO) THEN
          U = -ONE/A
*   Laguerre polynomials
      ELSE IF (A.LE.ZERO) THEN
          U = ZERO
          PLAGN2 = ZERO
          PLAGN1 = ONE
          G = ONE
          DO 10 N = 1,ITMAX
*SP         RN = REAL(N)
              RN = DBLE(N)
              PLAGN = ((RN-A-ONE)* (PLAGN1-PLAGN2)+ (RN+X)*PLAGN1)/RN
              T = G/ (PLAGN1*PLAGN)
              U = U + T
              IF (ABS(T).LT.ABS(U)*RELERR) RETURN
              G = G* (RN-A)/ (RN+ONE)
              PLAGN2 = PLAGN1
              PLAGN1 = PLAGN
   10     CONTINUE
*   Series expansion
      ELSE IF (X.LT.A+ONE) THEN
          CALL GAMMAC(A,EPS,XMAX,GAMMA,IERR)
          LOGGAM = .FALSE.
          IF (IERR.EQ.-1) LOGGAM = .TRUE.
          AP = A
          U = ONE/A
          DEL = U
          DO 20 N = 1,ITMAX
              AP = AP + ONE
              DEL = DEL*X/AP
              U = U + DEL
              IF (ABS(DEL).LT.ABS(U)*RELERR) THEN
                  IF (LOGGAM) THEN
                      ULOG = GAMMA + X + A*LOG(X)
                      U = EXP(ULOG) - U

                  ELSE
                      U = GAMMA*EXP(X)/X**A - U
                  END IF

                  RETURN

              END IF

   20     CONTINUE
*   Continued fraction
      ELSE
          GOLD = ZERO
          A0 = ONE
          A1 = X
          B0 = ZERO
          B1 = ONE
          FAC = ONE
          DO 30 N = 1,ITMAX
*SP         RN = REAL(N)
              RN = DBLE(N)
              ANA = RN - A
              A0 = (A1+A0*ANA)*FAC
              B0 = (B1+B0*ANA)*FAC
              ANF = RN*FAC
              A1 = X*A0 + ANF*A1
              B1 = X*B0 + ANF*B1
              IF (A1.NE.ZERO) THEN
                  FAC = ONE/A1
                  U = B1*FAC
                  IF (ABS(U-GOLD).LT.ABS(U)*RELERR) RETURN
                  GOLD = U
              END IF

   30     CONTINUE
      END IF

      RETURN

      END

* **********************************************************************
*
      SUBROUTINE ETARIE(S,EPS,XMAX,RELERR,ETA)
*
* **********************************************************************
* ETARIE returns in ETA the value of the eta function, for real argument
*        S, approximated with a relative error RELERR.  EPS and XMAX
*        represent the smallest positive floating-point number such that
*        1.0+EPS .NE. 1.0, and the largest finite floating-point number,
*        respectively.  For S .GT. -1 Levin's u transform [2] is used to
*        sum the alternating series (23.2.19) of [1], except when S is a
*        positive integer.  Otherwise the reflection formula (23.2.6) of
*        [1] is employed, involving gamma function evaluation, except in
*        the trivial zeros S = -2N.
*
* References:
*
*   [1] E. V. Haynsworth and K. Goldberg, "Bernoulli and Euler
*       Polynomials - Riemann Zeta Function", in "Handbook of
*       Mathematical Functions with Formulas, Graphs and Mathematical
*       Tables" (M. Abramowitz and I. A. Stegun, eds.), no. 55 in
*       National Bureau of Standards Applied Mathematics Series, ch. 23,
*       pp. 803-819, Washington, D.C.:  U.S. Government Printing Office,
*       1964.
*
*   [2] T. Fessler, W. F. Ford, D. A. Smith, "ALGORITHM 602. HURRY: An
*       acceleration algorithm for scalar sequences and series", ACM
*       Transactions on Mathematical Software, vol. 9, no. 3,
*       pp. 355-357, September  1983.
*
* If a single precision version is desired, change all occurrences of
* *SP in columns 1 to 3 to blanks and comment the corresponding double
* precision statements.
*
* Michele Goano, Politecnico di Torino  (goano@polito.it).
* Latest revision:  March 22, 1994.
* **********************************************************************
*   Parameters
*SP     REAL             ONE, PI, PILOG, TWO, ZERO
*SP     PARAMETER (ONE = 1.0E+0, PI = 3.141592653589793238462643E+0,
*SP  &             PILOG = 1.144729885849400174143427E+0, TWO = 2.0E+0,
*SP  &             ZERO = 0.0E+0)
*   Scalar arguments
*SP     REAL             S, EPS, XMAX, RELERR, ETA
*   Local scalars
*SP     REAL             ETALOG, GAMMA, TWOTOS
*   External subroutines
*   Intrinsic functions
* ----------------------------------------------------------------------
C       XBIG = LOG(XMAX)
C     .. Parameters ..
      DOUBLE PRECISION ONE,PI,PILOG,TWO,ZERO
      PARAMETER (ONE=1.0D+0,PI=3.141592653589793238462643D+0,
     +          PILOG=1.144729885849400174143427D+0,TWO=2.0D+0,
     +          ZERO=0.0D+0)
C     ..
C     .. Scalar Arguments ..
      DOUBLE PRECISION EPS,ETA,RELERR,S,XMAX
C     ..
C     .. Local Scalars ..
      DOUBLE PRECISION ETALOG,GAMMA,TWOTOS
      INTEGER IERR
      LOGICAL LOGGAM
C     ..
C     .. External Subroutines ..
      EXTERNAL ETALEV,ETAN,GAMMAC
C     ..
C     .. Intrinsic Functions ..
      INTRINSIC ABS,ANINT,EXP,LOG,MOD,NINT,SIN
C     ..
      ETA = ZERO
*
      IF (S.EQ.ZERO) THEN
          ETA = ONE/TWO

      ELSE IF (S.LT.ZERO .AND. MOD(S,TWO).EQ.ZERO) THEN
          ETA = ZERO

      ELSE IF (S.GT.-ONE) THEN
          IF (ABS(S-ANINT(S)).LE.ABS(S)*EPS) THEN
              CALL ETAN(NINT(S),ETA)

          ELSE
              CALL ETALEV(S,RELERR,ETA)
          END IF

      ELSE
          TWOTOS = TWO**S
          CALL GAMMAC(ONE-S,EPS,XMAX,GAMMA,IERR)
          LOGGAM = .FALSE.
          IF (IERR.EQ.-1) LOGGAM = .TRUE.
          CALL ETALEV(ONE-S,RELERR,ETA)
          IF (LOGGAM) THEN
              ETALOG = (S-ONE)*PILOG + GAMMA + LOG(ETA)
              ETA = (TWOTOS-TWO)/ (ONE-TWOTOS)*SIN(S*PI/TWO)*EXP(ETALOG)

          ELSE
              ETA = (TWOTOS-TWO)/ (ONE-TWOTOS)*SIN(S*PI/TWO)*
     +              PI** (S-ONE)*GAMMA*ETA
          END IF

      END IF

      RETURN

      END

* **********************************************************************
*
      SUBROUTINE ETALEV(S,RELERR,ETA)
*
* **********************************************************************
* ETALEV returns in ETA the value of the eta function, for real argument
*        S, approximated with a relative error RELERR.  Levin's u
*        transform [2] is used to sum the alternating series (23.2.19)
*        of [1].
*
* References:
*
*   [1] E. V. Haynsworth and K. Goldberg, "Bernoulli and Euler
*       Polynomials - Riemann Zeta Function", in "Handbook of
*       Mathematical Functions with Formulas, Graphs and Mathematical
*       Tables" (M. Abramowitz and I. A. Stegun, eds.), no. 55 in
*       National Bureau of Standards Applied Mathematics Series, ch. 23,
*       pp. 803-819, Washington, D.C.:  U.S. Government Printing Office,
*       1964.
*
*   [2] T. Fessler, W. F. Ford, D. A. Smith, "ALGORITHM 602. HURRY: An
*       acceleration algorithm for scalar sequences and series", ACM
*       Transactions on Mathematical Software, vol. 9, no. 3,
*       pp. 355-357, September  1983.
*
* If a single precision version is desired, change all occurrences of
* *SP in columns 1 to 3 to blanks and comment the corresponding double
* precision statements.
*
* Michele Goano, Politecnico di Torino  (goano@polito.it).
* Latest revision:  March 22, 1994.
* **********************************************************************
*   Parameters
*SP     REAL             ONE, ZERO
*SP     PARAMETER (ONE = 1.0E+0, ZERO = 0.0E+0)
*   Scalar arguments
*SP     REAL             S, RELERR, ETA
*   Local scalars
*SP     REAL             ETAOLD, SEGN, SUM, TERM
*   Local arrays
*SP     REAL             QNUM(ITMAX), QDEN(ITMAX)
*   External subroutines
*   Intrinsic functions
*SP     INTRINSIC ABS, REAL
* ----------------------------------------------------------------------
C     .. Parameters ..
      INTEGER ITMAX
      PARAMETER (ITMAX=100)
      DOUBLE PRECISION ONE,ZERO
      PARAMETER (ONE=1.0D+0,ZERO=0.0D+0)
C     ..
C     .. Scalar Arguments ..
      DOUBLE PRECISION ETA,RELERR,S
C     ..
C     .. Local Scalars ..
      DOUBLE PRECISION ETAOLD,SEGN,SUM,TERM
      INTEGER JTERM
C     ..
C     .. Local Arrays ..
      DOUBLE PRECISION QDEN(ITMAX),QNUM(ITMAX)
C     ..
C     .. External Subroutines ..
      EXTERNAL WHIZ
C     ..
C     .. Intrinsic Functions ..
      INTRINSIC ABS,DBLE
C     ..
      ETA = ZERO
*
      SEGN = ONE
      DO 10 JTERM = 1,ITMAX
          ETAOLD = ETA
*SP       TERM = SEGN/REAL(JTERM)**S
          TERM = SEGN/DBLE(JTERM)**S
          CALL WHIZ(TERM,JTERM,QNUM,QDEN,ETA,SUM)
*   Check truncation error and convergence
          IF (ABS(ETA-ETAOLD).LE.ABS(ETA)*RELERR) RETURN
          SEGN = -SEGN
   10 CONTINUE
      END

* **********************************************************************
*
      SUBROUTINE ETAN(N,ETA)
*
* **********************************************************************
* ETAN returns in ETA the value of the eta function for integer
*      nonnegative argument N, approximated to 25 significant decimal
*      digits.
*
* Reference:
*
*   E. V. Haynsworth and K. Goldberg, "Bernoulli and Euler Polynomials -
*   Riemann Zeta Function", in "Handbook of Mathematical Functions with
*   Formulas, Graphs and Mathematical Tables" (M. Abramowitz and
*   I. A. Stegun, eds.), no. 55 in National Bureau of Standards Applied
*   Mathematics Series, ch. 23, pp. 803-819, Washington, D.C.:  U.S.
*   Government Printing Office, 1964.
*
* If a single precision version is desired, change all occurrences of
* *SP in columns 1 to 3 to blanks and comment the corresponding double
* precision statements.
*
* Michele Goano, Politecnico di Torino  (goano@polito.it).
* Latest revision:  March 22, 1994.
* **********************************************************************
*   Parameters
*SP     REAL             HALF, ONE, ZERO
*SP     PARAMETER (HALF = 0.5E+0, ONE = 1.0E+0, ZERO = 0.0E+0)
*   Scalar arguments
*SP     REAL             ETA
*   Local arrays
*SP     REAL             ETABLE(84)
* ----------------------------------------------------------------------
C     .. Parameters ..
      DOUBLE PRECISION HALF,ONE,ZERO
      PARAMETER (HALF=0.5D+0,ONE=1.0D+0,ZERO=0.0D+0)
C     ..
C     .. Scalar Arguments ..
      DOUBLE PRECISION ETA
      INTEGER N
C     ..
C     .. Local Arrays ..
      DOUBLE PRECISION ETABLE(84)
C     ..
C     .. Save statement ..
      SAVE ETABLE
C     ..
C     .. Data statements ..
*SP     DATA ETABLE(1),  ETABLE(2),  ETABLE(3),  ETABLE(4),
*SP  &       ETABLE(5),  ETABLE(6),  ETABLE(7),  ETABLE(8),
*SP  &       ETABLE(9),  ETABLE(10), ETABLE(11), ETABLE(12),
*SP  &       ETABLE(13), ETABLE(14), ETABLE(15), ETABLE(16) /
*SP  &  0.6931471805599453094172321E+0, 0.8224670334241132182362076E+0,
*SP  &  0.9015426773696957140498036E+0, 0.9470328294972459175765032E+0,
*SP  &  0.9721197704469093059356551E+0, 0.9855510912974351040984392E+0,
*SP  &  0.9925938199228302826704257E+0, 0.9962330018526478992272893E+0,
*SP  &  0.9980942975416053307677830E+0, 0.9990395075982715656392218E+0,
*SP  &  0.9995171434980607541440942E+0, 0.9997576851438581908531797E+0,
*SP  &  0.9998785427632651154921750E+0, 0.9999391703459797181709542E+0,
*SP  &  0.9999695512130992380826329E+0, 0.9999847642149061064416828E+0 /
*SP     DATA ETABLE(17), ETABLE(18), ETABLE(19), ETABLE(20),
*SP  &       ETABLE(21), ETABLE(22), ETABLE(23), ETABLE(24),
*SP  &       ETABLE(25), ETABLE(26), ETABLE(27), ETABLE(28),
*SP  &       ETABLE(29), ETABLE(30), ETABLE(31), ETABLE(32) /
*SP  &  0.9999923782920410119769379E+0, 0.9999961878696101134796892E+0,
*SP  &  0.9999980935081716751068565E+0, 0.9999990466115815221150508E+0,
*SP  &  0.9999995232582155428163167E+0, 0.9999997616132308225478972E+0,
*SP  &  0.9999998808013184395032238E+0, 0.9999999403988923946283614E+0,
*SP  &  0.9999999701988569628344151E+0, 0.9999999850992319965687877E+0,
*SP  &  0.9999999925495504849635159E+0, 0.9999999962747534001087275E+0,
*SP  &  0.9999999981373694181121867E+0, 0.9999999990686822814539786E+0,
*SP  &  0.9999999995343403314542175E+0, 0.9999999997671698959514908E+0 /
*SP     DATA ETABLE(33), ETABLE(34), ETABLE(35), ETABLE(36),
*SP  &       ETABLE(37), ETABLE(38), ETABLE(39), ETABLE(40),
*SP  &       ETABLE(41), ETABLE(42), ETABLE(43), ETABLE(44),
*SP  &       ETABLE(45), ETABLE(46), ETABLE(47), ETABLE(48) /
*SP  &  0.9999999998835848580460305E+0, 0.9999999999417923990453159E+0,
*SP  &  0.9999999999708961895298095E+0, 0.9999999999854480914338848E+0,
*SP  &  0.9999999999927240446065848E+0, 0.9999999999963620219331688E+0,
*SP  &  0.9999999999981810108432087E+0, 0.9999999999990905053804789E+0,
*SP  &  0.9999999999995452526765309E+0, 0.9999999999997726263336959E+0,
*SP  &  0.9999999999998863131653248E+0, 0.9999999999999431565821547E+0,
*SP  &  0.9999999999999715782909081E+0, 0.9999999999999857891453976E+0,
*SP  &  0.9999999999999928945726800E+0, 0.9999999999999964472863337E+0 /
*SP     DATA ETABLE(49), ETABLE(50), ETABLE(51), ETABLE(52),
*SP  &       ETABLE(53), ETABLE(54), ETABLE(55), ETABLE(56),
*SP  &       ETABLE(57), ETABLE(58), ETABLE(59), ETABLE(60),
*SP  &       ETABLE(61), ETABLE(62), ETABLE(63), ETABLE(64) /
*SP  &  0.9999999999999982236431648E+0, 0.9999999999999991118215817E+0,
*SP  &  0.9999999999999995559107906E+0, 0.9999999999999997779553952E+0,
*SP  &  0.9999999999999998889776976E+0, 0.9999999999999999444888488E+0,
*SP  &  0.9999999999999999722444244E+0, 0.9999999999999999861222122E+0,
*SP  &  0.9999999999999999930611061E+0, 0.9999999999999999965305530E+0,
*SP  &  0.9999999999999999982652765E+0, 0.9999999999999999991326383E+0,
*SP  &  0.9999999999999999995663191E+0, 0.9999999999999999997831596E+0,
*SP  &  0.9999999999999999998915798E+0, 0.9999999999999999999457899E+0 /
*SP     DATA ETABLE(65), ETABLE(66), ETABLE(67), ETABLE(68),
*SP  &       ETABLE(69), ETABLE(70), ETABLE(71), ETABLE(72),
*SP  &       ETABLE(73), ETABLE(74), ETABLE(75), ETABLE(76),
*SP  &       ETABLE(77), ETABLE(78), ETABLE(79), ETABLE(80) /
*SP  &  0.9999999999999999999728949E+0, 0.9999999999999999999864475E+0,
*SP  &  0.9999999999999999999932237E+0, 0.9999999999999999999966119E+0,
*SP  &  0.9999999999999999999983059E+0, 0.9999999999999999999991530E+0,
*SP  &  0.9999999999999999999995765E+0, 0.9999999999999999999997882E+0,
*SP  &  0.9999999999999999999998941E+0, 0.9999999999999999999999471E+0,
*SP  &  0.9999999999999999999999735E+0, 0.9999999999999999999999868E+0,
*SP  &  0.9999999999999999999999934E+0, 0.9999999999999999999999967E+0,
*SP  &  0.9999999999999999999999983E+0, 0.9999999999999999999999992E+0 /
*SP     DATA ETABLE(81), ETABLE(82), ETABLE(83), ETABLE(84) /
*SP  &  0.9999999999999999999999996E+0, 0.9999999999999999999999998E+0,
*SP  &  0.9999999999999999999999999E+0, 0.9999999999999999999999999E+0 /
      DATA ETABLE(1),ETABLE(2),ETABLE(3),ETABLE(4),ETABLE(5),ETABLE(6),
     +     ETABLE(7),ETABLE(8),ETABLE(9),ETABLE(10),ETABLE(11),
     +     ETABLE(12),ETABLE(13),ETABLE(14),ETABLE(15),
     +     ETABLE(16)/0.6931471805599453094172321D+0,
     +     0.8224670334241132182362076D+0,
     +     0.9015426773696957140498036D+0,
     +     0.9470328294972459175765032D+0,
     +     0.9721197704469093059356551D+0,
     +     0.9855510912974351040984392D+0,
     +     0.9925938199228302826704257D+0,
     +     0.9962330018526478992272893D+0,
     +     0.9980942975416053307677830D+0,
     +     0.9990395075982715656392218D+0,
     +     0.9995171434980607541440942D+0,
     +     0.9997576851438581908531797D+0,
     +     0.9998785427632651154921750D+0,
     +     0.9999391703459797181709542D+0,
     +     0.9999695512130992380826329D+0,
     +     0.9999847642149061064416828D+0/
      DATA ETABLE(17),ETABLE(18),ETABLE(19),ETABLE(20),ETABLE(21),
     +     ETABLE(22),ETABLE(23),ETABLE(24),ETABLE(25),ETABLE(26),
     +     ETABLE(27),ETABLE(28),ETABLE(29),ETABLE(30),ETABLE(31),
     +     ETABLE(32)/0.9999923782920410119769379D+0,
     +     0.9999961878696101134796892D+0,
     +     0.9999980935081716751068565D+0,
     +     0.9999990466115815221150508D+0,
     +     0.9999995232582155428163167D+0,
     +     0.9999997616132308225478972D+0,
     +     0.9999998808013184395032238D+0,
     +     0.9999999403988923946283614D+0,
     +     0.9999999701988569628344151D+0,
     +     0.9999999850992319965687877D+0,
     +     0.9999999925495504849635159D+0,
     +     0.9999999962747534001087275D+0,
     +     0.9999999981373694181121867D+0,
     +     0.9999999990686822814539786D+0,
     +     0.9999999995343403314542175D+0,
     +     0.9999999997671698959514908D+0/
      DATA ETABLE(33),ETABLE(34),ETABLE(35),ETABLE(36),ETABLE(37),
     +     ETABLE(38),ETABLE(39),ETABLE(40),ETABLE(41),ETABLE(42),
     +     ETABLE(43),ETABLE(44),ETABLE(45),ETABLE(46),ETABLE(47),
     +     ETABLE(48)/0.9999999998835848580460305D+0,
     +     0.9999999999417923990453159D+0,
     +     0.9999999999708961895298095D+0,
     +     0.9999999999854480914338848D+0,
     +     0.9999999999927240446065848D+0,
     +     0.9999999999963620219331688D+0,
     +     0.9999999999981810108432087D+0,
     +     0.9999999999990905053804789D+0,
     +     0.9999999999995452526765309D+0,
     +     0.9999999999997726263336959D+0,
     +     0.9999999999998863131653248D+0,
     +     0.9999999999999431565821547D+0,
     +     0.9999999999999715782909081D+0,
     +     0.9999999999999857891453976D+0,
     +     0.9999999999999928945726800D+0,
     +     0.9999999999999964472863337D+0/
      DATA ETABLE(49),ETABLE(50),ETABLE(51),ETABLE(52),ETABLE(53),
     +     ETABLE(54),ETABLE(55),ETABLE(56),ETABLE(57),ETABLE(58),
     +     ETABLE(59),ETABLE(60),ETABLE(61),ETABLE(62),ETABLE(63),
     +     ETABLE(64)/0.9999999999999982236431648D+0,
     +     0.9999999999999991118215817D+0,
     +     0.9999999999999995559107906D+0,
     +     0.9999999999999997779553952D+0,
     +     0.9999999999999998889776976D+0,
     +     0.9999999999999999444888488D+0,
     +     0.9999999999999999722444244D+0,
     +     0.9999999999999999861222122D+0,
     +     0.9999999999999999930611061D+0,
     +     0.9999999999999999965305530D+0,
     +     0.9999999999999999982652765D+0,
     +     0.9999999999999999991326383D+0,
     +     0.9999999999999999995663191D+0,
     +     0.9999999999999999997831596D+0,
     +     0.9999999999999999998915798D+0,
     +     0.9999999999999999999457899D+0/
      DATA ETABLE(65),ETABLE(66),ETABLE(67),ETABLE(68),ETABLE(69),
     +     ETABLE(70),ETABLE(71),ETABLE(72),ETABLE(73),ETABLE(74),
     +     ETABLE(75),ETABLE(76),ETABLE(77),ETABLE(78),ETABLE(79),
     +     ETABLE(80)/0.9999999999999999999728949D+0,
     +     0.9999999999999999999864475D+0,
     +     0.9999999999999999999932237D+0,
     +     0.9999999999999999999966119D+0,
     +     0.9999999999999999999983059D+0,
     +     0.9999999999999999999991530D+0,
     +     0.9999999999999999999995765D+0,
     +     0.9999999999999999999997882D+0,
     +     0.9999999999999999999998941D+0,
     +     0.9999999999999999999999471D+0,
     +     0.9999999999999999999999735D+0,
     +     0.9999999999999999999999868D+0,
     +     0.9999999999999999999999934D+0,
     +     0.9999999999999999999999967D+0,
     +     0.9999999999999999999999983D+0,
     +     0.9999999999999999999999992D+0/
      DATA ETABLE(81),ETABLE(82),ETABLE(83),
     +     ETABLE(84)/0.9999999999999999999999996D+0,
     +     0.9999999999999999999999998D+0,
     +     0.9999999999999999999999999D+0,
     +     0.9999999999999999999999999D+0/
C     ..
* ----------------------------------------------------------------------
      ETA = ZERO
      IF (N.EQ.0) THEN
          ETA = HALF

      ELSE IF (N.LE.84) THEN
          ETA = ETABLE(N)

      ELSE IF (N.GT.84) THEN
          ETA = ONE
      END IF

      RETURN

      END

* **********************************************************************
*
      SUBROUTINE FERERR(ERRMSG)
*
* **********************************************************************
* FERERR prints on the standard output unit an explanatory message of
*        the error condition occured in the package which approximates
*        the complete and incomplete Fermi-Dirac integral.
*
* Michele Goano, Politecnico di Torino  (goano@polito.it).
* Latest revision:  March 22, 1994.
* **********************************************************************
*   Scalar arguments
* ----------------------------------------------------------------------
C     .. Scalar Arguments ..
      CHARACTER*(*) ERRMSG
C     ..
      WRITE (*,FMT=9000) ERRMSG
*   If you want to interrupt the execution after an error has occurred,
*   replace the RETURN statement with a STOP
      RETURN

 9000 FORMAT (A)
      END
*
* **********************************************************************
*
      SUBROUTINE GAMMAC(X,EPS,XINF,GAMMA,IERR)
C-----------------------------------------------------------------------
C This routine calculates the gamma function for a real argument X.  The
C logarithm of the gamma function is computed, and the error flag IERR
C is set to -1, whenever the result would be too large to be represented
C on the floating-point arithmetic system.  Computation is based on an
C algorithm outlined in W. J. Cody, 'An overview of software development
C for special functions', Lecture Notes in Mathematics, 506, Numerical
C Analysis Dundee, 1975, G. A. Watson (ed.), Springer Verlag, Berlin,
C 1976.  The program uses rational functions that approximate the gamma
C function to at least 20 significant decimal digits.  Coefficients for
C the approximation over the interval (1,2) are unpublished.  Those for
C the approximation for X .GE. 12 are from Hart et al., Computer
C Approximations, Wiley and Sons, New York, 1968.
C
C If a single precision version is desired, change all occurrences of CS
C in columns 1 and 2 to blanks and comment the corresponding double
C precision statements.
C
C Explanation of machine-dependent variables
C
C EPS    - the smallest positive floating-point number such that
C          1.0 + EPS .GT. 1.0
C XINF   - the largest machine representable floating-point number.
C XBIG   - the largest floating-point number such that EXP(XBIG) is
C          machine representable.
C
C Error returns
C
C  The program returns LOG(GAMMA) and sets IERR = -1 when overflow would
C  occur.
C
C Author: W. J. Cody
C         Argonne National Laboratory
C
C Revised by M. Goano, Politecnico di Torino, to take advantage of
C Fortran 77 control structures.
C
C Latest modification of the original version: May 18, 1982
C                     of the revised version:  March 21, 1994
C-----------------------------------------------------------------------
CS      REAL             C, EPS, FACT, GAMMA, HALF, ONE, P, PI, Q, RES,
CS   &                   SQRTPI, SUM, TWELVE, X, XBIG, XDEN, XINF,
CS   &                   XNUM, Y, Y1, YSQ, Z, ZERO
CS      INTRINSIC ALOG, EXP, FLOAT, IFIX, SIN
C-----------------------------------------------------------------------
C Mathematical constants
C-----------------------------------------------------------------------
CS      PARAMETER (ONE = 1.0E+0, HALF = 0.5E+0, TWELVE = 12.0E+0,
CS   &             ZERO = 0.0E+0, PI = 3.1415926535897932384626434E+0,
CS   &             SQRTPI = 0.9189385332046727417803297E+0)
C-----------------------------------------------------------------------
C SAVE declaration for the arrays of the coefficients
C-----------------------------------------------------------------------
C     .. Parameters ..
      DOUBLE PRECISION ONE,HALF,TWELVE,ZERO,PI,SQRTPI
      PARAMETER (ONE=1.0D+0,HALF=0.5D+0,TWELVE=12.0D+0,ZERO=0.0D+0,
     +          PI=3.1415926535897932384626434D+0,
     +          SQRTPI=0.9189385332046727417803297D+0)
C     ..
C     .. Scalar Arguments ..
      DOUBLE PRECISION EPS,GAMMA,X,XINF
      INTEGER IERR
C     ..
C     .. Local Scalars ..
      DOUBLE PRECISION FACT,RES,SUM,XBIG,XDEN,XNUM,Y,Y1,YSQ,Z
      INTEGER I,J,N
      LOGICAL PARITY
C     ..
C     .. Local Arrays ..
      DOUBLE PRECISION C(7),P(8),Q(8)
C     ..
C     .. Intrinsic Functions ..
      INTRINSIC DBLE,DEXP,DLOG,DSIN,FLOAT,IFIX,SNGL
C     ..
C     .. Save statement ..
      SAVE C,P,Q
C     ..
C     .. Data statements ..
C-----------------------------------------------------------------------
C Numerator and denominator coefficients for rational minimax
C approximation over (1,2)
C-----------------------------------------------------------------------
CS      DATA P /-1.71618513886549492533811E+0,
CS   &           2.47656508055759199108314E+1,
CS   &          -3.79804256470945635097577E+2,
CS   &           6.29331155312818442661052E+2,
CS   &           8.66966202790413211295064E+2,
CS   &          -3.14512729688483675254357E+4,
CS   &          -3.61444134186911729807069E+4,
CS   &           6.64561438202405440627855E+4/
CS      DATA Q /-3.08402300119738975254353E+1,
CS   &           3.15350626979604161529144E+2,
CS   &          -1.01515636749021914166146E+3,
CS   &          -3.10777167157231109440444E+3,
CS   &           2.25381184209801510330112E+4,
CS   &           4.75584627752788110767815E+3,
CS   &          -1.34659959864969306392456E+5,
CS   &          -1.15132259675553483497211E+5/
C-----------------------------------------------------------------------
C Coefficients for minimax approximation over (12, INF)
C-----------------------------------------------------------------------
CS      DATA C /-1.910444077728E-03,
CS   &           8.4171387781295E-04,
CS   &          -5.952379913043012E-04,
CS   &           7.93650793500350248E-04,
CS   &          -2.777777777777681622553E-03,
CS   &           8.333333333333333331554247E-02,
CS   &           5.7083835261E-03/
      DATA P/-1.71618513886549492533811D+0,2.47656508055759199108314D+1,
     +     -3.79804256470945635097577D+2,6.29331155312818442661052D+2,
     +     8.66966202790413211295064D+2,-3.14512729688483675254357D+4,
     +     -3.61444134186911729807069D+4,6.64561438202405440627855D+4/
      DATA Q/-3.08402300119738975254353D+1,3.15350626979604161529144D+2,
     +     -1.01515636749021914166146D+3,-3.10777167157231109440444D+3,
     +     2.25381184209801510330112D+4,4.75584627752788110767815D+3,
     +     -1.34659959864969306392456D+5,-1.15132259675553483497211D+5/
      DATA C/-1.910444077728D-03,8.4171387781295D-04,
     +     -5.952379913043012D-04,7.93650793500350248D-04,
     +     -2.777777777777681622553D-03,8.333333333333333331554247D-02,
     +     5.7083835261D-03/
C     ..
C-----------------------------------------------------------------------
C Machine dependent local variables
C-----------------------------------------------------------------------
CS      XBIG = ALOG(XINF)
      XBIG = DLOG(XINF)
C-----------------------------------------------------------------------
      IERR = 0
      PARITY = .FALSE.
      FACT = ONE
      N = 0
      Y = X
      IF (Y.LE.ZERO) THEN
C-----------------------------------------------------------------------
C Argument is negative
C-----------------------------------------------------------------------
          Y = -X
CS        J = IFIX(Y)
          J = IFIX(SNGL(Y))
CS        RES = Y - FLOAT(J)
          RES = Y - DBLE(FLOAT(J))
          IF (J.NE. (J/2)*2) PARITY = .TRUE.
CS        FACT = -PI/SIN(PI*RES)
          FACT = -PI/DSIN(PI*RES)
          Y = Y + ONE
      END IF
C-----------------------------------------------------------------------
C Argument is positive
C-----------------------------------------------------------------------
      IF (Y.LT.EPS) THEN
C-----------------------------------------------------------------------
C Argument .LT. EPS
C-----------------------------------------------------------------------
          RES = ONE/Y

      ELSE IF (Y.GE.TWELVE) THEN
C-----------------------------------------------------------------------
C Evaluate for argument .GE. 12.0
C-----------------------------------------------------------------------
          YSQ = Y*Y
          SUM = C(7)
          DO 10 I = 1,6
              SUM = SUM/YSQ + C(I)
   10     CONTINUE
CS        SUM = SUM/Y + (Y - HALF)*ALOG(Y) - Y + SQRTPI
          SUM = SUM/Y + (Y-HALF)*DLOG(Y) - Y + SQRTPI
          IF (SUM.GT.XBIG) THEN
C-----------------------------------------------------------------------
C Return the logarithm to avoid overflow
C-----------------------------------------------------------------------
              RES = SUM
              IERR = -1

          ELSE
CS          RES = EXP(SUM)
              RES = DEXP(SUM)
          END IF

      ELSE
          Y1 = Y
          IF (Y.GE.ONE) THEN
C-----------------------------------------------------------------------
C 1.0 .LT. argument .LT. 12.0, reduce argument if necessary
C-----------------------------------------------------------------------
CS          N = IFIX(Y) - 1
              N = IFIX(SNGL(Y)) - 1
CS          Y = Y - FLOAT(N)
              Y = Y - DBLE(FLOAT(N))
              Z = Y - ONE

          ELSE
C-----------------------------------------------------------------------
C 0.0 .LT. argument .LT. 1.0
C-----------------------------------------------------------------------
              Z = Y
              Y = Y + ONE
          END IF
C-----------------------------------------------------------------------
C Evaluate approximation for 1.0 .LT. argument .LT. 2.0
C-----------------------------------------------------------------------
          XNUM = ZERO
          XDEN = ONE
          DO 20 I = 1,8
              XNUM = (XNUM+P(I))*Z
              XDEN = XDEN*Z + Q(I)
   20     CONTINUE
          RES = XNUM/XDEN + ONE
          IF (Y.NE.Y1) THEN
              IF (Y1.GT.Y) THEN
C-----------------------------------------------------------------------
C Adjust result for case  2.0 .LT. argument .LT. 12.0
C-----------------------------------------------------------------------
                  DO 30 I = 1,N
                      RES = RES*Y
                      Y = Y + ONE
   30             CONTINUE

              ELSE
C-----------------------------------------------------------------------
C Adjust result for case  0.0 .LT. argument .LT. 1.0
C-----------------------------------------------------------------------
                  RES = RES/Y1
              END IF

          END IF

      END IF
C-----------------------------------------------------------------------
C Final adjustments and return
C-----------------------------------------------------------------------
      IF (PARITY) RES = -RES
      IF (FACT.NE.ONE) RES = FACT/RES
      GAMMA = RES
C  40   CONTINUE
      RETURN

      END
*
* **********************************************************************
*
      SUBROUTINE WHIZ(TERM,ITERM,QNUM,QDEN,RESULT,S)
************************************************************************
*     ALGORITHM 602, COLLECTED ALGORITHMS FROM ACM.
*     ALGORITHM APPEARED IN ACM-TRANS. MATH. SOFTWARE, VOL.9, NO. 3,
*     SEP., 1983, P. 355-357.
*
* The u algorithm for accelerating a series.
*
* Arguments:
*    TERM   = last element of series
*    ITERM   = order of TERM in the series = number of calls to WHIZ
*    QNUM   = backward diagonal of numerator array, at least N long
*    QDEN   = backward diagonal of denominator array, at least N long
*    RESULT = accelerated value of the sum
*    S      = simple sum of the series
*
* Inputs:  TERM, ITERM
*
* Outputs:  RESULT, S
*
* If a single precision version is desired, change all occurrences of
* *SP in columns 1 to 3 to blanks and comment the corresponding double
* precision statements.
*
* Revised by M. Goano, Politecnico di Torino.
* Latest modification of the revised version: April 12, 1993
************************************************************************
*   Parameters
*SP     REAL             ONE, ZERO
*SP     PARAMETER (ONE = 1.0E+0, ZERO = 0.0E+0)
*   Scalar arguments
*SP     REAL             RESULT, S, TERM
*   Array arguments
*SP     REAL             QNUM(*), QDEN(*)
*   Local scalars
*SP     REAL             C, FACTOR, FJ, FL, FTERM, RATIO
*   Intrinsic functions
*SP     INTRINSIC REAL
* ----------------------------------------------------------------------
C     .. Parameters ..
      DOUBLE PRECISION ONE,ZERO
      PARAMETER (ONE=1.0D+0,ZERO=0.0D+0)
C     ..
C     .. Scalar Arguments ..
      DOUBLE PRECISION RESULT,S,TERM
      INTEGER ITERM
C     ..
C     .. Array Arguments ..
      DOUBLE PRECISION QDEN(*),QNUM(*)
C     ..
C     .. Local Scalars ..
      DOUBLE PRECISION C,FACTOR,FJ,FL,FTERM,RATIO
      INTEGER J,K,L
C     ..
C     .. Intrinsic Functions ..
      INTRINSIC DBLE
C     ..
      IF (ITERM.EQ.1) S = ZERO
* Get ITERM diagonal
      S = TERM + S
      L = ITERM - 1
*SP     FTERM = REAL(ITERM)
      FTERM = DBLE(ITERM)
      QDEN(ITERM) = ONE/ (TERM*FTERM**2)
      QNUM(ITERM) = S*QDEN(ITERM)
      IF (ITERM.GT.1) THEN
          FACTOR = ONE
*SP       FL = REAL(L)
          FL = DBLE(L)
          RATIO = FL/FTERM
          DO 10 K = 1,L
              J = ITERM - K
*SP         FJ = REAL(J)
              FJ = DBLE(J)
              C = FACTOR*FJ/FTERM
              FACTOR = FACTOR*RATIO
              QDEN(J) = QDEN(J+1) - C*QDEN(J)
              QNUM(J) = QNUM(J+1) - C*QNUM(J)
   10     CONTINUE
      END IF

      RESULT = QNUM(1)/QDEN(1)
      RETURN

      END




