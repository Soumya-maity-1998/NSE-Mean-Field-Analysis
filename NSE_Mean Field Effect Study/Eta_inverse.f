	subroutine Etainv(dlhs,eta)
      implicit real *8 (a-h,o-z)
      order = 0.5d0
	eta1 = -5.0d0
	ieter = 1
10    continue
	call Fermi(order,eta1,fd1)
	eta2 = eta1 + 1.0d0
	call Fermi(order, eta2, fd2)
	factor = (eta2 - eta1)/(fd2 - fd1)
	eta = (factor*(dlhs - fd1)) + eta1
	call Fermi(order, eta, fd)	  
	dif = dabs(fd - dlhs)
c	write(*,*)
c	write(*,11)ieter,dif
11    format(i5, f9.5)
	if(dif.ge.0.00000001) then
	eta1 = eta
	ieter = ieter + 1
	goto 10
	end if
c	write(*,*)
c	write(*,*)"fd=",fd
c	write(*,12)ieter,fd,eta
12    format(i5, 2f9.5)
	return
	end