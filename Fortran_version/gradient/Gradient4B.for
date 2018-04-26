	program Gradient4B
	real n1,n2,n3
	complex e1,e2,e3
	
      dimension ff(8),c(8),t(8)
	common /Fan/n,c,t,ff,sum
      common /ZU/f,d1,d2,q,e1,e2,e3

       write(*,*)'Input wave length  Q'
      read(*,*) q

       write(*,*) 'Input n3, x3'
      read(*,*) n3,x3
      e3=cmplx(n3,-x3)

       write(*,*)"Input experimental f(8),cos(8),tg(8)"
	write(*,*) "f= "
	read(*,*) ff
	write(*,*) "cos ="
      read(*,*) c
      write(*,*) "tg= "
      read(*,*) t
       data a1,a2,h1,h2/0.002,0.002,1.0,1.0/
       write(*,*)'Input number of mesuremants'
       read(*,*) n

c    *********' Input of the layers parameters' ******

5      write(*,*)"Input  d1, d2"
      read(*,*) d1,d2
      write(*,*) 'Input n1, x1'
      read (*,*) n1, x1
      write(*,*) 'Input n2, x2'
      read (*,*) n2, x2
      open(2,file='Gradient4B.dat')
            k=0
      dn1=0
	dn2=0
	dd1=0
	dd2=0
	d=0
c    *********' Calculation of the layers parameters' ******
    
10	n1=n1-dn1*d
	n2=n2-dn2*d
	E=q**2*d
	d1=d1-dd1*E
	d2=d2-dd2*E
      
c   ******’Calculation of functional'************

	k=k+1
      e1=cmplx(n1,-x1)
      e2=cmplx(n2,-x2)     
      call Func
      F0=Sum
	

c   *****’Calculation of derivatives'************
      e1=cmplx(n1+a1,-x1)
      call Func
	F1=Sum
      e1=cmplx(n1-a1,-x1)
	call Func
	F2=Sum
	dn1=(F1-F2)/2./a1
      e1=cmplx(n1,-x1)
	d1=d1+h1
	call Func
	F1=Sum
	d1=d1-2*h1
	call Func
	F2=Sum
	dd1=(F1-F2)/2./h1
	d1=d1+h1
	e2=cmplx(n2+a2,-x2)
	call Func
	F1=Sum
      e2=cmplx(n2-a2,-x2)
	call Func
	F2=Sum
	dn2=(F1-F2)/2./a2
      e2=cmplx(n2,-x2)
      d2=d2+h2
	call Func
	F1=Sum
      d2=d2-2*h2
	call Func
	F2=Sum
	dd2=(F1-F2)/2./h2
      d2=d2+h2
c******** ’Calculation of gradient'********
      grad=dn1**2+dn2**2+(dd1**2+dd2**2)*q*q
      d=F0/grad
	
	     
c *********'Deriving of results’*********
      write(*,*)"n1=",n1,"n2=",n2,"d1=",d1,"d2=",d2,"Iteration",k
	write(2,*)
	write(2,*)"n1=",n1,"n2=",n2,"d1=",d1,"d2=",d2,"Iteration",k
      write(*,333)
      write(2,334)
	write(2,100)F0,grad
	write(*,100)F0,grad
      
333   format(1x,'Fact F',12x,'Grad G')
     * 
334   format(1x,'Fact F',16x,'Grad G')
     * 
100	format(1F9.6, 8x, 1F9.6)
	write(*,*)"Continue?,New parameters - 1, Next iteration -2, stop-0"
	read(*,*)j
	if(J.EQ.1) goto 5
	if(J.EQ.2) goto 10
	if(J.EQ.0) goto 20
20	stop
	end program Gradient4B

c****'Subroutines of the functionals calculation'*********

	Subroutine Func
      parameter(pi=3.1415926)
      common /Fan/n,c(8),t(8),ff(8),sum
      common /ZU/f,d1,d2,q,e1,e2,e3
      Sum=0
	Do i=1,n
	f=ff(i)*pi/180
	Call FZU (z,u)
	Sum= Sum+((c(i)-u)**2+(t(i)-z)**2)/2./n
	
	end do
      end SUBROUTINE Func 
	    	      	
c   --------Calculation of ellipsometric parameters--------------------------------------------	
	SUBROUTINE FZU(z,u)
      real f,q,d1,d2,ksi,delta
      
      complex dl1,dl2,e1,e2,e3,cf1,cf2,cf3
	complex r01p,r12p,r23p,r01s,r12s,r23s
	complex r,rs,rp,ex1,ex2,ex3
	parameter (pi=3.1415926)
	common /ZU/f,d1,d2,q,e1,e2,e3
      
	cf1=CSQRT(1-sin(f)**2/(e1*e1))
	cf2=CSQRT(1-sin(f)**2/(e2*e2))
	cf3=CSQRT(1-sin(f)**2/(e3*e3))
	r01p=(e1*cos(f)-cf1)/(e1*cos(f)+cf1)
	r12p=(e2*cf1-e1*cf2)/(e2*cf1+e1*cf2)
	r23p=(e3*cf2-e2*cf3)/(e3*cf2+e2*cf3)
	r01s=(cos(f)-e1*cf1)/(cos(f)+e1*cf1)
	r12s=(e1*cf1-e2*cf2)/(e1*cf1+e2*cf2)
	r23s=(e2*cf2-e3*cf3)/(e2*cf2+e3*cf3)
	dl1=2*pi/q*d1*CSQRT(e1*e1-sin(f)**2)
	ex1=CEXP(2*(0.,-1.)*dl1)
	dl2=2*pi/q*d2*CSQRT(e2*e2-sin(f)**2)
	ex2=CEXP(2*(0.,-1.)*dl2)
	ex3=CEXP(2*(0.,-1.)*(dl1+dl2))
c   -----------------------------------------------------
	rp=r01p+r12p*ex1+r01p*r12p*r23p*ex2+r23p*ex3
	rp=rp/(1+r01p*r12p*ex1+r12p*r23p*ex2+r01p*r23p*ex3)
	rs=r01s+r12s*ex1+r01s*r12s*r23s*ex2+r23s*ex3
	rs=rs/(1+r01s*r12s*ex1+r12s*r23s*ex2+r01s*r23s*ex3)
	r=rp/rs
	z=CABS(r)
	u=REAL(r)/z
	ksi=ATAN(z)*180/pi
	delta=ACOS(u)*180/pi
	end SUBROUTINE FZU