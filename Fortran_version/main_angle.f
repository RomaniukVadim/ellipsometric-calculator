      program MainAngle
!     ------Programm for calculation main angle and ellipticity-------
      real ff,q,d1,d2,ksi,delta,u,z,k1,k2,k3
      complex e1,e2,e3
      parameter (pi=3.1415926)
      common ff,e1,e2,e3,q,d1,d2
      write(*,*) 'Input E1, E2, E3 /*.,-*/'
      read(*,*)e1,e2,e3
      write(*,*) 'Input length of wave'
      read(*,*)q
      open(1,file='EXIT.dat')
      write(*,*) 'Input thicknessis d1 and d2'
      read(*,*)d1,d2
      write(*,*) 'Input thicknessis d1 and d2'
      read(*,*)d1,d2
 1    write(*,*) 'Input maximal angle of incidence'
      read(*,*)W
      write(*,*) 'Input minimal angle of incidence'
      read(*,*)V
      write(*,6)
      write(1,6)
      ff=W
      call FZU(z,u,delta,ksi)
      k1=u
      ff=V
      call FZU(z,u,delta,ksi)
      k2=u
      g=k1*k2
      if(f.lt.0) goto 2
      write(*,*) 'Root does not exist'
      goto 1
 2    ff=(W+V)/2
      call FZU(z,u,delta,ksi)
      k3=u
      if(abs(k3).le.0.0001) goto 4
      g=k1*k3
      if(g.lt.0) goto 3
      W=ff
      goto 2
 3    V=ff
      goto 2
 4    write(*,5) ff, u, z, delta, ksi
      write(1,5) ff, u, z, delta, ksi
      write(*,*) 'Continue? j=1, j=0 - Stop'
      read(*,*)j
      if(j.eq.1)goto 1
      if(j.eq.0)goto 7
 5    format(2x,f7.4,4(2x,f12.7))
 6    format(6x,'F',14x,'COS',14x,'TAN',14x,'DELTA',12x,'KSI')
 7    stop
      end program MainAngle
!     --------------Calculation of ellipsometric parameters-------------
      SUBROUTINE FZU(z,u,delta,ksi)
      real ff,q,d1,d2,ksi,delta,u,z
      complex dl1,dl2,e1,e2,e3,cf1,cf2,cf3
      complex r01p,r12p,r23p,r01s,r12s,r23s
      complex r,rs,rp,ex1,ex2,ex3
      parameter (pi=3.1415926)
      common ff,e1,e2,e3,q,d1,d2
      f=ff*pi/180
      cf1=CSQRT(1-sin(f)**2/(e1*e1))
      cf2=CSQRT(1-sin(f)**2/(e2*e2))
      cf3=CSQRT(1-sin(f)**2/(e3*e3))
      r01p=(e1*cos(f)-cf1)/(e1*cos(f)+cf1)
      r12p=(e2*cf1-e1*cf2)/(e2*cf1+e1*cf2)
      r23p=(e3*cf2-e2*cf3)/(e3*cf2+e2*cf3)
      r01s=(cos(f)-e1*cf1)/(cos(f)+e1*cf1)
      r12s=(e1*cf1-e2*cf2)/(e1*cf1+e2*cf2)
      r23s=(e2*cf1-e2*cf2)/(e1*cf1+e2*cf2)
      dl1=2*pi/q*d1*CSQRT(e1*e1-sin(f)**2)
      ex1=CEXP(2*(0.,-1.)*dl1)
      dl2=2*pi/q*d2*CSQRT(e2*e2-sin(f)**2)
      ex2=CEXP(2*(0.,-1.)*dl2)
      ex3=CEXP(2*(0.,-1.)*(dl1+dl2))
!     ---------------------------------------------------------------
      rp=r01p+r12p*ex1+r01p*r12p*r23p*ex2+r23p*ex3
      rp=rp/(1+r01p*r12p*ex1+r12p*r23p*ex2+r01p*r23p*ex3)
      rs=r01s+r12s*ex1+r01s*r12s*r23s*ex2+r23s*ex3
      rs=rs/(1+r01s+r12s*ex1+r12s*r23s*ex2+r01s*r23s*ex3)
      r=rp/rs
      z=CABS(r)
      u=REAL(r)/z
      ksi=ATAN(z)*180/pi
      delta=ACOS(u)*180/pi
      ff=f*180/pi
      end SUBROUTINE FZU

