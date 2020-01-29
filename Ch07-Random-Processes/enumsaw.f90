PROGRAM ENUMSAW
IMPLICIT NONE

INTEGER, PARAMETER :: MAXEDGE = 20
INTEGER, PARAMETER :: MAXZ = 4
INTEGER, PARAMETER :: MAXSTEP = 20

REAL*8 ce2 , ce4 , cr2 , cr4 , e2 , r2 , sde , sdr , t0 , t1 , v ,&
        & w , wk
INTEGER i , j , k , k1 , l , ldir , lx , ly , &
        & mx , my , n , ndr , nstep , ntrue

DIMENSION wk(MAXSTEP) , ce2(MAXSTEP) , ce4(MAXSTEP) , cr2(MAXSTEP)&
        & , cr4(MAXSTEP) , e2(MAXSTEP) , r2(MAXSTEP)
CHARACTER lat(-MAXEDGE:MAXEDGE,-MAXEDGE:MAXEDGE)
DIMENSION lx(0:MAXSTEP) , ly(0:MAXSTEP)
DIMENSION ldir(MAXSTEP) , ndr(2,MAXZ)
REAL ETIME , time(2)
DATA ndr/ - 1 , 0 , 1 , 0 , 0 , -1 , 0 , 1/
CHARACTER*12 out
!
!
!
   100  PRINT * , 'Enumerating SAW on square lattice: #steps ?'
        READ (5,*) nstep
        IF ( nstep<2 ) GOTO 100
        nstep = MIN(nstep,MAXSTEP)
        PRINT * , 'output file name ?'
        READ (5,99001) out
  99001 FORMAT (a12)
        DO i = 2 , nstep
           wk(i) = 0.D0
           ce2(i) = 0.D0
           ce4(i) = 0.D0
           cr2(i) = 0.D0
           cr4(i) = 0.D0
        ENDDO
        wk(1) = 1.D0
        ce2(1) = 1.D0
        ce4(1) = 1.D0
        cr2(1) = 1.D0
        cr4(1) = 1.D0
        DO i = -nstep , nstep
           DO j = -nstep , nstep
              lat(i,j) = '0'
           ENDDO
        ENDDO
        lat(0,0) = '1'
        lat(1,0) = '1'
        lx(0) = 0
        ly(0) = 0
        lx(1) = 1
        ly(1) = 0
        ldir(1) = 1
        e2(1) = 1.D0
        r2(1) = 1.D0
        n = 2
        ldir(2) = 1
  !
  !
  !
        t0 = ETIME(time)
  !
  !
  !
   200  k = ldir(n)
        i = lx(n-1)
        j = ly(n-1)
        DO l = k , MAXZ
           mx = i + ndr(1,l)
           my = j + ndr(2,l)
           IF ( lat(mx,my)=='0' ) THEN
              e2(n) = DFLOAT(mx)**2 + DFLOAT(my)**2
              r2(n) = r2(n-1) + e2(n)
              DO k1 = 1 , n - 1
                 r2(n) = r2(n) + DFLOAT(mx-lx(k1))**2 + DFLOAT(my-ly(k1)) &
                       & **2
              ENDDO
              wk(n) = wk(n) + 1.D0
              ce2(n) = ce2(n) + e2(n)
              ce4(n) = ce4(n) + e2(n)**2
              cr2(n) = cr2(n) + r2(n)
              cr4(n) = cr4(n) + r2(n)**2
              IF ( n<nstep ) THEN
                 ldir(n) = l + 1
                 lat(mx,my) = '1'
                 lx(n) = mx
                 ly(n) = my
                 n = n + 1
                 ldir(n) = 1
                 GOTO 200
              ENDIF
           ENDIF
  !
  !
  !
        ENDDO
        lat(i,j) = '0'
        n = n - 1
        IF ( n>1 ) GOTO 200
  !
  !
  !
        t1 = ETIME(time)
        ntrue = 0
        DO i = 1 , nstep
           IF ( wk(i)==0 ) GOTO 300
           ntrue = ntrue + 1
           v = 1.D0/DFLOAT(ntrue)**2
           w = 1.D0/wk(i)
           ce2(i) = ce2(i)*w
           sde = ce4(i)*w - ce2(i)**2
           IF ( sde<0 ) sde = 0.D0
           ce4(i) = DSQRT(sde)
           cr2(i) = cr2(i)*v*w
           sdr = cr4(i)*w*v**2 - cr2(i)**2
           IF ( sdr<0 ) sdr = 0.D0
           cr4(i) = DSQRT(sdr)
        ENDDO
   300  OPEN (1,FILE=out)
        REWIND (1)
        WRITE (6,99002)
  99002 FORMAT (/1x,'steps',2x,'ms r_ee',9x,'sd',10x,'ms r_g',10x,'sd',   &
              & 11x,'#wks/4')
        DO i = 1 , ntrue
           WRITE (6,99004) i , ce2(i) , ce4(i) , cr2(i) , cr4(i) , wk(i)
           WRITE (1,99004) i , ce2(i) , ce4(i) , cr2(i) , cr4(i) , wk(i)
        ENDDO
        WRITE (6,99003) t1 - t0
  99003 FORMAT (/1x,'CPU time (sec) = ',1p,e12.5)
  99004 FORMAT (1x,i3,1x,1p,5(e12.5,2x))
        END
  