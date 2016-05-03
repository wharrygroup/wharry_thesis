!***********************************************************************************************************************
!***********************************************************************************************************************
!****                                                                                                              *****                   
!****          MAIN PROGRAM                                                                                        *****
!****                                                                                                              *****
!****--------------------------------------------------------------------------------------------------------------*****
!****                                                                                                              *****
!****    THIS PROGRAM CALCULATES THE AMOUNT OF RADIATION INDUCED SEGREGATION FOR A TERNARY CONCENTRATED ALLOY.     *****
!**** THE FORMULATION IS BASED ON THE PERKS MODEL AND IS SOLVED NUMERICALLY USING THE GEAR SUBROUTINES.            *****
!****--------------------------------------------------------------------------------------------------------------*****
!**** This version is a rewrite of the original and is designed to conform to Fortran 95 standards.  It also       *****
!**** set the format of the program to a single style.  Below is a listing of changes to the original program other*****
!**** than that absolutly required by changes in Fortran from Fortran 2 through 95.                                *****
!****                                                                                                              *****
!**** Changes:                                                                                                     *****
!****         STEP changed to ISTEP                                                                                *****
!****         STOP changed to PSTOP   you just can't use a reserved word as a variable                             *****
!****         INDEX changed to IERR   you just can't use a reserved word as a variable                             *****
!****         In the original files were opened in the begining of the program and closed at the end - no longer   *****
!****                                                                                                              *****
!***********************************************************************************************************************
!
      PROGRAM LOCAL
!
!     Program Initialization Begins Here
!     ==================================
!     Variable Initialization
!     -----------------------
!     IMPLICIT REAL (Kind=8) :: (A-H,O-Z) ed had like this but got error
      IMPLICIT REAL*8 (A-H,O-Z)
!
      COMMON/CONC1/CA(50),CB(50),CC(50),CV(50),CI(50)
      COMMON/CONC2/YO(250)
      COMMON/CONC3/NA(49),NB(49),NC(49),NV(49),NI(49)
      COMMON/DIFF/DAV(49),DBV(49),DCV(49),DAIO(49),DBIO(49),DCIO(49),AL
      COMMON/GEO1/MESHSP(49),NAT
      COMMON/GEO2/XVALUE(50)
      COMMON/GEO3/MESHSI(49)
      COMMON/FLUX/JA(50),JB(50),JC(50),JV(50),JI(50),JA0,JB0,JC0
      COMMON/DEFCT1/RECA(50),RECB(50),RECC(50),DISLOC(50),CVTHER(50),DISPV(50),DISPI(50)
      COMMON/DEFCT2/DIFI(50),DIFV(50),BIASI,BIASV
      COMMON/CNTRL1/N,T0,MF,IERR
      COMMON/CNTRL2/ISTEP,PSTOP
      COMMON/CNTRL3/H0,EPS
      COMMON/CNTRL4/NSTEP
      COMMON/TIME/TOUTPT(20),TSTOP
      COMMON/TIME2/TOUT
      COMMON/DAMAGE/DISPRT
      COMMON/DEFCT3/TKT(49)
      COMMON/ENER1/EAA,EBB,ECC,Z
      COMMON/ENER2/EAB,EBC,EAC
      COMMON/ENER3/EAV,EBV,ECV
      COMMON/ENER4/ESA,ESB,ESC
      COMMON/ENER5/PREVA,PREVB,PREVC,LAMBDA
!
      integer (KIND=3) :: ISTEP
      character (KIND=1,LEN=1) :: PSTOP
!
!     Format Statements
!     -----------------
  100 FORMAT ("ERROR RETURN WITH IERR= ",I3)
!
!
!     Program Execution Starts Here
!     =============================
      ISTEP=0
      PSTOP='N'
!
      CALL INIT
!
      DO WHILE(PSTOP .EQ. 'N')
         CALL Prep(ISTEP, N, NSTEP, T0, MF, IERR, TOUT, TOUTPT, TSTOP, PSTOP)
         IF(PSTOP.EQ.'Y') exit
         CALL DRIVE(N,T0,H0,YO,TOUT,EPS,MF,IERR)         
         IF (IERR.NE.0) THEN
           open(UNIT=8,FILE='perks.err',STATUS='UNKNOWN')
            WRITE (8,100) IERR
            CLOSE(8)
            print *,  "terminated with error"
            go to 999
         END IF
         CALL OUTPT
      END DO
!
     print *,  "normal completion"
 999 close (6)
     close (7)
      STOP
!
!     Program Completed
!     =================
      END PROGRAM local
!
!***********************************************************************************************************************
!***********************************************************************************************************************
!*****     *****     ******                      Subroutine Start Here                         *****     *****     *****
!***********************************************************************************************************************
!***********************************************************************************************************************
!
!*****************************************************************************************
!*****     SUBROUTINE INIT                                                           *****
!*****-------------------------------------------------------------------------------*****
!*****     Subroutine init now sets up a number of variables based on information    *****
!*****     read in by fylein.                                                        *****
!*****-------------------------------------------------------------------------------*****
!***** CALLED BY: Main                                                               *****
!***** CALLS: nothing                                                                *****
!*****************************************************************************************
!   *************************************************************
!   1800,1850,3100,3150 are modified to allowed inputing different 
!   vacancy migration energy.     ---By J.Gan 12-2-1992
!   *************************************************************    
!
!
      subroutine Init
!
!     Subroutine initialization starts here
!     =====================================
!     Variables
!     ---------
      IMPLICIT REAL*8 (A-H,O-Z)
!
      DIMENSION DFRAC(50),TFRAC(49),SFRAC(50)
      DIMENSION CAFRAC(50),CBFRAC(50),CCFRAC(50)
      DIMENSION TEMP(49)
      DIMENSION NUIA(49),NUIB(49),NUIC(49)
!
      COMMON/CONC1/CA(50),CB(50),CC(50),CV(50),CI(50)
      COMMON/CONC2/YO(250)
      COMMON/DIFF/DAV(49),DBV(49),DCV(49),DAIO(49),DBIO(49),DCIO(49),AL
      COMMON/GEO1/MESHSP(49),NAT
      COMMON/GEO2/XVALUE(50)
      COMMON/DEFCT1/RECA(50),RECB(50),RECC(50),DISLOC(50),CVTHER(50),DISPV(50),DISPI(50)
      COMMON/CNTRL3/H0,EPS
      COMMON/CNTRL4/NSTEP
      COMMON/TIME/TOUTPT(20),TSTOP
      COMMON/DEFCT2/DIFI(50),DIFV(50),BIASI,BIASV
      COMMON/DEFCT3/TKT(49)
      COMMON/DAMAGE/DISPRT
      COMMON/ENER1/EAA,EBB,ECC,Z
      COMMON/ENER2/EAB,EBC,EAC
      COMMON/ENER3/EAV,EBV,ECV
      COMMON/ENER4/ESA,ESB,ESC
      COMMON/ENER5/PREVA,PREVB,PREVC,LAMBDA
!
      REAL (KIND=2) :: NAT,NUOV,NUOI,MESHSP
      REAL (KIND=2) :: NUIA,NUIB,NUIC,LAMBDA
      CHARACTER (KIND=1) :: FRAC
      character fn*80
      logical flag
!
!     Format Statements
!     -----------------
 1000 FORMAT(3F8.1,3I3)
 1100 FORMAT(2E12.4)
 1200 FORMAT(E19.8,2F11.8,F6.1)
 1300 FORMAT(F12.8)
 1400 FORMAT(2F10.7)
 1500 FORMAT(3E16.5)
 1600 FORMAT(4F6.3)
 1700 FORMAT(3F11.8)
 1800 FORMAT(3F11.8)
 1850 FORMAT(4F11.8)
 1900 FORMAT(2E12.4)
 2000 FORMAT(F11.8,F6.2,2F5.2)
 2100 FORMAT(6(E12.4))
 2200 FORMAT(A1)
 2300 FORMAT(3F4.2)
 2310 FORMAT(4F11.8)
 2312 FORMAT(3F11.8)
 2315 FORMAT(3F11.8)
 2320 FORMAT(6(F7.4))
 2321 FORMAT(6(F4.1))
 2322 FORMAT(6(F4.1))
 2323 FORMAT(6(F4.1))
 2324 FORMAT(6(F4.1))
 2325 FORMAT(6(F4.1))
 2331 FORMAT(1X,"TFRAC=",/,6(F7.4))
 2332 FORMAT(1X,"CAFRAC=",/,6(F7.4))
 2333 FORMAT(1X,"CBFRAC=",/,6(F7.4))
 2334 FORMAT(1X,"CCFRAC=",/,6(F7.4))
 2335 FORMAT(1X,"DFRAC=",/,6(F7.4))
 2336 FORMAT(1X,"SFRAC=",/,6(F7.4))
 2400 FORMAT(1X,"H0=",E12.4,5X,"EPS=",E12.4)
 2500 FORMAT(1X,"DISPRT=",E19.9,2X,"ETAV=",F11.8,2X,"ETAI=",F11.8,2X,"DOSE=",F6.1)
 2600 FORMAT(1X,"TEMP=",F12.8,"C")
 2700 FORMAT(1X,"CB=",F10.7,2X,"CC=",F10.7)
 2800 FORMAT(1X,"DISL=",E17.8,2X,"NAT=",E17.8,2X,"LAMBDA=",E17.8)
 2900 FORMAT(1X,"FAV=",F6.3,2X,"FBV=",F6.3,2X,"FCV=",F6.3,2X,"FI=",F6.3)
 3000 FORMAT(1X,"WAV=",F11.8,2X,"WBV=",F11.8,2X,"WCV=",F11.8)
 3050 FORMAT(1X,"WAI=",F11.8,2X,"WBI=",F11.8,2X,"WCI=",F11.8)
 3100 FORMAT(1X,"ECOHA=",F11.8,2X,"ECOHB=",F11.8,2X,"ECOHC=",F11.8)
 3150 FORMAT(1X,"EMIA=",F11.8,2X,"EMIB=",F11.8,2X,"EMIC=",F11.8,2X,"SV=",F11.8)
 3200 FORMAT(1X,"NUOV",E12.4,2X,"NUOI=",E12.4)
 3300 FORMAT(1X,"AL=",F11.8,2X,"Z=",F6.2,1X,"BIASV=",F5.2,1X,"BIASI=",F5.2)
 3310 FORMAT(1X,"EFA=",F11.8,1X,"EFB=",F11.8,1X,"EFC=",F11.8,1X,"EFGB=",F11.8)
 3312 FORMAT(1X,"EMA=",F11.8,1X,"EMB=",F11.8,1X,"EMC=",F11.8)
 3315 FORMAT(1X,"EORDAB=",F11.8,1X,"EORDAC=",F11.8,1X,"EORDBC=",F11.8)
!
!     Subroutine Execution Starts Here
!     ================================
!
! input file
      fn='f:\attachments_2010_04_27\perks.in'
!      lenth=index (fn,' ')-1
! length of trimmed string (.net win32 function) 
      lenth=len_trim(fn)
      j=1 
      inquire(file=fn(j:lenth),exist=flag)
      
      do while (.not.flag)
         print '("input file ",a," not found!... Enter alternate name or Q to quit")', fn(1:lenth) 
         read (*,'(a)') fn 
         lenth=len_trim(fn)
! identify leading blanks                         
         j=1
         do while (fn(j:j).eq.' ') 
           j = j+1
         end do   
         if (lenth.eq.j) then
            if (fn(j:j).eq.'Q'.or.fn(j:j).eq.'q') stop 
         end if                            
         inquire(file=fn(j:lenth),exist=flag)
      end do
      open(UNIT=5,FILE=fn(j:lenth),STATUS='OLD')
!
! ouput file      
      inquire(file='f:\attachments_2010_04_27\perks.out.txt',exist=flag)
      if (flag) then
         open(UNIT=6,FILE='f:\attachments_2010_04_27\perks.out.txt',STATUS='REPLACE')
      else
         open(UNIT=6,FILE='f:\attachments_2010_04_27\perks.out.txt',STATUS='NEW')
      end if
      
!      
! junk goes here ...... REMOVE this and also REMOVE the write(7,*) lines and close(7)
! when at it look for and REMOVE also these lines :
 !    print *,"TOUTPT,DOSE",TOUTPT(ISTEP),DOSE 
 ! and print *,"HUSED,NQUSED,NSTEP,NFE,NJE"
 ! and print *,HUSED,NQUSED,NSTEP,NFE,NJE
      fn='f:\attachments_2010_04_27\junk.txt'
      inquire(file=fn,exist=flag)    
      if (flag) then
         open(UNIT=7,FILE=fn,STATUS='REPLACE')
      else
         open(UNIT=7,FILE=fn,STATUS='NEW')
      end if
! end of junk

! 
      READ(5,1000) R1,R2,RF,N1,N2,N3
!
      READ(5,1100) H0,EPS
      READ(5,1200) DISPRT,ETAV,ETAI,DOSE
      READ(5,1300) TEMPC
      READ(5,1400) CONCB,CONCC
      READ(5,1500) DISL,NAT,LAMBDA
      READ(5,1600) FAV,FBV,FCV,FI
      READ(5,1700) WAV,WBV,WCV
      READ(5,1700) WAI,WBI,WCI
!
      READ(5,1800) ECOHA,ECOHB,ECOHC    
      READ(5,1850) EMIA,EMIB,EMIC,SV
      READ(5,2312) EMA,EMB,EMC
      READ(5,2310) EFA,EFB,EFC,EFGB
      READ(5,2315) EORDAB,EORDAC,EORDBC
      READ(5,1900) NUOV,NUOI
      READ(5,2000) AL,Z,BIASV,BIASI
!
      READ(5,2100) (TOUTPT(I),I=1,20)
      READ(5,2200) FRAC
!
      NSTEP=N1+N2+N3
!
      IF(FRAC.EQ.'N') THEN
         DO I=1,NSTEP-1
            TFRAC(I)=1.0
         END DO       
!
         DO I=1,NSTEP
            CAFRAC(I)=1.0           
            CBFRAC(I)=1.0         
            SFRAC(I)=1.0
            DFRAC(I)=1.0
            CCFRAC(I)=1.0
         END DO
!        
      ELSE
        READ(5,2320) (TFRAC(I),I=1,NSTEP-1)
        READ(5,2320) (CAFRAC(I),I=1,NSTEP)
        READ(5,2320) (CBFRAC(I),I=1,NSTEP)
        READ(5,2320) (CCFRAC(I),I=1,NSTEP)
        READ(5,2320) (DFRAC(I),I=1,NSTEP)
        READ(5,2320) (SFRAC(I),I=1,NSTEP)
      END IF
!
      WRITE(6,2400) H0,EPS
      WRITE(6,2500) DISPRT,ETAV,ETAI,DOSE
      WRITE(6,2600) TEMPC
      WRITE(6,2700) CONCB,CONCC
      WRITE(6,2800) DISL,NAT,LAMBDA
      WRITE(6,2900) FAV,FBV,FCV,FI
      WRITE(6,3000) WAV,WBV,WCV
      WRITE(6,3050) WAI,WBI,WCI
!
      WRITE(6,3100) ECOHA,ECOHB,ECOHC   
      WRITE(6,3150) EMIA,EMIB,EMIC,SV
      WRITE(6,3312) EMA,EMB,EMC
      WRITE(6,3310) EFA,EFB,EFC,EFGB
      WRITE(6,3315) EORDAB,EORDAC,EORDBC
      WRITE(6,3200) NUOV,NUOI
      WRITE(6,3300) AL,Z,BIASV,BIASI
!
      WRITE(6,2331) (TFRAC(I),I=1,NSTEP-1)
      WRITE(6,2332) (CAFRAC(I),I=1,NSTEP)
      WRITE(6,2333) (CBFRAC(I),I=1,NSTEP)
      WRITE(6,2334) (CCFRAC(I),I=1,NSTEP)
      WRITE(6,2335) (DFRAC(I),I=1,NSTEP)
      WRITE(6,2336) (SFRAC(I),I=1,NSTEP)
!
!     write(7,*)   "FRAC=",FRAC
!
      SCFAC=1.0E-09
      BOLTZ=8.617E-05
      TSTOP=DOSE/DISPRT
      EAA=ECOHA/(Z/2)
      EBB=ECOHB/(Z/2)
      ECC=ECOHC/(Z/2)
      EAB=0.5*(EAA+EBB)-EORDAB
      EAC=0.5*(EAA+ECC)-EORDAC
      EBC=0.5*(EBB+ECC)-EORDBC
      EAV=(ECOHA+EFA)/Z
      EBV=(ECOHB+EFB)/Z
      ECV=(ECOHC+EFC)/Z
      ESA=EMA+Z*(EAA+EAV)
      ESB=EMB+Z*(EBB+EBV)
      ESC=EMC+Z*(ECC+ECV)
      PREVA=NUOV*WAV*FAV
      PREVB=NUOV*WBV*FBV
      PREVC=NUOV*WCV*FCV
!
      CONCA=1.0-(CONCB+CONCC)
      do I=1,N1-1
         MESHSP(I)=R1*SCFAC/N1
      end do
!
      do I=N1,N1+N2-1
         MESHSP(I)=(R2-R1)*SCFAC/N2
      end do
!
      do I=N1+N2,N1+N2+N3-1
         MESHSP(I)=(RF-R2)*SCFAC/N3
      end do
!      
      do I=1,NSTEP
         DISPV(I)=DISPRT*ETAV*DFRAC(I)
         DISPI(I)=DISPRT*ETAI*DFRAC(I)
      end do
!
         XVALUE(1)=0.0
      do I=2,NSTEP
         XVALUE(I)=XVALUE(I-1)+MESHSP(I-1)
      end do
!
      do I=1,NSTEP
         CA(I)=CONCA*CAFRAC(I)
         CB(I)=CONCB*CBFRAC(I)
         CC(I)=CONCC*CCFRAC(I)
         CI(I)=0.0
         DISLOC(I)=DISL*SFRAC(I)
      end do
!
!     EFV=CA(1)*EFA+CB(1)*EFB+CC(1)*EFC  commented out in original program
!
      do I=1,NSTEP-1
         TEMP(I)=(TEMPC+273)*TFRAC(I)
         TKT(I)=BOLTZ*TEMP(I)
         NUIA(I)=NUOI*WAI*FI*EXP((-1*EMIA)/TKT(I))
         NUIB(I)=NUOI*WBI*FI*EXP((-1*EMIB)/TKT(I))
         NUIC(I)=NUOI*WCI*FI*EXP((-1*EMIC)/TKT(I))
         DAIO(I)=0.66667*NUIA(I)*LAMBDA**2
         DBIO(I)=0.66667*NUIB(I)*LAMBDA**2
         DCIO(I)=0.66667*NUIC(I)*LAMBDA**2
         CVTHER(I)=EXP(SV)*EXP((-1*EFGB)/TKT(I))
      end do
      CVTHER(NSTEP)=CVTHER(NSTEP-1)
      do I=2,NSTEP-1
         CVTHER(I)=0.5*(CVTHER(I)+CVTHER(I-1))
      end do
!
      do I=1,NSTEP
         CV(I)=CVTHER(I)
      end do
!
      do I=1,NSTEP
         YO(I)=CA(I)
      end do
      do I=NSTEP+1,2*NSTEP
         YO(I)=CB(I-NSTEP)
      end do
      do I=2*NSTEP+1,3*NSTEP
         YO(I)=CC(I-2*NSTEP)
      end do
      do I=3*NSTEP+1,4*NSTEP
         YO(I)=CV(I-3*NSTEP)
      end do
      do I=4*NSTEP+1,5*NSTEP
         YO(I)=CI(I-4*NSTEP)
      end do
!
!     Subroutine completed - Time to return
!     =====================================
!      close (5)
!      close (6)
!     write(7,*)   "init ok"
      return
      end subroutine Init
!
!*****************************************************************************************C
!*****     SUBROUTINE Prep                                                           *****
!*****-------------------------------------------------------------------------------*****
!*****                                                                               *****
!*****-------------------------------------------------------------------------------*****
!***** CALLED BY: Main                                                               *****
!***** CALLS: nothing                                                                *****
!*****************************************************************************************
!*****************************************************************************************
!
      subroutine Prep(ISTEP, N, NSTEP, T0, MF, IERR, TOUT, TOUTPT, TSTOP, PSTOP)
!
!     Subroutine initialization starts here
!     =====================================
!     Variables
!     ---------
      REAL (KIND=2) :: T0, TOUT, TOUTPT(20), TSTOP
      INTEGER (KIND=3) :: ISTEP, N, NSTEP, MF, IERR
      CHARACTER (KIND=1,LEN=1) :: PSTOP
!
!     Subroutine Execution Starts Here
!     ================================
      if (ISTEP .EQ. 0) then
         N=5*NSTEP
         T0=0
         MF=22
         IERR=1
         TOUT=TOUTPT(1)
         ISTEP=ISTEP+1
      else if ((TOUT .LT. TSTOP) .AND. (ISTEP .LT. 20)) then
            ISTEP=ISTEP+1
            TOUT=TOUTPT(ISTEP)
         else
            PSTOP='Y'
!         end if
      end if
!
!     Subroutine completed - Time to return
!     =====================================
!     write(7,*)   "prep ok"
      return
      end subroutine Prep
!
!*****************************************************************************************C
!*****     SUBROUTINE Outpt                                                          *****
!*****-------------------------------------------------------------------------------*****
!*****                                                                               *****
!*****-------------------------------------------------------------------------------*****
!***** CALLED BY: Main                                                               *****
!***** CALLS: nothing                                                                *****
!*****************************************************************************************
!
      subroutine Outpt
!
!     Subroutine initialization starts here
!     =====================================
!     Variables
!     ---------
      IMPLICIT REAL*8 (A-H,O-Z)
!
      DIMENSION CERR(50)
!
      COMMON/CONC1/CA(50),CB(50),CC(50),CV(50),CI(50)
      COMMON/CONC2/YO(250)
      COMMON/CNTRL2/ISTEP,PSTOP
      COMMON/CNTRL4/NSTEP
      COMMON/TIME/TOUTPT(20),TSTOP
      COMMON/DAMAGE/DISPRT
      COMMON/GEO2/XVALUE(50)
      COMMON/DEFCT1/RECA(50),RECB(50),RECC(50),DISLOC(50),CVTHER(50),DISPV(50),DISPI(50)      
      DIMENSION XOUT(50)
!
      INTEGER (KIND=3) :: ISTEP
!
!     Format Statements
!     -----------------
  100 FORMAT (/1X,"TIME=",E8.1,2X,"DOSE=",F8.2) 
  110 FORMAT (/7X,"POSITION",11X," CA",17X," CB",17X," CC",17X," CV",17x," CI")
  115 FORMAT (/1X)
  120 FORMAT (1X,6(E16.8,4X))
  130 FORMAT(/10X,"CASURF=",4x,F8.4,21X,"CBSURF=",6x,F8.4,19X,"CCSURF=",6x,F8.4)
!
!    Subroutine Execution Start Here
!    ===============================
      do I=1,NSTEP
         CA(I)=YO(I)
      end do
      do I=NSTEP+1,2*NSTEP
         CB(I-NSTEP)=YO(I)
      end do
      do I=2*NSTEP+1,3*NSTEP
         CC(I-2*NSTEP)=YO(I)
      end do 
      do I=3*NSTEP+1,4*NSTEP
         CV(I-3*NSTEP)=YO(I)
      end do
      do I=4*NSTEP+1,5*NSTEP
         CI(I-4*NSTEP)=YO(I)
      end do
!            
      DOSE=DISPRT*TOUTPT(ISTEP)
!      
!      open(UNIT=6,FILE='perks.out',STATUS='OLD',POSITION='APPEND')
      WRITE (6,'(/1X,"TIME=",E8.1,2X,"DOSE=",e10.4)') TOUTPT(ISTEP),DOSE
      print *,"TOUTPT,DOSE",TOUTPT(ISTEP),DOSE
      WRITE (6,110)
      do I=1,NSTEP
         CERR(I)=1-(CA(I)+CB(I)+CC(I))
         XOUT(I)=XVALUE(I)*1E9
      WRITE (6,120) XOUT(I),CA(I),CB(I),CC(I),CV(I),CI(I)
      end do
!
      SUM1=0
      SUM2=0
      do J=1,NSTEP
         SUM1=SUM1+EXP((-XOUT(J)/.8452))
         SUM2=SUM2+CA(J)*EXP((-XOUT(J)/.8452))
      end do
      CASURF= SUM2/SUM1
!
      SUM1=0
      SUM2=0
      do J=1,NSTEP
         SUM1=SUM1+EXP((-XOUT(J)/.7474))
         SUM2=SUM2+CB(J)*EXP((-XOUT(J)/.7474))
      end do
      CBSURF= SUM2/SUM1
!
!
      SUM1=0
      SUM2=0
      do J=1,NSTEP
         SUM1=SUM1+EXP((-XOUT(J)/.9472))
         SUM2=SUM2+CC(J)*EXP((-XOUT(J)/.9472))
      end do
      CCSURF= SUM2/SUM1
      TEMP1=CASURF
      TEMP2=CBSURF
      TEMP3=CCSURF
      CASURF=TEMP1/(TEMP1+TEMP2+TEMP3)
      CBSURF=TEMP2/(TEMP1+TEMP2+TEMP3)
      CCSURF=TEMP3/(TEMP1+TEMP2+TEMP3)
!
      WRITE(6,130) CASURF,CBSURF,CCSURF
!      close(6)
!
!     Subroutine completed - Time to return
!     =====================================
!     write(7,*)   "outpt ok"
      return
      end subroutine Outpt
!
!*****************************************************************************************
!*****     SUBROUTINE Drive                                                          *****
!*****-------------------------------------------------------------------------------*****
!*****                                                                               *****
!*****-------------------------------------------------------------------------------*****
!***** CALLED BY: Main                                                               *****
!***** CALLS: Stiff, Interp                                                          *****
!*****************************************************************************************
!
      subroutine Drive(N, T0, H0, Y0, TOUT, EPS, MF, IERR)
!
!     Subroutine initialization starts here
!     =====================================
!     Variables
!     ---------
      IMPLICIT REAL*8 (A-H,O-Z)
      INTEGER (KIND=3) :: N,MF,IERR
      INTEGER (KIND=3) :: NC,MFC,KFLAG,JSTART,IPIV,NSQ,NQUSED,NSTEP,NFE,NJE
      INTEGER (KIND=3) :: LOUT,I,N0,NHCUT,KGO
      COMMON /GEAR1/ T,H,HMIN,HMAX,EPSC,UROUND,NC,MFC,KFLAG,JSTART
      COMMON /GEAR2/ YMAX(250)
      COMMON /GEAR3/ ERROR(250)
      COMMON /GEAR4/ SAVE1(250)
      COMMON /GEAR5/ SAVE2(250)
      COMMON /GEAR6/ PW(62500)
      COMMON /GEAR7/ IPIV(250)
      COMMON /GEAR8/ EPSJ,NSQ
      COMMON /GEAR9/ HUSED,NQUSED,NSTEP,NFE,NJE
      DATA LOUT/6/
      DIMENSION Y0(250)
      DIMENSION Y(250,6)

!     Format Statements
!     -----------------
 15   FORMAT("WARNING..  T + H = T ON NEXT STEP.")
 105  FORMAT("KFLAG = -1 FROM INTEGRATOR AT T = ",E16.8/"ERROR TEST FAILED WITH ABS(H) = HMIN"/)
 115  FORMAT("H HAS BEEN REDUCED TO ",E16.8,"AND STEP WILL BE RETRIED"//)
 155  FORMAT("PROBLEM APPEARS UNSOLVABLE WITH GIVEN INPUT"//)
 205  FORMAT("KFLAG = -2 FROM INTEGRATOR AT T = ",E16.8,"H = ",E16.8/"THE REQUESTED ERROR IS SMALLER THAN CAN BE HANDLED"//)
 255  FORMAT("INTEGRATION HALTED BY DRIVER AT T = ",E16.8/"EPS TOO SMALL TO BE ATTAINED FOR THE MACHINE PRECISION"/)
 305  FORMAT("KFLAG = -3 FROM INTEGRATOR AT T = ",E16.8/"CORRECTOR CONVERGENCE COULD NOT BE ACHIEVED"/)
 405  FORMAT("ILLEGAL INPUT.. EPS .LE. 0."//)
 415  FORMAT("ILLEGAL INPUT.. N .LE. 0"//)
 425  FORMAT("ILLEGAL INPUT.. (T0-TOUT)*H .GE. 0."//)
 435  FORMAT("ILLEGAL INPUT.. IERR = ",I5//)
 445  FORMAT(//"IERR = -1 ON INPUT WITH (T-TOUT)*H .GE. 0."/"T = ",E16.8,"  TOUT = ",E16.8,"  H = ",E16.8/ &
            "INTERPOLATION WAS DONE AS ON NORMAL RETURN."/"DESIRED PARAMETER CHANGES WERE NOT MADE.")
!
!
!     Subroutine Execution Starts Here
!     ================================
      IF (IERR .EQ. 0) GO TO 20
      IF (IERR .EQ. 2) GO TO 25
      IF (IERR .EQ. -1) GO TO 30
      IF (IERR .EQ. 3) GO TO 40
      IF (IERR .NE. 1) GO TO 430
      IF (EPS .LE. 0.E0) GO TO 400
      IF (N .LE. 0) GO TO 410
      IF ((T0-TOUT)*H0 .GE. 0.E0) GO TO 420
      
      UROUND = 2.22E-16
      DO I = 1,N
         YMAX(I) = ABS(Y0(I))
!        IF (YMAX(I) .EQ. 0.E0) YMAX(I) = 1.E0
!        //////// THIS IS A CHANGE //////////
         IF(YMAX(I) .LT. 1.E0) YMAX(I)=1.E0
!        //////// END CHANGE ////////////////
         Y(I,1) = Y0(I)
      END DO
      NC = N
      T = T0
      H = H0
      IF ((T+H) .EQ. T) THEN
!       open(UNIT=6,FILE='perks.out',STATUS='OLD',POSITION='APPEND')
         WRITE(LOUT,15)
!       close(LOUT)
      END IF 
!------------------------------------------------------------------
!
      HMIN = ABS(H0)
      HMAX = ABS(T0-TOUT)*10.E0
      EPSC = EPS
      MFC = MF
      JSTART = 0
      N0 = N
      NSQ = N0*N0
      EPSJ = SQRT(UROUND)
      NHCUT = 0
      GO TO 50
!
!  TOUTP IS THE PREVIOUS VALUE OF TOUT FOR USE IN HMAX.
!
 20   HMAX = ABS(TOUT-TOUTP)*10.
      GO TO 80
!
 25   HMAX = ABS(TOUT-TOUTP)*10.E0
      IF ((T-TOUT)*H .GE. 0.E0) GO TO 500
      GO TO 85
!
 30   IF ((T-TOUT)*H .GE. 0.E0) GO TO 440
      JSTART = -1
      NC = N
      EPSC = EPS
      MFC = MF
!
 40   if ((T+H) .EQ. T) then
!       open(UNIT=6,FILE='perks.out',STATUS='OLD',POSITION='APPEND')
         WRITE(LOUT,15)
!       close(LOUT)
      end if
!
 50   CALL STIFF (Y, N0)
!
      KGO = 1 - KFLAG
!     GO TO (60, 100, 200, 300), KGO --- REPLACED COMPUTED GOTO WITH CASE CONSTRUCT
!     KFLAG  =   0,  -1,  -2,  -3
      select case (KGO)
      case (1)
           GO TO 60
      case (2)
           GO TO 100
      case (3)
           GO TO 200
      case (4)
           GO TO 300
      case default
           continue
      end select
!
 60   CONTINUE
! 
      D = 0.E0
      DO I = 1,N
        AYI = ABS(Y(I,1))
        YMAX(I) = MAX(YMAX(I), AYI)
        D = D + (AYI/YMAX(I))**2
      END DO
      D = D*(UROUND/EPS)**2
      IF (D .GT. DFLOAT(N)) GO TO 250
      IF (IERR .EQ. 3) GO TO 500
      IF (IERR .EQ. 2) GO TO 85
 80   IF ((T-TOUT)*H .LT. 0.E0) GO TO 40
      CALL INTERP (TOUT, Y, N0, Y0)
      GO TO 520
 85   IF (((T+H)-TOUT)*H .LE. 0.E0) GO TO 40
      IF (ABS(T-TOUT) .LE. 100.E0*UROUND*HMAX) GO TO 500
      IF ((T-TOUT)*H .GE. 0.E0) GO TO 500
      H = (TOUT - T)*(1.E0 - 4.E0*UROUND)
      JSTART = -1
      GO TO 40   
 !
 100  continue
!     open(UNIT=6,FILE='perks.out',STATUS='OLD',POSITION='APPEND')
      WRITE (LOUT,105) T
!      close(6)
 110  IF (NHCUT .EQ. 10) GO TO 150
      NHCUT = NHCUT + 1
      HMIN = HMIN*.1E0
      H = H*.1E0
!      open(UNIT=6,FILE='perks.out',STATUS='OLD',POSITION='APPEND')
      WRITE (LOUT,115) H
!      close(6)
      JSTART = -1
      GO TO 40
!
 150  continue
!     open(UNIT=6,FILE='perks.out',STATUS='OLD',POSITION='APPEND')
         WRITE (LOUT,155)
!       close(6)
      GO TO 500
!
 200  continue
!     open(UNIT=6,FILE='perks.out',STATUS='OLD',POSITION='APPEND')
         WRITE (LOUT,205) T,H
!       close(6)
      GO TO 500
!
 250  continue
!     open(UNIT=6,FILE='perks.out',STATUS='OLD',POSITION='APPEND')
         WRITE (LOUT,255) T
!       close(6)
      KFLAG = -2
      GO TO 500
!
 300  continue
!     open(UNIT=6,FILE='perks.out',STATUS='OLD',POSITION='APPEND')
         WRITE (LOUT,305) T
!       close(6)
      GO TO 110
!
 400  continue
!     open(UNIT=6,FILE='perks.out',STATUS='OLD',POSITION='APPEND')
         WRITE (LOUT,405)
!       close(6)
      IERR = -4
      GO TO 800
!
 410  continue
!     open(UNIT=6,FILE='perks.out',STATUS='OLD',POSITION='APPEND')
         WRITE (LOUT,415)
!       close(6)
      IERR = -4
      GO TO 800
!
 420  continue
 !    open(UNIT=6,FILE='perks.out',STATUS='OLD',POSITION='APPEND')
         WRITE (LOUT,425)
!       close(LOUT)
      IERR = -4
      GO TO 800
!
 430  continue
!     open(UNIT=6,FILE='perks.out',STATUS='OLD',POSITION='APPEND')
         WRITE (LOUT,435) IERR
!       close(LOUT)
      IERR = -4
      GO TO 800
!
 440  continue
!     open(UNIT=6,FILE='perks.out',STATUS='OLD',POSITION='APPEND')
         WRITE(LOUT,445) T,TOUT,H
!       close(LOUT)
      CALL INTERP (TOUT, Y, N0, Y0)
      IERR = -5
      GO TO 800
!
 500  TOUT = T
      DO I = 1,N
        Y0(I) = Y(I,1)
      END DO
 520  IERR = KFLAG
      TOUTP = TOUT
      H0 = HUSED
      IF (KFLAG .NE. 0) H0 = H
 800  CONTINUE
!
!     Subroutine completed - Time to return
!     =====================================
      write(7,*) "tout,HUSED,NQUSED,NSTEP,NFE,NJE"
      write(7,*) tout,HUSED,NQUSED,NSTEP,NFE,NJE
      write(7,*)   "drive ok"
      print *,"HUSED,NQUSED,NSTEP,NFE,NJE"
      print *,HUSED,NQUSED,NSTEP,NFE,NJE
      
      return
      end subroutine Drive
!
!*****************************************************************************************C
!*****     SUBROUTINE Interp                                                         *****
!*****-------------------------------------------------------------------------------*****
!*****                                                                               *****
!*****-------------------------------------------------------------------------------*****
!*****   CALLED BY: Drive                                                            *****
!*****   CALLS:  nothing                                                             *****
!*****************************************************************************************
!
      subroutine Interp(TOUT, Y, N0, Y0)
!
!     Subroutine initialization starts here
!     =====================================
!     Variables
!     ---------
      IMPLICIT REAL*8 (A-H,O-Z)
      REAL (KIND=2) :: TOUT, Y, Y0
      INTEGER (KIND=3) :: N0, N, IDUMMY, JSTART, I , L, J
      COMMON /GEAR1/ T,H,DUMMY(4),N,IDUMMY(2),JSTART
      DIMENSION Y0(N0),Y(N0,6)
!
!     Subroutine Execution Starts Here
!     ================================
      do I = 1,N
         Y0(I) = Y(I,1)
      end do
      L = JSTART + 1
      S = (TOUT - T)/H
      S1 = 1.E0
      outer: do J = 2,L
                S1 = S1*S
         inner: do I = 1,N
                   Y0(I) = Y0(I) + S1*Y(I,J) 
                end do inner
      end do outer
!
!     Subroutine completed - Time to return
!     =====================================
!     write(7,*)   "interp ok"
      return
      end subroutine Interp
!
!*****************************************************************************************
!*****     SUBROUTINE Stiff                                                          *****
!*****-------------------------------------------------------------------------------*****
!*****                                                                               *****
!*****-------------------------------------------------------------------------------*****
!*****   CALLED BY: Drive                                                            *****
!*****   CALLS:  Diffun, Coset, Pset, Sol                                            *****
!*****************************************************************************************
!
      SUBROUTINE STIFF (Y, N0)
!
!     Subroutine initialization starts here
!     =====================================
!     Variables
!     ---------
!     Format Statements
!     -----------------
      IMPLICIT REAL*8 (A-H,O-Z)
      INTEGER (KIND=3) :: N0,N,MF,KFLAG,JSTART,IPIV,NQUSED,NSTEP,NFE,NJE
      INTEGER (KIND=3) :: I,METH,MITER,NQ,L,IDOUB,MFOLD,NOLD,IRET,MEO,MIO,IWEVAL,MAXDER,LMAX,IREDO,J,NSTEPJ,J1,J2,M,IER,NEWQ
      COMMON /GEAR1/ T,H,HMIN,HMAX,EPS,UROUND,N,MF,KFLAG,JSTART
      COMMON /GEAR2/ YMAX(250)
      COMMON /GEAR3/ ERROR(250)
      COMMON /GEAR4/ SAVE1(250)
      COMMON /GEAR5/ SAVE2(250)
      COMMON /GEAR6/ PW(62500)
      COMMON /GEAR7/ IPIV(250)
      COMMON /GEAR9/ HUSED,NQUSED,NSTEP,NFE,NJE
      DIMENSION Y(N0,6)
      DIMENSION EL(13),TQ(4)
      DATA EL(2)/1.E0/, OLDL0/1.E0/
!
!     Subroutine Execution Starts Here
!     ================================

      KFLAG = 0
      TOLD = T            
      IF (JSTART .GT. 0) GO TO 200
      IF (JSTART .NE. 0) GO TO 120
      CALL DIFFUN (N, T, Y, SAVE1)
      DO I = 1,N
         Y(I,2) = H*SAVE1(I)
      END DO
      METH = MF/10
      MITER = MF - 10*METH
      NQ = 1           
      L = 2
      IDOUB = 3
      RMAX = 1.E+04
      RC = 0.E0
      CRATE = 1.E0
      EPSOLD = EPS
      HOLD = H
      MFOLD = MF
      NOLD = N
      NSTEP = 0
      NSTEPJ = 0
      NFE = 1
      NJE = 0
      IRET = 1

      GO TO 130
!
 120  IF (MF .EQ. MFOLD) GO TO 150
      MEO = METH
      MIO = MITER
      METH = MF/10
      MITER = MF - 10*METH
      MFOLD = MF
      IF (MITER .NE. MIO) IWEVAL = MITER
      IF (METH .EQ. MEO) GO TO 150
      IDOUB = L + 1
      IRET = 1
!
 130  continue
      CALL COSET (METH, NQ, EL, TQ, MAXDER)
      LMAX = MAXDER + 1
      RC = RC*EL(1)/OLDL0
      OLDL0 = EL(1)
!
 140  FN = DFLOAT(N)
      EDN = FN*(DBLE(TQ(1))*EPS)**2
      E   = FN*(DBLE(TQ(2))*EPS)**2
      EUP = FN*(DBLE(TQ(3))*EPS)**2
      BND = FN*(DBLE(TQ(4))*EPS)**2
!
!     GO TO (160, 170, 200), IRET --- REPLACED COMPUTED GOTO WITH CASE CONSTRUCT
      select case (IRET)
      case (1)
           GO TO 160
      case (2)
           GO TO 170
      case (3)
           GO TO 200
      case default
           continue
      end select
!
 150  IF ((EPS .EQ. EPSOLD) .AND. (N .EQ. NOLD)) GO TO 160
      EPSOLD = EPS
      NOLD = N
      IRET = 1
      GO TO 140
!
 160  IF (H .EQ. HOLD) GO TO 200
      RH = H/HOLD
      H = HOLD
      IREDO = 3
      GO TO 175
!
 170  RH =   MAX(RH,HMIN/ABS(H))
!
 175  RH =   MIN(RH,HMAX/ABS(H),RMAX)
      R1 = 1.E0
      DO J = 2,L
        R1 = R1*RH
        DO I = 1,N
          Y(I,J) = Y(I,J)*R1
        END DO
      END DO
      H = H*RH
      RC = RC*RH
      IDOUB = L + 1
      IF (IREDO .EQ. 0) GO TO 690
!
 200  IF (ABS(RC-1.E0) .GT. 0.3E0) IWEVAL = MITER
      IF (NSTEP .GE. NSTEPJ+20) IWEVAL = MITER
      T = T + H
      DO J1 = 1,NQ
        DO J2 = J1,NQ
          J = (NQ + J1) - J2
          DO I = 1,N
            Y(I,J) = Y(I,J) + Y(I,J+1)
          END DO
        END DO
      END DO
!
 220  DO I = 1,N
        ERROR(I) = 0.E0
      END DO
!
      M = 0
      CALL DIFFUN (N, T, Y, SAVE2)
      NFE = NFE + 1
      IF (IWEVAL .LE. 0) GO TO 290
      IWEVAL = 0
      RC = 1.E0
      NJE = NJE + 1
      NSTEPJ = NSTEP
!
!     GO TO (250, 240, 260), MITER --- REPLACED COMPUTED GOTO WITH CASE CONSTRUCT
      select case (MITER)
      case (1)
           GO TO 250
      case (2)
           GO TO 240
      case (3)
           GO TO 260
      case default
           continue
      end select     
!
 240  NFE = NFE + N
!
 250  CON = -H*EL(1)
      CALL PSET (Y, N0, CON, MITER, IER)
      IF (IER .NE. 0) GO TO 420
      GO TO 350
!
 260  R = EL(1)*.1E0
      DO 270 I = 1,N
!
 270    PW(I) = Y(I,1) + R*(H*SAVE2(I) - Y(I,2))
      CALL DIFFUN (N, T, PW, SAVE1)
      NFE = NFE + 1
      HL0 = H*EL(1)
      DO I = 1,N
        R0 = H*SAVE2(I) - Y(I,2)
        PW(I) = 1.E0
        D = .1E0*R0 - H*(SAVE1(I) - SAVE2(I))
        SAVE1(I) = 0.E0
        IF (ABS(R0) .LT. UROUND*YMAX(I)) GO TO 280
        IF (ABS(D) .EQ. 0.E0) GO TO 420
        PW(I) = .1E0*R0/D
        SAVE1(I) = PW(I)*R0
      END DO
!
 280  continue
      GO TO 370
!
 290  IF (MITER .NE. 0) GO TO (350, 350, 310), MITER
      D = 0.E0
      DO I = 1,N
        R = H*SAVE2(I) - Y(I,2)
        D = D + ( (R-ERROR(I))/YMAX(I) )**2
        SAVE1(I) = Y(I,1) + EL(1)*R
        ERROR(I) = R
      END DO
      GO TO 400
!
 310  PHL0 = HL0
      HL0 = H*EL(1)
      IF (HL0 .EQ. PHL0) GO TO 330
      R = HL0/PHL0
      DO 320 I = 1,N
        D = 1.E0 - R*(1.E0 - 1.E0/PW(I))
        IF (ABS(D) .EQ. 0.E0) GO TO 440
!
 320    PW(I) = 1.E0/D
!
 330  DO 340 I = 1,N
!
 340    SAVE1(I) = PW(I)*(H*SAVE2(I) - (Y(I,2) + ERROR(I)))
      GO TO 370
!
 350  DO 360 I = 1,N
!
 360    SAVE1(I) = H*SAVE2(I) - (Y(I,2) + ERROR(I))
      CALL SOL (N, N0, PW, SAVE1, IPIV)
!
 370  D = 0.E0
      DO 380 I = 1,N
        ERROR(I) = ERROR(I) + SAVE1(I)
        D = D + (SAVE1(I)/YMAX(I))**2
!
 380    SAVE1(I) = Y(I,1) + EL(1)*ERROR(I)
!
 400  IF (M .NE. 0) CRATE =   MAX(.9E0*CRATE,D/D1)
      ONE1 = 1.E0
      IF ((D*MIN(ONE1,2.E0*CRATE)) .LE. BND) GO TO 450
      D1 = D
      M = M + 1
      IF (M .EQ. 3) GO TO 410
      CALL DIFFUN (N, T, SAVE1, SAVE2)
      GO TO 290
!
 410  NFE = NFE + 2
      IF (IWEVAL .EQ. -1) GO TO 440
!
 420  T = TOLD
      RMAX = 2.E0
      DO J1 = 1,NQ
        DO J2 = J1,NQ
          J = (NQ + J1) - J2
          DO I = 1,N
            Y(I,J) = Y(I,J) - Y(I,J+1)
          END DO
        END DO
      END DO
      IF (ABS(H) .LE. HMIN*1.00001E0) GO TO 680
      RH = .25E0
      IREDO = 1
      GO TO 170
!
 440  IWEVAL = MITER
      GO TO 220
!
 450  IF (MITER .NE. 0) IWEVAL = -1
      NFE = NFE + M
      D = 0.E0
      DO I = 1,N
        D = D + (ERROR(I)/YMAX(I))**2
      END DO
      IF (D .GT. E) GO TO 500
!
      KFLAG = 0
      IREDO = 0
      NSTEP = NSTEP + 1
      HUSED = H
      NQUSED = NQ
!
      DO J = 1,L
        DO I = 1,N
           Y(I,J) = Y(I,J) + EL(J)*ERROR(I)
        END DO
      END DO
!
      IF (IDOUB .EQ. 1) GO TO 520
      IDOUB = IDOUB - 1
      IF (IDOUB .GT. 1) GO TO 700
      IF (L .EQ. LMAX) GO TO 700
      DO I = 1,N
         Y(I,LMAX) = ERROR(I)
      END DO
      GO TO 700
!
 500  KFLAG = KFLAG - 1
      T = TOLD
      DO J1 = 1,NQ
         DO J2 = J1,NQ
            J = (NQ + J1) - J2
            DO I = 1,N
               Y(I,J) = Y(I,J) - Y(I,J+1)
            END DO
         END DO
      END DO
      RMAX = 2.E0
      IF (ABS(H) .LE. HMIN*1.00001E0) GO TO 660
      IF (KFLAG .LE. -3) GO TO 640
      IREDO = 2
      PR3 = 1.D20
      GO TO 540
!
 520  PR3 = 1.D20
      IF (L .EQ. LMAX) GO TO 540
      D1 = 0.E0
      DO I = 1,N
        D1 = D1 + ((ERROR(I) - Y(I,LMAX))/YMAX(I))**2
      END DO
      ENQ3 = .5E0/DFLOAT(L+1)
      PR3 = ((D1/EUP)**ENQ3)*1.4E0 + 1.4E-06
!
 540  ENQ2 = .5E0/DFLOAT(L)
      PR2 = ((D/E)**ENQ2)*1.2E0 + 1.2E-06
      PR1 = 1.D20
      IF (NQ .EQ. 1) GO TO 560
      D = 0.E0
      DO 550 I = 1,N
!
 550    D = D + (Y(I,L)/YMAX(I))**2
      ENQ1 = .5E0/DFLOAT(NQ)
      PR1 = ((D/EDN)**ENQ1)*1.3E0 + 1.3E-06
!
 560  IF (PR2 .LE. PR3) GO TO 570
      IF (PR3 .LT. PR1) GO TO 590
      GO TO 580
!
 570  IF (PR2 .GT. PR1) GO TO 580
      NEWQ = NQ
      RH = 1.E0/PR2
      GO TO 620
!
 580  NEWQ = NQ - 1
      RH = 1.E0/PR1
      GO TO 620
!
 590  NEWQ = L
      RH = 1.E0/PR3
      IF (RH .LT. 1.1E0) GO TO 610
      DO I = 1,N
         Y(I,NEWQ+1) = ERROR(I)*EL(L)/DFLOAT(L)
      END DO
      GO TO 630
!
 610  IDOUB = 10
      GO TO 700
!
 620  IF ((KFLAG .EQ. 0) .AND. (RH .LT. 1.1E0)) GO TO 610 
      IF (NEWQ .EQ. NQ) GO TO 170
!
 630  NQ = NEWQ
      L = NQ + 1
      IRET = 2
      GO TO 130
!
 640  IF (KFLAG .EQ. -7) GO TO 670
      RH = .1E0
      RH = MAX(HMIN/ABS(H),RH)
      H = H*RH
      CALL DIFFUN (N, T, Y, SAVE1)
      NFE = NFE + 1
      DO I = 1,N
         Y(I,2) = H*SAVE1(I)
      END DO
      IWEVAL = MITER
      IDOUB = 10
      IF (NQ .EQ. 1) GO TO 200
      NQ = 1
      L = 2
      IRET = 3
      GO TO 130
!      
 660  KFLAG = -1
      GO TO 700
 670  KFLAG = -2
      GO TO 700
 680  KFLAG = -3
      GO TO 700
 690  RMAX = 10.E0
 700  HOLD = H
      JSTART = NQ
!
!     Subroutine completed - Time to return
!     =====================================
!     write(7,*)   "stiff ok"
      return
      end subroutine Stiff
!
!*****************************************************************************************C
!*****     SUBROUTINE Coset                                                          *****
!*****-------------------------------------------------------------------------------*****
!*****                                                                               *****
!*****-------------------------------------------------------------------------------*****
!***** CALLED BY: Stiff                                                              *****
!***** CALLS: nothing                                                                *****
!*****************************************************************************************
!
      subroutine Coset(METH, NQ, EL, TQ, MAXDER)
!
!     Subroutine initialization starts here
!     =====================================
!     Variables
!     ---------
!     Format Statements
!     -----------------
      IMPLICIT REAL*8 (A-H,O-Z)
!      REAL (KIND=8) :: PERTST(12,2,3), EL(13), TQ(4) ed added this line but got error
      INTEGER (KIND=3) :: METH, NQ, MAXDER, K
!
      DIMENSION PERTST(12,2,3),EL(13),TQ(4)
      DATA  PERTST / 1.,1.,2.,1.,.3158,.07407,.01391,.002182, 2.945E-4,3.492E-5,3.692E-6,3.524E-7,1.,1.,.5,.1667,.04167, &
                     1.,1.,1.,1.,1.,1.,1.,2.,12.,24.,37.89,53.33,70.08,87.97,106.9,126.7,147.4,168.8,191.0,2.0,4.5,      &
                     7.333,10.42,13.7,1.,1.,1.,1.,1.,1.,1.,12.0,24.0,37.89,53.33,70.08,87.97,106.9,126.7,147.4,168.8,    &
                     191.0,1., 3.0,6.0,9.167,12.5,1.,1.,1.,1.,1.,1.,1.,1. /            
!
!     Subroutine Execution Starts Here
!     ================================
      outer : select case (METH)
             case (1)
                      MAXDER = 12
             inner1 : select case (NQ)
                             case(1)
                                    EL(1) = 1.0E0
                             case(2)
                                    EL(1) = 0.5E0
                                    EL(3) = 0.5E0
                             case(3)
                                    EL(1) = 4.1666666666666667E-01
                                    EL(3) = 0.75E0 
                                    EL(4) = 1.6666666666666667E-01
                             case(4)
                                    EL(1) = 0.375E0
                                    EL(3) = 9.1666666666666667E-01
                                    EL(4) = 3.3333333333333333E-01
                                    EL(5) = 4.1666666666666667E-02
                             case(5)
                                    EL(1) = 3.4861111111111111E-01
                                    EL(3) = 1.0416666666666667E0
                                    EL(4) = 4.8611111111111111e-01
                                    EL(5) = 1.0416666666666667e-01
                                    EL(6) = 8.3333333333333333e-03
                             case(6)
                                    EL(1) = 3.2986111111111111e-01
                                    EL(3) = 1.1416666666666667e+00
                                    EL(4) = 0.625E+00
                                    EL(5) = 1.7708333333333333e-01
                                    EL(6) = 0.025E+00
                                    EL(7) = 1.3888888888888889e-03
                             case(7)
                                    EL(1) = 3.1559193121693122e-01
                                    EL(3) = 1.225E+00
                                    EL(4) = 7.5185185185185185e-01
                                    EL(5) = 2.5520833333333333e-01
                                    EL(6) = 4.8611111111111111e-02
                                    EL(7) = 4.8611111111111111e-03
                                    EL(8) = 1.9841269841269841e-04
                             case(8)
                                    EL(1) = 3.0422453703703704e-01
                                    EL(3) = 1.2964285714285714e+00
                                    EL(4) = 8.6851851851851852e-01
                                    EL(5) = 3.3576388888888889e-01
                                    EL(6) = 7.7777777777777778E-02
                                    EL(7) = 1.0648148148148148E-02
                                    EL(8) = 7.9365079365079365E-04
                                    EL(9) = 2.4801587301587302E-05
                             case(9)
                                    EL(1) = 2.9486800044091711E-01
                                    EL(3) = 1.3589285714285714E+00
                                    EL(4) = 9.7655423280423280E-01
                                    EL(5) = 0.4171875E+00
                                    EL(6) = 1.1135416666666667E-01
                                    EL(7) = 0.01875E+00
                                    EL(8) = 1.9345238095238095E-03
                                    EL(9) = 1.1160714285714286E-04
                                    EL(10)= 2.7557319223985891E-06
                             case(10)
                                    EL(1) = 2.8697544642857143E-01
                                    EL(3) = 1.4144841269841270E+00
                                    EL(4) = 1.0772156084656085E+00
                                    EL(5) = 4.9856701940035273E-01
                                    EL(6) = 0.1484375E+00
                                    EL(7) = 2.9060570987654321E-02
                                    EL(8) = 3.7202380952380952E-03
                                    EL(9) = 2.9968584656084656E-04
                                    EL(10)= 1.3778659611992945e-05
                                    EL(11)= 2.7557319223985891e-07
                             case(11)
                                    EL(1) = 2.8018959644393672e-01
                                    EL(3) = 1.4644841269841270e+00
                                    EL(4) = 1.1715145502645503e+00
                                    EL(5) = 5.7935819003527337e-01
                                    EL(6) = 1.8832286155202822e-01
                                    EL(7) = 4.1430362654320988e-02
                                    EL(8) = 6.2111441798941799e-03
                                    EL(9) = 6.2520667989417989e-04
                                    EL(10)= 4.0417401528512640e-05
                                    EL(11)= 1.5156525573192240E-06
                                    EL(12)= 2.5052108385441719E-08
                             case(12)
                                    EL(1) = 2.7426554003159906E-01
                                    EL(3) = 1.5099386724386724E+00
                                    EL(4) = 1.2602711640211640E+00
                                    EL(5) = 6.5923418209876543E-01
                                    EL(6) = 2.3045800264550265E-01
                                    EL(7) = 5.5697246105232216E-02
                                    EL(8) = 9.4394841269841270E-03
                                    EL(9) = 1.1192749669312169E-03
                                    EL(10)= 9.0939153439153439E-05
                                    EL(11)= 4.8225308641975309E-06
                                    EL(12)= 1.5031265031265031E-07
                                    EL(13)= 2.0876756987868099E-09
                      end select inner1                             
             case (2)
                      MAXDER = 5
             inner2 : select case (NQ)
                             case(1)
                                    EL(1) = 1.0E+00
                             case(2)
                                    EL(1) = 6.6666666666666667E-01
                                    EL(3) = 3.3333333333333333E-01
                             case(3)
                                    EL(1) = 5.4545454545454545E-01
                                    EL(3) = EL(1)
                                    EL(4) = 9.0909090909090909E-02
                             case(4)
                                    EL(1) = 0.48E+00
                                    EL(3) = 0.7E+00
                                    EL(4) = 0.2E+00
                                    EL(5) = 0.02E+00   
                             case(5)
                                    EL(1) = 4.3795620437956204E-01
                                    EL(3) = 8.2116788321167883E-01
                                    EL(4) = 3.1021897810218978E-01
                                    EL(5) = 5.4744525547445255E-02
                                    EL(6) = 3.6496350364963504E-03 
                     end select inner2
      end select outer
!
      do K = 1,3
         TQ(K) = PERTST(NQ,METH,K)
      end do
      TQ(4) = .5*TQ(2)/FLOAT(NQ+2)
!
!     Subroutine completed - Time to return
!     =====================================
!     write(7,*)   "coset ok"
      return
      end subroutine Coset
!
!*****************************************************************************************C
!*****     SUBROUTINE Pset                                                           *****
!*****-------------------------------------------------------------------------------*****
!*****                                                                               *****
!*****-------------------------------------------------------------------------------*****
!***** CALLED BY: Stiff                                                              *****
!***** CALLS: Pederv, Diffun, Dec                                                    *****
!*****************************************************************************************
!     
      subroutine Pset(Y, N0, CON, MITER, IER)
!
!     Subroutine initialization starts here
!     =====================================
!     Variables
!     ---------
      IMPLICIT REAL*8 (A-H,O-Z)
      INTEGER (KIND=3) :: N0,MITER,IER,N,IDUMMY,IPIV,NSQ,I,J1,J
      DIMENSION Y(N0,6)
      COMMON /GEAR1/ T,H,DUMMY(3),UROUND,N,IDUMMY(3)
      COMMON /GEAR2/ YMAX(250)
      COMMON /GEAR4/ SAVE1(250)
      COMMON /GEAR5/ SAVE2(250)
      COMMON /GEAR6/ PW(62500)
      COMMON /GEAR7/ IPIV(250)
      COMMON /GEAR8/ EPSJ,NSQ
!
!     Subroutine Execution Starts Here
!     ================================
      select case(MITER)
         case(1)
               CALL Pederv (N, T, Y, PW, N0)
               DO I = 1,NSQ
                  PW(I) = PW(I)*CON
               END DO
         case(2)
               D = 0.E0
               DO I = 1,N
                  D = D + SAVE2(I)**2
               END DO
               R0 = ABS(H)*SQRT(D)*1.E+03*UROUND
               J1 = 0
               DO J = 1,N
                  YJ = Y(J,1)
                  R = EPSJ*YMAX(J)
                  R = MAX(R,R0)
                  Y(J,1) = Y(J,1) + R
                  D = CON/R
                  CALL Diffun (N, T, Y, SAVE1)
                  DO I = 1,N
                     PW(I+J1) = (SAVE1(I) - SAVE2(I))*D
                  END DO
                  Y(J,1) = YJ
                  J1 = J1 + N0
               END DO
      end select
      
!  ADD IDENTITY MATRIX.
      J = 1
      DO I = 1,N
         PW(J) = PW(J) + 1.E0
         J = J + (N0 + 1)
      END DO
!  DO LU DECOMPOSITION ON P.
      CALL Dec (N, N0, PW, IPIV, IER)
!
!     Subroutine completed - Time to return
!     =====================================
!     write(7,*)   "pset ok"
      return
      end subroutine Pset
!
!*****************************************************************************************C
!*****     SUBROUTINE Dec                                                            *****
!*****-------------------------------------------------------------------------------*****
!*****                                                                               *****
!*****-------------------------------------------------------------------------------*****
!***** CALLED BY: Pset                                                               *****
!***** CALLS: nothing                                                                *****
!*****************************************************************************************
!     
      subroutine Dec(N, NDIM, A, IP, IER)      
!
!     Subroutine initialization starts here
!     =====================================
!     Variables
!     ---------
!     Format Statements
!     -----------------
      IMPLICIT REAL*8 (A-H,O-Z)
      INTEGER (KIND=3) :: N, NDIM, IP, IER, NM1, K, KP1, M, I, J
      DIMENSION A(NDIM,N), IP(N)
!
!     Subroutine Execution Start Here
!     ===============================
      IER = 0
      IP(N) = 1
      IF (N .EQ. 1) GO TO 70
      NM1 = N - 1
      OUTER: DO K = 1,NM1
        KP1 = K + 1
        M = K
        DO I = KP1,N
              IF (ABS(A(I,K)) .GT. ABS(A(M,K))) M = I
        END DO
        IP(K) = M
        T = A(M,K)
        IF (M .EQ. K) GO TO 20
        IP(N) = -IP(N)
        A(M,K) = A(K,K)
        A(K,K) = T
 20     IF (T .EQ. 0.E0) GO TO 80
        T = 1.E0/T
        do I = KP1,N
           A(I,K) = -A(I,K)*T
        end do
        DO J = KP1,N
          T = A(M,J)
          A(M,J) = A(K,J)
          A(K,J) = T
          IF (T .EQ. 0.E0) GO TO 50
             DO I = KP1,N
                A(I,J) = A(I,J) + A(I,K)*T
             END DO
        END DO
 50     CONTINUE
      END DO OUTER
 70   Continue
      K = N
      IF (A(N,N) .EQ. 0.E0) GO TO 80
      RETURN
 80   IER = K
      IP(N) = 0
!
!     Subroutine completed - Time to return
!     =====================================
!     write(7,*)   "dec ok"
      return
      end subroutine Dec
!
!*****************************************************************************************C
!*****     SUBROUTINE Sol                                                            *****
!*****-------------------------------------------------------------------------------*****
!*****                                                                               *****
!*****-------------------------------------------------------------------------------*****
!***** CALLED BY: Stiff                                                              *****
!***** CALLS: nothing                                                                *****
!*****************************************************************************************
!     
      subroutine Sol(N, NDIM, A, B, IP)
!
!     Subroutine initialization starts here
!     =====================================
!     Variables
!     ---------
      REAL (KIND=2) :: A, B, T
      INTEGER (KIND=3) :: N, NDIM, IP, NM1, K, KP1, M, I, KB, KM1
      DIMENSION :: A(NDIM,N), B(N), IP(N)
!
!     Subroutine Execution Starts Here
!     ================================
      IF (N .NE. 1) then
         NM1 = N - 1
         outera: do K = 1,NM1
                   KP1 = K + 1
                   M = IP(K)
                   T = B(M)
                   B(M) = B(K)
                   B(K) = T
           innera: do I = KP1,N
                     B(I) = B(I) + A(I,K)*T
                  end do innera
         end do outera
         outerb: do KB = 1,NM1
                   KM1 = N - KB
                   K = KM1 + 1
                   B(K) = B(K)/A(K,K)
                   T = -B(K)
            innerb: do I = 1,KM1
                      B(I) = B(I) + A(I,K)*T
                   end do innerb
         end do outerb
      END IF
      B(1) = B(1)/A(1,1)
!
!     Subroutine completed - Time to return
!     =====================================
!     write(7,*)   "sol ok"
      return
      end subroutine Sol
!
!*****************************************************************************************
!*****     SUBROUTINE Diffun                                                         *****
!*****-------------------------------------------------------------------------------*****
!*****     Subroutine Diffun calculates the time rate of change of each component    *****
!*****     (three atoms, vacancies, and interstitials) as inputs to the Gear         *****
!*****     subroutine.  Diffun is accesssed by the Stiff subroutine numerous times   *****
!*****     for each output time.  The subroutine Interp interpolates froom the last  *****
!*****     gear time step to the user requested output step.                         *****
!*****-------------------------------------------------------------------------------*****
!***** CALLED BY: Stiff, Pset                                                        *****
!***** CALLS: nothing                                                                *****
!*****************************************************************************************
!
!
      subroutine Diffun(N,T,Y,YDOT)
!
!     Subroutine initialization starts here
!     =====================================
!     Variables
!     ---------
!     Format Statements
!     -----------------
      IMPLICIT REAL*8 (A-H,O-Z)
!
      DIMENSION RECOMB(50),INTSINK(50),VACSINK(50),VACSOUR(50)
      DIMENSION DIVJA(50),DIVJB(50),DIVJC(50),DIVJV(50),DIVJI(50)
      DIMENSION YDOT(250),Y(250)
      DIMENSION CADOT(50),CBDOT(50),CCDOT(50),CVDOT(50),CIDOT(50)
      DIMENSION GRADCA(49),GRADCB(49),GRADCC(49),GRADCV(49),GRADCI(49)
      DIMENSION DA(49),DB(49),DC(49),DV(49),DI(49)
      DIMENSION JO(49),DAI(49),DBI(49),DCI(49)
      DIMENSION EA(49),EB(49),EC(49)
      DIMENSION NUVA(49),NUVB(49)
      DIMENSION NUVC(49),NUIA(49),NUIB(49),NUIC(49)
      DIMENSION NA(49),NB(49),NC(49),NV(49),NI(49)
!
      COMMON/CONC1/CA(50),CB(50),CC(50),CV(50),CI(50)
      COMMON/DIFF/DAV(49),DBV(49),DCV(49),DAIO(49),DBIO(49),DCIO(49),AL
      COMMON/GEO1/MESHSP(49),NAT
      COMMON/GEO3/MESHSI(49)
      COMMON/DEFCT1/RECA(50),RECB(50),RECC(50),DISLOC(50),CVTHER(50),DISPV(50),DISPI(50)
      COMMON/DEFCT2/DIFI(50),DIFV(50),BIASI,BIASV
      COMMON/FLUX/JA(50),JB(50),JC(50),JV(50),JI(50),JA0,JB0,JC0
      COMMON/CNTRL4/NSTEP
      COMMON/DEFCT3/TKT(49)
      COMMON/ENER1/EAA,EBB,ECC,Z
      COMMON/ENER2/EAB,EBC,EAC
      COMMON/ENER3/EAV,EBV,ECV
      COMMON/ENER4/ESA,ESB,ESC
      COMMON/ENER5/PREVA,PREVB,PREVC,LAMBDA
!
      REAL (KIND=2) :: MESHSP,NAT,MESHSI,JA,JB,JC,JV,JI
      REAL (KIND=2) :: NA,NB,NC,NV,NI,JO,INTSINK
      REAL (KIND=2) :: NUVA,NUVB,NUVC,LAMBDA
      REAL (KIND=2) :: NUIA,NUIB,NUIC
!
      do I=1,NSTEP
         CA(I)=Y(I)
      end do
      do I=NSTEP+1,2*NSTEP
         CB(I-NSTEP)=Y(I)
      end do
      do I=2*NSTEP+1,3*NSTEP
         CC(I-2*NSTEP)=Y(I)
      end do 
      do I=3*NSTEP+1,4*NSTEP
         CV(I-3*NSTEP)=Y(I)
      end do
      do I=4*NSTEP+1,5*NSTEP
         CI(I-4*NSTEP)=Y(I)
      end do
      do I=1,NSTEP-1
         NA(I)=0.5*(CA(I+1)+CA(I))
         NB(I)=0.5*(CB(I+1)+CB(I))
         NC(I)=0.5*(CC(I+1)+CC(I))
         NV(I)=0.5*(CV(I+1)+CV(I))
         NI(I)=0.5*(CI(I+1)+CI(I))
         DAI(I)=DAIO(I)
         DBI(I)=DBIO(I)
         DCI(I)=DCIO(I)
!         EA(I)=((ESA+ESA*NA(I)+ESB*NB(I)+ESC*NC(I))/2)-((Z*(NA(I)*EAA+NB(I)*EAB+NC(I)*EAC+NV(I)*EAV)) &
!              +(Z*(NA(I)*EAV+NB(I)*EBV+NC(I)*ECV)))
!         EB(I)=((ESB+ESA*NA(I)+ESB*NB(I)+ESC*NC(I))/2)-((Z*(NA(I)*EAB+NB(I)*EBB+NC(I)*EBC+NV(I)*EBV)) &
!              +(Z*(NA(I)*EAV+NB(I)*EBV+NC(I)*ECV)))
!         EC(I)=((ESC+ESA*NA(I)+ESB*NB(I)+ESC*NC(I))/2)-((Z*(NA(I)*EAC+NB(I)*EBC+NC(I)*ECC+NV(I)*ECV)) &
!              +(Z*(NA(I)*EAV+NB(I)*EBV+NC(I)*ECV)))
         EA(I)=1.20
         EB(I)=1.24
         EC(I)=1.04
         NUVA(I)=PREVA*EXP((-1*EA(I)/TKT(I)))
         NUVB(I)=PREVB*EXP((-1*EB(I)/TKT(I)))
         NUVC(I)=PREVC*EXP((-1*EC(I)/TKT(I)))
         DAV(I)=NUVA(I)*LAMBDA**2
         DBV(I)=NUVB(I)*LAMBDA**2
         DCV(I)=NUVC(I)*LAMBDA**2
         RECA(I)=(NUVA(I)+NUIA(I))*Z
         RECB(I)=(NUVB(I)+NUIB(I))*Z
         RECC(I)=(NUVC(I)+NUIC(I))*Z
         DIFV(I)=DAV(I)*NA(I)+DBV(I)*NB(I)+DCV(I)*NC(I)
         DIFI(I)=DAI(I)*NA(I)+DBI(I)*NB(I)+DCI(I)*NC(I)
      end do
!
      DIFV(NSTEP)=DIFV(NSTEP-1)
      DIFI(NSTEP)=DIFI(NSTEP-1)
      do I=2,NSTEP-1
         DIFV(I)=0.5*(DIFV(I)+DIFV(I-1))
         DIFI(I)=0.5*(DIFI(I)+DIFI(I-1))
      end do
!
      RECA(NSTEP)=RECA(NSTEP-1)
      RECB(NSTEP)=RECB(NSTEP-1)
      RECC(NSTEP)=RECC(NSTEP-1)
      CVTHER(NSTEP)=CVTHER(NSTEP-1)
      do I=2,NSTEP-1
         RECA(I)=0.5*(RECA(I)+RECA(I-1))
         RECB(I)=0.5*(RECB(I)+RECB(I-1))
         RECC(I)=0.5*(RECC(I)+RECC(I-1))
         CVTHER(I)=0.5*(CVTHER(I)+CVTHER(I-1))
      end do
!
      JA0=0.0
      JB0=0.0
      JC0=0.0
      JA(NSTEP)=0.0
      JB(NSTEP)=0.0
      JC(NSTEP)=0.0
      JV(NSTEP)=0.0
      JI(NSTEP)=0.0
!
      do I=1,NSTEP-1
!
         GRADCA(I)=(CA(I+1)-CA(I))/MESHSP(I)
         GRADCB(I)=(CB(I+1)-CB(I))/MESHSP(I)
         GRADCC(I)=(CC(I+1)-CC(I))/MESHSP(I)
         GRADCV(I)=(CV(I+1)-CV(I))/MESHSP(I)
         GRADCI(I)=(CI(I+1)-CI(I))/MESHSP(I)
!
         DA(I)=DAV(I)*NV(I)+DAI(I)*NI(I)
         DB(I)=DBV(I)*NV(I)+DBI(I)*NI(I)
         DC(I)=DCV(I)*NV(I)+DCI(I)*NI(I)
         DV(I)=DAV(I)*NA(I)+DBV(I)*NB(I)+DCV(I)*NC(I)
         DI(I)=DAI(I)*NA(I)+DBI(I)*NB(I)+DCI(I)*NC(I)
!
      end do
!
      do I=1,NSTEP-1
         JV(I)=NAT*(-1*DV(I)*GRADCV(I)+NV(I)*AL*(DAV(I)*GRADCA(I)+DBV(I)*GRADCB(I)+DCV(I)*GRADCC(I)))
         JI(I)=NAT*(-1*DI(I)*GRADCI(I)-NI(I)*AL*(DAI(I)*GRADCA(I)+DBI(I)*GRADCB(I)+DCI(I)*GRADCC(I)))
         JO(I)=JI(I)-JV(I)
         JA(I)=NAT*(-1*DA(I)*AL*GRADCA(I)+NA(I)*(DAV(I)*GRADCV(I)-DAI(I)*GRADCI(I)))-JO(I)*NA(I)
         JB(I)=NAT*(-1*DB(I)*AL*GRADCB(I)+NB(I)*(DBV(I)*GRADCV(I)-DBI(I)*GRADCI(I)))-JO(I)*NB(I)
         JC(I)=NAT*(-1*DC(I)*AL*GRADCC(I)+NC(I)*(DCV(I)*GRADCV(I)-DCI(I)*GRADCI(I)))-JO(I)*NC(I)
!
         JV(I)=JV(I)-JO(I)*NV(I)
         JI(I)=JI(I)-JO(I)*NI(I)
      end do
!
      DIVJA(1)=2.0*(JA(1)-JA0)/MESHSP(1)
      DIVJB(1)=2.0*(JB(1)-JB0)/MESHSP(1)
      DIVJC(1)=2.0*(JC(1)-JC0)/MESHSP(1)
!
      do I=2,NSTEP-1
         MESHSI(I)=0.5*(MESHSP(I)+MESHSP(I-1))
         DIVJA(I)=(JA(I)-JA(I-1))/MESHSI(I)
         DIVJB(I)=(JB(I)-JB(I-1))/MESHSI(I)
         DIVJC(I)=(JC(I)-JC(I-1))/MESHSI(I)
         DIVJV(I)=(JV(I)-JV(I-1))/MESHSI(I)
         DIVJI(I)=(JI(I)-JI(I-1))/MESHSI(I)
      end do
!
      DIVJA(NSTEP)=2.0*(JA(NSTEP)-JA(NSTEP-1))/MESHSP(NSTEP-1)
      DIVJB(NSTEP)=2.0*(JB(NSTEP)-JB(NSTEP-1))/MESHSP(NSTEP-1)
      DIVJC(NSTEP)=2.0*(JC(NSTEP)-JC(NSTEP-1))/MESHSP(NSTEP-1)
      DIVJV(NSTEP)=2.0*(JV(NSTEP)-JV(NSTEP-1))/MESHSP(NSTEP-1)
      DIVJI(NSTEP)=2.0*(JI(NSTEP)-JI(NSTEP-1))/MESHSP(NSTEP-1)
!
      do I=1,NSTEP
         CADOT(I)=-1*DIVJA(I)/NAT
         CBDOT(I)=-1*DIVJB(I)/NAT
         CCDOT(I)=-1*DIVJC(I)/NAT
      end do
!
      do I=1,NSTEP
         RECOMB(I)=RECA(I)*CA(I)+RECB(I)*CB(I)+RECC(I)*CC(I)
         INTSINK(I)=DISLOC(I)*DIFI(I)
         VACSINK(I)=DISLOC(I)*DIFV(I)
         VACSOUR(I)=DISLOC(I)*DIFV(I)*CVTHER(I)
      end do
!
      CVDOT(1)=0.0
      CIDOT(1)=0.0
      do I=2,NSTEP
        CVDOT(I)=-1*DIVJV(I)/NAT-RECOMB(I)*CV(I)*CI(I)-BIASV*VACSINK(I)*CV(I)+VACSOUR(I)+DISPV(I)
        CIDOT(I)=-1*DIVJI(I)/NAT-RECOMB(I)*CV(I)*CI(I)-BIASI*INTSINK(I)*CI(I)+DISPI(I)
      end do
!
      do I=1,NSTEP
         Y(I)=CA(I)
      end do
!
      do I=NSTEP+1,2*NSTEP
         Y(I)=CB(I-NSTEP)
      end do
!
      do I=2*NSTEP+1,3*NSTEP
         Y(I)=CC(I-2*NSTEP)
      end do
!
      do I=3*NSTEP+1,4*NSTEP
         Y(I)=CV(I-3*NSTEP)
      end do
!
      do I=4*NSTEP+1,5*NSTEP
         Y(I)=CI(I-4*NSTEP)
      end do
!
      do I=1,NSTEP
         YDOT(I)=CADOT(I)
      end do
!
      do I=NSTEP+1,2*NSTEP
         YDOT(I)=CBDOT(I-NSTEP)
      end do     
!
      do I=2*NSTEP+1,3*NSTEP
         YDOT(I)=CCDOT(I-2*NSTEP)
      end do     
!
      do I=3*NSTEP+1,4*NSTEP
         YDOT(I)=CVDOT(I-3*NSTEP)
      end do     
!
      do I=4*NSTEP+1,5*NSTEP
         YDOT(I)=CIDOT(I-4*NSTEP)
      end do
!
!     Subroutine completed - Time to return
!     =====================================
!!     write(7,*)   "diffun ok"
      return
      end subroutine Diffun
!
!*****************************************************************************************C
!*****     SUBROUTINE Pederv                                                         *****
!*****-------------------------------------------------------------------------------*****
!*****                                                                               *****
!*****-------------------------------------------------------------------------------*****
!***** CALLED BY: Pset                                                               *****
!***** CALLS: nothing                                                                *****
!*****************************************************************************************
!
      SUBROUTINE Pederv(N,T,Y,PW,N0)
!
!     Subroutine initialization starts here
!     =====================================
!     Variables
!     ---------
!     Format Statements
!     -----------------

      IMPLICIT REAL*8 (A-H,O-Z)
!
!     Subroutine completed - Time to return
!     =====================================
!     write(7,*)   "pederv ok"
      return
      end subroutine Pederv
!
!***********************************************************************************************************************
!***********************************************************************************************************************
!*****     *****     ******                      Subroutines End Here                          *****     *****     *****
!***********************************************************************************************************************
!***********************************************************************************************************************
