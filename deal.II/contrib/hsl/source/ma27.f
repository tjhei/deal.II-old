C *******************************************************************
* COPYRIGHT (c) 1993 AEA Technology

C None of the comments in this Copyright notice between the lines
C of asterisks shall be removed or altered in any way.

C This Package is intended for compilation without modification,
C so most of the embedded comments have been removed.

C ALL USE IS SUBJECT TO LICENCE. For full details of an HSL ARCHIVE
C Licence, see http://hsl.rl.ac.uk/archive/cou.html

C Please note that for an HSL ARCHIVE Licence:

C 1. The Package must not be copied for use by any other person.
C    Supply of any part of the library by the Licensee to a third party
C    shall be subject to prior written agreement between AEA
C    Technology plc and the Licensee on suitable terms and conditions,
C    which will include financial conditions.
C 2. All information on the Package is provided to the Licensee on the
C    understanding that the details thereof are confidential.
C 3. All publications issued by the Licensee that include results obtained
C    with the help of one or more of the Packages shall acknowledge the
C    use of the Packages. The Licensee will notify the Numerical Analysis
C    Group at Rutherford Appleton Laboratory of any such publication.
C 4. The Packages may be modified by or on behalf of the Licensee
C    for such use in research applications but at no time shall such
C    Packages or modifications thereof become the property of the
C    Licensee. The Licensee shall make available free of charge to the
C    copyright holder for any purpose all information relating to
C    any modification.
C 5. Neither CCLRC nor AEA Technology plc shall be liable for any
C    direct or consequential loss or damage whatsoever arising out of
C    the use of Packages by the Licensee.
C *******************************************************************

*######DATE 13 Jan 1993
C       Toolpack tool decs employed.
C       MA27SD reference removed - replaced for the vax (9/12/93).
C       MA27ED and MA27FD not used in MA27CD.
C       SAVE statements added.
C       DATA statements made PARAMETER.
C 19/3/98 If there is an error (IFLAG<0), NSTEPS is set to zero and no
C     monitor printing is produced on exit. Extra tests for NZ=0 added.
C 16/4/98 Small alteration to format of monitor printing
C 7/12/98 Real copy of NELIM used in OPS calculation.
C 26/3/99 Special treatment of full and near-full rows added to MA27AD.
C 29/3/99 Test on NSTEPS added to MA27BD.
C 29/3/00 Test on absolute size of pivots added (MA27TD/PIVTOL).
C 3/7/00  Bug associated with 26/3/99 changes corrected in MA27HD.

      SUBROUTINE MA27AD(N,NZ,IRN,ICN,IW,LIW,IKEEP,IW1,NSTEPS,IFLAG)
      INTEGER IFLAG,LIW,N,NSTEPS,NZ
      INTEGER ICN(*),IKEEP(N,3),IRN(*),IW(LIW),IW1(N,2)
      INTEGER I,IWFR,K,L1,L2,LLIW
      EXTERNAL MA27GD,MA27HD,MA27JD,MA27KD,MA27LD,MA27MD,MA27SD
      INTRINSIC MIN0
      COMMON /MA27DD/U,LP,MP,LDIAG
      COMMON /MA27ED/OPS,IERROR,NRLTOT,NIRTOT,NRLNEC,NIRNEC,NRLADU,
     +       NIRADU,NRLBDU,NIRBDU,NCMPA,NCMPBR,NCMPBI,NTWO,NEIG
      COMMON /MA27FD/IOVFLO,NEMIN,IFRLVL(20)
      DOUBLE PRECISION OPS,U
      INTEGER IERROR,IOVFLO,LDIAG,LP,MP,NCMPA,NCMPBI,NCMPBR,NEIG,NEMIN,
     +        NIRADU,NIRBDU,NIRNEC,NIRTOT,NRLADU,NRLBDU,NRLNEC,NRLTOT,
     +        NTWO
      INTEGER IFRLVL
      SAVE /MA27DD/,/MA27ED/,/MA27FD/
      IF (LDIAG.LE.0 .OR. MP.LE.0) GO TO 40
      WRITE (MP,FMT=10) N,NZ,LIW,IFLAG
   10 FORMAT(/,/,' ENTERING MA27AD WITH      N     NZ      LIW  IFLAG',
     +       /,21X,I7,I7,I9,I7)
      NSTEPS = 0
      K = MIN0(8,NZ)
      IF (LDIAG.GT.1) K = NZ
      IF (K.GT.0) WRITE (MP,FMT=20) (IRN(I),ICN(I),I=1,K)
   20 FORMAT (17H MATRIX NON-ZEROS,/,4 (I9,I6),/,
     +       (I9,I6,I9,I6,I9,I6,I9,I6))
      K = MIN0(10,N)
      IF (LDIAG.GT.1) K = N
      IF (IFLAG.EQ.1 .AND. K.GT.0) WRITE (MP,FMT=30) (IKEEP(I,1),I=1,K)
   30 FORMAT (12H IKEEP(.,1)=,10I6,/, (12X,10I6))
   40 IF (N.LT.1 .OR. N.GT.IOVFLO) GO TO 70
      IF (NZ.LT.0) GO TO 100
      LLIW = LIW - 2*N
      L1 = LLIW + 1
      L2 = L1 + N
      IF (IFLAG.EQ.1) GO TO 50
      IF (LIW.LT.2*NZ+3*N+1) GO TO 130
      IFLAG = 0
      CALL MA27GD(N,NZ,IRN,ICN,IW,LLIW,IW1,IW1(1,2),IW(L1),IWFR,IFLAG)
      CALL MA27HD(N,IW1,IW,LLIW,IWFR,IW(L1),IW(L2),IKEEP(1,2),
     +            IKEEP(1,3),IKEEP)
      GO TO 60
   50 IF (LIW.LT.NZ+3*N+1) GO TO 120
      CALL MA27JD(N,NZ,IRN,ICN,IKEEP,IW,LLIW,IW1,IW1(1,2),IW(L1),IWFR,
     +            IFLAG)
      CALL MA27KD(N,IW1,IW,LLIW,IWFR,IKEEP,IKEEP(1,2),IW(L1),IW(L2))
   60 CALL MA27LD(N,IW1,IW(L1),IKEEP,IKEEP(1,2),IKEEP(1,3),IW(L2),
     +            NSTEPS)
      IF(NZ.GE.1) IW(1) = IRN(1) + 1
      CALL MA27MD(N,NZ,IRN,ICN,IKEEP,IKEEP(1,3),IKEEP(1,2),IW(L2),
     +            NSTEPS,IW1,IW1(1,2),IW)
      GO TO 160
   70 IFLAG = -1
      IF (LP.GT.0) WRITE (LP,FMT=80) IFLAG
   80 FORMAT (42H **** ERROR RETURN FROM MA27AD **** IFLAG=,I3)
      IF (LP.GT.0) WRITE (LP,FMT=90) N
   90 FORMAT (30H VALUE OF N OUT OF RANGE ... =,I10)
      GO TO 160
  100 IFLAG = -2
      IF (LP.GT.0) WRITE (LP,FMT=80) IFLAG
      IF (LP.GT.0) WRITE (LP,FMT=110) NZ
  110 FORMAT (30H VALUE OF NZ OUT OF RANGE .. =,I10)
      GO TO 160
  120 IERROR = NZ + 3*N + 1
      GO TO 140
  130 IERROR = 2*NZ + 3*N + 1
  140 IFLAG = -3
      IF (LP.GT.0) WRITE (LP,FMT=80) IFLAG
      IF (LP.GT.0) WRITE (LP,FMT=150) LIW,IERROR
  150 FORMAT (38H LIW TOO SMALL, MUST BE INCREASED FROM,I10,3H TO,
     +       9H AT LEAST,I10)
  160 IF (LDIAG.LE.0 .OR. MP.LE.0 .OR. IFLAG.LT.0) GO TO 200
      WRITE (MP,FMT=170) NSTEPS,IFLAG,OPS,IERROR,NRLTOT,NIRTOT,NRLNEC,
     +  NIRNEC,NRLADU,NIRADU,NCMPA
  170 FORMAT (/,53H LEAVING MA27AD WITH NSTEPS  IFLAG    OPS IERROR NRLT
     +       ,2HOT,7H NIRTOT,/,20X,2I7,F7.0,3I7,/,20X,
     +       20H NRLNEC NIRNEC NRLAD,15HU NIRADU  NCMPA,/,20X,6I7)
      K = MIN0(9,N)
      IF (LDIAG.GT.1) K = N
      IF (K.GT.0) WRITE (MP,FMT=30) (IKEEP(I,1),I=1,K)
      K = MIN0(K,NSTEPS)
      IF (K.GT.0) WRITE (MP,FMT=180) (IKEEP(I,2),I=1,K)
  180 FORMAT (12H IKEEP(.,2)=,10I6,/, (12X,10I6))
      IF (K.GT.0) WRITE (MP,FMT=190) (IKEEP(I,3),I=1,K)
  190 FORMAT (12H IKEEP(.,3)=,10I6,/, (12X,10I6))
  200 RETURN
      END
      SUBROUTINE MA27BD(N,NZ,IRN,ICN,A,LA,IW,LIW,IKEEP,NSTEPS,MAXFRT,
     +                  IW1,IFLAG)
      INTEGER IFLAG,LA,LIW,MAXFRT,N,NSTEPS,NZ
      DOUBLE PRECISION A(LA)
      INTEGER ICN(*),IKEEP(N,3),IRN(*),IW(LIW),IW1(N)
      INTEGER I,IAPOS,IBLK,IPOS,IROWS,J1,J2,JJ,K,KBLK,KZ,LEN,NCOLS,
     +        NROWS,NZ1
      EXTERNAL MA27ND,MA27OD
      INTRINSIC IABS,MIN0
      COMMON /MA27DD/U,LP,MP,LDIAG
      COMMON /MA27ED/OPS,IERROR,NRLTOT,NIRTOT,NRLNEC,NIRNEC,NRLADU,
     +       NIRADU,NRLBDU,NIRBDU,NCMPA,NCMPBR,NCMPBI,NTWO,NEIG
      COMMON /MA27FD/IOVFLO,NEMIN,IFRLVL(20)
      DOUBLE PRECISION OPS,U
      INTEGER IERROR,IOVFLO,LDIAG,LP,MP,NCMPA,NCMPBI,NCMPBR,NEIG,NEMIN,
     +        NIRADU,NIRBDU,NIRNEC,NIRTOT,NRLADU,NRLBDU,NRLNEC,NRLTOT,
     +        NTWO
      INTEGER IFRLVL
      SAVE /MA27DD/,/MA27ED/,/MA27FD/
      IF (LDIAG.LE.0 .OR. MP.LE.0) GO TO 60
      WRITE (MP,FMT=10) N,NZ,LA,LIW,NSTEPS,U
   10 FORMAT (/,/,
     + ' ENTERING MA27BD WITH      N     NZ       LA      LIW',
     +       ' NSTEPS      U',/,21X,I7,I7,I9,I9,I7,1PE10.2)
      KZ = MIN0(6,NZ)
      IF (LDIAG.GT.1) KZ = NZ
      IF (NZ.GT.0) WRITE (MP,FMT=20) (A(K),IRN(K),ICN(K),K=1,KZ)
   20 FORMAT (17H MATRIX NON-ZEROS,/,1X,2 (1P,D16.3,2I6),/,
     +       (1X,1P,D16.3,2I6,1P,D16.3,2I6))
      K = MIN0(9,N)
      IF (LDIAG.GT.1) K = N
      IF (K.GT.0) WRITE (MP,FMT=30) (IKEEP(I,1),I=1,K)
   30 FORMAT (12H IKEEP(.,1)=,10I6,/, (12X,10I6))
      K = MIN0(K,NSTEPS)
      IF (K.GT.0) WRITE (MP,FMT=40) (IKEEP(I,2),I=1,K)
   40 FORMAT (12H IKEEP(.,2)=,10I6,/, (12X,10I6))
      IF (K.GT.0) WRITE (MP,FMT=50) (IKEEP(I,3),I=1,K)
   50 FORMAT (12H IKEEP(.,3)=,10I6,/, (12X,10I6))
   60 IF (N.LT.1 .OR. N.GT.IOVFLO) GO TO 70
      IF (NZ.LT.0) GO TO 100
      IF (LIW.LT.NZ) GO TO 120
      IF (LA.LT.NZ+N) GO TO 150
      IF (NSTEPS.LT.1 .OR. NSTEPS.GT.N) GO TO 175
      CALL MA27ND(N,NZ,NZ1,A,LA,IRN,ICN,IW,LIW,IKEEP,IW1,IFLAG)
      IF (IFLAG.EQ.-3) GO TO 130
      IF (IFLAG.EQ.-4) GO TO 160
      CALL MA27OD(N,NZ1,A,LA,IW,LIW,IKEEP,IKEEP(1,3),NSTEPS,MAXFRT,
     +            IKEEP(1,2),IW1,IFLAG)
      IF (IFLAG.EQ.-3) GO TO 130
      IF (IFLAG.EQ.-4) GO TO 160
      IF (IFLAG.EQ.-5) GO TO 180
      IF (IFLAG.EQ.-6) GO TO 200
      IF (IFLAG.EQ.3 .AND. MP.GT.0) WRITE (MP,FMT=65) IFLAG,IERROR
   65 FORMAT (52H *** WARNING MESSAGE FROM SUBROUTINE MA27BD *** IFLA,
     +       3HG =,I2,/,5X,25HMATRIX IS SINGULAR. RANK=,I5)
      GO TO 220
   70 IFLAG = -1
      IF (LP.GT.0) WRITE (LP,FMT=80) IFLAG
   80 FORMAT (42H **** ERROR RETURN FROM MA27BD **** IFLAG=,I3)
      IF (LP.GT.0) WRITE (LP,FMT=90) N
   90 FORMAT (30H VALUE OF N OUT OF RANGE ... =,I10)
      GO TO 220
  100 IFLAG = -2
      IF (LP.GT.0) WRITE (LP,FMT=80) IFLAG
      IF (LP.GT.0) WRITE (LP,FMT=110) NZ
  110 FORMAT (30H VALUE OF NZ OUT OF RANGE .. =,I10)
      GO TO 220
  120 IFLAG = -3
      IERROR = NZ
  130 IF (LP.GT.0) WRITE (LP,FMT=80) IFLAG
      IF (LP.GT.0) WRITE (LP,FMT=140) LIW,IERROR
  140 FORMAT (38H LIW TOO SMALL, MUST BE INCREASED FROM,I10,3H TO,
     +       9H AT LEAST,I10)
      GO TO 220
  150 IFLAG = -4
      IERROR = NZ + N
  160 IF (LP.GT.0) WRITE (LP,FMT=80) IFLAG
      IF (LP.GT.0) WRITE (LP,FMT=170) LA,IERROR
  170 FORMAT (38H LA TOO SMALL, MUST BE INCREASED FROM ,I10,3H TO,
     +       9H AT LEAST,I10)
      GO TO 220
  175 IFLAG = -7
      IF (LP.GT.0) WRITE (LP,FMT=80) IFLAG
      IF (LP.GT.0) WRITE (LP,FMT='(A)') ' NSTEPS is out of range'
      GO TO 220
  180 IF (LP.GT.0) WRITE (LP,FMT=80) IFLAG
      IF (LP.GT.0) WRITE (LP,FMT=190) IERROR
  190 FORMAT (20H ZERO PIVOT AT STAGE,I10,24H WHEN INPUT MATRIX DECLA,
     +       12HRED DEFINITE)
      GO TO 220
  200 IF (LP.GT.0) WRITE (LP,FMT=80) IFLAG
      IF (LP.GT.0) WRITE (LP,FMT=210)
  210 FORMAT (36H CHANGE IN SIGN OF PIVOT ENCOUNTERED,13H WHEN FACTORI,
     +       28HNG ALLEGEDLY DEFINITE MATRIX)
  220 IF (LDIAG.LE.0 .OR. MP.LE.0 .OR. IFLAG.LT.0) GO TO 310
      WRITE (MP,FMT=230) MAXFRT,IFLAG,NRLBDU,NIRBDU,NCMPBR,NCMPBI,NTWO,
     +  IERROR
  230 FORMAT (/,20H LEAVING MA27BD WITH,/,10X,
     +       36H  MAXFRT  IFLAG NRLBDU NIRBDU NCMPBR,
     +       21H NCMPBI   NTWO IERROR,/,11X,8I7)
      IF (IFLAG.LT.0) GO TO 310
      KBLK = IABS(IW(1)+0)
      IF (KBLK.EQ.0) GO TO 310
      IF (LDIAG.EQ.1) KBLK = 1
      IPOS = 2
      IAPOS = 1
      DO 300 IBLK = 1,KBLK
        NCOLS = IW(IPOS)
        NROWS = IW(IPOS+1)
        J1 = IPOS + 2
        IF (NCOLS.GT.0) GO TO 240
        NCOLS = -NCOLS
        NROWS = 1
        J1 = J1 - 1
  240   WRITE (MP,FMT=250) IBLK,NROWS,NCOLS
  250   FORMAT (14H BLOCK PIVOT =,I8,8H NROWS =,I8,8H NCOLS =,I8)
        J2 = J1 + NCOLS - 1
        IPOS = J2 + 1
        WRITE (MP,FMT=260) (IW(JJ),JJ=J1,J2)
  260   FORMAT (17H COLUMN INDICES =,10I6,/, (17X,10I6))
        WRITE (MP,FMT=270)
  270   FORMAT (46H REAL ENTRIES .. EACH ROW STARTS ON A NEW LINE)
        LEN = NCOLS
        DO 290 IROWS = 1,NROWS
          J1 = IAPOS
          J2 = IAPOS + LEN - 1
          WRITE (MP,FMT=280) (A(JJ),JJ=J1,J2)
  280     FORMAT (1P,5D13.3)
          LEN = LEN - 1
          IAPOS = J2 + 1
  290   CONTINUE
  300 CONTINUE
  310 RETURN
      END
      SUBROUTINE MA27CD(N,A,LA,IW,LIW,W,MAXFRT,RHS,IW1,NSTEPS)
      INTEGER LA,LIW,MAXFRT,N,NSTEPS
      DOUBLE PRECISION A(LA),RHS(N),W(MAXFRT)
      INTEGER IW(LIW),IW1(NSTEPS)
      INTEGER I,IAPOS,IBLK,IPOS,IROWS,J1,J2,JJ,K,KBLK,LATOP,LEN,NBLK,
     +        NCOLS,NROWS
      EXTERNAL MA27QD,MA27RD
      INTRINSIC IABS,MIN0
      COMMON /MA27DD/U,LP,MP,LDIAG
      DOUBLE PRECISION U
      INTEGER LDIAG,LP,MP
      SAVE /MA27DD/
      IF (LDIAG.LE.0 .OR. MP.LE.0) GO TO 110
      WRITE (MP,FMT=10) N,LA,LIW,MAXFRT,NSTEPS
   10 FORMAT (/,/,49H ENTERING MA27CD WITH      N     LA    LIW MAXFRT,
     +       8H  NSTEPS,/,21X,5I7)
      KBLK = IABS(IW(1)+0)
      IF (KBLK.EQ.0) GO TO 90
      IF (LDIAG.EQ.1) KBLK = 1
      IPOS = 2
      IAPOS = 1
      DO 80 IBLK = 1,KBLK
        NCOLS = IW(IPOS)
        NROWS = IW(IPOS+1)
        J1 = IPOS + 2
        IF (NCOLS.GT.0) GO TO 20
        NCOLS = -NCOLS
        NROWS = 1
        J1 = J1 - 1
   20   WRITE (MP,FMT=30) IBLK,NROWS,NCOLS
   30   FORMAT (14H BLOCK PIVOT =,I8,8H NROWS =,I8,8H NCOLS =,I8)
        J2 = J1 + NCOLS - 1
        IPOS = J2 + 1
        WRITE (MP,FMT=40) (IW(JJ),JJ=J1,J2)
   40   FORMAT (17H COLUMN INDICES =,10I6,/, (17X,10I6))
        WRITE (MP,FMT=50)
   50   FORMAT (46H REAL ENTRIES .. EACH ROW STARTS ON A NEW LINE)
        LEN = NCOLS
        DO 70 IROWS = 1,NROWS
          J1 = IAPOS
          J2 = IAPOS + LEN - 1
          WRITE (MP,FMT=60) (A(JJ),JJ=J1,J2)
   60     FORMAT (1P,5D13.3)
          LEN = LEN - 1
          IAPOS = J2 + 1
   70   CONTINUE
   80 CONTINUE
   90 K = MIN0(10,N)
      IF (LDIAG.GT.1) K = N
      IF (N.GT.0) WRITE (MP,FMT=100) (RHS(I),I=1,K)
  100 FORMAT (4H RHS,1P,5D13.3,/, (4X,1P,5D13.3))
  110 IF (IW(1).LT.0) GO TO 130
      NBLK = IW(1)
      IF (NBLK.GT.0) GO TO 140
      DO 120 I = 1,N
        RHS(I) = 0.0D0
  120 CONTINUE
      GO TO 150
  130 NBLK = -IW(1)
  140 CALL MA27QD(N,A,LA,IW(2),LIW-1,W,MAXFRT,RHS,IW1,NBLK,LATOP)
      CALL MA27RD(N,A,LA,IW(2),LIW-1,W,MAXFRT,RHS,IW1,NBLK,LATOP)
  150 IF (LDIAG.LE.0 .OR. MP.LE.0) GO TO 170
      WRITE (MP,FMT=160)
  160 FORMAT (/,/,20H LEAVING MA27CD WITH)
      IF (N.GT.0) WRITE (MP,FMT=100) (RHS(I),I=1,K)
  170 RETURN
      END
      BLOCK DATA MA27SD
      COMMON /MA27DD/U,LP,MP,LDIAG
      COMMON /MA27FD/IOVFLO,NEMIN,IFRLVL(20)
      DOUBLE PRECISION U
      INTEGER IOVFLO,LDIAG,LP,MP,NEMIN
      INTEGER IFRLVL
      COMMON /MA27TD/FRATIO,PIVTOL
      DOUBLE PRECISION FRATIO,PIVTOL
      SAVE /MA27DD/,/MA27FD/,/MA27TD/
      DATA U/0.1D0/,LP/6/,MP/6/,LDIAG/0/
      DATA FRATIO/1.0D0/, PIVTOL/0.0D0/
      DATA IOVFLO/2139062143/,NEMIN/1/,
     +     IFRLVL(1),IFRLVL(2),IFRLVL(3),IFRLVL(4),IFRLVL(5),IFRLVL(6),
     +     IFRLVL(7),IFRLVL(8),IFRLVL(9),IFRLVL(10),IFRLVL(11),
     +     IFRLVL(12),IFRLVL(13),IFRLVL(14),IFRLVL(15),IFRLVL(16),
     +     IFRLVL(17),IFRLVL(18),IFRLVL(19),IFRLVL(20)/32639,32639,
     +     32639,32639,14,9,8,8,9,10,32639,32639,32639,32689,24,11,9,8,
     +     9,10/
      END
      SUBROUTINE MA27GD(N,NZ,IRN,ICN,IW,LW,IPE,IQ,FLAG,IWFR,IFLAG)
      INTEGER IFLAG,IWFR,LW,N,NZ
      INTEGER FLAG(N),ICN(*),IPE(N),IQ(N),IRN(*),IW(LW)
      INTEGER I,ID,J,JN,K,K1,K2,L,LAST,LR,N1,NDUP
      COMMON /MA27DD/U,LP,MP,LDIAG
      COMMON /MA27ED/OPS,IERROR,NRLTOT,NIRTOT,NRLNEC,NIRNEC,NRLADU,
     +       NIRADU,NRLBDU,NIRBDU,NCMPA,NCMPBR,NCMPBI,NTWO,NEIG
      DOUBLE PRECISION OPS,U
      INTEGER IERROR,LDIAG,LP,MP,NCMPA,NCMPBI,NCMPBR,NEIG,NIRADU,NIRBDU,
     +        NIRNEC,NIRTOT,NRLADU,NRLBDU,NRLNEC,NRLTOT,NTWO
      SAVE /MA27DD/,/MA27ED/
      IERROR = 0
      DO 10 I = 1,N
        IPE(I) = 0
   10 CONTINUE
      LR = NZ
      IF (NZ.EQ.0) GO TO 120
      DO 110 K = 1,NZ
        I = IRN(K)
        J = ICN(K)
        IF (I-J) 20,40,30
   20   IF (I.GE.1 .AND. J.LE.N) GO TO 90
        GO TO 50
   30   IF (J.GE.1 .AND. I.LE.N) GO TO 90
        GO TO 50
   40   IF (I.GE.1 .AND. I.LE.N) GO TO 80
   50   IERROR = IERROR + 1
        IFLAG = 1
        IF (IERROR.LE.1 .AND. MP.GT.0) WRITE (MP,FMT=60) IFLAG
   60   FORMAT (52H *** WARNING MESSAGE FROM SUBROUTINE MA27AD *** IFLA,
     +         3HG =,I2)
        IF (IERROR.LE.10 .AND. MP.GT.0) WRITE (MP,FMT=70) K,I,J
   70   FORMAT (I6,19HTH NON-ZERO (IN ROW,I6,11H AND COLUMN,I6,
     +         9H) IGNORED)
   80   I = 0
        J = 0
        GO TO 100
   90   IPE(I) = IPE(I) + 1
        IPE(J) = IPE(J) + 1
  100   IW(K) = J
        LR = LR + 1
        IW(LR) = I
  110 CONTINUE
  120 IQ(1) = 1
      N1 = N - 1
      IF (N1.LE.0) GO TO 140
      DO 130 I = 1,N1
        FLAG(I) = 0
        IF (IPE(I).EQ.0) IPE(I) = -1
        IQ(I+1) = IPE(I) + IQ(I) + 1
        IPE(I) = IQ(I)
  130 CONTINUE
  140 LAST = IPE(N) + IQ(N)
      FLAG(N) = 0
      IF (LR.GE.LAST) GO TO 160
      K1 = LR + 1
      DO 150 K = K1,LAST
        IW(K) = 0
  150 CONTINUE
  160 IPE(N) = IQ(N)
      IWFR = LAST + 1
      IF (NZ.EQ.0) GO TO 230
      DO 220 K = 1,NZ
        J = IW(K)
        IF (J.LE.0) GO TO 220
        L = K
        IW(K) = 0
        DO 210 ID = 1,NZ
          IF (L.GT.NZ) GO TO 170
          L = L + NZ
          GO TO 180
  170     L = L - NZ
  180     I = IW(L)
          IW(L) = 0
          IF (I.LT.J) GO TO 190
          L = IQ(J) + 1
          IQ(J) = L
          JN = IW(L)
          IW(L) = -I
          GO TO 200
  190     L = IQ(I) + 1
          IQ(I) = L
          JN = IW(L)
          IW(L) = -J
  200     J = JN
          IF (J.LE.0) GO TO 220
  210   CONTINUE
  220 CONTINUE
  230 NDUP = 0
      DO 280 I = 1,N
        K1 = IPE(I) + 1
        K2 = IQ(I)
        IF (K1.LE.K2) GO TO 240
        IPE(I) = 0
        IQ(I) = 0
        GO TO 280
  240   DO 260 K = K1,K2
          J = -IW(K)
          IF (J.LE.0) GO TO 270
          L = IQ(J) + 1
          IQ(J) = L
          IW(L) = I
          IW(K) = J
          IF (FLAG(J).NE.I) GO TO 250
          NDUP = NDUP + 1
          IW(L) = 0
          IW(K) = 0
  250     FLAG(J) = I
  260   CONTINUE
  270   IQ(I) = IQ(I) - IPE(I)
        IF (NDUP.EQ.0) IW(K1-1) = IQ(I)
  280 CONTINUE
      IF (NDUP.EQ.0) GO TO 310
      IWFR = 1
      DO 300 I = 1,N
        K1 = IPE(I) + 1
        IF (K1.EQ.1) GO TO 300
        K2 = IQ(I) + IPE(I)
        L = IWFR
        IPE(I) = IWFR
        IWFR = IWFR + 1
        DO 290 K = K1,K2
          IF (IW(K).EQ.0) GO TO 290
          IW(IWFR) = IW(K)
          IWFR = IWFR + 1
  290   CONTINUE
        IW(L) = IWFR - L - 1
  300 CONTINUE
  310 RETURN
      END
      SUBROUTINE MA27HD(N,IPE,IW,LW,IWFR,NV,NXT,LST,IPD,FLAG)
      INTEGER IWFR,LW,N
      INTEGER FLAG(N),IPD(N),IPE(N),IW(LW),LST(N),NV(N),NXT(N)
      INTEGER I,ID,IDL,IDN,IE,IP,IS,JP,JP1,JP2,JS,K,K1,K2,KE,KP,KP0,KP1,
     +        KP2,KS,L,LEN,LIMIT,LN,LS,LWFR,MD,ME,ML,MS,NEL,NFLG,NP,
     +        NP0,NS,NVPIV,NVROOT,ROOT
      EXTERNAL MA27ID
      INTRINSIC IABS,MIN0
      COMMON /MA27ED/OPS,IERROR,NRLTOT,NIRTOT,NRLNEC,NIRNEC,NRLADU,
     +       NIRADU,NRLBDU,NIRBDU,NCMPA,NCMPBR,NCMPBI,NTWO,NEIG
      COMMON /MA27FD/IOVFLO,NEMIN,IFRLVL(20)
      DOUBLE PRECISION OPS
      INTEGER IERROR,IOVFLO,NCMPA,NCMPBI,NCMPBR,NEIG,NEMIN,NIRADU,
     +        NIRBDU,NIRNEC,NIRTOT,NRLADU,NRLBDU,NRLNEC,NRLTOT,NTWO
      INTEGER IFRLVL
      COMMON /MA27TD/FRATIO,PIVTOL
      DOUBLE PRECISION FRATIO,PIVTOL
      SAVE /MA27ED/,/MA27FD/,/MA27TD/
      DO 10 I = 1,N
        IPD(I) = 0
        NV(I) = 1
        FLAG(I) = IOVFLO
   10 CONTINUE
      MD = 1
      NCMPA = 0
      NFLG = IOVFLO
      NEL = 0
      ROOT = N+1
      NVROOT = 0
      DO 30 IS = 1,N
        K = IPE(IS)
        IF (K.LE.0) GO TO 20
        ID = IW(K) + 1
        NS = IPD(ID)
        IF (NS.GT.0) LST(NS) = IS
        NXT(IS) = NS
        IPD(ID) = IS
        LST(IS) = -ID
        GO TO 30
   20   NEL = NEL + 1
        FLAG(IS) = -1
        NXT(IS) = 0
        LST(IS) = 0
   30 CONTINUE
      DO 340 ML = 1,N
        IF (NEL+NVROOT+1.GE.N) GO TO 350
        DO 40 ID = MD,N
          MS = IPD(ID)
          IF (MS.GT.0) GO TO 50
   40   CONTINUE
   50   MD = ID
        NVPIV = NV(MS)
        NS = NXT(MS)
        NXT(MS) = 0
        LST(MS) = 0
        IF (NS.GT.0) LST(NS) = -ID
        IPD(ID) = NS
        ME = MS
        NEL = NEL + NVPIV
        IDN = 0
        KP = IPE(ME)
        FLAG(MS) = -1
        IP = IWFR
        LEN = IW(KP)
        DO 140 KP1 = 1,LEN
          KP = KP + 1
          KE = IW(KP)
          IF (FLAG(KE).LE.-2) GO TO 60
          IF (FLAG(KE).LE.0) THEN
             IF (IPE(KE).NE.-ROOT) GO TO 140
             KE = ROOT
             IF (FLAG(KE).LE.0) GO TO 140
          END IF
          JP = KP - 1
          LN = LEN - KP1 + 1
          IE = MS
          GO TO 70
   60     IE = KE
          JP = IPE(IE)
          LN = IW(JP)
   70     DO 130 JP1 = 1,LN
            JP = JP + 1
            IS = IW(JP)
            IF (FLAG(IS).LE.0) THEN
               IF (IPE(IS).EQ.-ROOT) THEN
                  IS = ROOT
                  IW(JP) = ROOT
                  IF (FLAG(IS).LE.0) GO TO 130
               ELSE
                  GO TO 130
               END IF
            END IF
            FLAG(IS) = 0
            IF (IWFR.LT.LW) GO TO 100
            IPE(MS) = KP
            IW(KP) = LEN - KP1
            IPE(IE) = JP
            IW(JP) = LN - JP1
            CALL MA27ID(N,IPE,IW,IP-1,LWFR)
            JP2 = IWFR - 1
            IWFR = LWFR
            IF (IP.GT.JP2) GO TO 90
            DO 80 JP = IP,JP2
              IW(IWFR) = IW(JP)
              IWFR = IWFR + 1
   80       CONTINUE
   90       IP = LWFR
            JP = IPE(IE)
            KP = IPE(ME)
  100       IW(IWFR) = IS
            IDN = IDN + NV(IS)
            IWFR = IWFR + 1
            LS = LST(IS)
            LST(IS) = 0
            NS = NXT(IS)
            NXT(IS) = 0
            IF (NS.GT.0) LST(NS) = LS
            IF (LS) 110,130,120
  110       LS = -LS
            IPD(LS) = NS
            GO TO 130
  120       NXT(LS) = NS
  130     CONTINUE
          IF (IE.EQ.MS) GO TO 150
          IPE(IE) = -ME
          FLAG(IE) = -1
  140   CONTINUE
  150   NV(MS) = IDN + NVPIV
        IF (IWFR.EQ.IP) GO TO 330
        K1 = IP
        K2 = IWFR - 1
        LIMIT = NINT(FRATIO*(N-NEL))
        DO 310 K = K1,K2
          IS = IW(K)
          IF (IS.EQ.ROOT) GO TO 310
          IF (NFLG.GT.2) GO TO 170
          DO 160 I = 1,N
            IF (FLAG(I).GT.0) FLAG(I) = IOVFLO
            IF (FLAG(I).LE.-2) FLAG(I) = -IOVFLO
  160     CONTINUE
          NFLG = IOVFLO
  170     NFLG = NFLG - 1
          ID = IDN
          KP1 = IPE(IS) + 1
          NP = KP1
          KP2 = IW(KP1-1) + KP1 - 1
          DO 220 KP = KP1,KP2
            KE = IW(KP)
          IF (FLAG(KE).EQ.-1) THEN
             IF (IPE(KE).NE.-ROOT) GO TO 220
             KE = ROOT
             IW(KP) = ROOT
             IF (FLAG(KE).EQ.-1) GO TO 220
          END IF
          IF (FLAG(KE).GE.0) GO TO 230
            JP1 = IPE(KE) + 1
            JP2 = IW(JP1-1) + JP1 - 1
            IDL = ID
            DO 190 JP = JP1,JP2
              JS = IW(JP)
              IF (FLAG(JS).LE.NFLG) GO TO 190
              ID = ID + NV(JS)
              FLAG(JS) = NFLG
  190       CONTINUE
            IF (ID.GT.IDL) GO TO 210
            DO 200 JP = JP1,JP2
              JS = IW(JP)
              IF (FLAG(JS).NE.0) GO TO 210
  200       CONTINUE
            IPE(KE) = -ME
            FLAG(KE) = -1
            GO TO 220
  210       IW(NP) = KE
            FLAG(KE) = -NFLG
            NP = NP + 1
  220     CONTINUE
          NP0 = NP
          GO TO 250
  230     KP0 = KP
          NP0 = NP
          DO 240 KP = KP0,KP2
            KS = IW(KP)
            IF (FLAG(KS).LE.NFLG) THEN
               IF (IPE(KS).EQ.-ROOT) THEN
                  KS = ROOT
                  IW(KP) = ROOT
                  IF (FLAG(KS).LE.NFLG) GO TO 240
               ELSE
                  GO TO 240
               END IF
            END IF
            ID = ID + NV(KS)
            FLAG(KS) = NFLG
            IW(NP) = KS
            NP = NP + 1
  240     CONTINUE
  250     IF (ID.GE.LIMIT) GO TO 295
          IW(NP) = IW(NP0)
          IW(NP0) = IW(KP1)
          IW(KP1) = ME
          IW(KP1-1) = NP - KP1 + 1
          JS = IPD(ID)
          DO 280 L = 1,N
            IF (JS.LE.0) GO TO 300
            KP1 = IPE(JS) + 1
            IF (IW(KP1).NE.ME) GO TO 300
            KP2 = KP1 - 1 + IW(KP1-1)
            DO 260 KP = KP1,KP2
              IE = IW(KP)
              IF (IABS(FLAG(IE)+0).GT.NFLG) GO TO 270
  260       CONTINUE
            GO TO 290
  270       JS = NXT(JS)
  280     CONTINUE
  290     IPE(JS) = -IS
          NV(IS) = NV(IS) + NV(JS)
          NV(JS) = 0
          FLAG(JS) = -1
          NS = NXT(JS)
          LS = LST(JS)
          IF (NS.GT.0) LST(NS) = IS
          IF (LS.GT.0) NXT(LS) = IS
          LST(IS) = LS
          NXT(IS) = NS
          LST(JS) = 0
          NXT(JS) = 0
          IF (IPD(ID).EQ.JS) IPD(ID) = IS
          GO TO 310
  295     IF (NVROOT.EQ.0) THEN
            ROOT = IS
            IPE(IS) = 0
          ELSE
            IW(K) = ROOT
            IPE(IS) = -ROOT
            NV(ROOT) = NV(ROOT) + NV(IS)
            NV(IS) = 0
            FLAG(IS) = -1
          END IF
          NVROOT = NV(ROOT)
          GO TO 310
  300     NS = IPD(ID)
          IF (NS.GT.0) LST(NS) = IS
          NXT(IS) = NS
          IPD(ID) = IS
          LST(IS) = -ID
          MD = MIN0(MD,ID)
  310   CONTINUE
        DO 320 K = K1,K2
          IS = IW(K)
          IF (NV(IS).EQ.0) GO TO 320
          FLAG(IS) = NFLG
          IW(IP) = IS
          IP = IP + 1
  320   CONTINUE
        IWFR = K1
        FLAG(ME) = -NFLG
        IW(IP) = IW(K1)
        IW(K1) = IP - K1
        IPE(ME) = K1
        IWFR = IP + 1
        GO TO 335
  330   IPE(ME) = 0
  335   CONTINUE
  340 CONTINUE
  350 DO 360 IS = 1,N
        IF(NXT(IS).NE.0 .OR. LST(IS).NE.0) THEN
          IF (NVROOT.EQ.0) THEN
            ROOT = IS
            IPE(IS) = 0
          ELSE
            IPE(IS) = -ROOT
          END IF
          NVROOT = NVROOT + NV(IS)
          NV(IS) = 0
         END IF
  360 CONTINUE
      DO 370 IE = 1,N
        IF (IPE(IE).GT.0) IPE(IE) = -ROOT
  370 CONTINUE
      IF(NVROOT.GT.0)NV(ROOT)=NVROOT
      END
      SUBROUTINE MA27ID(N,IPE,IW,LW,IWFR)
      INTEGER IWFR,LW,N
      INTEGER IPE(N),IW(LW)
      INTEGER I,IR,K,K1,K2,LWFR
      COMMON /MA27ED/OPS,IERROR,NRLTOT,NIRTOT,NRLNEC,NIRNEC,NRLADU,
     +       NIRADU,NRLBDU,NIRBDU,NCMPA,NCMPBR,NCMPBI,NTWO,NEIG
      DOUBLE PRECISION OPS
      INTEGER IERROR,NCMPA,NCMPBI,NCMPBR,NEIG,NIRADU,NIRBDU,NIRNEC,
     +        NIRTOT,NRLADU,NRLBDU,NRLNEC,NRLTOT,NTWO
      SAVE /MA27ED/
      NCMPA = NCMPA + 1
      DO 10 I = 1,N
        K1 = IPE(I)
        IF (K1.LE.0) GO TO 10
        IPE(I) = IW(K1)
        IW(K1) = -I
   10 CONTINUE
      IWFR = 1
      LWFR = IWFR
      DO 60 IR = 1,N
        IF (LWFR.GT.LW) GO TO 70
        DO 20 K = LWFR,LW
          IF (IW(K).LT.0) GO TO 30
   20   CONTINUE
        GO TO 70
   30   I = -IW(K)
        IW(IWFR) = IPE(I)
        IPE(I) = IWFR
        K1 = K + 1
        K2 = K + IW(IWFR)
        IWFR = IWFR + 1
        IF (K1.GT.K2) GO TO 50
        DO 40 K = K1,K2
          IW(IWFR) = IW(K)
          IWFR = IWFR + 1
   40   CONTINUE
   50   LWFR = K2 + 1
   60 CONTINUE
   70 RETURN
      END
      SUBROUTINE MA27JD(N,NZ,IRN,ICN,PERM,IW,LW,IPE,IQ,FLAG,IWFR,IFLAG)
      INTEGER IFLAG,IWFR,LW,N,NZ
      INTEGER FLAG(N),ICN(*),IPE(N),IQ(N),IRN(*),IW(LW),PERM(N)
      INTEGER I,ID,IN,J,JDUMMY,K,K1,K2,L,LBIG,LEN
      INTRINSIC MAX0
      COMMON /MA27DD/U,LP,MP,LDIAG
      COMMON /MA27ED/OPS,IERROR,NRLTOT,NIRTOT,NRLNEC,NIRNEC,NRLADU,
     +       NIRADU,NRLBDU,NIRBDU,NCMPA,NCMPBR,NCMPBI,NTWO,NEIG
      COMMON /MA27FD/IOVFLO,NEMIN,IFRLVL(20)
      DOUBLE PRECISION OPS,U
      INTEGER IERROR,IOVFLO,LDIAG,LP,MP,NCMPA,NCMPBI,NCMPBR,NEIG,NEMIN,
     +        NIRADU,NIRBDU,NIRNEC,NIRTOT,NRLADU,NRLBDU,NRLNEC,NRLTOT,
     +        NTWO
      INTEGER IFRLVL
      SAVE /MA27DD/,/MA27ED/,/MA27FD/
      IFLAG = 0
      IERROR = 0
      DO 10 I = 1,N
        IQ(I) = 0
   10 CONTINUE
      IF (NZ.EQ.0) GO TO 110
      DO 100 K = 1,NZ
        I = IRN(K)
        J = ICN(K)
        IW(K) = -I
        IF (I-J) 20,40,30
   20   IF (I.GE.1 .AND. J.LE.N) GO TO 80
        GO TO 50
   30   IF (J.GE.1 .AND. I.LE.N) GO TO 80
        GO TO 50
   40   IW(K) = 0
        IF (I.GE.1 .AND. I.LE.N) GO TO 100
   50   IERROR = IERROR + 1
        IFLAG = 1
        IW(K) = 0
        IF (IERROR.LE.1 .AND. MP.GT.0) WRITE (MP,FMT=60) IFLAG
   60   FORMAT (52H *** WARNING MESSAGE FROM SUBROUTINE MA27AD *** IFLA,
     +         3HG =,I2)
        IF (IERROR.LE.10 .AND. MP.GT.0) WRITE (MP,FMT=70) K,I,J
   70   FORMAT (I6,19HTH NON-ZERO (IN ROW,I6,11H AND COLUMN,I6,
     +         9H) IGNORED)
        GO TO 100
   80   IF (PERM(J).GT.PERM(I)) GO TO 90
        IQ(J) = IQ(J) + 1
        GO TO 100
   90   IQ(I) = IQ(I) + 1
  100 CONTINUE
  110 IWFR = 1
      LBIG = 0
      DO 120 I = 1,N
        L = IQ(I)
        LBIG = MAX0(L,LBIG)
        IWFR = IWFR + L
        IPE(I) = IWFR - 1
  120 CONTINUE
      IF (NZ.EQ.0) GO TO 250
      DO 160 K = 1,NZ
        I = -IW(K)
        IF (I.LE.0) GO TO 160
        L = K
        IW(K) = 0
        DO 150 ID = 1,NZ
          J = ICN(L)
          IF (PERM(I).LT.PERM(J)) GO TO 130
          L = IPE(J)
          IPE(J) = L - 1
          IN = IW(L)
          IW(L) = I
          GO TO 140
  130     L = IPE(I)
          IPE(I) = L - 1
          IN = IW(L)
          IW(L) = J
  140     I = -IN
          IF (I.LE.0) GO TO 160
  150   CONTINUE
  160 CONTINUE
      K = IWFR - 1
      L = K + N
      IWFR = L + 1
      DO 190 I = 1,N
        FLAG(I) = 0
        J = N + 1 - I
        LEN = IQ(J)
        IF (LEN.LE.0) GO TO 180
        DO 170 JDUMMY = 1,LEN
          IW(L) = IW(K)
          K = K - 1
          L = L - 1
  170   CONTINUE
  180   IPE(J) = L
        L = L - 1
  190 CONTINUE
      IF (LBIG.GE.IOVFLO) GO TO 210
      DO 200 I = 1,N
        K = IPE(I)
        IW(K) = IQ(I)
        IF (IQ(I).EQ.0) IPE(I) = 0
  200 CONTINUE
      GO TO 250
  210 IWFR = 1
      DO 240 I = 1,N
        K1 = IPE(I) + 1
        K2 = IPE(I) + IQ(I)
        IF (K1.LE.K2) GO TO 220
        IPE(I) = 0
        GO TO 240
  220   IPE(I) = IWFR
        IWFR = IWFR + 1
        DO 230 K = K1,K2
          J = IW(K)
          IF (FLAG(J).EQ.I) GO TO 230
          IW(IWFR) = J
          IWFR = IWFR + 1
          FLAG(J) = I
  230   CONTINUE
        K = IPE(I)
        IW(K) = IWFR - K - 1
  240 CONTINUE
  250 RETURN
      END
      SUBROUTINE MA27KD(N,IPE,IW,LW,IWFR,IPS,IPV,NV,FLAG)
      INTEGER IWFR,LW,N
      INTEGER FLAG(N),IPE(N),IPS(N),IPV(N),IW(LW),NV(N)
      INTEGER I,IE,IP,J,JE,JP,JP1,JP2,JS,KDUMMY,LN,LWFR,ME,MINJS,ML,MS
      EXTERNAL MA27ID
      INTRINSIC MIN0
      COMMON /MA27ED/OPS,IERROR,NRLTOT,NIRTOT,NRLNEC,NIRNEC,NRLADU,
     +       NIRADU,NRLBDU,NIRBDU,NCMPA,NCMPBR,NCMPBI,NTWO,NEIG
      DOUBLE PRECISION OPS
      INTEGER IERROR,NCMPA,NCMPBI,NCMPBR,NEIG,NIRADU,NIRBDU,NIRNEC,
     +        NIRTOT,NRLADU,NRLBDU,NRLNEC,NRLTOT,NTWO
      SAVE /MA27ED/
      DO 10 I = 1,N
        FLAG(I) = 0
        NV(I) = 0
        J = IPS(I)
        IPV(J) = I
   10 CONTINUE
      NCMPA = 0
      DO 100 ML = 1,N
        MS = IPV(ML)
        ME = MS
        FLAG(MS) = ME
        IP = IWFR
        MINJS = N
        IE = ME
        DO 70 KDUMMY = 1,N
          JP = IPE(IE)
          LN = 0
          IF (JP.LE.0) GO TO 60
          LN = IW(JP)
          DO 50 JP1 = 1,LN
            JP = JP + 1
            JS = IW(JP)
            IF (FLAG(JS).EQ.ME) GO TO 50
            FLAG(JS) = ME
            IF (IWFR.LT.LW) GO TO 40
            IPE(IE) = JP
            IW(JP) = LN - JP1
            CALL MA27ID(N,IPE,IW,IP-1,LWFR)
            JP2 = IWFR - 1
            IWFR = LWFR
            IF (IP.GT.JP2) GO TO 30
            DO 20 JP = IP,JP2
              IW(IWFR) = IW(JP)
              IWFR = IWFR + 1
   20       CONTINUE
   30       IP = LWFR
            JP = IPE(IE)
   40       IW(IWFR) = JS
            MINJS = MIN0(MINJS,IPS(JS)+0)
            IWFR = IWFR + 1
   50     CONTINUE
   60     IPE(IE) = -ME
          JE = NV(IE)
          NV(IE) = LN + 1
          IE = JE
          IF (IE.EQ.0) GO TO 80
   70   CONTINUE
   80   IF (IWFR.GT.IP) GO TO 90
        IPE(ME) = 0
        NV(ME) = 1
        GO TO 100
   90   MINJS = IPV(MINJS)
        NV(ME) = NV(MINJS)
        NV(MINJS) = ME
        IW(IWFR) = IW(IP)
        IW(IP) = IWFR - IP
        IPE(ME) = IP
        IWFR = IWFR + 1
  100 CONTINUE
      RETURN
      END
      SUBROUTINE MA27LD(N,IPE,NV,IPS,NE,NA,ND,NSTEPS)
      INTEGER N,NSTEPS
      INTEGER IPE(N),IPS(N),NA(N),ND(N),NE(N),NV(N)
      INTEGER I,IB,IF,IL,IS,ISON,K,L,NR
      COMMON /MA27FD/IOVFLO,NEMIN,IFRLVL(20)
      INTEGER IOVFLO,NEMIN
      INTEGER IFRLVL
      SAVE /MA27FD/
      DO 10 I = 1,N
        IPS(I) = 0
        NE(I) = 0
   10 CONTINUE
      DO 20 I = 1,N
        IF (NV(I).GT.0) GO TO 20
        IF = -IPE(I)
        IS = -IPS(IF)
        IF (IS.GT.0) IPE(I) = IS
        IPS(IF) = -I
   20 CONTINUE
      NR = N + 1
      DO 50 I = 1,N
        IF (NV(I).LE.0) GO TO 50
        IF = -IPE(I)
        IF (IF.EQ.0) GO TO 40
        IS = -IPS(IF)
        IF (IS.LE.0) GO TO 30
        IPE(I) = IS
   30   IPS(IF) = -I
        GO TO 50
   40   NR = NR - 1
        NE(NR) = I
   50 CONTINUE
      IS = 1
      I = 0
      DO 160 K = 1,N
        IF (I.GT.0) GO TO 60
        I = NE(NR)
        NE(NR) = 0
        NR = NR + 1
        IL = N
        NA(N) = 0
   60   DO 70 L = 1,N
          IF (IPS(I).GE.0) GO TO 80
          ISON = -IPS(I)
          IPS(I) = 0
          I = ISON
          IL = IL - 1
          NA(IL) = 0
   70   CONTINUE
   80   IPS(I) = K
        NE(IS) = NE(IS) + 1
        IF (NV(I).LE.0) GO TO 120
        IF (IL.LT.N) NA(IL+1) = NA(IL+1) + 1
        NA(IS) = NA(IL)
        ND(IS) = NV(I)
        IF (NA(IS).NE.1) GO TO 90
        IF (ND(IS-1)-NE(IS-1).EQ.ND(IS)) GO TO 100
   90   IF (NE(IS).GE.NEMIN) GO TO 110
        IF (NA(IS).EQ.0) GO TO 110
        IF (NE(IS-1).GE.NEMIN) GO TO 110
  100   NA(IS-1) = NA(IS-1) + NA(IS) - 1
        ND(IS-1) = ND(IS) + NE(IS-1)
        NE(IS-1) = NE(IS) + NE(IS-1)
        NE(IS) = 0
        GO TO 120
  110   IS = IS + 1
  120   IB = IPE(I)
        IF (IB) 150,140,130
  130   NA(IL) = 0
  140   I = IB
        GO TO 160
  150   I = -IB
        IL = IL + 1
  160 CONTINUE
      NSTEPS = IS - 1
      RETURN
      END
      SUBROUTINE MA27MD(N,NZ,IRN,ICN,PERM,NA,NE,ND,NSTEPS,LSTKI,LSTKR,
     +                  IW)
      INTEGER N,NSTEPS,NZ
      INTEGER ICN(*),IRN(*),IW(*),LSTKI(N),LSTKR(N),NA(NSTEPS),
     +        ND(NSTEPS),NE(NSTEPS),PERM(N)
      INTEGER I,INEW,IOLD,IORG,IROW,ISTKI,ISTKR,ITOP,ITREE,JOLD,JORG,K,
     +        LSTK,NASSR,NELIM,NFR,NSTK,NUMORG,NZ1,NZ2
      DOUBLE PRECISION DELIM
      INTRINSIC MAX0,MIN0
      COMMON /MA27ED/OPS,IERROR,NRLTOT,NIRTOT,NRLNEC,NIRNEC,NRLADU,
     +       NIRADU,NRLBDU,NIRBDU,NCMPA,NCMPBR,NCMPBI,NTWO,NEIG
      DOUBLE PRECISION OPS
      INTEGER IERROR,NCMPA,NCMPBI,NCMPBR,NEIG,NIRADU,NIRBDU,NIRNEC,
     +        NIRTOT,NRLADU,NRLBDU,NRLNEC,NRLTOT,NTWO
      SAVE /MA27ED/
      IF (NZ.EQ.0) GO TO 20
      IF (IRN(1).NE.IW(1)) GO TO 20
      IRN(1) = IW(1) - 1
      NZ2 = 0
      DO 10 IOLD = 1,N
        INEW = PERM(IOLD)
        LSTKI(INEW) = LSTKR(IOLD) + 1
        NZ2 = NZ2 + LSTKR(IOLD)
   10 CONTINUE
      NZ1 = NZ2/2 + N
      NZ2 = NZ2 + N
      GO TO 60
   20 DO 30 I = 1,N
        LSTKI(I) = 1
   30 CONTINUE
      NZ1 = N
      IF (NZ.EQ.0) GO TO 50
      DO 40 I = 1,NZ
        IOLD = IRN(I)
        JOLD = ICN(I)
        IF (IOLD.LT.1 .OR. IOLD.GT.N) GO TO 40
        IF (JOLD.LT.1 .OR. JOLD.GT.N) GO TO 40
        IF (IOLD.EQ.JOLD) GO TO 40
        NZ1 = NZ1 + 1
        IROW = MIN0(PERM(IOLD)+0,PERM(JOLD)+0)
        LSTKI(IROW) = LSTKI(IROW) + 1
   40 CONTINUE
   50 NZ2 = NZ1
   60 ISTKI = 0
      ISTKR = 0
      OPS = 0.0D0
      NRLADU = 0
      NIRADU = 1
      NIRTOT = NZ1
      NRLTOT = NZ1
      NIRNEC = NZ2
      NRLNEC = NZ2
      NUMORG = 0
      ITOP = 0
      DO 100 ITREE = 1,NSTEPS
        NELIM = NE(ITREE)
        DELIM = NELIM
        NFR = ND(ITREE)
        NSTK = NA(ITREE)
        NASSR = NFR* (NFR+1)/2
        IF (NSTK.NE.0) NASSR = NASSR - LSTKR(ITOP) + 1
        NRLTOT = MAX0(NRLTOT,NRLADU+NASSR+ISTKR+NZ1)
        NIRTOT = MAX0(NIRTOT,NIRADU+NFR+2+ISTKI+NZ1)
        NRLNEC = MAX0(NRLNEC,NRLADU+NASSR+ISTKR+NZ2)
        NIRNEC = MAX0(NIRNEC,NIRADU+NFR+2+ISTKI+NZ2)
        DO 70 IORG = 1,NELIM
          JORG = NUMORG + IORG
          NZ2 = NZ2 - LSTKI(JORG)
   70   CONTINUE
        NUMORG = NUMORG + NELIM
        IF (NSTK.LE.0) GO TO 90
        DO 80 K = 1,NSTK
          LSTK = LSTKR(ITOP)
          ISTKR = ISTKR - LSTK
          LSTK = LSTKI(ITOP)
          ISTKI = ISTKI - LSTK
          ITOP = ITOP - 1
   80   CONTINUE
   90   NRLADU = NRLADU + (NELIM* (2*NFR-NELIM+1))/2
        NIRADU = NIRADU + 2 + NFR
        IF (NELIM.EQ.1) NIRADU = NIRADU - 1
        OPS = OPS + ((NFR*DELIM* (NFR+1)- (2*NFR+1)*DELIM* (DELIM+1)/2+
     +        DELIM* (DELIM+1)* (2*DELIM+1)/6)/2)
        IF (ITREE.EQ.NSTEPS) GO TO 100
        IF (NFR.EQ.NELIM) GO TO 100
        ITOP = ITOP + 1
        LSTKR(ITOP) = (NFR-NELIM)* (NFR-NELIM+1)/2
        LSTKI(ITOP) = NFR - NELIM + 1
        ISTKI = ISTKI + LSTKI(ITOP)
        ISTKR = ISTKR + LSTKR(ITOP)
        NIRTOT = MAX0(NIRTOT,NIRADU+ISTKI+NZ1)
        NIRNEC = MAX0(NIRNEC,NIRADU+ISTKI+NZ2)
  100 CONTINUE
      NRLNEC = MAX0(NRLNEC,N+MAX0(NZ,NZ1))
      NRLTOT = MAX0(NRLTOT,N+MAX0(NZ,NZ1))
      NRLNEC = MIN0(NRLNEC,NRLTOT)
      NIRNEC = MAX0(NZ,NIRNEC)
      NIRTOT = MAX0(NZ,NIRTOT)
      NIRNEC = MIN0(NIRNEC,NIRTOT)
      RETURN
      END
      SUBROUTINE MA27ND(N,NZ,NZ1,A,LA,IRN,ICN,IW,LIW,PERM,IW2,IFLAG)
      DOUBLE PRECISION ZERO
      PARAMETER (ZERO=0.0D0)
      INTEGER IFLAG,LA,LIW,N,NZ,NZ1
      DOUBLE PRECISION A(LA)
      INTEGER ICN(*),IRN(*),IW(LIW),IW2(N),PERM(N)
      DOUBLE PRECISION ANEXT,ANOW
      INTEGER I,IA,ICH,II,IIW,INEW,IOLD,IPOS,J1,J2,JJ,JNEW,JOLD,JPOS,K
      INTRINSIC MIN0
      COMMON /MA27DD/U,LP,MP,LDIAG
      COMMON /MA27ED/OPS,IERROR,NRLTOT,NIRTOT,NRLNEC,NIRNEC,NRLADU,
     +       NIRADU,NRLBDU,NIRBDU,NCMPA,NCMPBR,NCMPBI,NTWO,NEIG
      DOUBLE PRECISION OPS,U
      INTEGER IERROR,LDIAG,LP,MP,NCMPA,NCMPBI,NCMPBR,NEIG,NIRADU,NIRBDU,
     +        NIRNEC,NIRTOT,NRLADU,NRLBDU,NRLNEC,NRLTOT,NTWO
      SAVE /MA27DD/,/MA27ED/
      IFLAG = 0
      IA = LA
      DO 10 IOLD = 1,N
        IW2(IOLD) = 1
        A(IA) = ZERO
        IA = IA - 1
   10 CONTINUE
      IERROR = 0
      NZ1 = N
      IF (NZ.EQ.0) GO TO 80
      DO 70 K = 1,NZ
        IOLD = IRN(K)
        IF (IOLD.GT.N .OR. IOLD.LE.0) GO TO 30
        JOLD = ICN(K)
        IF (JOLD.GT.N .OR. JOLD.LE.0) GO TO 30
        INEW = PERM(IOLD)
        JNEW = PERM(JOLD)
        IF (INEW.NE.JNEW) GO TO 20
        IA = LA - N + IOLD
        A(IA) = A(IA) + A(K)
        GO TO 60
   20   INEW = MIN0(INEW,JNEW)
        IW2(INEW) = IW2(INEW) + 1
        IW(K) = -IOLD
        NZ1 = NZ1 + 1
        GO TO 70
   30   IFLAG = 1
        IERROR = IERROR + 1
        IF (IERROR.LE.1 .AND. MP.GT.0) WRITE (MP,FMT=40) IFLAG
   40   FORMAT (52H *** WARNING MESSAGE FROM SUBROUTINE MA27BD *** IFLA,
     +         3HG =,I2)
        IF (IERROR.LE.10 .AND. MP.GT.0) WRITE (MP,FMT=50) K,IRN(K),
     +      ICN(K)
   50   FORMAT (I6,19HTH NON-ZERO (IN ROW,I6,11H AND COLUMN,I6,
     +         9H) IGNORED)
   60   IW(K) = 0
   70 CONTINUE
   80 IF (NZ.LT.NZ1 .AND. NZ1.NE.N) GO TO 100
      K = 1
      DO 90 I = 1,N
        K = K + IW2(I)
        IW2(I) = K
   90 CONTINUE
      GO TO 120
  100 K = 1
      DO 110 I = 1,N
        K = K + IW2(I) - 1
        IW2(I) = K
  110 CONTINUE
  120 IF (NZ1.GT.LIW) GO TO 210
      IF (NZ1+N.GT.LA) GO TO 220
      IF (NZ1.EQ.N) GO TO 180
      DO 140 K = 1,NZ
        IOLD = -IW(K)
        IF (IOLD.LE.0) GO TO 140
        JOLD = ICN(K)
        ANOW = A(K)
        IW(K) = 0
        DO 130 ICH = 1,NZ
          INEW = PERM(IOLD)
          JNEW = PERM(JOLD)
          INEW = MIN0(INEW,JNEW)
          IF (INEW.EQ.PERM(JOLD)) JOLD = IOLD
          JPOS = IW2(INEW) - 1
          IOLD = -IW(JPOS)
          ANEXT = A(JPOS)
          A(JPOS) = ANOW
          IW(JPOS) = JOLD
          IW2(INEW) = JPOS
          IF (IOLD.EQ.0) GO TO 140
          ANOW = ANEXT
          JOLD = ICN(JPOS)
  130   CONTINUE
  140 CONTINUE
      IF (NZ.GE.NZ1) GO TO 180
      IPOS = NZ1
      JPOS = NZ1 - N
      DO 170 II = 1,N
        I = N - II + 1
        J1 = IW2(I)
        J2 = JPOS
        IF (J1.GT.JPOS) GO TO 160
        DO 150 JJ = J1,J2
          IW(IPOS) = IW(JPOS)
          A(IPOS) = A(JPOS)
          IPOS = IPOS - 1
          JPOS = JPOS - 1
  150   CONTINUE
  160   IW2(I) = IPOS + 1
        IPOS = IPOS - 1
  170 CONTINUE
  180 DO 190 IOLD = 1,N
        INEW = PERM(IOLD)
        JPOS = IW2(INEW) - 1
        IA = LA - N + IOLD
        A(JPOS) = A(IA)
        IW(JPOS) = -IOLD
  190 CONTINUE
      IPOS = NZ1
      IA = LA
      IIW = LIW
      DO 200 I = 1,NZ1
        A(IA) = A(IPOS)
        IW(IIW) = IW(IPOS)
        IPOS = IPOS - 1
        IA = IA - 1
        IIW = IIW - 1
  200 CONTINUE
      GO TO 230
  210 IFLAG = -3
      IERROR = NZ1
      GO TO 230
  220 IFLAG = -4
      IERROR = NZ1 + N
  230 RETURN
      END
      SUBROUTINE MA27OD(N,NZ,A,LA,IW,LIW,PERM,NSTK,NSTEPS,MAXFRT,NELIM,
     +                  IW2,IFLAG)
      DOUBLE PRECISION ZERO,HALF,ONE
      PARAMETER (ZERO=0.0D0,HALF=0.5D0,ONE=1.0D0)
      INTEGER IFLAG,LA,LIW,MAXFRT,N,NSTEPS,NZ
      DOUBLE PRECISION A(LA)
      INTEGER IW(LIW),IW2(N),NELIM(NSTEPS),NSTK(NSTEPS),PERM(N)
      DOUBLE PRECISION AMAX,AMULT,AMULT1,AMULT2,DETPIV,RMAX,SWOP,THRESH,
     +                 TMAX,UU
      INTEGER AINPUT,APOS,APOS1,APOS2,APOS3,ASTK,ASTK2,AZERO,I,IASS,
     +        IBEG,IDUMMY,IELL,IEND,IEXCH,IFR,IINPUT,IOLDPS,IORG,IPIV,
     +        IPMNP,IPOS,IROW,ISNPIV,ISTK,ISTK2,ISWOP,IWPOS,IX,IY,J,J1,
     +        J2,JCOL,JDUMMY,JFIRST,JJ,JJ1,JJJ,JLAST,JMAX,JMXMIP,JNEW,
     +        JNEXT,JPIV,JPOS,K,K1,K2,KDUMMY,KK,KMAX,KROW,LAELL,LAPOS2,
     +        LIELL,LNASS,LNPIV,LT,LTOPST,NASS,NBLK,NEWEL,NFRONT,NPIV,
     +        NPIVP1,NTOTPV,NUMASS,NUMORG,NUMSTK,PIVSIZ,POSFAC,POSPV1,
     +        POSPV2
      EXTERNAL MA27PD
      INTRINSIC DABS,DMAX1,DMIN1,MAX0,MIN0
      COMMON /MA27DD/U,LP,MP,LDIAG
      COMMON /MA27ED/OPS,IERROR,NRLTOT,NIRTOT,NRLNEC,NIRNEC,NRLADU,
     +       NIRADU,NRLBDU,NIRBDU,NCMPA,NCMPBR,NCMPBI,NTWO,NEIG
      DOUBLE PRECISION OPS,U
      INTEGER IERROR,LDIAG,LP,MP,NCMPA,NCMPBI,NCMPBR,NEIG,NIRADU,NIRBDU,
     +        NIRNEC,NIRTOT,NRLADU,NRLBDU,NRLNEC,NRLTOT,NTWO
      COMMON /MA27TD/FRATIO,PIVTOL
      DOUBLE PRECISION FRATIO,PIVTOL
      INTEGER IDIAG
      SAVE /MA27DD/,/MA27ED/,/MA27TD/
      IDIAG(IX,IY) = ((IY-1)* (2*IX-IY+2))/2
      NBLK = 0
      NTWO = 0
      NEIG = 0
      NCMPBI = 0
      NCMPBR = 0
      MAXFRT = 0
      UU = DMIN1(U,HALF)
      UU = DMAX1(UU,-HALF)
      DO 10 I = 1,N
        IW2(I) = 0
   10 CONTINUE
      IWPOS = 2
      POSFAC = 1
      ISTK = LIW - NZ + 1
      ISTK2 = ISTK - 1
      ASTK = LA - NZ + 1
      ASTK2 = ASTK - 1
      IINPUT = ISTK
      AINPUT = ASTK
      AZERO = 0
      NTOTPV = 0
      NUMASS = 0
      DO 760 IASS = 1,NSTEPS
        NASS = NELIM(IASS)
        NEWEL = IWPOS + 1
        JFIRST = N + 1
        NFRONT = 0
        NUMSTK = NSTK(IASS)
        LTOPST = 1
        LNASS = 0
        IF (NUMSTK.EQ.0) GO TO 80
        J2 = ISTK - 1
        LNASS = NASS
        LTOPST = ((IW(ISTK)+1)*IW(ISTK))/2
        DO 70 IELL = 1,NUMSTK
          JNEXT = JFIRST
          JLAST = N + 1
          J1 = J2 + 2
          J2 = J1 - 1 + IW(J1-1)
          DO 60 JJ = J1,J2
            J = IW(JJ)
            IF (IW2(J).GT.0) GO TO 60
            JNEW = PERM(J)
            IF (JNEW.LE.NUMASS) NASS = NASS + 1
            DO 20 IDUMMY = 1,N
              IF (JNEXT.EQ.N+1) GO TO 30
              IF (PERM(JNEXT).GT.JNEW) GO TO 30
              JLAST = JNEXT
              JNEXT = IW2(JLAST)
   20       CONTINUE
   30       IF (JLAST.NE.N+1) GO TO 40
            JFIRST = J
            GO TO 50
   40       IW2(JLAST) = J
   50       IW2(J) = JNEXT
            JLAST = J
            NFRONT = NFRONT + 1
   60     CONTINUE
   70   CONTINUE
        LNASS = NASS - LNASS
   80   NUMORG = NELIM(IASS)
        J1 = IINPUT
        DO 150 IORG = 1,NUMORG
          J = -IW(J1)
          DO 140 IDUMMY = 1,LIW
            JNEW = PERM(J)
            IF (IW2(J).GT.0) GO TO 130
            JLAST = N + 1
            JNEXT = JFIRST
            DO 90 JDUMMY = 1,N
              IF (JNEXT.EQ.N+1) GO TO 100
              IF (PERM(JNEXT).GT.JNEW) GO TO 100
              JLAST = JNEXT
              JNEXT = IW2(JLAST)
   90       CONTINUE
  100       IF (JLAST.NE.N+1) GO TO 110
            JFIRST = J
            GO TO 120
  110       IW2(JLAST) = J
  120       IW2(J) = JNEXT
            NFRONT = NFRONT + 1
  130       J1 = J1 + 1
            IF (J1.GT.LIW) GO TO 150
            J = IW(J1)
            IF (J.LT.0) GO TO 150
  140     CONTINUE
  150   CONTINUE
        IF (NEWEL+NFRONT.LT.ISTK) GO TO 160
        CALL MA27PD(A,IW,ISTK,ISTK2,IINPUT,2)
        IF (NEWEL+NFRONT.LT.ISTK) GO TO 160
        IERROR = LIW + 1 + NEWEL + NFRONT - ISTK
        GO TO 770
  160   J = JFIRST
        DO 170 IFR = 1,NFRONT
          NEWEL = NEWEL + 1
          IW(NEWEL) = J
          JNEXT = IW2(J)
          IW2(J) = NEWEL - (IWPOS+1)
          J = JNEXT
  170   CONTINUE
        MAXFRT = MAX0(MAXFRT,NFRONT)
        IW(IWPOS) = NFRONT
        LAELL = ((NFRONT+1)*NFRONT)/2
        APOS2 = POSFAC + LAELL - 1
        IF (NUMSTK.NE.0) LNASS = LNASS* (2*NFRONT-LNASS+1)/2
        IF (POSFAC+LNASS-1.GE.ASTK) GO TO 180
        IF (APOS2.LT.ASTK+LTOPST-1) GO TO 190
  180   CALL MA27PD(A,IW,ASTK,ASTK2,AINPUT,1)
        IF (POSFAC+LNASS-1.GE.ASTK) GO TO 780
        IF (APOS2.GE.ASTK+LTOPST-1) GO TO 780
  190   IF (APOS2.LE.AZERO) GO TO 220
        APOS = AZERO + 1
        LAPOS2 = MIN0(APOS2,ASTK-1)
        IF (LAPOS2.LT.APOS) GO TO 210
        DO 200 K = APOS,LAPOS2
          A(K) = ZERO
  200   CONTINUE
  210   AZERO = APOS2
  220   IF (NUMSTK.EQ.0) GO TO 260
        DO 250 IELL = 1,NUMSTK
          J1 = ISTK + 1
          J2 = ISTK + IW(ISTK)
          DO 240 JJ = J1,J2
            IROW = IW(JJ)
            IROW = IW2(IROW)
            APOS = POSFAC + IDIAG(NFRONT,IROW)
            DO 230 JJJ = JJ,J2
              J = IW(JJJ)
              APOS2 = APOS + IW2(J) - IROW
              A(APOS2) = A(APOS2) + A(ASTK)
              A(ASTK) = ZERO
              ASTK = ASTK + 1
  230       CONTINUE
  240     CONTINUE
          ISTK = J2 + 1
  250   CONTINUE
  260   DO 280 IORG = 1,NUMORG
          J = -IW(IINPUT)
          IROW = IW2(J)
          APOS = POSFAC + IDIAG(NFRONT,IROW)
          DO 270 IDUMMY = 1,NZ
            APOS2 = APOS + IW2(J) - IROW
            A(APOS2) = A(APOS2) + A(AINPUT)
            AINPUT = AINPUT + 1
            IINPUT = IINPUT + 1
            IF (IINPUT.GT.LIW) GO TO 280
            J = IW(IINPUT)
            IF (J.LT.0) GO TO 280
  270     CONTINUE
  280   CONTINUE
        NUMASS = NUMASS + NUMORG
        J1 = IWPOS + 2
        J2 = IWPOS + NFRONT + 1
        DO 290 K = J1,J2
          J = IW(K)
          IW2(J) = 0
  290   CONTINUE
        LNPIV = -1
        NPIV = 0
        DO 650 KDUMMY = 1,NASS
          IF (NPIV.EQ.NASS) GO TO 660
          IF (NPIV.EQ.LNPIV) GO TO 660
          LNPIV = NPIV
          NPIVP1 = NPIV + 1
          JPIV = 1
          DO 640 IPIV = NPIVP1,NASS
            JPIV = JPIV - 1
            IF (JPIV.EQ.1) GO TO 640
            APOS = POSFAC + IDIAG(NFRONT-NPIV,IPIV-NPIV)
            IF (UU.GT.ZERO) GO TO 320
            IF (ABS(A(APOS)).LE.PIVTOL) GO TO 790
            IF (NTOTPV.GT.0) GO TO 300
            IF (A(APOS).GT.ZERO) ISNPIV = 1
            IF (A(APOS).LT.ZERO) ISNPIV = -1
  300       IF (A(APOS).GT.ZERO .AND. ISNPIV.EQ.1) GO TO 560
            IF (A(APOS).LT.ZERO .AND. ISNPIV.EQ.-1) GO TO 560
            IF (IFLAG.NE.2) IERROR = 0
            IERROR = IERROR + 1
            IFLAG = 2
            I = NTOTPV + 1
            IF (MP.GT.0 .AND. IERROR.LE.10) WRITE (MP,FMT=310) IFLAG,I
  310       FORMAT (48H *** WARNING MESSAGE FROM SUBROUTINE MA27BD *** ,
     +             7HIFLAG =,I2,/,6H PIVOT,I6,
     +             27H HAS DIFFERENT SIGN FROM TH,10HE PREVIOUS,4H ONE)
            ISNPIV = -ISNPIV
            IF (UU.EQ.ZERO) GO TO 560
            GO TO 800
  320       AMAX = ZERO
            TMAX = AMAX
            J1 = APOS + 1
            J2 = APOS + NASS - IPIV
            IF (J2.LT.J1) GO TO 340
            DO 330 JJ = J1,J2
              IF (DABS(A(JJ)).LE.AMAX) GO TO 330
              JMAX = IPIV + JJ - J1 + 1
              AMAX = DABS(A(JJ))
  330       CONTINUE
  340       J1 = J2 + 1
            J2 = APOS + NFRONT - IPIV
            IF (J2.LT.J1) GO TO 360
            DO 350 JJ = J1,J2
              TMAX = DMAX1(DABS(A(JJ)),TMAX)
  350       CONTINUE
  360       RMAX = DMAX1(TMAX,AMAX)
            APOS1 = APOS
            KK = NFRONT - IPIV
            LT = IPIV - (NPIV+1)
            IF (LT.EQ.0) GO TO 380
            DO 370 K = 1,LT
              KK = KK + 1
              APOS1 = APOS1 - KK
              RMAX = DMAX1(RMAX,DABS(A(APOS1)))
  370       CONTINUE
  380       IF (DABS(A(APOS)).GT.MAX(PIVTOL,UU*RMAX)) GO TO 450
            IF (ABS(AMAX).LE.PIVTOL) GO TO 640
            APOS2 = POSFAC + IDIAG(NFRONT-NPIV,JMAX-NPIV)
            DETPIV = A(APOS)*A(APOS2) - AMAX*AMAX
            THRESH = DABS(DETPIV)
            THRESH = THRESH/ (UU*DMAX1(DABS(A(APOS))+AMAX,
     +               DABS(A(APOS2))+AMAX))
            IF (THRESH.LE.RMAX) GO TO 640
            RMAX = ZERO
            J1 = APOS2 + 1
            J2 = APOS2 + NFRONT - JMAX
            IF (J2.LT.J1) GO TO 400
            DO 390 JJ = J1,J2
              RMAX = DMAX1(RMAX,DABS(A(JJ)))
  390       CONTINUE
  400       KK = NFRONT - JMAX + 1
            APOS3 = APOS2
            JMXMIP = JMAX - IPIV - 1
            IF (JMXMIP.EQ.0) GO TO 420
            DO 410 K = 1,JMXMIP
              APOS2 = APOS2 - KK
              KK = KK + 1
              RMAX = DMAX1(RMAX,DABS(A(APOS2)))
  410       CONTINUE
  420       IPMNP = IPIV - NPIV - 1
            IF (IPMNP.EQ.0) GO TO 440
            APOS2 = APOS2 - KK
            KK = KK + 1
            DO 430 K = 1,IPMNP
              APOS2 = APOS2 - KK
              KK = KK + 1
              RMAX = DMAX1(RMAX,DABS(A(APOS2)))
  430       CONTINUE
  440       IF (THRESH.LE.RMAX) GO TO 640
            PIVSIZ = 2
            GO TO 460
  450       PIVSIZ = 1
  460       IROW = IPIV - NPIV
            DO 550 KROW = 1,PIVSIZ
              IF (IROW.EQ.1) GO TO 530
              J1 = POSFAC + IROW
              J2 = POSFAC + NFRONT - (NPIV+1)
              IF (J2.LT.J1) GO TO 480
              APOS2 = APOS + 1
              DO 470 JJ = J1,J2
                SWOP = A(APOS2)
                A(APOS2) = A(JJ)
                A(JJ) = SWOP
                APOS2 = APOS2 + 1
  470         CONTINUE
  480         J1 = POSFAC + 1
              J2 = POSFAC + IROW - 2
              APOS2 = APOS
              KK = NFRONT - (IROW+NPIV)
              IF (J2.LT.J1) GO TO 500
              DO 490 JJJ = J1,J2
                JJ = J2 - JJJ + J1
                KK = KK + 1
                APOS2 = APOS2 - KK
                SWOP = A(APOS2)
                A(APOS2) = A(JJ)
                A(JJ) = SWOP
  490         CONTINUE
  500         IF (NPIV.EQ.0) GO TO 520
              APOS1 = POSFAC
              KK = KK + 1
              APOS2 = APOS2 - KK
              DO 510 JJ = 1,NPIV
                KK = KK + 1
                APOS1 = APOS1 - KK
                APOS2 = APOS2 - KK
                SWOP = A(APOS2)
                A(APOS2) = A(APOS1)
                A(APOS1) = SWOP
  510         CONTINUE
  520         SWOP = A(APOS)
              A(APOS) = A(POSFAC)
              A(POSFAC) = SWOP
              IPOS = IWPOS + NPIV + 2
              IEXCH = IWPOS + IROW + NPIV + 1
              ISWOP = IW(IPOS)
              IW(IPOS) = IW(IEXCH)
              IW(IEXCH) = ISWOP
  530         IF (PIVSIZ.EQ.1) GO TO 550
              IF (KROW.EQ.2) GO TO 540
              IROW = JMAX - (NPIV+1)
              JPOS = POSFAC
              POSFAC = POSFAC + NFRONT - NPIV
              NPIV = NPIV + 1
              APOS = APOS3
              GO TO 550
  540         NPIV = NPIV - 1
              POSFAC = JPOS
  550       CONTINUE
            IF (PIVSIZ.EQ.2) GO TO 600
  560       A(POSFAC) = ONE/A(POSFAC)
            IF (A(POSFAC).LT.ZERO) NEIG = NEIG + 1
            J1 = POSFAC + 1
            J2 = POSFAC + NFRONT - (NPIV+1)
            IF (J2.LT.J1) GO TO 590
            IBEG = J2 + 1
            DO 580 JJ = J1,J2
              AMULT = -A(JJ)*A(POSFAC)
              IEND = IBEG + NFRONT - (NPIV+JJ-J1+2)
CDIR$ IVDEP
              DO 570 IROW = IBEG,IEND
                JCOL = JJ + IROW - IBEG
                A(IROW) = A(IROW) + AMULT*A(JCOL)
  570         CONTINUE
              IBEG = IEND + 1
              A(JJ) = AMULT
  580       CONTINUE
  590       NPIV = NPIV + 1
            NTOTPV = NTOTPV + 1
            JPIV = 1
            POSFAC = POSFAC + NFRONT - NPIV + 1
            GO TO 640
  600       IPOS = IWPOS + NPIV + 2
            NTWO = NTWO + 1
            IW(IPOS) = -IW(IPOS)
            POSPV1 = POSFAC
            POSPV2 = POSFAC + NFRONT - NPIV
            SWOP = A(POSPV2)
            IF (DETPIV.LT.ZERO) NEIG = NEIG + 1
            IF (DETPIV.GT.ZERO .AND. SWOP.LT.ZERO) NEIG = NEIG + 2
            A(POSPV2) = A(POSPV1)/DETPIV
            A(POSPV1) = SWOP/DETPIV
            A(POSPV1+1) = -A(POSPV1+1)/DETPIV
            J1 = POSPV1 + 2
            J2 = POSPV1 + NFRONT - (NPIV+1)
            IF (J2.LT.J1) GO TO 630
            JJ1 = POSPV2
            IBEG = POSPV2 + NFRONT - (NPIV+1)
            DO 620 JJ = J1,J2
              JJ1 = JJ1 + 1
              AMULT1 = - (A(POSPV1)*A(JJ)+A(POSPV1+1)*A(JJ1))
              AMULT2 = - (A(POSPV1+1)*A(JJ)+A(POSPV2)*A(JJ1))
              IEND = IBEG + NFRONT - (NPIV+JJ-J1+3)
CDIR$ IVDEP
              DO 610 IROW = IBEG,IEND
                K1 = JJ + IROW - IBEG
                K2 = JJ1 + IROW - IBEG
                A(IROW) = A(IROW) + AMULT1*A(K1) + AMULT2*A(K2)
  610         CONTINUE
              IBEG = IEND + 1
              A(JJ) = AMULT1
              A(JJ1) = AMULT2
  620       CONTINUE
  630       NPIV = NPIV + 2
            NTOTPV = NTOTPV + 2
            JPIV = 2
            POSFAC = POSPV2 + NFRONT - NPIV + 1
  640     CONTINUE
  650   CONTINUE
  660   IF (NPIV.NE.0) NBLK = NBLK + 1
        IOLDPS = IWPOS
        IWPOS = IWPOS + NFRONT + 2
        IF (NPIV.EQ.0) GO TO 690
        IF (NPIV.GT.1) GO TO 680
        IW(IOLDPS) = -IW(IOLDPS)
        DO 670 K = 1,NFRONT
          J1 = IOLDPS + K
          IW(J1) = IW(J1+1)
  670   CONTINUE
        IWPOS = IWPOS - 1
        GO TO 690
  680   IW(IOLDPS+1) = NPIV
  690   LIELL = NFRONT - NPIV
        IF (LIELL.EQ.0 .OR. IASS.EQ.NSTEPS) GO TO 750
        IF (IWPOS+LIELL.LT.ISTK) GO TO 700
        CALL MA27PD(A,IW,ISTK,ISTK2,IINPUT,2)
  700   ISTK = ISTK - LIELL - 1
        IW(ISTK) = LIELL
        J1 = ISTK
        KK = IWPOS - LIELL - 1
CDIR$ IVDEP
        DO 710 K = 1,LIELL
          J1 = J1 + 1
          KK = KK + 1
          IW(J1) = IW(KK)
  710   CONTINUE
        LAELL = ((LIELL+1)*LIELL)/2
        KK = POSFAC + LAELL
        IF (KK.NE.ASTK) GO TO 720
        ASTK = ASTK - LAELL
        GO TO 740
  720   KMAX = KK - 1
CDIR$ IVDEP
        DO 730 K = 1,LAELL
          KK = KK - 1
          ASTK = ASTK - 1
          A(ASTK) = A(KK)
  730   CONTINUE
        KMAX = MIN0(KMAX,ASTK-1)
        DO 735 K = KK,KMAX
          A(K) = ZERO
  735   CONTINUE
  740   AZERO = MIN0(AZERO,ASTK-1)
  750   IF (NPIV.EQ.0) IWPOS = IOLDPS
  760 CONTINUE
      IW(1) = NBLK
      IF (NTWO.GT.0) IW(1) = -NBLK
      NRLBDU = POSFAC - 1
      NIRBDU = IWPOS - 1
      IF (NTOTPV.EQ.N) GO TO 810
      IFLAG = 3
      IERROR = NTOTPV
      GO TO 810
  770 IFLAG = -3
      GO TO 810
  780 IFLAG = -4
      IERROR = LA + MAX0(POSFAC+LNASS,APOS2-LTOPST+2) - ASTK
      GO TO 810
  790 IFLAG = -5
      IERROR = NTOTPV + 1
      GO TO 810
  800 IFLAG = -6
      IERROR = NTOTPV + 1
  810 RETURN
      END
      SUBROUTINE MA27PD(A,IW,J1,J2,ITOP,IREAL)
      INTEGER IREAL,ITOP,J1,J2
      DOUBLE PRECISION A(*)
      INTEGER IW(*)
      INTEGER IPOS,JJ,JJJ
      COMMON /MA27ED/OPS,IERROR,NRLTOT,NIRTOT,NRLNEC,NIRNEC,NRLADU,
     +       NIRADU,NRLBDU,NIRBDU,NCMPA,NCMPBR,NCMPBI,NTWO,NEIG
      DOUBLE PRECISION OPS
      INTEGER IERROR,NCMPA,NCMPBI,NCMPBR,NEIG,NIRADU,NIRBDU,NIRNEC,
     +        NIRTOT,NRLADU,NRLBDU,NRLNEC,NRLTOT,NTWO
      SAVE /MA27ED/
      IPOS = ITOP - 1
      IF (J2.EQ.IPOS) GO TO 50
      IF (IREAL.EQ.2) GO TO 20
      NCMPBR = NCMPBR + 1
      IF (J1.GT.J2) GO TO 40
      DO 10 JJJ = J1,J2
        JJ = J2 - JJJ + J1
        A(IPOS) = A(JJ)
        IPOS = IPOS - 1
   10 CONTINUE
      GO TO 40
   20 NCMPBI = NCMPBI + 1
      IF (J1.GT.J2) GO TO 40
      DO 30 JJJ = J1,J2
        JJ = J2 - JJJ + J1
        IW(IPOS) = IW(JJ)
        IPOS = IPOS - 1
   30 CONTINUE
   40 J2 = ITOP - 1
      J1 = IPOS + 1
   50 RETURN
      END
      SUBROUTINE MA27QD(N,A,LA,IW,LIW,W,MAXFNT,RHS,IW2,NBLK,LATOP)
      INTEGER LA,LATOP,LIW,MAXFNT,N,NBLK
      DOUBLE PRECISION A(LA),RHS(N),W(MAXFNT)
      INTEGER IW(LIW),IW2(NBLK)
      DOUBLE PRECISION W1,W2
      INTEGER APOS,IBLK,IFR,ILVL,IPIV,IPOS,IRHS,IROW,IST,J,J1,J2,J3,JJ,
     +        JPIV,K,K1,K2,K3,LIELL,NPIV
      INTRINSIC IABS,MIN0
      COMMON /MA27FD/IOVFLO,NEMIN,IFRLVL(20)
      INTEGER IOVFLO,NEMIN
      INTEGER IFRLVL
      SAVE /MA27FD/
      APOS = 1
      IPOS = 1
      J2 = 0
      IBLK = 0
      NPIV = 0
      DO 140 IROW = 1,N
        IF (NPIV.GT.0) GO TO 90
        IBLK = IBLK + 1
        IF (IBLK.GT.NBLK) GO TO 150
        IPOS = J2 + 1
        IW2(IBLK) = IPOS
        LIELL = -IW(IPOS)
        NPIV = 1
        IF (LIELL.GT.0) GO TO 10
        LIELL = -LIELL
        IPOS = IPOS + 1
        NPIV = IW(IPOS)
   10   J1 = IPOS + 1
        J2 = IPOS + LIELL
        ILVL = MIN0(NPIV,10)
        IF (LIELL.LT.IFRLVL(ILVL)) GO TO 90
        IFR = 0
        DO 20 JJ = J1,J2
          J = IABS(IW(JJ)+0)
          IFR = IFR + 1
          W(IFR) = RHS(J)
   20   CONTINUE
        JPIV = 1
        J3 = J1
        DO 70 IPIV = 1,NPIV
          JPIV = JPIV - 1
          IF (JPIV.EQ.1) GO TO 70
          IF (IW(J3).LT.0) GO TO 40
          JPIV = 1
          J3 = J3 + 1
          APOS = APOS + 1
          IST = IPIV + 1
          IF (LIELL.LT.IST) GO TO 70
          W1 = W(IPIV)
          K = APOS
          DO 30 J = IST,LIELL
            W(J) = W(J) + A(K)*W1
            K = K + 1
   30     CONTINUE
          APOS = APOS + LIELL - IST + 1
          GO TO 70
   40     JPIV = 2
          J3 = J3 + 2
          APOS = APOS + 2
          IST = IPIV + 2
          IF (LIELL.LT.IST) GO TO 60
          W1 = W(IPIV)
          W2 = W(IPIV+1)
          K1 = APOS
          K2 = APOS + LIELL - IPIV
          DO 50 J = IST,LIELL
            W(J) = W(J) + W1*A(K1) + W2*A(K2)
            K1 = K1 + 1
            K2 = K2 + 1
   50     CONTINUE
   60     APOS = APOS + 2* (LIELL-IST+1) + 1
   70   CONTINUE
        IFR = 0
        DO 80 JJ = J1,J2
          J = IABS(IW(JJ)+0)
          IFR = IFR + 1
          RHS(J) = W(IFR)
   80   CONTINUE
        NPIV = 0
        GO TO 140
   90   IF (IW(J1).LT.0) GO TO 110
        NPIV = NPIV - 1
        APOS = APOS + 1
        J1 = J1 + 1
        IF (J1.GT.J2) GO TO 140
        IRHS = IW(J1-1)
        W1 = RHS(IRHS)
        K = APOS
        DO 100 J = J1,J2
          IRHS = IABS(IW(J)+0)
          RHS(IRHS) = RHS(IRHS) + A(K)*W1
          K = K + 1
  100   CONTINUE
        APOS = APOS + J2 - J1 + 1
        GO TO 140
  110   NPIV = NPIV - 2
        J1 = J1 + 2
        APOS = APOS + 2
        IF (J1.GT.J2) GO TO 130
        IRHS = -IW(J1-2)
        W1 = RHS(IRHS)
        IRHS = IW(J1-1)
        W2 = RHS(IRHS)
        K1 = APOS
        K3 = APOS + J2 - J1 + 2
        DO 120 J = J1,J2
          IRHS = IABS(IW(J)+0)
          RHS(IRHS) = RHS(IRHS) + W1*A(K1) + W2*A(K3)
          K1 = K1 + 1
          K3 = K3 + 1
  120   CONTINUE
  130   APOS = APOS + 2* (J2-J1+1) + 1
  140 CONTINUE
  150 LATOP = APOS - 1
      RETURN
      END
      SUBROUTINE MA27RD(N,A,LA,IW,LIW,W,MAXFNT,RHS,IW2,NBLK,LATOP)
      INTEGER LA,LATOP,LIW,MAXFNT,N,NBLK
      DOUBLE PRECISION A(LA),RHS(N),W(MAXFNT)
      INTEGER IW(LIW),IW2(NBLK)
      DOUBLE PRECISION W1,W2
      INTEGER APOS,APOS2,I1RHS,I2RHS,IBLK,IFR,IIPIV,IIRHS,ILVL,IPIV,
     +        IPOS,IRHS,IST,J,J1,J2,JJ,JJ1,JJ2,JPIV,JPOS,K,LIELL,LOOP,
     +        NPIV
      INTRINSIC IABS,MIN0
      COMMON /MA27FD/IOVFLO,NEMIN,IFRLVL(20)
      INTEGER IOVFLO,NEMIN
      INTEGER IFRLVL
      SAVE /MA27FD/
      APOS = LATOP + 1
      NPIV = 0
      IBLK = NBLK + 1
      DO 180 LOOP = 1,N
        IF (NPIV.GT.0) GO TO 110
        IBLK = IBLK - 1
        IF (IBLK.LT.1) GO TO 190
        IPOS = IW2(IBLK)
        LIELL = -IW(IPOS)
        NPIV = 1
        IF (LIELL.GT.0) GO TO 10
        LIELL = -LIELL
        IPOS = IPOS + 1
        NPIV = IW(IPOS)
   10   JPOS = IPOS + NPIV
        J2 = IPOS + LIELL
        ILVL = MIN0(10,NPIV) + 10
        IF (LIELL.LT.IFRLVL(ILVL)) GO TO 110
        J1 = IPOS + 1
        IFR = 0
        DO 20 JJ = J1,J2
          J = IABS(IW(JJ)+0)
          IFR = IFR + 1
          W(IFR) = RHS(J)
   20   CONTINUE
        JPIV = 1
        DO 90 IIPIV = 1,NPIV
          JPIV = JPIV - 1
          IF (JPIV.EQ.1) GO TO 90
          IPIV = NPIV - IIPIV + 1
          IF (IPIV.EQ.1) GO TO 30
          IF (IW(JPOS-1).LT.0) GO TO 60
   30     JPIV = 1
          APOS = APOS - (LIELL+1-IPIV)
          IST = IPIV + 1
          W1 = W(IPIV)*A(APOS)
          IF (LIELL.LT.IST) GO TO 50
          JJ1 = APOS + 1
          DO 40 J = IST,LIELL
            W1 = W1 + A(JJ1)*W(J)
            JJ1 = JJ1 + 1
   40     CONTINUE
   50     W(IPIV) = W1
          JPOS = JPOS - 1
          GO TO 90
   60     JPIV = 2
          APOS2 = APOS - (LIELL+1-IPIV)
          APOS = APOS2 - (LIELL+2-IPIV)
          IST = IPIV + 1
          W1 = W(IPIV-1)*A(APOS) + W(IPIV)*A(APOS+1)
          W2 = W(IPIV-1)*A(APOS+1) + W(IPIV)*A(APOS2)
          IF (LIELL.LT.IST) GO TO 80
          JJ1 = APOS + 2
          JJ2 = APOS2 + 1
          DO 70 J = IST,LIELL
            W1 = W1 + W(J)*A(JJ1)
            W2 = W2 + W(J)*A(JJ2)
            JJ1 = JJ1 + 1
            JJ2 = JJ2 + 1
   70     CONTINUE
   80     W(IPIV-1) = W1
          W(IPIV) = W2
          JPOS = JPOS - 2
   90   CONTINUE
        IFR = 0
        DO 100 JJ = J1,J2
          J = IABS(IW(JJ)+0)
          IFR = IFR + 1
          RHS(J) = W(IFR)
  100   CONTINUE
        NPIV = 0
        GO TO 180
  110   IF (NPIV.EQ.1) GO TO 120
        IF (IW(JPOS-1).LT.0) GO TO 150
  120   NPIV = NPIV - 1
        APOS = APOS - (J2-JPOS+1)
        IIRHS = IW(JPOS)
        W1 = RHS(IIRHS)*A(APOS)
        J1 = JPOS + 1
        IF (J1.GT.J2) GO TO 140
        K = APOS + 1
        DO 130 J = J1,J2
          IRHS = IABS(IW(J)+0)
          W1 = W1 + A(K)*RHS(IRHS)
          K = K + 1
  130   CONTINUE
  140   RHS(IIRHS) = W1
        JPOS = JPOS - 1
        GO TO 180
  150   NPIV = NPIV - 2
        APOS2 = APOS - (J2-JPOS+1)
        APOS = APOS2 - (J2-JPOS+2)
        I1RHS = -IW(JPOS-1)
        I2RHS = IW(JPOS)
        W1 = RHS(I1RHS)*A(APOS) + RHS(I2RHS)*A(APOS+1)
        W2 = RHS(I1RHS)*A(APOS+1) + RHS(I2RHS)*A(APOS2)
        J1 = JPOS + 1
        IF (J1.GT.J2) GO TO 170
        JJ1 = APOS + 2
        JJ2 = APOS2 + 1
        DO 160 J = J1,J2
          IRHS = IABS(IW(J)+0)
          W1 = W1 + RHS(IRHS)*A(JJ1)
          W2 = W2 + RHS(IRHS)*A(JJ2)
          JJ1 = JJ1 + 1
          JJ2 = JJ2 + 1
  160   CONTINUE
  170   RHS(I1RHS) = W1
        RHS(I2RHS) = W2
        JPOS = JPOS - 2
  180 CONTINUE
  190 RETURN
      END
