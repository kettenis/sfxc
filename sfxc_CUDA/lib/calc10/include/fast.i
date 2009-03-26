!
! >>>>> INCLUDE-BLOCK with descriptions data structures used for fast extension
!       of SOLVE
!       fast.i  31-Dec-96  v5.8  (c)  L. Petrov  --  06-MAY-2002 10:34:23
!
        CHARACTER  IDENT_B3D*12, IDENT_B1B3D*12
        PARAMETER  ( IDENT_B3D   = 'B3D v5.8    ' ) ! Identifier of B3D
        PARAMETER  ( IDENT_B1B3D = 'B1B3D v5.8  ' ) ! Identifier of B1B3D
!
        INTEGER*4   MAX_PSG, MAX_SEG
        INTEGER*4   F__GLO, F__CUS,  F__NES
        INTEGER*4   F__UND, F__NONE, F__PRD, F__B3D, F__B1D, F__B1B3D
        INTEGER*4   F__LOC, F__SEG,  F__FUL, F__DEL, F__RAT
        INTEGER*4   F__MFR, F__MSL,  F__MFL
!
        INTEGER*4   F__APP, F__PRI, F__TIM, F__MON
!
        PARAMETER ( MAX_PSG = 256 )  ! max number of segmented parameters in
!                                    ! one segment
        PARAMETER ( MAX_SEG  = 128 ) ! max number of segments
        PARAMETER ( F__GLO   =   1 )
        PARAMETER ( F__CUS   =   2 )
        PARAMETER ( F__NES   =   3 )
        PARAMETER ( F__UND   =  -1 )
        PARAMETER ( F__NONE  =   0 )
        PARAMETER ( F__PRD   =   1 )
        PARAMETER ( F__B3D   =   2 )
        PARAMETER ( F__B1D   =   3 )
        PARAMETER ( F__B1B3D =   4 )
        PARAMETER ( F__LOC   =   2 )
        PARAMETER ( F__SEG   =   3 )
        PARAMETER ( F__FUL   =   4 )
        PARAMETER ( F__DEL   =  11 )
        PARAMETER ( F__RAT   =  12 )
        PARAMETER ( F__MFR   =  31 ) ! Memory is free
        PARAMETER ( F__MSL   =  32 ) ! Memory allocated for the solution
        PARAMETER ( F__MFL   =  33 ) ! Memory allocated for the solution and for
!                                    ! keeping off-diagonal terms of cov.mat.
        PARAMETER ( F__APP =  1 )  ! Debug mode: appereance
        PARAMETER ( F__PRI =  2 )  ! Debug mode: printout
        PARAMETER ( F__TIM = 11 )  ! Debug mode: timer
        PARAMETER ( F__MON = 12 )  ! Debug mode: monitor
!
      TYPE      PLACE__STRU
         INTEGER*4    IND_GEN (MAX_PAR) ! Indices all non-zero derivatives
!                                       !   in the array
         INTEGER*4    IND_GLO (MAX_PAR) ! Indices of global parameters
         INTEGER*4    IND_LOC (MAX_PAR) ! Indeces of local parameters
         INTEGER*4    IND_SG1 (MAX_PSG) ! Indices of parameters in the
!                                       !   current segment
         INTEGER*4    IND_SG2 (MAX_PSG) ! Indices of parameters in the
!                                       !   next segment
! ------ Equations for delay
!
         REAL*8       EQU_GEN (MAX_PAR) ! Full array of coefficients of equ/cond
         REAL*8       EQU_GLO (MAX_PAR) ! Coefficients for global equat/condit
         REAL*8       EQU_LOC (MAX_PAR) ! Coefficients for local  equat/condit
         REAL*8       EQU_SG1 (MAX_PSG) ! Coeff. for cur/blo segm. equat/condit
         REAL*8       EQU_SG2 (MAX_PSG) ! Coeff. for next/blo segm. equat/condi
!                                       !   next segment
! ------ Equations for rate
!
         REAL*8       RAT_GEN (MAX_PAR) ! Full array of coefficients of equ/cond
         REAL*8       RAT_GLO (MAX_PAR) ! Coefficiencts for global equat/condit
         REAL*8       RAT_LOC (MAX_PAR) ! Coefficients for local  equat/condit
         REAL*8       RAT_SG1 (MAX_PSG) ! Coeff. for cur/blo segm. equat/condit
         REAL*8       RAT_SG2 (MAX_PSG) ! Coeff. for next/blo segm. equat/condi
!!
         INTEGER*4    N_GEN             ! Total number of non-zero
!                                       !   derivatives in the array
         INTEGER*4    N_GLO             ! Number of non-zero global derivatives
         INTEGER*4    N_LOC             ! Number of non-zero local derivatives
         INTEGER*4    N_SG1             ! Number of non-zero derivatives at
!                                       !   the current segment
         INTEGER*4    N_SG2             ! Number of non-zero derivatives at
!                                       !   the next segment
         INTEGER*4    CLO_SEG           ! Index of the current clock   segment
         INTEGER*4    ATM_SEG           ! Index of the current atmosph segment
         INTEGER*4    EOP_SEG           ! Index of the current EOP     segment
!                                       !   at the next segment
         INTEGER*4    CLO_SEG_LAST      ! Index of the last clock  segment
         INTEGER*4    ATM_SEG_LAST      ! Index of the last atmos. segment
         INTEGER*4    EOP_SEG_LAST      ! Index of the last EOP    segment
!
         INTEGER*4    PREV_CSG          ! Index of before last common segment
         INTEGER*4    LAST_CSG          ! Index of common segment for last equa
         INTEGER*4    CURR_CSG          ! Index of current common segment
         INTEGER*4    STATUS            ! Field with status. Brings flag:
!                                       ! whether to use rates or not.
      END TYPE  PLACE__STRU   !  PLACE__STRU    !
!C
      TYPE      B3D__STRU
         INTEGER*4  FIRST_FIELD   ! Filler. Brings the number of database
!
         CHARACTER  IDENT*12      ! Identifier of the current version of B3D
!
         CHARACTER  DBNAME*10     ! Database name
         INTEGER*4  DBVER         ! Database version
         CHARACTER  DBNAME_MES*16 ! Message with database name and version
         INTEGER*4  DBNAME_LEN    ! Length of DBNAME_MES
         INTEGER*4  NOBS_T        ! Total number of observations in session
         INTEGER*4  NOBS_A        ! Actually used number of obs in session
!
         INTEGER*4  NBS    ! Number of blocks of segmented parameters
         INTEGER*4  N_PAR  ! Total number of parameters
!
         INTEGER*4  N_CLO  ! Number of clock parameters at one segment
         INTEGER*4  N_ATM  ! Number of atmosphere parameters at one segment
         INTEGER*4  N_EOP  ! Number of EOP at one segment
!
         INTEGER*4  K_CLO  ! Multiplicacy of the clock
         INTEGER*4  K_ATM  ! Multiplicacy of the atmosphere
         INTEGER*4  K_EOP  ! Multiplicacy of the EOP
!
         INTEGER*4  N_GLO  ! Total number of global parameters in current sess
         INTEGER*4  N_LOC  ! Total number of local parameters in current sess
         INTEGER*4  N_SGM  ! Total number of segmented parameters
         INTEGER*4  SB     ! Number of segmented param. at block (except last)
         INTEGER*4  SX     ! Number of segmented parameters at the last block
!
         INTEGER*4  NX_CLO           ! Number of tail segments for clocks
         INTEGER*4  NX_ATM           ! Number of tail segments for atmosphere
         INTEGER*4  NX_EOP           ! Number of tail segments for EOP
!
         INTEGER*4  MEM_ADR   !  address of the first byte of allocated memory
         INTEGER*4  MEM_SIZE  !  total size of allocated dynamic memory
         INTEGER*4  MEM_SIZE2 !  size of dynamic memory to be read-written
         INTEGER*4  MEM_STAT  !  status of memory allocation
!
         REAL*8     RCOND         ! Condition number
         REAL*8     JD_NOM_FIRST  ! Nominal time of the first observation (JD)
         REAL*8     JD_ACT_FIRST  ! Actual  time of the first observation (JD)
         REAL*8     JD_NOM_LAST   ! Nominal time of the last  observation (JD)
         REAL*8     JD_ACT_LAST   ! Actual  time of the last  observation (JD)
!
! ------ Lists of the sources/stations/baselines used in solution and
! ------ lists of the sources/stations/baselines rejected on the flight by
! ------ PROC. They are saved in order to supply this information to ARCPE and
! ------ BACK to force them deselect these objects from solution or estimation.
!
         INTEGER*4  U_SOU   ! Length of list of sources   in solution
         INTEGER*4  U_STA   ! Length of list of stations  in solution
         INTEGER*4  U_BAS   ! Length of list of baselines in solution
         INTEGER*4  R_SOU   ! Length of list of rejected  sources
         INTEGER*4  R_STA   ! Length of list of rejected  stations
         INTEGER*4  R_BAS   ! Length of list of rejected  baselines
         INTEGER*4  UIS_SOU(MAX_ARC_SRC), &  ! List of sources   in solution
     .              UIS_STA(MAX_ARC_STA), &  ! List of stations  in solution
     .              UIS_BAS(MAX_ARC_BSL)  ! List of baselines in solution
         INTEGER*4  RIS_SOU(MAX_ARC_SRC), &  ! List of rejected  sources
     .              RIS_STA(MAX_ARC_STA), &  ! List of rejected  stations
     .              RIS_BAS(MAX_ARC_BSL)  ! List of rejected  baselines
!
         INTEGER*4  MARKER_1
!
         INTEGER*4  BLO    (MAX_PAR) ! Number of segment
         INTEGER*4  PL     (MAX_PAR) ! Place at the segment
         LOGICAL*4  CURR   (MAX_PAR) ! Put parameter in current block
         LOGICAL*4  NEXT   (MAX_PAR) ! Put parameter in the next block
!
         INTEGER*4  INF_GLO(MAX_PAR) ! Index in full matrix for global paramet.
         INTEGER*4  INF_LOC(MAX_PAR) ! Index in full matrix for local parametrs
         INTEGER*4  INF_SGM(MAX_PSG, MAX_SEG) ! Index in full matrix for
!                                    ! parameters in segmented blocks
         INTEGER*4  INF_SGX(MAX_PSG) ! Index in full matrix for parameters in
!                                      last segmented block
!
         REAL*8     DT(MAX_OBS)      ! Corrected O-C for delay after FLYBY
         REAL*8     RT(MAX_OBS)      ! Corrected O-C for rate  after FLYBY
         REAL*8     SUWSQ_TAU        ! weighted sum of squares of o-c for tau
         REAL*8     SUWSQ_FRE        ! weighted sum of squares of o-c for fre
!C
         INTEGER*4  MARKER_2
!
         INTEGER*4  AD_B0   ! Address of B0-block
         INTEGER*4  AD_Z0   ! Address of Z0-vector
         INTEGER*4  AD_E0   ! Address of estimates of global parameters
         INTEGER*4  AD_U0   ! Address of scale vector for global parameters
!
         INTEGER*4  AD_B(MAX_SEG)  ! Array of addresses for vertical band
         INTEGER*4  AD_C(MAX_SEG)  ! Array of addresses for diagonal band
         INTEGER*4  AD_D(MAX_SEG)  ! Array of addresses for down-diagonal band
         INTEGER*4  AD_ZS(MAX_SEG) ! Array of vectors of right parts for segm
         INTEGER*4  AD_ES(MAX_SEG) ! Array of address of estimates segm. par.
         INTEGER*4  AD_US(MAX_SEG) ! Array addresses of scale vectors for
!                                  !       segmented parameters
!
         INTEGER*4  AD_BX  ! Address for B block for the last segment
         INTEGER*4  AD_CX  ! Address for C block for the last segment
         INTEGER*4  AD_DX  ! Address for D block for the last segment
         INTEGER*4  AD_ZSX ! Address for ZL vector for the last segment
         INTEGER*4  AD_ESX ! Address for vector of estimates for last segment
         INTEGER*4  AD_USX ! Address for vector of scales for the last segment
!
         INTEGER*4  AD_Q0  ! Address of global V*a vector needed for update
         INTEGER*4  AD_QS(MAX_SEG) ! Address of local V*a vectors for update
         INTEGER*4  AD_QSX         ! Address of last local vector for update
         INTEGER*4  AD_CVF         ! Address of off-diaginal terms of Cov.mat.
!
         INTEGER*4  AD_N00  ! Address of temporary matrix
         INTEGER*4  AD_N10  ! Address of temporary matrix
         INTEGER*4  AD_N11  ! Address of temporary matrix
         INTEGER*4  AD_N20  ! Address of temporary matrix
         INTEGER*4  AD_N21  ! Address of temporary matrix
         INTEGER*4  AD_N22  ! Address of temporary matrix
         INTEGER*4  AD_N12  ! Address of temporary matrix
         INTEGER*4  AD_VG0  ! Address of temporary vector
         INTEGER*4  AD_VS1  ! Address of temporary vector
         INTEGER*4  AD_VS2  ! Address of temporary vector
!
         INTEGER*4  LAST_FIELD   ! Filler
      END TYPE  B3D__STRU   ! B3D__STRU    !
!C
      TYPE      B1B3D__STRU
         INTEGER*4  FIRST_FIELD   ! Filler
         CHARACTER  IDENT*12 ! Identifier of the current version of B1B3D
!
         INTEGER*4  AD_W00   ! Address of W00-block
         INTEGER*4  AD_Z00   ! Address of Z00-vector
         INTEGER*4  AD_E00   ! Address of estimates of global parameters
         INTEGER*4  AD_U00   ! Address of scale vector for global parameters
!
         INTEGER*4  AD_WI0   ! Address of local-global block
         INTEGER*4  AD_BI0   ! Address of local-local block
         INTEGER*4  AD_ZI0   ! Address of right parts of local parameters
         INTEGER*4  AD_EI0   ! Address of estimates of local parameters
         INTEGER*4  AD_UI0   ! Address of scales of local parameters
!
         INTEGER*4  AD_WIJ(MAX_SEG) ! Array of addr. of segmented-global param.
         INTEGER*4  AD_BIJ(MAX_SEG) ! Array of addr. of segmented-local  param.
         INTEGER*4  AD_CIJ(MAX_SEG) ! Array of addr. of diagonal segm.-segm.
         INTEGER*4  AD_DIJ(MAX_SEG) ! Array of addr. of down diagonal segm.-seg
         INTEGER*4  AD_ZIJ(MAX_SEG) ! Array of vectors of right parts for segm
         INTEGER*4  AD_EIJ(MAX_SEG) ! Array of estimates of segmented paramet.
         INTEGER*4  AD_UIJ(MAX_SEG) ! Array of addresses of scale vectors for
!                                  !        segmented parameters
!
         INTEGER*4  AD_NGG   ! Address of temporary matrix
         INTEGER*4  AD_NLG   ! Address of temporary matrix
         INTEGER*4  AD_NLL   ! Address of temporary matrix
         INTEGER*4  AD_NS1G  ! Address of temporary matrix
         INTEGER*4  AD_NS1L  ! Address of temporary matrix
         INTEGER*4  AD_NS1S1 ! Address of temporary matrix
         INTEGER*4  AD_NS2G  ! Address of temporary matrix
         INTEGER*4  AD_NS2L  ! Address of temporary matrix
         INTEGER*4  AD_NS2S2 ! Address of temporary matrix
         INTEGER*4  AD_NS2S1 ! Address of temporary matrix
         INTEGER*4  AD_NS1S2 ! Address of temporary matrix
!
         INTEGER*4  AD_VG    ! Address of temporary vector
         INTEGER*4  AD_VL    ! Address of temporary vector
         INTEGER*4  AD_VS1   ! Address of temporary vector
         INTEGER*4  AD_VS2   ! Address of temporary vector
!
         INTEGER*4  MEM_ADR   !  address of the first byte of allocated memory
         INTEGER*4  MEM_SIZE  !  total size of allocated dynamic memory
         INTEGER*4  MEM_SIZE2 !  size of dynamic memory to be read-written
         INTEGER*4  MEM_STAT  !  status of memory allocation
         INTEGER*4  LAST_FIELD   ! Filler
      END TYPE  B1B3D__STRU   ! B1B3D__STRU    !
!
!
      INTEGER*4  FM_VAR, FMI_VAR,
     .           FD_VAR, FDI_VAR,
     .           FC_VAR, FCI_VAR, N$
      PARAMETER  ( FM_VAR  = 5 ) ! Total number of variants values FAST_MODE
      PARAMETER  ( FD_VAR  = 5 ) ! Total number of variants values FAST_DBG
      PARAMETER  ( FC_VAR  = 4 ) ! Total number of variants values FAST_COV
      PARAMETER  ( FMI_VAR = 2 ) ! Number of variants values FAST_MODE for
!                                !        interactive case
      PARAMETER  ( FDI_VAR = 5 ) ! The same for FAST_DBG
      PARAMETER  ( FCI_VAR = 2 ) ! The same for FAST_COV
!
      INTEGER*4  FM_VAL(FM_VAR), FM_LEN(FM_VAR)
      CHARACTER  FM_STR(FM_VAR)*12, FM_ABR(FM_VAR)*3
      DATA   ( FM_VAL(N$), FM_STR(N$), FM_LEN(N$), FM_ABR(N$),
     .                                             N$=1, FM_VAR )
     .       /
     .         F__NONE,   'None        ',   4,    'NON',
     .         F__B3D,    'B3D         ',   3,    'B3D',
     .         F__B1B3D,  'B1B3D       ',   5,    'B1B',
     .         F__PRD,    'Dot_product ',  11,    'DOT',
     .         F__B1D,    'B1D         ',  11,    'B1D'
     .       /
      INTEGER*4  FD_VAL(FD_VAR), FD_LEN(FD_VAR)
      CHARACTER  FD_STR(FD_VAR)*12, FD_ABR(FD_VAR)*3
      DATA   ( FD_VAL(N$), FD_STR(N$), FD_LEN(N$), FD_ABR(N$),
     .                                             N$=1, FD_VAR )
     .       /
     .         F__NONE,   'None        ',   4,    'NON',
     .         F__APP,    'Appearance  ',  10,    'APP',
     .         F__PRI,    'Printout    ',   8,    'PRI',
     .         F__TIM,    'Timer       ',   5,    'TIM',
     .         F__MON,    'Monitor     ',   7,    'MON'
     .       /
      INTEGER*4  FC_VAL(FC_VAR), FC_LEN(FC_VAR)
      CHARACTER  FC_STR(FC_VAR)*12, FC_ABR(FC_VAR)*3
      DATA   ( FC_VAL(N$), FC_STR(N$), FC_LEN(N$), FC_ABR(N$),
     .                                             N$=1, FC_VAR )
     .       /
     .         F__GLO,    'Global      ',   6,    'GLO',
     .         F__LOC,    'Local       ',   5,    'LOC',
     .         F__SEG,    'Segmented   ',   9,    'SEG',
     .         F__FUL,    'Full        ',   4,    'FUL'
     .       /
!
! <<<<< end of INCLUDE-BLOCK  fast.i
!
