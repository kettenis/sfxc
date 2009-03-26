!
! >>>>> INCLUDE-BLOCK with descriptions of the data structures used by the
!       DiaGI (Dialogue Graphic Interface) utility
!
!       diagi.i  13-OCT-97 v2.53  (c)  L. Petrov  --  2006.05.30_12:10:16
!
! --- Limits specifications
!
      INTEGER*4    MBST, MHLP, MLST, MOST, MPST, MWST, MCLR, MDEV, MSPL,
     .             MUSF, MUSA
      INTEGER*4    N$DIAGI, N1$DIAGI, N2$DIAGI, N3$DIAGI
      PARAMETER  ( MBST = 5    ) ! max number of error bar styles [0,MBST-1]
      PARAMETER  ( MHLP = 1024 ) ! max number of lines for help buffer
      PARAMETER  ( MLST = 3    ) ! max number of line styles
      PARAMETER  ( MOST = 2    ) ! max number of overplot modes
      PARAMETER  ( MPST = 5    ) ! max number of point styles
      PARAMETER  ( MWST = 3    ) ! max number of line width modes
      PARAMETER  ( MCLR = 32   ) ! max number of functions to plotted (colours)
      PARAMETER  ( MDEV = 10   ) ! number of supported devices
      PARAMETER  ( MSPL = 512  ) ! number of points for spline interpolation
      PARAMETER  ( MUSF =  64  ) ! max number of user function
      PARAMETER  ( MUSA =  32  ) ! max number of argument in a user function
!
! --- Possible values for ITRM field -- reaction for termination
!
      INTEGER*4  DIAGI__CLOSE, DIAGI__ERASE, DIAGI__CLOSE_VERBOSE,
     .           DIAGI__KEEP, DIAGI__CONT, DIAGI__QUIT, DIAGI__NOMP
      PARAMETER  ( DIAGI__CLOSE = 0 )
      PARAMETER  ( DIAGI__CLOSE_VERBOSE = 1 )
      PARAMETER  ( DIAGI__ERASE =  2 )
      PARAMETER  ( DIAGI__KEEP  =  3 )
      PARAMETER  ( DIAGI__CONT  =  1 )
      PARAMETER  ( DIAGI__QUIT  = -1 )
      PARAMETER  ( DIAGI__NOMP  =  4 )
!
! --- Built-in defaults
!
      INTEGER*4   IBST__DEF, ILST__DEF, IOST__DEF, IPST__DEF,
     .            IWST__DEF, ICL1__DEF, ICL2__DEF, ICL3__DEF,
     .            IXS__MIN,  IXS__MAX,  IBT__MIN,  IDEV_MUL
      CHARACTER   ZAG__DEF*10, NAME__DEF*10, ARG_UNITS__DEF*1
      PARAMETER ( IBST__DEF = 1 ) ! error bar style
      PARAMETER ( ILST__DEF = 2 ) ! line style
      PARAMETER ( IOST__DEF = 0 ) ! overplot style
      PARAMETER ( IPST__DEF = 4 ) ! point style
      PARAMETER ( IWST__DEF = 1 ) ! line width mode
      PARAMETER ( ICL1__DEF = 1 ) ! default colour index for the first  function
      PARAMETER ( ICL2__DEF = 2 ) ! default colour index for the second function
      PARAMETER ( ICL3__DEF = 3 ) ! default colour index for the third function
      PARAMETER ( IXS__MIN  = 1 ) ! Minimal code of XS-device
      PARAMETER ( IXS__MAX  = 3 ) ! Maximal code of XS-device
      PARAMETER ( IDEV_MUL  = 4 ) ! Device type for Multi-DiaGI
      PARAMETER ( IBT__MIN  = 5 ) ! Minimal code for non-interactive device
      PARAMETER ( ZAG__DEF  = 'DiaGI plot' ) ! default plot title
      PARAMETER ( NAME__DEF = '/tmp/diagi' ) ! default plot name
      PARAMETER ( ARG_UNITS__DEF = ' '     ) ! default units of argument
!
! --- Supported environment variables
!
      CHARACTER   DIAGI_IBST*10, DIAGI_ILST*10, DIAGI_IOST*10,   DIAGI_IPST*10,
     .            DIAGI_IWST*10, DIAGI_CTIT*10, DIAGI_UNIT*10,   DIAGI_ICL1*10,
     .            DIAGI_ICL2*10, DIAGI_ICL3*10, DIAGI_SCREEN*12, DIAGI_PRICOM*12
      PARAMETER  ( DIAGI_IBST = 'DIAGI_IBST' )
      PARAMETER  ( DIAGI_ILST = 'DIAGI_ILST' )
      PARAMETER  ( DIAGI_IOST = 'DIAGI_IOST' )
      PARAMETER  ( DIAGI_IPST = 'DIAGI_IPST' )
      PARAMETER  ( DIAGI_IWST = 'DIAGI_IWST' )
      PARAMETER  ( DIAGI_CTIT = 'DIAGI_CTIT' )
      PARAMETER  ( DIAGI_UNIT = 'DIAGI_UNIT' )
      PARAMETER  ( DIAGI_ICL1 = 'DIAGI_ICL1' )
      PARAMETER  ( DIAGI_ICL2 = 'DIAGI_ICL2' )
      PARAMETER  ( DIAGI_ICL3 = 'DIAGI_ICL3' )
      PARAMETER  ( DIAGI_SCREEN = 'DIAGI_SCREEN' ) ! Command for printing
      PARAMETER  ( DIAGI_PRICOM = 'DIAGI_PRICOM' ) ! Command for printing
      CHARACTER  DIAGI_LABEL__DEF*43, MULTI_DIAGI_LABEL__DEF*33, DIAGI_OUT*10,
     .           PS_DIAGI*3,  GIF_DIAGI*4, OUT_DIAGI*4, SAV_DIAGI*4,
     .           TAB_DIAGI*4, HELP_DIR*8,
     .           DIAGI_HLP_FIL0*32, DIAGI_HLP_TIT0*32,
     .           DIAGI_HLP_FIL1*32, DIAGI_HLP_TIT1*32,
     .           DIAGI_HLP_FIL2*32, DIAGI_HLP_TIT2*32,
     .           DIAGI_HLP_FIL3*32,
     .           DIAGI_HLP_FIL4*32,
     .           DIAGI_HLP_FIL5*32,
     .           DIAGI_HLP_FIL6*32
      PARAMETER  ( DIAGI_LABEL__DEF =
     .            'DiaGI v 2.53  Enter command (? for help) >>' ) ! DiaGI label
      PARAMETER  ( MULTI_DIAGI_LABEL__DEF =
     .            'Multi_DiaGI v 2.53 hit ? for help' ) ! Multi_DiaGI label
      PARAMETER  ( DIAGI_OUT    = '/tmp/diagi'   ) ! Template file name for
!                                                  ! the print files
      PARAMETER  ( PS_DIAGI     = '.ps'  ) ! Extension for postscript output
      PARAMETER  ( GIF_DIAGI    = '.gif' ) ! Extension for gif output
      PARAMETER  ( OUT_DIAGI    = '.out' ) ! Extension for template output
      PARAMETER  ( SAV_DIAGI    = '.sav' ) ! Extension for save output
      PARAMETER  ( TAB_DIAGI    = '.tab' ) ! Extension for save table
      PARAMETER  ( HELP_DIR = 'HELP_DIR' ) ! Name of the environment variable
!                             ! with directory name where help files are kept
!
! --- Names and titles of help files
!
      PARAMETER  ( DIAGI_HLP_FIL0 = 'diagi_0.hlp                     ' )
      PARAMETER  ( DIAGI_HLP_TIT0 = 'DiaGI. Command usage.           ' )
      PARAMETER  ( DIAGI_HLP_FIL1 = 'diagi_1.hlp                     ' )
      PARAMETER  ( DIAGI_HLP_TIT1 = 'DiaGI. User guide.              ' )
      PARAMETER  ( DIAGI_HLP_FIL2 = 'diagi_2.hlp                     ' )
      PARAMETER  ( DIAGI_HLP_TIT2 = 'DiaGI. Description of commands. ' )
      PARAMETER  ( DIAGI_HLP_FIL3 = 'diagi_3.hlp                     ' )
      PARAMETER  ( DIAGI_HLP_FIL4 = 'diagi_4.hlp                     ' )
      PARAMETER  ( DIAGI_HLP_FIL5 = 'diagi_5.hlp                     ' )
      PARAMETER  ( DIAGI_HLP_FIL6 = 'diagi_6.hlp                     ' )
!
      CHARACTER    DIAGI__PGDN*1, DIAGI__PGUP*1
!
! --- Definition moved into diagi.f and multi_diagi.f due to a bug
! --- in HP Fortran compiler
!
!      PARAMETER  ( DIAGI__PGDN = CHAR(221) ) ! PageDown key code
!      PARAMETER  ( DIAGI__PGUP = CHAR(220) ) ! PageUp   key code
!
! --- Specification of the minimal acceptable relation of coordinate interval
! --- (for both x- and y- coordinates) to the values of coordinate
!
      REAL*4       DIAGI_EPS
      PARAMETER  ( DIAGI_EPS = 1.E-6 )
      REAL*4       DIAGI_FIE
      PARAMETER  ( DIAGI_FIE = 0.02 ) ! Fields for min box (relative units)
!
      CHARACTER  DEVS(MDEV)*5
      REAL*4     XLEFTS(MDEV), XRIGHTS(MDEV), YBOTS(MDEV), YTOPS(MDEV),
     .           SCH_LABS(MDEV), YSH_LABS(MDEV),
     .           SCH_TITS(MDEV), YSH_TITS(MDEV), SCH_FRMS(MDEV),
     .           YSH_ARU(MDEV)
      INTEGER*4  ISLW_LABS(MDEV), ISLW_TITS(MDEV),
     .           IWID_THN, IWID_MED(MDEV), IWID_THK(MDEV)
      DATA ( DEVS(N$DIAGI), XLEFTS(N$DIAGI), XRIGHTS(N$DIAGI),
     .                      YBOTS(N$DIAGI),  YTOPS(N$DIAGI),
     .       SCH_LABS(N$DIAGI),  YSH_LABS(N$DIAGI), ISLW_LABS(N$DIAGI),
     .       SCH_TITS(N$DIAGI),  YSH_TITS(N$DIAGI), ISLW_TITS(N$DIAGI),
     .       SCH_FRMS(N$DIAGI),  IWID_MED(N$DIAGI), IWID_THK(N$DIAGI),
     .       YSH_ARU(N$DIAGI),
     .       N$DIAGI=1,MDEV ) /
!
! -- Table of device characteristics:
! --  1) PGPLOT-type device name,
! --  2) shift of the bounfing box to the left edge;
! --  3) x-coordiante of the right edge;
! --  4) shift to the bottom edge;
! --  5) y-coordinate of the top edge;
! --  6) font height of DiaGI labels (PGPLOT relative units);
! --  7) relative shift for the placement of DiaGI label;
! --  8) line width style for the labels;
! --  9) font height for the title in PGPLOT units;
! -- 10) relative shift for placement of the title;
! -- 11) line width for the title;
! -- 12) font height for drawing the digital labels (in PGPLOT units);
! -- 13) Width of medium line (PGPLOT units);
! -- 14) Width of thick line.
! -- 15) relative shift for placement of the units for argument
! --
! -- All sizes are in mm
!
!       (1)     (2) (3)    (4) (5)   (6)    (7)   (8) (9) (10) (11)(12)(13)(14)
!                                                         [15]
!
     . '/XS  ', 30., 360., 20., 260., 1.10, -0.07, 3, 1.2, 1.015, 5, 0.6, 5,  8,
     .                                                    -0.07,
     . '/XS  ', 25., 304., 15., 200., 1.20, -0.07, 3, 1.5, 1.015, 5, 0.8, 5, 10,
     .                                                    -0.06,
     . '/XS  ', 25., 264., 15., 165., 1.20, -0.07, 3, 1.5, 1.015, 5, 0.8, 5, 10,
     .                                                    -0.06,
     . '/XS  ',  0.,  50.,  0.,  30., 0.80,  0.00, 1, 0.7,-1.00,  1, 0.5, 1,  2,
     .                                                    -0.12,
     . '/CPS ', 30., 240., 15., 190., 1.20, -0.07, 3, 1.3, 1.03,  3, 0.7, 3,  7,
     .                                                    -0.08,
     . '/VCPS', 30., 153., 28., 118., 1.20, -0.07, 2, 1.0, 1.03,  2, 0.9, 2,  5,
     .                                                    -0.09,
     . '/VCPS', 30.,  81., 28.,  74., 1.20, -0.07, 2, 1.0, 1.05,  2, 1.1, 2,  5,
     .                                                    -0.09,
     . '/GIF ', 30., 240., 15., 190., 1.20, -0.07, 3, 1.3, 1.02,  4, 0.9, 5, 10,
     .                                                    -0.08,
     . '/GIF ', 30., 153., 28., 118., 1.20, -0.07, 2, 1.1, 1.03,  2, 0.9, 2,  6,
     .                                                    -0.09,
     . '/GIF ', 30.,  81., 28.,  74., 1.20, -0.07, 1, 1.1, 1.05,  1, 1.0, 1,  4,
     .                                                    -0.13 
     .                        /
      PARAMETER  ( IWID_THN = 1 ) ! Wifth of the thin lines
      INTEGER*4    GIF_RES
      PARAMETER  ( GIF_RES = 85 ) ! Resoluton (pixels per inch) of GIF "device"
!                                 ! as it hardcoded in PGPLOT
      REAL*4     BOX_BOTMES(2,4)
      DATA       BOX_BOTMES  /   & ! Right (X) and Top(Y) coord. of bottom message
     .           1.000, 0.052,   & ! for /XS device 1 (huge)
     .           1.000, 0.045,   & ! for /XS device 1 (big)
     .           1.000, 0.058,   & ! for /XS device 2 (small)
     .           0.799, 0.030    & ! for /XS device 3 (Multi_DiaGI)
     .                       /
      REAL*4     DIAGI_XRM
      PARAMETER  ( DIAGI_XRM = 0.166 ) ! Size of right margin as a share of left one
!
! --- Size of the circles
!
      REAL*4        RAD_TINY_MM, RAD_SMALL_MM, RAD_LARGE_MM
      INTEGER*4     NPTS_SMALL,  NPTS_LARGE
      PARAMETER   ( RAD_TINY_MM  = 0.35 ) ! Radius of tiny  circle (mm)
      PARAMETER   ( RAD_SMALL_MM = 0.5  ) ! Radius of small circle (mm)
      PARAMETER   ( RAD_LARGE_MM = 1.0  ) ! Radius of large circle (mm)
      PARAMETER   ( NPTS_SMALL   = 16   ) ! Number of points for small circle
      PARAMETER   ( NPTS_LARGE   = 32   ) ! Number of points for large circle
!
! --- Font specifications for help messages
!
      INTEGER*4   ISLW_HLT, ISCF_HLT, ISLW_HLP, ISCF_HLP
      REAL*4       SCH_HLT,  SCH_HLP
      PARAMETER  ( ISLW_HLT = 8    ) ! Line wirdth for help title
      PARAMETER  ( ISCF_HLT = 2    ) ! Font style for help title
      PARAMETER  (  SCH_HLT = 2.00 ) ! Font size for help title
      PARAMETER  ( ISLW_HLP = 4    ) ! Line wirdth for help messages
      PARAMETER  ( ISCF_HLP = 2    ) ! Font style for help messages
      PARAMETER  (  SCH_HLP = 1.30 ) ! Font size for help messages
!
! --- Data structure for holding coordinates of rectangular boxes
!
      TYPE      DIAGI_BOXES
          REAL*4    XLB
          REAL*4    YLB
          REAL*4    XTU
          REAL*4    YTU
      END TYPE  DIAGI_BOXES ! DIAGI_BOXES !
!
! --- Data structure for holding parameters of DiaGI
!
      TYPE      DIAGI_STRU
!
! ------- External (user-defined) parameters
!
          INTEGER*4  IFIRST_FIELD  ! First address keeper (not used)
          INTEGER*4  IDEV          ! Device type for XS
          INTEGER*4  NCLR          ! Number of the functions to be plotted
          INTEGER*4  NPOI(MCLR)    ! Array of number of points for each function
          INTEGER*4  ADR_X8(MCLR)  ! Arrays of addresses of the 1-st element of
!                    ! the REAL*8 arrays keeping arguments of the function.
          INTEGER*4  ADR_Y8(MCLR)  ! Arrays of addresses of the 1-st element of
!                    ! the REAL*8 arrays keeping values of the function.
          INTEGER*4  ADR_E8(MCLR)  ! Arrays of addresses of the 1-st element of
!                    ! the REAL*8 arrays keeping errors of the function.
          LOGICAL*4  LER(MCLR)     ! Array of flags using errors of the function
          INTEGER*4  ICOL(MCLR)    ! Attribute of colour usage
          INTEGER*4  MCOL_R(MCLR)  ! R- intensity for the main colour
          INTEGER*4  MCOL_G(MCLR)  ! G- intensity for the main colour
          INTEGER*4  MCOL_B(MCLR)  ! B- intensity for the main colour
          INTEGER*4  SCOL_R(MCLR)  ! R- intensity for the shadow colour
          INTEGER*4  SCOL_G(MCLR)  ! G- intensity for the shadow colour
          INTEGER*4  SCOL_B(MCLR)  ! B- intensity for the shadow colour
          INTEGER*4  IBST(MCLR)    ! Error bar styel for the function
          INTEGER*4  ILST(MCLR)    ! Line  style for the function
          INTEGER*4  IOST(MCLR)    ! Overplot style for the function
          INTEGER*4  IPST(MCLR)    ! Point style for the function
          INTEGER*4  IWST(MCLR)    ! Line width mode
          INTEGER*4  ICLR          ! Current colour ( main function)
          REAL*4     XMIN          ! X-world coordinate of the left  boundary
          REAL*4     XMAX          ! X-world coordinate of the right boundary
          REAL*4     YMIN          ! Y-world coordinate of the bottom boundary
          REAL*4     YMAX          ! Y-world coordinate of the top boundary
          CHARACTER  ZAG*128       ! Title of the plot
          CHARACTER  ARG_UNITS*128 ! Title and units of arguments
          CHARACTER  NAME*128      ! Output name of the plot
          INTEGER*4  ITRM          ! Code action for termination
          INTEGER*4  IBATCH        ! Code of batch mode. (0 for interactive)
!
! ------- These fields for advanced users. Left them zero, unless you are 
! ------- really understand what you are doing.
!
          INTEGER*4  NUSER_FUNC       ! The number of user functions
          INTEGER*4  USER_FUNC(MUSF)  ! Addresses of user functions
          INTEGER*4  USER_ARG(0:MUSA,MUSF) ! Argument list for user functions
          CHARACTER  USER_CHR(MUSF)*1 ! Binding symbol of the user functiuon
	  INTEGER*4  INIT_USER_FUNC   ! Index of initialization user function
	  INTEGER*4  UPDATE_USER_FUNC ! Index of update user function
	  INTEGER*4  QUIT_USER_FUNC   ! Index of quit user function
	  INTEGER*4  MD_IN            ! Input  parameter for Multi_DiaGI
	  INTEGER*4  MD_OUT           ! Output parameter for Multi_DiaGI
!
! ------- Internal (hidden) parameters which should not be defined by user,
! ------- since DIAGI initializes them. They should never be attempt to
! ------- be changed, except in DiaGI user functions. A great care should
! ------- be taken.
!
          INTEGER*4  ADR_X4(MCLR)  ! Array of address of the 1-st element of
!                    ! the REAL*4 arrays keeping arguemnts of the function
          INTEGER*4  ADR_Y4(MCLR)  ! Array of address of the 1-st element of
!                    ! the REAL*4 arrays keeping values of the function
          INTEGER*4  ADR_E4(MCLR)  ! Array of address of the 1-st element of
!                    ! the REAL*4 arrays keeping errors of the function
	  INTEGER*4  MEM_ADR(MCLR) ! Address of the first byte of a block 
!                                  ! of allocated dynamic memory
	  INTEGER*4  MEM_LEN(MCLR) ! Length of the allocated dynamic memory
!
! ------- Current status of some unternal parameters
!
          REAL*4     XC             ! Current cursor position
          REAL*4     YC             ! Current cursor position
          CHARACTER  MESS_BOT*128   ! Current bottom message
          CHARACTER  MESS_BOT_SAVED*128   ! Saved bottom message
	  INTEGER*4  IPQ            ! Index of the last point inquired
	  INTEGER*4  ICQ            ! Index of the last color inquired
!
! ------- Specifications of the common parameters for all plots at the
! ------- current device
!
          CHARACTER  DEVICE*5  ! PGPLOT-like device name;
	  CHARACTER  FILLER_1*3  ! Filler
          REAL*4     XLEFT     ! Shift to the left edge;
          REAL*4     XRIGHT    ! X-coordiante of the right edge;
          REAL*4     YBOT      ! Shift to the bottom edge;
          REAL*4     YTOP      ! Y-coordinate of the top edge;
          REAL*4     SCH_LAB   ! Font height of DiaGI labels;
          REAL*4     YSH_LAB   ! Relative shift for placement of DiaGI label;
          INTEGER*4  ISLW_LAB  ! Line width style for the labels;
          REAL*4     SCH_TIT   ! Font height for the title in PGPLOT units;
          REAL*4     YSH_TIT   ! Relative shift for placement of the title;
          INTEGER*4  ISLW_TIT  ! Line width for the title;
          REAL*4     SCH_FRM   ! Font height for drawing the digital labels.
          INTEGER*4  IWD_LINS(MWST) ! Widths of the thin, medium and thick lines
          REAL*4     YSH_ARU   ! Relative shift for placement of the arg units;
          REAL*4     RAD_SMALL ! Radii of small circle (in mm)
          REAL*4     RAD_LARGE ! Radii of large circle (in mm)
!
          INTEGER*4  IBST_SAV(MCLR) ! Saved initial error bar styel for function
          INTEGER*4  ILST_SAV(MCLR) ! Saved initial
          INTEGER*4  IPST_SAV(MCLR) ! Saved initial
          INTEGER*4  IWST_SAV(MCLR) ! Saved initial
          INTEGER*4  ICLR_SAV       ! Saved initial current colour
          REAL*4     XMIN_SAV       ! Saved initial X-coor. of left   boundary
          REAL*4     XMAX_SAV       ! Saved initial X-coor. of right  boundary
          REAL*4     YMIN_SAV       ! Saved initial Y-coor. of bottom boundary
          REAL*4     YMAX_SAV       ! Saved initial Y-coor. of top    boundary
          CHARACTER  ZAG_SAV*128    ! Saved initial plot title
          LOGICAL*4  SET_VP         ! If true viewport will be reset before
!                                   ! plotting
          LOGICAL*4  ERASE_SCREEN   ! If true screen will be erased before
	  CHARACTER  LAST_KEY*1     ! Code of the last key pressed
	  CHARACTER  FILLER_2*3     ! Filler
!
          INTEGER*4  STATUS  ! status of DIAGIS data structure
      END TYPE  DIAGI_STRU  ! DIAGI_STRU !
!
! --- Symbolic names of the DIAGS statuses
!
      INTEGER*4  DIA__UND,  DIA__DEF, DIA__ALL
      PARAMETER  ( DIA__UND = -1 ) ! DIAGI is undefined
      PARAMETER  ( DIA__DEF =  1 ) ! DIAGI is defined but memory is not grabbed
      PARAMETER  ( DIA__ALL =  2 ) ! DIAGI is defined and memory is allocated
!
! --- Predefined colour table for internal needs of DiaGI
!
      INTEGER*4  BCG_CLRI,   FRG_CLRI,   ERR_CLRI,   SLG_CLRI,   DEG_CLRI
      INTEGER*4  BCG_CLR(3), FRG_CLR(3), ERR_CLR(3), SLG_CLR(3), DEG_CLR(3)
      DATA   BCG_CLRI,  ( BCG_CLR(N$DIAGI), N$DIAGI=1,3 ),
     .       FRG_CLRI,  ( FRG_CLR(N$DIAGI), N$DIAGI=1,3 ),
     .       ERR_CLRI,  ( ERR_CLR(N$DIAGI), N$DIAGI=1,3 ),
     .       SLG_CLRI,  ( SLG_CLR(N$DIAGI), N$DIAGI=1,3 ),
     .       DEG_CLRI,  ( DEG_CLR(N$DIAGI), N$DIAGI=1,3 )
     .    /
     .              0,   220,  220,  220,   & !  BCG  Background colour
     .              1,     0,    0,    0,   & !  FRG  Foreground colour
     .              2,   256,   60,   60,   & !  ERR  Error colour
     .              3,   208,  208,  208,   & !  SLG  Slightly grey colour
     .              4,   180,  180,  180    & !  DEG  Deply grey colour
     .    /
!
! --- Colour tables of main and shadow colours for plotted functions
!
      INTEGER*4  ITAB_CLR(MCLR,2), IRGB_DEF(MCLR,2,3)
      DATA ( (   ITAB_CLR (N1$DIAGI,N2$DIAGI),
     .         ( IRGB_DEF (N1$DIAGI,N2$DIAGI,N3$DIAGI), N3$DIAGI=1,3 ),
     .                                                  N2$DIAGI=1,2   ),
     .                                                  N1$DIAGI=1,MCLR   )
     .   /
     .      5,   67, 180,  38,     6,  192, 220, 185,  & !  1 (H=108)
     .      7,   38,  67, 180,     8,  199, 203, 220,  & !  2 (H=228)
     .      9,  180,  38,  67,    10,  220, 199, 203,  & !  3 (H=348)
     .     11,   38, 180, 151,    12,  185, 220, 213,  & !  4
     .     13,  151,  38, 180,    14,  213, 185, 220,  & !  5
     .     15,  180, 151,  38,    16,  220, 213, 185,  & !  6
     .     17,  240,  14,  14,    18,  240, 211, 211,  & !  7
     .     23,   14,  14, 240,    24,  211, 211, 240,  & !  8
     .     25,  126, 160,  59,    26,  211, 220, 194,  & !  9
     .     27,   93,  59, 160,    28,  207, 202, 220,  & ! 10
     .     19,  127, 240,  14,    20,  224, 240, 211,  & ! 11
     .     21,   14, 240, 240,    22,  211, 240, 240,  & ! 12
     .     29,    0,   0,   0,    30,  210, 210, 210,  & ! 13
     .     31,  144, 144, 144,    32,  210, 210, 210,  & ! 14
     .     33,  172, 172, 172,    34,  210, 210, 210,  & ! 15
     .     35,  200, 200, 200,    36,  210, 210, 210,  & ! 16
     .     37,   72, 208,  38,    38,  189, 210, 185,  & ! 17
     .     39,   38,  67, 180,    40,  185, 192, 220,  & ! 18
     .     41,  180,  38,  67,    42,  220, 185, 192,  & ! 19
     .     43,   38, 180, 151,    44,  185, 220, 213,  & ! 20
     .     45,  151,  38, 180,    46,  213, 185, 220,  & ! 21
     .     47,  180, 151,  38,    48,  220, 213, 185,  & ! 22
     .     49,  240,  14,  14,    50,  220, 213, 185,  & ! 23
     .     55,   14,  14, 240,    56,  220, 213, 185,  & ! 24
     .     57,  126, 160,  59,    58,  211, 220, 194,  & ! 25
     .     59,   93,  59, 160,    60,  202, 194, 220,  & ! 26
     .     51,  127, 240,  14,    52,  220, 213, 185,  & ! 27
     .     53,  196, 196, 196,    54,  208, 208, 208,  & ! 28
     .     61,  208, 208, 208,    62,  220, 220, 220,  & ! 29
     .     63,  220, 220, 220,    64,  220, 220, 220,  & ! 30  ! transparent
     .     65,  240, 240, 240,    66,  255, 255, 255,  & ! 31
     .     67,  255, 255, 255,    68,  255, 255, 255   & ! 32  ! white
     .   /
!
! <<<<< end of INCLUDE-BLOCK  diagi.i
!
