!
! >>>>> Include block with astronomical constants
! >>>>> 2004.11.18 (c)  L. Petrov  v 1.04  17-APR-2006 21:47:53
!
      REAL*8     PI__NUM, P2I, PI2, MM__TO__SEC, ARCSEC__TO__RAD,
     .           RAD__TO__ARCSEC, SEC__TO__RAD, RAD__TO__SEC,
     .           MAS__TO__RAD, RAD__TO__MAS, RAD__TO__MSEC,
     .           MSEC__TO__RAD, DEG__TO__RAD, YEAR__TO__SEC, YEAR__TO__DAY,
     .           J2000__JD, J2000__YR, JYEAR__DAYS, CENT__TO__SEC
      INTEGER*4  J2000__MJD
      PARAMETER  ( PI__NUM = 3.141592653589793D0 )
      PARAMETER  ( PI2     = 2.0D0*PI__NUM       )
      PARAMETER  ( P2I     = PI__NUM/2.0D0       )
      PARAMETER  ( MM__TO__SEC   = 0.001D0/299792458.D0 ) ! number of mm in 1 sec
      PARAMETER  ( RAD__TO__MAS  = 3600.0D0*1000.0D0*180.0D0/PI__NUM   )
      PARAMETER  ( ARCSEC__TO__RAD = PI__NUM/(180.0D0*3600.0D0) )
      PARAMETER  ( RAD__TO__ARCSEC = (180.0D0*3600.0D0)/PI__NUM  )
      PARAMETER  ( SEC__TO__RAD    = PI__NUM/(12.0D0*3600.0D0) )
      PARAMETER  ( RAD__TO__SEC    = (12.0D0*3600.0D0)/PI__NUM )
      PARAMETER  ( MAS__TO__RAD  = PI__NUM/(3600.0D0*180.0D0*1000.0D0) )
      PARAMETER  ( RAD__TO__MSEC = RAD__TO__MAS/15.0D0 )
      PARAMETER  ( MSEC__TO__RAD = MAS__TO__RAD*15.0D0 )
      PARAMETER  ( DEG__TO__RAD  = PI__NUM/180.0D0 )
      PARAMETER  ( YEAR__TO__SEC = 31556925.9747D0 ) ! Tropical year
      PARAMETER  ( YEAR__TO__DAY = YEAR__TO__SEC/86400.0D0 ) ! Tropical year
      REAL*8       OM__EAR ! Nominal angular rotation of the Earth
!!      PARAMETER  ( OM__EAR = 7.292115146706387D-05 ) ! rad/sec
      PARAMETER  ( OM__EAR = 7.292115146706979D-05 ) ! rad/sec
      PARAMETER  ( J2000__JD   = 2451545.0D0 ) ! 2000.01.01_12:00:00
      PARAMETER  ( J2000__MJD  = 51544       ) ! 2000.01.01_00:00:00
      PARAMETER  ( J2000__YR   = 2000.0D0    )
      PARAMETER  ( JYEAR__DAYS = 365.25D0    ) ! Length of the Julian day
      PARAMETER  ( CENT__TO__SEC = 36525.0D0*86400.0D0 )
!
      REAL*8       UT1__TO__E3 ! E3(t) in rad = UT1__TO__E3 * {UT1-TAI}(t) in sec
      PARAMETER  ( UT1__TO__E3 = -1.002737909D0*PI2/86400.0D0 )
!
! >>>>> End of include block for package astro_constants.i
!
