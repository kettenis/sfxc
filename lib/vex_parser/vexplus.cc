//-----------------------------------------------------------------------------
//
//
// Original File: vexplus.C
// Original Date: 9 feb 1998 Author: Henk W. Klijn Hesselink
// Complete Rewrite: 23 Mar 1998: Huib Jan van Langevelde
// Taken over: 29 apr 1998 by Friso Olnon
// Description  : Wrapper around E. Himwich vex-parsing software
// 
//-----------------------------------------------------------------------------

#include "vexplus.h"
#include <assert.h>
#include <string.h>

static char rcsid[]="@(#)$Id: vexplus.C,v 3.6 2006/01/16 11:21:29 jive_cc Exp $";

//________________GENERAL_ACCESS_________________________________________
//
//------------------------------------------------------------------------
// Method:        parseVex
// Description:   open Vex file and parse it to memory
// Input:        
// Output:      
// Return value:  0, Ok
//               -1, could not open file
//               -2, error parsing vex-file to memory
// Side effects: 
//------------------------------------------------------------------------
using namespace std;

int VexPlus::parseVex()
{
    return  vex_open( const_cast<char*>(filename.c_str()), &vexp);
}

//________________$ANTENNA_______________________________________________
//
//------------------------------------------------------------------------
// Method:        AxisMount
// Description:   return the mount of the antenna identified with stat
// Input:         station
// Output:        .
// Return value:  null-string, failed
//                _ separated, concatenated mount, eg az_el
//               
// Side effects:  Is assuming 1 antenna / station!
//------------------------------------------------------------------------
string VexPlus::AxisMount( const string& stat ) const {

  char *value1, *value2, *units;
  int islink, isname;
  void* ptr;
  
  char stat_c[128];
  stat.copy( stat_c, string::npos );
  stat_c[stat.length()] = 0;

  if( !(ptr = get_station_lowl(stat_c,T_AXIS_TYPE,B_ANTENNA,vexp)) ) 
    return string("");
  if( (vex_field(T_AXIS_TYPE, ptr, 1,
		 &islink,&isname,&value1,&units)) ) return string("");

  if( (vex_field(T_AXIS_TYPE, ptr, 2,
		 &islink,&isname,&value2,&units)) ) return string("");

  return string(value1) + "_" + string(value2);
}

//------------------------------------------------------------------------
// Method:        AxisOffset
// Description:   return the axis offset in az-el offset
// Input:         station
// Output:        .
// Return value:  0, not specified or failed
//                value in meters
//               
// Side effects:  
//------------------------------------------------------------------------
double VexPlus::AxisOffset( const string& stat ) const {

  char *value, *units;
  int islink, isname;
  void* ptr;
  
  char stat_c[128];
  stat.copy( stat_c, string::npos );
  stat_c[stat.length()] = 0;

  if( !(ptr = get_station_lowl(stat_c,T_AXIS_OFFSET,B_ANTENNA,vexp)) ) 
    return 0;
  if( (vex_field(T_AXIS_OFFSET, ptr, 1,
		 &islink,&isname,&value,&units)) ) return 0;
  
  // [[SHORTCUT]] Assume axis offset is in meters in VEX
  return atof( value );
}

//________________$BBCS__________________________________________________
//
//------------------------------------------------------------------------
// Method:        N_BBCs
// Description:   Number of bbcs in a BBC ref
// Input:         station & mode of bbcs
// Output:        
// Return value:  0, no channels or error
//                > 0 number of BBCS
//               
// Side effects: 
//------------------------------------------------------------------------
int VexPlus::N_BBCs( const string& stat, const string& mode ) const {
  int icount = 0;
  char stat_c[128], mode_c[128];
  stat.copy( stat_c, string::npos );
  stat_c[stat.length()] = 0;
  mode.copy( mode_c, string::npos );
  mode_c[mode.length()] = 0;
  
  if( (get_all_lowl(stat_c,mode_c,T_BBC_ASSIGN,B_BBC,vexp)) ) {
    do {
      icount++;
    } while ( (get_all_lowl_next()) );
  }
  return icount;
}

//------------------------------------------------------------------------
// Method:        Link_bbc_if
// Description:   return the linkname that links a chan_def to a if
// Input:         station, mode of bbc, icount chan_def -1 to return
// Output:        .
// Return value:  null-string, unresolved or not a link
//               
//               
// Side effects: 
//------------------------------------------------------------------------
string VexPlus::Link_bbc_if( const string& stat, const string& mode, 
			     const int& icount ) const {
  int islink, isname;
  char* link, *units;
  void* ptr;

  char stat_c[128], mode_c[128];
  stat.copy( stat_c, string::npos );
  stat_c[stat.length()] = 0;
  mode.copy( mode_c, string::npos );
  mode_c[mode.length()] = 0;

  if( !(ptr = get_all_lowl(stat_c,mode_c,T_BBC_ASSIGN,B_BBC,vexp)) ) 
    return string("");
  if( (vex_field(T_BBC_ASSIGN, ptr, 3,
		       &islink,&isname,&link,&units)) ) 
    return string("");
  if( !(islink || isname) ) return string("");

  if( icount ) {
    for( int i = 0; i < icount; i++ ) {
      if( !(ptr = get_all_lowl_next()) ) return string("");
    }
    if( (vex_field(T_BBC_ASSIGN, ptr, 3,
		   &islink,&isname,&link,&units)) ) 
      return string("");

  }

  return string(link);

}

//------------------------------------------------------------------------
// Method:        Resolve_bbc_freq
// Description:   return the linkname set for a bbc link from frequency
// Input:         station & mode of bbc, icount chan_def -1 to return
// Output:        .
// Return value:  null-string, unresolved or not a link
//                link name
//               
// Side effects: 
//------------------------------------------------------------------------
string VexPlus::Resolve_bbc_freq( const string& stat, const string& mode, 
				  const int& icount ) const {
  int islink, isname;
  char* name, *units;
  void* ptr;

  char stat_c[128], mode_c[128];
  stat.copy( stat_c, string::npos );
  stat_c[stat.length()] = 0;
  mode.copy( mode_c, string::npos );
  mode_c[mode.length()] = 0;
  
  if( !(ptr = get_all_lowl(stat_c,mode_c,T_BBC_ASSIGN,B_BBC,vexp)) ) 
    return string("");
  if( (vex_field(T_BBC_ASSIGN, ptr, 1,
		       &islink,&isname,&name,&units)) ) 
    return string("");
  if( !(islink || isname) ) return string("");

  if( icount ) {
    for( int i = 0; i < icount; i++ ) {
      if( !(ptr = get_all_lowl_next()) ) return string("");
    }
    if( (vex_field(T_BBC_ASSIGN, ptr, 1,
		   &islink,&isname,&name,&units)) ) 
      return 0;
  }

  return string(name);
}

//________________$CLOCK_________________________________________
//
//------------------------------------------------------------------------
// Method:        ClockOffset
// Description:   return the station's formatter clock offset
// Input:         station
// Output:        offset in microseconds
// Return value:  0, not given or error
//               
// Side effects:  [[TODO]] Assumed: no 'clock breaks'
//------------------------------------------------------------------------
double VexPlus::ClockOffset( const string& stat ) const
{
  char *value, *units;
  int islink, isname;
  void* ptr;
  
  char stat_c[128];
  stat.copy( stat_c, string::npos );
  stat_c[stat.length()] = 0;

  if( !(ptr = get_station_lowl(stat_c,T_CLOCK_EARLY,B_CLOCK,vexp)) ) 
    return 0;
  if( (vex_field(T_CLOCK_EARLY, ptr, 2,
		 &islink,&isname,&value,&units)) ) return 0;

  // [[SHORTCUT]] Assume offset in microseconds in VEX
  return atof( value );
}

//------------------------------------------------------------------------
// Method:        ClockRate
// Description:   return the station's formatter clock offset rate
// Input:         station
// Output:        rate in microseconds/second
// Return value:  0, not given or error
//               
// Side effects:  [[TODO]] Assumed: no 'clock breaks'
//------------------------------------------------------------------------
double VexPlus::ClockRate( const string& stat ) const
{
  char *value, *units;
  int islink, isname;
  void* ptr;
  
  char stat_c[128];
  stat.copy( stat_c, string::npos );
  stat_c[stat.length()] = 0;

   if( !(ptr = get_station_lowl(stat_c,T_CLOCK_EARLY,B_CLOCK,vexp)) ) 
    return 0;
  if( (vex_field(T_CLOCK_EARLY, ptr, 4,
		 &islink,&isname,&value,&units)) ) return 0;

  // [[SHORTCUT]] Assume rate in microseconds/second in VEX
  return atof( value );
}

//------------------------------------------------------------------------
// Method:        ClockEpoch
// Description:   return the epoch of origin of this clock model
// Input:         station
// Output:        epoch in VEX-format character string
// Return value:  null-string, not given or error
//               
// Side effects:  [[TODO]] Assumed: no 'clock breaks'
//------------------------------------------------------------------------
string VexPlus::ClockEpoch( const string& stat ) const
{
  char *value, *units;
  int islink, isname;
  void* ptr;
  
  char stat_c[128];
  stat.copy( stat_c, string::npos );
  stat_c[stat.length()] = 0;

  if( !(ptr = get_station_lowl(stat_c,T_CLOCK_EARLY,B_CLOCK,vexp)) ) 
    return string("");
  if( (vex_field(T_CLOCK_EARLY, ptr, 3,
		 &islink,&isname,&value,&units)) ) return string("");
  return string(value);
}

//________________$DAS___________________________________________________
//
//------------------------------------------------------------------------
// Method:        Recorder
// Description:   return the recorder type used at stat
// Input:         station
// Output:        .
// Return value:  null-string, failed
//                recorder string
//               
// Side effects:  [[TODO]] Recorders should be enumerated?
//------------------------------------------------------------------------
string VexPlus::Recorder( const string& stat ) const {

  char *value, *units;
  int islink, isname;
  void* ptr;
  
  char stat_c[128];
  stat.copy( stat_c, string::npos );
  stat_c[stat.length()] = 0;

  if( !(ptr = get_station_lowl(stat_c,T_RECORD_TRANSPORT_TYPE,B_DAS,vexp)) ) 
    return string("");
  if( (vex_field(T_RECORD_TRANSPORT_TYPE, ptr, 1,
		 &islink,&isname,&value,&units)) ) return string("");

  return string(value);
}

//------------------------------------------------------------------------
// Method:        MediumLength
// Description:   return the length of medium used at stat (feet or bytes)
// Input:         station, icount chan_def -1 to return
// Output:        .
// Return value:  0, failed
//                value in feet
//               
// Side effects:  
//------------------------------------------------------------------------
double VexPlus::MediumLength( const string& stat ) const {

  char *value, *units;
  int islink, isname;
  void* ptr;
  
  char stat_c[128];
  stat.copy( stat_c, string::npos );
  stat_c[stat.length()] = 0;

  if( !(ptr = get_station_lowl(stat_c,T_TAPE_LENGTH,B_DAS,vexp)) ) 
    return 0;
  if( (vex_field(T_TAPE_LENGTH, ptr, 1,
		 &islink,&isname,&value,&units)) ) return 0;
  
  // [[SHORTCUT]] Assume medium length is in ft in VEX
  return atof( value );
}

//------------------------------------------------------------------------
// Method:        TapeDensity
// Description:   return the density of tape recorded at stat
// Input:         station, icount chan_def -1 to return
// Output:        .
// Return value:  0, failed
//                value in bpi
//               
// Side effects:  
//------------------------------------------------------------------------
double VexPlus::TapeDensity( const string& stat ) const {

  char *value, *units;
  int islink, isname;
  void* ptr;
  
  char stat_c[128];
  stat.copy( stat_c, string::npos );
  stat_c[stat.length()] = 0;

  if( !(ptr = get_station_lowl(stat_c,T_RECORD_DENSITY,B_DAS,vexp)) ) 
    return 0;
  if( (vex_field(T_RECORD_DENSITY, ptr, 1,
		 &islink,&isname,&value,&units)) ) return 0;

  // [[SHORTCUT]] Density is always in bpi in VEX
  return atof(value);
}

//________________$EOP____________________________________________________
//
//------------------------------------------------------------------------
// Method:        TAI_UTC
// Description:   return the tai-utc in seconds
// Input:         none, global values.
// Output:        .
// Return value:  9e9, no number or error
//               
//               
// Side effects: 
//------------------------------------------------------------------------
double VexPlus::TAI_UTC() const {

  void* ptr;  
  int   islink, isname;
  char *value, *units;

  if( !(ptr = get_global_lowl(T_TAI_UTC,B_EOP,vexp)) ) 
    return 9e9;
  if( (vex_field(T_TAI_UTC, ptr, 1,
		 &islink,&isname,&value,&units)) ) 
    return 9e9;
  // [[SHORTCUT]] Assuming utc offset is in seconds
  return atof( value );
}

//------------------------------------------------------------------------
// Method:        A1_TAI
// Description:   return the A1_TAI in seconds
// Input:         none, global value
// Output:        .
// Return value:  9e9, no number or error
//               
//               
// Side effects: 
//------------------------------------------------------------------------
double VexPlus::A1_TAI() const {

  void* ptr;  
  int   islink, isname;
  char *value, *units;

  if( !(ptr = get_global_lowl(T_A1_TAI,B_EOP,vexp)) ) 
    return 9e9;
  if( (vex_field(T_TAI_UTC, ptr, 1,
		 &islink,&isname,&value,&units)) ) 
    return 9e9;
  // [[SHORTCUT]] Assuming utc offset is in seconds
  return atof( value );
}

//------------------------------------------------------------------------
// Method:        EOPEpoch
// Description:   return the tai-utc
// Input:         none, global values
// Output:        .
// Return value:  null-string, no epoch or error
//               
//               
// Side effects: 
//------------------------------------------------------------------------
string VexPlus::EOPEpoch() const {

  void* ptr;  
  int   islink, isname;
  char *value, *units;

  if( !(ptr = get_global_lowl(T_EOP_REF_EPOCH,B_EOP,vexp)) ) 
    return string("");
  if( (vex_field(T_EOP_REF_EPOCH, ptr, 1,
		 &islink,&isname,&value,&units)) ) 
    return string("");
  return string(value);
}

//------------------------------------------------------------------------
// Method:        N_EOP_points
// Description:   return the Number of EOP points
// Input:         none, global settings
// Output:        .
// Return value:  0, no number or error
//               
//               
// Side effects: 
//------------------------------------------------------------------------
int VexPlus::N_EOP_Points() const {

  void* ptr;  
  int   islink, isname;
  char *value, *units;

  if( !(ptr = get_global_lowl(T_NUM_EOP_POINTS,B_EOP,vexp)) ) 
    return 0;
  if( (vex_field(T_NUM_EOP_POINTS, ptr, 1,
		 &islink,&isname,&value,&units)) ) 
    return 0;
  // [[SHORTCUT]] Assuming utc offset is in seconds
  return atoi( value );
}

//------------------------------------------------------------------------
// Method:        EOP_interval
// Description:   return the interval of the EOP points IN HOURS
// Input:         none, global settings
// Output:        .
// Return value:  0, no number or error
//               
//               
// Side effects: 
//------------------------------------------------------------------------
double VexPlus::EOP_interval() const {

  void* ptr;  
  int   islink, isname;
  char *value, *units;

  if( !(ptr = get_global_lowl(T_EOP_INTERVAL,B_EOP,vexp)) ) 
    return 0;
  if( (vex_field(T_EOP_INTERVAL, ptr, 1,
		 &islink,&isname,&value,&units)) ) 
    return 0;
  // [[SHORTCUT]] Assuming value is in hours
  return atof( value );
}

//------------------------------------------------------------------------
// Method:        UT1_UTC
// Description:   return the icount-1 ut1-utc value in sec
// Input:         none, global settings
// Output:        .
// Return value:  9e9, no number or error
//               
//               
// Side effects: 
//------------------------------------------------------------------------
double VexPlus::UT1_UTC(const int& icount) const {

  void* ptr;  
  int   islink, isname;
  char *value, *units;

  if( !(ptr = get_global_lowl(T_UT1_UTC,B_EOP,vexp)) ) 
    return 9e9;
  if( (vex_field(T_UT1_UTC, ptr, icount+1,
		 &islink,&isname,&value,&units)) ) 
    return 9e9;
  // [[SHORTCUT]] Assuming utc offset is in seconds
  return atof( value );
}


//------------------------------------------------------------------------
// Method:        XWobble
// Description:   return the icount-1 xwobble value in asec
// Input:         none, global settings
// Output:        .
// Return value:  9e9, no number or error
//               
//               
// Side effects: 
//------------------------------------------------------------------------
double VexPlus::XWobble(const int& icount) const {

  void* ptr;  
  int   islink, isname;
  char *value, *units;

  if( !(ptr = get_global_lowl(T_X_WOBBLE,B_EOP,vexp)) ) 
    return 9e9;
  if( (vex_field(T_X_WOBBLE, ptr, icount+1,
		 &islink,&isname,&value,&units)) ) 
    return 9e9;
  // [[SHORTCUT]] Assuming wobble is in asec
  return atof( value );
}

//------------------------------------------------------------------------
// Method:        YWobble
// Description:   return the icount-1 ywobble value in asec
// Input:         none, global settings
// Output:        .
// Return value:  9e9, no number or error
//               
//               
// Side effects: 
//------------------------------------------------------------------------
double VexPlus::YWobble(const int& icount) const {

  void* ptr;  
  int   islink, isname;
  char *value, *units;

  if( !(ptr = get_global_lowl(T_Y_WOBBLE,B_EOP,vexp)) ) 
    return 9e9;
  if( (vex_field(T_Y_WOBBLE, ptr, icount+1,
		 &islink,&isname,&value,&units)) ) 
    return 9e9;
  // [[SHORTCUT]] Assuming wobble is in asec
  return atof( value );
}

//------------------------------------------------------------------------
// Method:        DeltaPsi
// Description:   return the icount-1 delta_psi value in asec
// Input:         none, global settings
// Output:        .
// Return value:  0 if absent or error
//               
//               
// Side effects: 
//------------------------------------------------------------------------
double VexPlus::DeltaPsi(const int& icount) const {

  void* ptr;  
  int   islink, isname;
  char *value, *units;

  if( !(ptr = get_global_lowl(T_DELTA_PSI,B_EOP,vexp)) ) 
    return 0.0;
  if( (vex_field(T_DELTA_PSI, ptr, icount+1,
		 &islink,&isname,&value,&units)) ) 
    return 0.0;
  // [[SHORTCUT]] Assuming delta_psi is in asec
  return atof( value );
}

//------------------------------------------------------------------------
// Method:        DeltaEps
// Description:   return the icount-1 delta_eps value in asec
// Input:         none, global settings
// Output:        .
// Return value:  0 if absent or error
//               
//               
// Side effects: 
//------------------------------------------------------------------------
double VexPlus::DeltaEps(const int& icount) const {

  void* ptr;  
  int   islink, isname;
  char *value, *units;

  if( !(ptr = get_global_lowl(T_DELTA_EPS,B_EOP,vexp)) ) 
    return 0.0;
  if( (vex_field(T_DELTA_EPS, ptr, icount+1,
		 &islink,&isname,&value,&units)) ) 
    return 0.0;
  // [[SHORTCUT]] Assuming delta_eps is in asec
  return atof( value );
}

//________________$EXPER_________________________________________________
//
//------------------------------------------------------------------------
// Method:        ExperNo
// Description:   return the experiment number
// Input:         station, mode of freq, icount chan_def -1 to return
// Output:        .
// Return value:  0, no number or error
//               
//               
// Side effects: 
//------------------------------------------------------------------------
int VexPlus::ExperNo() const {

  void* ptr;  
  int   islink, isname;
  char *value, *units;

  if( !(ptr = get_global_lowl(T_EXPER_NUM,B_EXPER,vexp)) ) 
    return 0;
  if( (vex_field(T_EXPER_NUM, ptr, 1,
		 &islink,&isname,&value,&units)) ) 
    return 0;
  
  return atoi( value );
}
//------------------------------------------------------------------------
// Method:        ExperName
// Description:   return the experiment number
// Input:         station, mode of freq, icount chan_def -1 to return
// Output:        .
// Return value:  null-string, no name
//               -1, error??  
//               
// Side effects: 
//------------------------------------------------------------------------
string VexPlus::ExperName() const {

  void* ptr;  
  int   islink, isname;
  char *value, *units;

  if( !(ptr = get_global_lowl(T_EXPER_NAME,B_EXPER,vexp)) ) 
    return string("");
  if( (vex_field(T_EXPER_NAME, ptr, 1,
		 &islink,&isname,&value,&units)) ) 
    return string("");
  
  return string(value);
}

//________________$FREQ__________________________________________________
//
//------------------------------------------------------------------------
// Method:        N_FreqChans
// Description:   Number of channels in a freq ref
// Input:         station & mode of freq
// Output:        
// Return value:  0, no channels or error
//                > 0 number of channels
//               
// Side effects: 
//------------------------------------------------------------------------
int VexPlus::N_FreqChans( const string& stat,
			  const string& mode ) const {
 int icount = 0;
 char stat_c[128], mode_c[128];
 stat.copy( stat_c, string::npos );
 stat_c[stat.length()] = 0;
 mode.copy( mode_c, string::npos );
 mode_c[mode.length()] = 0;
 
 if( (get_all_lowl(stat_c,mode_c,T_CHAN_DEF,B_FREQ,vexp)) ) {
   do {
     icount++;
   } while ( (get_all_lowl_next()) );
 }
 return icount;
}

//------------------------------------------------------------------------
// Method:        SkyFreq
// Description:   return the total LO value (band edge) in MHz
// Input:         station & mode of freq, icount chan_def -1 to return
// Output:        .
// Return value:  0, failed
//                freq in MHz
//               
// Side effects: 
//------------------------------------------------------------------------
double VexPlus::SkyFreq( const string& stat, const string& mode, 
			 const int& icount ) const {

  char *value, *units;
  int islink, isname;
  void* ptr;
  
  char stat_c[128], mode_c[128];
  stat.copy( stat_c, string::npos );
  stat_c[stat.length()] = 0;
  mode.copy( mode_c, string::npos );
  mode_c[mode.length()] = 0;
  
  if( !(ptr = get_all_lowl(stat_c,mode_c,T_CHAN_DEF,B_FREQ,vexp)) ) 
    return 0;
  if( (vex_field(T_CHAN_DEF, ptr, 2,
		 &islink,&isname,&value,&units)) ) return 0;

  if( icount ) {
    for( int i = 0; i < icount; i++ ) {
      if( !(ptr = get_all_lowl_next()) ) return 0;
    }
    if( (vex_field(T_CHAN_DEF, ptr, 2, 
		   &islink,&isname,&value,&units)) ) 
      return 0;

  }
  return freqinMHz( string(value), string(units) );

}

//------------------------------------------------------------------------
// Method:        SampleRate
// Description:   return the total LO value (band edge) in MHz
// Input:         station & mode of freq, icount chan_def -1 to return
// Output:        .
// Return value:  0, failed
//                freq in MHz
//               
// Side effects: 
//------------------------------------------------------------------------
double VexPlus::SampleRate( const string& stat, const string& mode ) const {

  char *value, *units;
  int islink, isname;
  void* ptr;
  
  char stat_c[128], mode_c[128];
  stat.copy( stat_c, string::npos );
  stat_c[stat.length()] = 0;
  mode.copy( mode_c, string::npos );
  mode_c[mode.length()] = 0;
  
  if( !(ptr = get_all_lowl(stat_c,mode_c,T_SAMPLE_RATE,B_FREQ,vexp)) ) 
    return 0;
  if( (vex_field(T_SAMPLE_RATE, ptr, 1,
		 &islink,&isname,&value,&units)) ) return 0;

  return atof( value );

  // [[SIMPLIFICATION]] sample rate is in Ms/s

}


//------------------------------------------------------------------------
// Method:        BW
// Description:   return the filter width in Hz
// Input:         station & mode of freq, icount chan_def -1 to return
// Output:        .
// Return value:  0, failed
//                bandwidth in Hz
//               
// Side effects: 
//------------------------------------------------------------------------
double VexPlus::BW( const string& stat, const string& mode, 
		    const int& icount ) const {

  char *value, *units;
  int islink, isname;
  void* ptr;
  
  char stat_c[128], mode_c[128];
  stat.copy( stat_c, string::npos );
  stat_c[stat.length()] = 0;
  mode.copy( mode_c, string::npos );
  mode_c[mode.length()] = 0;
  
  if( !(ptr = get_all_lowl(stat_c,mode_c,T_CHAN_DEF,B_FREQ,vexp)) ) 
    return 0;
  if( (vex_field(T_CHAN_DEF, ptr, 4,
		 &islink,&isname,&value,&units)) ) return 0;

  if( icount ) {
    for( int i = 0; i < icount; i++ ) {
      if( !(ptr = get_all_lowl_next()) ) return 0;
    }
    if( (vex_field(T_CHAN_DEF, ptr, 4, 
		   &islink,&isname,&value,&units)) ) 
      return 0;

  }
  return freqinHz( value, units );

}

//------------------------------------------------------------------------
// Method:        SideBand
// Description:   return the net sideband as a string.
// Input:         station & mode of freq, icount chan_def -1 to return
// Output:        .
// Return value:  null-string, failed
//                Net sideband
//               
// Side effects: 
//------------------------------------------------------------------------
string VexPlus::SideBand( const string& stat, const string& mode, 
			  const int& icount ) const {

  char *value, *units;
  int islink, isname;
  void* ptr;
  
  char stat_c[128], mode_c[128];
  stat.copy( stat_c, string::npos );
  stat_c[stat.length()] = 0;
  mode.copy( mode_c, string::npos );
  mode_c[mode.length()] = 0;

  if( !(ptr = get_all_lowl(stat_c,mode_c,T_CHAN_DEF,B_FREQ,vexp)) ) 
    return string("");
  if( (vex_field(T_CHAN_DEF, ptr, 3,
		 &islink,&isname,&value,&units)) ) return string("");

  if( icount ) {
    for( int i = 0; i < icount; i++ ) {
      if( !(ptr = get_all_lowl_next()) ) return string("");
    }
    if( (vex_field(T_CHAN_DEF, ptr, 3, 
		   &islink,&isname,&value,&units)) ) 
      return string("");

  }
  return string(value);

}

//------------------------------------------------------------------------
// Method:        Link_freq_track
// Description:   return the linkname that links a chan_def to a track
// Input:         station, mode of freq, icount chan_def -1 to return
// Output:        .
// Return value:  null-string, unresolved or not a link
//               
//               
// Side effects: 
//------------------------------------------------------------------------
string VexPlus::Link_freq_track( const string& stat, const string& mode, 
				 const int& icount ) const {
  int islink, isname;
  char* link, *units;
  void* ptr;

  char stat_c[128], mode_c[128];
  stat.copy( stat_c, string::npos );
  stat_c[stat.length()] = 0;
  mode.copy( mode_c, string::npos );
  mode_c[mode.length()] = 0;

  if( !(ptr = get_all_lowl(stat_c,mode_c,T_CHAN_DEF,B_FREQ,vexp)) ) 
    return string("");
  if( (vex_field(T_CHAN_DEF, ptr, 5, &islink,&isname,&link,&units)) ) 
    return string("");
  if( !(islink || isname) ) 
    return string("");

  if( icount ) {
    for( int i = 0; i < icount; i++ ) {
      if( !(ptr = get_all_lowl_next()) )
	return string("");
    }
    if( (vex_field(T_CHAN_DEF, ptr, 5, &islink,&isname,&link,&units)) ) 
      return string("");
  }

  return string(link);

}

//------------------------------------------------------------------------
// Method:        Link_freq_bbc
// Description:   return the linkname that links a chan_def to a bbc
// Input:         station, mode of freq, icount chan_def -1 to return
// Output:        .
// Return value:  null-string, unresolved or not a link
//               
//               
// Side effects: 
//------------------------------------------------------------------------
string VexPlus::Link_freq_bbc( const string& stat, const string& mode, 
			       const int& icount ) const {
  int islink, isname;
  char* link, *units;
  void* ptr;

  char stat_c[128], mode_c[128];
  stat.copy( stat_c, string::npos );
  stat_c[stat.length()] = 0;
  mode.copy( mode_c, string::npos );
  mode_c[mode.length()] = 0;

  if( !(ptr = get_all_lowl(stat_c,mode_c,T_CHAN_DEF,B_FREQ,vexp)) ) 
    return string("");
  if( (vex_field(T_CHAN_DEF, ptr, 6,
		       &islink,&isname,&link,&units)) ) 
    return string("");
  if( !(islink || isname) ) return string("");

  if( icount ) {
    for( int i = 0; i < icount; i++ ) {
      if( !(ptr = get_all_lowl_next()) ) return string("");
    }
    if( (vex_field(T_CHAN_DEF, ptr, 6,
		   &islink,&isname,&link,&units)) ) 
      return string("");

  }
  
  return string(link);

}

//________________$HEAD_POS_____________________________________________
//
//------------------------------------------------------------------------
// Method:        N_HeadPositions
// Description:   N of HeadPositions
// Input:         station & mode of HeadPosition
// Output:        
// Return value:  0, no HeadPositions
//                # HeadPositions
//               
// Side effects: 
//------------------------------------------------------------------------
int VexPlus::N_HeadPositions( const string& stat,
		     const string& mode ) const {

  char stat_c[128], mode_c[128];
  stat.copy( stat_c, string::npos );
  stat_c[stat.length()] = 0;
  mode.copy( mode_c, string::npos );
  mode_c[mode.length()] = 0;

  int icount = 0;

  if( (get_all_lowl(stat_c,mode_c,T_HEADSTACK_POS,B_HEAD_POS,vexp)) ) {
    do {
      icount++;
    } while( (get_all_lowl_next()) );
  }
  return icount;
}

//------------------------------------------------------------------------
// Method:        HeadIndex
// Description:   HeadPosition Index number icount-1 for station and mode
// Input:         station & mode of HeadPosition
// Output:        
// Return value:  0, fails
//                HeadPosition Index
//               
// Side effects: 
//------------------------------------------------------------------------
int VexPlus::HeadIndex( const string& stat, const string& mode,
			int const &icount ) const {

  int islink, isname;
  char* value, *units;

  char stat_c[128], mode_c[128];
  stat.copy( stat_c, string::npos );
  stat_c[stat.length()] = 0;
  mode.copy( mode_c, string::npos );
  mode_c[mode.length()] = 0;
  void* ptr;

  if( !(ptr = get_all_lowl(stat_c,mode_c,T_HEADSTACK_POS,B_HEAD_POS,vexp)) )
    return 0;
  if( (vex_field(T_HEADSTACK_POS, ptr, 1, 
		       &islink,&isname,&value,&units)) ) return 0;
  if( icount ) {
    for( int i = 0; i < icount; i++ ) {
      if( !(ptr = get_all_lowl_next()) ) return 0;
    }
    if( (vex_field(T_HEADSTACK_POS, ptr, 1, 
		   &islink,&isname,&value,&units)) ) 
      return 0;
  }
  return atoi(value);
}

//------------------------------------------------------------------------
// Method:        HeadPosition
// Description:   HeadPosition name icount-1 for station and mode
// Input:         station & mode of HeadPosition Order
// Output:        
// Return value:  0, fails
//                HeadPosition name
//               
// Side effects: 
//------------------------------------------------------------------------
double VexPlus::HeadPosition( const string& stat, const string& mode,
			      int const &icount ) const {

  int islink, isname;
  char* value, *units;

  char stat_c[128], mode_c[128];
  stat.copy( stat_c, string::npos );
  stat_c[stat.length()] = 0;
  mode.copy( mode_c, string::npos );
  mode_c[mode.length()] = 0;
  void* ptr;

  if( !(ptr = get_all_lowl(stat_c,mode_c,T_HEADSTACK_POS,B_HEAD_POS,vexp)) )
    return 0;
  if( (vex_field(T_HEADSTACK_POS, ptr, 2, 
		       &islink,&isname,&value,&units)) ) return 0;
  if( icount ) {
    for( int i = 0; i < icount; i++ ) {
      if( !(ptr = get_all_lowl_next()) ) return 0;
    }
    if( (vex_field(T_HEADSTACK_POS, ptr, 2, 
		   &islink,&isname,&value,&units)) ) 
      return 0;
  }
  // [[SHORTCUT]] Assuming offset is in microns
  return atof(value);

}

//------------------------------------------------------------------------
// Method:        HeadPosition
// Description:   HeadPosition name icount-1 for station and mode
// Input:         station, mode and headstack number (1-4) of HeadPosition Order
// Output:        
// Return value:  0, fails
//                HeadPosition name
//               
// Side effects: 
//------------------------------------------------------------------------
double VexPlus::HeadPosition( const string& stat, const string& mode, int hs,
			      int const &icount ) const {

  int islink, isname;
  char* value, *units;

  char stat_c[128], mode_c[128];
  stat.copy( stat_c, string::npos );
  stat_c[stat.length()] = 0;
  mode.copy( mode_c, string::npos );
  mode_c[mode.length()] = 0;
  void* ptr;

  if( !(ptr = get_all_lowl(stat_c,mode_c,T_HEADSTACK_POS,B_HEAD_POS,vexp)) )
    return 0;
  if( (vex_field(T_HEADSTACK_POS, ptr, 1+hs, 
		       &islink,&isname,&value,&units)) ) return 0;
  if( icount ) {
    for( int i = 0; i < icount; i++ ) {
      if( !(ptr = get_all_lowl_next()) ) return 0;
    }
    if( (vex_field(T_HEADSTACK_POS, ptr, 1+hs, 
		   &islink,&isname,&value,&units)) ) 
      return 0;
  }
  // [[SHORTCUT]] Assuming offset is in microns
  return atof(value);
}

//________________$IF__________________________________________________
//
//------------------------------------------------------------------------
// Method:        N_Ifs
// Description:   Number of channels in a if ref
// Input:         station & mode of if
// Output:        
// Return value:  0, no channels or error
//                > 0 number of IFs
//               
// Side effects: 
//------------------------------------------------------------------------
int VexPlus::N_IFs( const string& stat, const string& mode ) const {
  int icount = 0;
  char stat_c[128], mode_c[128];
  stat.copy( stat_c, string::npos );
  stat_c[stat.length()] = 0;
  mode.copy( mode_c, string::npos );
  mode_c[mode.length()] = 0;
  
  if( (get_all_lowl(stat_c,mode_c,T_IF_DEF,B_IF,vexp)) ) {
    do {
      icount++;
    } while ( (get_all_lowl_next()) );
  }
  return icount;
}

//------------------------------------------------------------------------
// Method:        Resolve_if_if
// Description:   return the linkname set a if link to a if
// Input:         station & mode of if, icount if_def -1 to return
// Output:        .
// Return value:  null-string, unresolved or not a link
//               
//               
// Side effects: 
//------------------------------------------------------------------------
string VexPlus::Resolve_if_bbc( const string& stat, const string& mode, 
				const int& icount ) const {
  int islink, isname;
  char* name, *units;
  void* ptr;

  char stat_c[128], mode_c[128];
  stat.copy( stat_c, string::npos );
  stat_c[stat.length()] = 0;
  mode.copy( mode_c, string::npos );
  mode_c[mode.length()] = 0;
  
  if( !(ptr = get_all_lowl(stat_c,mode_c,T_IF_DEF,B_IF,vexp)) ) 
    return string("");
  if( (vex_field(T_IF_DEF, ptr, 1,
		       &islink,&isname,&name,&units)) ) 
    return string("");
  if( !(islink || isname) ) return string("");

  if( icount ) {
    for( int i = 0; i < icount; i++ ) {
      if( !(ptr = get_all_lowl_next()) ) return string("");
    }
    if( (vex_field(T_IF_DEF, ptr, 1,
		   &islink,&isname,&name,&units)) ) 
      return string("");
  }

  return string(name);
}

//------------------------------------------------------------------------
// Method:        Pol
// Description:   return the Pol in IF block
// Input:         station & mode of pol, icount if_def -1 to return
// Output:        .
// Return value:  null-string, failed
//                L or R
//               
// Side effects: 
//------------------------------------------------------------------------
string VexPlus::Pol( const string& stat, const string& mode, 
		     const int& icount ) const {

  char *value, *units;
  int islink, isname;
  void* ptr;
  
  char stat_c[128], mode_c[128];
  stat.copy( stat_c, string::npos );
  stat_c[stat.length()] = 0;
  mode.copy( mode_c, string::npos );
  mode_c[mode.length()] = 0;
  
  if( !(ptr = get_all_lowl(stat_c,mode_c,T_IF_DEF,B_IF,vexp)) ) 
    return string("");
  if( (vex_field(T_IF_DEF, ptr, 3,
		 &islink,&isname,&value,&units)) ) return string("");

  if( icount ) {
    for( int i = 0; i < icount; i++ ) {
      if( !(ptr = get_all_lowl_next()) ) return string("");
    }
    if( (vex_field(T_IF_DEF, ptr, 3, 
		   &islink,&isname,&value,&units)) ) 
      return string("");

  }
  return string(value);

}

//------------------------------------------------------------------------
// Method:        LO
// Description:   return local oscilator frequency
// Input:         station & mode of if, icount if_def -1 to return
// Output:        .
// Return value:  null-string, unresolved or not a link
//               
//               
// Side effects: 
//------------------------------------------------------------------------
double VexPlus::LO(const std::string& stat,
				  const std::string& mode,
				  const int& icount) const
{
  char *value, *units;
  int islink, isname;
  void* ptr;
  
  char stat_c[128], mode_c[128];
  stat.copy( stat_c, string::npos );
  stat_c[stat.length()] = 0;
  mode.copy( mode_c, string::npos );
  mode_c[mode.length()] = 0;
  
  if( !(ptr = get_all_lowl(stat_c,mode_c,T_IF_DEF,B_IF,vexp)) ) 
    return 0;
  if( (vex_field(T_IF_DEF, ptr, 4,
		 &islink,&isname,&value,&units)) ) return 0;
  
  if( icount ) {
    for( int i = 0; i < icount; i++ ) {
      if( !(ptr = get_all_lowl_next()) ) return 0;
    }
    if( (vex_field(T_IF_DEF, ptr, 4, 
		   &islink,&isname,&value,&units)) ) 
      return 0;
  }
  return freqinHz( string(value), string(units) );
}

//------------------------------------------------------------------------
// Method:        Pol_by_Freq
// Description:   return the pol, given the iband in a FREQ def
//                A higher level routine, resolving links
// Input:         station & mode of pol, icount chan_def -1 to return
// Output:        .
// Return value:  null-string, failed
//                L or R
//               
// Side effects: 
//------------------------------------------------------------------------
string VexPlus::Pol_by_Freq(const string& stat, const string& mode, 
			    const int& icount ) const {
  
  // Find link name to bbc in freq
  string link1(Link_freq_bbc(stat, mode, icount));
  if( link1.length() == 0 ) return string("");

  // Loop over all BBC lines in appropriate def
  for( int ibbc = 0; ibbc < N_BBCs(stat, mode); ibbc++ ) {

    // Get the link name back
    string resol1(Resolve_bbc_freq(stat, mode, ibbc));
    if( resol1.length() == 0 ) return string("");

    // And if they are the same
    if( link1 == resol1 ) {
  
      // Find the link name of the IF
      string link2(Link_bbc_if(stat, mode, ibbc));
      if( link2.length() == 0 ) return string("");

      // And loop these
      for( int iif = 0; iif < N_IFs(stat, mode); iif++ ) {

	// Should be a link
	string resol2(Resolve_if_bbc(stat, mode, iif));
	if( resol2.length() == 0 ) return string("");

	// Have we found it?
	if( link2 ==  resol2 ) {
  
	  // Return the Pol
	  return Pol( stat, mode, iif );
	  
	}
      }
    }
  }
  return string("");

}

//------------------------------------------------------------------------
// Method:        PCalFreqInterval
// Description:   return the Phase-cal frequency interval in IF block
// Input:         station & mode of freq, icount if_def -1 to return
// Output:        .
// Return value:  0, failed or no phase-cal present
//                freq interval in Hz
//               
// Side effects: 
//------------------------------------------------------------------------
double VexPlus::PCalFreqInterval (const std::string& stat,
				  const std::string& mode,
				  const int& icount) const
{
  char *value, *units;
  int islink, isname;
  void* ptr;
  
  char stat_c[128], mode_c[128];
  stat.copy( stat_c, string::npos );
  stat_c[stat.length()] = 0;
  mode.copy( mode_c, string::npos );
  mode_c[mode.length()] = 0;
  
  if( !(ptr = get_all_lowl(stat_c,mode_c,T_IF_DEF,B_IF,vexp)) ) 
    return 0;
  if( (vex_field(T_IF_DEF, ptr, 6,
		 &islink,&isname,&value,&units)) ) return 0;
  
  if( icount ) {
    for( int i = 0; i < icount; i++ ) {
      if( !(ptr = get_all_lowl_next()) ) return 0;
    }
    if( (vex_field(T_IF_DEF, ptr, 6, 
		   &islink,&isname,&value,&units)) ) 
      return 0;
  }
  return freqinHz( string(value), string(units) );
}

//------------------------------------------------------------------------
// Method:        PCalFreqInterval_by_Freq
// Description:   return the Phase-cal frequency interval, given the iband in a FREQ def
//                A higher level routine, resolving links
// Input:         station & mode of PCAL tones, icount chan_def -1 to return
// Output:        .
// Return value:  0, failed or no phase-cal present
//                freq interval in Hz
//               
// Side effects: 
//------------------------------------------------------------------------
double VexPlus::PCalFreqInterval_by_Freq(const string& stat,
					 const string& mode, 
					 const int& icount ) const {

  // Find link name to bbc in freq
  string link1(Link_freq_bbc(stat, mode, icount));
  if( link1.length() == 0 ) return 0;
  
  // Loop over all BBC lines in appropriate def
  for( int ibbc = 0; ibbc < N_BBCs(stat, mode); ibbc++ ) {
    
    // Get the link name back
    string resol1(Resolve_bbc_freq(stat, mode, ibbc));
    if( resol1.length() == 0 ) return 0;
    
    // And if they are the same
    if( link1 == resol1 ) {
      
      // Find the link name of the IF
      string link2(Link_bbc_if(stat, mode, ibbc));
      if( link2.length() == 0 ) return 0;
      
      // And loop these
      for( int iif = 0; iif < N_IFs(stat, mode); iif++ ) {
	
	// Should be a link
	string resol2(Resolve_if_bbc(stat, mode, iif));
	if( resol2.length() == 0 ) return 0;
	
	// Have we found it?
	if( link2 ==  resol2 ) {
	  
	  // Return the PCalFreqInterval
	  return PCalFreqInterval( stat, mode, iif );
	  
	}
      }
    }
  }
  return 0;

}

//------------------------------------------------------------------------
// Method:        PCalFreqBase
// Description:   return the Phase-cal base frequency in IF block
// Input:         station & mode of freq, icount if_def -1 to return
// Output:        .
// Return value:  0, failed
//                freq in Hz
//               
// Side effects: 
//------------------------------------------------------------------------
double VexPlus::PCalFreqBase (const std::string& stat,
			      const std::string& mode,
			      const int& icount) const
{
  char *value, *units;
  int islink, isname;
  void* ptr;
  
  char stat_c[128], mode_c[128];
  stat.copy( stat_c, string::npos );
  stat_c[stat.length()] = 0;
  mode.copy( mode_c, string::npos );
  mode_c[mode.length()] = 0;
  
  if( !(ptr = get_all_lowl(stat_c,mode_c,T_IF_DEF,B_IF,vexp)) ) 
    return 0;
  if( (vex_field(T_IF_DEF, ptr, 7,
		 &islink,&isname,&value,&units)) ) return 0;
  
  if( icount ) {
    for( int i = 0; i < icount; i++ ) {
      if( !(ptr = get_all_lowl_next()) ) return 0;
    }
    if( (vex_field(T_IF_DEF, ptr, 7, 
		   &islink,&isname,&value,&units)) ) 
      return 0;
  }
  return freqinHz( string(value), string(units) );
}

//------------------------------------------------------------------------
// Method:        PCalFreqBase_by_Freq
// Description:   return the Phase-cal base frequency, given the iband in a FREQ def
//                A higher level routine, resolving links
// Input:         station & mode of PCAL tones, icount chan_def -1 to return
// Output:        .
// Return value:  0, failed
//                freq in Hz
//               
// Side effects: 
//------------------------------------------------------------------------
double VexPlus::PCalFreqBase_by_Freq(const string& stat,
				     const string& mode, 
				     const int& icount ) const {

  // Find link name to bbc in freq
  string link1(Link_freq_bbc(stat, mode, icount));
  if( link1.length() == 0 ) return 0;
  
  // Loop over all BBC lines in appropriate def
  for( int ibbc = 0; ibbc < N_BBCs(stat, mode); ibbc++ ) {
    
    // Get the link name back
    string resol1(Resolve_bbc_freq(stat, mode, ibbc));
    if( resol1.length() == 0 ) return 0;
    
    // And if they are the same
    if( link1 == resol1 ) {
      
      // Find the link name of the IF
      string link2(Link_bbc_if(stat, mode, ibbc));
      if( link2.length() == 0 ) return 0;
      
      // And loop these
      for( int iif = 0; iif < N_IFs(stat, mode); iif++ ) {
	
	// Should be a link
	string resol2(Resolve_if_bbc(stat, mode, iif));
	if( resol2.length() == 0 ) return 0;
	
	// Have we found it?
	if( link2 ==  resol2 ) {
	  
	  // Return the PCalFreqBase
	  return PCalFreqBase( stat, mode, iif );
	  
	}
      }
    }
  }
  return 0;

}

//________________$ROLL__________________________________________________
//
//------------------------------------------------------------------------
// Method:        Roll
// Description:   0 for no roll (undefined else)
// Input:         station & mode of Roll def
// Output:        
// Return value:  0, no bits roll
//                > Some kind of rolling
//               
// Side effects: [[SIMPLIFICATION]] Fails for rolling
//------------------------------------------------------------------------
int VexPlus::Roll( const string& stat, const string& mode ) const {

  int islink, isname;
  char* value, *units;

  char stat_c[128], mode_c[128];
  stat.copy( stat_c, string::npos );
  stat_c[stat.length()] = 0;
  mode.copy( mode_c, string::npos );
  mode_c[mode.length()] = 0;
  void* ptr;
  
  // First see whether there is a roll statement
  if( (ptr = get_all_lowl(stat_c,mode_c,T_ROLL,B_ROLL,vexp)) ) {
    vex_field(T_ROLL, ptr, 1, 
	      &islink,&isname,&value,&units);
    if( !(strcmp(value,"off")) ) return 0;
    else return 99;
  }
  // else if there is any roll def
  if( (ptr = get_all_lowl(stat_c,mode_c,T_ROLL_DEF,B_ROLL,vexp)) )
    return 99;
  
  return 0;
}

//________________$MODE__________________________________________________
//
// Method:        N_Modes
// Description:   Number of modes in VEX file
// Input:         
// Output:        
// Return value:  0, no modes, default
//                > 0 number of modes
//               
// Side effects: 
//------------------------------------------------------------------------
int VexPlus::N_Modes( ) const {

  int icount = 0;

  char* ptr = 0;
  if( (ptr = get_mode_def( vexp )) ) {
    do {
      icount++;
    } while ( (ptr = get_mode_def_next()) );
  }
  return icount;
}

//------------------------------------------------------------------------
// Method:        Mode
// Description:   Return the icount+1 mode in the list
// Input:         icount the mode number -1 in the list 
// Output:        .
// Return value:  null-string, error
//                name of mode
//               
// Side effects: 
//------------------------------------------------------------------------
string VexPlus::Mode( const int& icount ) const { 

  char* mode;
  if( !(mode = get_mode_def( vexp )) ) return string("");
  if( icount ) 
    for( int i = 0; i < icount; i++ ) mode = get_mode_def_next();
      
  return string(mode);
}

//________________$PASS_ORDER_____________________________________________
//
//------------------------------------------------------------------------
// Method:        N_Passes
// Description:   N of passes
// Input:         station & mode of Pass Order
// Output:        
// Return value:  0, no passes
//                # passes
//               
// Side effects: 
//------------------------------------------------------------------------
int VexPlus::N_Passes( const string& stat,
		       const string& mode ) const {

  int islink, isname;
  char* value, *units;

  char stat_c[128], mode_c[128];
  stat.copy( stat_c, string::npos );
  stat_c[stat.length()] = 0;
  mode.copy( mode_c, string::npos );
  mode_c[mode.length()] = 0;
  void* ptr;

  int icount = 0;

  if( !(ptr = get_all_lowl(stat_c,mode_c,T_PASS_ORDER,B_PASS_ORDER,vexp)) )
    return 0;
  while( !(vex_field(T_PASS_ORDER, ptr, icount+1, 
		       &islink,&isname,&value,&units)) ) {
    icount++;
  }
  
  return icount;
}

//------------------------------------------------------------------------
// Method:        Pass
// Description:   pass name icount-1 for station and mode
// Input:         station & mode of Pass Order
// Output:        
// Return value:  null-string, fails
//                pass name
//               
// Side effects: 
//------------------------------------------------------------------------
string VexPlus::Pass( const string& stat, const string& mode,
		      int const &icount ) const {

  int islink, isname;
  char* value, *units;

  char stat_c[128], mode_c[128];
  stat.copy( stat_c, string::npos );
  stat_c[stat.length()] = 0;
  mode.copy( mode_c, string::npos );
  mode_c[mode.length()] = 0;
  void* ptr;

  if( !(ptr = get_all_lowl(stat_c,mode_c,T_PASS_ORDER,B_PASS_ORDER,vexp)) )
    return string("");

  if( (vex_field(T_PASS_ORDER, ptr, icount+1, 
		       &islink,&isname,&value,&units)) ) return string("");
  return string(value);
}


//________________$SITE___________________________________________________
//
//------------------------------------------------------------------------
// Method:        Site
// Description:   return the site name for a station
// Input:         station & mode of freq, icount chan_def -1 to return
// Output:        .
// Return value:  null-string, failed
//                site name
//               
// Side effects:  Is assuming 1 site / station!
//------------------------------------------------------------------------
string VexPlus::Site( const string& stat ) const {

  char *value, *units;
  int islink, isname;
  void* ptr;
  
  char stat_c[128];
  stat.copy( stat_c, string::npos );
  stat_c[stat.length()] = 0;

  if( !(ptr = get_station_lowl(stat_c,T_SITE_NAME,B_SITE,vexp)) ) 
    return string("");
  if( (vex_field(T_SITE_NAME, ptr, 1,
		 &islink,&isname,&value,&units)) ) return string("");

  //if there is more than one, this is still too simple
  if( (get_station_lowl_next( )) ) return string("");

  return string(value);
}

//------------------------------------------------------------------------
// Method:        TwoLetter
// Description:   return the two letter code = site ID for a station
// Input:         station & mode of freq, icount chan_def -1 to return
// Output:        .
// Return value:  null-string, failed
//                siteID
//               
// Side effects:  Is assuming 1 site / station!
//------------------------------------------------------------------------
string VexPlus::TwoLetter( const string& stat ) const {

  char *value, *units;
  int islink, isname;
  void* ptr;
  
  char stat_c[128];
  stat.copy( stat_c, string::npos );
  stat_c[stat.length()] = 0;

  if( !(ptr = get_station_lowl(stat_c,T_SITE_ID,B_SITE,vexp)) ) 
    return string("");
  if( (vex_field(T_SITE_ID, ptr, 1,
		 &islink,&isname,&value,&units)) ) return string("");

  //if there is more than one, this is still too simple
  if( (get_station_lowl_next( )) ) return string("");

  return string(value);
}

//------------------------------------------------------------------------
// Method:        SiteX
// Description:   return the site X position for a station
// Input:         station & mode of freq, icount chan_def -1 to return
// Output:        .
// Return value:  0, failed
//                X position
//               
// Side effects:  Is assuming 1 site / station!
//------------------------------------------------------------------------
double VexPlus::SiteX( const string& stat ) const {

  char *value, *units;
  int islink, isname;
  void* ptr;
  
  char stat_c[128];
  stat.copy( stat_c, string::npos );
  stat_c[stat.length()] = 0;

  if( !(ptr = get_station_lowl(stat_c,T_SITE_POSITION,B_SITE,vexp)) ) 
    return 0;
  if( (vex_field(T_SITE_POSITION, ptr, 1,
		 &islink,&isname,&value,&units)) ) return 0;

  //if there is more than one, this is still too simple
  if( (get_station_lowl_next( )) ) return 0;

  return atof( value );
}


//------------------------------------------------------------------------
// Method:        SiteY
// Description:   return the site Y position for a station
// Input:         station & mode of freq, icount chan_def -1 to return
// Output:        .
// Return value:  0, failed
//                Y position
//               
// Side effects:  Is assuming 1 site / station!
//------------------------------------------------------------------------
double VexPlus::SiteY( const string& stat ) const {

  char *value, *units;
  int islink, isname;
  void* ptr;
  
  char stat_c[128];
  stat.copy( stat_c, string::npos );
  stat_c[stat.length()] = 0;

  if( !(ptr = get_station_lowl(stat_c,T_SITE_POSITION,B_SITE,vexp)) ) 
    return 0;
  if( (vex_field(T_SITE_POSITION, ptr, 2,
		 &islink,&isname,&value,&units)) ) return 0;

  //if there is more than one, this is still too simple
  if( (get_station_lowl_next( )) ) return 0;

  return atof( value );
}


//------------------------------------------------------------------------
// Method:        SiteZ
// Description:   return the site Z position for a station
// Input:         station & mode of freq, icount chan_def -1 to return
// Output:        .
// Return value:  0, failed
//                Z position
//               
// Side effects:  Is assuming 1 site / station!
//------------------------------------------------------------------------
double VexPlus::SiteZ( const string& stat ) const {

  char *value, *units;
  int islink, isname;
  void* ptr;
  
  char stat_c[128];
  stat.copy( stat_c, string::npos );
  stat_c[stat.length()] = 0;

  if( !(ptr = get_station_lowl(stat_c,T_SITE_POSITION,B_SITE,vexp)) ) 
    return 0;
  if( (vex_field(T_SITE_POSITION, ptr, 3,
		 &islink,&isname,&value,&units)) ) return 0;

  //if there is more than one, this is still too simple
  if( (get_station_lowl_next( )) ) return 0;

  return atof( value );
}



//________________$SOURCE_______________________________________________
//
//------------------------------------------------------------------------
// Method:        N_Sources
// Description:   Total number of sources in VEX
// Input:         
// Output:        
// Return value:  0, No sources = default
//                > 0 number of sources
//               
// Side effects: 
//------------------------------------------------------------------------
int VexPlus::N_Sources( ) const {

  int icount = 0;

  char * ptr = 0;
  if( (ptr = get_source_def( vexp )) ) {
    do {
      icount++;
    } while ( (ptr = get_source_def_next()) );
  }
  return icount;
}

//------------------------------------------------------------------------
// Method:        SourceName
// Description:   Return the icount+1 source in the list
// Input:         icount the source number -1 in the list 
// Output:        .
// Return value:  null-string, error
//                name of source
//               
// Side effects: 
//------------------------------------------------------------------------
string VexPlus::SourceName( const int& icount ) const { 

  void* ptr;
  char* source;
  int islink, isname;
  char* value, *units;

  if( !(source = get_source_def( vexp )) ) return string("");
  if( icount ) 
    for( int i = 0; i < icount; i++ ) source = get_source_def_next();

  if( !(source) ) return string("");
  if( !(ptr = get_source_lowl( source, T_SOURCE_NAME, vexp)) ) return string("");
  if( (vex_field(T_SOURCE_NAME, ptr, 1, 
		 &islink,&isname,&value,&units)) ) return string("");
  
  return string(value);
}

//------------------------------------------------------------------------
// Method:        RefEpoch
// Description:   Return the icount+1 source in the list
// Input:         icount the source number -1 in the list 
// Output:        .
// Return value:  nul-string, error
//                Epoch of the reference frame
//               
// Side effects: 
//------------------------------------------------------------------------
string VexPlus::RefEpoch( const int& icount ) const { 

  void* ptr;
  char* source;
  int islink, isname;
  char* value, *units;

  if( !(source = get_source_def( vexp )) ) return string("");
  if( icount ) 
    for( int i = 0; i < icount; i++ ) source = get_source_def_next();

  if( !(source) ) return string("");
  if( !(ptr = get_source_lowl( source, T_REF_COORD_FRAME, vexp)) ) return string("");
  if( (vex_field(T_REF_COORD_FRAME, ptr, 1, 
		 &islink,&isname,&value,&units)) ) return string("");
  
  return string(value);
}

//------------------------------------------------------------------------
// Method:        SourceType
// Description:   Return the icount+1 source in the list
// Input:         icount the source number -1 in the list 
// Output:        .
// Return value:  null-string, error
//                name of source
//               
// Side effects: 
//------------------------------------------------------------------------
string VexPlus::SourceType( const int& icount ) const { 

  void* ptr;
  char* source;
  int islink, isname;
  char* value, *units;

  if( !(source = get_source_def( vexp )) ) return string("");
  if( icount ) 
    for( int i = 0; i < icount; i++ ) source = get_source_def_next();

  if( !(source) ) return string("");
  if( !(ptr = get_source_lowl( source, T_SOURCE_TYPE, vexp)) ) return string("");
  if( (vex_field(T_SOURCE_TYPE, ptr, 1, 
		 &islink,&isname,&value,&units)) ) return string("");
  
  return string(value);
}

//------------------------------------------------------------------------
// Method:        IAUName
// Description:   Return the icount+1 source in the list
// Input:         icount the source number -1 in the list 
// Output:        .
// Return value:  null-string, error
//                name of source
//               
// Side effects: 
//------------------------------------------------------------------------
string VexPlus::IAUName( const int& icount ) const { 

  void* ptr;
  char* source;
  int islink, isname;
  char* value, *units;

  if( !(source = get_source_def( vexp )) ) return string("");
  if( icount ) 
    for( int i = 0; i < icount; i++ ) source = get_source_def_next();

  if( !(source) ) return string("");
  if( !(ptr = get_source_lowl( source, T_IAU_NAME, vexp)) ) return string("");
  if( (vex_field(T_IAU_NAME, ptr, 1, 
		 &islink,&isname,&value,&units)) ) return string("");
  
  return string(value);
}

//------------------------------------------------------------------------
// Method:        Source_RA
// Description:   Return the Right Ascencion icount+1 source
// Input:         icount: the source number -1 in the list 
// Output:        .
// Return value:  9e9 error
//                RA in radians
//               
// Side effects: 
//------------------------------------------------------------------------
double VexPlus::Source_RA( const int& icount ) const { 

  void* ptr;
  char* source;
  int islink, isname;
  char* value, *units;

  if( !(source = get_source_def( vexp )) ) return 9e9;
  if( icount ) 
    for( int i = 0; i < icount; i++ ) source = get_source_def_next();

  if( !(source) ) return 9e9;
  if( !(ptr = get_source_lowl( source, T_RA, vexp)) ) return 9e9;
  if( (vex_field(T_RA, ptr, 1, 
		 &islink,&isname,&value,&units)) ) return 9e9;

  int   hours, minutes;
  double seconds;

  if( !(sscanf( value, "%dh%dm%lfs", &hours, &minutes, &seconds )) )
      return 9e9;

  return ((hours*3600 + 60*minutes + seconds ) * 2 * PI)/SECS_PER_DAY;
}

//------------------------------------------------------------------------
// Method:        Source_Dec
// Description:   Return the declination for icount+1 source
// Input:         icount the source number -1 in the list 
// Output:        .
// Return value:  9e9 error           // little tricky 
//                declination in radians
//               
// Side effects: 
//------------------------------------------------------------------------
double VexPlus::Source_Dec( const int& icount ) const { 

  void* ptr;
  char* source;
  int islink, isname;
  char* value, *units;

  if( !(source = get_source_def( vexp )) ) return 9e9;
  if( icount ) 
    for( int i = 0; i < icount; i++ ) source = get_source_def_next();

  if( !(source) ) return 9e9;
  if( !(ptr = get_source_lowl( source, T_DEC, vexp)) ) return 9e9;
  if( (vex_field(T_DEC, ptr, 1, 
		 &islink,&isname,&value,&units)) ) return 9e9;

  int   degs, minutes;
  double seconds;

  if( !(sscanf( value, "%dd%d\'%lf\"", &degs, &minutes, &seconds )) )
      return 9e9;
  // Convert degrees to radials
  if( strchr(value, '-') )
    return -1*(PI/180)*(abs(degs)+minutes/60.0+seconds/3600);
  else 
    return (PI/180)*(abs(degs)+minutes/60.0+seconds/3600);
  return 9e9;
}

//------------------------------------------------------------------------
// Method:        Source_RARate
// Description:   Return the Right Ascencion icount+1 source
// Input:         icount: the source number -1 in the list 
// Output:        .
// Return value:  9e9 error
//                RA rate in radians/yr
//               
// Side effects: 
//------------------------------------------------------------------------
double VexPlus::Source_RARate( const int& icount ) const { 

  void* ptr;
  char* source;
  int islink, isname;
  char* value, *units;

  if( !(source = get_source_def( vexp )) ) return 9e9;
  if( icount ) 
    for( int i = 0; i < icount; i++ ) source = get_source_def_next();

  if( !(source) ) return 9e9;
  if( !(ptr = get_source_lowl( source, T_RA_RATE, vexp)) ) return 9e9;
  if( (vex_field(T_RA, ptr, 1, 
		 &islink,&isname,&value,&units)) ) return 9e9;

  if( units != "asec/yr" ) return 9e9;

  return (atof(value) * 2 * PI)/SECS_PER_DAY;
}

//------------------------------------------------------------------------
// Method:        Source_DecRate
// Description:   Return the Right Ascencion icount+1 source
// Input:         icount: the source number -1 in the list 
// Output:        .
// Return value:  9e9 error
//                RA in radians
//               
// Side effects: 
//------------------------------------------------------------------------
double VexPlus::Source_DecRate( const int& icount ) const { 

  void* ptr;
  char* source;
  int islink, isname;
  char* value, *units;

  if( !(source = get_source_def( vexp )) ) return 9e9;
  if( icount ) 
    for( int i = 0; i < icount; i++ ) source = get_source_def_next();

  if( !(source) ) return 9e9;
  if( !(ptr = get_source_lowl( source, T_DEC_RATE, vexp)) ) return 9e9;
  if( (vex_field(T_DEC_RATE, ptr, 1, 
		 &islink,&isname,&value,&units)) ) return 9e9;

  if( units != "asec/yr" ) return 9e9;

  return (atof(value) * 2 * PI)/SECS_PER_DAY;

}

//------------------------------------------------------------------------
// Method:        Source_Epoch
// Description:   Return the Right Ascencion icount+1 source
// Input:         icount: the source number -1 in the list 
// Output:        .
// Return value:  null-string, error
//                RA in radians
//               
// Side effects: 
//------------------------------------------------------------------------
string VexPlus::Source_Epoch( const int& icount ) const { 

  void* ptr;
  char* source;
  int islink, isname;
  char* value, *units;

  if( !(source = get_source_def( vexp )) ) return string("");
  if( icount ) 
    for( int i = 0; i < icount; i++ ) source = get_source_def_next();

  if( !(source) ) return string("");
  if( !(ptr = get_source_lowl( source, T_SOURCE_POSITION_EPOCH, vexp)) ) return string("");
  if( (vex_field(T_SOURCE_POSITION_EPOCH, ptr, 1, 
		 &islink,&isname,&value,&units)) ) return string("");

  return string(value);
}

//------------------------------------------------------------------------
// Method:        Vel_LSR
// Description:   Return the Right Ascencion icount+1 source
// Input:         icount: the source number -1 in the list 
// Output:        .
// Return value:  9e9 error
//                RA in radians
//               
// Side effects: 
//------------------------------------------------------------------------
double VexPlus::Vel_LSR( const int& icount ) const { 

  void* ptr;
  char* source;
  int islink, isname;
  char* value, *units;

  if( !(source = get_source_def( vexp )) ) return 9e9;
  if( icount ) 
    for( int i = 0; i < icount; i++ ) source = get_source_def_next();

  if( !(source) ) return 9e9;
  if( !(ptr = get_source_lowl( source, T_VELOCITY_WRT_LSR, vexp)) ) return 9e9;
  if( (vex_field(T_VELOCITY_WRT_LSR, ptr, 1, 
		 &islink,&isname,&value,&units)) ) return 9e9;

  return atof( value );
}



//________________$STATION_______________________________________________
//
//------------------------------------------------------------------------
// Method:        N_Stations
// Description:   Total number of stations in VEX
// Input:         
// Output:        
// Return value:  0, No stations = default
//                > 0 number of stations
//               
// Side effects: 
//------------------------------------------------------------------------
int VexPlus::N_Stations( ) const {

  int icount = 0;
  char * ptr = 0;
  if( (ptr = get_station_def(vexp)) ) {
    do {
      icount++;
    } while ( (ptr = get_station_def_next()) );
  }
  return icount;
}

//------------------------------------------------------------------------
// Method:        Station
// Description:   Return the icount+1 station in the list
// Input:         icount the station number -1 in the list 
// Output:        .
// Return value:  null-string, error
//                name of station
//               
// Side effects: 
//------------------------------------------------------------------------
string VexPlus::Station( const int& icount ) const { 

  char *station;
  if( !(station = get_station_def( vexp )) ) return string("");
  if( icount ) 
    for( int i = 0; i < icount; i++ ) station = get_station_def_next();
      
  return string(station);
}

//________________$TAPELOG_OBS_______________________________________________
//
//------------------------------------------------------------------------
// Method:        N_VSNs
// Description:   Number of VSNs in a BBC ref
// Input:         station & mode of VSNs
// Output:        
// Return value:  0, no channels or error
//                > 0 number of VSNS
//               
// Side effects: 
//------------------------------------------------------------------------
int VexPlus::N_VSNs( const string& stat ) const {
  int icount = 0;
  char stat_c[128];
  stat.copy( stat_c, string::npos );
  stat_c[stat.length()] = 0;

  if( (get_station_lowl(stat_c,T_VSN,B_TAPELOG_OBS,vexp)) ) {
    do {
      icount++;
    } while ( (get_station_lowl_next()) );
  }
  return icount;
}

//------------------------------------------------------------------------
// Method:        VSN     
// Description:   return the net sideband as a string.
// Input:         station & mode of freq, icount chan_def -1 to return
// Output:        .
// Return value:  0, failed
//                Net sideband
//               
// Side effects: 
//------------------------------------------------------------------------
string VexPlus::VSN( const string& stat, const int& icount ) const {

  char *value, *units;
  int islink, isname;
  void* ptr;
  
  char stat_c[128];
  stat.copy( stat_c, string::npos );
  stat_c[stat.length()] = 0;

  
  if( !(ptr = get_station_lowl(stat_c,T_VSN,B_TAPELOG_OBS,vexp)) ) 
    return string("");
  if( (vex_field(T_VSN, ptr, 2,
		 &islink,&isname,&value,&units)) ) return string("");

  if( icount ) {
    for( int i = 0; i < icount; i++ ) {
      if( !(ptr = get_station_lowl_next()) ) return string("");
    }
    if( (vex_field(T_VSN, ptr, 2, 
		   &islink,&isname,&value,&units)) ) 
      return string("");

  }
  return string(value);

}

//------------------------------------------------------------------------
// Method:        VSN_start
// Description:   return the net sideband as a string.
// Input:         station & mode of freq, icount chan_def -1 to return
// Output:        .
// Return value:  null-string, failed
//                Net sideband
//               
// Side effects: 
//------------------------------------------------------------------------
string VexPlus::VSN_start( const string& stat, const int& icount ) const {

  char *value, *units;
  int islink, isname;
  void* ptr;
  
  char stat_c[128];
  stat.copy( stat_c, string::npos );
  stat_c[stat.length()] = 0;
  
  if( !(ptr = get_station_lowl(stat_c,T_VSN,B_TAPELOG_OBS,vexp)) ) 
    return string("");
  if( (vex_field(T_VSN, ptr, 3,
		 &islink,&isname,&value,&units)) ) return string("");

  if( icount ) {
    for( int i = 0; i < icount; i++ ) {
      if( !(ptr = get_station_lowl_next()) ) return string("");
    }
    if( (vex_field(T_VSN, ptr, 3, 
		   &islink,&isname,&value,&units)) ) 
      return string("");

  }
  return string(value);

}

//------------------------------------------------------------------------
// Method:        VSN_end  
// Description:   return the net sideband as a string.
// Input:         station & mode of freq, icount chan_def -1 to return
// Output:        .
// Return value:  null-string, failed
//                Net sideband
//               
// Side effects: 
//------------------------------------------------------------------------
string VexPlus::VSN_end( const string& stat, const int& icount ) const {

  char *value, *units;
  int islink, isname;
  void* ptr;
  
  char stat_c[128];
  stat.copy( stat_c, string::npos );
  stat_c[stat.length()] = 0;
  
  if( !(ptr = get_station_lowl(stat_c,T_VSN,B_TAPELOG_OBS,vexp)) ) 
    return string("");
  if( (vex_field(T_VSN, ptr, 4,
		 &islink,&isname,&value,&units)) ) return string("");

  if( icount ) {
    for( int i = 0; i < icount; i++ ) {
      if( !(ptr = get_station_lowl_next()) ) return string("");
    }
    if( (vex_field(T_VSN, ptr, 4, 
		   &islink,&isname,&value,&units)) ) 
      return string("");

  }
  return string(value);

}


//________________$TRACKS________________________________________________
//
//------------------------------------------------------------------------
// Method:        N_TrackLines
// Description:   Number of fan_out def lines in Track def
// Input:         station & mode of Track def
// Output:        
// Return value:  0, no bitstreams or error or fanin
//                > 0 number of Fanout defs
//               
// Side effects: [[SIMPLIFICATION]] Fails for fan-in
//------------------------------------------------------------------------
int VexPlus::N_TrackLines( const string& stat, const string& mode ) const {

  int icount = 0;
  char stat_c[128], mode_c[128];
  stat.copy( stat_c, string::npos );
  stat_c[stat.length()] = 0;
  mode.copy( mode_c, string::npos );
  mode_c[mode.length()] = 0;
  
  if( (get_all_lowl(stat_c,mode_c,T_FANOUT_DEF,B_TRACKS,vexp)) ) {
    do {
      icount++;
    } while ( (get_all_lowl_next()) );
  }
  return icount;
}

//------------------------------------------------------------------------
// Method:        N_BitStreams
// Description:   Number of Bitstream from Track def
// Input:         station & mode of Track def
// Output:        
// Return value:  0, no bits or error or fanin
//                > 0 number of BitsStreams in every subpass
//               
// Side effects: [[SIMPLIFICATION]] Fails for fan-in
//------------------------------------------------------------------------
int VexPlus::N_BitStreams( const string& stat, const string& mode ) const {

  int islink, isname;
  char* value, *units;

  char stat_c[128], mode_c[128];
  stat.copy( stat_c, string::npos );
  stat_c[stat.length()] = 0;
  mode.copy( mode_c, string::npos );
  mode_c[mode.length()] = 0;
  void* ptr;

  char *subpass;
  int icount = 0;

  if( (ptr = get_all_lowl(stat_c,mode_c,T_FANOUT_DEF,B_TRACKS,vexp)) ) {
    do {
      vex_field(T_FANOUT_DEF, ptr, 1, &islink,&isname,&value,&units);
      if( icount == 0 ) subpass = value;
      if( !(strcmp(subpass,value)) ) icount++;
    } while ( (ptr = get_all_lowl_next()) );
  }
  return icount;
}

//------------------------------------------------------------------------
// Method:        N_Bits
// Description:   Number of Bitstream from Track def
// Input:         station & mode of Track def
// Output:        
// Return value:  0, no bits or error or fanin
//                > 0 number of Bits
//               
// Side effects: [[SIMPLIFICATION]] Fails for fan-in
//------------------------------------------------------------------------
int VexPlus::N_Bits( const string& stat, const string& mode ) const
{

  int islink, isname;
  char* value, *units;

  char stat_c[128], mode_c[128];
  stat.copy( stat_c, string::npos );
  stat_c[stat.length()] = 0;
  mode.copy( mode_c, string::npos );
  mode_c[mode.length()] = 0;
  void* ptr;
  
  int n_bits = 1;

  if( (ptr = get_all_lowl(stat_c,mode_c,T_FANOUT_DEF,B_TRACKS,vexp)) ) {
    do {
      vex_field(T_FANOUT_DEF, ptr, 3, &islink,&isname,&value,&units);
      if( (strcmp(value,"sign")) && (strcmp(value,"mag")) )
        return 0;
      else 
        if( !(strcmp(value,"mag")) ) n_bits = 2;
    } while ( (ptr = get_all_lowl_next()) );
  }
  return n_bits;
}

//------------------------------------------------------------------------
// Method:        Fanout
// Description:   Fan out factor
// Input:         station & mode of Bitstream
// Output:        
// Return value:  0, no channels or error, or fan-in
//                1,2,4 Fanout factor
//               
// Side effects: 
//------------------------------------------------------------------------
int VexPlus::FanOut( const string& stat, const string& mode ) const {

  int islink, isname;
  char* value, *units;

  char stat_c[128], mode_c[128];
  stat.copy( stat_c, string::npos );
  stat_c[stat.length()] = 0;
  mode.copy( mode_c, string::npos );
  mode_c[mode.length()] = 0;
  void* ptr;

  if( !(ptr = get_all_lowl(stat_c,mode_c,T_FANOUT_DEF,B_TRACKS,vexp)) )
    return 0;
    
  if( (vex_field(T_FANOUT_DEF, ptr, 5, &islink,&isname,&value,&units)) )
    return 0;

  //at least one track if next fails return 1
  if( (vex_field(T_FANOUT_DEF, ptr, 6, &islink,&isname,&value,&units)) )
    return 1;

  //at least two tracks
  if( (vex_field(T_FANOUT_DEF, ptr, 7, &islink,&isname,&value,&units)) )
    return 2;
  
  // there are 3 than there must be 4, so this must work
  if( (vex_field(T_FANOUT_DEF, ptr, 8, &islink,&isname,&value,&units)) )
    return 0;

  return 4;
}
//------------------------------------------------------------------------
// Method:        N_Tracks
// Description:   Total number of tracks
// Input:         station & mode of track def
// Output:        
// Return value:  0, no channels or error, or fan-in
//                
//               
// Side effects:  Higher level
//------------------------------------------------------------------------
int VexPlus::N_Tracks( const string& stat, const string& mode ) const {

  return FanOut(stat,mode) * N_BitStreams(stat,mode);

}

//------------------------------------------------------------------------
// Method:        Resolve_track_freq
// Description:   return the linkname set a track link to a freq
// Input:         station & mode of track, icount chan_def -1 to return
// Output:        .
// Return value:  null-string, unresolved or not a link
//               
//               
// Side effects: 
//------------------------------------------------------------------------
string VexPlus::Resolve_track_freq( const string& stat, const string& mode, 
				    const int& icount ) const {
  int islink, isname;
  char* name, *units;
  void* ptr;

  char stat_c[128], mode_c[128];
  stat.copy( stat_c, string::npos );
  stat_c[stat.length()] = 0;
  mode.copy( mode_c, string::npos );
  mode_c[mode.length()] = 0;

  if( !(ptr = get_all_lowl(stat_c,mode_c,T_FANOUT_DEF,B_TRACKS,vexp)) ) 
    return string("");
  if( (vex_field(T_FANOUT_DEF, ptr, 2,
		       &islink,&isname,&name,&units)) ) 
    return string("");
  if( !(islink || isname) ) return string("");

  if( icount ) {
    for( int i = 0; i < icount; i++ ) {
      if( !(ptr = get_all_lowl_next()) ) return string("");
    }
    if( (vex_field(T_FANOUT_DEF, ptr, 2,
		   &islink,&isname,&name,&units)) ) 
      return string("");
  }

  return string(name);
}

//------------------------------------------------------------------------
// Method:        Track
// Description:   return 'Chan-ID' linkword for track icount
// Input:         station, mode of freq, icount track
// Output:        .
// Return value:  null-string, failed
//               
//               
// Side effects: 
//------------------------------------------------------------------------
string VexPlus::Track( const string& stat, const string& mode,const int& icount ) const 
{

  char *value, *units;
  int islink, isname;
  void* ptr;

  char stat_c[128], mode_c[128];
  stat.copy( stat_c, string::npos );
  stat_c[stat.length()] = 0;
  mode.copy( mode_c, string::npos );
  mode_c[mode.length()] = 0;

  int n_lines = N_TrackLines(stat,mode);
  int n_fan = FanOut(stat,mode);


  if( !(ptr = get_all_lowl(stat_c,mode_c,T_FANOUT_DEF,B_TRACKS,vexp)) ) 
      return string("");

  for( int iline = 0; iline < n_lines; iline++ ) {

    for( int ifan = 0; ifan < n_fan; ifan++ ) {

      if( (vex_field(T_FANOUT_DEF, ptr, 5+ifan,&islink,&isname,&value,&units) ) )
        return string("");

      int trackno = atoi( value );
      if( trackno == icount ) {
        vex_field(T_FANOUT_DEF, ptr,2,&islink,&isname,&value,&units);
        return string(value);
      }
    }
    if( !(ptr = get_all_lowl_next()) ) return string("");
  }
  // Came to the end?
  return string("");

}

//------------------------------------------------------------------------
// Method:        TrackSubpass
// Description:   return Subpass code for the icount-th fanout_def line
// Input:         station, mode of freq, icount track
// Output:        .
// Return value:  null-string, failed
//               
//               
// Side effects: 
//------------------------------------------------------------------------
string VexPlus::TrackSubpass( const string& stat, const string& mode, 
			      const int& icount ) const {

  int islink, isname;
  char* name, *units;
  void* ptr;

  char stat_c[128], mode_c[128];
  stat.copy( stat_c, string::npos );
  stat_c[stat.length()] = 0;
  mode.copy( mode_c, string::npos );
  mode_c[mode.length()] = 0;

  if( !(ptr = get_all_lowl(stat_c,mode_c,T_FANOUT_DEF,B_TRACKS,vexp)) ) 
    return string("");
  if( (vex_field(T_FANOUT_DEF, ptr, 1,
		       &islink,&isname,&name,&units)) ) 
    return string("");
  if( !(islink || isname) ) return string("");

  if( icount ) {
    for( int i = 0; i < icount; i++ ) {
      if( !(ptr = get_all_lowl_next()) ) return string("");
    }
    if( (vex_field(T_FANOUT_DEF, ptr, 1,
		   &islink,&isname,&name,&units)) ) 
      return string("");
  }

  if (!name) return string(""); //empty value (050530fmo)
  return string(name);
}

//------------------------------------------------------------------------
// Method:        TrackSignMag
// Description:   return bitstream type for the icount-th fanout_def line
// Input:         station, mode of freq, icount track
// Output:        .
// Return value:  null-string, failed
//               
//               
// Side effects: 
//------------------------------------------------------------------------
string VexPlus::TrackSignMag( const string& stat, const string& mode, 
			      const int& icount ) const {

  int islink, isname;
  char* name, *units;
  void* ptr;

  char stat_c[128], mode_c[128];
  stat.copy( stat_c, string::npos );
  stat_c[stat.length()] = 0;
  mode.copy( mode_c, string::npos );
  mode_c[mode.length()] = 0;

  if( !(ptr = get_all_lowl(stat_c,mode_c,T_FANOUT_DEF,B_TRACKS,vexp)) ) 
    return string("");
  if( (vex_field(T_FANOUT_DEF, ptr, 3,
		       &islink,&isname,&name,&units)) ) 
    return string("");
  if( !(islink || isname) ) return string("");

  if( icount ) {
    for( int i = 0; i < icount; i++ ) {
      if( !(ptr = get_all_lowl_next()) ) return string("");
    }
    if( (vex_field(T_FANOUT_DEF, ptr, 3,
		   &islink,&isname,&name,&units)) ) 
      return string("");
  }

  return string(name);
}

//------------------------------------------------------------------------
// Method:        HeadstackNr
// Description:   return headstack number for the icount-th fanout_def line
// Input:         station, mode of freq, icount track
// Output:        
// Return value:  0, error
//                headstack number [1-4]
//               
// Side effects: 
//------------------------------------------------------------------------
int VexPlus::HeadstackNr( const string& stat, const string& mode, 
			  const int& icount ) const {
  
  int islink, isname;
  char* value, *units;
  int stackNr = -99;
  
  char stat_c[128], mode_c[128];
  stat.copy( stat_c, string::npos );
  stat_c[stat.length()] = 0;
  mode.copy( mode_c, string::npos );
  mode_c[mode.length()] = 0;
  void* ptr;
  
  if( !(ptr = get_all_lowl(stat_c,mode_c,T_FANOUT_DEF,B_TRACKS,vexp)) ) 
    return -4;
  if( (vex_field(T_FANOUT_DEF, ptr, 4,
		 &islink,&isname,&value,&units)) ) 
    return -1;
  
  if( icount ) {
    for( int i = 0; i < icount; i++ ) {
      if( !(ptr = get_all_lowl_next()) ) return -2;
    }
    if( (vex_field(T_FANOUT_DEF, ptr, 4,
		   &islink,&isname,&value,&units)) ) 
      return -3;
  }
  
  stackNr = atoi( value );
  return stackNr;
}

//------------------------------------------------------------------------
// Method:        TrackFormat
// Description:   return the format of the tapes
// Input:         station & mode of tapes
// Output:        .
// Return value:  null-string, failed
//                format string
//               
// Side effects: 
//------------------------------------------------------------------------
string VexPlus::TrackFormat( const string& stat, const string& mode ) const {

  char *value, *units;
  int islink, isname;
  void* ptr;
  
  char stat_c[128], mode_c[128];
  stat.copy( stat_c, string::npos );
  stat_c[stat.length()] = 0;
  mode.copy( mode_c, string::npos );
  mode_c[mode.length()] = 0;
  
  if( !(ptr = get_all_lowl(stat_c,mode_c,T_TRACK_FRAME_FORMAT,B_TRACKS,vexp)) ) 
    return string("");
  if( (vex_field(T_TRACK_FRAME_FORMAT, ptr, 1,
		 &islink,&isname,&value,&units)) ) return string("");

  return string(value);

}

//------------------------------------------------------------------------
// Method:        Modulation
// Description:   return the format of the tapes
// Input:         station & mode of tapes
// Output:        .
// Return value:  0, not present no modulation
//                1 modulation on
//               
// Side effects: 
//------------------------------------------------------------------------
int VexPlus::Modulation( const string& stat, const string& mode ) const {

  char *value, *units;
  int islink, isname;
  void* ptr;
  
  char stat_c[128], mode_c[128];
  stat.copy( stat_c, string::npos );
  stat_c[stat.length()] = 0;
  mode.copy( mode_c, string::npos );
  mode_c[mode.length()] = 0;
  
  if( !(ptr = get_all_lowl(stat_c,mode_c,T_DATA_MODULATION,B_TRACKS,vexp)) ) 
    return 0;
  if( (vex_field(T_TRACK_FRAME_FORMAT, ptr, 1,
		 &islink,&isname,&value,&units)) ) return 0;

  if( !strcmp(value,"on") ) return 1;

  return 0;

}

//------------------------------------------------------------------------
// Method:        TrackNr
// Description:   Get the icount's (0-based) track number.
// Input:         station & mode of Bitstream, index icount
// Output:        
// Return value:  0, error
//                track number within the headstack
//               
// Side effects: 
//------------------------------------------------------------------------
int VexPlus::TrackNr( const string& stat, const string& mode,
                      const int& icount) const {

  int islink, isname;
  char* value, *units;
  int trackNr;
  
  char stat_c[128], mode_c[128];
  stat.copy( stat_c, string::npos );
  stat_c[stat.length()] = 0;
  mode.copy( mode_c, string::npos );
  mode_c[mode.length()] = 0;
  void* ptr;
  
  const int fanOut  = FanOut(stat,mode);
  const int lineNr  = icount/fanOut; //0-based fanout_def lineNr 
  const int fieldNr = 5 + icount%fanOut; //fanout_def fieldNr; 5-8
  
  if( !(ptr = get_all_lowl(stat_c,mode_c,T_FANOUT_DEF,B_TRACKS,vexp)) )
    return 0;
  
  //
  // Skip to the right TRACKS.fanout_def line.
  //
  for(int iline=0; iline<lineNr; iline++)
    {
      if( !(ptr = get_all_lowl_next()) ) return 0;
    }
  
  //
  // Get the right field from that line.
  //
  if( (vex_field(T_FANOUT_DEF, ptr, fieldNr, &islink,&isname,&value,&units)) ) 
    return 0;
  
  trackNr = atoi( value );
  
  return trackNr;
}
//________________$SCHED__________________________________________________
//
//------------------------------------------------------------------------
// Method:        N_Scans
// Description:   Return the Number of scans for icount+1st scan
// Input:         stat the station
// Output:        .
// Return value:  0 error, or nothing found
//                Number of scans
//               
// Side effects: 
//------------------------------------------------------------------------
int VexPlus::N_Scans( const string& stat ) const {

  Llist *lowl;
  void* ptr;

  char stat_c[128];
  stat.copy( stat_c, string::npos );
  stat_c[stat.length()] = 0;
  
  int icount = 0;
  char* scanname;
  if( (ptr = get_scan_station( &lowl, &scanname, stat_c, vexp)) ) {
    do {
      icount++;
    } while ( (ptr = get_scan_station_next(&lowl, &scanname)) );
  }
  return icount;

}

//------------------------------------------------------------------------
// Method:        ScanName
// Description:   Return the name for icount+1st scan
// Input:         stat the station, icount the scan number -1 in the list 
// Output:        .
// Return value:  null-string, error
//                string
//               
// Side effects: 
//------------------------------------------------------------------------
string VexPlus::ScanName(const string& stat, const int& icount ) const {

  Llist *lowl;
  void* ptr;

  char stat_c[128];
  stat.copy( stat_c, string::npos );
  stat_c[stat.length()] = 0;

  char* scanname;
  get_scan_station(&lowl, &scanname, stat_c, vexp );

  if( icount ) {
    for( int i = 0; i < icount; i++ ) {
      if( !(ptr = get_scan_station_next(&lowl, &scanname)) ) return string("");
    }
  }

  return string(scanname);

}

//------------------------------------------------------------------------
// Method:        ScanName
// Description:   Return the name for icount+1st scan
// Input:         icount the scan number -1 in the list 
// Output:        .
// Return value:  null-string, error
//                string
//               
// Side effects: 
//------------------------------------------------------------------------
string VexPlus::ScanName(const int& icount ) const {

  void* ptr = NULL;
  char* scanname;

  get_scan(&scanname, vexp );
  if( icount ) {
    for( int i = 0; i < icount; i++ ) {
      if( !(ptr = get_scan_next(&scanname)) ) return string("");
    }
  }

  return string(scanname);

}

//------------------------------------------------------------------------
// Method:        ScanStart
// Description:   Return the starttime for icount+1st scan
// Input:         stat the station, icount the scan number -1 in the list 
// Output:        .
// Return value:  null-string, error
//                string of time
//               
// Side effects: 
//------------------------------------------------------------------------
string VexPlus::ScanStart(const string& stat, const int& icount ) const {

  Llist *lowl;
  void* ptr;

  char stat_c[128];
  stat.copy( stat_c, string::npos );
  stat_c[stat.length()] = 0;

  char* scanname;
  get_scan_station(&lowl, &scanname, stat_c, vexp );
  ptr=get_scan_start(lowl);

  if( icount ) {
    for( int i = 0; i < icount; i++ ) {
      if( !(ptr = get_scan_station_next(&lowl, &scanname)) ) return string("");
    }
    if( !(ptr = get_scan_start(lowl)) ) 
      return string("");    
  }

  return string((char*)ptr);

}

//------------------------------------------------------------------------
// Method:        ScanStart // NGHK
// Description:   Return the starttime for icount+1st scan
// Input:         icount the scan number -1 in the list 
// Output:        .
// Return value:  null-string, error
//                string of time
//               
// Side effects: 
//------------------------------------------------------------------------
string VexPlus::ScanStart(const int& icount ) const {
  Llist *scan = Scan(icount);
  void *ptr;
  
  if( !(ptr = get_scan_start(scan)) ) 
    return string("");    
  
  return string((char*)ptr);

}


//------------------------------------------------------------------------
// Method:        ScanName // NGHK
// Description:   Return the icount+1st scan
// Input:         icount the scan number -1 in the list 
// Output:        .
// Return value:  a pointer to the scan or NULL on error
//               
// Side effects: 
//------------------------------------------------------------------------
Llist* VexPlus::Scan( const int& icount ) const {
  void* ptr = NULL;
  char* scanname;

  get_scan(&scanname, vexp );
  if( icount ) {
    for( int i = 0; i < icount; i++ ) {
      if( !(ptr = get_scan_next(&scanname)) ) {
        std::cout << i << " " << string(scanname) << std::endl;
        return NULL;
      }
    }
  }

  return (Llist*)ptr;
}


//------------------------------------------------------------------------
// Method:        ScanMode
// Description:   Return the mode for icount+1st scan
// Input:         stat the station, icount the scan number -1 in the list 
// Output:        .
// Return value:  null-string, error
//                string of mode name
//               
// Side effects: 
//------------------------------------------------------------------------
string VexPlus::ScanMode(const string& stat, const int& icount ) const {

  Llist *lowl;
  void* ptr;

  char stat_c[128];
  stat.copy( stat_c, string::npos );
  stat_c[stat.length()] = 0;

  char* scanname;
  get_scan_station(&lowl, &scanname, stat_c, vexp );
  ptr=get_scan_mode(lowl);

  if( icount ) {
    for( int i = 0; i < icount; i++ ) {
      if( !(ptr = get_scan_station_next(&lowl, &scanname)) ) return string("");
    }
    if( !(ptr = get_scan_mode(lowl)) ) 
      return string("");    
  }

  return string((char*)ptr);

}

//------------------------------------------------------------------------
// Method:        ScanSource
// Description:   Return the source for icount+1st scan
// Input:         stat the station, icount the scan number -1 in the list 
// Output:        .
// Return value:  null-string, error
//                string of source key
//               
// Side effects: 
//------------------------------------------------------------------------
string VexPlus::ScanSource(const string& stat, const int& icount ) const {

  Llist *lowl;
  void* ptr;

  char stat_c[128];
  stat.copy( stat_c, string::npos );
  stat_c[stat.length()] = 0;

  char* scanname;
  get_scan_station(&lowl, &scanname, stat_c, vexp );
  ptr=get_scan_source(lowl);

  if( icount ) {
    for( int i = 0; i < icount; i++ ) {
      if( !(ptr = get_scan_station_next(&lowl, &scanname)) ) return string("");
    }
    if( !(ptr = get_scan_source(lowl)) ) 
      return string("");
  }

  return string((char*)ptr);

}

//------------------------------------------------------------------------
// Method:        ScanPass
// Description:   Return pass id for icount+1st scan and statio
// Input:         stat the station, icount the scan number -1 in the list 
// Output:        .
// Return value:  null-string, error
//                string of pass key
//               
// Side effects: 
//------------------------------------------------------------------------
string VexPlus::ScanPass(const string& stat, const int& icount ) const {

  char *value, *units;
  int islink, isname;
  void* ptr;
  Llist *lowl;
  
  char stat_c[128];
  stat.copy( stat_c, string::npos );
  stat_c[stat.length()] = 0;

  char* scanname;
  ptr = get_scan_station(&lowl, &scanname, stat_c, vexp );
  if( (vex_field(T_STATION, ptr, 5,
		 &islink,&isname,&value,&units)) ) return string("");

  if( icount ) {
    for( int i = 0; i < icount; i++ ) {
      if( !(ptr = get_scan_station_next(&lowl, &scanname)) ) return string("");
    }
    if( (vex_field(T_STATION, ptr, 5,
		   &islink,&isname,&value,&units)) ) 
      return string("");    
  }

  if (!value) return string(""); //empty value (050530fmo)
  return string(value);
}

//------------------------------------------------------------------------
// Method:        ScanDuration
// Description:   Return duration for icount+1st scan and station
// Input:         stat the station, icount the scan number -1 in the list 
// Output:        .
// Return value:  0 error
//                Double duration
//               
// Side effects:  ASSUMPTION made that time is in seconds
//------------------------------------------------------------------------
double VexPlus::ScanDuration(const string& stat, const int& icount ) const {

  char *value, *units;
  int islink, isname;
  void* ptr;
  Llist *lowl;
  
  char stat_c[128];
  stat.copy( stat_c, string::npos );
  stat_c[stat.length()] = 0;

  char* scanname;
  ptr = get_scan_station(&lowl, &scanname, stat_c, vexp );
  if( (vex_field(T_STATION, ptr, 3,
		 &islink,&isname,&value,&units)) ) return 0;

  if( icount ) {
    for( int i = 0; i < icount; i++ ) {
      if( !(ptr = get_scan_station_next(&lowl, &scanname)) ) return 0;
    }
    if( (vex_field(T_STATION, ptr, 3,
		   &islink,&isname,&value,&units)) ) 
      return 0;    
  }

  // [[SHORTCUT]] Assuming duration is in seconds
  return atof( value );
}

//------------------------------------------------------------------------
// Method:        Footage
// Description:   Return footage (or byte position) for icount+1st scan and station
// Input:         stat the station, icount the scan number -1 in the list 
// Output:        .
// Return value:  <0 error
//                Double footages
//               
// Side effects: ASSUMPTION made that footage is in ft. If the VEX
//               field has "GB" units, convert to bytes.
//------------------------------------------------------------------------
double VexPlus::Footage(const string& stat, const int& icount ) const {

  char *value, *units;
  int islink, isname;
  void* ptr;
  Llist *lowl;
  
  char stat_c[128];
  stat.copy( stat_c, string::npos );
  stat_c[stat.length()] = 0;

  char* scanname;
  ptr = get_scan_station(&lowl, &scanname, stat_c, vexp );
  if( (vex_field(T_STATION, ptr, 4,
		 &islink,&isname,&value,&units)) ) return -1;

  if( icount ) {
    for( int i = 0; i < icount; i++ ) {
      if( !(ptr = get_scan_station_next(&lowl, &scanname)) ) return -1;
    }
    if( (vex_field(T_STATION, ptr, 4,
		   &islink,&isname,&value,&units)) ) 
      return 0;    
  }

  // [[SHORTCUT]] Assuming footage is in ft and bytage in bytes
  if (units && string(units)=="GB") return atof(value)*1e9;
  return atof( value );
}

//------------------------------------------------------------------------
// Method:        DataStart
// Description:   Return data start offset for icount+1st scan for station
// Input:         stat the station, icount the scan number -1 in the list 
// Output:        .
// Return value:  0 error
//                Double time_offset
//               
// Side effects:  ASSUMPTION made that time is in seconds
//------------------------------------------------------------------------
double VexPlus::DataStart(const string& stat, const int& icount ) const {

  char *value, *units;
  int islink, isname;
  void* ptr;
  Llist *lowl;
  
  char stat_c[128];
  stat.copy( stat_c, string::npos );
  stat_c[stat.length()] = 0;

  char* scanname;
  ptr = get_scan_station(&lowl, &scanname, stat_c, vexp );
  if( (vex_field(T_STATION, ptr, 2,
		 &islink,&isname,&value,&units)) ) return 0;

  if( icount ) {
    for( int i = 0; i < icount; i++ ) {
      if( !(ptr = get_scan_station_next(&lowl, &scanname)) ) return 0;
    }
    if( (vex_field(T_STATION, ptr, 2,
		   &islink,&isname,&value,&units)) ) 
      return 0;    
  }

  // [[SHORTCUT]] Assuming time offset is in seconds
  return atof( value );
}

//------------------------------------------------------------------------
// Method:        freqinMHz( )
// Description:   Return a string of value units in MHz
// Input:         string of value, string of units
// Output:        value of frequency in MHz
// Return value:  Negative if not recognized
// Side effects:  
//------------------------------------------------------------------------
double freqinMHz(const string& freqin, const string& unitsin) {

  double freq = atof(freqin.c_str());
  if (unitsin ==  "MHz")
    {
    }
  else if (unitsin == "GHz")
    {
      freq *= 1e3;
    }
  else if (unitsin == "kHz")
    {
      freq *= 1e-3;
    }
  else if (unitsin == "Hz")
    {
      freq *= 1e-6;
    }
  else if (unitsin == "mHz")
    {
      freq *= 1e-9;
    }
  else
    {
      cout << "Not a supported measure of units: " << unitsin << endl;
      freq = -1;
    }
  return freq;
}

//------------------------------------------------------------------------
// Method:        freqinHz( )
// Description:   Return a string of value units in Hz
// Input:         string of value, string of units
// Output:        value of frequency in Hz
// Return value:  Negative if not recognized
// Side effects:  
//------------------------------------------------------------------------
double freqinHz(const string& freqin, const string& unitsin) {

  double freq = atof(freqin.c_str());
  if (unitsin == "MHz")
    {
      freq *= 1e6;
    }
  else if (unitsin == "GHz")
    {
      freq *= 1e9;
    }
  else if (unitsin == "kHz")
    {
      freq *= 1e3;
    }
  else if (unitsin == "Hz")
    {
    }
  else if (unitsin == "mHz")
    {
      freq *= 1e-3;
    }
  else
    {
      cout << "Not a supported measure of units: " << unitsin << endl;
      freq = -1;
    }
  return freq;
}

/*

int VexPlus::first_track(const string& stat, const string& mode) const {
  // Example: get the 1st track assigment for the frequency block
  
  string link(Link_freq_track(stat,mode, 0));
  if (link.length() == 0) return -1;

  string resolved(Resolve_track_freq(stat, mode, 0));
  if (resolved.length() == 0) return -1;

  if (link == resolved) return -2;
  
  string trackno = Track(stat,mode,0);
  if (trackno.length() == 0) return -1;

  int tracknr = atoi(trackno.c_str())
  if (tracknr <= 0 ) return -9;

  return tracknr;
}


*/
