/* Copyright (c) 2007 Joint Institute for VLBI in Europe (Netherlands)
 * All rights reserved.
 *
 * Author(s): Ruud Oerlemans  <Oerlemans@JIVE.nl>, 2007
 *            Nico Kruithof   <Kruithof@JIVE.nl>, 2007
 *            Huseyin Ozdemir <Ozdemir@JIVE.nl>, 2007
 *
 * $Id: Uvw_model.cc 304 2007-09-05 08:09:10Z ozdemir $
 *
 * Class function definitions for station specific data
 */

//the class definitions and function definitions
#include "utils.h"
#include "uvw_model.h"

//standard c includes
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

//includes for system calls
#include <sys/types.h>
#include <fcntl.h>
#include <unistd.h>

//c++ includes
#include <iostream>
#include <iomanip>
#include <fstream>
#include <string>


//*****************************************************************************
//function definitions
//*****************************************************************************

//default constructor, set default values
Uvw_model::Uvw_model()
    : end_scan(0), acc_u(NULL), acc_v(NULL), acc_w(NULL), splineakima_u(NULL), splineakima_v(NULL),
    splineakima_w(NULL) {}

//destructor
Uvw_model::~Uvw_model() {}

void Uvw_model::operator=(const Uvw_model &other) {
  Uvw_model();

  times = other.times;
  u = other.u;
  v = other.v;
  w = other.w;
  initialise_spline_for_next_scan();
}


bool Uvw_model::operator==(const Uvw_model &other) const {
  return true;
}

//read the delay table, do some checks and
//calculate coefficients for parabolic interpolation
int Uvw_model::open(const char *delayTableName) {
  std::ifstream in(delayTableName);
  double line[5];
  int32_t hsize;

  in.read(reinterpret_cast < char * > (&hsize), sizeof(int32_t));
  char station[hsize];
  in.read(reinterpret_cast < char * > (station), hsize*sizeof(char));

  while (in.read(reinterpret_cast < char * > (line), 5*sizeof(double))) {
    // The time read from file is in seconds, whereas the software correlator
    // works with times in microseconds
    times.push_back(line[0]*1000000);
    u.push_back(line[1]);
    v.push_back(line[2]);
    w.push_back(line[3]);
  }
  initialise_spline_for_next_scan();

  return 0;
}

void Uvw_model::initialise_spline_for_next_scan() {

  SFXC_ASSERT(end_scan < times.size()-1);
  size_t next_end_scan = end_scan+2;

  while ((next_end_scan < times.size()) &&
         (times[next_end_scan] != 0)) {
    next_end_scan ++;
  }

  if (splineakima_u != NULL) {
    gsl_spline_free(splineakima_u);
    gsl_interp_accel_free(acc_u);
  }
  if (splineakima_v != NULL) {
    gsl_spline_free(splineakima_v);
    gsl_interp_accel_free(acc_v);
  }
  if (splineakima_w != NULL) {
    gsl_spline_free(splineakima_w);
    gsl_interp_accel_free(acc_w);
  }

  // Initialise the Akima spline
  acc_u = gsl_interp_accel_alloc();
  acc_v = gsl_interp_accel_alloc();
  acc_w = gsl_interp_accel_alloc();
  if (end_scan != 0) end_scan++;
  int n_pts = next_end_scan-end_scan;

  // End scan now points to the beginning of the next scan and
  // the next scan has n_pts data points
  splineakima_u = gsl_spline_alloc(gsl_interp_akima, n_pts);
  splineakima_v = gsl_spline_alloc(gsl_interp_akima, n_pts);
  splineakima_w = gsl_spline_alloc(gsl_interp_akima, n_pts);

  gsl_spline_init(splineakima_u,
                  &times[end_scan],
                  &u[end_scan],
                  n_pts);
  gsl_spline_init(splineakima_v,
                  &times[end_scan],
                  &v[end_scan],
                  n_pts);
  gsl_spline_init(splineakima_w,
                  &times[end_scan],
                  &w[end_scan],
                  n_pts);
  end_scan = next_end_scan;
}

//calculates u,v, and w at time(microseconds)
void Uvw_model::get_uvw(int64_t time, double *u, double *v, double *w) {
  if (times.empty()) {
    DEBUG_MSG("times.empty()");
    SFXC_ASSERT(!times.empty());
  }
  while (times[end_scan-1] < time) {
    initialise_spline_for_next_scan();
  }
  SFXC_ASSERT(splineakima_u != NULL);
  SFXC_ASSERT(splineakima_v != NULL);
  SFXC_ASSERT(splineakima_w != NULL);
  *u = gsl_spline_eval (splineakima_u, time, acc_u);
  *v = gsl_spline_eval (splineakima_v, time, acc_v);
  *w = gsl_spline_eval (splineakima_w, time, acc_w);
}

//calculates the delay for the delayType at time in microseconds
//get the next line from the delay table file
std::ofstream& Uvw_model::uvw_values(std::ofstream &output, int64_t starttime,
                                     int64_t stoptime, double inttime) {
  int64_t time=(int64_t)(starttime + inttime*1000/2);
  double gsl_u, gsl_v, gsl_w;
  output.precision(14);
  while (time < stoptime) {
    while (times[end_scan] < time) initialise_spline_for_next_scan();
    gsl_u = gsl_spline_eval (splineakima_u, time, acc_u);
    gsl_v = gsl_spline_eval (splineakima_v, time, acc_v);
    gsl_w = gsl_spline_eval (splineakima_w, time, acc_w);
    double ttime  = time/1000;

    output.write(reinterpret_cast < char * > (&ttime), sizeof(double));
    output.write(reinterpret_cast < char * > (&gsl_u), sizeof(double));
    output.write(reinterpret_cast < char * > (&gsl_v), sizeof(double));
    output.write(reinterpret_cast < char * > (&gsl_w), sizeof(double));
    time += (int64_t)inttime*1000;
  }
  return output;
}
