/* Copyright (c) 2007 Joint Institute for VLBI in Europe (Netherlands)
 * All rights reserved.
 *
 * Author(s): Nico Kruithof <Kruithof@JIVE.nl>, 2007
 *                   Aard Keimpema <keimpema@jive.nl>, 2008
 *
 * $Id: vlba_header.h 800 2008-06-11 11:49:47Z kruithof $
 *
 */

#ifndef VLBA_HEADER_H
#define VLBA_HEADER_H

#include "utils.h"

/** VLBA frame header in byte chunks, without parity bits. */
class VLBA_header  {
public:
  // N is the size of one input word in bytes
  VLBA_header();

  void set_header(int N, unsigned char *buffer_);
  bool check_header(uint8_t mask);

  int nTracks();

  int headstack(int track);
  int dar(int track);
  int track(int track_);
  bool is_sign(int track);
  bool is_magn(int track);

  // Time in microseconds since midnight (approx)
  int64_t microseconds(int track);
  // Time in secons since midnight (truncated time)
  int64_t seconds(int track);
  // Modified Julian day % 1000
  int julian_day(int track);

  int find_track(int headstack, int track);

  bool checkCRC(uint8_t mask);
  void recomputeCRC(uint8_t mask);

  bool is_valid(uint8_t mask);
private:
  template <class Type>
    void crc16(Type *crcBlock, Type *data, int datawords, Type mask);

  // Binary coded decimal:
  int BCD(int word, unsigned int track, unsigned char *header_);
  int BCD_REVERSE(int word, unsigned int track, unsigned char *header_);

  int get_bit(int word, int track, unsigned char *header_) {
    return (header_[word*N + track/8] >> track%8)&1;
  }

private:
  unsigned char* header;
  unsigned char* header_aux;
  unsigned char* buffer;

  // Number of bytes per data word (#tracks/8)
  int N;
};

#endif // VLBA_HEADER_H
