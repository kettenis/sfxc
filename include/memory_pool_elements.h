/* Copyright (c) 2007 Joint Institute for VLBI in Europe (Netherlands)
 * All rights reserved.
 *
 * Author(s): Nico Kruithof <Kruithof@JIVE.nl>, 2007
 *
 * $Id$
 *
 */

#ifndef BUFFER_H
#define BUFFER_H

#include <vector>
#include <assert.h>

// Ugly dependency to fftw3...
// needed for fftw_malloc that return
// optimized alignment.
#include <fftw3.h>

#include "utils.h"

/// Use this element for the buffer class if you want to store an array
/// on every position.
///// DEPRECATED SHOULD BE REMOVED...
template <class T, int N>
class Buffer_element {
public:
  typedef T value_type;
  inline int size() {
    return N;
  }

  T &operator[](int i) {
    assert(i >= 0);
    assert(i < N);
    return _buffer[i];
  }
  T *buffer() {
    return &_buffer[0];
  }
private:
  T _buffer[N];
};

/// Use this class if you want to allocate large number of element
/// static array declaration may fails if the array is too large...
template <class T, int N>
class Buffer_element_large {
public:
  typedef T value_type;
  inline int size() {
    return N;
  }

  Buffer_element_large() {
    //std::cout << "Building array of size" << N << std::endl;
    _buffer = static_cast<T*>(fftw_malloc(sizeof(T)*N));
  }

  Buffer_element_large(const Buffer_element_large& src) {
    //std::cout << "Building by copy an array of size" << N << std::endl;
    _buffer = static_cast<T*>(fftw_malloc(sizeof(T)*N));
    memcpy(_buffer, src._buffer, sizeof(T)*N );
  }

  ~Buffer_element_large() {
    //std::cout << "Deleting array of size" << N << std::endl;
    if (_buffer != NULL) {
      fftw_free(_buffer);
    }
  }

  inline T &operator[](int i) {
    assert(i >= 0);
    assert(i < N);
    return (_buffer)[i];
  }

  inline T *buffer() {
    return _buffer;
  }
private:

  T* _buffer;
};

/// Use this class if you want to allocate large number of element
/// static array declaration may fail if the array is too large...
template <class T>
class Aligned_vector {
public:
  typedef T value_type;
  inline size_t size() const {
    return size_;
  }

  Aligned_vector() {
    size_ = 0;
    buffer_ = NULL;
  }

  ~Aligned_vector() {
    // DEBUG_MSG("Deleting array element of size " << size());
    if (buffer_ != NULL) {
      fftw_free(buffer_);
    }
  }

  void resize(size_t size) {
    if ( size != size_ ) {
      if ( buffer_ == NULL ) {
        // fftw_malloc insure that the allocated data
        // is nicely aligned.
        buffer_ = static_cast<T*>( fftw_malloc( sizeof(T)*size ) );
        size_ = size;
      } else {
        T* oldbuffer = buffer_;
        buffer_ = static_cast<T*>( fftw_malloc( sizeof(T)*size ) );
        size_t min_size = std::min(size_,size);
        size_ = size;

        memcpy(buffer_, oldbuffer, sizeof(T)*min_size);

        fftw_free(oldbuffer);
      }
    }
  }


  inline T& operator[](int i) {
    assert(i >= 0);
    assert(i < (int)size_ );
    return buffer_[i];
  }

  inline const T& operator[](int i) const {
    assert(i >= 0);
    assert(i < (int)size_ );
    return buffer_[i];
  }

  inline T* buffer() {
    return buffer_;
  }
private:

  // We cannot use a std::vector because we cannot control that
  // the data are properly aligned to be used with fftw_xx or SSE
  T* buffer_;
  size_t size_;
};

template<class T>
class Buffer_element_vector : public Aligned_vector<T> {
public:
  typedef T value_type;
};

#endif // BUFFER_H