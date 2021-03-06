/* Copyright (c) 2007 Joint Institute for VLBI in Europe (Netherlands)

 * Copyright (c) 2007 University of Amsterdam (Netherlands)

 * All rights reserved.

 *

 * Author(s): Nico Kruithof <Kruithof@JIVE.nl>, 2007

 *            Damien Marchal <dmarchal@science.uva.nl>, 2007

 *

 *

 * This file is part of:

 *   - containers library

 * This file contains:

 *   - Declaration and definition of the pc_queue

 */
#ifndef PC_QUEUE_H

#define PC_QUEUE_H



#include <iostream>

#include <vector>

#include <queue>


#include "mutex.h"
#include "raiimutex.h"
#include "condition.h"
#include "exception_common.h"

#include "allocator.h"


#ifdef ENABLE_TEST_UNIT
#include "Test_unit.h"
#endif // ENABLE_TEST_UNIT

/************************************************

* @class PC_queue
* @desc A queue implementation that is
* adequate for asynchrnous-thread-safe

* application. This class doesn't do any
* memory managment related to the object that
* are in the queue, please consult PC_buffer
* for a nice buffer implementation.
*
* Semantics:
*      - push is thread-safe and non-blocking.
*      - pop is thread-safe an blocking.

*      - pop_non_blocking is thread-safe and....
*      - empty is thread-safe non blocking
*

***********************************************/

template<class T>

class PC_queue {

public:
  typedef T   Type;

  typedef T* pType;


  PC_queue() {}

  virtual ~PC_queue() {}



  void push( pType element ) {

    RAIIMutex rc(m_queuecond);

    m_queue.push(element);

    if ( m_queue.size() != 0 ) m_queuecond.signal();

  }



  pType pop() {

    RAIIMutex rc(m_queuecond);

    if ( m_queue.size() == 0 ) m_queuecond.wait();

    Type element = m_queue.front();

    m_queue.pop();

    return element;

  }



  pType pop_non_blocking() {

    RAIIMutex rc(m_queuecond);

    if ( m_queue.size() == 0 ) MTHROW("Trying to pop from on empty queue.");

    pType element = m_queue.front();

    m_queue.pop();

    return element;

  }



  bool empty() {

    RAIIMutex rc(m_queuecond);

    return m_queue.size()==0;

  }

#ifdef ENABLE_TEST_UNIT
class Test : public Test_aclass<PC_queue> {
  public:
    void tests();
  };
#endif // ENABLE_TEST_UNIT


private:

  std::queue<pType> m_queue;

  Condition m_queuecond;

};


/////////////////// IMPLEMENTATION (I hate c++ template) ///////////////
#ifdef ENABLE_TEST_UNIT
template<class T>
void PC_queue<T>::Test::tests() {
  PC_queue<T> queue;
  T obj;
  T *pobj=NULL;

  TEST_ASSERT( queue.empty() );
  TEST_EXCEPTION_THROW( queue.pop_non_blocking() );
  TEST_EXCEPTION_NTHROW( queue.push( &obj ) );
  TEST_EXCEPTION_NTHROW ( queue.push( NULL ) );
  TEST_ASSERT( !queue.empty() );
  TEST_EXCEPTION_NTHROW( pobj = queue.pop_non_blocking() );
  TEST_ASSERT( pobj == &obj );
  TEST_EXCEPTION_NTHROW( pobj = queue.pop_non_blocking() );
  TEST_ASSERT( pobj ==  NULL );
  TEST_ASSERT( queue.empty() );
  TEST_EXCEPTION_THROW( queue.pop_non_blocking() );
  TEST_ASSERT( queue.empty() );
}
#endif // ENABLE_TEST_UNIT





#endif // PC_QUEUE_H

