#ifndef DATA_WRITER_TCP_H
#define DATA_WRITER_TCP_H

#include <Data_writer.h>

#include <vector>

class Data_writer_tcp : public Data_writer {
public:
  Data_writer_tcp(int port = 2630);
  void open_connection();

  //Data_writer_tcp(UINT64 *ip_addr, int nAddr, unsigned short int port);
  ~Data_writer_tcp();
  
  unsigned int get_port();  

  void flush();
private:
  UINT64 do_put_bytes(UINT64 nBytes, char *buff);

  int connection_socket, socket;
  int port;
};

#endif // DATA_WRITER_TCP_H
