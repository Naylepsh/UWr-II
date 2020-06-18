#include <netinet/ip.h>
#include <netinet/ip_icmp.h>
#include <arpa/inet.h>

const int MAX_SEGMENT_SIZE = 1000;
static const int MESSAGE_SIZE = 30;

class Datagram
{
public:
  unsigned int begin;
  unsigned int size;
  unsigned int end;
  bool is_acknowledged;
  char data[MAX_SEGMENT_SIZE];

  Datagram(unsigned int begin, unsigned int size);
  void get(int sockfd, struct sockaddr_in *address);
  void receive_data(char *data);
  bool operator==(const Datagram &datagram);
};