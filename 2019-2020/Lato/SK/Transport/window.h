#include <list>
#include "datagram.h"

#define MIN(a,b) (((a)<(b))?(a):(b))

static const unsigned int MAX_WINDOW_SIZE = 500;

class SlidingSenderWindow
{
  std::size_t size;

public:
  std::list<Datagram> datagrams;

public:
  SlidingSenderWindow(std::size_t size);
  void send_datagrams(int sockfd, struct sockaddr_in *address);
  void receive_datagram(unsigned int start, unsigned int size, char *buffer);
  void update();

private:
  bool must_be_enlarged();
  void enlarge_window();
};