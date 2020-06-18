// Sebastian Kondraciuk 298451
#include <iostream>
#include "window.h"

SlidingSenderWindow::SlidingSenderWindow(std::size_t size)
{
  this->size = size;

  unsigned int i = 0;
  while (i < MAX_WINDOW_SIZE && must_be_enlarged())
  {
    enlarge_window();
    i++;
  }
}

bool SlidingSenderWindow::must_be_enlarged()
{
  if (this->datagrams.size() == 0)
  {
    return true;
  }
  auto lastDatagram = this->datagrams.back();
  return lastDatagram.end < this->size;
}

void SlidingSenderWindow::enlarge_window()
{
  unsigned int begin = this->datagrams.size() == 0 ? 0 : this->datagrams.back().end;
  unsigned int size = MIN(this->size - begin, MAX_SEGMENT_SIZE);

  Datagram datagram(begin, size);
  this->datagrams.push_back(datagram);
}

void SlidingSenderWindow::send_datagrams(int sockfd, struct sockaddr_in *address)
{
  for (auto datagram : this->datagrams)
  {
    if (!datagram.is_acknowledged)
    {
      datagram.get(sockfd, address);
    }
  }
}

void SlidingSenderWindow::update()
{
  if (this->datagrams.front().is_acknowledged)
  {
    this->datagrams.pop_front();
    if (!this->datagrams.empty() && must_be_enlarged())
    {
      enlarge_window();
    }
  }
}

void SlidingSenderWindow::receive_datagram(unsigned int start, unsigned int size, char *buffer)
{
  Datagram received(start, size);
  for (auto datagram = datagrams.begin(); datagram != datagrams.end(); datagram++)
  {
    if (*datagram == received && !datagram->is_acknowledged)
    {
      datagram->receive_data(buffer);
      break;
    }
  }
}
