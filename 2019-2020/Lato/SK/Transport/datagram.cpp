// Sebastian Kondraciuk 298451
#include "datagram.h"
#include <cstdio>
#include <cstring>
#include <stdexcept>
#include <algorithm>

Datagram::Datagram(unsigned int begin, unsigned int size)
{
  this->begin = begin;
  this->size = size;
  this->end = begin + size;
  this->is_acknowledged = false;
}

void Datagram::get(int sockfd, struct sockaddr_in *address)
{
  std::string message = "GET " + std::to_string(begin) + " " + std::to_string(size) + "\n";
  auto bytes_sent = sendto(sockfd, message.c_str(), message.length(), 0, (struct sockaddr *)address, sizeof(*address));
  if (bytes_sent == -1)
  {
    throw std::runtime_error("Unexpected error while sending a segment");
  }
}

void Datagram::receive_data(char *data)
{
  std::copy(data, data + this->size, this->data);
  this->is_acknowledged = true;
}

bool Datagram::operator==(const Datagram &datagram)
{
  return this->begin == datagram.begin && this->size == datagram.size;
}
