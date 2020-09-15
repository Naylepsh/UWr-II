// Sebastian Kondraciuk 298451
#include "downloader.h"
#include <cstring>
#include <iostream>
#include <cmath>

Downloader::Downloader(
    char *ip, unsigned short port, char *file_name, unsigned int file_size) : window(file_size)
{
  validate_ip(ip);

  initialize_file(file_name, file_size);
  initialize_socket(ip, port);

  this->lar = 0;
  this->number_of_datagrams_to_send = (unsigned int)ceil((double) file_size / MAX_SEGMENT_SIZE);
}

void Downloader::validate_ip(char *ip)
{
  struct sockaddr_in recipient;
  bzero(&recipient, sizeof(recipient));
  recipient.sin_family = AF_INET;
  int is_valid_ip = inet_pton(AF_INET, ip, &(recipient.sin_addr));
  if (!is_valid_ip)
  {
    throw std::invalid_argument("Invalid ip address");
  }
}

void Downloader::initialize_file(char *file_name, unsigned int file_size)
{
  this->file_name = file_name;
  this->file_size = file_size;
  this->file = fopen(this->file_name, "w");
}

void Downloader::initialize_socket(char *ip, unsigned short port)
{
  this->port = port;
  this->sockfd = socket(AF_INET, SOCK_DGRAM, 0);
  memset(&address, 0, sizeof(address));
  address.sin_family = AF_INET;
  address.sin_port = htons(this->port);
  inet_pton(AF_INET, ip, &address.sin_addr);
}

void Downloader::download_file()
{
  this->download_finished = false;
  this->window.send_datagrams(this->sockfd, &(this->address));

  struct timeval tv; tv.tv_sec = 0; tv.tv_usec = MAX_RESPONSE_TIME;
  fd_set descriptors;
  while (!this->download_finished)
  {
    FD_ZERO(&descriptors);
    FD_SET(this->sockfd, &descriptors);

    int ready = select(this->sockfd + 1, &descriptors, NULL, NULL, &tv);
    if (ready == -1)
    {
      throw std::runtime_error("Unexpected error while selecting a socket");
    }
    else if (ready == 0)
    {
      this->window.send_datagrams(this->sockfd, &(this->address));
      this->window.send_datagrams(this->sockfd, &(this->address));
      tv.tv_sec = 0; tv.tv_usec = MAX_RESPONSE_TIME;
    }
    else
    {
      progress_download();
    }
  }
}

void Downloader::progress_download()
{
  receive_datagram();

  if (is_ready_to_save())
  {
    save_to_file();
    this->window.update();
    this->lar++;
  }

  if (is_entire_file_saved())
  {
    fclose(this->file);
    this->download_finished = true;
  }
}

bool Downloader::is_ready_to_save()
{
  return this->window.datagrams.front().is_acknowledged;
}

bool Downloader::is_entire_file_saved()
{
  return this->lar == this->number_of_datagrams_to_send;
}

void Downloader::save_to_file()
{
  auto datagram = this->window.datagrams.front();
  auto bytes_saved = fwrite(datagram.data, sizeof(char), datagram.size, this->file);
  if (bytes_saved != datagram.size)
  {
    throw std::runtime_error("Unexpected error during writting to file");
  }
}

bool Downloader::is_valid_datagram(struct sockaddr_in *sender)
{
  return (this->address.sin_port == sender->sin_port &&
          this->address.sin_addr.s_addr == sender->sin_addr.s_addr);
}

bool Downloader::load_data_from_datagram(char *buffer, unsigned int &start, unsigned int &size)
{
  int items_filled = sscanf((char *)buffer, "DATA %u %u\n", &start, &size);
  return items_filled == 2;
}

void Downloader::receive_datagram()
{
  struct sockaddr_in sender;
  socklen_t sock_len = sizeof(sender);
  char buffer[IP_MAXPACKET];
  ssize_t res_length = recvfrom(sockfd, buffer, IP_MAXPACKET, MSG_DONTWAIT, (struct sockaddr *)&sender, &sock_len);

  if (res_length < 0)
  {
    throw std::runtime_error("Couldn't receive a datagram from recvfrom");
  }

  if (is_valid_datagram(&sender))
  {
    unsigned int start;
    unsigned int received_size;
    bool loaded_properly = load_data_from_datagram((char *)buffer, start, received_size);
    if (loaded_properly)
    {
      unsigned int offset = DATAGRAM_OFFSET + std::to_string(start).length() + std::to_string(received_size).length();
      this->window.receive_datagram(start, received_size, (char *)buffer + offset);
    }
  }
}
