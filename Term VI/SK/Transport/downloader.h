#include "window.h"
#include <cstdio>

static const unsigned int MAX_RESPONSE_TIME = 100000;
static const unsigned int DATAGRAM_OFFSET = 7; // "DATA x y\n" has 7 chars excluding x and y

class Downloader
{
private:
  FILE *file;
  char *file_name;
  unsigned int file_size;
  unsigned short port;
  int sockfd;
  struct sockaddr_in address;
  unsigned int number_of_datagrams_to_send;
  unsigned int lar; 
  SlidingSenderWindow window;
  bool download_finished;

public:
  Downloader(char *ip, unsigned short port, char *file_name, unsigned int file_size);
  void download_file();
  void receive_datagram();

private:
  void validate(char *ip);
  void validate_ip(char *ip);
  void initialize_file(char *file_name, unsigned int file_size);
  void initialize_socket(char *ip, unsigned short port);
  void progress_download();
  bool is_valid_datagram(struct sockaddr_in *received);
  bool load_data_from_datagram(char *buffer, unsigned int &start, unsigned int &size);
  bool is_ready_to_save();
  bool is_entire_file_saved();
  void save_to_file();
};
