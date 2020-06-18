// Sebastian Kondraciuk 298451
#include <iostream>
#include "downloader.h"
#include <climits>

void validate_file_size(long long file_size)
{
  if (file_size < 0)
  {
    throw std::invalid_argument("File size cannot be negative");
  }
}

void validate_port(int port) {
  if (port < 1) {
    throw std::invalid_argument("Port has to be an integer greater than 0");
  }

  if (port > USHRT_MAX) {
    throw std::invalid_argument("Port has to be an integer lower than 65536");
  }
}

int main(int argc, char *argv[])
{
  using namespace std;

  try
  {
    if (argc != 5)
    {
      throw std::invalid_argument("Invalid number of arguments.\nUsage: ./transport ip port output_file bytes");
    }

    char *ip = argv[1];
    int port = stoi(argv[2]);
    char *file_name = argv[3];
    long long file_size = stoll(argv[4]);

    validate_port(port);
    validate_file_size(file_size);

    Downloader downloader(ip, (unsigned short)port, file_name, (unsigned int)file_size);
    downloader.download_file();
    return EXIT_SUCCESS;
  }
  catch (exception &e)
  {
    cerr << e.what() << endl;
    return EXIT_FAILURE;
  }
}
