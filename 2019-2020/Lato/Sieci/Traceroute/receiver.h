#include <netinet/ip.h>
#include <netinet/ip_icmp.h>
#include <arpa/inet.h>
#include <cstdio>
#include <cerrno>
#include <cstring>
#include <vector>
#include "packet.h"

class PacketReceiver {
    int sockfd;
    fd_set descriptors;

    public:
    PacketReceiver(int sockfd, fd_set descriptors);

    std::vector<Packet> ReceiveMultiple(int id, int seq, const unsigned long how_many);
    Packet ReceiveSingle();
};