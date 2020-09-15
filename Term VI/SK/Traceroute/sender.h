#include <netinet/ip.h>
#include <netinet/ip_icmp.h>
#include <arpa/inet.h>
#include <cassert>
#include <cstring>
#include <iostream>

class PacketSender {
    int32_t sockfd;
    struct sockaddr_in recipient;
    
    public:
    PacketSender(int32_t sockfd, struct sockaddr_in& recipient);
    void SendMultiple(int ttl, int id, int seq_start, int how_many);
    void SendSingle(int ttl, int id, int seq);
};
