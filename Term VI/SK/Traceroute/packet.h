#include <chrono>
#include <ctime>
#include <vector>
#include <set>
#include <iostream>

class Packet {
    public:
    int id;
    int seq;
    char * ip_str;
    std::chrono::high_resolution_clock::time_point received_at;
    bool got_echo_reply;

    Packet();
    Packet(int id, int seq, char * ip_str, 
           std::chrono::high_resolution_clock::time_point received_at, bool got_echo_reply);
};

void PrintRoute(std::vector<Packet>& packets, std::chrono::high_resolution_clock::time_point sent_at, 
std::ostream& os, const unsigned long max_packets_per_request);