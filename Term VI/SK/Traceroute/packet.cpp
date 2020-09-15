#include "packet.h"

Packet::Packet() {
    this->id = -1;
    this->seq = -1;
    this->ip_str = nullptr;
    this->received_at = std::chrono::high_resolution_clock::now();
}

Packet::Packet(int id, int seq, char * ip_str, 
    std::chrono::high_resolution_clock::time_point time, bool got_echo_reply) {
        
    this->id = id;
    this->seq = seq;
    this->ip_str = ip_str;
    this->received_at = time;
    this->got_echo_reply = got_echo_reply;
}

