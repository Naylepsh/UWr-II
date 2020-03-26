#include "receiver.h"

PacketReceiver::PacketReceiver(int sockfd, fd_set descriptors) {
    this->sockfd = sockfd;
    this->descriptors = descriptors;
}

std::vector<Packet> PacketReceiver::ReceiveMultiple(int id, int seq, const unsigned long how_many) {
    std::vector<Packet> packets;
    struct timeval tv; tv.tv_sec = 1; tv.tv_usec = 0;
    while (packets.size() < how_many) {
        FD_ZERO(&this->descriptors);
        FD_SET(this->sockfd, &this->descriptors);
        int ready = select(sockfd+1, &descriptors, NULL, NULL, &tv);
        if (ready < 0) {
            throw std::runtime_error("Couldn't handle socket select.");
        } else if (ready == 0) {
            break;
        } else if (ready == 1) {
            Packet packet = ReceiveSingle();
            if (packet.id == id && (packet.seq / how_many) == (seq / how_many)) {
                packets.push_back(packet);
            }
        } else {
            throw std::runtime_error("This shouldn't have ever happened");
        }
    }
    return packets;
}

Packet PacketReceiver::ReceiveSingle() {
    struct sockaddr_in sender;
    socklen_t sender_len = sizeof(sender);
    u_int8_t buffer[IP_MAXPACKET];

    ssize_t packet_len = recvfrom(this->sockfd, buffer, IP_MAXPACKET, 0, (struct sockaddr*)&sender, &sender_len);
    std::chrono::high_resolution_clock::time_point received_at = std::chrono::high_resolution_clock::now();

    if (packet_len < 0) {
        throw std::runtime_error("Couldn't receive a packet from recvfrom.");
    }

    char ip_str[20];
    inet_ntop(AF_INET, &(sender.sin_addr), ip_str, sizeof(ip_str));

    struct ip* ip_header = (struct ip*) buffer;
    u_int8_t* icmp_packet = buffer + 4 * ip_header->ip_hl;
    struct icmp*	icmp_header = ( struct icmp* )icmp_packet;

    if(icmp_header->icmp_type == ICMP_ECHOREPLY) {
        bool got_echo_reply = true;
		return Packet(icmp_header->icmp_hun.ih_idseq.icd_id, 
                      icmp_header->icmp_hun.ih_idseq.icd_seq, 
                      ip_str, received_at, got_echo_reply);
	} else if (icmp_header->icmp_type == ICMP_TIME_EXCEEDED) {
        icmp_packet += 8;
        icmp_packet += 4 * ((struct ip*) icmp_packet)->ip_hl;
		struct icmp* icmp_time_ex = (struct icmp*) icmp_packet;
	    bool got_echo_reply = false;
        return Packet(icmp_time_ex->icmp_hun.ih_idseq.icd_id, 
                      icmp_time_ex->icmp_hun.ih_idseq.icd_seq, 
                      ip_str, received_at, got_echo_reply);
    } else {
        return Packet();
    }
}
