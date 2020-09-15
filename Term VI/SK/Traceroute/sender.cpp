#include "sender.h"

u_int16_t ComputeIcmpChecksum (const void *buff, int length) {
    u_int32_t sum;
    const u_int16_t* ptr = (const u_int16_t *)buff;
    assert (length % 2 == 0);
    for (sum = 0; length > 0; length -= 2)
        sum += *ptr++;
    sum = (sum >> 16) + (sum & 0xffff);
    return (u_int16_t)(~(sum + (sum >> 16)));
}

icmp CreatePacketHeader(int id, int seq) {
    struct icmp header;
    header.icmp_type = ICMP_ECHO;
    header.icmp_code = 0;
    header.icmp_hun.ih_idseq.icd_id = id;
    header.icmp_hun.ih_idseq.icd_seq = seq;
    header.icmp_cksum = 0;
    header.icmp_cksum = ComputeIcmpChecksum ((u_int16_t*)&header, sizeof(header));
    return header;
}

PacketSender::PacketSender(int32_t sockfd, struct sockaddr_in& recipient) {
    this->sockfd = sockfd;
    this->recipient = recipient;
}

void PacketSender::SendMultiple(int ttl, int id, int seq_start, int how_many) {
    for (int i = 0; i < how_many; i++) {
        SendSingle(ttl, id, seq_start + i);
    }
}

void PacketSender::SendSingle(int ttl, int id, int seq) {
    struct icmp header = CreatePacketHeader(id, seq);

    int res = setsockopt(sockfd, IPPROTO_IP, IP_TTL, &ttl, sizeof(int));
    if (res == -1) {
        throw std::runtime_error("Couldn't handle setting a socket.");
    }
    ssize_t bytes_sent = sendto(sockfd, &header, sizeof(header), 0, (struct sockaddr*)&recipient, sizeof(recipient));
    if (bytes_sent == -1) {
        throw std::runtime_error("Couldn't handle sending a packet.");
    }
}
