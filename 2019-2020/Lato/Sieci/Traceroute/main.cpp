#include <stdio.h>
#include <unistd.h>
#include <exception>
#include "sender.h"
#include "receiver.h"

void DisplayRoute(
    std::vector<Packet>& packets, 
    std::chrono::high_resolution_clock::time_point sent_at, 
    std::ostream& os, const unsigned long max_packets_per_request) {
        
    using std::endl;
    using namespace std::chrono;
    std::set<char *> ip_addresses;
    for (auto packet : packets) {
        if (packet.id != -1) {
            ip_addresses.insert(packet.ip_str);
        }
    }

    if (ip_addresses.empty()) {
        os << "*" << endl;
        return;
    }

    for (char * ip_address : ip_addresses) {
        os << ip_address << " ";
    }

    if (packets.size() < max_packets_per_request) {
        os << "???" << endl;
        return;
    }

    unsigned int average_packet_time = 0;
    for (auto packet : packets) {
        average_packet_time += duration_cast<milliseconds>(packet.received_at - sent_at).count();
    }
    average_packet_time /= max_packets_per_request;
    os << average_packet_time << "ms" << endl;
}

int main(int argc, char *argv[]) {
    using namespace std;

    if (argc != 2) {
        cerr << "Invalid number of arguments (need exactly 1 - IP Adress)" << endl;
        return EXIT_FAILURE;
    }

    char * destination_ip = argv[1];

    struct sockaddr_in recipient;
    bzero (&recipient, sizeof(recipient));
    recipient.sin_family = AF_INET;
    int is_valid_ip = inet_pton(AF_INET, destination_ip, &(recipient.sin_addr));
    if (!is_valid_ip) {
        cerr << "Invalid IP" << endl;
        return EXIT_FAILURE;
    } 
    
    int seq = 0;
    const unsigned long PACKETS_PER_REQUEST = 3;
    int sockfd = socket(AF_INET, SOCK_RAW, IPPROTO_ICMP);
    PacketSender packet_sender = PacketSender(sockfd, recipient);
    fd_set fdset;
    int pid = getpid();
    PacketReceiver packet_receiver = PacketReceiver(sockfd, fdset);
    bool is_at_dest = false;
    for (int ttl = 1; ttl <= 30; ttl++) {
        try {
            packet_sender.SendMultiple(ttl, pid, seq, PACKETS_PER_REQUEST);
            chrono::high_resolution_clock::time_point sent_at = chrono::high_resolution_clock::now();

            vector<Packet> packets = packet_receiver.ReceiveMultiple(pid, seq, PACKETS_PER_REQUEST);
            seq += PACKETS_PER_REQUEST;

            cout << ttl << ". ";
            DisplayRoute(packets, sent_at, std::cout, PACKETS_PER_REQUEST);

            for (auto packet : packets) {
                if (packet.got_echo_reply) {
                    is_at_dest = true;
                }
            }
            if (is_at_dest) {
                break;
            }
        } catch (exception& e) {
            cerr << e.what() << endl;
            return EXIT_FAILURE;
        }
    }
    return EXIT_SUCCESS;
}
