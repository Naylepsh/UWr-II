CXX = g++
CXXFLAGS = -std=c++11 -Wall -Wextra

traceroute : main.o packet.o sender.o receiver.o
	$(CXX) $(CXXFLAGS) -o traceroute main.o packet.o sender.o receiver.o

main.o : main.cpp

packet.o : packet.cpp packet.h

sender.o : sender.cpp sender.h

receiver.o : receiver.cpp receiver.h

clean :
	rm -f main.o packet.o sender.o receiver.o

distclean : 
	rm -f traceroute main.o packet.o sender.o receiver.o