CXX = g++
CXXFLAGS = -std=c++11 -Wall -Wextra

transport : main.o downloader.o window.o datagram.o
	$(CXX) $(CXXFLAGS) -o transport downloader.o main.o window.o datagram.o

main.o : main.cpp

datagram.o : datagram.cpp datagram.h

window.o : window.cpp window.h

downloader.o : downloader.cpp downloader.h

clean :
	rm -f main.o datagram.o window.o downloader.o

distclean : 
	rm -f transport main.o datagram.o window.o downloader.o