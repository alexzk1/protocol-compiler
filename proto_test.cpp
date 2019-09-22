#include <cerrno>
#include <iostream>
#include <sstream>
#include <sys/socket.h>
#include <arpa/inet.h>
#include <unistd.h>
#include "demo.h"

#define	IP(a,b,c,d)	(((a) << 24) | ((b) << 16) | ((c) << 8) | (d))

using namespace protocol::demo;

int main(int argc, char** argv)
{
    int s = socket(AF_INET, SOCK_DGRAM, 0);

    if (-1 != s) {
	sockaddr_in in;

	in.sin_family = AF_INET;
	in.sin_port = htons(6805);
	in.sin_addr.s_addr = htonl(IP(131,225,120,73));

	for (size_t ii = 100000; ii > 0; --ii) {
	    message::Chat msg;

	    msg.stamp.reset(new int64_t(ii - 1));
	    msg.sender = "Rich";
	    msg.receiver = "Charlie";
	    msg.msg = argc == 1 ? "Was that you!?!" : argv[1];
	    msg.salary.reset(new double(10.74));

	    std::ostringstream os;

	    msg.marshal(os);

	    std::string const buffer(os.str());

	    if (-1 == sendto(s, buffer.data(), buffer.size(), 0, (sockaddr*) &in, sizeof(in)))
		std::cout << "error sending packet -- " << errno << "\n";
	    else {
		char buf[8192];

		ssize_t const len = recv(s, buf, sizeof(buf), 0);

		if (len == -1)
		    std::cout << "error receiving packet -- " << errno << "\n";
		else {
		    std::istringstream is(std::string(buf, buf + len));

		    message::Base::Ptr it = message::Base::unmarshal(is);

		    {
			message::Chat const* msgPtr = dynamic_cast<message::Chat const*>(it.get());

			if (msgPtr)
			    ;
#if 0
			    std::cout << "Chat { stamp: " << (msgPtr->stamp.get() ? *msgPtr->stamp : 0) << ", sender: \"" <<
				msgPtr->sender << "\", receiver: \"" << msgPtr->receiver <<
				"\", msg: \"" << msgPtr->msg << "\", salary: $" << (msgPtr->salary.get() ? *msgPtr->salary : 0.0) <<
				" }\n";
#endif
		    }
		    {
			message::Ack const* ackPtr = dynamic_cast<message::Ack const*>(it.get());

			if (ackPtr)
			    std::cout << "Ack { }\n";
		    }
		}
	    }
	}
	close(s);
    } else
	std::cout << "couldn't open socket\n";

    return 0;
}
