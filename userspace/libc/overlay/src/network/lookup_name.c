/*
 * CuBit libc: host name lookup (replaces musl's lookup_name.c).
 *
 * CuBit programs do not resolve names themselves: netstack resolves a name
 * when a connection is opened, inside the program's network scope (the
 * scope names hosts and allows or denies DNS). So getaddrinfo keeps the
 * name: numeric addresses and "localhost" are literal; any other name gets a
 * placeholder IPv4 address that stands for it (net.c), and connecting to that
 * address opens "@net:tcp:<name>:<port>". No resolver files, no DNS traffic
 * from the program.
 */
#include <netdb.h>
#include <string.h>
#include <strings.h>
#include <sys/socket.h>
#include "lookup.h"

hidden uint32_t __cubit_name_address(const char *name);

int __lookup_name(struct address buf[static MAXADDRS], char canon[static 256],
	const char *name, int family, int flags)
{
	*canon = 0;
	if (name) {
		size_t l = strnlen(name, 255);
		if (l - 1 >= 254) return EAI_NONAME;
		memcpy(canon, name, l + 1);
	}
	if (family != AF_UNSPEC && family != AF_INET && family != AF_INET6)
		return EAI_FAMILY;

	/* No name: the wildcard (passive) or loopback address. */
	if (!name) {
		memset(&buf[0], 0, sizeof buf[0]);
		buf[0].family = AF_INET;
		if (!(flags & AI_PASSIVE)) {
			static const uint8_t lo[4] = { 127, 0, 0, 1 };
			memcpy(buf[0].addr, lo, 4);
		}
		return 1;
	}

	int cnt = __lookup_ipliteral(buf, name, family);
	if (cnt) return cnt;
	if (flags & AI_NUMERICHOST) return EAI_NONAME;
	if (family == AF_INET6) return EAI_NONAME;      /* names are IPv4 placeholders */

	memset(&buf[0], 0, sizeof buf[0]);
	buf[0].family = AF_INET;
	if (!strcasecmp(name, "localhost") || !strcasecmp(name, "localhost.")) {
		static const uint8_t lo[4] = { 127, 0, 0, 1 };
		memcpy(buf[0].addr, lo, 4);
		return 1;
	}
	uint32_t a = __cubit_name_address(name);   /* network byte order */
	if (!a) return EAI_MEMORY;
	memcpy(buf[0].addr, &a, 4);
	return 1;
}
