/* CuBit: there is no shell. Programs are started with posix_spawn, by a
 * name the caller's manifest declares (docs/process-arguments.md). */
#include <stdlib.h>
#include <errno.h>

int system(const char *cmd)
{
	/* POSIX: system(NULL) asks whether a command processor exists. */
	if (!cmd) return 0;
	errno = ENOSYS;
	return -1;
}
