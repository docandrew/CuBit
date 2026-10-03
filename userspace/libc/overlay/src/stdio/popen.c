/* CuBit: there is no shell (docs/process-arguments.md); pclose stays
 * musl's and never sees a stream from here. */
#include <stdio.h>
#include <errno.h>

FILE *popen(const char *cmd, const char *mode)
{
	(void)cmd;
	(void)mode;
	errno = ENOSYS;
	return 0;
}
