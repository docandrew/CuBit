/* CuBit: children's exits arrive as kernel events (src/cubit/process.c).
 * Resource usage is not tracked: ru is zeroed. */
#define _GNU_SOURCE
#include <sys/wait.h>
#include <sys/resource.h>

pid_t __cubit_wait4(pid_t, int *, int, struct rusage *);

pid_t wait4(pid_t pid, int *status, int options, struct rusage *ru)
{
	return __cubit_wait4(pid, status, options, ru);
}
