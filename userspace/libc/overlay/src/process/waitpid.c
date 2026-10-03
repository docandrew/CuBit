/* CuBit: children's exits arrive as kernel events (src/cubit/process.c). */
#include <sys/wait.h>
#include <sys/resource.h>

pid_t __cubit_wait4(pid_t, int *, int, struct rusage *);

pid_t waitpid(pid_t pid, int *status, int options)
{
	return __cubit_wait4(pid, status, options, 0);
}
