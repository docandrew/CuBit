/* CuBit: no fork or exec; procmgr starts the program (src/cubit/process.c).
 * posix_spawnp (musl's) reaches here too: names are procmgr's, no PATH. */
#include <spawn.h>
#include <errno.h>

int __cubit_spawn(pid_t *, const char *, char *const [], char *const []);

int posix_spawn(pid_t *restrict res, const char *restrict path,
	const posix_spawn_file_actions_t *fa,
	const posix_spawnattr_t *restrict attr,
	char *const argv[restrict], char *const envp[restrict])
{
	(void)attr;     /* no signals, groups or scheduler classes to apply */
	if (fa && fa->__actions) return ENOTSUP;
	return __cubit_spawn(res, path, argv, envp);
}
