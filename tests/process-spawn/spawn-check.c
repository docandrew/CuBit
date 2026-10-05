/*
 * Processes test parent (docs/process-arguments.md): starts children with
 * posix_spawn through procmgr's OP_LAUNCH, with arguments and environment,
 * and collects their exit status with waitpid. Markers go to the kernel
 * console; the headless test (tests/headless/run.sh --test processes)
 * checks them.
 */
#include <errno.h>
#include <spawn.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/wait.h>
#include <unistd.h>
#include <cubit/debug.h>

enum {
	EXIT_BASIC = 42, EXIT_MANY = 7, EXIT_ADA = 43, EXIT_RUST = 44, EXIT_CWD = 45,
	MANY_ARGUMENTS = 1000, CONCURRENT = 3, OVERSIZED_BYTES = 70000,
};

static int failures;

static void say(const char *text)
{
	cubit_debug_write(text, strlen(text));
}

static void check(int ok, const char *what)
{
	char line[160];
	snprintf(line, sizeof line, "spawn-check: %s %s\n", what,
		ok ? "PASS" : "FAIL");
	say(line);
	if (!ok) failures++;
}

static pid_t start(const char *program, char *const argv[], char *const envp[],
	int *error)
{
	pid_t pid = 0;
	*error = posix_spawn(&pid, program, 0, 0, argv, envp);
	return *error ? 0 : pid;
}

/* Wait for pid; its exit code, or -1 if it did not exit normally. */
static int exit_code(pid_t pid)
{
	int status = 0;
	if (waitpid(pid, &status, 0) != pid || !WIFEXITED(status)) return -1;
	return WEXITSTATUS(status);
}

int main(void)
{
	int error;
	char *basic[] = { "args-check.app", "basic", "two words", "", "--flag=x", 0 };
	char *environment[] = { "CUBIT_TEST=1", "EMPTY=", 0 };

	/* No children yet. */
	errno = 0;
	check(waitpid(-1, 0, 0) == -1 && errno == ECHILD,
		"waitpid without children is ECHILD");

	pid_t pid = start("args-check.app", basic, environment, &error);
	check(pid > 0 && exit_code(pid) == EXIT_BASIC,
		"argv, environment and exit status 42 reach parent");

	/* A leading '/' names the same program (procmgr names have no root). */
	pid = start("/args-check.app", basic, environment, &error);
	check(pid > 0 && exit_code(pid) == EXIT_BASIC, "absolute program name");

	/* Several children at once, collected in any order. */
	char *exit0[] = { "args-check.app", "exit", "0", 0 };
	char *exit255[] = { "args-check.app", "exit", "255", 0 };
	char *exit300[] = { "args-check.app", "exit", "300", 0 };
	char **lists[CONCURRENT] = { exit0, exit255, exit300 };
	const int want[CONCURRENT] = { 0, 255, 300 % 256 };
	pid_t pids[CONCURRENT];
	int seen = 0;
	for (int k = 0; k < CONCURRENT; k++)
		pids[k] = start("args-check.app", lists[k], 0, &error);
	for (int n = 0; n < CONCURRENT; n++) {
		int status;
		pid_t done = waitpid(-1, &status, 0);
		for (int k = 0; k < CONCURRENT; k++)
			if (done == pids[k] && pids[k] > 0 && WIFEXITED(status) &&
			    WEXITSTATUS(status) == want[k])
				seen |= 1 << k;
	}
	check(seen == (1 << CONCURRENT) - 1,
		"three children, exit codes 0, 255 and 300 (low 8 bits) via waitpid(-1)");

	/* WNOHANG while the child still runs, then its code. */
	char *sleeper[] = { "args-check.app", "sleep", "5", 0 };
	pid = start("args-check.app", sleeper, 0, &error);
	int status = 0;
	check(pid > 0 && waitpid(pid, &status, WNOHANG) == 0,
		"WNOHANG while the child runs");
	check(pid > 0 && exit_code(pid) == 5, "then its exit code 5");

	/* A process the kernel stops is reported as killed, not exited. */
	char *fault[] = { "args-check.app", "fault", 0 };
	pid = start("args-check.app", fault, 0, &error);
	status = 0;
	check(pid > 0 && waitpid(pid, &status, 0) == pid && WIFSIGNALED(status),
		"faulting child reported as stopped (WIFSIGNALED)");

	/* 1000 arguments. */
	static char *many[MANY_ARGUMENTS + 1];
	static char text[MANY_ARGUMENTS][24];
	many[0] = "args-check.app";
	many[1] = "many";
	for (int k = 2; k < MANY_ARGUMENTS; k++) {
		snprintf(text[k], sizeof text[k], "argument-%d", k);
		many[k] = text[k];
	}
	many[MANY_ARGUMENTS] = 0;
	pid = start("args-check.app", many, 0, &error);
	check(pid > 0 && exit_code(pid) == EXIT_MANY, "1000 arguments");

	/* Over the 64 KiB launch limit: refused before procmgr is asked. */
	static char huge[OVERSIZED_BYTES];
	memset(huge, 'x', sizeof huge - 1);
	char *oversized[] = { "args-check.app", huge, 0 };
	pid = start("args-check.app", oversized, 0, &error);
	check(pid == 0 && error == E2BIG, "oversized arguments are E2BIG");

	pid = start("no-such-program.app", basic, 0, &error);
	check(pid == 0 && error == ENOENT, "missing program is ENOENT");

	/* Launch authority: only names this program's manifest declares, and
	 * never a child holding more than this program does. */
	char *other[] = { "logstore.svc", 0 };
	pid = start("logstore.svc", other, 0, &error);
	check(pid == 0 && error == EACCES,
		"a program the manifest does not name is EACCES (Not_Granted)");
	char *greedy[] = { "greedy-check.app", "basic", 0 };
	pid = start("greedy-check.app", greedy, 0, &error);
	check(pid == 0 && error == EACCES,
		"a child asking for more authority than its launcher is EACCES");

	/* One way to start a program: no fork, exec or shell. */
	errno = 0;
	check(fork() == -1 && errno == ENOSYS, "fork is ENOSYS");
	errno = 0;
	check(execve("args-check.app", basic, environment) == -1 &&
		errno == ENOSYS, "execve is ENOSYS");
	errno = 0;
	check(system("args-check.app") == -1 && errno == ENOSYS,
		"system is ENOSYS");
	errno = 0;
	check(popen("args-check.app", "r") == 0 && errno == ENOSYS,
		"popen is ENOSYS");

	/* Other runtimes: Ada.Command_Line and Rust std::env::args. */
	char *ada[] = { "ada-args-check.app", "alpha", "two words", "", 0 };
	pid = start("ada-args-check.app", ada, environment, &error);
	check(pid > 0 && exit_code(pid) == EXIT_ADA,
		"Ada.Command_Line arguments and Set_Exit_Status 43");
	char *rust[] = { "rust-args-check.app", "alpha", "two words", "", 0 };
	pid = start("rust-args-check.app", rust, environment, &error);
	check(pid > 0 && exit_code(pid) == EXIT_RUST,
		"Rust std::env::args, env::var and exit code 44");

	/* The working directory, last (it then follows every child). A
	 * process starts with none chosen, so children get none; one chosen
	 * by chdir is passed on, and procmgr refuses a child that may not
	 * read it. chdir refuses a directory the process may not read. */
	char *cwd_root[] = { "args-check.app", "cwd", "/", 0 };
	pid = start("args-check.app", cwd_root, 0, &error);
	check(pid > 0 && exit_code(pid) == EXIT_CWD,
		"no working directory chosen: a child starts at /");
	char here[64];
	check(chdir("/tls") == 0 && getcwd(here, sizeof here) &&
	      strcmp(here, "/tls") == 0, "chdir and getcwd");
	char *cwd_tls[] = { "args-check.app", "cwd", "/tls", 0 };
	pid = start("args-check.app", cwd_tls, 0, &error);
	check(pid > 0 && exit_code(pid) == EXIT_CWD,
		"after chdir, a child starts there (/tls)");
	pid = start("ada-args-check.app", ada, environment, &error);
	check(pid == 0 && error == EACCES,
		"a child that may not read the working directory is refused (EACCES)");
	check(chdir("..") < 0 && errno == EACCES && getcwd(here, sizeof here) &&
	      strcmp(here, "/tls") == 0,
	      "chdir to a directory it may not read is refused, cwd unchanged");

	errno = 0;
	check(waitpid(-1, 0, WNOHANG) == -1 && errno == ECHILD,
		"all children collected");
	say(failures ? "PROCESS-SPAWN: FAIL\n" : "PROCESS-SPAWN: PASS\n");
	return failures ? 1 : 0;
}
