/*
 * Launch-argument child for the processes test (docs/process-arguments.md).
 * argv[1] selects a check; results go to the kernel console as markers and
 * in the exit status, which the parent (spawn-check.c) collects.
 */
#include <errno.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include <time.h>
#include <cubit/debug.h>

extern char **environ;

/* Exit codes the parent expects (spawn-check.c). */
enum {
	EXIT_BASIC = 42, EXIT_MANY = 7, EXIT_CWD = 45, EXIT_FAILED = 99,
	MANY_ARGUMENTS = 1000, SLEEP_MILLISECONDS = 300,
};

static void say(const char *text)
{
	cubit_debug_write(text, strlen(text));
}

static int count_environment(void)
{
	int n = 0;
	for (char **e = environ; e && *e; e++) n++;
	return n;
}

int main(int argc, char **argv)
{
	if (argc == 1 && strcmp(argv[0], "cubit-program") == 0 &&
	    count_environment() == 0) {
		/* Started by the startup profile: no launch block at all. */
		say("args-check: no launch block gives argv {cubit-program} PASS\n");
		return 0;
	}
	if (argc < 2) {
		say("args-check: FAIL no mode\n");
		return EXIT_FAILED;
	}
	const char *mode = argv[1];
	if (strcmp(mode, "basic") == 0) {
		const char *const want[] = { "args-check.app", "basic", "two words",
			"", "--flag=x" };
		int ok = argc == 5 && argv[argc] == NULL;
		for (int k = 0; ok && k < 5; k++)
			ok = strcmp(argv[k], want[k]) == 0;
		const char *test = getenv("CUBIT_TEST"), *empty = getenv("EMPTY");
		ok = ok && test && strcmp(test, "1") == 0 && empty && !*empty &&
			count_environment() == 2;
		/* POSIX lets a program write to its argument strings. */
		argv[2][0] = 'T';
		ok = ok && strcmp(argv[2], "Two words") == 0;
		say(ok ? "args-check: argv, environment and writable strings PASS\n"
		       : "args-check: FAIL basic\n");
		return ok ? EXIT_BASIC : EXIT_FAILED;
	}
	if (strcmp(mode, "many") == 0) {
		int ok = argc == MANY_ARGUMENTS;
		char want[32];
		for (int k = 2; ok && k < argc; k++) {
			snprintf(want, sizeof want, "argument-%d", k);
			ok = strcmp(argv[k], want) == 0;
		}
		say(ok ? "args-check: 1000 arguments PASS\n"
		       : "args-check: FAIL many\n");
		return ok ? EXIT_MANY : EXIT_FAILED;
	}
	if (strcmp(mode, "cwd") == 0 && argc == 3) {
		/* The launcher's working directory, from the launch block. */
		char here[256];
		int ok = getcwd(here, sizeof here) && strcmp(here, argv[2]) == 0;
		say(ok ? "args-check: working directory from the launcher PASS\n"
		       : "args-check: FAIL cwd\n");
		return ok ? EXIT_CWD : EXIT_FAILED;
	}
	if (strcmp(mode, "exit") == 0 && argc == 3)
		return atoi(argv[2]);
	if (strcmp(mode, "sleep") == 0 && argc == 3) {
		struct timespec t = { 0, SLEEP_MILLISECONDS * 1000000L };
		nanosleep(&t, 0);
		return atoi(argv[2]);
	}
	if (strcmp(mode, "fault") == 0) {
		volatile int *nothing = 0;
		*nothing = 1;           /* the kernel stops the process */
		return EXIT_FAILED;
	}
	/* Delegated places (delegate-check): may this child read argv[2]? */
	if ((strcmp(mode, "read") == 0 || strcmp(mode, "denied") == 0) && argc == 3) {
		FILE *f = fopen(argv[2], "r");
		char line[64] = { 0 };
		int readable = f && fgets(line, sizeof line, f) != 0;
		int want = strcmp(mode, "read") == 0;
		if (f) fclose(f);
		say(readable == want ? "args-check: delegated read as expected PASS\n"
		                     : "args-check: delegated read FAIL\n");
		return readable == want ? 0 : EXIT_FAILED;
	}
	say("args-check: FAIL unknown mode\n");
	return EXIT_FAILED;
}
