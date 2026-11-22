// Native C++ checks for the CuBit libc + libstdc++ (userspace/libc).
#include <algorithm>
#include <condition_variable>
#include <cstdio>
#include <iostream>
#include <map>
#include <memory>
#include <mutex>
#include <sstream>
#include <stdexcept>
#include <string>
#include <thread>
#include <vector>
#include <cstdarg>
#include <cubit/debug.h>

static int failures;

// Verdicts go to the debug console; stdout is the program's CuBit stream.
static void say(const char *fmt, ...)
{
	char buf[256];
	va_list ap;
	va_start(ap, fmt);
	int n = std::vsnprintf(buf, sizeof buf, fmt, ap);
	va_end(ap);
	if (n > (int)sizeof buf - 1) n = sizeof buf - 1;
	cubit_debug_write(buf, (size_t)n);
}

static void check(bool ok, const char *name)
{
	say("cxx-check: %s %s\n", name, ok ? "PASS" : "FAIL");
	if (!ok) failures++;
}

struct Counter {
	static int live;
	Counter() { live++; }
	~Counter() { live--; }
};
int Counter::live = 0;

static thread_local std::string per_thread = "main";

int main()
{
	say("cxx-check: hello from C++ on CuBit\n");
	std::cout << "cxx-check: to the stdout stream" << std::endl;
	check(bool(std::cout), "iostream writes to the CuBit stream");

	bool caught = false;
	try {
		auto guard = std::make_unique<Counter>();
		throw std::runtime_error("boom");
	} catch (const std::exception &e) {
		caught = std::string(e.what()) == "boom" && Counter::live == 0;
	}
	check(caught, "exceptions unwind and run destructors");

	std::mutex m;
	std::condition_variable cv;
	bool go = false;
	long total = 0;
	std::vector<std::thread> threads;
	std::vector<std::string> names(6);
	for (int i = 0; i < 6; i++) {
		threads.emplace_back([&, i] {
			per_thread = "t" + std::to_string(i);
			{
				std::unique_lock<std::mutex> l(m);
				cv.wait(l, [&] { return go; });
			}
			for (int k = 0; k < 10000; k++) {
				std::lock_guard<std::mutex> l(m);
				total++;
			}
			names[i] = per_thread;
		});
	}
	{
		std::lock_guard<std::mutex> l(m);
		go = true;
	}
	cv.notify_all();
	for (auto &t : threads) t.join();
	check(total == 60000, "std::thread, mutex, condition_variable");
	check(names[3] == "t3" && per_thread == "main", "thread_local std::string");

	std::map<std::string, int> counts;
	std::istringstream in("a b c a b a");
	for (std::string w; in >> w;) counts[w]++;
	check(counts["a"] == 3 && counts["b"] == 2, "streams and map");

	std::vector<int> v{5, 3, 9, 1};
	std::sort(v.begin(), v.end());
	check(v.front() == 1 && v.back() == 9, "algorithms");

	say("%s\n", failures ? "CXX: FAIL" : "CXX: PASS");
	return failures != 0;
}
