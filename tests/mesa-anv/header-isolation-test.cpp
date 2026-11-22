#include <cstdint>
#include <string>
#include <vector>
#include <atomic>
#include <pthread.h>
#include <sys/mman.h>
#if __has_include(<linux/types.h>)
#error Linux headers must not be reachable in the CuBit target probe
#endif
static_assert(sizeof(std::uint64_t) == 8);
std::string cubit_header_probe(const std::vector<unsigned>& input)
{
   std::atomic<unsigned> count{0};
   for (auto value : input) count.fetch_add(value);
   return std::to_string(count.load());
}
