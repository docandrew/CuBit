#pragma once
#include <stdbool.h>
#include <stdint.h>

/* Adapter values, NOT a C overlay of CuBit's native IPC message structure.
 * Transport copies fields explicitly and returns false on IPC failure.
 * All calls must use the same lifetime-pinned authorized endpoint; no
 * reconnect/retry between identity, topology and clock (mixed devices unsafe).
 */
struct cubit_gpu_query_message {
   uint32_t label;
   uint8_t length, flags;
   uint16_t reserved;
   uint64_t words[4];
};
typedef bool (*cubit_gpu_query_call)(void *endpoint,
                                    const struct cubit_gpu_query_message *request,
                                    struct cubit_gpu_query_message *reply);
struct cubit_gpu_vm_contract {
   uint64_t address_space_size;
   bool private_context;
};
/* Live admitted-session observation, not PCI defaults or backing capacity.
 * Validated v1 contract is private raw48 PPGTT. Output clears on failure.
 * The endpoint must remain pinned; later operations still check authority. */
bool cubit_gpu_query_vm(cubit_gpu_query_call call, void *endpoint,
                        struct cubit_gpu_vm_contract *out);
enum cubit_gpu_memory_contract {
   CUBIT_GPU_MEMORY_UNAVAILABLE = 0,
   CUBIT_GPU_MEMORY_OWNED_WB_EXPLICIT = 1,
   CUBIT_GPU_MEMORY_OWNED_WB_COHERENT = 2,
};
/* Owned RAM only, never aperture/imported memory. Same pinned endpoint as
 * allocation; does not reserve memory or replace synchronization. Failure
 * clears output so stale coherence cannot survive an unsuccessful query. */
bool cubit_gpu_query_memory(cubit_gpu_query_call call, void *endpoint,
                            enum cubit_gpu_memory_contract *out);
struct cubit_gpu_device_snapshot {
   uint16_t device;
   uint8_t pci_revision, dss_mask;
   uint16_t eu_mask;
};

/* Metadata only. Success does NOT mean a Vulkan device may be exposed.
 * Memory, timestamps, KMD, VM/queue/sync remain required.
 * Output unchanged on failure; transport ownership is caller responsibility.
 */
bool cubit_gpu_query_device(cubit_gpu_query_call call, void *endpoint,
                            struct cubit_gpu_device_snapshot *out);
/* Retained CS clock only; same pinned endpoint/ownership as device query.
 * Failure leaves output unchanged. Does not imply timestamp-read support. */
bool cubit_gpu_query_timestamp(cubit_gpu_query_call call, void *endpoint,
                               uint32_t *hz);

/* Shared backing pool observation, never a reservation or per-client quota.
 * Failure clears all fields. Closing buffers currently reclaims neither bytes
 * nor tickets. Zero tickets means no allocations even with free byte space. */
struct cubit_gpu_budget_snapshot {
   uint64_t total, retained, available, max_allocation;
   uint32_t unused_tickets;
};
bool cubit_gpu_query_budget(cubit_gpu_query_call call, void *endpoint,
                            struct cubit_gpu_budget_snapshot *out);

/* Caller owns this slot for the entire query sequence, without slot
 * replacement by another thread. No process ID lookup or implicit grants. */
struct cubit_gpu_native_endpoint { uint64_t slot; };
bool cubit_gpu_native_query_call(void *endpoint,
                                 const struct cubit_gpu_query_message *request,
                                 struct cubit_gpu_query_message *reply);

struct intel_device_info;
/* Populate Mesa defaults + measured topology and retained CS clock, leaving KMD INVALID.
 * Runs upstream topology/stepping finalization, but NOT exposable: provider
 * still needs memory, KMD, VM/queue/sync and timestamp-counter support.
 * Output unchanged on failure, including Mesa force-probe denial.
 */
bool cubit_mesa_query_device_defaults(cubit_gpu_query_call call, void *endpoint,
                                      struct intel_device_info *out);
/* Factory input: measured defaults plus authenticated native VM contract.
 * Sets context isolation and GPU VA capacity, not backing capacity. Still
 * leaves KMD INVALID; memory/queue/sync admission and factory remain required.
 * Output unchanged on failure. Same lifetime-pinned endpoint for all calls. */
bool cubit_mesa_query_runtime_device(cubit_gpu_query_call call, void *endpoint,
                                     struct intel_device_info *out);
