/* Diagnostic fixture only. Adapter callbacks hold the Mesa lifetime mutex.
 * Capture once without allocation/IPC; emit only after returning to the probe.
 * Operation names must be static strings, never caller-owned memory. */
static unsigned transport_failure_state;
static const char *transport_failure_operation;
static uint32_t transport_failure_status, transport_failure_handle;
void cubit_test_mesa_transport_failure(const char *, uint32_t, uint32_t);
void cubit_test_mesa_transport_failure(const char *operation, uint32_t status,
                                      uint32_t handle)
{
   unsigned empty = 0;
   if (!__atomic_compare_exchange_n(&transport_failure_state, &empty, 1, false,
                                   __ATOMIC_RELAXED, __ATOMIC_RELAXED))
      return;
   transport_failure_operation = operation;
   transport_failure_status = status;
   transport_failure_handle = handle;
   __atomic_store_n(&transport_failure_state, 2, __ATOMIC_RELEASE);
}
static void report_transport_failure(void)
{
   unsigned ready = 2;
   if (!__atomic_compare_exchange_n(&transport_failure_state, &ready, 3, false,
                                   __ATOMIC_ACQUIRE, __ATOMIC_RELAXED))
      return;
   report("MESA-TRANSPORT first-failure operation=%s status=%u handle=%u\n",
          transport_failure_operation, transport_failure_status,
          transport_failure_handle);
}
