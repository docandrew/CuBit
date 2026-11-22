#include "../../../userspace/mesa/anv/native_gpu_presenter.h"
static struct cubit_presenter presenter;
uint32_t presenter_probe_open(cubit_endpoint render, cubit_endpoint desktop, uint64_t surface)
{
   return cubit_presenter_attach_completed_linear(&presenter, render,
      desktop, 1, surface, 4096, 32, 32, 128);
}
uint32_t presenter_probe_release(void)
{
   return cubit_presenter_release(&presenter);
}
