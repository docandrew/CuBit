/* Checked Ada test glue, linked without a hosted exception unwinder.
 * A failed check terminates the app; it never returns or resumes rendering. */
#include <stdlib.h>
#include <cubit/debug.h>
static _Noreturn void failed(void)
{
    static const char message[]="MESA-GALLERY Ada runtime check failed; terminating\n";
    cubit_debug_write(message,sizeof(message)-1);
    abort();
}
#define CHECK(name) _Noreturn void name(const char *file,int line) { (void)file; (void)line; failed(); }
CHECK(__gnat_rcheck_CE_Range_Check)
CHECK(__gnat_rcheck_CE_Overflow_Check)
CHECK(__gnat_rcheck_CE_Discriminant_Check)
CHECK(__gnat_rcheck_CE_Invalid_Data)
CHECK(__gnat_rcheck_CE_Index_Check)
CHECK(__gnat_rcheck_CE_Divide_By_Zero)
