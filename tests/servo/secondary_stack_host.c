/* Actual CuBit GNAT stack/runtime objects on Linux; no kernel syscall path. */
#include <assert.h>
#include <stdint.h>
#include <stdio.h>
extern void * __wrap___gnat_get_secondary_stack(void);
extern void servo_stack_testinit(void);
extern uint32_t cubit_servo_stack_check(void);
int main(void) {
    /* Getter must initialize without depending on binder elaboration. */
    void *first = __wrap___gnat_get_secondary_stack();
    assert(first && (uintptr_t)first % 16 == 0);
    assert(cubit_servo_stack_check() == 1);
    servo_stack_testinit();
    for (unsigned i = 0; i < 1000; ++i) {
        assert(__wrap___gnat_get_secondary_stack() == first);
        assert(cubit_servo_stack_check() == 1);
    }
    puts("SERVO-SECONDARY-STACK: PASS pre-elaboration init, stable aligned storage, 1000 native-runtime mark/allocate/release cycles");
}
