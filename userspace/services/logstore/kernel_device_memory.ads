pragma Ada_2022;
with CuBit.Log_Records;

--  The kernel's device-memory diagnostics (Device_Memory_Publication) as log
--  records: each refused request becomes a Warning starting with
--  "kernel: device-memory denied", the boot classification Information
--  records starting with "kernel: physical map". Logstore calls Poll every
--  turn of its loop (at least once a second); it reads the page the kernel
--  mapped into it at registration, never waits and never calls the kernel.
--  Each record is also echoed to the debug console.
generic
   with procedure Note (Text : String; Level : CuBit.Log_Records.Severity);
package Kernel_Device_Memory is
   procedure Poll;
end Kernel_Device_Memory;
