pragma Ada_2022;
with Interfaces;

--  The manifest request for the VM creation authority (Kernel_Object
--  VM_Create, VMM-001, docs/hypervisor.md): a request, never a grant.
--  procmgr is the root issuer of VM creation: it installs a copy with Write
--  only when the program's startup entry approves it, (vm-create
--  approve-declared), and only for a program the trusted startup plan
--  started. No shipped manifest declares it yet. Its startup entry has kind
--  VM_Create; the program reads it with CuBit.Handles.Startup.VM_Create_Named
--  (Binding).
package CuBit.VM_Create_Authority with Pure, SPARK_Mode is
   --  Its .cubit.caps request kind (after the test console object's 23).
   Manifest_Request : constant Interfaces.Unsigned_8 := 24;
   --  Its rights field in .cubit.caps: Write_Only (the encoding's 2).
   Manifest_Rights : constant Interfaces.Unsigned_8 := 2;
   --  The startup name a program looks for.
   Binding : constant String := "vm-create";
end CuBit.VM_Create_Authority;
