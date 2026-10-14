------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  @summary
--  The capability system calls, word for word (CAP-002). Private: only the
--  CuBit.Handles hierarchy sees handle words or these call numbers.
------------------------------------------------------------------------------
pragma Ada_2022;

private package CuBit.Handles.Kernel is

   --  Call numbers of every system call that takes or returns a handle
   --  (kernel Syscall.SyscallNumber). Nothing outside this hierarchy names
   --  them: a program reaches them only through typed handles.
   Kill                        : constant Unsigned_64 := 7;
   Send_Event                  : constant Unsigned_64 := 19;
   Map_Framebuffer             : constant Unsigned_64 := 29;
   In_8                        : constant Unsigned_64 := 30;
   Out_8                       : constant Unsigned_64 := 31;
   In_16                       : constant Unsigned_64 := 32;
   Out_16                      : constant Unsigned_64 := 33;
   In_32                       : constant Unsigned_64 := 36;
   Out_32                      : constant Unsigned_64 := 37;
   Submit_Via_Endpoint         : constant Unsigned_64 := 42;
   Move_Reply                  : constant Unsigned_64 := 51;
   Reply_And_Consume           : constant Unsigned_64 := 52;
   Spawn                       : constant Unsigned_64 := 60;
   Map_Device                  : constant Unsigned_64 := 70;
   Process_List                : constant Unsigned_64 := 71;
   Resume                      : constant Unsigned_64 := 73;
   Allocate_DMA                : constant Unsigned_64 := 74;
   Enable_IRQ                  : constant Unsigned_64 := 75;
   Map_Into                    : constant Unsigned_64 := 76;
   Set_CPU                     : constant Unsigned_64 := 78;
   Set_Latency_Contract        : constant Unsigned_64 := 81;
   Inspect                     : constant Unsigned_64 := 84;
   Create_Grant_Via_Capability : constant Unsigned_64 := 106;
   Acquire_Grant_Via_Capability : constant Unsigned_64 := 112;
   Derive_Grant_Via_Capability : constant Unsigned_64 := 122;
   Install_Launch_Arguments    : constant Unsigned_64 := 126;
   Send_Control                : constant Unsigned_64 := 127;
   Call_Via_Endpoint           : constant Unsigned_64 := 128;
   Send_Via_Endpoint           : constant Unsigned_64 := 129;
   Allocate_Device_Vectors     : constant Unsigned_64 := 130;
   Release_Device_Vectors      : constant Unsigned_64 := 131;
   Read_Device_Vector          : constant Unsigned_64 := 132;
   Boot_Timeline               : constant Unsigned_64 := 133;
   Close_Handle                : constant Unsigned_64 := 137;
   Install_Capability          : constant Unsigned_64 := 138;
   Reply_With_Capability       : constant Unsigned_64 := 139;
   Call_Offering_Capability    : constant Unsigned_64 := 140;
   Accept_Offered_Capability   : constant Unsigned_64 := 141;
   Reply_Saved_With_Capability : constant Unsigned_64 := 142;
   Derive_Capability           : constant Unsigned_64 := 143;
   Register_Driver             : constant Unsigned_64 := 2000;

   --  One system call with up to seven arguments (the seventh in R12, as
   --  the kernel's entry takes it).
   function Call
     (Number : Unsigned_64;
      A0, A1, A2, A3, A4, A5, A6 : Unsigned_64 := 0) return Unsigned_64;

   --  Handle words (the parent's private part, for the glue).
   function Word (Item : Endpoint) return Unsigned_64 is (Item.Word);
   function Word (Item : Reply) return Unsigned_64 is (Item.Word);
   function Word (Item : Process) return Unsigned_64 is (Item.Word);
   function Word (Item : Cspace) return Unsigned_64 is (Item.Word);
   function Word (Item : Device_Memory) return Unsigned_64 is (Item.Word);
   function Word (Item : IO_Ports) return Unsigned_64 is (Item.Word);
   function Word (Item : Interrupt) return Unsigned_64 is (Item.Word);
   function Word (Item : Notification) return Unsigned_64 is (Item.Word);
   function Word (Item : Scheduling) return Unsigned_64 is (Item.Word);
   function Word (Item : Foreign_Handle) return Unsigned_64 is (Item.Word);

   --  Handles from words the kernel gave this process (never from a
   --  number a program chose).
   function To_Endpoint (W : Unsigned_64) return Endpoint is ((Word => W));
   function To_Reply (W : Unsigned_64) return Reply is ((Word => W));
   function To_Process (W : Unsigned_64) return Process is ((Word => W));
   function To_Cspace (W : Unsigned_64) return Cspace is ((Word => W));
   function To_Device_Memory (W : Unsigned_64) return Device_Memory is ((Word => W));
   function To_IO_Ports (W : Unsigned_64) return IO_Ports is ((Word => W));
   function To_Interrupt (W : Unsigned_64) return Interrupt is ((Word => W));
   function To_Notification (W : Unsigned_64) return Notification is ((Word => W));
   function To_Scheduling (W : Unsigned_64) return Scheduling is ((Word => W));
   function To_Foreign (W : Unsigned_64) return Foreign_Handle is ((Word => W));

   function Derivation_Words (Item : Derivation) return ABI.Derivation is
     (Item.Words);

   --  The refusal a result word reports (Invalid_Argument for a word that
   --  is neither a handle nor a known code).
   function Refusal_Of (W : Unsigned_64) return Refusal is
     (if ABI.Is_Refusal_Word (W) then ABI.To_Refusal (W) else ABI.Invalid_Argument);

end CuBit.Handles.Kernel;
