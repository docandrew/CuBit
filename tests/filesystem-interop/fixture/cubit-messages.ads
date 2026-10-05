with Interfaces; use Interfaces;
package CuBit.Messages is
   type MessageTag is record
      label : Unsigned_32;
      length, flags : Unsigned_8;
      reserved : Unsigned_16;
   end record;
   type MessageWords is array (0 .. 3) of Unsigned_64;
   type Message is record
      tag : MessageTag;
      authorityTag : Unsigned_64 := 0;
      words : MessageWords;
   end record;
   NULL_MESSAGE : constant Message := ((0, 0, 0, 0), 0, [others => 0]);
   function capCall (slot : Unsigned_64; msg : in out Message) return MessageTag;
   procedure debugPrint (value : String);
   type Bytes is array (Natural range <>) of Unsigned_8;
   Grant_Buffer : Bytes (0 .. 4095) := [others => 0];
   procedure Open_Image (Path : String);
   procedure Close_Image;
   --  Test-only: owned-memory allocation (zero-filled, page aligned), as
   --  the kernel's SYSCALL_ALLOCATE_OWNED_MEMORY. Other calls return Last.
   SYSCALL_ALLOCATE_OWNED_MEMORY : constant Unsigned_64 := 115;
   --  The wall clock as the kernel publishes it (UTC ms at monotonic 0) and
   --  the monotonic ms clock. Hosted: the offset is CUBIT_TEST_WALL_CLOCK
   --  (UTC seconds) when set, else unknown (Last), and monotonic time is 0.
   SYSCALL_INFO : constant Unsigned_64 := 15;
   SYSCALL_GETTIME : constant Unsigned_64 := 27;
   SYSINFO_WALL_CLOCK_OFFSET : constant Unsigned_64 := 1403;
   function syscall
     (call : Unsigned_64; arg0 : Unsigned_64 := 0; arg1 : Unsigned_64 := 0;
      arg2 : Unsigned_64 := 0; arg3 : Unsigned_64 := 0;
      arg4 : Unsigned_64 := 0; arg5 : Unsigned_64 := 0) return Unsigned_64;
end CuBit.Messages;
