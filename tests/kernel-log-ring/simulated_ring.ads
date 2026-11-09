------------------------------------------------------------------------------
--  A kernel log ring in host memory: the kernel writer's steps
--  (kernel/src/kernel_log.adb, store) over the shared layout, a reader
--  through the shared drain generic, and what it delivered and dropped.
------------------------------------------------------------------------------
pragma Ada_2022;
with Interfaces; use Interfaces;
with Kernel_Log_Publication; use Kernel_Log_Publication;

package Simulated_Ring is
   procedure Reset (Capacity : Record_Capacity);

   --  One kernel line, as Kernel_Log.Append_Kernel_Line stores it.
   procedure Write (Text : String; Time_Ms : Unsigned_64 := 0);

   --  The kernel is overwriting record Number's slot: its Sequence says
   --  Writing (a reader must count Number as dropped).
   procedure Begin_Overwrite (Number : Record_Number);

   --  One reader step: Seen advances; the results are below.
   procedure Drain (Budget : Record_Capacity; Accounted : out Unsigned_64; More : out Boolean);

   Seen : Unsigned_64 := 0;

   Maximum_Results : constant := 8_192;
   type Number_List is array (1 .. Maximum_Results) of Unsigned_64;
   type Run is record
      First, Last : Unsigned_64 := 0;
   end record;
   type Run_List is array (1 .. Maximum_Results) of Run;

   Delivered       : Number_List := [others => 0];
   Delivered_Count : Natural := 0;
   Delivered_Text_Ok : Boolean := True;   --  each delivered text named its number
   Runs            : Run_List := [others => (0, 0)];
   Run_Count       : Natural := 0;

   function Total return Unsigned_64;
end Simulated_Ring;
