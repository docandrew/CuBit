--  A fake four-slot block device behind CuBit.Block_Driver. The test
--  decides when each slot's command finishes and how it turns out.
with Interfaces; use Interfaces;
with CuBit.Block_Work; use CuBit.Block_Work;
with CuBit.Block_Driver;
package Test_Device is
   Block_Bytes : constant := 512;
   Disk_Blocks : constant := 64;
   Deadline : constant := 5_000;

   subtype Slot is Natural range 0 .. 3;

   type Bytes_View is array (Positive range <>) of Unsigned_8;
   Disk : Bytes_View (1 .. Block_Bytes * Disk_Blocks) := [others => 0];

   --  Per slot: what was posted, whether it is done, and how it went.
   Posted : array (Slot) of Boolean := [others => False];
   Work_Of : array (Slot) of Work_Order;
   Done : array (Slot) of Boolean := [others => False];
   Fail : array (Slot) of Boolean := [others => False];
   Short : array (Slot) of Boolean := [others => False];
   Cancelled : array (Slot) of Boolean := [others => False];
   Posts, Collects : Natural := 0;
   --  Commands finish inside Post (a synchronous device).
   Instant : Boolean := False;

   Notes : array (Driver_Event) of Natural := [others => 0];
   Last_Count : array (Driver_Event) of Unsigned_64 := [others => 0];

   procedure Post (Item : Slot; Work : Work_Order);
   function Finished (Item : Slot) return Boolean;
   procedure Collect (Item : Slot; Work : Work_Order; Outcome : out Work_Outcome);
   procedure Cancel (Item : Slot);
   procedure Note (Event : Driver_Event; Count : Unsigned_64);

   package Driver is new CuBit.Block_Driver
     (Slot => Slot, Writes => Write_Grant_Required,
      Deadline_Microseconds => Deadline, Post => Post,
      Finished => Finished, Collect => Collect, Cancel => Cancel, Note => Note);

   --  The same hardware, open to every client's writes (use one at a time).
   package Open_Driver is new CuBit.Block_Driver
     (Slot => Slot, Writes => Any_Client_Writes,
      Deadline_Microseconds => Deadline, Post => Post,
      Finished => Finished, Collect => Collect, Cancel => Cancel, Note => Note);
end Test_Device;
