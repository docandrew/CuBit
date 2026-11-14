pragma Ada_2022;
with Interfaces; use Interfaces;
with CuBit.Channels;
with CuBit.Control_Events;
with CuBit.Log_Sources;
with CuBit.Messages;
private with Heap_Arrays;

--  Who each publisher is (MET-002): what the launchers' registrar notices
--  say of each process they start (CuBit.Log_Sources, the same notices
--  logstore takes, LOG-003), so a series carries its program's identity and
--  name as labels. procmgr (and later devmgr) open one registrar channel
--  each, producing, under the registrar tag only a launcher can stamp; a
--  process never names itself. Identity comes from the kernel-stamped
--  opener, every notice is decoded as untrusted.
--
--  Identities grow on the process heap up to Maximum_Sources; past it the
--  longest-exited is forgotten first (its series keep their labels).
package Metric_Sources is
   --  Registrar channels are numbered from here, apart from outlets'.
   First_Number : constant := 16#8000#;
   Maximum_Registrars : constant := 4;
   Batch_Notices : constant := 64;
   Maximum_Sources : constant := 4_096;

   type Table is limited private;

   procedure Open
     (T : in out Table; From : CuBit.Messages.Process_ID; Authority : Unsigned_64;
      Request : CuBit.Messages.Message; Reply : out CuBit.Messages.Message);
   function Owns (Number : Unsigned_64) return Boolean is
     (Number > First_Number and then Number <= First_Number + Maximum_Registrars);
   procedure Close (T : in out Table; From : CuBit.Messages.Process_ID; Request : CuBit.Messages.Message);
   procedure Ended (T : in out Table; Event : CuBit.Control_Events.Event);
   procedure Drain (T : in out Table; Pending : out Boolean);

   --  What the launcher said of Process (Known False: nothing yet).
   function Known (T : Table; Process : Unsigned_64) return Boolean;
   function Identity_Of (T : Table; Process : Unsigned_64) return String;
   function Program_Of (T : Table; Process : Unsigned_64) return String;
   --  Notices given up for want of memory.
   function Lost (T : Table) return Unsigned_64;
private
   type Registrar is record
      Owner : CuBit.Messages.Process_ID := CuBit.Messages.No_Process;
      Authority : Unsigned_64 := 0;
      Link : CuBit.Channels.Channel;
   end record;
   type Registrar_Array is array (1 .. Maximum_Registrars) of Registrar;
   type Known_Source is record
      Who : CuBit.Log_Sources.Source_Identity;
      Exited : Boolean := False;
      Order : Unsigned_64 := 0;
   end record;
   package Source_Blocks is new Heap_Arrays (Known_Source);
   type Table is limited record
      Registrars : Registrar_Array;
      Sources : Source_Blocks.Block := Source_Blocks.Empty;
      Used : Natural := 0;
      Order : Unsigned_64 := 0;
      Losses : Unsigned_64 := 0;
   end record;
end Metric_Sources;
