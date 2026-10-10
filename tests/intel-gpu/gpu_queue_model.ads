with Interfaces; use Interfaces;
with System;
with CuBit.GPU_Queues;
with Intel_GPU_GuC_Submission_Policy;
with Intel_GPU_Ring_Reservation;
-- A Linux-hosted model of the GPU, the GuC and the driver's selection for
-- the GPU-001 step 2 queue service tests. Not hardware evidence.
--
-- Each context executes its published jobs in ring order, Latency turns
-- each, once a kick has submitted them. The ring is modelled byte by byte:
-- every byte remembers the value of the segment that wrote it, and a write
-- over a byte whose segment's successor has not completed is an overwrite
-- of unretired work (Overwrites counts them; the tests require zero).
package GPU_Queue_Model is
   package Q renames CuBit.GPU_Queues;
   subtype Session_Id is Positive range 1 .. 2;
   Ring_Bytes : constant := 16_384;
   Microseconds_Per_Turn : constant := 1_000;
   Setup_Bytes : constant := 384;

   type Region is array (1 .. 2 * 4096) of Unsigned_8 with Alignment => 4096;
   type Region_Access is access all Region;
   Client_Regions, Server_Regions : array (Session_Id) of Region_Access;

   -- Knobs.
   Latency : Positive := 3;               -- turns per job
   Hung : Boolean := False;               -- the GPU stops progressing
   Regress : Boolean := False;            -- the timeline reads back lower
   Ahead : Boolean := False;              -- the timeline reads past what was published
   Enable_Delay : Natural := 0;           -- turns before MODE_DONE follows an enable
   Torn : Boolean := False;               -- every read is unstable
   Ownership : Boolean := True;
   Backpressure_Every : Natural := 0;     -- every Nth kick is refused for now
   Execute_Bytes : Unsigned_32 := 384;
   Signal_Bytes : Unsigned_32 := 120;
   Unmapped_Handle : constant Unsigned_32 := 16#DEAD#;

   -- Observations.
   Overwrites, Bad_Tails, Kicks, Enables, Quarantines : Natural := 0;
   Calls_OK, Calls_Failed : Natural := 0;
   Last_Call_Value : Unsigned_64 := 0;
   Wakes_Answered : Natural := 0;
   Last_Wake : Q.Wake_Result := Q.Woken;
   Last_Wake_Session : Session_Id := 1;
   Quarantined : array (Session_Id) of Boolean := [others => False];
   Clock : Unsigned_64 := 1_000_000;

   procedure Reset;
   procedure Advance;   -- one turn of GPU time and the clock

   function Select_Context (S : Session_Id; C : Q.Context_Index) return Boolean;
   function Owner_Ready return Boolean;
   function Batch_Ready (Handle, GPU, Offset, Bytes : Unsigned_64) return Boolean;
   function Scheduling_Resident return Boolean;
   function Publish_Ready return Boolean;
   procedure Write_Segment
     (Operation : Q.Opcode; V, Batch : Unsigned_64;
      Plan : Intel_GPU_Ring_Reservation.Plan; Expected_Tail : Unsigned_32; OK : out Boolean);
   function Segment_Bytes (Operation : Q.Opcode) return Unsigned_32;
   procedure Kick (Enable : Boolean; Result : out Intel_GPU_GuC_Submission_Policy.Kick_Result);
   procedure Read_Timeline (V : out Unsigned_64; OK : out Boolean);
   function Now_Us return Unsigned_64;
   procedure Quarantine (S : Session_Id; Why : Q.Fault_Reason);
   procedure Call_Finished (S : Session_Id; V : Unsigned_64; OK : Boolean);
   procedure Answer_Wake (S : Session_Id; Result : Q.Wake_Result);
   function Client_Region (S : Session_Id) return System.Address;
   function Server_Region (S : Session_Id) return System.Address;

   function Completed (S : Session_Id; C : Q.Context_Index) return Unsigned_64;
end GPU_Queue_Model;
