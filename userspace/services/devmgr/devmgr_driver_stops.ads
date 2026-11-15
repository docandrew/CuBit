pragma Ada_2022;
with Interfaces; use Interfaces;
with CuBit.Process_IDs; use CuBit.Process_IDs;
with CuBit.System_Stops;

--  The root stop's driver stage (PWR-003, CuBit.System_Stops): procmgr asks
--  devmgr, as the drivers' supervisor, to stop the children it started.
--  devmgr sends each Stop (a control event; a driver quiesces its DMA on
--  its normal stop path), answers procmgr at once, takes their exits as
--  they come, and ends any still running when the grace has passed. It
--  never stops procmgr, and it starts nothing afterwards (devmgr refuses
--  further launches once Stopping). procmgr polls Phase and the counts.
package Devmgr_Driver_Stops is
   package STP renames CuBit.System_Stops;

   --  How long an ended child has for its exit report to arrive.
   End_Deadline_Ms : constant := 2_000;

   --  Stop every child devmgr started except Except (procmgr) and, with
   --  Keep_Tail, those whose launch it declared Power_Tail
   --  (Devmgr_Launches.Power_Tail). Asked: how many were sent Stop.
   procedure Begin_Stop
     (Grace : STP.Grace_Ms; Except : Process_ID; Keep_Tail : Boolean;
      Now : Unsigned_64; Asked : out Natural; Receipt : out STP.Driver_Receipt);

   --  Take the exits that arrived; at the grace end the rest; at the end
   --  deadline finish.
   procedure Step (Now : Unsigned_64);

   function Phase return STP.Driver_Phase;
   function Stopping return Boolean is (STP."=" (Phase, STP.Stopping));
   --  Stopped clean, exited failing, forced, still running (STP.Counts_Word).
   function Counts_Word return Unsigned_64;
   --  When devmgr must look again (0: never).
   function Wake_At return Unsigned_64;
end Devmgr_Driver_Stops;
