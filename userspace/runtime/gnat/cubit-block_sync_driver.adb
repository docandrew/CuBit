------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
------------------------------------------------------------------------------
pragma Ada_2022;
with CuBit.Block_Driver;

package body CuBit.Block_Sync_Driver is

   type Slot is range 0 .. 0;
   Last_Outcome : Work_Outcome;

   --  Commands finish inside Post, so no deadline is ever reached; this is
   --  the limit Poll would apply if one were outstanding.
   Command_Microseconds : constant := 1_000_000;

   procedure Post (Item : Slot; Work : Work_Order);
   function Finished (Item : Slot) return Boolean;
   procedure Collect (Item : Slot; Work : Work_Order; Outcome : out Work_Outcome);

   procedure Post (Item : Slot; Work : Work_Order) is
      pragma Unreferenced (Item);
      Durable : Boolean;
      Moved : Unsigned_64;
   begin
      case Work.Kind is
         when Cache_Flush =>
            Flush (Durable);
            Last_Outcome := (Passed => Durable, Moved => 0);
         when Block_Transfer =>
            Transfer (Work.Granted.Item, Work.Granted.Buffer, Moved);
            Last_Outcome := (Passed => Moved = Bytes (Work.Granted.Item),
                             Moved  => Moved);
      end case;
   end Post;

   function Finished (Item : Slot) return Boolean is
      pragma Unreferenced (Item);
   begin
      return True;
   end Finished;

   procedure Collect (Item : Slot; Work : Work_Order; Outcome : out Work_Outcome) is
      pragma Unreferenced (Item, Work);
   begin
      Outcome := Last_Outcome;
   end Collect;

   --  Nothing outlives Post, so there is never anything to cancel.
   procedure Cancel (Item : Slot) is null;

   --  Not logged: a refused write is answered as an error, and no command
   --  can expire.
   procedure Note (Event : Driver_Event; Count : Unsigned_64) is null;

   package Driver is new CuBit.Block_Driver
     (Slot                  => Slot,
      Writes                => Writes,
      Deadline_Microseconds => Command_Microseconds,
      Post                  => Post,
      Finished              => Finished,
      Collect               => Collect,
      Cancel                => Cancel,
      Note                  => Note);

   procedure Start
     (State    : Device_State;
      Identity : CuBit.Device_Identities.Identity) renames Driver.Start;

   procedure Serve_One renames Driver.Wait_And_Serve;
end CuBit.Block_Sync_Driver;
