pragma Ada_2022;
with ACPI_EC_Event_Types;
-- Logical admission and retained-completion state used by the actual owner.
-- Open operations include interrupted calls: they never imply hardware progress,
-- quiescence,source authority or permission to release any backing.
package ACPI_EC_Supervisor_Control with SPARK_Mode, Pure is
   package E renames ACPI_EC_Event_Types;
   use type E.Controller_ID, E.Epoch_ID, E.Token_ID, E.Notice;
   type Phase is (Fresh, Ready, Advance_Open, Cleanup_Open, Stopped_State);
   type Snapshot is record
      Stage : Phase := Fresh;
      Pending : E.Notice_Option;
      Charge, Ceiling : Natural := 0;
   end record;
   type State is private with Default_Initial_Condition => Valid (State);
   function View (Item : State) return Snapshot with Global => null;
   function Notice_Valid (Event : E.Notice) return Boolean is
     (Event.Controller /= 0 and then Event.Epoch /= 0 and then Event.Token /= 0) with Global => null;
   function Valid (Item : State) return Boolean is
     ((if View (Item).Pending.Present then
         Notice_Valid (View (Item).Pending.Item) and then View (Item).Charge <= View (Item).Ceiling
       else View (Item).Charge = 0 and then View (Item).Ceiling = 0)
      and then (if View (Item).Stage = Fresh then not View (Item).Pending.Present)) with Global => null;
   function Entry_Closed (Item : State) return Boolean is
     (View (Item).Stage in Advance_Open | Cleanup_Open | Stopped_State) with Global => null;
   function Report_Complete (Item : State) return Boolean is
     (View (Item).Stage in Ready | Stopped_State) with Global => null;
   function Cleanup_Allowed (Item : State) return Boolean is
     (View (Item).Stage = Stopped_State) with Global => null;
   procedure Enter_Advance (Item : in out State; Accepted : out Boolean)
     with Global => null, Always_Terminates, Pre => Valid (Item),
       Post => Valid (Item) and then Accepted = (View (Item'Old).Stage in Fresh | Ready)
         and then (if Accepted then View (Item) = (View (Item'Old) with delta Stage => Advance_Open)
                   else Item = Item'Old);
   procedure Finish_Advance (Item : in out State; Terminal : Boolean)
     with Global => null, Always_Terminates,
       Pre => Valid (Item) and then View (Item).Stage = Advance_Open,
       Post => Valid (Item) and then View (Item) = (View (Item'Old) with delta
         Stage => (if Terminal then Stopped_State else Ready));
   type Stop_Result is (Stop_Accepted, Stop_Already_Closed, Stop_Incomplete);
   function Stop_Decision (Item : State) return Stop_Result is
     (case View (Item).Stage is
        when Fresh | Ready => Stop_Accepted,
        when Stopped_State => Stop_Already_Closed,
        when Advance_Open | Cleanup_Open => Stop_Incomplete) with Global => null;
   procedure Request_Stop (Item : in out State; Result : out Stop_Result)
     with Global => null, Always_Terminates, Pre => Valid (Item),
       Post => Valid (Item) and then Result = Stop_Decision (Item'Old)
         and then (if Result = Stop_Accepted then
           View (Item) = (View (Item'Old) with delta Stage => Stopped_State)
         else Item = Item'Old);
   function Retain_Allowed (Item : State; Event : E.Notice; Charge, Ceiling : Natural) return Boolean is
     (View (Item).Stage = Advance_Open and then not View (Item).Pending.Present
      and then Notice_Valid (Event) and then Charge <= Ceiling) with Global => null;
   procedure Retain (Item : in out State; Event : E.Notice; Charge, Ceiling : Natural;
      Accepted : out Boolean)
     with Global => null, Always_Terminates, Pre => Valid (Item),
       Post => Valid (Item) and then Accepted = Retain_Allowed (Item'Old, Event, Charge, Ceiling)
         and then (if Accepted then View (Item) = (Stage => Advance_Open,
           Pending => (Present => True, Item => Event), Charge => Charge, Ceiling => Ceiling)
         else Item = Item'Old);
   function Confirmation_Allowed (Item : State; Expected : E.Notice) return Boolean is
     (View (Item).Stage = Advance_Open and then View (Item).Pending.Present
      and then View (Item).Pending.Item = Expected) with Global => null;
   -- Called only after a truthful confirmed handoff; exact identity is still
   -- checked here. Unknown/refused handoff must leave this storage untouched.
   procedure Confirm (Item : in out State; Expected : E.Notice; Accepted : out Boolean)
     with Global => null, Always_Terminates, Pre => Valid (Item),
       Post => Valid (Item) and then Accepted = Confirmation_Allowed (Item'Old, Expected)
         and then (if Accepted then View (Item) = (Stage => Advance_Open,
           Pending => (Present => False), Charge => 0, Ceiling => 0)
         else Item = Item'Old);
   procedure Enter_Cleanup (Item : in out State; Accepted : out Boolean)
     with Global => null, Always_Terminates, Pre => Valid (Item),
       Post => Valid (Item) and then Accepted = Cleanup_Allowed (Item'Old)
         and then (if Accepted then View (Item) = (View (Item'Old) with delta Stage => Cleanup_Open)
                   else Item = Item'Old);
   procedure Finish_Cleanup (Item : in out State)
     with Global => null, Always_Terminates,
       Pre => Valid (Item) and then View (Item).Stage = Cleanup_Open,
       Post => Valid (Item) and then View (Item) = (View (Item'Old) with delta Stage => Stopped_State);
private
   type State is record
      Info : Snapshot;
   end record;
   function View (Item : State) return Snapshot is (Item.Info);
end ACPI_EC_Supervisor_Control;
