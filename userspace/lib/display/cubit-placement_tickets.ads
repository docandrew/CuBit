pragma Ada_2022;
with Interfaces;
with CuBit.Display_Outputs;

--  Serialized desktop-owner adapter. Tickets are local values, not grants.
package CuBit.Placement_Tickets with SPARK_Mode, Pure is
   package R renames CuBit.Display_Outputs.Registry;
   package W renames R.W;
   type Window_Incarnation is new Interfaces.Unsigned_64
     range 1 .. Interfaces.Unsigned_64'Last;
   type Intent_Revision is new Interfaces.Unsigned_64
     range 1 .. Interfaces.Unsigned_64'Last;
   type Window_Version is record
      Instance : Window_Incarnation := 1;
      Intent : Intent_Revision := 1;
   end record;
   type Ticket is private;
   type Admission is
     (May_Apply, No_Proposal, Output_State_Changed, Window_Replaced,
      User_Intent_Changed, Interaction_In_Progress);
   function Proposal (Item : Ticket) return W.Plan_Result;
   function Prepare
     (Registry : R.State; Window : Window_Version;
      Desired : W.Preference;
      Previous : W.Display_Choice := (Known => False);
      Policy : W.Fallback_Policy := W.Await_Preferred_Output;
      Interaction : W.Interaction_State := W.Idle) return Ticket;
   function Fresh
     (Registry : R.State; Item : Ticket; Window : Window_Version;
      Interaction : W.Interaction_State) return Boolean with Ghost;
   --  Check and application must share the owner's serialized transition.
   --  Recheck after any await/yield: this is not a reservation or a lock.
   function Check
     (Registry : R.State; Item : Ticket; Window : Window_Version;
      Interaction : W.Interaction_State) return Admission
     with Post =>
       ((Check'Result = May_Apply) = Fresh
         (Registry, Item, Window, Interaction));
private
   type Ticket is record
      View : R.Snapshot;
      Owner : Window_Version;
      Plan : W.Plan_Result;
   end record;
   function Proposal (Item : Ticket) return W.Plan_Result is (Item.Plan);
end CuBit.Placement_Tickets;
