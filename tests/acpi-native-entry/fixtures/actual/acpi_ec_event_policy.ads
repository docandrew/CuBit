pragma Ada_2022;
with ACPI_EC_Event_Types;
with CuBit.Process_IDs;
--  Actual replay-control state used by the receiver. This pure policy neither
--  authenticates kernel metadata nor validates namespace/provider readiness.
package ACPI_EC_Event_Policy with SPARK_Mode, Pure is
   package E renames ACPI_EC_Event_Types;
   package P renames CuBit.Process_IDs;
   use type E.Controller_ID, E.Epoch_ID, E.Token_ID, E.Event_Number, P.Process_ID;
   type Description is record
      Used, Enabled, Held, Uncertain, Guarding : Boolean := False;
      Controller : E.Controller_ID := 0;
      Epoch : E.Epoch_ID := 0;
      Sender : P.Process_ID := P.No_Process;
      Last : E.Token_ID := 0;
      Last_Number : E.Event_Number := E.Event_Number'First;
   end record;
   Empty : constant Description := (others => <>);
   type State is private with Default_Initial_Condition => Valid (State);
   function View (Item : State) return Description;
   function Valid (Item : State) return Boolean is
     ((if View (Item).Used then
         View (Item).Controller /= 0 and then View (Item).Epoch /= 0
         and then P.Is_Process (View (Item).Sender)
       else View (Item) = Empty)
      and then (if View (Item).Guarding then View (Item).Held));
   function Identity_Valid (Controller : E.Controller_ID; Epoch : E.Epoch_ID;
      Sender : P.Process_ID) return Boolean is
     (Controller /= 0 and then Epoch /= 0 and then P.Is_Process (Sender));
   type Binding_Result is (Bound, Invalid_Identity, Other_Controller,
                          Route_Held, Already_Active, Old_Epoch);
   function Binding_Decision (Item : State; Controller : E.Controller_ID;
      Epoch : E.Epoch_ID; Sender : P.Process_ID) return Binding_Result is
     (if not Identity_Valid (Controller, Epoch, Sender) then Invalid_Identity
      elsif View (Item).Used and then View (Item).Controller /= Controller then Other_Controller
      elsif View (Item).Held or else View (Item).Uncertain then Route_Held
      elsif View (Item).Enabled then Already_Active
      elsif View (Item).Used and then Epoch <= View (Item).Epoch then Old_Epoch
      else Bound) with Pre => Valid (Item);
   procedure Bind (Item : in out State; Controller : E.Controller_ID;
      Epoch : E.Epoch_ID; Sender : P.Process_ID; Result : out Binding_Result)
     with Pre => Valid (Item), Post => Valid (Item)
       and then Result = Binding_Decision (Item'Old, Controller, Epoch, Sender)
       and then (if Result = Bound then
         View (Item) = (Used => True, Enabled => True, Controller => Controller,
           Epoch => Epoch, Sender => Sender, others => <>)
         and then (if View (Item'Old).Used then
           Controller = View (Item'Old).Controller and then Epoch > View (Item'Old).Epoch
           and then not View (Item'Old).Enabled and then not View (Item'Old).Held
           and then not View (Item'Old).Uncertain)
       else Item = Item'Old);
   procedure Revoke (Item : in out State; Controller : E.Controller_ID;
      Epoch : E.Epoch_ID; Accepted : out Boolean)
     with Pre => Valid (Item), Post => Valid (Item)
       and then Accepted = (View (Item'Old).Used and then View (Item'Old).Enabled
         and then View (Item'Old).Controller = Controller and then View (Item'Old).Epoch = Epoch)
       and then (if Accepted then View (Item) = (View (Item'Old) with delta Enabled => False)
                 else Item = Item'Old);
   type Admission is (Admitted, Unknown_Controller, Disabled, Wrong_Sender,
      Wrong_Epoch, Scope_Changed, Invalid_Token, Duplicate, Token_Conflict,
      Old_Token, Field_Uncertain, Busy);
   function Decision (Item : State; Sender : P.Process_ID; Event : E.Notice;
      Scope_Current : Boolean) return Admission is
     (if not View (Item).Used or else Event.Controller /= View (Item).Controller then Unknown_Controller
      elsif not View (Item).Enabled then Disabled
      elsif Sender /= View (Item).Sender then Wrong_Sender
      elsif Event.Epoch /= View (Item).Epoch then Wrong_Epoch
      elsif not Scope_Current then Scope_Changed
      elsif Event.Token = 0 then Invalid_Token
      elsif Event.Token = View (Item).Last then
        (if Event.Number = View (Item).Last_Number then Duplicate else Token_Conflict)
      elsif Event.Token < View (Item).Last then Old_Token
      elsif View (Item).Uncertain or else View (Item).Guarding then Field_Uncertain
      elsif View (Item).Held then Busy
      else Admitted) with Pre => Valid (Item);
   procedure Claim (Item : in out State; Sender : P.Process_ID;
      Event : E.Notice; Scope_Current : Boolean; Result : out Admission)
     with Pre => Valid (Item), Post => Valid (Item)
       and then Result = Decision (Item'Old, Sender, Event, Scope_Current)
       and then (if Result = Admitted then
         View (Item) = (View (Item'Old) with delta
           Last => Event.Token, Last_Number => Event.Number, Held => True)
         and then Event.Token > View (Item'Old).Last and then Event.Token /= 0
         and then Event.Controller = View (Item'Old).Controller
         and then Event.Epoch = View (Item'Old).Epoch
         and then Sender = View (Item'Old).Sender and then Scope_Current
       else Item = Item'Old);
   function Matches (Item : State; Event : E.Notice) return Boolean is
     (View (Item).Used and then Event.Token /= 0
      and then Event.Controller = View (Item).Controller
      and then Event.Epoch = View (Item).Epoch
      and then Event.Token = View (Item).Last and then Event.Number = View (Item).Last_Number);
   procedure Guard (Item : in out State; Event : E.Notice; Accepted : out Boolean)
     with Pre => Valid (Item), Post => Valid (Item)
       and then Accepted = (Matches (Item'Old, Event) and then View (Item'Old).Held
         and then not View (Item'Old).Uncertain and then not View (Item'Old).Guarding)
       and then (if Accepted then View (Item) = (View (Item'Old) with delta Guarding => True)
                 else Item = Item'Old);
   --  Caller supplies truthful observations after normal evaluator/cleanup
   --  return. Committed uncertainty is sticky; provisional call guard clears.
   procedure Settle (Item : in out State; Event : E.Notice;
      Pending, Uncertain : Boolean; Accepted : out Boolean)
     with Pre => Valid (Item), Post => Valid (Item)
       and then Accepted = (Matches (Item'Old, Event) and then View (Item'Old).Held)
       and then (if Accepted then View (Item) = (View (Item'Old) with delta
           Held => Pending, Uncertain => View (Item'Old).Uncertain or Uncertain, Guarding => False)
                 else Item = Item'Old);
private
   type State is record Data : Description; end record;
   function View (Item : State) return Description is (Item.Data);
end ACPI_EC_Event_Policy;
