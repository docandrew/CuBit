with CuBit.Protocols.Stream_Connections;

--  Serialized, single-binding lifecycle. No allocation, mapping, or IPC.
--  The owning adapter authenticates callers/evidence and pins live resources.
package CuBit.Protocols.Stream_Bindings with Pure, SPARK_Mode is
   package Connections renames CuBit.Protocols.Stream_Connections;
   subtype Live_Id is Unsigned_64 range 1 .. Unsigned_64'Last;
   type Optional_Route (Present : Boolean := False) is record
      case Present is
         when False => null;
         when True =>
            Transport_Ticket : Live_Id;
            Value : Connections.Request;
      end case;
   end record;
   type Phase is (Idle, Preparing, Retiring);
   type View is record
      Generation : Live_Id := 1;
      State : Phase := Idle;
      Ticket : Unsigned_64 := 0;
      Next_Ticket : Unsigned_64 := 1;
      Active, Staged : Optional_Route;
   end record;
   type Binding (Identity : Live_Id) is limited private;
   --  Internal inspection only; an external inspector needs its own authority.
   function Inspect (Item : Binding) return View;
   function Reference (Item : Binding) return Connections.Binding_Reference;
   type Result is
     (Succeeded, Not_Authorized, Stale_Binding, Busy, Exhausted,
      Wrong_Transition, Resources_Not_Ready);

   procedure Prepare
     (Item : in out Binding; Desired : Connections.Request;
      Evidence : Connections.Approvals; Ticket : out Unsigned_64;
      Outcome : out Result)
     with Post =>
       (Outcome = Succeeded or
          (Inspect (Item) = Inspect (Item)'Old and Ticket = 0)) and
       (Outcome /= Succeeded or
          (Inspect (Item).State = Preparing and Ticket /= 0)) and
       Inspect (Item).Active = Inspect (Item)'Old.Active and
       Inspect (Item).Generation = Inspect (Item)'Old.Generation;

   --  Evidence is freshly resolved at commit, not cached at prepare.
   --  Ready means the adapter pinned and admitted the proposed transport.
   procedure Commit
     (Item : in out Binding; Actor, Ticket : Unsigned_64;
      Evidence : Connections.Approvals; Ready : Boolean;
      Outcome : out Result)
     with Post =>
       (Outcome = Succeeded or Inspect (Item) = Inspect (Item)'Old) and
       (Outcome /= Succeeded or
          (Inspect (Item).Active.Present and
           Inspect (Item).Generation > Inspect (Item)'Old.Generation));

   --  Abort preserves the active route; prepared resources go to retirement.
   procedure Abort_Preparation
     (Item : in out Binding; Actor, Ticket : Unsigned_64;
      Outcome : out Result)
     with Post =>
       Inspect (Item).Active = Inspect (Item)'Old.Active and
       Inspect (Item).Generation = Inspect (Item)'Old.Generation and
       (Outcome = Succeeded or Inspect (Item) = Inspect (Item)'Old);

   --  Trusted resource-owner acknowledgement, never an untrusted app boolean.
   --  Quiescent means outstanding uses ended and mappings are safely released.
   procedure Finish_Retirement
     (Item : in out Binding; Ticket : Unsigned_64; Quiescent : Boolean;
      Outcome : out Result)
     with Post =>
       Inspect (Item).Active = Inspect (Item)'Old.Active and
       Inspect (Item).Generation = Inspect (Item)'Old.Generation and
       (Outcome = Succeeded or Inspect (Item) = Inspect (Item)'Old);
private
   type Transition (State : Phase := Idle) is record
      case State is
         when Idle => null;
         when Preparing | Retiring =>
            Ticket : Live_Id;
            Transport_Ticket : Live_Id;
            Route : Connections.Request;
      end case;
   end record;
   type Binding (Identity : Live_Id) is limited record
      Generation : Live_Id := 1;
      Next_Ticket : Unsigned_64 := 1;
      Active : Optional_Route;
      Change : Transition;
   end record;
end CuBit.Protocols.Stream_Bindings;
