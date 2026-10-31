pragma Ada_2022;
with ACPI_Query_Sessions;
with ACPI_Protocol;
with ACPI_Passive_Query;
generic
   with package Session_Owner is new ACPI_Query_Sessions (<>);
   Wait_Forever : ACPI_Passive_Query.Word;
   with procedure Prepare_Local
     (Owner : in out Session_Owner.Loans.Backend_Context;
      Now, Deadline : out ACPI_Passive_Query.Word; Clock_Usable, Ready : out Boolean);
   with procedure Poll_Firmware
     (Owner : in out Session_Owner.Loans.Backend_Context; Handled : out Boolean);
   -- None_Ready and Query_Ready are distinct. Other kinds mean service work
   -- already handled by its owning adapter. Raw labels/loan words are decoded
   -- by that typed adapter, never interpreted as addresses here.
   type Request_Kind is (<>);
   None_Ready, Query_Ready : Request_Kind;
   with procedure Poll_Request
     (Owner : in out Session_Owner.Loans.Backend_Context; Kind : out Request_Kind;
      Sender : out Session_Owner.Loans.Peer_Identity; Loan : out Session_Owner.Loans.Loan_Reference;
      Stamp : out ACPI_Protocol.Role_Tag; Length : out ACPI_Passive_Query.Word);
   with procedure Reply_Immediate
     (Owner : in out Session_Owner.Loans.Backend_Context;
      Sender : Session_Owner.Loans.Peer_Identity; Result : Session_Owner.Report; Delivered : out Boolean);
   -- Kernel wait only, not a synchronous call to a peer. True includes deadline
   -- and spurious wakes: queues must be polled independently on the next step.
   with procedure Wait_Activity
     (Owner : in out Session_Owner.Loans.Backend_Context; Deadline : ACPI_Passive_Query.Word; Usable : out Boolean);
package ACPI_Query_Pump with SPARK_Mode is
   package Sessions renames Session_Owner;
   package Loans renames Sessions.Loans;
   package Wire renames Sessions.Wire;
   package Pages renames Sessions.Pages;
   use type Wire.Word;
   subtype Retry_Interval is Wire.Word range 1 .. Wire.Word'Last - 1;
   type State is private;
   function Valid (Item : State) return Boolean;
   function Pending (Item : State) return Boolean;
   type Outcome is (Worked, Waited, Local_Stopped, Clock_Regressed, Clock_Invalid,
                    Retry_Clock_Unavailable, Deadline_Overflow, Wait_Unavailable);
   type Report is record
      Status : Outcome := Local_Stopped;
      Cleanup, Request : Sessions.Report;
      Firmware_Handled, Request_Taken : Boolean := False;
      Immediate_Delivered : Boolean := False;
      Wait_Deadline : Wire.Word := 0;
      Observed_Now, Previous_Now : Wire.Word := 0;
   end record;
   -- Serialized step: local deadline work, one cleanup attempt, one firmware
   -- poll, one service poll, then wait only if no actual work was done. Failed
   -- cleanup alone is not work (so it cannot create a retry spin). Callbacks
   -- are bounded, return normally, and must not reenter this pump or its session.
   -- Prepare_Local uses the scheduler's monotonic millisecond epoch and consumes
   -- expired local one-shot deadlines before supplying the next one.
   procedure Step
     (Item : in out State; Owner : in out Loans.Backend_Context; Retry_Delay : Retry_Interval;
      Request_Data, Response_Data : out Pages.Byte_Array; Result : out Report)
     with Pre => Valid (Item), Post => Valid (Item);
private
   type State is record
      Session : Sessions.State;
      Seen_Clock : Boolean := False;
      Last_Now : Wire.Word := 0;
   end record;
   function Valid (Item : State) return Boolean is (Sessions.Valid (Item.Session));
   function Pending (Item : State) return Boolean is (Sessions.Has_Saved_Reply (Item.Session));
end ACPI_Query_Pump;
