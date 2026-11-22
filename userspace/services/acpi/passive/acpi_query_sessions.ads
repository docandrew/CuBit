pragma Ada_2022;
with ACPI_Protocol;
with ACPI_Observer_Loans;
generic
   with package Loan_Owner is new ACPI_Observer_Loans (<>);
   type Reply_Handle is private;
   No_Reply : Reply_Handle;
   with procedure Save_Reply
     (Owner : in out Loan_Owner.Backend_Context; Handle : out Reply_Handle; Accepted : out Boolean);
   -- Answer consumes the saved handle on delivery success AND failure, matching
   -- CuBit.Handles.IPC.Answer. No blocking call or reentry into this session.
   with procedure Answer
     (Owner : in out Loan_Owner.Backend_Context; Handle : Reply_Handle;
      Operation : Loan_Owner.Report; Delivered : out Boolean);
package ACPI_Query_Sessions with SPARK_Mode is
   package Loans renames Loan_Owner;
   package Wire renames Loans.Wire;
   package Pages renames Loans.Pages;
   type State is private;
   function Valid (Item : State) return Boolean;
   function Ready (Item : State) return Boolean;
   function Has_Saved_Reply (Item : State) return Boolean;
   type Outcome is (Idle, Answered, Delivery_Lost, Deferred, Busy, Denied, Invalid_Extent, Save_Refused);
   type Report is record
      Status : Outcome := Idle;
      -- Current unsaved request still owns its thread reply authority. The
      -- native caller may send a typed refusal now; no loan was acquired.
      Immediate_Reply : Boolean := False;
      Operation : Loans.Report;
   end record;
   procedure Begin_Request
     (Item : in out State; Owner : in out Loans.Backend_Context; Stamp : ACPI_Protocol.Role_Tag;
      Sender : Loans.Peer_Identity; Loan : Loans.Loan_Reference; Declared_Length : Wire.Word;
      Request_Data, Response_Data : out Pages.Byte_Array; Result : out Report)
     with Pre => Valid (Item), Post => Valid (Item);
   -- Exactly one return attempt per call, no work replay or spin. Invoke from
   -- a bounded event-loop checkpoint and keep servicing firmware events. A
   -- false Answer result consumes authority; it must never trigger replay.
   procedure Progress (Item : in out State; Owner : in out Loans.Backend_Context; Result : out Report)
     with Pre => Valid (Item), Post => Valid (Item);
private
   type State is record
      Loan_State : Loans.State;
      Saved : Boolean := False;
      Handle : Reply_Handle := No_Reply;
      Pending : Loans.Report;
   end record;
   function Valid (Item : State) return Boolean is (not Loans.Holding (Item.Loan_State) or else Item.Saved);
   function Ready (Item : State) return Boolean is (not Item.Saved and then not Loans.Holding (Item.Loan_State));
   function Has_Saved_Reply (Item : State) return Boolean is (Item.Saved);
end ACPI_Query_Sessions;
