with Interfaces;
with ACPI_EC_Types;
with ACPI_EC_Transactions;
--  Trusted EC backend catalog and serialized request owner. Install/Revoke are
--  privileged admission operations, never deserialized AML requests. Firmware
--  declarations alone do not justify an entry. Sender is authenticated receive
--  metadata, not a word from the caller. No address is accepted by Submit.
generic
   with package Engine is new ACPI_EC_Transactions (<>);
   Maximum_Registers : Positive := 32;
package ACPI_EC_Registers with SPARK_Mode is
   use ACPI_EC_Types;
   type Register_ID is new Interfaces.Unsigned_64;
   type Epoch_ID is new Interfaces.Unsigned_64;
   type Owner_ID is new Interfaces.Unsigned_64;
   type Operation_ID is new Interfaces.Unsigned_64;
   type Admission is record
      Register : Register_ID := 0;
      Epoch : Epoch_ID := 0;
      Owner : Owner_ID := 0;
      Cell : Cell_Offset := 0;
      Readable : Boolean := False;
      Writable : Boolean := False;
      Exact_Write_Safe : Boolean := False;
      Write_Mask : Byte := 0;
   end record;
   procedure Install (Item : Admission; Accepted : out Boolean);
   procedure Revoke (Register : Register_ID; Epoch : Epoch_ID;
                     Accepted : out Boolean);
   type Submission is (Started, Denied, Busy, Engine_Unavailable, Exhausted);
   procedure Submit
     (Sender : Owner_ID; Register : Register_ID; Epoch : Epoch_ID;
      For_Write : Boolean; Value : Byte; Deadline : Microseconds;
      Budget : Work_Budget; Result : out Submission;
      Token : out Operation_ID);
   --  Privileged event controller only, after independently admitted source
   --  delivery. Shares the engine and token sequence with named Field requests.
   --  This is not a wire operation and conveys no register/cell authority.
   procedure Submit_Query
     (Deadline : Microseconds; Budget : Work_Budget; Result : out Submission;
      Token : out Operation_ID);
   type Query_Completion is record
      Present : Boolean := False;
      Token : Operation_ID := 0;
      Status : Outcome := Idle;
      Event : Byte := 0;
      Effects_Possible : Boolean := False;
      Needs_Recovery : Boolean := False;
   end record;
   --  Separate delivery route: a Field endpoint cannot steal a query event.
   --  Event=0 is spurious/no-event and must never invoke _Q00.
   procedure Take_Query (Item : out Query_Completion);
   procedure Step;
   procedure Request_Stop;
   type Completion is record
      Present : Boolean := False;
      Token : Operation_ID := 0;
      Register : Register_ID := 0;
      Epoch : Epoch_ID := 0;
      Owner : Owner_ID := 0;
      Status : Outcome := Idle;
      Value : Byte := 0;
      Effects_Possible : Boolean := False;
      Needs_Recovery : Boolean := False;
   end record;
   --  Exactly one completion delivery per accepted request. Recovery retains
   --  the slot and engine ownership after delivery; no reset/replay API exists.
   procedure Take_Completion (Item : out Completion);
   type Progress_Kind is (No_Work, Run_Step, Wait_Until);
   type Progress is record
      Kind : Progress_Kind := No_Work;
      At_Time : Microseconds := 0;
   end record;
   --  Serialized advisory observation; no clock callback or hardware access.
   --  Now is the owner's current monotonic microsecond sample. Wait time never
   --  exceeds the accepted request's deadline. Terminal results request a step
   --  so their owning route can collect them; delivered recovery needs no work.
   --  No_Work is NOT quiescence: inspect Retained_For_Recovery independently.
   --  Engine Step still checks its own fresh clock/ownership/budget guards.
   function Next_Progress (Now : Microseconds) return Progress;
   function Active return Boolean;
   function Retained_For_Recovery return Boolean;
private
   subtype Slot_Index is Positive range 1 .. Maximum_Registers;
   type Register_Entry is record
      Used : Boolean := False;
      Enabled : Boolean := False;
      Item : Admission;
   end record;
   type Catalog is array (Slot_Index) of Register_Entry;
   Entries : Catalog;
   Occupied : Boolean := False;
   Delivered : Boolean := False;
   Selected : Slot_Index := Slot_Index'First;
   Sequence : Operation_ID := 0;
   Request_Deadline : Microseconds := 0;
   Writing : Boolean := False;
   Querying : Boolean := False;
end ACPI_EC_Registers;
