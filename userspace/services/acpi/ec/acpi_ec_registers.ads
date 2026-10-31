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
package ACPI_EC_Registers with SPARK_Mode, Initial_Condition => Catalog_Coherent is
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
   pragma Unevaluated_Use_Of_Old (Allow);
   type State_View is private with Ghost;
   function State_Of return State_View with Ghost;
   function Catalog_Coherent return Boolean with Ghost;
   function Coherent return Boolean with Ghost;
   -- Local initialization establishes Catalog_Coherent. The composition owner
   -- separately preserves Engine.Coherent before the public operations.
   function Admissible (Item : Admission) return Boolean with Global => null;
   function Install_Transition (Before, After : State_View; Item : Admission;
      Accepted : Boolean) return Boolean with Ghost, Global => null;
   function Revoke_Transition (Before, After : State_View; Register : Register_ID;
      Epoch : Epoch_ID; Accepted : Boolean) return Boolean with Ghost, Global => null;
   procedure Install (Item : Admission; Accepted : out Boolean)
     with Pre => Coherent, Post => Coherent and then
       Install_Transition (State_Of'Old, State_Of, Item, Accepted);
   procedure Revoke (Register : Register_ID; Epoch : Epoch_ID;
                     Accepted : out Boolean)
     with Pre => Coherent, Post => Coherent and then
       Revoke_Transition (State_Of'Old, State_Of, Register, Epoch, Accepted);
   type Submission is (Started, Denied, Busy, Engine_Unavailable, Exhausted);
   function Submit_Transition (Before, After : State_View; Sender : Owner_ID;
      Register : Register_ID; Epoch : Epoch_ID; For_Write : Boolean; Value : Byte;
      Deadline : Microseconds; Budget : Work_Budget; Result : Submission;
      Token : Operation_ID) return Boolean with Ghost, Global => null;
   function Query_Transition (Before, After : State_View; Deadline : Microseconds;
      Budget : Work_Budget; Result : Submission; Token : Operation_ID)
      return Boolean with Ghost, Global => null;
   procedure Submit
     (Sender : Owner_ID; Register : Register_ID; Epoch : Epoch_ID;
      For_Write : Boolean; Value : Byte; Deadline : Microseconds;
      Budget : Work_Budget; Result : out Submission;
      Token : out Operation_ID)
     with Pre => Coherent, Post => Coherent and then
       Submit_Transition (State_Of'Old, State_Of, Sender, Register, Epoch,
         For_Write, Value, Deadline, Budget, Result, Token);
   --  Privileged event controller only, after independently admitted source
   --  delivery. Shares the engine and token sequence with named Field requests.
   --  This is not a wire operation and conveys no register/cell authority.
   procedure Submit_Query
     (Deadline : Microseconds; Budget : Work_Budget; Result : out Submission;
      Token : out Operation_ID)
     with Pre => Coherent, Post => Coherent and then
       Query_Transition (State_Of'Old, State_Of, Deadline, Budget, Result, Token);
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
   procedure Take_Query (Item : out Query_Completion)
     with Pre => Coherent, Post => Coherent;
   procedure Step with Pre => Coherent, Post => Coherent;
   procedure Request_Stop with Pre => Coherent, Post => Coherent;
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
   procedure Take_Completion (Item : out Completion)
     with Pre => Coherent, Post => Coherent;
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
   type State_View is record
      Entries : Catalog;
      Occupied, Delivered, Writing, Querying : Boolean;
      Selected : Slot_Index;
      Sequence : Operation_ID;
      Deadline : Microseconds;
      Hardware : Engine.State_View;
   end record;
   function State_Of return State_View is
     ((Entries, Occupied, Delivered, Writing, Querying, Selected,
       Sequence, Request_Deadline, Engine.State_Of));
   function Admissible (Item : Admission) return Boolean is
     (Item.Register /= 0 and then Item.Epoch /= 0 and then Item.Owner /= 0
      and then (Item.Readable or Item.Writable)
      and then (if Item.Writable then Item.Exact_Write_Safe else Item.Write_Mask = 0));
   function Matches (Item : Register_Entry; Sender : Owner_ID;
      Register : Register_ID; Epoch : Epoch_ID; For_Write : Boolean; Value : Byte)
      return Boolean is
     (Item.Used and then Item.Enabled and then Item.Item.Register = Register
      and then Item.Item.Epoch = Epoch and then Item.Item.Owner = Sender
      and then (if For_Write then Item.Item.Writable and Item.Item.Exact_Write_Safe
        and (Value and not Item.Item.Write_Mask) = 0
        else Item.Item.Readable and Value = 0));
   function Catalog_Coherent return Boolean is
     ((for all I in Slot_Index =>
        (if Entries (I).Used then Admissible (Entries (I).Item))
        and then (if Entries (I).Enabled then Entries (I).Used)
        and then (for all J in Slot_Index =>
          (if I /= J and then Entries (I).Used and then Entries (J).Used
           then Entries (I).Item.Register /= Entries (J).Item.Register)))
      and then (if Occupied then Sequence > 0)
      and then (if Occupied and not Querying then Entries (Selected).Used and Entries (Selected).Enabled));
   function Coherent return Boolean is (Catalog_Coherent and then Engine.Coherent);
   function Installable (Before : State_View; Item : Admission) return Boolean is
     (Admissible (Item) and then
       (if (for some I in Slot_Index => Before.Entries (I).Used and then Before.Entries (I).Item.Register = Item.Register)
        then (for some I in Slot_Index => Before.Entries (I).Used
          and then Before.Entries (I).Item.Register = Item.Register
          and then not Before.Entries (I).Enabled
          and then not (Before.Occupied and not Before.Querying and Before.Selected = I)
          and then Item.Epoch > Before.Entries (I).Item.Epoch)
        else (for some I in Slot_Index => not Before.Entries (I).Used))) with Ghost;
   function Install_Transition (Before, After : State_View; Item : Admission;
      Accepted : Boolean) return Boolean is
     (Accepted = Installable (Before, Item)
      and then (if Accepted then
        After = (Before with delta Entries => After.Entries)
        and then (for some I in Slot_Index =>
          After.Entries (I) = (Used => True, Enabled => True, Item => Item)
          and then (for all J in Slot_Index =>
            (if J /= I then After.Entries (J) = Before.Entries (J))))
        else After = Before));
   function Revocable (Before : State_View; Register : Register_ID; Epoch : Epoch_ID;
      Index : Slot_Index) return Boolean is
     (Before.Entries (Index).Used and then Before.Entries (Index).Enabled
      and then Before.Entries (Index).Item.Register = Register
      and then Before.Entries (Index).Item.Epoch = Epoch
      and then not (Before.Occupied and not Before.Querying and Before.Selected = Index)) with Ghost;
   function Revoke_Transition (Before, After : State_View; Register : Register_ID;
      Epoch : Epoch_ID; Accepted : Boolean) return Boolean is
     (Accepted = (for some I in Slot_Index => Revocable (Before, Register, Epoch, I))
      and then (if Accepted then (for some I in Slot_Index =>
        Revocable (Before, Register, Epoch, I) and then
        After = (Before with delta Entries => (Before.Entries with delta I =>
          (Before.Entries (I) with delta Enabled => False)))) else After = Before));
   function Startable (Before : State_View) return Boolean is
     (Before.Hardware.Status = Idle and not Before.Hardware.Recovery) with Ghost;
   function Submit_Transition (Before, After : State_View; Sender : Owner_ID;
      Register : Register_ID; Epoch : Epoch_ID; For_Write : Boolean; Value : Byte;
      Deadline : Microseconds; Budget : Work_Budget; Result : Submission;
      Token : Operation_ID) return Boolean is
     (Result = (if Before.Occupied then Busy
        elsif Before.Sequence = Operation_ID'Last then Exhausted
        elsif not (for some I in Slot_Index => Matches (Before.Entries (I), Sender, Register, Epoch, For_Write, Value)) then Denied
        elsif not Startable (Before) then Engine_Unavailable else Started)
      and then (if Result /= Started then Token = 0 and After = Before
        else Before.Sequence < Operation_ID'Last and then Token = Before.Sequence + 1
        and then (for some I in Slot_Index => Matches (Before.Entries (I), Sender, Register, Epoch, For_Write, Value)
          and then After = (Before with delta Occupied => True, Delivered => False,
            Selected => I, Sequence => Token, Deadline => Deadline, Writing => For_Write,
            Querying => False, Hardware => After.Hardware)
          and then Engine.Start_Transition (Before.Hardware, After.Hardware,
            (if For_Write then Write_Cell else Read_Cell), Before.Entries (I).Item.Cell,
            Value, Deadline, Budget, True))));
   function Query_Transition (Before, After : State_View; Deadline : Microseconds;
      Budget : Work_Budget; Result : Submission; Token : Operation_ID) return Boolean is
     (Result = (if Before.Occupied then Busy elsif Before.Sequence = Operation_ID'Last then Exhausted
        elsif not Startable (Before) then Engine_Unavailable else Started)
      and then (if Result /= Started then Token = 0 and After = Before
        else Before.Sequence < Operation_ID'Last and then Token = Before.Sequence + 1
        and then After = (Before with delta Occupied => True, Delivered => False,
          Querying => True, Sequence => Token, Deadline => Deadline, Hardware => After.Hardware)
        and then Engine.Start_Transition (Before.Hardware, After.Hardware, Query_Event, 0, 0, Deadline, Budget, True)));
end ACPI_EC_Registers;
