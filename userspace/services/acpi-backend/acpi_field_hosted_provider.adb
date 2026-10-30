pragma Ada_2022;
with ACPI_FADT.Transactions;
with ACPI_Region_Protocol;
with AML_Identity.Issuer;
with Hardware_Authority;
package body ACPI_Field_Hosted_Provider is
   use type Interfaces.Unsigned_64;
   use type AML_Identity.Identity;
   use type AML_Field_Access.Endpoint_ID;
   use type AML_Field_Access.Register_ID;
   use type AML_Field_Access.Observation;
   use type ACPI_Region_Policy.Address_Space;
   function Identity (IO : Context) return AML_Identity.Identity is (IO.Identity);
   function Pending (IO : Context) return Boolean is
     (IO.Captured.Present or else
      (for some R of IO.Registers => ACPI_Region_Policy.Busy (R.Region)));
   function Uncertain (IO : Context) return AML_Field_Access.Observation is
     (IO.Captured);
   function Completed_Fault (IO : Context) return AML_Field_Access.Observation is
     (IO.Fault);
   function Observe_Hardware (IO : Context) return Hardware_State is (IO.Hardware);
   -- Named IDs are opaque full-width values, never array indices or addresses.
   function Find (IO : Context; ID : AML_Field_Access.Register_ID) return Slot_Count is
   begin
      if ID = AML_Field_Access.No_Register then return 0; end if;
      for R in IO.Registers'Range loop
         if IO.Registers (R).Used and then
           ACPI_Region_Policy.Policy (IO.Registers (R).Region).Register_Authority.Resource_ID =
             Interfaces.Unsigned_64 (ID)
         then return R; end if;
      end loop;
      return 0;
   end Find;
   procedure Install
     (IO : in out Context; Endpoint : AML_Field_Access.Endpoint_ID;
      Configuration : ACPI_Region_Policy.Configuration;
      Exact_Write_Safe : Boolean; Accepted : out Boolean) is
      Issued : Boolean;
      Slot : Slot_Count;
   begin
      Accepted := False;
      if IO.Dispatching or else Pending (IO)
        or else Endpoint = AML_Field_Access.No_Endpoint
        or else (IO.Endpoint /= AML_Field_Access.No_Endpoint and then IO.Endpoint /= Endpoint)
        or else not ACPI_Region_Policy.Valid (Configuration)
        or else not Hardware_Authority.Valid (Configuration.Register_Authority)
      then return; end if;
      Slot := Find (IO, AML_Field_Access.Register_ID (Configuration.Register_Authority.Resource_ID));
      if Slot = 0 then
         for R in IO.Registers'Range loop
            if not IO.Registers (R).Used then Slot := R; exit; end if;
         end loop;
      end if;
      if Slot = 0 then return; end if;
      if ACPI_Region_Policy.Active (IO.Registers (Slot).Region)
        or else ACPI_Region_Policy.Epoch (IO.Registers (Slot).Region) = Interfaces.Unsigned_64'Last
        or else ACPI_Region_Policy.Receipt (IO.Registers (Slot).Region) = Interfaces.Unsigned_64'Last
      then return; end if;
      if IO.Identity = AML_Identity.No_Identity then
         AML_Identity.Issuer.Issue (IO.Identity, Issued);
         if not Issued then return; end if;
      end if;
      -- Never replace a used State: epoch and receipt high-water survive revoke.
      ACPI_Region_Policy.Install (IO.Registers (Slot).Region, Configuration, Accepted);
      if Accepted then
         IO.Registers (Slot).Used := True;
         IO.Registers (Slot).Exact_Write_Safe := Exact_Write_Safe;
         IO.Endpoint := Endpoint;
      end if;
   end Install;
   procedure Describe
     (IO : Context; Target : AML_Field_Access.Target;
      Geometry : out AML_Field_Layout.Register_Geometry; Accepted : out Boolean) is
      Slot : constant Slot_Count := Find (IO, Target.Register);
   begin
      Geometry := (others => <>); Accepted := False;
      if IO.Dispatching or else Pending (IO) or else Slot = 0
        or else Target.Provider = AML_Identity.No_Identity
        or else Target.Provider /= IO.Identity
        or else Target.Endpoint = AML_Field_Access.No_Endpoint
        or else Target.Endpoint /= IO.Endpoint
        or else not ACPI_Region_Policy.Active (IO.Registers (Slot).Region)
        or else Target.Epoch /= ACPI_Region_Policy.Epoch (IO.Registers (Slot).Region)
      then return; end if;
      declare
         C : constant ACPI_Region_Policy.Configuration :=
           ACPI_Region_Policy.Policy (IO.Registers (Slot).Region);
      begin
         if not ACPI_Region_Policy.Valid (C)
           or else not Hardware_Authority.Valid (C.Register_Authority)
         then return; end if;
         Geometry := (Region => (Space => (if C.Space = ACPI_Region_Policy.Memory_Space
           then AML_Field_Layout.Memory_Space else AML_Field_Layout.IO_Space),
           Base => C.Base, Length => C.Length),
           Byte_Offset => C.Register_Offset, Width => C.Register_Width);
         Accepted := True;
      end;
   end Describe;
   procedure Revoke_Register
     (IO : in out Context; Target : AML_Field_Access.Target; Accepted : out Boolean) is
      Geometry : AML_Field_Layout.Register_Geometry;
      Slot : Slot_Count;
   begin
      Describe (IO, Target, Geometry, Accepted);
      if not Accepted then return; end if;
      Slot := Find (IO, Target.Register);
      ACPI_Region_Policy.Revoke (IO.Registers (Slot).Region);
   end Revoke_Register;
   function Free_Confirmation (IO : Context) return Confirmation_Count is
   begin
      for C in IO.Confirmed'Range loop
         if not IO.Confirmed (C).Present then return C; end if;
      end loop;
      return 0;
   end Free_Confirmation;
   procedure Authorize
     (IO : Context; Target : AML_Field_Access.Target;
      For_Write : Boolean; Accepted : out Boolean) is
      Geometry : AML_Field_Layout.Register_Geometry;
      Slot : Slot_Count;
      Full_Mask : Interfaces.Unsigned_64;
   begin
      Describe (IO, Target, Geometry, Accepted);
      if not Accepted then return; end if;
      Accepted := False;
      if Free_Confirmation (IO) = 0 then return; end if;
      Slot := Find (IO, Target.Register);
      declare
         C : constant ACPI_Region_Policy.Configuration := ACPI_Region_Policy.Policy (IO.Registers (Slot).Region);
      begin
         if ACPI_Region_Policy.Busy (IO.Registers (Slot).Region)
           or else ACPI_Region_Policy.Receipt (IO.Registers (Slot).Region) = Interfaces.Unsigned_64'Last
           or else not ACPI_Region_Policy.Resolve (IO.Registers (Slot).Region, C.Tag,
             Target.Epoch, C.Register_Offset, C.Register_Width, For_Write).Allowed
           or else not Hardware_Authority.Permits (C.Register_Authority,
             Interfaces.Unsigned_64 (Target.Register), For_Write, 0)
         then return; end if;
         Full_Mask := (case C.Register_Width is
            when ACPI_FADT.Transactions.Byte_Access => 16#FF#,
            when ACPI_FADT.Transactions.Word_Access => 16#FFFF#,
            when ACPI_FADT.Transactions.Dword_Access => 16#FFFF_FFFF#,
            when ACPI_FADT.Transactions.Qword_Access => Interfaces.Unsigned_64'Last);
         Accepted := not For_Write or else
           (IO.Registers (Slot).Exact_Write_Safe and then C.Register_Authority.Write_Mask = Full_Mask);
      end;
   end Authorize;
   procedure Dispatch
     (IO : in out Context; Item : AML_Field_Access.Request;
      Reply : out AML_Field_Access.Response) is
      Geometry : AML_Field_Layout.Register_Geometry;
      Accepted : Boolean;
      Packet, Answer : ACPI_Region_Protocol.Packet;
      Ticket : Interfaces.Unsigned_64;
      Slot : Slot_Count;
      Full_Mask : Interfaces.Unsigned_64;
      Reserved : Confirmation_Count;
   begin
      Reply := (others => <>);
      Describe (IO, Item.Destination, Geometry, Accepted);
      if not Accepted then return; end if;
      -- Any admitted transaction can become uncertain. Reserve bounded receipt
      -- capacity before effects, even if this particular call later completes.
      Reserved := Free_Confirmation (IO);
      if Reserved = 0 then return; end if;
      Slot := Find (IO, Item.Destination.Register);
      declare
         C : constant ACPI_Region_Policy.Configuration :=
           ACPI_Region_Policy.Policy (IO.Registers (Slot).Region);
      begin
         Full_Mask := (case C.Register_Width is
            when ACPI_FADT.Transactions.Byte_Access => 16#FF#,
            when ACPI_FADT.Transactions.Word_Access => 16#FFFF#,
            when ACPI_FADT.Transactions.Dword_Access => 16#FFFF_FFFF#,
            when ACPI_FADT.Transactions.Qword_Access => Interfaces.Unsigned_64'Last);
         if Item.For_Write and then (not IO.Registers (Slot).Exact_Write_Safe
           or else C.Register_Authority.Write_Mask /= Full_Mask) then return; end if;
         Packet := (Label => (if Item.For_Write then Backend.Write_Operation else Backend.Read_Operation),
           Data => [Item.Destination.Epoch, Interfaces.Unsigned_64 (Item.Destination.Register),
                    Item.Value, 0], others => <>);
         -- Exclusive caller ownership remains required. The guard rejects
         -- synchronous callback reentry; an escaping exception leaves it set.
         IO.Reserved_Confirmation := Reserved;
         IO.Dispatching := True;
         Backend.Dispatch (IO.Registers (Slot).Region, IO.Hardware, C.Tag, Packet, Answer, Ticket);
         IO.Dispatching := False;
      end;
      if Answer.Data (0) /= Backend.Outcome'Pos (Backend.Indeterminate) then
         IO.Reserved_Confirmation := 0;
      end if;
      if Answer.Data (0) = Backend.Outcome'Pos (Backend.Done) then
         Reply.Status := AML_Field_Access.Completed;
         Reply.Value := Answer.Data (1) or Interfaces.Shift_Left (Answer.Data (2), 32);
      elsif Answer.Data (0) = Backend.Outcome'Pos (Backend.Backend_Fault) then
         IO.Fault := (Present => True, Provider => IO.Identity, Item => Item,
                      Operation => AML_Field_Access.No_Operation);
         Reply := (Status => AML_Field_Access.Failed_Completed,
                   Provider => IO.Identity, Value => 0,
                   Operation => AML_Field_Access.No_Operation);
      elsif Answer.Data (0) = Backend.Outcome'Pos (Backend.Indeterminate) then
         IO.Pending_Slot := Slot; IO.Pending_Ticket := Ticket;
         IO.Captured := (Present => True, Provider => IO.Identity, Item => Item,
            Operation => AML_Field_Access.Operation_ID (Ticket));
         Reply := (Status => AML_Field_Access.Indeterminate, Provider => IO.Identity, Value => 0,
                   Operation => IO.Captured.Operation);
      end if;
   end Dispatch;
   procedure Confirm_Completion
     (IO : in out Context; Expected : AML_Field_Access.Observation;
      Terminal_Evidence : Boolean; Accepted : out Boolean) is
   begin
      Accepted := False;
      if IO.Dispatching or else not Terminal_Evidence or else not Expected.Present
        or else IO.Captured /= Expected or else IO.Pending_Ticket = 0
        or else IO.Pending_Slot = 0
        or else IO.Reserved_Confirmation = 0
        or else IO.Confirmed (IO.Reserved_Confirmation).Present
        or else Find (IO, Expected.Item.Destination.Register) /= IO.Pending_Slot
        or else Expected.Provider /= IO.Identity
        or else Expected.Item.Destination.Provider /= IO.Identity
        or else Expected.Item.Destination.Endpoint /= IO.Endpoint
        or else Expected.Item.Destination.Epoch /=
          ACPI_Region_Policy.Epoch (IO.Registers (IO.Pending_Slot).Region)
      then return; end if;
      ACPI_Region_Policy.Finish_Access
        (IO.Registers (IO.Pending_Slot).Region, IO.Pending_Ticket, Accepted);
      if Accepted then
         IO.Confirmed (IO.Reserved_Confirmation) := IO.Captured;
         IO.Reserved_Confirmation := 0;
         IO.Captured := AML_Field_Access.No_Observation;
         IO.Pending_Ticket := 0; IO.Pending_Slot := 0;
      end if;
   end Confirm_Completion;
   procedure Reconciled
     (IO : Context; Expected : AML_Field_Access.Observation; Accepted : out Boolean) is
   begin
      Accepted := Expected.Present and then not IO.Dispatching
        and then (for some C of IO.Confirmed => C = Expected);
      -- This is a historical provider fact, independent of a later pending
      -- transaction. The arena owns its separate reconciliation decision.
   end Reconciled;
   procedure Retire_Confirmation
     (IO : in out Context; Expected : AML_Field_Access.Observation;
      Namespace_Reconciled : Boolean; Accepted : out Boolean) is
   begin
      Accepted := False;
      if IO.Dispatching or else not Namespace_Reconciled or else not Expected.Present then
         return;
      end if;
      for C in IO.Confirmed'Range loop
         if IO.Confirmed (C) = Expected then
            IO.Confirmed (C) := AML_Field_Access.No_Observation;
            Accepted := True;
            return;
         end if;
      end loop;
      -- Never alter the current reservation, pending observation or policy.
   end Retire_Confirmation;
end ACPI_Field_Hosted_Provider;
