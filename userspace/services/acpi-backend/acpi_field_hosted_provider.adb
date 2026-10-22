pragma Ada_2022;
with ACPI_FADT.Transactions;
with ACPI_Region_Protocol;
with AML_Identity.Issuer;
package body ACPI_Field_Hosted_Provider is
   use type Interfaces.Unsigned_64;
   use type AML_Identity.Identity;
   use type AML_Field_Access.Endpoint_ID;
   use type AML_Field_Access.Register_ID;
   use type AML_Field_Access.Observation;
   use type ACPI_Region_Policy.Address_Space;
   function Identity (IO : Context) return AML_Identity.Identity is (IO.Identity);
   function Pending (IO : Context) return Boolean is
     (IO.Captured.Present or else ACPI_Region_Policy.Busy (IO.Region));
   function Uncertain (IO : Context) return AML_Field_Access.Observation is
     (IO.Captured);
   function Completed_Fault (IO : Context) return AML_Field_Access.Observation is
     (IO.Fault);
   function Observe_Hardware (IO : Context) return Hardware_State is (IO.Hardware);
   procedure Install
     (IO : in out Context; Endpoint : AML_Field_Access.Endpoint_ID;
      Configuration : ACPI_Region_Policy.Configuration;
      Exact_Write_Safe : Boolean; Accepted : out Boolean) is
      Issued : Boolean;
   begin
      Accepted := False;
      if Pending (IO) or else Endpoint = AML_Field_Access.No_Endpoint then return; end if;
      if ACPI_Region_Policy.Active (IO.Region)
        or else not ACPI_Region_Policy.Valid (Configuration)
        or else ACPI_Region_Policy.Epoch (IO.Region) = Interfaces.Unsigned_64'Last
      then return; end if;
      if IO.Identity = AML_Identity.No_Identity then
         AML_Identity.Issuer.Issue (IO.Identity, Issued);
         if not Issued then return; end if;
      end if;
      ACPI_Region_Policy.Install (IO.Region, Configuration, Accepted);
      if Accepted then
         IO.Endpoint := Endpoint; IO.Exact_Write_Safe := Exact_Write_Safe;
      end if;
   end Install;
   procedure Describe
     (IO : Context; Target : AML_Field_Access.Target;
      Geometry : out AML_Field_Layout.Register_Geometry; Accepted : out Boolean) is
      C : constant ACPI_Region_Policy.Configuration := ACPI_Region_Policy.Policy (IO.Region);
   begin
      Geometry := (others => <>); Accepted := False;
      if Pending (IO) or else not ACPI_Region_Policy.Active (IO.Region)
        or else Target.Provider = AML_Identity.No_Identity
        or else Target.Provider /= IO.Identity
        or else Target.Endpoint = AML_Field_Access.No_Endpoint
        or else Target.Endpoint /= IO.Endpoint
        or else Target.Epoch /= ACPI_Region_Policy.Epoch (IO.Region)
        or else Target.Register = AML_Field_Access.No_Register
        or else Interfaces.Unsigned_64 (Target.Register) /= C.Register_Authority.Resource_ID
      then return; end if;
      Geometry := (Region => (Space => (if C.Space = ACPI_Region_Policy.Memory_Space
        then AML_Field_Layout.Memory_Space else AML_Field_Layout.IO_Space),
        Base => C.Base, Length => C.Length),
        Byte_Offset => C.Register_Offset, Width => C.Register_Width);
      Accepted := True;
   end Describe;
   procedure Dispatch
     (IO : in out Context; Item : AML_Field_Access.Request;
      Reply : out AML_Field_Access.Response) is
      Geometry : AML_Field_Layout.Register_Geometry;
      Accepted : Boolean;
      Packet, Answer : ACPI_Region_Protocol.Packet;
      Ticket : Interfaces.Unsigned_64;
      C : constant ACPI_Region_Policy.Configuration := ACPI_Region_Policy.Policy (IO.Region);
      Full_Mask : Interfaces.Unsigned_64;
   begin
      Reply := (others => <>);
      Describe (IO, Item.Destination, Geometry, Accepted);
      if not Accepted then return; end if;
      -- Full-register exact writes require explicit platform safety admission.
      -- Readable+Writable is never treated as RMW permission.
      Full_Mask := (case C.Register_Width is
         when ACPI_FADT.Transactions.Byte_Access => 16#FF#,
         when ACPI_FADT.Transactions.Word_Access => 16#FFFF#,
         when ACPI_FADT.Transactions.Dword_Access => 16#FFFF_FFFF#,
         when ACPI_FADT.Transactions.Qword_Access => Interfaces.Unsigned_64'Last);
      if Item.For_Write and then (not IO.Exact_Write_Safe
        or else C.Register_Authority.Write_Mask /= Full_Mask) then return; end if;
      Packet := (Label => (if Item.For_Write then Backend.Write_Operation else Backend.Read_Operation),
        Data => [Item.Destination.Epoch, Interfaces.Unsigned_64 (Item.Destination.Register),
                 Item.Value, 0], others => <>);
      -- The stamp is trusted backend configuration, never supplied by AML.
      Backend.Dispatch (IO.Region, IO.Hardware, C.Tag, Packet, Answer, Ticket);
      if Answer.Data (0) = Backend.Outcome'Pos (Backend.Done) then
         Reply.Status := AML_Field_Access.Completed;
         Reply.Value := Answer.Data (1) or Interfaces.Shift_Left (Answer.Data (2), 32);
      elsif Answer.Data (0) = Backend.Outcome'Pos (Backend.Backend_Fault) then
         -- Region_IO has already completed and revoked this access. Do not
         -- create a pending receipt or call Finish for a terminal fault.
         IO.Fault := (Present => True, Provider => IO.Identity, Item => Item,
                      Operation => AML_Field_Access.No_Operation);
         Reply := (Status => AML_Field_Access.Failed_Completed,
                   Provider => IO.Identity, Value => 0,
                   Operation => AML_Field_Access.No_Operation);
      elsif Answer.Data (0) = Backend.Outcome'Pos (Backend.Indeterminate) then
         IO.Pending_Ticket := Ticket;
         IO.Captured := (Present => True, Provider => IO.Identity, Item => Item,
            Operation => AML_Field_Access.Operation_ID (Ticket));
         -- Local adapter correlation deliberately derived from the private
         -- receipt; it is not transmitted as a Finish-capable wire ticket.
         Reply := (Status => AML_Field_Access.Indeterminate, Provider => IO.Identity, Value => 0,
                   Operation => IO.Captured.Operation);
      end if;
   end Dispatch;
   procedure Confirm_Completion
     (IO : in out Context; Expected : AML_Field_Access.Observation;
      Terminal_Evidence : Boolean; Accepted : out Boolean) is
   begin
      Accepted := False;
      if not Terminal_Evidence or else not Expected.Present
        or else IO.Captured /= Expected or else IO.Pending_Ticket = 0 then return; end if;
      ACPI_Region_Policy.Finish_Access (IO.Region, IO.Pending_Ticket, Accepted);
      if Accepted then
         IO.Confirmed := IO.Captured;
         IO.Captured := AML_Field_Access.No_Observation;
         IO.Pending_Ticket := 0;
      end if;
   end Confirm_Completion;
   procedure Reconciled
     (IO : Context; Expected : AML_Field_Access.Observation; Accepted : out Boolean) is
   begin
      Accepted := Expected.Present and then not Pending (IO) and then IO.Confirmed = Expected;
   end Reconciled;
end ACPI_Field_Hosted_Provider;
