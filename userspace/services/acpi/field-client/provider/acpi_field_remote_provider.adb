pragma Ada_2022;
with AML_Decode;
with AML_Identity.Issuer;
with AML_Field_Datums;
with ACPI_FADT.Transactions;
package body ACPI_Field_Remote_Provider with SPARK_Mode is
   package A renames AML_Field_Access;
   package L renames AML_Field_Layout;
   package T renames ACPI_FADT.Transactions;
   use type Interfaces.Unsigned_64;
   use type AML_Identity.Identity;
   use type A.Endpoint_ID;
   use type A.Register_ID;
   use type A.Observation;
   use type T.Access_Width;
   use type AML_Decode.Byte;
   function Valid (Item : Descriptor) return Boolean is
      G : constant L.Register_Geometry := Item.Geometry;
      Octets : constant Interfaces.Unsigned_64 := Interfaces.Unsigned_64 (T.Octets (G.Width));
   begin
      return Item.Register /= A.No_Register and then Item.Epoch /= 0
        and then (Item.Readable or else Item.Writable)
        and then (Item.Writable or else Item.Write_Mask = 0)
        and then Item.Write_Mask <= AML_Field_Datums.Width_Mask (G.Width)
        and then G.Region.Space in L.Memory_Space | L.IO_Space
        and then G.Region.Length /= 0
        and then G.Region.Length - 1 <= Interfaces.Unsigned_64'Last - G.Region.Base
        and then G.Byte_Offset < G.Region.Length
        and then Octets <= G.Region.Length - G.Byte_Offset
        and then T.Aligned (G.Region.Base + G.Byte_Offset, G.Width)
        and then (if G.Region.Space = L.IO_Space then
          G.Region.Base <= L.IO_Last_Address
          and then G.Region.Length - 1 <= L.IO_Last_Address - G.Region.Base
          and then G.Width /= T.Qword_Access);
   end Valid;
   function Identity (IO : Context) return AML_Identity.Identity is (IO.Identity);
   function Pending (IO : Context) return Boolean is
     (IO.Dispatching or else IO.Captured.Present or else Client.Pending (IO.Lower));
   function Uncertain (IO : Context) return A.Observation is (IO.Captured);
   function Completed_Fault (IO : Context) return A.Observation is (IO.Fault);
   function Find (IO : Context; Register : A.Register_ID) return Slot_Count is
   begin
      if Register = A.No_Register then return 0; end if;
      for S in IO.Entries'Range loop
         if IO.Entries (S).Used and then IO.Entries (S).Item.Register = Register then return S; end if;
      end loop;
      return 0;
   end Find;
   function Matches (IO : Context; Target : A.Target; Slot : Slot_Count) return Boolean is
     (IO.Initialized and then Slot /= 0 and then Target.Provider = IO.Identity
      and then Target.Provider /= AML_Identity.No_Identity and then Target.Endpoint = IO.Symbol
      and then Target.Register = IO.Entries (Slot).Item.Register
      and then Target.Epoch = IO.Entries (Slot).Item.Epoch);
   function Free_Confirmation (IO : Context) return Confirmation_Count is
   begin
      for C in IO.Confirmed'Range loop
         if not IO.Confirmed (C).Present then return C; end if;
      end loop;
      return 0;
   end Free_Confirmation;
   procedure Initialize
     (IO : in out Context; Destination : Endpoint; Symbol : A.Endpoint_ID; Accepted : out Boolean) is
      Issued : Boolean;
   begin
      Accepted := False;
      if IO.Initialized or else Destination = No_Endpoint or else Symbol = A.No_Endpoint then return; end if;
      AML_Identity.Issuer.Issue (IO.Identity, Issued);
      if not Issued then return; end if;
      Client.Initialize (IO.Lower, Destination, Accepted);
      if Accepted then IO.Initialized := True; IO.Symbol := Symbol; end if;
   end Initialize;
   procedure Install (IO : in out Context; Item : Descriptor; Accepted : out Boolean) is
      Slot : Slot_Count;
   begin
      Accepted := False;
      if not IO.Initialized or else Pending (IO) or else not Valid (Item) then return; end if;
      Slot := Find (IO, Item.Register);
      if Slot = 0 then
         for S in IO.Entries'Range loop
            if not IO.Entries (S).Used then Slot := S; exit; end if;
         end loop;
      elsif IO.Entries (Slot).Active or else Item.Epoch <= IO.Entries (Slot).Item.Epoch then return;
      end if;
      if Slot = 0 then return; end if;
      IO.Entries (Slot) := (Used => True, Active => True, Item => Item); Accepted := True;
   end Install;
   procedure Revoke_Register
     (IO : in out Context; Target : A.Target; Accepted : out Boolean) is
      Slot : constant Slot_Count := Find (IO, Target.Register);
   begin
      Accepted := not IO.Dispatching and then Matches (IO, Target, Slot)
        and then IO.Entries (Slot).Active;
      if Accepted then IO.Entries (Slot).Active := False; end if;
   end Revoke_Register;
   procedure Describe
     (IO : Context; Target : A.Target; Geometry : out L.Register_Geometry; Accepted : out Boolean) is
      Slot : constant Slot_Count := Find (IO, Target.Register);
   begin
      Geometry := (others => <>);
      Accepted := not Pending (IO) and then Matches (IO, Target, Slot)
        and then IO.Entries (Slot).Active;
      if Accepted then Geometry := IO.Entries (Slot).Item.Geometry; end if;
   end Describe;
   procedure Authorize
     (IO : Context; Target : A.Target; For_Write : Boolean; Accepted : out Boolean) is
      Slot : constant Slot_Count := Find (IO, Target.Register);
   begin
      Accepted := not Pending (IO) and then Free_Confirmation (IO) /= 0
        and then Matches (IO, Target, Slot) and then IO.Entries (Slot).Active;
      if Accepted then
         declare
            D : constant Descriptor := IO.Entries (Slot).Item;
         begin
            Accepted := (if For_Write then D.Writable and then D.Exact_Write_Safe
              and then D.Write_Mask = AML_Field_Datums.Width_Mask (D.Geometry.Width)
              else D.Readable);
         end;
      end if;
   end Authorize;
   procedure Dispatch (IO : in out Context; Item : A.Request; Reply : out A.Response) is
      Accepted : Boolean;
      Slot : Slot_Count;
      Now, Deadline : ACPI_Region_Protocol.Time_Instant;
      Result : Client.Result;
   begin
      Reply := (others => <>);
      Authorize (IO, Item.Destination, Item.For_Write, Accepted);
      if not Accepted then return; end if;
      Slot := Find (IO, Item.Destination.Register);
      if Slot = 0 then return; end if;
      if (not Item.For_Write and then Item.Value /= 0)
        or else Item.Value > AML_Field_Datums.Width_Mask (IO.Entries (Slot).Item.Geometry.Width)
      then return; end if;
      IO.Reserved := Free_Confirmation (IO);
      IO.Dispatching := True;
      Read_Window (Now, Deadline);
      Client.Dispatch (IO.Lower, IO.Transport, Now, Deadline,
        (Epoch => ACPI_Region_Protocol.Epoch_Number (Item.Destination.Epoch),
         Register => ACPI_Region_Protocol.Register_ID (Item.Destination.Register),
         For_Write => Item.For_Write, Value => Item.Value), Result);
      IO.Dispatching := False;
      case Result.Status is
         when Client.Rejected => null;
         when Client.Completed =>
            if Result.Value > AML_Field_Datums.Width_Mask (IO.Entries (Slot).Item.Geometry.Width) then
               IO.Entries (Slot).Active := False;
               IO.Fault := (Present => True, Provider => IO.Identity, Item => Item,
                 Operation => A.No_Operation);
               Reply := (Status => A.Failed_Completed, Provider => IO.Identity, others => <>);
            else
               Reply := (Status => A.Completed, Provider => IO.Identity,
                 Value => Result.Value, Operation => A.No_Operation);
            end if;
         when Client.Failed_Completed =>
            IO.Entries (Slot).Active := False;
            IO.Fault := (Present => True, Provider => IO.Identity, Item => Item, Operation => A.No_Operation);
            Reply := (Status => A.Failed_Completed, Provider => IO.Identity, others => <>);
         when Client.Indeterminate =>
            IO.Lower_Receipt := Client.Uncertain (IO.Lower);
            IO.Captured := (Present => True, Provider => IO.Identity, Item => Item,
              Operation => A.Operation_ID (Result.Operation));
            Reply := (Status => A.Indeterminate, Provider => IO.Identity,
              Value => 0, Operation => IO.Captured.Operation);
            return;
      end case;
      IO.Reserved := 0;
   end Dispatch;
   procedure Confirm_Completion
     (IO : in out Context; Expected : A.Observation; Terminal_Evidence : Boolean; Accepted : out Boolean) is
      Retired : Boolean;
   begin
      Accepted := False;
      if IO.Dispatching or else not Expected.Present or else Expected /= IO.Captured
        or else not Terminal_Evidence or else IO.Reserved = 0
        or else IO.Confirmed (IO.Reserved).Present
      then return; end if;
      Client.Confirm_Completion (IO.Lower, IO.Lower_Receipt, True, Accepted);
      if not Accepted then return; end if;
      -- The provider consumes the inner fact; the arena consumes this outer
      -- history later. Retirement below performs no IPC and cannot replay.
      Client.Retire_Confirmation (IO.Lower, IO.Lower_Receipt, True, Retired);
      Accepted := Retired;
      if Accepted then
         IO.Confirmed (IO.Reserved) := IO.Captured;
         IO.Captured := A.No_Observation;
         IO.Lower_Receipt := Client.No_Observation;
         IO.Reserved := 0;
      end if;
   end Confirm_Completion;
   procedure Reconciled (IO : Context; Expected : A.Observation; Accepted : out Boolean) is
   begin
      Accepted := not IO.Dispatching and then Expected.Present
        and then (for some C of IO.Confirmed => C = Expected);
   end Reconciled;
   procedure Retire_Confirmation
     (IO : in out Context; Expected : A.Observation; Namespace_Reconciled : Boolean; Accepted : out Boolean) is
   begin
      Accepted := False;
      if IO.Dispatching or else not Namespace_Reconciled or else not Expected.Present then return; end if;
      for C in IO.Confirmed'Range loop
         if IO.Confirmed (C) = Expected then
            IO.Confirmed (C) := A.No_Observation; Accepted := True; return;
         end if;
      end loop;
   end Retire_Confirmation;
end ACPI_Field_Remote_Provider;
