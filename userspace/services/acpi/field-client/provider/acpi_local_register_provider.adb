pragma Ada_2022;
with AML_Identity.Issuer;
with AML_Register_Views;
with AML_Field_Datums;
package body ACPI_Local_Register_Provider with SPARK_Mode is
   package P renames ACPI_Inventory_Protocol;
   use type Interfaces.Unsigned_64, Catalog.Revision_Number, Catalog.P.Address_Space;
   use type A.Endpoint_ID, A.Register_ID, A.Operation_ID, AML_Identity.Identity;
   function Status (IO : Context) return Phase is (IO.Stage);
   function Pending (IO : Context) return Boolean is
     (IO.Working or else IO.Captured.Present or else Catalog.Observe (Hardware).Working
      or else Catalog.Observe (Hardware).Pending /= 0);
   function Is_Ready (IO : Context) return Boolean is
      Header : constant Catalog.Inventory := Catalog.Header (Hardware, Scope);
   begin
      return IO.Stage = Ready and then not Pending (IO)
        and then Header.Available and then Header.Revision = IO.Revision;
   end Is_Ready;
   function Identity (IO : Context) return AML_Identity.Identity is
     (if IO.Stage = Ready then IO.Issued else AML_Identity.No_Identity);
   function Uncertain (IO : Context) return A.Observation is (IO.Captured);
   function Completed_Fault (IO : Context) return A.Observation is (IO.Fault);
   function Register_Count (IO : Context) return Natural is
     (if Is_Ready (IO) then IO.Count else 0);
   function Target_At (IO : Context; Slot : Index) return A.Target is
     ((IO.Issued, IO.Symbol, IO.Items (Slot).Description.Epoch,
       A.Register_ID (IO.Items (Slot).Description.ID)));
   function Find (IO : Context; Target : A.Target) return Natural is
   begin
      if not Is_Ready (IO) or else Target.Provider = AML_Identity.No_Identity
        or else Target.Provider /= IO.Issued or else Target.Endpoint /= IO.Symbol
      then return 0; end if;
      for Slot in 1 .. IO.Count loop
         if Target.Register = A.Register_ID (IO.Items (Slot).Description.ID)
           and then Target.Epoch = IO.Items (Slot).Description.Epoch then return Slot; end if;
      end loop;
      return 0;
   end Find;
   procedure Initialize (IO : in out Context; Symbol : A.Endpoint_ID; Accepted : out Boolean) is
      Header : constant Catalog.Inventory := Catalog.Header (Hardware, Scope);
      Issued : Boolean;
   begin
      Accepted := False;
      if IO.Stage /= Fresh then return; end if;
      IO.Stage := Initializing;
      if Symbol = A.No_Endpoint or else not Header.Available
        or else Header.Count > Maximum_Registers or else Pending (IO)
      then IO.Stage := Failed; return; end if;
      for Slot in 1 .. Header.Count loop
         declare
            D : constant Catalog.Description := Catalog.Describe (Hardware, Scope, Header.Revision, Slot);
            Value : P.Description;
         begin
            if not D.Available then IO.Stage := Failed; return; end if;
            Value := (ID => D.Item.Register_Authority.Resource_ID, Epoch => D.Epoch,
               Base => D.Item.Base, Length => D.Item.Length, Offset => D.Item.Register_Offset,
               Space => (if D.Item.Space = Catalog.P.Memory_Space then P.Memory_Space else P.IO_Space),
               Readable => D.Item.Register_Authority.Readable, Writable => D.Item.Register_Authority.Writable,
               Delegable => D.Item.Register_Authority.Delegable, Exact_Write_Safe => D.Exact_Write_Safe,
               Widths => P.Width_Set (D.Item.Widths), Width => D.Item.Register_Width,
               Write_Mask => D.Item.Register_Authority.Write_Mask);
            -- Reuse the pure descriptor predicate, never its wire codec.
            if not P.Valid (Value) then IO.Stage := Failed; return; end if;
            for Prior in 1 .. Slot - 1 loop
               if IO.Items (Prior).Description.ID = Value.ID then IO.Stage := Failed; return; end if;
            end loop;
            IO.Items (Slot) := (Value,
               (Region => (Space => (if D.Item.Space = Catalog.P.Memory_Space then L.Memory_Space else L.IO_Space),
                  Base => Value.Base, Length => Value.Length, PCI => (Known => False)),
                Byte_Offset => Value.Offset, Width => Value.Width));
            if not AML_Register_Views.Valid_Register (IO.Items (Slot).Geometry) then
               IO.Stage := Failed; return;
            end if;
         end;
      end loop;
      declare Last : constant Catalog.Inventory := Catalog.Header (Hardware, Scope); begin
         if not Last.Available or else Last.Revision /= Header.Revision
           or else Last.Count /= Header.Count then IO.Stage := Failed; return; end if;
      end;
      AML_Identity.Issuer.Issue (IO.Issued, Issued);
      if not Issued then IO.Stage := Failed; return; end if;
      IO.Symbol := Symbol; IO.Revision := Header.Revision; IO.Count := Header.Count;
      IO.Stage := Ready; Accepted := True;
   end Initialize;
   procedure Export_Target (IO : Context; Ordinal : Positive;
      Target : out A.Target; Accepted : out Boolean) is
   begin
      Target := A.No_Target; Accepted := Is_Ready (IO) and then Ordinal <= IO.Count;
      if Accepted then Target := Target_At (IO, Ordinal); end if;
   end Export_Target;
   procedure Describe (IO : Context; Target : A.Target;
      Geometry : out L.Register_Geometry; Accepted : out Boolean) is
      Slot : constant Natural := Find (IO, Target);
   begin
      Geometry := (others => <>); Accepted := Slot /= 0;
      if Accepted then Geometry := IO.Items (Slot).Geometry; end if;
   end Describe;
   procedure Authorize (IO : Context; Target : A.Target;
      For_Write : Boolean; Accepted : out Boolean) is
      Slot : constant Natural := Find (IO, Target);
   begin
      Accepted := Slot /= 0 and then IO.Sequence /= A.Operation_ID'Last;
      if Accepted then
         declare D : constant P.Description := IO.Items (Slot).Description; begin
            Accepted := (if For_Write then D.Writable and then D.Exact_Write_Safe else D.Readable);
         end;
      end if;
   end Authorize;
   procedure Select_Target (IO : in out Context; Geometry : L.Register_Geometry;
      For_Write : Boolean; Target : out A.Target; Accepted : out Boolean) is
      Allowed : Boolean;
   begin
      Target := A.No_Target; Accepted := False;
      if not Is_Ready (IO) then return; end if;
      for Slot in 1 .. IO.Count loop
         if AML_Register_Views.Same_Transaction (IO.Items (Slot).Geometry, Geometry) then
            Authorize (IO, Target_At (IO, Slot), For_Write, Allowed);
            if Allowed then
               if Accepted then Target := A.No_Target; Accepted := False; return; end if;
               Target := Target_At (IO, Slot); Accepted := True;
            end if;
         end if;
      end loop;
   end Select_Target;
   procedure Dispatch (IO : in out Context; Item : A.Request; Reply : out A.Response) is
      Accepted : Boolean;
      Slot : Natural;
      Result : Catalog.Register_Response;
   begin
      Reply := (others => <>);
      Authorize (IO, Item.Destination, Item.For_Write, Accepted);
      if not Accepted then return; end if;
      Slot := Find (IO, Item.Destination);
      if Slot = 0 or else (not Item.For_Write and then Item.Value /= 0)
        or else Item.Value > AML_Field_Datums.Width_Mask (IO.Items (Slot).Geometry.Width)
        or else (Item.For_Write and then
          (Item.Value and not IO.Items (Slot).Description.Write_Mask) /= 0)
      then return; end if;
      IO.Sequence := IO.Sequence + 1; IO.Working := True;
      Catalog.Execute_Register (Hardware, Scope,
        (Catalog.Register_ID (Item.Destination.Register), Catalog.Record_Epoch (Item.Destination.Epoch),
         Item.For_Write, Item.Value), Result);
      IO.Working := False;
      case Result.Status is
         when Catalog.Wire.Denied | Catalog.Wire.Malformed => null;
         when Catalog.Wire.Done =>
            Reply := (A.Completed, IO.Issued, Result.Value, A.No_Operation);
         when Catalog.Wire.Backend_Fault =>
            IO.Fault := (True, IO.Issued, Item, A.No_Operation);
            Reply := (Status => A.Failed_Completed, Provider => IO.Issued, others => <>);
         when Catalog.Wire.Indeterminate =>
            IO.Captured := (True, IO.Issued, Item, IO.Sequence);
            IO.Hardware_Ticket := Result.Pending_Ticket;
            Reply := (A.Indeterminate, IO.Issued, 0, IO.Sequence);
      end case;
   end Dispatch;
   procedure Reconciled (IO : Context; Expected : A.Observation; Accepted : out Boolean) is
      pragma Unreferenced (IO, Expected);
   begin Accepted := False; end Reconciled;
end ACPI_Local_Register_Provider;
