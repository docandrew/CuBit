pragma Ada_2022;
with AML_Register_Views;
package body ACPI_Field_Inventory_Owner is
   package A renames AML_Field_Access;
   package L renames AML_Field_Layout;
   use type A.Endpoint_ID;
   function Status (IO : Context) return Startup_State is (IO.Stage);
   function Identity (IO : Context) return AML_Identity.Identity is
     (if IO.Stage = Ready then Provider.Identity (IO.Lower) else AML_Identity.No_Identity);
   function Target_At (IO : Context; Index : Inventory.Register_Index) return A.Target is
     ((Provider.Identity (IO.Lower), IO.Symbol,
       IO.Entries (Index).Epoch, IO.Entries (Index).Register));
   function Register_Count (IO : Context) return Natural is
     (if IO.Stage = Ready then IO.Count else 0);
   procedure Export_Target
     (IO : Context; Ordinal : Positive; Target : out A.Target; Accepted : out Boolean)
   is
      Geometry : L.Register_Geometry;
   begin
      Target := A.No_Target; Accepted := False;
      if IO.Stage /= Ready or else Ordinal > IO.Count then return; end if;
      Provider.Describe (IO.Lower, Target_At (IO, Ordinal), Geometry, Accepted);
      if Accepted then Target := Target_At (IO, Ordinal); end if;
   end Export_Target;
   procedure Initialize
     (IO : in out Context; Destination : Endpoint; Symbol : A.Endpoint_ID;
      Deadline : ACPI_Inventory_Protocol.Wire.Time_Instant; Accepted : out Boolean)
   is
      Copy : Inventory.Snapshot;
      Entries : Descriptors;
      Installed : Boolean;
   begin
      Accepted := False;
      if IO.Stage /= Fresh then return; end if;
      IO.Stage := Initializing;
      if Destination = No_Endpoint or else Symbol = A.No_Endpoint then
         IO.Stage := Failed; return;
      end if;
      Inventory.Load (IO.Metadata, Destination, Deadline, Copy);
      if not Copy.Complete then IO.Stage := Failed; return; end if;
      for Index in Inventory.Register_Index loop
         exit when Index > Copy.Count;
         declare Item : constant ACPI_Inventory_Protocol.Description := Copy.Items (Index); begin
            Entries (Index) :=
              (Register => A.Register_ID (Item.ID), Epoch => Item.Epoch,
               Geometry => (Region =>
                 (Space => (case Item.Space is
                    when ACPI_Inventory_Protocol.Memory_Space => L.Memory_Space,
                    when ACPI_Inventory_Protocol.IO_Space => L.IO_Space,
                    when ACPI_Inventory_Protocol.Embedded_Control_Space => L.Embedded_Control_Space),
                  Base => Item.Base, Length => Item.Length, PCI => (Known => False)),
                 Byte_Offset => Item.Offset, Width => Item.Width),
               Readable => Item.Readable, Writable => Item.Writable,
               Exact_Write_Safe => Item.Exact_Write_Safe, Write_Mask => Item.Write_Mask);
            if not Provider.Valid (Entries (Index)) then IO.Stage := Failed; return; end if;
         end;
      end loop;
      Provider.Initialize (IO.Lower, Destination, Symbol, Installed);
      if not Installed then IO.Stage := Failed; return; end if;
      for Index in Inventory.Register_Index loop
         exit when Index > Copy.Count;
         Provider.Install (IO.Lower, Entries (Index), Installed);
         if not Installed then IO.Stage := Failed; return; end if;
      end loop;
      IO.Entries := Entries; IO.Count := Copy.Count; IO.Symbol := Symbol;
      IO.Stage := Ready; Accepted := True;
   end Initialize;
   procedure Select_Target
     (IO : Context; Geometry : L.Register_Geometry; For_Write : Boolean;
      Target : out A.Target; Accepted : out Boolean)
   is
      Allowed : Boolean;
   begin
      Target := A.No_Target; Accepted := False;
      if IO.Stage /= Ready then return; end if;
      for Index in Inventory.Register_Index loop
         exit when Index > IO.Count;
         if AML_Register_Views.Same_Transaction (IO.Entries (Index).Geometry, Geometry) then
            Provider.Authorize (IO.Lower, Target_At (IO, Index), For_Write, Allowed);
            if Allowed then
               if Accepted then Target := A.No_Target; Accepted := False; return; end if;
               Target := Target_At (IO, Index); Accepted := True;
            end if;
         end if;
      end loop;
   end Select_Target;
   procedure Describe
     (IO : Context; Target : A.Target; Geometry : out L.Register_Geometry; Accepted : out Boolean) is
   begin
      Geometry := (others => <>); Accepted := False;
      if IO.Stage = Ready then Provider.Describe (IO.Lower, Target, Geometry, Accepted); end if;
   end Describe;
   procedure Authorize
     (IO : Context; Target : A.Target; For_Write : Boolean; Accepted : out Boolean) is
   begin
      Accepted := False;
      if IO.Stage = Ready then Provider.Authorize (IO.Lower, Target, For_Write, Accepted); end if;
   end Authorize;
   procedure Dispatch (IO : in out Context; Item : A.Request; Reply : out A.Response) is
   begin
      Reply := (others => <>);
      if IO.Stage = Ready then Provider.Dispatch (IO.Lower, Item, Reply); end if;
   end Dispatch;
   procedure Revoke_Register (IO : in out Context; Target : A.Target; Accepted : out Boolean) is
   begin
      Accepted := False;
      if IO.Stage = Ready then Provider.Revoke_Register (IO.Lower, Target, Accepted); end if;
   end Revoke_Register;
   function Pending (IO : Context) return Boolean is
     (IO.Stage = Ready and then Provider.Pending (IO.Lower));
   function Uncertain (IO : Context) return A.Observation is
     (if IO.Stage = Ready then Provider.Uncertain (IO.Lower) else A.No_Observation);
   function Completed_Fault (IO : Context) return A.Observation is
     (if IO.Stage = Ready then Provider.Completed_Fault (IO.Lower) else A.No_Observation);
   procedure Confirm_Completion
     (IO : in out Context; Expected : A.Observation; Terminal_Evidence : Boolean; Accepted : out Boolean) is
   begin
      Accepted := False;
      if IO.Stage = Ready then Provider.Confirm_Completion (IO.Lower, Expected, Terminal_Evidence, Accepted); end if;
   end Confirm_Completion;
   procedure Reconciled (IO : Context; Expected : A.Observation; Accepted : out Boolean) is
   begin
      Accepted := False;
      if IO.Stage = Ready then Provider.Reconciled (IO.Lower, Expected, Accepted); end if;
   end Reconciled;
   procedure Retire_Confirmation
     (IO : in out Context; Expected : A.Observation; Namespace_Reconciled : Boolean; Accepted : out Boolean) is
   begin
      Accepted := False;
      if IO.Stage = Ready then Provider.Retire_Confirmation (IO.Lower, Expected, Namespace_Reconciled, Accepted); end if;
   end Retire_Confirmation;
end ACPI_Field_Inventory_Owner;
