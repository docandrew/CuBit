pragma Ada_2022;
with Ada.Command_Line;
with Ada.Text_IO;
with GNAT.Source_Info;
with Interfaces; use Interfaces;
with AML_Field_Access;
with AML_Field_Layout;
with AML_Identity;
with ACPI_Backend_Binding_Types;
with ACPI_Backend_Catalog;
with ACPI_Backend_NVS_Records;
with ACPI_Local_NVS_Provider;
with ACPI_Local_Register_Provider;
with ACPI_Platform_Field_Provider;
with ACPI_Platform_Region_Cache;
with ACPI_Local_Field_Mux;
with Empty_PCI_Fixture;
with Platform_Pool_ABI;
with ACPI_Firmware_Data_Layouts;
procedure NVS_Mux_Authority_Tests is
   package A renames AML_Field_Access;
   package L renames AML_Field_Layout;
   package F renames ACPI_Firmware_Data_Layouts;
   package B renames ACPI_Backend_Binding_Types;
   subtype Word is Unsigned_64;
   use type A.Request,A.Target,A.Completion,AML_Identity.Identity,B.Outcome;
   Base : constant Word := 16#8000_0000#;
   VA : constant Word := 16#6000_0000_0000#;
   Scope : constant Word := 16#F123_4567_0000_0001#;
   Mode : constant String := Ada.Command_Line.Argument (1);
   Checks,Effects,Pool_Maps,Pool_Reads,Validations : Natural := 0;
   Gate,Class_Valid : Boolean := True;
   Reenter : Boolean := Mode = "reentry";
   procedure Nested;
   procedure Check (Good : Boolean; Where : String := GNAT.Source_Info.Source_Location) is
   begin Checks := Checks+1; if not Good then raise Program_Error with Mode & Checks'Image & " " & Where; end if; end Check;
   procedure Inspect (Cap : Natural; Info : out B.Description) is
   begin Check (Cap = 1); Info := (True,B.Memory_Resource,Base,8192,True,False,False); end Inspect;
   procedure Map (Cap : Natural; Physical,Virtual,Pages : Word; Read_Only : Boolean; Mapped : out Boolean) is
   begin Check (Cap = 1 and Physical = Base and Virtual = VA and Pages = 2 and Read_Only); Mapped := True; end Map;
   procedure Clock (Now : out B.Tick; Available : out Boolean) is
   begin Now := 1; Available := True; end Clock;
   procedure No_IO (Address : Word; Width : F.T.Access_Width; Writing : Boolean; Input : Word;
      Output : out Word; Completed : out Boolean) is
      pragma Unreferenced (Address,Width,Writing,Input);
   begin Output := 0; Completed := False; Check (False); end No_IO;
   procedure No_Ports (Cap : Natural; Port : Unsigned_16; Width : F.T.Access_Width; Writing : Boolean; Input : Word;
      Output : out Word; Completed : out Boolean) is
      pragma Unreferenced (Cap,Port,Width,Writing,Input);
   begin Output := 0; Completed := False; Check (False); end No_Ports;
   package Catalog is new ACPI_Backend_Catalog
      (Natural,0,Natural,0,Inspect,Inspect,Map,Clock,No_IO,No_Ports,1,4,1,VA,VA+8192,2);
   Hardware : Catalog.Context;
   function Is_NVS (Window : Catalog.W.Window_ID) return Boolean is
      use type Catalog.W.Window_ID;
   begin return Window = 1 and Class_Valid; end Is_NVS;
   procedure Read_NVS (Address : Word; Width : F.T.Access_Width; Value : out Word; Completed : out Boolean) is
      use type F.T.Access_Width;
   begin
      Check (Gate and Address >= VA and Address < VA+8192 and Width = F.T.Byte_Access);
      Effects := Effects+1; Value := (if Mode = "bad-value" then 256 else 16#42#);
      Completed := Mode /= "unknown"; if Reenter then Nested; end if;
   end Read_NVS;
   package Records is new ACPI_Backend_NVS_Records (Catalog,Hardware,Is_NVS,Read_NVS,4);
   Backing : Records.Context;
   function Allowed (Writing : Boolean) return Boolean is
      pragma Unreferenced (Writing);
   begin return Gate; end Allowed;
   package Named is new ACPI_Local_Register_Provider (Catalog,Hardware,Scope,4);
   package Data is new ACPI_Local_NVS_Provider (Records,Backing,Scope,Allowed,1);
   function Pool_Cap (Cap : Natural) return Boolean is (Cap = 2);
   function Exclusive (First,Limit : Word) return Boolean is (First = VA+16#10000# and Limit = VA+16#20000#);
   procedure Pool_Clock (Now : out Word; Available : out Boolean) is
   begin Now := 1; Available := True; end Pool_Clock;
   procedure Pool_Map (Cap : Natural; Physical,Virtual,Pages : Word; Read_Only : Boolean;
      Status : out Word; Why : out Platform_Pool_ABI.Refusal_Record) is
   begin
      Check (Cap = 2 and Physical = Base+16384 and Virtual = VA+16#10000# and Pages = 2 and not Read_Only);
      Pool_Maps := Pool_Maps+1; Status := 0; Why := [others => 0];
   end Pool_Map;
   procedure Pool_IO (Address : Word; Width : F.T.Access_Width; Writing : Boolean; Input : Word;
      Output : out Word; Completed : out Boolean) is
      pragma Unreferenced (Address,Width,Input);
   begin Pool_Reads := Pool_Reads+1; Output := (if Writing then 0 else 16#7E#); Completed := True; end Pool_IO;
   package Cache is new ACPI_Platform_Region_Cache (Natural,0,Pool_Cap,Exclusive,Pool_Map,Pool_Clock,Allowed,Pool_IO,4,8);
   Memory : Cache.Context;
   type Declaration is null record;
   procedure Validate (Item : Declaration; Geometry : L.Register_Geometry;
      Origin : out Cache.Source; Accepted : out Boolean) is
      pragma Unreferenced (Item,Geometry);
   begin Validations := Validations+1; Origin := ("TEST",Scope,0,3); Accepted := True; end Validate;
   function Compatible (First_Page,Pages : Word) return Boolean is
     (First_Page = (Base+16384)/F.Page_Bytes and Pages = 2);
   package Pool is new ACPI_Platform_Field_Provider (Cache,Memory,Declaration,Validate,Compatible);
   package Empty_PCI is new Empty_PCI_Fixture (Allowed);
   package Provider is new ACPI_Local_Field_Mux (Declaration,Named,Pool,Data,Empty_PCI.Provider,Allowed);
   IO,Foreign : Provider.Context;
   Target,T,Pool_Target,Named_Target : A.Target;
   Geometry : L.Register_Geometry := (Region => (L.Memory_Space,Base+16,32,(Known => False)),others => <>);
   Back : L.Register_Geometry;
   Reply : A.Response;
   Bound : B.Report;
   Added : Catalog.Add_Report;
   use type Catalog.Add_Code;
   State : Records.Outcome;
   OK : Boolean;
   Before : Natural;
   use type Records.Outcome,L.Register_Geometry;
   procedure Nested is
      Good : Boolean;
      Inner : A.Target;
      R : A.Response;
   begin
      Provider.Resolve (IO,(null record),Geometry,100,Inner,Good); Check (not Good);
      Provider.Dispatch (IO,(Target,False,0),R); Check (R.Status = A.Denied);
      Provider.Close (IO); Check (Provider.Pending (IO));
   end Nested;
begin
   Catalog.Bind_Memory (Hardware,(1,Catalog.P.Memory_Space,Base,8192,True,False,False),1,100,Bound); Check (Bound.Status = B.Bound);
   Catalog.Authorize_Scope (Hardware,Scope,OK); Check (OK);
   Catalog.Add (Hardware,1,(Tag => Scope,Base => Base,Length => 1,Space => Catalog.P.Memory_Space,
      Readable => True,Writable => False,Widths => [F.T.Byte_Access => True,others => False],
      Register_Authority => (1,True,False,False,0),Register_Offset => 0,Register_Width => F.T.Byte_Access),
      Catalog.W.Exact_Writes_Admitted,Added); Check (Added.Status = Catalog.Added);
   Catalog.Seal (Hardware,OK); Check (OK);
   Records.Configure (Backing,Scope,State); Check (State = Records.Configured);
   if Mode /= "empty" then
      for I in 1 .. (if Mode = "ambiguous" then 2 else 1) loop
         Records.Add (Backing,Records.Record_ID (I),1,State); Check (State = Records.Added);
      end loop;
   end if;
   Records.Seal (Backing,State); Check (State = Records.Sealed);
   Cache.Initialize (Memory,2,VA+16#10000#,VA+16#20000#,OK); Check (OK);
   Provider.Initialize (IO,16#F000_0000_0000_0001#,OK); Check (OK);
   Provider.Resolve (IO,(null record),Geometry,100,Target,OK);
   if Mode = "ambiguous" then Check (not OK and Effects = 0);
   elsif Mode = "empty" then Check (not OK and Effects = 0);
   else
      Check (OK and Target.Provider = Provider.Identity (IO));
      Provider.Resolve (IO,(null record),Geometry,100,T,OK); Check (OK and T = Target and Provider.Register_Count (IO) = 2);
      Provider.Describe (IO,Target,Back,OK); Check (OK and Back = Geometry);
      Provider.Authorize (IO,Target,True,OK); Check (not OK);
      Provider.Select_Target (IO,Geometry,True,T,OK); Check (not OK);
      Provider.Dispatch (IO,(Target,True,1),Reply); Check (Reply.Status = A.Denied and Effects = 0);
      Provider.Dispatch (IO,(Target,False,1),Reply); Check (Reply.Status = A.Denied and Effects = 0);
      Provider.Initialize (Foreign,77,OK); Check (OK);
      Provider.Dispatch (Foreign,(Target,False,0),Reply); Check (Reply.Status = A.Denied and Effects = 0);
      if Mode in "unknown" | "bad-value" then
         declare G : constant L.Register_Geometry := (Region => (L.Memory_Space,Base+16384,8192,(Known => False)),others => <>); begin
            Provider.Resolve (IO,(null record),G,100,Pool_Target,OK); Check (OK and Pool_Maps = 1 and Validations = 1);
         end;
         Provider.Export_Target (IO,1,Named_Target,OK); Check (OK);
      end if;
      if Mode = "guard-denied" then Gate := False; end if;
      if Mode = "revoked" then Class_Valid := False;
      elsif Mode = "stale" then Catalog.Revoke_Scope (Hardware,Scope,OK); Check (OK);
      end if;
      Provider.Dispatch (IO,(Target,False,0),Reply);
      if Mode = "guard-denied" then
         Check (Reply.Status = A.Denied and Effects = 0 and Provider.Pending (IO) and not Provider.Is_Ready (IO));
         Gate := True; Provider.Dispatch (IO,(Target,False,0),Reply); Check (Reply.Status = A.Denied and Effects = 0);
      elsif Mode in "unknown" | "bad-value" then
         Check (Reply.Status = A.Indeterminate and Effects = 1 and Provider.Uncertain (IO).Present);
         Check (Provider.Uncertain (IO).Item = A.Request'(Target,False,0));
         Provider.Reconciled (IO,Provider.Uncertain (IO),OK); Check (not OK);
         Provider.Dispatch (IO,(Target,False,0),Reply); Check (Reply.Status = A.Denied and Effects = 1);
         Provider.Dispatch (IO,(Pool_Target,False,0),Reply); Check (Reply.Status = A.Denied and Pool_Reads = 0);
         Provider.Dispatch (IO,(Named_Target,False,0),Reply); Check (Reply.Status = A.Denied and Effects = 1);
      elsif Mode = "stale" then
         Check (Reply.Status = A.Denied and Effects = 0 and not Provider.Is_Ready (IO));
      elsif Mode = "revoked" then
         Check (Reply.Status = A.Failed_Completed and Provider.Completed_Fault (IO).Present and Effects = 0);
         Check (not Provider.Is_Ready (IO));
      else
         Check (Reply.Status = A.Completed and Reply.Value = 16#42# and Effects = 1);
         for I in 1 .. 64 loop
            Geometry.Byte_Offset := Word (I mod 32); Geometry.Region.Base := Base+16+Word (I/32)*32;
            Provider.Resolve (IO,(null record),Geometry,100,T,OK); Check (OK);
         end loop;
         Check (Provider.Register_Count (IO) > 1);
         Provider.Export_Target (IO,Provider.Register_Count (IO),T,OK); Check (OK);
         Check (Pool_Maps = 0 and Validations = 0);
         Geometry := (Region => (L.Memory_Space,Base+16384,8192,(Known => False)),others => <>);
         Provider.Resolve (IO,(null record),Geometry,100,Pool_Target,OK); Check (OK and Pool_Maps = 1 and Validations = 1);
         Provider.Dispatch (IO,(Pool_Target,True,16#AA#),Reply); Check (Reply.Status = A.Completed and Pool_Reads = 1);
         Geometry := (Region => (L.Memory_Space,Base+16,32,(Known => False)),others => <>);
         Provider.Select_Target (IO,Geometry,True,T,OK); Check (not OK and Pool_Maps = 1 and Validations = 1);
         Provider.Dispatch (IO,(Target,True,16#AA#),Reply); Check (Reply.Status = A.Denied and Pool_Reads = 1);
         Provider.Select_Target (IO,Geometry,False,T,OK); Check (OK and T = Target);
         Geometry := (Region => (L.Memory_Space,Base-1,2,(Known => False)),others => <>);
         Provider.Resolve (IO,(null record),Geometry,100,T,OK); Check (not OK);
         Geometry.Region.Base := Base+8191; Provider.Resolve (IO,(null record),Geometry,100,T,OK); Check (not OK);
         Geometry.Region.Base := Base+8192; Provider.Resolve (IO,(null record),Geometry,100,T,OK); Check (not OK);
         Geometry.Region.Base := Word'Last; Provider.Resolve (IO,(null record),Geometry,100,T,OK); Check (not OK);
         Geometry := (Region => (L.Memory_Space,Base,8192,(Known => True,others => <>)),others => <>);
         Provider.Resolve (IO,(null record),Geometry,100,T,OK); Check (not OK);
         Check (Pool_Maps = 1 and Validations = 2);
         Before := Effects; Provider.Close (IO); Provider.Dispatch (IO,(Target,False,0),Reply); Check (Reply.Status = A.Denied and Effects = Before);
      end if;
   end if;
   if Mode = "empty" then Check (Pool_Maps = 0 and Validations = 1); elsif Mode in "unknown" | "bad-value" then Check (Pool_Maps = 1 and Validations = 1); elsif Mode /= "normal" and Mode /= "reentry" then Check (Pool_Maps = 0 and Validations = 0); end if;
   Ada.Text_IO.Put_Line ("LOCAL NVS MUX " & Mode & " checks" & Checks'Image);
end NVS_Mux_Authority_Tests;
