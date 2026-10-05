with Interfaces;
with CCL.Resource_Sections;
with CuBit.Network_Authority;
with CuBit.Render_Authority;
with CuBit.Launch_Authority;
with CuBit.Program_Descriptions;

--  The checked content of an executable manifest and of a service catalog,
--  whatever notation it was read from. CCL.Manifests.Encoding writes the
--  ELF sections from it, so every frontend produces the same bytes for the
--  same declaration (docs/ccl-typed-manifests.md).
package CCL.Manifests.Model with SPARK_Mode => On is
   use Interfaces;

   MAX_REQUESTS : constant := MAX_BINDINGS;
   --  Current procmgr ABI: slot 0 is bootstrap, slot 63 is the reply slot.
   subtype Slot_Number is Natural range 1 .. 62;
   type Rights_Kind is (Read_Only, Write_Only, Read_Write);
   for Rights_Kind use (Read_Only => 1, Write_Only => 2, Read_Write => 3);
   --  Device and scheduling requests travel in .cubit.resources, never in
   --  .cubit.caps: device resources are authorized by a path separate from
   --  endpoint delegation (docs/ccl-driver-manifests.md).
   type Request_Kind is
     (Framebuffer_Request, Service_Request, Notification_Request, Network_Request,
      Render_Request, Device_Memory_Request, IO_Port_Request, Interrupt_Request, DMA_Request,
      Scheduling_Request);
   package Sections renames CCL.Resource_Sections;
   for Request_Kind use
     (Framebuffer_Request => 1, Service_Request => 2, Notification_Request => 7,
      Network_Request => CuBit.Network_Authority.Manifest_Request,
      Render_Request => CuBit.Render_Authority.Manifest_Request,
      Device_Memory_Request => Sections.Resource_Kind'Enum_Rep (Sections.Device_Memory),
      IO_Port_Request => Sections.Resource_Kind'Enum_Rep (Sections.IO_Ports),
      Interrupt_Request => Sections.Resource_Kind'Enum_Rep (Sections.Interrupt),
      DMA_Request => Sections.Resource_Kind'Enum_Rep (Sections.DMA),
      Scheduling_Request => Sections.Resource_Kind'Enum_Rep (Sections.Scheduling));
   subtype Resource_Request is Request_Kind
     range Device_Memory_Request .. Scheduling_Request;
   subtype Device_Request is Request_Kind
     range Device_Memory_Request .. DMA_Request;

   --  Which devices a driver may be bound to. devmgr's discovery supplies the
   --  actual device; a manifest never names an address or a vector. The wire
   --  codes and limits are CCL.Resource_Sections', whose decoder startup uses.
   subtype Match_Kind is Sections.Match_Kind;
   use all type Sections.Match_Kind;
   subtype Platform_Device is Sections.Platform_Device;
   subtype Interrupt_Mode is Sections.Interrupt_Mode;
   PAGE_BYTES : constant := Sections.PAGE_BYTES;
   PCI_BAR_COUNT : constant := Sections.PCI_BAR_COUNT;
   MAX_PLATFORM_RESOURCES : constant := Sections.MAX_PLATFORM_RESOURCES;
   MAX_DEVICE_MEMORY_BYTES : constant := Sections.MAX_DEVICE_MEMORY_BYTES;
   MAX_DMA_BYTES : constant := Sections.MAX_DMA_BYTES;
   IO_PORT_SPACE : constant := Sections.IO_PORT_SPACE;
   MAX_INTERRUPT_VECTORS : constant := Sections.MAX_INTERRUPT_VECTORS;
   PCI_ID_LAST : constant := Sections.PCI_ID_LAST;
   PCI_CODE_LAST : constant := Sections.PCI_CODE_LAST;

   type Request is record
      Kind : Request_Kind := Service_Request;
      Service : Unsigned_32 := 0;
      Network : CuBit.Network_Authority.Scope;
      Rights : Rights_Kind := Read_Only;
      Slot : Slot_Number := Slot_Number'First;
      Name : Binding_Name;
      --  Resource requests: BAR or platform resource index (or interrupt
      --  mode), size/count/budget, and the scheduling period.
      Index : Unsigned_32 := 0;
      Amount : Unsigned_64 := 0;
      Extra : Unsigned_64 := 0;
   end record;
   type Request_Array is array (Positive range 1 .. MAX_REQUESTS) of Request;
   subtype Metadata_Text is Binding_Name;

   type Service_Binding is record
      Kind : Request_Kind := Service_Request;
      Name : Metadata_Text;
      ID : Unsigned_32 := 0;
      Rights : Rights_Kind := Read_Only;
   end record;
   type Service_Array is array (Positive range 1 .. MAX_REQUESTS) of Service_Binding;
   MAX_SCOPES : constant := 16;
   type Access_Right is (Read_File, Write_File, Execute_File, Create_File);
   for Access_Right use (Read_File => 1, Write_File => 2, Execute_File => 4, Create_File => 8);
   type Access_Rights is array (Access_Right) of Boolean;
   --  Tls_Domain entries carry a CuBit.TLS_Scopes pattern ("host:port")
   --  with the Read_File bit meaning "connect"; see Add_TLS_Scope.
   type Access_Domain is (Filesystem_Domain, Config_Domain, Tls_Domain);
   for Access_Domain use
     (Filesystem_Domain => 0, Config_Domain => 1, Tls_Domain => 2);
   type Scope is record
      Domain : Access_Domain := Filesystem_Domain;
      Path : Metadata_Text;
      Rights : Access_Rights := [others => False];
   end record;
   type Scope_Array is array (Positive range 1 .. MAX_SCOPES) of Scope;

   subtype Request_Count is Natural range 0 .. MAX_REQUESTS;
   subtype Scope_Count_Type is Natural range 0 .. MAX_SCOPES;
   MATCH_VALUES : constant := 3;
   type Match_Value_Array is array (Positive range 1 .. MATCH_VALUES) of Unsigned_16;

   --  The programs an executable may start by name (may_launch).
   MAX_LAUNCHES : constant := CuBit.Launch_Authority.Maximum_Names;
   subtype Launch_Count_Type is Natural range 0 .. MAX_LAUNCHES;
   type Launch_Array is array (Positive range 1 .. MAX_LAUNCHES) of Metadata_Text;

   --  One executable's requests, in declaration order: capability slots are
   --  assigned in this order across kinds.
   type Declaration is record
      Identity, Version : Metadata_Text;
      Requests : Request_Array := [others => (others => <>)];
      Count : Request_Count := 0;
      Scopes : Scope_Array := [others => (others => <>)];
      Scope_Count : Scope_Count_Type := 0;
      --  An explicit statement that the executable requests no capabilities:
      --  an empty .cubit.caps section rather than none.
      Explicit_No_Requests : Boolean := False;
      Match : Match_Kind := No_Match;
      Match_Values : Match_Value_Array := [others => 0];
      Launches : Launch_Array := [others => (others => <>)];
      Launch_Count : Launch_Count_Type := 0;
      --  The program's description: typed parameters and how they render
      --  into argv, its ports and its descriptor map; no section when it
      --  declares none of these.
      Description : CuBit.Program_Descriptions.Signature;
   end record;

   --  The build's service catalog: names, IDs and offered rights, the
   --  application slot range, and the fixed bindings.
   type Catalog_Model is record
      Services : Service_Array := [others => (others => <>)];
      Service_Count : Request_Count := 0;
      First_Slot, Last_Slot : Slot_Number := Slot_Number'First;
      Fixed : Binding_Array := [others => (others => <>)];
      Fixed_Count : Natural range 0 .. MAX_BINDINGS := 0;
   end record;

   --  Rules every frontend applies, so they accept exactly the same values.
   --  Identity and version: 1 .. 64 of [A-Za-z0-9._-].
   function Valid_Metadata_Text (Item : String) return Boolean is
     (Item'Length in 1 .. Binding_Name_Length'Last and then
      (for all C of Item => C in 'a' .. 'z' | 'A' .. 'Z' | '0' .. '9' | '.' | '-' | '_'));
   --  Lowercase kebab names map injectively to Ada Slot_<name> identifiers:
   --  a leading letter, no trailing or doubled hyphens.
   function Valid_Binding_Name (Item : String) return Boolean;
   --  An authority scope's exact bytes: 1 .. 64 printable ASCII, no
   --  wildcards or backslash, no "." or ".." components, no "//".
   function Valid_Scope_Path (Path : String) return Boolean;
end CCL.Manifests.Model;
