with Interfaces;
with CCL.Resource_Sections;
with CCL.Device_Sections;
with CuBit.Network_Authority;
with CuBit.Render_Authority;
with CuBit.Launch_Authority;
with CuBit.Program_Descriptions;
with CuBit.Startup_Tables;
with CuBit.Channel_Limits;

--  The checked content of an executable manifest and of a service catalog,
--  whatever notation it was read from. CCL.Manifests.Encoding writes the
--  ELF sections from it, so every frontend produces the same bytes for the
--  same declaration (docs/ccl-typed-manifests.md).
package CCL.Manifests.Model with SPARK_Mode => On is
   use Interfaces;

   MAX_REQUESTS : constant := MAX_BINDINGS;
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

   --  Which devices a driver may be bound to (its `drives`, written to
   --  .cubit.devices; docs/device-matching.md), and what its resource
   --  indexes refer to, derived from them. devmgr's discovery supplies the
   --  actual device; a manifest never names an address or a vector. The wire
   --  codes and limits are CCL.Resource_Sections' and CCL.Device_Sections'.
   package Devices renames CCL.Device_Sections;
   subtype Basis_Kind is Sections.Basis_Kind;
   use all type Sections.Basis_Kind;
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
      Name : Binding_Name;
      --  Resource requests: BAR or platform resource index (or interrupt
      --  mode), size/count/budget, and the scheduling period.
      Index : Unsigned_32 := 0;
      Amount : Unsigned_64 := 0;
      Extra : Unsigned_64 := 0;
      --  Whether the launch fails without it (the manifest's `optional`).
      Requirement : CuBit.Startup_Tables.Need := CuBit.Startup_Tables.Required;
      --  The catalog service's typed client (its startup function's type).
      Client : Service_Client := Plain;
   end record;
   type Request_Array is array (Positive range 1 .. MAX_REQUESTS) of Request;
   subtype Metadata_Text is Binding_Name;

   type Service_Binding is record
      Kind : Request_Kind := Service_Request;
      Name : Metadata_Text;
      ID : Unsigned_32 := 0;
      Rights : Rights_Kind := Read_Only;
      Client : Service_Client := Plain;
   end record;
   --  A service catalog names each kernel registry role (0 .. MAXIMUM_ROLE,
   --  System_Publications.Maximum_Role) at most once as a service and once
   --  as a notification: its bound is the registry's, not one manifest's.
   MAXIMUM_ROLE : constant := 127;
   MAX_CATALOG_ENTRIES : constant := 2 * (MAXIMUM_ROLE + 1);
   subtype Catalog_Count is Natural range 0 .. MAX_CATALOG_ENTRIES;
   type Service_Array is array (Positive range 1 .. MAX_CATALOG_ENTRIES) of Service_Binding;
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

   --  The programs an executable may start by name (may_launch).
   MAX_LAUNCHES : constant := CuBit.Launch_Authority.Maximum_Names;
   subtype Launch_Count_Type is Natural range 0 .. MAX_LAUNCHES;
   type Launch_Array is array (Positive range 1 .. MAX_LAUNCHES) of Metadata_Text;

   --  A server's limits on one contract's channels (Channel_Servers): opens
   --  past an At_Most Per_Client from one client, or past an At_Most Total,
   --  are refused; Server_Limit is the server's own live capacity, spelled
   --  out (CuBit.Channel_Limits).
   MAX_CHANNEL_OFFERS : constant := 8;
   MAX_CHANNEL_TOTAL : constant := CuBit.Channel_Limits.Maximum_Total;
   subtype Channel_Limit is CuBit.Channel_Limits.Channel_Limit;
   subtype Channel_Class is CuBit.Channel_Limits.Channel_Class;
   type Channel_Offer is record
      Contract : Metadata_Text;
      Per_Client : Channel_Limit := (Kind => CuBit.Channel_Limits.At_Most, Count => 1);
      Total : Channel_Limit := (Kind => CuBit.Channel_Limits.At_Most, Count => 1);
      Class : Channel_Class := CuBit.Channel_Limits.Normal;
   end record;
   subtype Channel_Offer_Count is Natural range 0 .. MAX_CHANNEL_OFFERS;
   type Channel_Offer_Array is array (Positive range 1 .. MAX_CHANNEL_OFFERS) of Channel_Offer;

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
      Drives : Devices.Match_Table;
      Basis : Sections.Basis_Info;
      Launches : Launch_Array := [others => (others => <>)];
      Launch_Count : Launch_Count_Type := 0;
      --  The program's description: typed parameters and how they render
      --  into argv, its ports and its descriptor map; no section when it
      --  declares none of these.
      Description : CuBit.Program_Descriptions.Signature;
      --  The process's handle limit, and the channels it serves.
      Handles : CuBit.Startup_Tables.Handle_Limit;
      Channels : Channel_Offer_Array := [others => (others => <>)];
      Channel_Count : Channel_Offer_Count := 0;
   end record;

   --  The build's service catalog: names, service roles and offered rights.
   type Catalog_Model is record
      Services : Service_Array := [others => (others => <>)];
      Service_Count : Catalog_Count := 0;
   end record;

   --  Rules every frontend applies, so they accept exactly the same values.
   --  Identity and version: 1 .. 64 of [A-Za-z0-9._-].
   function Valid_Metadata_Text (Item : String) return Boolean is
     (Item'Length in 1 .. Binding_Name_Length'Last and then
      (for all C of Item => C in 'a' .. 'z' | 'A' .. 'Z' | '0' .. '9' | '.' | '-' | '_'));
   --  Lowercase kebab names map injectively to the generated Startup
   --  package's function names: a leading letter, no trailing or doubled
   --  hyphens.
   function Valid_Binding_Name (Item : String) return Boolean;
   --  Add a `drives` entry: Accepted False (with Diagnostic) for an invalid
   --  entry, a duplicate, a 17th entry, or one whose resource basis
   --  conflicts with an earlier entry's (PCI and Platform, or two Platform
   --  devices). PCI entries give the PCI basis, a Platform entry the
   --  platform basis; ACPI and USB entries none.
   procedure Add_Drive
     (Decl : in out Declaration; Item : Devices.Device_Match;
      Accepted : out Boolean; Diagnostic : out Diagnostic_Code);
   --  An authority scope's exact bytes: 1 .. 64 printable ASCII, no
   --  wildcards or backslash, no "." or ".." components, no "//".
   function Valid_Scope_Path (Path : String) return Boolean;
end CCL.Manifests.Model;
