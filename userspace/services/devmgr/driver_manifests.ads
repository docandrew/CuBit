------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  @summary
--  What a program's manifest asks for, read from its ELF image (CAP-002,
--  docs/capability-handles.md; docs/msi-interrupts.md)
--
--  @description
--  A program's typed CCL manifest (manifest.ccl) is compiled into its ELF:
--  * .cubit.startup: its declared startup table (CuBit.Startup_Tables),
--    every entry Unassigned, in request order;
--  * .cubit.caps: the capability requests (services, notifications,
--    framebuffer, network, render) in request order;
--  * .cubit.resources: the device and scheduling requests in request order,
--    decoded with the proved CCL.Resource_Sections.Decode;
--  * .cubit.access: the authority scopes; devmgr installs the Config ones
--    for the services it starts (CAP-003: no undeclared wildcard).
--  Entry I of the startup table is request I: the K-th entry of a resource
--  kind is the K-th .cubit.resources entry, the K-th other entry the K-th
--  .cubit.caps entry (Source_Of). devmgr builds the child's startup table
--  from these (Devmgr_Launches); nothing here grants anything.
--
--  The image is untrusted: every reader is proved free of run-time errors
--  and only accepts sections that lie inside it.
------------------------------------------------------------------------------
pragma Ada_2022;
with Interfaces; use Interfaces;
with CCL.Resource_Sections;
with CuBit.Startup_Tables;

package Driver_Manifests with SPARK_Mode => On is
   use CCL.Resource_Sections;
   package ST renames CuBit.Startup_Tables;

   --  .cubit.caps: "TIBC" little-endian, version, count; 16-byte entries
   --  (request kind, rights, reserved u16, service u32, descriptor u64).
   CAPS_MAGIC         : constant := 16#4342_4954#;
   CAPS_VERSION       : constant := 1;
   CAPS_HEADER_BYTES  : constant := 8;
   CAPS_ENTRY_BYTES   : constant := 16;
   MAX_CAPABILITIES   : constant := ST.Maximum_Entries;
   MAX_CAPS_BYTES     : constant := CAPS_HEADER_BYTES + MAX_CAPABILITIES * CAPS_ENTRY_BYTES;

   type Capability_Request is record
      --  The request kind's wire code (CCL.Manifests.Model.Request_Kind).
      Kind       : Unsigned_8 := 0;
      Rights     : Rights_Kind := Read_Only;
      --  The service or notification role.
      Service    : Unsigned_32 := 0;
      Descriptor : Unsigned_64 := 0;
   end record;
   subtype Capability_Count is Natural range 0 .. MAX_CAPABILITIES;
   type Capability_Array is array (Positive range 1 .. MAX_CAPABILITIES) of Capability_Request;

   --  .cubit.access: "CACC" little-endian, version, count, 8 reserved bytes;
   --  80-byte entries (rights mask, prefix length, domain, 5 reserved, the
   --  64-byte prefix, 8 reserved), as CCL.Manifests.Encoding writes them.
   ACCESS_MAGIC        : constant := 16#4343_4143#;
   ACCESS_VERSION      : constant := 1;
   ACCESS_HEADER_BYTES : constant := 16;
   ACCESS_ENTRY_BYTES  : constant := 80;
   --  CCL.Manifests.Model.MAX_SCOPES.
   MAX_SCOPES          : constant := 16;
   MAX_ACCESS_BYTES    : constant := ACCESS_HEADER_BYTES + MAX_SCOPES * ACCESS_ENTRY_BYTES;
   --  CCL.Manifests.Model.Access_Domain'Enum_Rep (Config_Domain).
   CONFIG_DOMAIN       : constant := 1;
   MAX_PREFIX_BYTES        : constant := 64;

   --  One Config scope: config.svc's ACL entry (rights mask, key prefix).
   subtype Prefix_Length is Natural range 0 .. MAX_PREFIX_BYTES;
   type Prefix_Bytes is array (1 .. MAX_PREFIX_BYTES) of Unsigned_8;
   type Config_Scope is record
      Rights : Unsigned_8 := 0;
      Length : Prefix_Length := 0;
      Prefix : Prefix_Bytes := [others => 0];
   end record;
   subtype Scope_Count is Natural range 0 .. MAX_SCOPES;
   type Config_Scope_Array is array (Positive range 1 .. MAX_SCOPES) of Config_Scope;

   type Manifest is record
      Startup_Found    : Boolean := False;
      Declared         : ST.Table := ST.Empty_Table;
      Capabilities     : Capability_Array := [others => (others => <>)];
      Capability_Total : Capability_Count := 0;
      Resources_Found  : Boolean := False;
      Resources        : Section_Plan;
      --  Its Config scopes (none when .cubit.access is absent or malformed).
      Config_Scopes    : Config_Scope_Array := [others => (others => <>)];
      Config_Total     : Scope_Count := 0;
   end record;

   --  A resource-kind startup entry (its request is in .cubit.resources).
   function Resource_Entry (Kind : ST.Capability_Kind) return Boolean is
     (Kind in ST.Device_Memory | ST.IO_Ports | ST.Interrupt | ST.DMA | ST.Scheduling);

   --  The image's manifest. Missing or malformed sections read as absent
   --  (Startup_Found False: the program declares no startup table).
   function Read (Image : Byte_Array) return Manifest
   with Pre  => Image'First = 1 and then Image'Last < Natural'Last,
        Post => (if Read'Result.Startup_Found
                 then ST.Valid_Table (Read'Result.Declared)
                      and then ST.Declaration (Read'Result.Declared));

   --  Where startup entry Index came from: its .cubit.caps or
   --  .cubit.resources entry, or None when the sections disagree.
   type Source_Kind is (None, Capability_Source, Resource_Source);
   type Source is record
      Kind  : Source_Kind := None;
      Index : Natural := 0;
   end record;

   function Source_Of (M : Manifest; Index : ST.Entry_Index) return Source
   with Post => (case Source_Of'Result.Kind is
                   when None => True,
                   when Capability_Source =>
                     Source_Of'Result.Index in 1 .. M.Capability_Total,
                   when Resource_Source =>
                     M.Resources_Found
                     and then Source_Of'Result.Index in 1 .. M.Resources.Count);

   --  The first message-signaled interrupt request (MSI or MSI-X), or
   --  Present False.
   type Interrupt_Request is record
      Present : Boolean := False;
      Mode    : Interrupt_Mode := MSI_X;
      Vectors : Positive range 1 .. MAX_INTERRUPT_VECTORS := 1;
   end record;
   function Interrupt_Of (M : Manifest) return Interrupt_Request;

end Driver_Manifests;
