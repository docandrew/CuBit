------------------------------------------------------------------------------
--  devmgr's manifest reader over real ccl-manifest output (ELF objects),
--  and over every truncation and single-byte corruption of it.
------------------------------------------------------------------------------
pragma Ada_2022;
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Streams.Stream_IO;
with Interfaces; use Interfaces;
with CCL.Resource_Sections; use CCL.Resource_Sections;
with CuBit.Startup_Tables;
with Driver_Manifests; use Driver_Manifests;

procedure Main is
   package ST renames CuBit.Startup_Tables;
   use type ST.Capability_Kind;
   use type ST.Need;
   Checks : Natural := 0;

   procedure Check (Condition : Boolean; What : String) is
   begin
      Checks := Checks + 1;
      if not Condition then
         raise Program_Error with "driver manifests: " & What;
      end if;
   end Check;

   function Load (Name : String) return Byte_Array is

      F : Ada.Streams.Stream_IO.File_Type;
   begin
      Ada.Streams.Stream_IO.Open (F, Ada.Streams.Stream_IO.In_File, "objects/" & Name & ".o");
      declare
         Size : constant Natural := Natural (Ada.Streams.Stream_IO.Size (F));
         Data : Byte_Array (1 .. Size);
         Raw : Ada.Streams.Stream_Element_Array (1 .. Ada.Streams.Stream_Element_Offset (Size));
         Last : Ada.Streams.Stream_Element_Offset;
      begin
         Ada.Streams.Stream_IO.Read (F, Raw, Last);
         Ada.Streams.Stream_IO.Close (F);
         for I in Data'Range loop
            Data (I) := Unsigned_8 (Raw (Ada.Streams.Stream_Element_Offset (I)));
         end loop;
         return Data;
      end;
   end Load;

   function Label (M : Manifest; I : ST.Entry_Index) return String is
     (String (M.Declared.Entries (I).Label.Text (1 .. M.Declared.Entries (I).Label.Length)));

   --  Whatever the image, the reader's answer is self-consistent.
   procedure Consistent (M : Manifest) is
      S : Source;
   begin
      if not M.Startup_Found then
         return;
      end if;
      Check (ST.Valid_Table (M.Declared) and then ST.Declaration (M.Declared), "valid table");
      for I in 1 .. M.Declared.Count loop
         S := Source_Of (M, I);
         case S.Kind is
            when None => null;
            when Capability_Source =>
               Check (not Resource_Entry (M.Declared.Entries (I).Kind), "capability source kind");
            when Resource_Source =>
               Check (Resource_Entry (M.Declared.Entries (I).Kind), "resource source kind");
         end case;
      end loop;
   end Consistent;

   type Values is array (Positive range <>) of Unsigned_8;
   Hostile : constant Values := [0, 1, 16#7F#, 16#FF#];

   procedure Robust (Name : String) is
      Image : constant Byte_Array := Load (Name);
      M : Manifest;
   begin
      --  Every truncation.
      for Length in 0 .. Image'Length loop
         Consistent (Read (Image (1 .. Length)));
      end loop;
      --  Every byte set to a few hostile values.
      for I in Image'Range loop
         for Value of Hostile loop
            declare
               Bad : Byte_Array := Image;
            begin
               Bad (I) := Value;
               M := Read (Bad);
               Consistent (M);
            end;
         end loop;
      end loop;
   end Robust;

   Hda : constant Manifest := Read (Load ("hda"));
   Mixer : constant Manifest := Read (Load ("mixer"));
   Ata : constant Manifest := Read (Load ("ata"));
   Netstack : constant Manifest := Read (Load ("netstack"));
   Netmgr : constant Manifest := Read (Load ("netmgr"));
   Net_Prefix : constant String (1 .. 4) := "net.";
   Empty : constant Byte_Array (1 .. 0) := [others => 0];
begin
   --  No image, no manifest.
   Check (not Read (Empty).Startup_Found, "empty image");

   --  hda.drv: registers, controller, streams, registration, mixer, ready.
   Check (Hda.Startup_Found and Hda.Declared.Count = 6, "hda table");
   Check (Label (Hda, 1) = "registers" and Hda.Declared.Entries (1).Kind = ST.Device_Memory, "hda 1");
   Check (Label (Hda, 2) = "controller" and Hda.Declared.Entries (2).Kind = ST.Interrupt, "hda 2");
   Check (Label (Hda, 3) = "streams" and Hda.Declared.Entries (3).Kind = ST.DMA
          and Hda.Declared.Entries (3).Requirement = ST.Optional, "hda 3 (DMA is optional)");
   Check (Label (Hda, 4) = "registration" and Hda.Declared.Entries (4).Kind = ST.Notification, "hda 4");
   Check (Label (Hda, 5) = "mixer" and Hda.Declared.Entries (5).Kind = ST.Endpoint, "hda 5");
   Check (Label (Hda, 6) = "ready" and Hda.Declared.Entries (6).Kind = ST.Endpoint, "hda 6");
   Check (Hda.Capability_Total = 3 and Hda.Resources_Found and Hda.Resources.Count = 3, "hda sections");
   Check (Source_Of (Hda, 1) = (Resource_Source, 1) and Source_Of (Hda, 2) = (Resource_Source, 2)
          and Source_Of (Hda, 3) = (Resource_Source, 3), "hda resource sources");
   Check (Source_Of (Hda, 4) = (Capability_Source, 1) and Source_Of (Hda, 5) = (Capability_Source, 2)
          and Source_Of (Hda, 6) = (Capability_Source, 3), "hda capability sources");
   Check (Hda.Capabilities (1).Service = 8 and Hda.Capabilities (1).Rights = Write_Only,
          "hda registration: role 8, manage");
   Check (Hda.Capabilities (2).Service = 9 and Hda.Capabilities (3).Service = 7
          and Hda.Capabilities (3).Rights = Read_Write, "hda services: mixer, device manager");
   Check (Interrupt_Of (Hda) = (Present => True, Mode => MSI, Vectors => 1), "hda interrupt");
   Check (Hda.Resources.Entries (1).Kind = Device_Memory and Hda.Resources.Entries (1).Amount = 16384,
          "hda registers request");
   Check (Source_Of (Hda, 7) = (None, 0), "beyond the table");

   --  mixer.svc: a scheduling resource first, then capabilities.
   Check (Mixer.Startup_Found and Mixer.Declared.Count = 4, "mixer table");
   Check (Source_Of (Mixer, 1) = (Resource_Source, 1)
          and Mixer.Resources.Entries (1).Kind = Scheduling
          and Mixer.Resources.Entries (1).Amount = 1500 and Mixer.Resources.Entries (1).Extra = 5000,
          "mixer scheduling");
   Check (Source_Of (Mixer, 3) = (Capability_Source, 2)
          and Mixer.Declared.Entries (3).Requirement = ST.Optional, "mixer hda optional");
   Check (not Interrupt_Of (Mixer).Present, "mixer has no message interrupt");

   --  ata.drv: platform resources (ports and a line), no message interrupt.
   Check (Ata.Startup_Found and Ata.Resources_Found and Ata.Resources.Count = 3, "ata");
   Check (Label (Ata, 1) = "command-block" and Ata.Resources.Entries (1).Kind = IO_Ports
          and Ata.Resources.Entries (1).Amount = 8, "ata command block");
   Check (not Interrupt_Of (Ata).Present, "ata line is not message-signaled");

   --  netstack.svc: no resources at all.
   Check (Netstack.Startup_Found and not Netstack.Resources_Found
          and Netstack.Capability_Total = Netstack.Declared.Count, "netstack");
   for I in 1 .. Netstack.Declared.Count loop
      Check (Source_Of (Netstack, I) = (Capability_Source, I), "netstack sources");
   end loop;

   --  Config scopes (CAP-003): netmgr.svc declares exactly "net.", read
   --  only, which devmgr installs for it; the drivers declare none.
   Check (Netmgr.Config_Total = 1, "netmgr declares one config scope");
   Check (Netmgr.Config_Scopes (1).Length = 4
          and then (for all J in 1 .. 4 =>
                      Character'Val (Netmgr.Config_Scopes (1).Prefix (J)) = Net_Prefix (J)),
          "netmgr's config scope is net.");
   Check (Netmgr.Config_Scopes (1).Rights = 1, "netmgr's config scope is read only");
   Check (Hda.Config_Total = 0 and Netstack.Config_Total = 0, "drivers declare no config scope");

   Robust ("hda");
   Robust ("netmgr");
   Robust ("mixer");
   Robust ("ata");
   Robust ("netstack");
   Put_Line ("DRIVER-MANIFESTS: PASS" & Natural'Image (Checks) & " checks");
end Main;
