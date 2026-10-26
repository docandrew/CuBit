------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  @summary
--  The install payload, read through filesystem.svc: a directory holding
--  esp/ (the boot files), root/ (the image closure) and INDEX, the list of
--  their files (one "esp/<path>" or "root/<path>" per line; directories
--  follow from the paths, sizes from the files; a line may name, after a
--  tab, where the file is below the medium's root). Walk builds the two
--  Install_Trees from it; Read gives a file's bytes to the layout writer.
--  The installer's manifest scopes say what it may read (the live
--  medium).
------------------------------------------------------------------------------
pragma Ada_2022;
with Interfaces; use Interfaces;
with Disk_Layout;
with Install_Tree;

package Installer_Payload is

   type Walk_Result is (Walked, Unreadable, Too_Large);

   --  Opens the loan through which every request moves its bytes.
   procedure Initialize (Ready : out Boolean);

   --  The payload's trees, as the last Walk listed them (one copy only:
   --  the writer reads them in place).
   type Payload_Trees is array (Disk_Layout.Install_Volume) of Install_Tree.Tree;
   Trees : Payload_Trees;

   --  Reads Source/INDEX into Trees (ESP_Volume, Root_Volume).
   procedure Walk (Source : String; Result : out Walk_Result);

   --  Data'Length bytes of File from Offset (the writer's Read).
   procedure Read
     (Volume : Disk_Layout.Install_Volume; File : Install_Tree.Entry_Index;
      Offset : Unsigned_64; Data : out Disk_Layout.Bytes; OK : out Boolean);

   --  Closes the file Read keeps open between calls.
   procedure Finish;

   --  Progress since the last Walk: files Read has opened, bytes it gave.
   function Files_Read return Unsigned_64;
   function Bytes_Read return Unsigned_64;

end Installer_Payload;
