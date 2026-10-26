------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  @summary
--  The install payload, read through filesystem.svc: a directory holding
--  esp/ (the boot files) and root/ (the image closure). Walk lists a tree
--  into an Install_Tree; Read gives a file's bytes to the layout writer.
--  The installer's manifest scopes say which payload directories it may
--  read (the live medium's install/ directory).
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

   --  Lists Source/esp into Trees (ESP_Volume) and Source/root into Trees
   --  (Root_Volume).
   procedure Walk (Source : String; Result : out Walk_Result);

   --  Data'Length bytes of File from Offset (the writer's Read).
   procedure Read
     (Volume : Disk_Layout.Install_Volume; File : Install_Tree.Entry_Index;
      Offset : Unsigned_64; Data : out Disk_Layout.Bytes; OK : out Boolean);

   --  Closes the file Read keeps open between calls.
   procedure Finish;

end Installer_Payload;
