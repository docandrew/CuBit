------------------------------------------------------------------------------
--  Linux-hosted test of the installer's layout writer: writes the layout
--  into a sparse image file, then reads the GPT back through the UFS
--  driver's decoder (Partition_Tables). tests/disk-layout/run.sh then
--  checks the same image with sgdisk, fsck.fat and e2fsck.
--
--  usage: main IMAGE BLOCK_BYTES DISK_MIB ESP_DIR ROOT_DIR
--  The two directories are the payloads: their trees are written into the
--  ESP and the root.
------------------------------------------------------------------------------
pragma Ada_2022;
with Ada.Command_Line; use Ada.Command_Line;
with Ada.Streams; use Ada.Streams;
with Ada.Streams.Stream_IO; use Ada.Streams.Stream_IO;
with Ada.Text_IO;
with Ada.Directories;
with GNAT.OS_Lib;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Install_Tree;
with Interfaces; use Interfaces;
with Disk_Layout; use Disk_Layout;
with GPT_Image;
with Ext2_Image;
with Layout_Writer;
with Install_Steps;
with Partition_Tables;

procedure Main is
   use type Partition_Tables.MBR_Kind, Partition_Tables.Header_Result, Partition_Tables.GUID;
   use type Partition_Tables.Bytes;
   use type Install_Steps.Step;
   use type Install_Tree.Entry_Kind;
   Failures : Natural := 0;

   procedure Check (Condition : Boolean; Name : String) is
   begin
      if not Condition then
         Failures := Failures + 1;
      end if;
      Ada.Text_IO.Put_Line ("disk-layout: " & Name & (if Condition then " PASS" else " FAIL"));
   end Check;

   --  The image through one read-write descriptor (writes and read-backs
   --  interleave).
   Disk : GNAT.OS_Lib.File_Descriptor := GNAT.OS_Lib.Invalid_FD;

   procedure Seek (Offset : Unsigned_64) is
   begin
      GNAT.OS_Lib.Lseek (Disk, Long_Integer (Offset), GNAT.OS_Lib.Seek_Set);
   end Seek;

   procedure Put (Offset : Byte_Offset; Data : Bytes; OK : out Boolean) is
      Copy : constant Bytes (1 .. Data'Length) := Data;
   begin
      Seek (Offset);
      OK := GNAT.OS_Lib.Write (Disk, Copy'Address, Copy'Length) = Copy'Length;
   end Put;

   Zero_Chunk : constant Bytes (1 .. 1024 * 1024) := [others => 0];

   procedure Zero (Offset : Byte_Offset; Length : Unsigned_64; OK : out Boolean) is
      Done : Unsigned_64 := 0;
   begin
      Seek (Offset);
      OK := True;
      while OK and then Done < Length loop
         declare
            Step : constant Natural := Natural (Unsigned_64'Min (Length - Done, Zero_Chunk'Length));
         begin
            OK := GNAT.OS_Lib.Write (Disk, Zero_Chunk'Address, Step) = Step;
            Done := Done + Unsigned_64 (Step);
         end;
      end loop;
   end Zero;

   function Read_At (Offset : Unsigned_64; Count : Positive) return Bytes is
      Result : Bytes (1 .. Count) := [others => 0];
      Got : Integer;
   begin
      Seek (Offset);
      Got := GNAT.OS_Lib.Read (Disk, Result'Address, Count);
      if Got /= Count then
         raise Program_Error with "short read of the image";
      end if;
      return Result;
   end Read_At;

   --  Each payload entry's host path.
   type Path_Array is array (Install_Tree.Entry_Index) of Unbounded_String;
   ESP_Paths, Root_Paths : Path_Array;
   ESP_Files, Root_Files : Install_Tree.Tree;

   procedure Read
     (Volume : Install_Volume; File : Install_Tree.Entry_Index; Offset : Unsigned_64;
      Data : out Bytes; OK : out Boolean)
   is
      Source : File_Type;
      Chunk : Stream_Element_Array (1 .. Stream_Element_Offset (Data'Length));
      Last : Stream_Element_Offset;
   begin
      Open (Source, In_File, To_String
              (if Volume = ESP_Volume then ESP_Paths (File) else Root_Paths (File)));
      Set_Index (Source, Positive_Count (Offset + 1));
      Read (Source, Chunk, Last);
      Close (Source);
      OK := Last = Chunk'Last;
      for I in Data'Range loop
         Data (I) := Unsigned_8 (Chunk (Stream_Element_Offset (I - Data'First + 1)));
      end loop;
   end Read;

   --  Adds Directory's contents (sorted by name) under Parent, depth first.
   procedure Walk
     (Files : in out Install_Tree.Tree; Paths : in out Path_Array;
      Directory : String; Parent : Install_Tree.Parent_Index)
   is
      use Ada.Directories;
      type Name_List is array (1 .. 1024) of Unbounded_String;
      Names : Name_List;
      Count : Natural := 0;
      Search : Search_Type;
      Item : Directory_Entry_Type;
      Added : Install_Tree.Add_Result;
      use type Install_Tree.Add_Result;
   begin
      Start_Search (Search, Directory, "*");
      while More_Entries (Search) loop
         Get_Next_Entry (Search, Item);
         if Simple_Name (Item) /= "." and then Simple_Name (Item) /= ".." then
            Count := Count + 1;
            Names (Count) := To_Unbounded_String (Simple_Name (Item));
         end if;
      end loop;
      End_Search (Search);
      for I in 2 .. Count loop
         for J in reverse 2 .. I loop
            exit when Names (J - 1) <= Names (J);
            declare
               T : constant Unbounded_String := Names (J);
            begin
               Names (J) := Names (J - 1);
               Names (J - 1) := T;
            end;
         end loop;
      end loop;
      for I in 1 .. Count loop
         declare
            Name : constant String := To_String (Names (I));
            Full : constant String := Compose (Directory, Name);
            Is_Directory : constant Boolean := Kind (Full) = Ada.Directories.Directory;
         begin
            Install_Tree.Add
              (Files, (if Is_Directory then Install_Tree.Directory else Install_Tree.Regular_File),
               Parent, Name, (if Is_Directory then 0 else Unsigned_64 (Size (Full))), Added,
               Executable => not Is_Directory and then GNAT.OS_Lib.Is_Executable_File (Full));
            if Added /= Install_Tree.Added then
               raise Program_Error with "payload entry refused: " & Full;
            end if;
            Paths (Files.Count) := To_Unbounded_String (Full);
            if Is_Directory then
               Walk (Files, Paths, Full, Files.Count);
            end if;
         end;
      end loop;
   end Walk;

   procedure Get (Offset : Byte_Offset; Data : out Bytes; OK : out Boolean) is
   begin
      Data := Read_At (Offset, Data'Length);
      OK := True;
   end Get;

   --  Read-back bytes against the source file's (the device reads back
   --  what the source gave: compared directly here; the installer compares
   --  SHA-256 digests instead).
   Mismatches : Natural := 0;
   procedure Check
     (Volume : Install_Volume; File : Install_Tree.Entry_Index; Offset : Unsigned_64;
      Data : Bytes; Final : Boolean; Matches : out Boolean)
   is
      pragma Unreferenced (Final);
      Source : Bytes (1 .. Data'Length);
      OK : Boolean := True;
   begin
      if Data'Length > 0 then
         Read (Volume, File, Offset, Source, OK);
      end if;
      Matches := OK and then Source = Data;
      if not Matches then
         Mismatches := Mismatches + 1;
      end if;
   end Check;

   procedure Flush (OK : out Boolean) is
   begin
      OK := True;
   end Flush;

   --  Interrupting an install (power lost): once the writer has entered
   --  Stop_At, one more write lands and every later one fails (Done: never).
   Stop_At : Install_Steps.Step := Install_Steps.Done;
   Armed, Stopped : Boolean := False;
   procedure Enter (Step : Install_Steps.Step) is
   begin
      if Step = Stop_At and then Stop_At /= Install_Steps.Done then
         Armed := True;
      end if;
   end Enter;

   procedure Guarded_Put (Offset : Byte_Offset; Data : Bytes; OK : out Boolean) is
   begin
      if Stopped then
         OK := False;
      else
         Put (Offset, Data, OK);
         Stopped := Armed;
      end if;
   end Guarded_Put;

   procedure Guarded_Zero (Offset : Byte_Offset; Length : Unsigned_64; OK : out Boolean) is
   begin
      if Stopped then
         OK := False;
      else
         Zero (Offset, Length, OK);
         Stopped := Armed;
      end if;
   end Guarded_Zero;

   package Writer is new Layout_Writer (Guarded_Put, Guarded_Zero, Get, Read, Check, Flush, Enter);

   Path : constant String := Argument (1);
   Block : constant Device_Block_Bytes := Device_Block_Bytes'Value (Argument (2));
   Disk_MiB : constant Unsigned_64 := Unsigned_64'Value (Argument (3));
   Shape : constant Disk_Shape :=
     (Block_Size => Block, Blocks => Disk_MiB * Mebibyte / Unsigned_64 (Block));
   function Id (Seed : Unsigned_8) return GUID is ([for I in GUID'Range => Seed + Unsigned_8 (I)]);
   Layout : Writer.Install_Layout :=
     (Disk => (ESP_Size => Minimum_ESP_MiB, Disk_Id => Id (16#10#),
               ESP_Id => Id (16#20#), Root_Id => Id (16#30#)),
      ESP_Serial => 16#CB17_0001#, ESP_Label => "CUBIT ESP  ",
      Root => (Inode_Ratio => 65_536, Id => Id (16#40#),
               Name => "cubit-root" & [1 .. 6 => ASCII.NUL], Created => 1_793_000_000),
      Generation_First => Install_Tree.Entry_Index'Last);
   Result : Writer.Write_Result;
   Reached : Install_Steps.Step;
   use type Writer.Write_Result;
begin
   --  CRC-32/ISO-HDLC check value.
   Check (Partition_Tables.CRC32 ([for C of String'("123456789") => Character'Pos (C)]) = 16#CBF4_3926#,
          "crc32 check value");
   Check (Fits (Shape, Layout.Disk), "layout fits");
   --  The IdeaPad's UFS disk (31,246,336 blocks of 4 KiB) with a 1 GiB ESP:
   --  planned only, not written.
   declare
      Laptop : constant Disk_Shape := (Block_Size => 4096, Blocks => 31_246_336);
      Request : constant Layout_Request := (Layout.Disk with delta ESP_Size => 1024);
      Root : constant Ext2_Image.Volume :=
        Ext2_Image.Plan ((Last_Usable (Laptop) - Root_First (Laptop, Request) + 1) * 4096,
                         Layout.Root.Inode_Ratio);
   begin
      Check (Fits (Laptop, Request) and then Ext2_Image.Valid (Root),
             "laptop layout plans (root groups" & Root.Groups'Image &
             ", inode table blocks per group" & Root.Inode_Table_Blocks'Image & ")");
   end;
   Disk := GNAT.OS_Lib.Create_File (Path, GNAT.OS_Lib.Binary);
   GNAT.OS_Lib.Close (Disk);
   Disk := GNAT.OS_Lib.Open_Read_Write (Path, GNAT.OS_Lib.Binary);
   --  The disk's size: its last byte.
   declare
      Last_Byte : constant Bytes (1 .. 1) := [1 => 0];
      OK : Boolean;
   begin
      Put (Shape.Blocks * Unsigned_64 (Block) - 1, Last_Byte, OK);
   end;
   Walk (ESP_Files, ESP_Paths, Argument (4), Install_Tree.Volume_Root);
   Walk (Root_Files, Root_Paths, Argument (5), Install_Tree.Volume_Root);
   --  The tree's last file stands for the generation records (Writing_Generation).
   for E in reverse 1 .. Root_Files.Count loop
      if Root_Files.Entries (E).Kind = Install_Tree.Regular_File then
         Layout.Generation_First := E;
         exit;
      end if;
   end loop;
   --  SYS-002: an install interrupted at any step before Making_Bootable
   --  leaves a disk that cannot boot, even over an earlier complete install
   --  (the first run below): its ESP has no FAT boot sector (signature
   --  55 AA at bytes 510-511) and, before Writing_ESP, its root no ext2
   --  superblock (magic EF53 at byte 1080).
   Writer.Write_Disk (Shape, Layout, ESP_Files, Root_Files, Result, Reached);
   Check (Result = Writer.Written and then Reached = Install_Steps.Done, "complete install written");
   for Interrupted in Install_Steps.Partitioning .. Install_Steps.Making_Bootable loop
      Stop_At := Interrupted;
      Armed := False;
      Stopped := False;
      Writer.Write_Disk (Shape, Layout, ESP_Files, Root_Files, Result, Reached);
      declare
         B : constant Unsigned_64 := Unsigned_64 (Block);
         ESP_Head : constant Bytes := Read_At (ESP_First (Shape) * B, 512);
         Root_Head : constant Bytes := Read_At (Root_First (Shape, Layout.Disk) * B, 2048);
         ESP_Boots : constant Boolean := ESP_Head (511) = 16#55# and then ESP_Head (512) = 16#AA#;
         Root_Mounts : constant Boolean := Root_Head (1081) = 16#53# and then Root_Head (1082) = 16#EF#;
      begin
         Check (Result /= Writer.Written and then not ESP_Boots
                and then (if Interrupted in Install_Steps.Formatting_Root .. Install_Steps.Writing_Generation
                          then not Root_Mounts),
                "interrupted at " & Install_Steps.Name (Interrupted) & ": not bootable" &
                (if Interrupted in Install_Steps.Formatting_Root .. Install_Steps.Writing_Generation
                 then ", root not mountable" else ""));
      end;
   end loop;
   Stop_At := Install_Steps.Done;
   Armed := False;
   Stopped := False;
   Mismatches := 0;
   Writer.Write_Disk (Shape, Layout, ESP_Files, Root_Files, Result, Reached);
   Check (Result = Writer.Written and then Reached = Install_Steps.Done and then Mismatches = 0,
          "layout written and read back");

   Check (Partition_Tables.Classify_MBR (Read_At (0, Partition_Tables.Sector_Bytes)) = Partition_Tables.Protective_GPT, "protective MBR");
   for Copy in GPT_Image.Header_Copy loop
      declare
         Location : constant Unsigned_64 := GPT_Image.Header_LBA (Shape, Copy);
         Header : Partition_Tables.GPT_Header;
         Outcome : Partition_Tables.Header_Result;
      begin
         Partition_Tables.Decode_Header (Read_At (Location * Unsigned_64 (Block), Block), Location,
                        Shape.Blocks, Header, Outcome);
         Check (Outcome = Partition_Tables.Header_Valid
                and then Header.Entries_LBA = GPT_Image.Entries_LBA (Shape, Copy)
                and then Header.Entries_CRC =
                  Partition_Tables.CRC32 (Read_At (Header.Entries_LBA * Unsigned_64 (Block), Entry_Array_Bytes))
                and then Header.First_Usable = First_Usable (Shape)
                and then Header.Last_Usable = Last_Usable (Shape),
                GPT_Image.Header_Copy'Image (Copy) & " header decodes valid");
         declare
            Table : constant Bytes :=
              Read_At (Header.Entries_LBA * Unsigned_64 (Block), Entry_Array_Bytes);
            ESP : constant Partition_Tables.Partition_Entry := Partition_Tables.Decode_Entry (Table, 0);
            Root : constant Partition_Tables.Partition_Entry := Partition_Tables.Decode_Entry (Table, Entry_Bytes);
         begin
            Check (ESP.Kind = Partition_Tables.EFI_System and then Root.Kind = Partition_Tables.Linux_Filesystem
                   and then ESP.First = ESP_First (Shape)
                   and then Root.First = Root_First (Shape, Layout.Disk)
                   and then Root.Last = Last_Usable (Shape)
                   and then Partition_Tables.Entry_In_Range (Header, ESP) and then Partition_Tables.Entry_In_Range (Header, Root)
                   and then Partition_Tables.Is_Unused (Partition_Tables.Decode_Entry (Table, 2 * Entry_Bytes)),
                   GPT_Image.Header_Copy'Image (Copy) & " entries");
         end;
      end;
   end loop;
   GNAT.OS_Lib.Close (Disk);
   declare
      Root_Volume : constant Ext2_Image.Volume :=
        Ext2_Image.Plan ((Last_Usable (Shape) - Root_First (Shape, Layout.Disk) + 1)
                         * Unsigned_64 (Block), Layout.Root.Inode_Ratio);
   begin
      Ada.Text_IO.Put_Line
        ("disk-layout: esp-offset=" & Unsigned_64'Image (ESP_First (Shape) * Unsigned_64 (Block))
         & " esp-bytes=" & Unsigned_64'Image (MiB_Blocks (Shape, Layout.Disk.ESP_Size)
                                              * Unsigned_64 (Block))
         & " root-offset=" & Unsigned_64'Image (Root_First (Shape, Layout.Disk)
                                                * Unsigned_64 (Block))
         & " root-groups=" & Unsigned_32'Image (Root_Volume.Groups));
   end;
   if Failures = 0 then
      Ada.Text_IO.Put_Line ("DISK-LAYOUT-CHECK: PASS");
   else
      Ada.Text_IO.Put_Line ("DISK-LAYOUT-CHECK: FAIL");
      Set_Exit_Status (Ada.Command_Line.Failure);
   end if;
end Main;
