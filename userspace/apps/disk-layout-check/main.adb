------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  @summary
--  Headless check of the installer's layout writer on a real block device
--  (tests/headless ufs-layout): opens disk 0 read-write through
--  disk-install, writes the GPT, the FAT32 ESP and the ext2 root through
--  CuBit.Block_Client, and flushes. The host then checks the image with
--  sgdisk, fsck.fat and e2fsck (tests/disk-layout/check-image.sh).
------------------------------------------------------------------------------
pragma Ada_2022;
with Interfaces; use Interfaces;
with CuBit.Messages; use CuBit.Messages;
with CuBit.Block_Devices; use CuBit.Block_Devices;
with CuBit.Block_Client; use CuBit.Block_Client;
with CuBit.Disk_Install; use CuBit.Disk_Install;
with CuBit.Disk_Install.Client;
with Disk_Layout; use Disk_Layout;
with Layout_Writer;
with Startup;

procedure Main is
   package DIC renames CuBit.Disk_Install.Client;

   Session_Pages : constant Page_Count := 16;
   --  TODO(STARTUP-001): wait on the disk's readiness event instead of
   --  asking again; polling is a stopgap until readiness events exist.
   Disk_Wait_Milliseconds : constant := 30_000;
   Disk_Poll_Milliseconds : constant := 250;

   Item : Session;
   Block : Unsigned_64 := 1;
   --  The session buffer holds only zeros (Zero need not clear it again).
   Buffer_Zero : Boolean := False;

   type Byte_View is array (Natural range <>) of Unsigned_8;

   procedure Clear_Buffer is
      View : Byte_View (0 .. Session_Pages * Page_Bytes - 1)
        with Import, Address => Buffer (Item);
   begin
      View := [others => 0];
      Buffer_Zero := True;
   end Clear_Buffer;

   procedure Put (Offset : Byte_Offset; Data : Bytes; OK : out Boolean) is
      View : Byte_View (0 .. Data'Length - 1) with Import, Address => Buffer (Item);
      Moved : Unsigned_64;
      Result : Transfer_Result;
   begin
      OK := Offset mod Block = 0 and then Unsigned_64 (Data'Length) mod Block = 0
        and then Unsigned_64 (Data'Length) / Block in 1 .. Transfer_Limit (Item);
      if not OK then
         return;
      end if;
      for I in View'Range loop
         View (I) := Data (Data'First + I);
      end loop;
      Buffer_Zero := False;
      Write (Item, Offset / Block, Unsigned_64 (Data'Length) / Block, False, Moved, Result);
      OK := Result = Transferred;
   end Put;

   procedure Zero (Offset : Byte_Offset; Length : Unsigned_64; OK : out Boolean) is
      Next : Unsigned_64 := Offset / Block;
      Left : Unsigned_64 := Length / Block;
      Step : Unsigned_64;
      Moved : Unsigned_64;
      Result : Transfer_Result;
   begin
      OK := Offset mod Block = 0 and then Length mod Block = 0;
      if not Buffer_Zero then
         Clear_Buffer;
      end if;
      while OK and then Left > 0 loop
         Step := Unsigned_64'Min (Left, Transfer_Limit (Item));
         Write (Item, Next, Step, False, Moved, Result);
         OK := Result = Transferred;
         Next := Next + Step;
         Left := Left - Step;
      end loop;
   end Zero;

   package Writer is new Layout_Writer (Put, Zero);
   use type Writer.Write_Result;

   function Id (Seed : Unsigned_8) return GUID is
     ([for I in GUID'Range => Seed + Unsigned_8 (I)]);

   Disk : DIC.Opened_Disk;
   Opening : Open_Result;
   Result : Writer.Write_Result;
   Durable : Boolean;
   Started : constant Unsigned_64 := syscall (SYSCALL_GETTIME);
   Ignore : Unsigned_64;
begin
   loop
      Disk := DIC.Open (Startup.Disk_Install, (Disk => 0, Mode => Read_Write));
      exit when Disk.Opened
        or else syscall (SYSCALL_GETTIME) - Started > Disk_Wait_Milliseconds;
      Ignore := syscall (SYSCALL_SLEEP, Disk_Poll_Milliseconds);
   end loop;
   if not Disk.Opened then
      debugPrint ("disk-layout-check: no disk FAIL" & ASCII.LF);
      return;
   end if;
   Open (Disk.Device, Session_Pages, Item, Opening);
   if Opening /= Opened then
      debugPrint ("disk-layout-check: session FAIL" & ASCII.LF);
      return;
   end if;
   declare
      Size : constant Unsigned_32 := Description (Item).logicalBlockSize;
   begin
      if Size not in 512 | 4096 then
         debugPrint ("disk-layout-check: unsupported block size FAIL" & ASCII.LF);
         return;
      end if;
      Block := Unsigned_64 (Size);
      declare
         Shape : constant Disk_Shape :=
           (Block_Size => Device_Block_Bytes (Size),
            Blocks => Unsigned_64'Min (Description (Item).blockCount, Maximum_Disk_Blocks));
         --  The same layout tests/disk-layout writes on Linux.
         Layout : constant Writer.Install_Layout :=
           (Disk => (ESP_Size => Minimum_ESP_MiB, Disk_Id => Id (16#10#),
                     ESP_Id => Id (16#20#), Root_Id => Id (16#30#)),
            ESP_Serial => 16#CB17_0001#, ESP_Label => "CUBIT ESP  ",
            Root => (Inode_Ratio => 65_536, Id => Id (16#40#),
                     Name => "cubit-root" & [1 .. 6 => ASCII.NUL],
                     Created => 1_793_000_000));
      begin
         if not Fits (Shape, Layout.Disk) then
            debugPrint ("disk-layout-check: disk too small FAIL" & ASCII.LF);
            return;
         end if;
         Writer.Write_Disk (Shape, Layout, Result);
      end;
   end;
   Flush (Item, Durable);
   if Result = Writer.Written and then Durable then
      debugPrint ("disk-layout-check: layout written and flushed PASS" & ASCII.LF);
      debugPrint ("TEST: PASS ufs-layout" & ASCII.LF);
   else
      debugPrint ("disk-layout-check: layout FAIL" & ASCII.LF);
   end if;
end Main;
