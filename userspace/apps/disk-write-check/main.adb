------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  @summary
--  Headless check of whole-disk writes behind the block write grant
--  (HW-001 step 4, tests/headless ufs-write), standing in for the
--  installer: it holds the disk-install authority.
--
--  1. devmgr refuses Open_Disk through the ordinary device-manager endpoint
--     and for a disk it does not serve.
--  2. Opened read-only, disk 0 describes itself read only, reads, and
--     refuses a write; the block is unchanged afterwards.
--  3. Opened read-write, it takes a multi-block write and a FUA write,
--     flushes, and reads both back. The host then checks the image file.
------------------------------------------------------------------------------
pragma Ada_2022;
with Interfaces; use Interfaces;
with System.Storage_Elements; use System.Storage_Elements;
with CuBit.Messages; use CuBit.Messages;
with CuBit.Handles;
with CuBit.Block_Devices; use CuBit.Block_Devices;
with CuBit.Block_Client; use CuBit.Block_Client;
with CuBit.Disk_Install; use CuBit.Disk_Install;
with CuBit.Disk_Install.Client;
with Startup;

procedure Main is
   package DIC renames CuBit.Disk_Install.Client;

   --  Where the pattern goes (tests/headless/check-ufs-write.py reads the
   --  same blocks): inside the image's basic data partition.
   Pattern_Block : constant Unsigned_64 := 4096;
   Pattern_Blocks : constant Unsigned_64 := 4;
   FUA_Block : constant Unsigned_64 := Pattern_Block + Pattern_Blocks;
   Session_Pages : constant Page_Count := 16;
   Disk_Wait_Milliseconds : constant := 30_000;
   Disk_Poll_Milliseconds : constant := 250;
   --  The first bytes of every written block, then (offset * 7 + block)
   --  mod 256 for the rest.
   Magic : constant String := "CUBIT-UFS-WRITE ";
   Byte_Values : constant := 256;
   Offset_Stride : constant := 7;

   Passed : Boolean := True;

   procedure Check (Condition : Boolean; Name : String) is
   begin
      if not Condition then
         Passed := False;
      end if;
      debugPrint ("disk-write-check: " & Name &
                  (if Condition then " PASS" else " FAIL") & ASCII.LF);
   end Check;

   function Expected (Block : Unsigned_64; Offset : Natural) return Unsigned_8 is
     (if Offset < Magic'Length
      then Character'Pos (Magic (Magic'First + Offset))
      else Unsigned_8 ((Unsigned_64 (Offset) * Offset_Stride + Block) mod Byte_Values));

   type Byte_View is array (Natural range <>) of Unsigned_8;

   --  Fills Count blocks of the session buffer with the pattern of blocks
   --  First onwards.
   procedure Fill (Item : Session; First, Count : Unsigned_64) is
      Size : constant Natural := Natural (Description (Item).logicalBlockSize);
      View : Byte_View (0 .. Natural (Count) * Size - 1)
        with Import, Address => Buffer (Item);
   begin
      for I in View'Range loop
         View (I) := Expected (First + Unsigned_64 (I / Size), I mod Size);
      end loop;
   end Fill;

   function Holds_Pattern (Item : Session; First, Count : Unsigned_64) return Boolean is
      Size : constant Natural := Natural (Description (Item).logicalBlockSize);
      View : Byte_View (0 .. Natural (Count) * Size - 1)
        with Import, Address => Buffer (Item);
   begin
      for I in View'Range loop
         if View (I) /= Expected (First + Unsigned_64 (I / Size), I mod Size) then
            return False;
         end if;
      end loop;
      return True;
   end Holds_Pattern;

   procedure Clear (Item : Session) is
      View : Byte_View (0 .. Session_Pages * Page_Bytes - 1)
        with Import, Address => Buffer (Item);
   begin
      View := [others => 0];
   end Clear;

   Disk : DIC.Opened_Disk;
   Item : Session;
   Opening : Open_Result;
   Moved : Unsigned_64;
   Result : Transfer_Result;
   Durable : Boolean;
begin
   --  1. No capability without the badge, or for a disk devmgr lacks.
   Disk := DIC.Open (Startup.Device_Manager, (Disk => 0, Mode => Read_Write));
   Check (not Disk.Opened, "device-manager endpoint cannot open a disk");

   --  2. Read only: describe, read, refused write. The UFS driver starts
   --  after logstore, so disk 0 may appear only after this program runs:
   --  it is asked for until Disk_Wait_Milliseconds have passed.
   declare
      Started : constant Unsigned_64 := syscall (SYSCALL_GETTIME);
      Ignore : Unsigned_64;
   begin
      loop
         Disk := DIC.Open (Startup.Disk_Install, (Disk => 0, Mode => Read_Only));
         exit when Disk.Opened
           or else syscall (SYSCALL_GETTIME) - Started > Disk_Wait_Milliseconds;
         Ignore := syscall (SYSCALL_SLEEP, Disk_Poll_Milliseconds);
      end loop;
   end;
   Check (Disk.Opened, "disk 0 opened read-only");
   if Disk.Opened then
      Open (Disk.Device, Session_Pages, Item, Opening);
      Check (Opening = Opened, "read-only session");
      if Item.Open then
         Check (Is_Read_Only (Description (Item)), "read-only describe");
         Read (Item, 0, 1, Moved, Result);
         declare
            Signature : Byte_View (0 .. 1)
              with Import, Address => Buffer (Item) + Storage_Offset (510);
         begin
            Check (Result = Transferred and then Signature = [16#55#, 16#AA#],
                   "read block 0 (protective MBR)");
         end;
         Fill (Item, Pattern_Block, 1);
         Write (Item, Pattern_Block, 1, False, Moved, Result);
         Check (Result = Refused and then Moved = 0, "write without grant refused");
         Clear (Item);
         Read (Item, Pattern_Block, 1, Moved, Result);
         Check (Result = Transferred and then not Holds_Pattern (Item, Pattern_Block, 1),
                "refused write left the block unchanged");
      end if;
   end if;

   Disk := DIC.Open (Startup.Disk_Install, (Disk => 1, Mode => Read_Only));
   Check (not Disk.Opened, "no disk 1");

   --  3. Read-write: write, FUA write, flush, read back.
   Disk := DIC.Open (Startup.Disk_Install, (Disk => 0, Mode => Read_Write));
   Check (Disk.Opened, "disk 0 opened read-write");
   if Disk.Opened then
      Open (Disk.Device, Session_Pages, Item, Opening);
      Check (Opening = Opened, "read-write session");
      if Item.Open then
         Check (not Is_Read_Only (Description (Item))
                and then Can_Persist (Description (Item))
                and then Supports_FUA (Description (Item)),
                "read-write describe (flush, FUA)");
         Fill (Item, Pattern_Block, Pattern_Blocks);
         Write (Item, Pattern_Block, Pattern_Blocks, False, Moved, Result);
         Check (Result = Transferred, "write" & Pattern_Blocks'Image & " blocks");
         Fill (Item, FUA_Block, 1);
         Write (Item, FUA_Block, 1, True, Moved, Result);
         Check (Result = Transferred, "FUA write");
         Flush (Item, Durable);
         Check (Durable, "flush");
         Clear (Item);
         Read (Item, Pattern_Block, Pattern_Blocks + 1, Moved, Result);
         Check (Result = Transferred
                and then Holds_Pattern (Item, Pattern_Block, Pattern_Blocks + 1),
                "readback");
      end if;
   end if;

   if Passed then
      debugPrint ("TEST: PASS ufs-write" & ASCII.LF);
   else
      debugPrint ("TEST: FAIL ufs-write" & ASCII.LF);
   end if;
end Main;
