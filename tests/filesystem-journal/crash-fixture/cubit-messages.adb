with Ada.Environment_Variables;
with System.Storage_Elements;
with Ada.Containers.Hashed_Maps;
with Ada.Direct_IO;
with Ada.Environment_Variables;
with Ada.Strings.Unbounded;
with Ada.Text_IO;
with GNAT.OS_Lib;
with CuBit.Block_Devices; use CuBit.Block_Devices;

--  A file-backed block device with a volatile write cache and power cuts.
--  Completed writes are held in a cache overlay (reads see them) until a
--  FLUSH, or a FUA write, makes them durable in the image. CUBIT_CUT=N cuts
--  power as command N arrives: the process exits and the image keeps only
--  what was durable, plus a CUBIT_LOSS choice of the cached writes (all,
--  none, or every other one): a volatile cache may persist any subset.
--  CUBIT_FAIL=N instead fails command N with an error reply, before its
--  transfer or (CUBIT_FAIL_MODE=AFTER) after it; the run continues.
package body CuBit.Messages is
   type Sector is array (Natural range 0 .. 511) of Unsigned_8;
   package IO is new Ada.Direct_IO (Sector);
   function Hash (Key : Unsigned_64) return Ada.Containers.Hash_Type is
     (Ada.Containers.Hash_Type (Key mod Ada.Containers.Hash_Type'Modulus));
   package Sector_Maps is new Ada.Containers.Hashed_Maps
     (Key_Type => Unsigned_64, Element_Type => Sector, Hash => Hash,
      Equivalent_Keys => "=");
   type Loss_Mode is (Keep_All, Lose_All, Lose_Alternate);

   Image : IO.File_Type;
   Image_Size : Unsigned_64;
   Cached : Sector_Maps.Map;
   Order : Sector_Maps.Map; -- sector -> sequence of its latest cached write
   Commands : Natural := 0;
   Cut_At : Natural := 0; -- 0: never
   Loss : Loss_Mode := Keep_All;
   Fail_At : Natural := 0; -- 0: never
   Fail_After : Boolean := False;

   procedure Persist (Number : Unsigned_64; Data : Sector) is
   begin
      IO.Write (Image, Data, IO.Positive_Count (Number + 1));
   end Persist;

   procedure Drain is
   begin
      for Position in Cached.Iterate loop
         Persist (Sector_Maps.Key (Position), Sector_Maps.Element (Position));
      end loop;
      Cached.Clear;
      Order.Clear;
   end Drain;

   procedure Power_Cut is
   begin
      for Position in Cached.Iterate loop
         declare
            Stamp : constant Sector := Order.Element (Sector_Maps.Key (Position));
         begin
            if Loss = Keep_All or else
              (Loss = Lose_Alternate and then Stamp (0) mod 2 = 0)
            then
               Persist (Sector_Maps.Key (Position), Sector_Maps.Element (Position));
            end if;
         end;
      end loop;
      IO.Close (Image);
      Ada.Text_IO.Put_Line ("POWER CUT at command" & Commands'Image);
      GNAT.OS_Lib.OS_Exit (0);
   end Power_Cut;

   procedure Open_Image (Path : String) is
      use Ada.Environment_Variables;
   begin
      IO.Open (Image, IO.Inout_File, Path);
      Image_Size := Unsigned_64 (IO.Size (Image)) * 512;
      if Exists ("CUBIT_CUT") then
         Cut_At := Natural'Value (Value ("CUBIT_CUT"));
      end if;
      if Exists ("CUBIT_LOSS") then
         Loss := Loss_Mode'Value (Value ("CUBIT_LOSS"));
      end if;
      if Exists ("CUBIT_FAIL") then
         Fail_At := Natural'Value (Value ("CUBIT_FAIL"));
      end if;
      Fail_After := Exists ("CUBIT_FAIL_MODE") and then
        Value ("CUBIT_FAIL_MODE") = "AFTER";
   end Open_Image;

   procedure Close_Image is
   begin
      if IO.Is_Open (Image) then
         Drain;
         IO.Close (Image);
      end if;
      Ada.Text_IO.Put_Line ("DEVICE COMMANDS" & Commands'Image);
   end Close_Image;

   function Device_Commands return Natural is (Commands);

   procedure debugPrint (value : String) is
   begin
      Ada.Text_IO.Put (value);
   end debugPrint;

   function capCall (slot : Unsigned_64; msg : in out Message) return MessageTag is
      pragma Unreferenced (slot);
      Op : constant Unsigned_32 := msg.tag.label;
      First : constant Unsigned_64 := msg.words (0);
      Count : constant Unsigned_64 := msg.words (2) * 512;
   begin
      if Op /= OP_DESCRIBE_DEVICE then
         Commands := Commands + 1;
         if Commands = Cut_At then
            Power_Cut;
         end if;
         if Commands = Fail_At and then not Fail_After then
            Ada.Text_IO.Put_Line ("FAILED command" & Commands'Image);
            msg := ((REPLY_ERROR, 1, 0, 0), 0, [others => 0]);
            return msg.tag;
         end if;
      end if;
      if Op = OP_DESCRIBE_DEVICE then
         msg := ((REPLY_OK, 4, 0, 0), 0,
                 [Image_Size / 512, Pack_Sizes (512, 512), 8,
                  Pack_Properties (FEATURE_FLUSH or FEATURE_VOLATILE_CACHE or FEATURE_FUA,
                                   Fixed_Media)]);
      elsif Op = OP_FLUSH_DEVICE then
         Drain;
         msg := ((REPLY_OK, 1, 0, 0), 0, [others => 0]);
      else
         pragma Assert (Op in OP_READ_BLOCKS | OP_WRITE_BLOCKS);
         pragma Assert (Count in 1 .. Grant_Buffer'Length);
         pragma Assert (First * 512 <= Image_Size and then Count <= Image_Size - First * 512);
         for Index in 0 .. Natural (Count / 512) - 1 loop
            declare
               Data : Sector with Import, Address => Grant_Buffer (Index * 512)'Address;
               Number : constant Unsigned_64 := First + Unsigned_64 (Index);
               Stamp : Sector := [others => 0];
            begin
               if Op = OP_READ_BLOCKS then
                  if Cached.Contains (Number) then
                     Data := Cached.Element (Number);
                  else
                     IO.Read (Image, Data, IO.Positive_Count (Number + 1));
                  end if;
               elsif (msg.tag.flags and WRITE_FLAG_FUA) /= 0 then
                  Cached.Exclude (Number);
                  Order.Exclude (Number);
                  Persist (Number, Data);
               else
                  Stamp (0) := Unsigned_8 (Commands mod 256);
                  Cached.Include (Number, Data);
                  Order.Include (Number, Stamp);
               end if;
            end;
         end loop;
         msg := ((REPLY_OK, 1, 0, 0), 0, [0 => Count, others => 0]);
      end if;
      if Commands = Fail_At and then Fail_After and then Op /= OP_DESCRIBE_DEVICE then
         Ada.Text_IO.Put_Line ("FAILED command" & Commands'Image);
         msg := ((REPLY_ERROR, 1, 0, 0), 0, [others => 0]);
      end if;
      return msg.tag;
   end capCall;
   function syscall
     (call : Unsigned_64; arg0 : Unsigned_64 := 0; arg1 : Unsigned_64 := 0;
      arg2 : Unsigned_64 := 0; arg3 : Unsigned_64 := 0;
      arg4 : Unsigned_64 := 0; arg5 : Unsigned_64 := 0) return Unsigned_64
   is
      pragma Unreferenced (arg1, arg2, arg3, arg4, arg5);
      Page : constant := 4096;
      type Region is array (Natural range <>) of Unsigned_8;
      type Region_Access is access Region;
   begin
      if call = SYSCALL_GETTIME then
         return 0;
      elsif call = SYSCALL_INFO and then arg0 = SYSINFO_WALL_CLOCK_OFFSET then
         declare
            Clock : constant String :=
              Ada.Environment_Variables.Value ("CUBIT_TEST_WALL_CLOCK", "");
         begin
            return (if Clock = "" then Unsigned_64'Last
                    else Unsigned_64'Value (Clock) * 1_000);
         end;
      end if;
      if call /= SYSCALL_ALLOCATE_OWNED_MEMORY or else arg0 = 0 or else
        arg0 > 16 * 1024 * 1024
      then
         return Unsigned_64'Last;
      end if;
      declare
         Area : constant Region_Access :=
           new Region'(0 .. Natural (arg0) + Page - 1 => 0);
         Base : constant Unsigned_64 := Unsigned_64
           (System.Storage_Elements.To_Integer (Area.all (0)'Address));
      begin
         return (Base + Page - 1) and not (Page - 1);
      end;
   end syscall;
end CuBit.Messages;
