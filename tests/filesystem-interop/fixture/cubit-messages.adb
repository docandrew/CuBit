with System.Storage_Elements;
with Ada.Direct_IO;
with Ada.Strings.Unbounded;
with Ada.Text_IO;
with Ada.Environment_Variables;
with Ada.Containers.Vectors;
with GNAT.OS_Lib;
with CuBit.Block_Devices; use CuBit.Block_Devices;

package body CuBit.Messages is
   type Sector is array (Natural range 0 .. 511) of Unsigned_8;
   package IO is new Ada.Direct_IO (Sector);
   Image : IO.File_Type;
   Image_Path : Ada.Strings.Unbounded.Unbounded_String;
   Image_Size : Unsigned_64;

   --  CUBIT_TEST_CUT_AFTER_WRITES=N: a power cut after the Nth write
   --  request completes (the process ends when the next write arrives),
   --  for crash-order tests (tests/filesystem-fat). Unset: no cut.
   --  CUBIT_TEST_CUT_LOSES says what a volatile write cache lost at the
   --  cut, of the writes since the last flush: "none" (default), "all",
   --  "older" (all but the last request) or "random:SEED" (each request
   --  kept or lost by a seeded coin). The lost ones are undone.
   Cut_After : constant Long_Long_Integer :=
     Long_Long_Integer'Value (Ada.Environment_Variables.Value ("CUBIT_TEST_CUT_AFTER_WRITES", "-1"));
   Cut_Loses : constant String := Ada.Environment_Variables.Value ("CUBIT_TEST_CUT_LOSES", "none");
   Writes_Done : Long_Long_Integer := 0;
   type Undo is record
      Request : Long_Long_Integer;
      Position : IO.Positive_Count;
      Old : Sector;
   end record;
   package Undo_Logs is new Ada.Containers.Vectors (Positive, Undo);
   Unflushed : Undo_Logs.Vector;

   --  Undo what the cut lost (see above), newest first.
   procedure Lose_Unflushed is
      Seed : Unsigned_64 := 0;
      Last : constant Long_Long_Integer := Writes_Done;
      function Lost (Request : Long_Long_Integer) return Boolean is
         Mixed : Unsigned_64 := (Seed + Unsigned_64 (Request)) * 16#9E37_79B9_7F4A_7C15#;
      begin
         if Cut_Loses = "all" then
            return True;
         elsif Cut_Loses = "older" then
            return Request /= Last;
         elsif Cut_Loses'Length > 7 and then Cut_Loses (Cut_Loses'First .. Cut_Loses'First + 6) = "random:" then
            Mixed := Mixed xor Shift_Right (Mixed, 29);
            return (Mixed and 16#100#) /= 0;
         end if;
         return False;
      end Lost;
   begin
      if Cut_Loses'Length > 7 and then Cut_Loses (Cut_Loses'First .. Cut_Loses'First + 6) = "random:" then
         Seed := Unsigned_64'Value (Cut_Loses (Cut_Loses'First + 7 .. Cut_Loses'Last));
      end if;
      for I in reverse 1 .. Integer (Unflushed.Length) loop
         if Lost (Unflushed (I).Request) then
            IO.Write (Image, Unflushed (I).Old, Unflushed (I).Position);
         end if;
      end loop;
   end Lose_Unflushed;

   procedure Open_Image (Path : String) is
   begin
      Image_Path := Ada.Strings.Unbounded.To_Unbounded_String (Path);
      IO.Open (Image, IO.Inout_File, Path);
      Image_Size := Unsigned_64 (IO.Size (Image)) * 512;
   end Open_Image;

   procedure Close_Image is
   begin
      if IO.Is_Open (Image) then IO.Close (Image); end if;
   end Close_Image;

   procedure debugPrint (value : String) is
   begin
      Ada.Text_IO.Put (value);
   end debugPrint;

   function capCall (slot : CuBit.Handles.Endpoint; msg : in out Message; Deadline : Unsigned_64)
     return MessageTag
   is
      pragma Unreferenced (Deadline);
      pragma Unreferenced (slot);
      Op : constant Unsigned_32 := msg.tag.label;
      Offset : constant Unsigned_64 := msg.words (0) * 512;
      Count : constant Unsigned_64 := msg.words (2) * 512;
   begin
      if Op = OP_DESCRIBE_DEVICE then
         msg := ((label => REPLY_OK, length => 4, protocol => 0, ipc => <>, reserved => 0), 0,
                 [Image_Size / 512, Pack_Sizes (512, 512), 8,
                  Pack_Properties (FEATURE_FLUSH or FEATURE_VOLATILE_CACHE, Fixed_Media)]);
      elsif Op = OP_FLUSH_DEVICE then
         --  Flush host buffering; no power-loss durability claim is made.
         IO.Close (Image);
         IO.Open (Image, IO.Inout_File, Ada.Strings.Unbounded.To_String (Image_Path));
         Unflushed.Clear;
         msg := ((label => REPLY_OK, length => 1, protocol => 0, ipc => <>, reserved => 0), 0, [others => 0]);
      else
         pragma Assert (Op in OP_READ_BLOCKS | OP_WRITE_BLOCKS);
         if Op = OP_WRITE_BLOCKS and then Cut_After >= 0 then
            if Writes_Done = Cut_After then
               Lose_Unflushed;
               IO.Close (Image);
               Ada.Text_IO.Put_Line ("fixture: power cut after" & Writes_Done'Image & " writes");
               GNAT.OS_Lib.OS_Exit (3);
            end if;
            Writes_Done := Writes_Done + 1;
         end if;
         pragma Assert (Count in 1 .. Grant_Buffer'Length);
         pragma Assert (Offset <= Image_Size and then Count <= Image_Size - Offset);
         for Index in 0 .. Natural (Count / 512) - 1 loop
            declare
               Data : Sector with Import,
                 Address => Grant_Buffer (Index * 512)'Address;
               Position : constant IO.Positive_Count :=
                 IO.Positive_Count (Offset / 512 + Unsigned_64 (Index) + 1);
            begin
               if Op = OP_READ_BLOCKS then
                  IO.Read (Image, Data, Position);
               else
                  if Cut_After >= 0 and then Cut_Loses /= "none" then
                     declare
                        Old : Sector;
                     begin
                        IO.Read (Image, Old, Position);
                        Unflushed.Append (Undo'(Request => Writes_Done, Position => Position, Old => Old));
                     end;
                  end if;
                  IO.Write (Image, Data, Position);
               end if;
            end;
         end loop;
         msg := ((label => REPLY_OK, length => 1, protocol => 0, ipc => <>, reserved => 0), 0, [0 => Count, others => 0]);
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
