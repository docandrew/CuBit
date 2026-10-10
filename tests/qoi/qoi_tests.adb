--  Hosted tests of CuBit.QOI (tests/qoi/README.md): reference round trips
--  in many chunk sizes, malformed and truncated streams, deterministic bit
--  flips, and the real wallpaper assets when they have been built.
with Ada.Command_Line;
with Ada.Directories;
with Ada.Streams.Stream_IO;
with Ada.Strings.Fixed;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Text_IO; use Ada.Text_IO;
with Interfaces; use Interfaces;
with CuBit.QOI; use CuBit.QOI;

procedure QOI_Tests is
   Fixtures : constant String := Ada.Command_Line.Argument (1);
   --  Optional: the built wallpaper package and its raster references.
   Assets   : constant String :=
     (if Ada.Command_Line.Argument_Count >= 3 then Ada.Command_Line.Argument (2) else "");
   References : constant String :=
     (if Ada.Command_Line.Argument_Count >= 3 then Ada.Command_Line.Argument (3) else "");
   Checks : Natural := 0;
   Failures : Natural := 0;

   type Chunk_Sizes is array (Positive range <>) of Natural;
   All_Sizes   : constant Chunk_Sizes := [0, 1, 2, 3, 5, 13, 64, 4096];
   Few_Sizes   : constant Chunk_Sizes := [0, 1, 3];
   type Bytes_Access is access Byte_Array;
   type Pixels_Access is access Pixel_Buffer;

   procedure Check (Condition : Boolean; What : String) is
   begin
      Checks := Checks + 1;
      if not Condition then
         Failures := Failures + 1;
         Put_Line ("FAIL: " & What);
      end if;
   end Check;

   function Load (Path : String) return Bytes_Access is
      use Ada.Streams.Stream_IO;
      File : Ada.Streams.Stream_IO.File_Type;
      Size : constant Natural := Natural (Ada.Directories.Size (Path));
      Data : constant Bytes_Access := new Byte_Array (1 .. Size);
   begin
      Open (File, In_File, Path);
      Byte_Array'Read (Stream (File), Data.all);
      Close (File);
      return Data;
   end Load;

   --  RGBA bytes to the decoder's 16#AARRGGBB# pixels.
   function Expected (RGBA : Byte_Array; Index : Natural) return Pixel is
      Base : constant Positive := RGBA'First + Index * 4;
   begin
      return Shift_Left (Unsigned_32 (RGBA (Base + 3)), 24) or
        Shift_Left (Unsigned_32 (RGBA (Base)), 16) or
        Shift_Left (Unsigned_32 (RGBA (Base + 1)), 8) or Unsigned_32 (RGBA (Base + 2));
   end Expected;

   --  Decode Data in chunks of Chunk bytes (0: all at once).
   procedure Decode
     (Data : Byte_Array; Limit : Pixel_Limit; Chunk : Natural;
      Output : in out Pixel_Buffer; D : out Decoder)
   is
      First : Positive := Data'First;
      Last  : Natural;
   begin
      Start (D, Limit);
      while First <= Data'Last loop
         Last := (if Chunk = 0 then Data'Last else Natural'Min (Data'Last, First + Chunk - 1));
         Feed (D, Data (First .. Last), Output);
         First := Last + 1;
      end loop;
      Finish (D);
   end Decode;

   procedure Valid_Fixture (File, Reference : String; W, H : Positive) is
      Data : constant Bytes_Access := Load (Fixtures & "/" & File);
      RGBA : constant Bytes_Access := Load (Fixtures & "/" & Reference);
      Count : constant Positive := W * H;
      Output : constant Pixels_Access := new Pixel_Buffer (0 .. Count - 1);
      D : Decoder;
      Same : Boolean;
   begin
      for Chunk of All_Sizes loop
         Output.all := [others => 16#DEAD_BEEF#];
         Decode (Data.all, Count, Chunk, Output.all, D);
         Check (Current (D) = Complete, File & " completes, chunk" & Chunk'Image &
                " error " & Error (D)'Image);
         Check (Width (D) = W and then Height (D) = H and then
                Written (D) = Count, File & " size");
         Same := True;
         for I in 0 .. Count - 1 loop
            Same := Same and then Output (I) = Expected (RGBA.all, I);
         end loop;
         Check (Same, File & " pixels, chunk" & Chunk'Image);
      end loop;
      --  A larger caller limit and buffer: the image still decodes exactly
      --  and nothing past it is written.
      declare
         Wide : constant Pixels_Access := new Pixel_Buffer (0 .. Count + 99);
      begin
         Wide.all := [others => 16#1234_5678#];
         Decode (Data.all, Count + 100, 7, Wide.all, D);
         Check (Current (D) = Complete and then
                (for all I in Count .. Count + 99 => Wide (I) = 16#1234_5678#),
                File & " larger limit");
      end;
      --  Every proper prefix is rejected, never a partial image.
      if Data'Length <= 4096 then
         for Cut in 0 .. Data'Length - 1 loop
            Decode (Data (1 .. Cut), Count, 0, Output.all, D);
            Check (Current (D) = Failed and then Error (D) = Truncated,
                   File & " prefix" & Cut'Image);
         end loop;
      end if;
   end Valid_Fixture;

   procedure Malformed_Fixture (File : String; Limit : Pixel_Limit; Expect : Failure) is
      Data : constant Bytes_Access := Load (Fixtures & "/" & File);
      Output : constant Pixels_Access := new Pixel_Buffer (0 .. Limit - 1);
      D : Decoder;
   begin
      for Chunk of Few_Sizes loop
         Decode (Data.all, Limit, Chunk, Output.all, D);
         Check (Current (D) = Failed and then Error (D) = Expect,
                File & " rejected as " & Expect'Image & ", got " & Current (D)'Image &
                " " & Error (D)'Image);
         Check (Written (D) <= Total (D) and then Total (D) <= Limit, File & " bounded");
      end loop;
   end Malformed_Fixture;

   --  Deterministic bit flips: each mutant decodes or is rejected, with
   --  the output count bounded (the decoder is total; -gnata checks it).
   procedure Flip_Fixture (File : String; Limit : Pixel_Limit; Mutants : Positive) is
      Original : constant Bytes_Access := Load (File);
      Data : constant Bytes_Access := new Byte_Array'(Original.all);
      Output : constant Pixels_Access := new Pixel_Buffer (0 .. Limit - 1);
      State : Unsigned_64 := 16#9E37_79B9_7F4A_7C15#;
      Completed, Rejected : Natural := 0;
      D : Decoder;
      function Next return Unsigned_64 is
      begin
         State := State xor Shift_Left (State, 13);
         State := State xor Shift_Right (State, 7);
         State := State xor Shift_Left (State, 17);
         return State;
      end Next;
   begin
      for M in 1 .. Mutants loop
         Data.all := Original.all;
         for Flip in 1 .. 1 + Natural (Next mod 3) loop
            declare
               At_Byte : constant Positive := 1 + Natural (Next mod Unsigned_64 (Data'Length));
            begin
               Data (At_Byte) := Data (At_Byte) xor Shift_Left (1, Natural (Next mod 8));
            end;
         end loop;
         Decode (Data.all, Limit, 1 + Natural (Next mod 50), Output.all, D);
         Check (Current (D) in Complete | Failed and then Written (D) <= Total (D) and then
                Total (D) <= Limit, File & " mutant" & M'Image);
         if Current (D) = Complete then
            Completed := Completed + 1;
         else
            Rejected := Rejected + 1;
         end if;
      end loop;
      Put_Line ("flips " & File & ":" & Mutants'Image & " mutants," & Completed'Image &
                " decoded," & Rejected'Image & " rejected");
   end Flip_Fixture;

   procedure Asset (Name : String; W, H : Positive) is
      Path : constant String := Assets & "/" & Name & ".qoi";
      Reference : constant String := References & "/" & Name & ".rgba";
   begin
      if Assets = "" or else not Ada.Directories.Exists (Path) or else
        not Ada.Directories.Exists (Reference)
      then
         Put_Line ("skip asset " & Name & " (not built)");
         return;
      end if;
      declare
         Data : constant Bytes_Access := Load (Path);
         RGBA : constant Bytes_Access := Load (Reference);
         Count : constant Positive := W * H;
         Output : constant Pixels_Access := new Pixel_Buffer (0 .. Count - 1);
         D : Decoder;
         Same : Boolean := True;
      begin
         Decode (Data.all, Count, 65_536, Output.all, D);
         Check (Current (D) = Complete and then Width (D) = W and then
                Height (D) = H, Name & " asset decodes");
         for I in 0 .. Count - 1 loop
            Same := Same and then Output (I) = Expected (RGBA.all, I);
         end loop;
         Check (Same, Name & " asset pixels equal the raster");
         Decode (Data.all, Count - 1, 0, Output.all, D);
         Check (Current (D) = Failed and then Error (D) = Over_Limit, Name & " over limit");
         Flip_Fixture (Path, Count, 40);
      end;
   end Asset;

   Manifest : Ada.Text_IO.File_Type;
begin
   Open (Manifest, In_File, Fixtures & "/manifest.txt");
   while not End_Of_File (Manifest) loop
      declare
         Line : constant String := Get_Line (Manifest);
         Fields : array (1 .. 6) of Unbounded_String;
         First : Positive := Line'First;
         Space : Natural;
      begin
         for F in Fields'Range loop
            Space := Ada.Strings.Fixed.Index (Line (First .. Line'Last), " ");
            if Space = 0 then Space := Line'Last + 1; end if;
            Fields (F) := To_Unbounded_String (Line (First .. Space - 1));
            First := Space + 1;
         end loop;
         declare
            File : constant String := To_String (Fields (1));
            Limit : constant Pixel_Limit := Pixel_Limit'Value (To_String (Fields (5)));
            Expect : constant String := To_String (Fields (6));
         begin
            if Expect = "Complete" then
               Valid_Fixture (File, To_String (Fields (2)),
                              Positive'Value (To_String (Fields (3))),
                              Positive'Value (To_String (Fields (4))));
               Flip_Fixture (Fixtures & "/" & File, Limit, 2_000);
            else
               Malformed_Fixture (File, Limit, Failure'Value (Expect));
            end if;
         end;
      end;
   end loop;
   Close (Manifest);
   Asset ("cubes", 2048, 576);
   Asset ("cubie", 2048, 1152);
   Put_Line ("QOI tests:" & Checks'Image & " checks," & Failures'Image & " failures");
   if Failures > 0 then
      Ada.Command_Line.Set_Exit_Status (Ada.Command_Line.Failure);
   end if;
end QOI_Tests;
