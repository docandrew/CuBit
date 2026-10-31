pragma Ada_2022;
with Ada.Text_IO;
with Ada.Command_Line;
with Ada.Streams;
with Ada.Streams.Stream_IO;
with AML_Copied_Tables;
with Firmware_Tables;
with Interfaces;
procedure Store_Tests is
   package C renames AML_Copied_Tables;
   package F renames Firmware_Tables;
   package IO renames Ada.Streams.Stream_IO;
   use type F.Byte, F.Bytes, F.Admission, C.Outcome, C.Table_ID;
   use type Interfaces.Unsigned_32, Ada.Streams.Stream_Element_Offset;
   Checks : Natural := 0;
   Fuel : constant := 100_000;
   Fixture_Limit : constant := 4096;
   procedure Check (Condition : Boolean; Why : String) is
   begin
      Checks := Checks + 1;
      if not Condition then raise Program_Error with Why; end if;
   end Check;
   function Read_Table return F.Bytes is
      File : IO.File_Type;
   begin
      Check (Ada.Command_Line.Argument_Count = 1, "fixture path");
      IO.Open (File, IO.In_File, Ada.Command_Line.Argument (1));
      Check (IO.Size (File) in IO.Count (F.Table_Header_Size) .. Fixture_Limit, "bounded fixture");
      declare
         Raw : Ada.Streams.Stream_Element_Array (1 .. Ada.Streams.Stream_Element_Offset (IO.Size (File)));
         Last : Ada.Streams.Stream_Element_Offset;
         Data : F.Bytes (1 .. Raw'Length);
      begin
         IO.Read (File, Raw, Last); IO.Close (File);
         Check (Last = Raw'Last, "complete fixture");
         for I in Data'Range loop Data (I) := F.Byte (Raw (Ada.Streams.Stream_Element_Offset (I))); end loop;
         return Data;
      end;
   end Read_Table;
   Original : constant F.Bytes := Read_Table;
   Image : F.Bytes (Original'Range) := Original;
   Changed : F.Bytes (Original'Range) := Original;
   Store : C.State (2, 2 * Original'Length);
   Result : C.Report;
   ID : C.Table_ID;
   procedure Repair (Data : in out F.Bytes) is
      Sum : F.Byte := 0;
      Checksum_Offset : constant := 9;
   begin
      Data (Data'First + Checksum_Offset) := 0;
      for B of Data loop Sum := Sum + B; end loop;
      Data (Data'First + Checksum_Offset) := 0 - Sum;
   end Repair;
   procedure Same (S : C.State; Table : C.Table_ID; Data : F.Bytes) is
   begin
      Check (C.Present (S, Table) and then C.Length (S, Table) = Data'Length, "stable extent");
      for I in Data'Range loop
         Check (C.Byte_At (S, Table, I - Data'First) = Data (I), "exact immutable retained byte");
      end loop;
   end Same;
begin
   Check (C.Valid (Store) and then C.Count (Store) = 0 and then C.Used_Bytes (Store) = 0,
     "default empty valid store");
   C.Retain (Store, Image, Fuel, Result); ID := Result.ID;
   Check (Result.Status = C.Added and then ID = 1 and then Result.Admission = F.Accepted,
     "admit checked SDT and issue first local ID");
   Check (Result.Extent = Image'Length and then Result.Charged = F.Table_Header_Size + 2 * Image'Length,
     "charge header checksum and complete copy");
   Image := [others => 16#FF#]; Same (Store, ID, Original);
   C.Retain (Store, Original, Fuel, Result);
   Check (Result.Status = C.Already_Retained and then Result.ID = ID
     and then C.Count (Store) = 1 and then C.Used_Bytes (Store) = Original'Length,
     "full-byte duplicate consumes no storage or new identity");
   Changed (Changed'First + 24) := Changed (Changed'First + 24) xor 1; Repair (Changed);
   C.Retain (Store, Changed, Fuel, Result);
   Check (Result.Status = C.Added and then Result.ID = 2 and then C.Count (Store) = 2,
     "different valid bytes get independent copied table");
   Same (Store, ID, Original); Same (Store, 2, Changed);
   C.Retain (Store, Original, Fuel, Result);
   Check (Result.Status = C.Already_Retained and then Result.ID = ID,
     "duplicate still works at table and byte capacity");
   declare
      Third : F.Bytes := Changed;
   begin
      Third (Third'First + 24) := Third (Third'First + 24) xor 2; Repair (Third);
      C.Retain (Store, Third, Fuel, Result);
      Check (Result.Status = C.Table_Limit and then Result.ID = C.No_Table
        and then C.Count (Store) = 2, "full store refuses a new table without publication");
   end;
   Same (Store, ID, Original); Same (Store, 2, Changed);
   declare
      Shifted : constant F.Bytes (11 .. 10 + Original'Length) := Original;
      Trailing : F.Bytes (1 .. Original'Length + 3) := [others => 16#A5#];
      Exact : C.State (1, Original'Length);
   begin
      Trailing (1 .. Original'Length) := Original;
      C.Retain (Exact, Trailing, Fuel, Result);
      Check (Result.Status = C.Added and then C.Used_Bytes (Exact) = Original'Length,
        "ignore trailing source bytes and copy declared extent exactly");
      C.Retain (Exact, Shifted, Fuel, Result);
      Check (Result.Status = C.Already_Retained, "non-one source lower bound preserves matching");
      Same (Exact, 1, Original);
   end;
   declare
      Disabled : C.State (0, 0);
      Small : C.State (2, Original'Length - 1);
   begin
      C.Retain (Disabled, Original, Fuel, Result);
      Check (C.Valid (Disabled) and then Result.Status = C.Table_Limit
        and then C.Count (Disabled) = 0, "zero quotas are explicit disabled storage");
      C.Retain (Small, Original, Fuel, Result);
      Check (Result.Status = C.Byte_Limit and then Result.ID = C.No_Table
        and then C.Count (Small) = 0 and then C.Used_Bytes (Small) = 0,
        "byte quota refusal preserves empty state");
   end;
   for Limit in 0 .. F.Table_Header_Size + 2 * Original'Length - 1 loop
      declare S : C.State (1, Original'Length); begin
         C.Retain (S, Original, Limit, Result);
         Check (Result.Status = C.Budget_Exceeded and then Result.Charged = Limit
           and then Result.ID = C.No_Table and then C.Count (S) = 0
           and then C.Used_Bytes (S) = 0 and then C.Valid (S),
           "every insufficient admission/copy fuel preserves unpublished state");
      end;
   end loop;
   declare
      Invalid : F.Bytes := Original;
      S : C.State (2, 2 * Original'Length);
   begin
      Invalid (Invalid'Last) := Invalid (Invalid'Last) xor 1;
      C.Retain (S, Invalid, Fuel, Result);
      Check (Result.Status = C.Invalid_Table and then Result.Admission = F.Bad_Checksum
        and then C.Count (S) = 0, "checksum failure never publishes");
      Invalid := Original; Invalid (5 .. 8) := [others => 16#FF#];
      C.Retain (S, Invalid, Fuel, Result);
      Check (Result.Status = C.Invalid_Table and then Result.Raw_Length = Interfaces.Unsigned_32'Last
        and then Result.Extent = 0 and then C.Count (S) = 0,
        "preserve wide rejected length without narrowing or allocation");
      Invalid := Original; Invalid (5 .. 8) := [others => 0];
      C.Retain (S, Invalid, Fuel, Result);
      Check (Result.Status = C.Invalid_Table and then Result.Admission = F.Invalid_Length,
        "reject undersized encoded extent");
      Invalid := Original; Invalid (1 .. 4) := [Character'Pos ('F'), Character'Pos ('A'),
        Character'Pos ('C'), Character'Pos ('S')]; Repair (Invalid);
      C.Retain (S, Invalid, Fuel, Result);
      Check (Result.Status = C.Non_SDT and then C.Count (S) = 0, "FACS cannot become copied SDT");
      for Size in 0 .. F.Table_Header_Size - 1 loop
         C.Retain (S, Original (1 .. Size), Fuel, Result);
         Check (Result.Status = C.Invalid_Table and then Result.Charged = 0
           and then C.Count (S) = 0, "short header remains unexamined and unpublished");
      end loop;
   end;
   declare
      Window : F.Bytes (31 .. 38);
      Copied : Boolean;
      Empty : F.Bytes (1 .. 0);
   begin
      C.Copy_Window (Store, ID, Original'Length - Window'Length, Window, Copied);
      Check (Copied and then Window = Original (Original'Last - Window'Length + 1 .. Original'Last),
        "exact final window with non-one output lower bound");
      Window := [others => 16#FF#];
      C.Copy_Window (Store, ID, Original'Length - Window'Length + 1, Window, Copied);
      Check (not Copied and then (for all B of Window => B = 0), "refused window is fully zeroed");
      C.Copy_Window (Store, ID, Original'Length, Empty, Copied);
      Check (Copied, "empty exact-end window is valid");
      C.Copy_Window (Store, C.No_Table, 0, Window, Copied);
      Check (not Copied and then (for all B of Window => B = 0), "invalid ID never leaks old output");
      C.Copy_Window (Store, ID, Natural'Last, Window, Copied);
      Check (not Copied, "maximum invalid offset is refused without arithmetic overflow");
   end;
   Same (Store, ID, Original); Same (Store, 2, Changed);
   declare
      One_Mebibyte : constant := 1_024 * 1_024;
      Large_Extent : constant := One_Mebibyte + 1;
      Large_Fuel : constant := F.Table_Header_Size + 2 * Large_Extent + 1;
      Header_Length_Offset : constant := 4;
      Encoded_Length_Bytes : constant := 4;
      Bits_Per_Byte : constant := 8;
      Large : F.Bytes (1 .. Large_Extent) := [others => 0];
      Owned : C.State (1, Large_Extent);
      Window : F.Bytes (1 .. 8);
      Copied : Boolean;
   begin
      Large (Original'Range) := Original;
      for I in 0 .. Encoded_Length_Bytes - 1 loop
         Large (Large'First + Header_Length_Offset + I) := F.Byte
           (Interfaces.Shift_Right (Interfaces.Unsigned_32 (Large_Extent), Bits_Per_Byte * I) and 16#FF#);
      end loop;
      Repair (Large);
      C.Retain (Owned, Large, Large_Fuel, Result);
      Check (Result.Status = C.Added and then Result.Extent = Large_Extent
        and then C.Used_Bytes (Owned) = Large_Extent, "table larger than one MiB uses explicit capacity");
      Check (C.Byte_At (Owned, 1, 0) = Large (1)
        and then C.Byte_At (Owned, 1, Large_Extent - 1) = 0,
        "large exact extent preserves first and last bytes");
      Large (Large'Last) := 16#FF#;
      Check (C.Byte_At (Owned, 1, Large_Extent - 1) = 0,
        "large copied table survives source mutation");
      C.Copy_Window (Owned, 1, Large_Extent - Window'Length, Window, Copied);
      Check (Copied and then (for all B of Window => B = 0), "large final window has no truncation");
      Large (Large'Last) := 0;
      C.Retain (Owned, Large, Large_Fuel, Result);
      Check (Result.Status = C.Already_Retained and then Result.ID = 1
        and then C.Used_Bytes (Owned) = Large_Extent, "large duplicate reuses exact owned extent at capacity");
      C.Copy_Window (Owned, C.Table_ID'Last, 0, Window, Copied);
      Check (not Copied and then (for all B of Window => B = 0), "maximum invalid ID remains bounded");
   end;
   Ada.Text_IO.Put_Line ("Copied table checks" & Checks'Image);
end Store_Tests;
