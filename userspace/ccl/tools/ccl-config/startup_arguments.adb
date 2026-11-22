------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
------------------------------------------------------------------------------
pragma Ada_2022;
with Ada.Directories;
with Ada.Streams.Stream_IO;
with Ada.Text_IO;
with Interfaces;
with CCL.Typed_Values;
with CuBit.Parameter_Signatures;
with SPARKNaCl;
with SPARKTLSCrypto.Hashing.SHA256;

package body Startup_Arguments is
   use Ada.Streams;
   use type Interfaces.Unsigned_8;
   use type Interfaces.Unsigned_64;
   package PS renames CuBit.Parameter_Signatures;
   use type PS.Validation, PS.Identity;

   --  ELF64: the section header table's offset, entry size and count, and
   --  in each entry its type, offset and size.
   ELF_Header_Bytes : constant := 64;
   Section_Table_At : constant := 40;
   Section_Entry_Size_At : constant := 58;
   Section_Count_At : constant := 60;
   Section_Entry_Bytes : constant := 64;
   Section_Type_At : constant := 4;
   Section_Offset_At : constant := 24;
   Section_Size_At : constant := 32;
   SHT_PROGBITS : constant := 1;

   type Outcome is (No_Descriptor, Found, Too_Long);
   subtype Unsigned_64_Index is Interfaces.Unsigned_64;

   --  The program's .cubit.parameters: the PROGBITS section that starts
   --  with the descriptor's magic.
   procedure Read_Descriptor
     (Path : String; Item : out PS.Bytes; Length : out Natural; Result : out Outcome)
   is
      File : Stream_IO.File_Type;
      function Read (At_Offset : Unsigned_64_Index; Count : Positive) return Stream_Element_Array;
      function Read (At_Offset : Unsigned_64_Index; Count : Positive) return Stream_Element_Array is
         Data : Stream_Element_Array (1 .. Stream_Element_Offset (Count)) := [others => 0];
         Last : Stream_Element_Offset;
      begin
         Stream_IO.Set_Index (File, Stream_IO.Positive_Count (At_Offset + 1));
         Stream_IO.Read (File, Data, Last);
         return Data;
      end Read;
      function Value (Data : Stream_Element_Array; At_Offset, Bytes : Natural) return Interfaces.Unsigned_64 is
         Result : Interfaces.Unsigned_64 := 0;
      begin
         for K in reverse 1 .. Bytes loop
            Result := Interfaces.Shift_Left (Result, 8) or
              Interfaces.Unsigned_64 (Data (Data'First + Stream_Element_Offset (At_Offset + K - 1)));
         end loop;
         return Result;
      end Value;
      Size : Interfaces.Unsigned_64;
   begin
      Item := [others => 0];
      Length := 0;
      Result := No_Descriptor;
      Stream_IO.Open (File, Stream_IO.In_File, Path);
      Size := Interfaces.Unsigned_64 (Stream_IO.Size (File));
      if Size < ELF_Header_Bytes then
         Stream_IO.Close (File);
         return;
      end if;
      declare
         Header : constant Stream_Element_Array := Read (0, ELF_Header_Bytes);
         Table : constant Interfaces.Unsigned_64 := Value (Header, Section_Table_At, 8);
         Entry_Size : constant Interfaces.Unsigned_64 := Value (Header, Section_Entry_Size_At, 2);
         Count : constant Interfaces.Unsigned_64 := Value (Header, Section_Count_At, 2);
      begin
         if Entry_Size /= Section_Entry_Bytes or else Table = 0 or else Table > Size
           or else Count > (Size - Table) / Section_Entry_Bytes
         then
            Stream_IO.Close (File);
            return;
         end if;
         for I in 0 .. Count - 1 loop
            declare
               Section : constant Stream_Element_Array :=
                 Read (Unsigned_64_Index (Table + I * Section_Entry_Bytes), Section_Entry_Bytes);
               Offset : constant Interfaces.Unsigned_64 := Value (Section, Section_Offset_At, 8);
               Bytes : constant Interfaces.Unsigned_64 := Value (Section, Section_Size_At, 8);
            begin
               if Value (Section, Section_Type_At, 4) = SHT_PROGBITS
                 and then Bytes >= PS.Magic_Bytes and then Offset <= Size and then Bytes <= Size - Offset
               then
                  declare
                     Magic : constant Stream_Element_Array := Read (Unsigned_64_Index (Offset), PS.Magic_Bytes);
                  begin
                     if Interfaces.Unsigned_8 (Magic (1)) = PS.Magic_0
                       and then Interfaces.Unsigned_8 (Magic (2)) = PS.Magic_1
                       and then Interfaces.Unsigned_8 (Magic (3)) = PS.Magic_2
                       and then Interfaces.Unsigned_8 (Magic (4)) = PS.Magic_3
                     then
                        if Bytes > PS.Maximum_Descriptor_Bytes then
                           Result := Too_Long;
                        else
                           declare
                              Data : constant Stream_Element_Array :=
                                Read (Unsigned_64_Index (Offset), Positive (Bytes));
                           begin
                              Length := Natural (Bytes);
                              for K in 1 .. Length loop
                                 Item (K) := Interfaces.Unsigned_8 (Data (Stream_Element_Offset (K)));
                              end loop;
                              Result := Found;
                           end;
                        end if;
                        Stream_IO.Close (File);
                        return;
                     end if;
                  end;
               end if;
            end;
         end loop;
      end;
      Stream_IO.Close (File);
   end Read_Descriptor;

   function SHA256 (Text : String) return PS.Identity is
      use type SPARKNaCl.N32;
      Message : SPARKNaCl.Byte_Seq (0 .. SPARKNaCl.N32 (Text'Length) - 1);
      Digest : SPARKTLSCrypto.Hashing.SHA256.Digest;
      Result : PS.Identity := [others => 0];
   begin
      for K in Text'Range loop
         Message (SPARKNaCl.N32 (K - Text'First)) := Character'Pos (Text (K));
      end loop;
      SPARKTLSCrypto.Hashing.SHA256.Hash (Digest, Message);
      for I in Result'Range loop
         Result (I) := Interfaces.Unsigned_8 (Digest (SPARKNaCl.Index_32 (I - 1)));
      end loop;
      return Result;
   end SHA256;

   procedure Check
     (Profile_Name, Source : String; Plan : CCL.Configurations.Configuration_Plan;
      Directory : String; Success : out Boolean)
   is
      function Line_Column (Offset : Natural) return String is
         Line, Column : Positive := 1;
      begin
         for I in Source'First .. Source'First + Natural'Min (Offset, Source'Length + 1) - 2 loop
            if Source (I) = ASCII.LF then Line := Line + 1; Column := 1; else Column := Column + 1; end if;
         end loop;
         return "line" & Positive'Image (Line) & ", column" & Positive'Image (Column);
      end Line_Column;
      --  An entry's position, past the blanks before its executable name.
      function Name_At (Offset : Natural) return Natural is
         At_Name : Natural := Offset;
      begin
         while At_Name in 1 .. Source'Length and then Source (Source'First + At_Name - 1) in ' ' | ASCII.LF | ASCII.HT loop
            At_Name := At_Name + 1;
         end loop;
         return At_Name;
      end Name_At;
      procedure Refuse (At_Offset : Natural; Text : String) is
      begin
         Ada.Text_IO.Put_Line (Ada.Text_IO.Standard_Error, Profile_Name & ": " & Line_Column (At_Offset) & ": " & Text);
         Success := False;
      end Refuse;
   begin
      Success := True;
      for Item of Plan.Launches (1 .. Plan.Launch_Count) loop
         declare
            Name : constant String := Item.Executable.Data (1 .. Item.Executable.Length);
            Path : constant String := Directory & "/" & Name;
            Given : constant String := Item.Arguments.Data (1 .. Item.Arguments.Length);
            Descriptor : PS.Bytes (1 .. PS.Maximum_Descriptor_Bytes);
            Length : Natural := 0;
            Read : Outcome := No_Descriptor;
         begin
            if not Ada.Directories.Exists (Path) then
               if Given'Length > 0 then
                  Refuse (Item.Arguments_At, Name & " has arguments, but " & Path &
                          " is not there to check them against");
               end if;
            else
               Read_Descriptor (Path, Descriptor, Length, Read);
               case Read is
                  when Too_Long =>
                     Refuse (Name_At (Item.Entry_At), Name & ": its .cubit.parameters is longer than any descriptor");
                  when No_Descriptor =>
                     if Given'Length > 0 then
                        Refuse (Item.Arguments_At, Name & " declares no parameters, so it takes no arguments");
                     end if;
                  when Found =>
                     if PS.Validate (Descriptor (1 .. Length)) /= PS.Valid then
                        Refuse (Name_At (Item.Entry_At), Name & ": its .cubit.parameters is malformed");
                     elsif SHA256 (PS.Signature_Of (Descriptor (1 .. Length))) /=
                       PS.Identity_Of (Descriptor (1 .. Length))
                     then
                        Refuse (Name_At (Item.Entry_At), Name & ": its .cubit.parameters identity is not the SHA-256 " &
                                "of its signature");
                     elsif Given'Length = 0 then
                        Refuse (Name_At (Item.Entry_At), Name & " requires its parameters, a " &
                                PS.Root_Of (Descriptor (1 .. Length)) & "; give (arguments (" &
                                PS.Root_Of (Descriptor (1 .. Length)) & " field => value ...))");
                     else
                        declare
                           Checked : CCL.Typed_Values.Check_Result;
                        begin
                           CCL.Typed_Values.Check
                             (PS.Signature_Of (Descriptor (1 .. Length)), PS.Root_Of (Descriptor (1 .. Length)),
                              Given, Checked);
                           if not Checked.Success then
                              Refuse (Item.Arguments_At + (if Checked.Position > 0 then Checked.Position - 1 else 0),
                                      Name & ": " & Checked.Message (1 .. Checked.Message_Length));
                           end if;
                        end;
                     end if;
               end case;
            end if;
         end;
      end loop;
   end Check;
end Startup_Arguments;
