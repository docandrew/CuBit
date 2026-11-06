with Ada.Text_IO; with Ada.Command_Line; with Ada.Streams.Stream_IO;
with Interfaces; use Interfaces;
with Firmware_Tables; with Firmware_Tables.MCFG; with Firmware_Tables.MCFG_Original;
procedure Equivalence_Tests is
   package F renames Firmware_Tables;
   package N renames F.MCFG;
   package O renames F.MCFG_Original;
   type Image is array (1 .. 6) of Unsigned_64;
   Checks, Cases, Files : Natural := 0;
   Random : Unsigned_64 := 16#D1B5_4A32_D192_ED03#;
   procedure Check (Good : Boolean) is
   begin Checks := Checks + 1; if not Good then raise Program_Error with Checks'Image; end if; end Check;
   function Normalize (V : N.Table_Metadata) return Image is
     (if V.Valid then [1,Unsigned_64(V.Revision),V.Reserved,Unsigned_64(V.Count),0,0] else [others=>0]);
   function Normalize (V : O.Table_Metadata) return Image is
     (if V.Valid then [1,Unsigned_64(V.Revision),V.Reserved,Unsigned_64(V.Count),0,0] else [others=>0]);
   function Normalize (V : N.Allocation_Result) return Image is
     (if V.Valid then [1,V.Value.Base,Unsigned_64(V.Value.Segment),Unsigned_64(V.Value.First_Bus),
       Unsigned_64(V.Value.Last_Bus),Unsigned_64(V.Value.Reserved)] else [others=>0]);
   function Normalize (V : O.Allocation_Result) return Image is
     (if V.Valid then [1,V.Value.Base,Unsigned_64(V.Value.Segment),Unsigned_64(V.Value.First_Bus),
       Unsigned_64(V.Value.Last_Bus),Unsigned_64(V.Value.Reserved)] else [others=>0]);
   procedure Compare (Data : F.Bytes; Expected_Valid : Boolean) is
      Before : constant O.Table_Metadata := O.Decode(Data);
      After : constant N.Table_Metadata := N.Decode(Data);
   begin
      Cases := Cases + 1;
      Check(Before.Valid=Expected_Valid and After.Valid=Expected_Valid);
      Check(Normalize(Before)=Normalize(After));
      for Index in 0 .. (if Before.Valid then Before.Count+1 else 1) loop
         Check(Normalize(O.Read_Allocation(Data,Index))=Normalize(N.Read_Allocation(Data,Index)));
      end loop;
      Check(Normalize(O.Read_Allocation(Data,Natural'Last))=Normalize(N.Read_Allocation(Data,Natural'Last)));
   end Compare;
   procedure Next is
   begin
      Random:=Random xor Shift_Left(Random,13);
      Random:=Random xor Shift_Right(Random,7);
      Random:=Random xor Shift_Left(Random,17);
   end Next;
   procedure Put (Data : in out F.Bytes; Offset : Natural; Value : Unsigned_64; Width : Positive) is
   begin
      for I in 0..Width-1 loop Data(Data'First+Offset+I):=F.Byte(Shift_Right(Value,8*I) and 255);end loop;
   end Put;
   procedure Seal (Data : in out F.Bytes) is
      Sum : F.Byte:=0;
   begin
      Data(Data'First+9):=0;for B of Data loop Sum:=Sum+B;end loop;
      Data(Data'First+9):=0-Sum;
   end Seal;
   procedure Fill (Data : in out F.Bytes; Count : Natural; Segment : Unsigned_16) is
   begin
      Data:=[others=>0];
      Data(Data'First..Data'First+3):=[Character'Pos('M'),Character'Pos('C'),Character'Pos('F'),Character'Pos('G')];
      Put(Data,4,Unsigned_64(Data'Length),4);Put(Data,8,Random and 255,1);
      Put(Data,F.Table_Header_Size,Random,8);
      for I in 1..Count loop
         declare Offset:constant Natural:=N.Fixed_Size+(I-1)*N.Allocation_Size;begin
            Next;Put(Data,Offset,Random,8);Put(Data,Offset+8,Unsigned_64(Segment),2);
            Put(Data,Offset+10,Random and 255,1);Put(Data,Offset+11,Shift_Right(Random,8) and 255,1);
            Next;Put(Data,Offset+12,Random and 16#FFFF_FFFF#,4);
         end;
      end loop;
      Seal(Data);
   end Fill;
   Extent : constant Positive:=N.Fixed_Size+N.Allocation_Size;
begin
   -- Every possible 16-bit segment value, with alternating low/high origins.
   for Segment in Unsigned_16 loop
      declare
         First:constant Positive:=(if Segment mod 2=0 then 1 else Positive'Last-(Extent-1));
         Data:F.Bytes(First..First+(Extent-1));
      begin Fill(Data,1,Segment);Compare(Data,True);end;
   end loop;
   -- Empty through four-allocation tables and full-width deterministic words.
   for Sample in 0..4095 loop
      declare
         Count:constant Natural:=Sample mod 5;
         Length:constant Positive:=N.Fixed_Size+Count*N.Allocation_Size;
         First:constant Positive:=(if Sample mod 3=0 then Positive'Last-(Length-1) else 17);
         Data:F.Bytes(First..First+(Length-1));
      begin Fill(Data,Count,Unsigned_16(Sample));Compare(Data,True);end;
   end loop;
   declare Data:F.Bytes(1..Extent);begin
      Fill(Data,1,16#ABCD#);
      for Length in 0..Extent-1 loop Compare(Data(1..Length),False);end loop;
      declare Extra:F.Bytes(1..Extent+1):=[others=>0];begin Extra(1..Extent):=Data;Compare(Extra,False);end;
      Data(10):=Data(10) xor 1;Compare(Data,False);Seal(Data);
      Data(1):=Character'Pos('X');Seal(Data);Compare(Data,False);Data(1):=Character'Pos('M');Seal(Data);
      declare Partial:F.Bytes:=Data(1..Extent-1);begin Put(Partial,4,Unsigned_64(Partial'Length),4);Seal(Partial);Compare(Partial,False);end;
      Put(Data,4,16#FFFF_FFFF#,4);Seal(Data);Compare(Data,False);
   end;
   for Argument in 1..Ada.Command_Line.Argument_Count loop
      declare
         package IO renames Ada.Streams.Stream_IO;
         use type IO.Count,Ada.Streams.Stream_Element_Offset;
         File:IO.File_Type;
      begin
         IO.Open(File,IO.In_File,Ada.Command_Line.Argument(Argument));
         if IO.Size(File)>IO.Count(Natural'Last) then raise Program_Error with "table exceeds Ada array extent";end if;
         declare
            Length:constant Natural:=Natural(IO.Size(File));
            Wire:Ada.Streams.Stream_Element_Array(1..Ada.Streams.Stream_Element_Offset(Length));
            Last:Ada.Streams.Stream_Element_Offset;Data:F.Bytes(1..Length);
         begin
            IO.Read(File,Wire,Last);Check(Last=Wire'Last);IO.Close(File);
            for I in Data'Range loop Data(I):=F.Byte(Wire(Ada.Streams.Stream_Element_Offset(I)));end loop;
            Compare(Data,True);Files:=Files+1;
            declare Value:constant N.Allocation_Result:=N.Read_Allocation(Data,1);begin
               Check(Value.Valid and then Value.Value.Base=16#B000_0000# and then Value.Value.Segment=0
                 and then Value.Value.First_Bus=0 and then Value.Value.Last_Bus=255);
            end;
         end;
      end;
   end loop;
   Ada.Text_IO.Put_Line("MCFG equivalence checks"&Checks'Image&" cases"&Cases'Image&" files"&Files'Image);
end Equivalence_Tests;
