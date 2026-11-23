pragma Ada_2022;
package body Power_ACPI_Identity with SPARK_Mode is

   Last_Result : constant Word := Result_Code'Pos (Result_Code'Last);
   Maximum_Word : constant Word := Word (Integer_64'Last);
   --  A canonical name segment's characters.
   Segment_Characters : constant := 4;
   Byte_Bits : constant := 8;
   Byte_Mask : constant Word := 16#FF#;
   Separator : constant Character := '.';
   Padding : constant Character := '_';

   procedure Decode
     (Data : Words; Generation : out Word; Code : out Result_Code; Total : out Path_Length; Valid : out Boolean) is
   begin
      Generation := Data (0);
      Code := Not_Ready;
      Total := 0;
      Valid := Data (0) <= Maximum_Word and then Data (1) <= Last_Result;
      if not Valid then
         return;
      end if;
      Code := Result_Code'Val (Data (1));
      if Code = OK then
         Valid := Data (2) <= Maximum_Path_Characters;
         if Valid then
            Total := Path_Length (Data (2));
         end if;
      else
         --  A refused read carries no length and no characters.
         Valid := Data (2) = 0 and then Data (3) = 0;
      end if;
   end Decode;

   function Byte_Of (Chunk : Word; Position : Natural) return Word is
     (Shift_Right (Chunk, Byte_Bits * Position) and Byte_Mask)
   with Pre => Position < Chunk_Characters + 1;

   procedure Append
     (P : in out Path; Total : Path_Length; Chunk : Word; Valid : out Boolean)
   is
      Count : constant Positive := Natural'Min (Chunk_Characters, Total - P.Length);
      Next : Path := P;
   begin
      --  Past the path's end every byte is zero.
      Valid := (for all I in Count .. Chunk_Characters => Byte_Of (Chunk, I) = 0);
      if not Valid then
         return;
      end if;
      for I in 0 .. Count - 1 loop
         pragma Loop_Invariant (Next.Length = P.Length
                                and then (for all J in 1 .. P.Length + I => Path_Character (Next.Text (J))));
         declare
            C : constant Character := Character'Val (Byte_Of (Chunk, I));
         begin
            if not Path_Character (C) then
               Valid := False;
               return;
            end if;
            Next.Text (P.Length + I + 1) := C;
         end;
      end loop;
      Next := (Length => P.Length + Count, Text => Next.Text);
      P := Next;
   end Append;

   --  The segment that ends at Last (exclusive of its separator before),
   --  as First .. Last; First > Last when there is none.
   procedure Segment_Before (P : Path; Last : Natural; First : out Positive)
   with Pre  => Last <= P.Length,
        Post => First in 1 .. Last + 1
                and then (for all I in First .. Last => P.Text (I) not in Separator | '\')
   is
   begin
      First := Last + 1;
      while First > 1 and then P.Text (First - 1) not in Separator | '\' loop
         pragma Loop_Invariant (First in 2 .. Last + 1
                                and then (for all I in First .. Last => P.Text (I) not in Separator | '\'));
         pragma Loop_Variant (Decreases => First);
         First := First - 1;
      end loop;
   end Segment_Before;

   --  P.Text (First .. Last) without its padding, appended to Into.
   procedure Add_Segment (Into : in out PS.Source_Name; P : Path; First, Last : Natural; Valid : out Boolean)
   with Pre  => Last <= P.Length and then First >= 1
                and then (for all I in First .. Last => P.Text (I) not in Separator | '\'),
        Post => Into.Length <= Into.Length'Old + Segment_Characters
   is
      Stop : Natural := Last;
   begin
      Valid := False;
      if Last < First or else Last - First + 1 /= Segment_Characters then
         return;
      end if;
      while Stop >= First and then P.Text (Stop) = Padding loop
         pragma Loop_Invariant (Stop in First .. Last);
         pragma Loop_Variant (Decreases => Stop);
         Stop := Stop - 1;
      end loop;
      if Stop < First or else Into.Length + (Stop - First + 1) > PS.Name_Capacity then
         return;
      end if;
      for I in First .. Stop loop
         pragma Loop_Invariant (Into.Length = Into.Length'Loop_Entry + (I - First)
                                and then Into.Length + (Stop - I + 1) <= PS.Name_Capacity);
         pragma Assert (Path_Character (P.Text (I)));
         Into.Text (Into.Length + 1) := P.Text (I);
         Into.Length := Into.Length + 1;
      end loop;
      Valid := True;
   end Add_Segment;

   function Short_Name (P : Path) return PS.Source_Name is
      Result : PS.Source_Name;
      First : Positive;
      Valid : Boolean;
   begin
      if P.Length = 0 then
         return Result;
      end if;
      Segment_Before (P, P.Length, First);
      Add_Segment (Result, P, First, P.Length, Valid);
      return (if Valid then Result else (others => <>));
   end Short_Name;

   function Long_Name (P : Path) return PS.Source_Name is
      Result : PS.Source_Name;
      First, Before : Positive;
      Valid : Boolean;
   begin
      if P.Length = 0 then
         return Result;
      end if;
      Segment_Before (P, P.Length, First);
      if First < 3 or else P.Text (First - 1) /= Separator then
         return Result;
      end if;
      Segment_Before (P, First - 2, Before);
      Add_Segment (Result, P, Before, First - 2, Valid);
      if not Valid or else Result.Length = PS.Name_Capacity then
         return (others => <>);
      end if;
      Result.Text (Result.Length + 1) := Separator;
      Result.Length := Result.Length + 1;
      Add_Segment (Result, P, First, P.Length, Valid);
      return (if Valid then Result else (others => <>));
   end Long_Name;

   procedure Name_Sources
     (Sources : in out PS.Source_Array; Count : PS.Source_Count; Paths : Path_Array)
   is
      Old : constant PS.Source_Array := Sources with Ghost;
      type Name_Array is array (PS.Source_Index) of PS.Source_Name;
      Shorts, Longs : Name_Array := [others => (others => <>)];
   begin
      for I in 1 .. Count loop
         Shorts (I) := Short_Name (Paths (I));
         Longs (I) := Long_Name (Paths (I));
      end loop;
      for I in 1 .. Count loop
         pragma Loop_Invariant
           (for all K in PS.Source_Index =>
              Sources (K) = (Old (K) with delta Name => Sources (K).Name)
              and then (if K >= I or else Paths (K).Length = 0 then Sources (K).Name = Old (K).Name));
         if Paths (I).Length > 0 then
            if Shorts (I).Length > 0
              and then (for all J in 1 .. Count => J = I or else Shorts (J) /= Shorts (I))
            then
               Sources (I).Name := Shorts (I);
            elsif Longs (I).Length > 0
              and then (for all J in 1 .. Count => J = I or else Longs (J) /= Longs (I))
            then
               Sources (I).Name := Longs (I);
            end if;
         end if;
      end loop;
   end Name_Sources;
end Power_ACPI_Identity;
