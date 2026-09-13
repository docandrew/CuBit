pragma Ada_2022;
package body User_Buffer_Copy with SPARK_Mode is
   procedure Copy (Source, Length : Unsigned_64; Success : out Boolean) is
      Offset : Unsigned_64 := 0;
   begin
      Success := False;
      if not Valid_Range (Source, Length) then return; end if;
      while Offset < Length loop
         pragma Loop_Invariant (Offset <= Length);
         pragma Loop_Variant (Decreases => Length - Offset);
         declare
            Address : constant Unsigned_64 := Source + Offset;
            Within_Page : constant Unsigned_64 := Address mod Page_Size;
            Count : constant Unsigned_64 := Unsigned_64'Min
              (Page_Size - Within_Page, Length - Offset);
            OK : Boolean;
         begin
            Read_Chunk (Address - Within_Page, Natural (Within_Page), Offset,
                        Positive (Count), OK);
            if not OK then return; end if;
            Offset := Offset + Count;
         end;
      end loop;
      Success := True;
   end Copy;

   procedure Copy_Name (Source : Unsigned_64; Name : out String; Success : out Boolean) is
      Address : Unsigned_64 := Source;
      Value : Character;
      OK : Boolean;
   begin
      Name := [others => ASCII.NUL];
      Success := False;
      if Name'Length = 0 then
         Success := True;
         return;
      end if;
      if not Valid_Range (Source, 1) then return; end if;
      for I in Name'Range loop
         pragma Loop_Invariant (Address >= Source);
         if Address >= User_Limit then return; end if;
         Read_Byte (Address, Value, OK);
         if not OK then return; end if;
         Name (I) := Value;
         exit when Value = ASCII.NUL;
         Address := Address + 1;
      end loop;
      Success := True;
   end Copy_Name;
end User_Buffer_Copy;
