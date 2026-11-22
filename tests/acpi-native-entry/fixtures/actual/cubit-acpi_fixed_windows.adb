pragma Ada_2022;

package body CuBit.ACPI_Fixed_Windows with SPARK_Mode is

   --  Item's readable length: its buffer, cut to its own Length field;
   --  zero when it is not the table Signature names.
   function Readable (Item : Byte_Array; Signature : String) return Natural
   with Pre  => Item'First = 1 and then Item'Length <= Maximum_Table_Bytes
                and then Signature'Length = Signature_Bytes,
        Post => Readable'Result <= Item'Length;

   --  The Bytes-byte little-endian field at Offset, if all of it is within
   --  the first Limit bytes; Present False otherwise.
   procedure Field
     (Item : Byte_Array; Limit, Offset, Bytes : Natural;
      Value : out Unsigned_64; Present : out Boolean)
   with Pre  => Item'First = 1 and then Limit <= Item'Length
                and then Item'Length <= Maximum_Table_Bytes
                and then Offset <= Maximum_Table_Bytes and then Bytes in 1 .. 8,
        Post => (if not Present then Value = 0);

   --  Where a generic address structure at Offset says a block starts:
   --  Absent (no structure, or address zero), In_IO, or Elsewhere
   --  (another address space: no I/O window).
   type GAS_Kind is (Absent, In_IO, Elsewhere);
   procedure GAS
     (Item : Byte_Array; Limit, Offset : Natural; Kind : out GAS_Kind; Address : out Unsigned_64)
   with Pre => Item'First = 1 and then Limit <= Item'Length
               and then Item'Length <= Maximum_Table_Bytes
               and then Offset <= Maximum_Table_Bytes - GAS_Bytes;

   function Readable (Item : Byte_Array; Signature : String) return Natural is
      Low : Natural;   --  the Length field's three low bytes
   begin
      if Item'Length < Header_Bytes then
         return 0;
      end if;
      for I in 0 .. Signature_Bytes - 1 loop
         if Item (1 + I) /= Character'Pos (Signature (Signature'First + I)) then
            return 0;
         end if;
      end loop;
      Low := Natural (Item (Length_Offset + 1)) + 2 ** 8 * Natural (Item (Length_Offset + 2))
             + 2 ** 16 * Natural (Item (Length_Offset + 3));
      --  A nonzero top byte states more than any buffer holds.
      if Item (Length_Offset + 4) /= 0 or else Low >= Item'Length then
         return Item'Length;
      elsif Low < Header_Bytes then
         return 0;
      end if;
      return Low;
   end Readable;

   procedure Field
     (Item : Byte_Array; Limit, Offset, Bytes : Natural;
      Value : out Unsigned_64; Present : out Boolean)
   is
   begin
      Value := 0;
      Present := Offset + Bytes <= Limit;
      if Present then
         for I in 0 .. Bytes - 1 loop
            Value := Value or Shift_Left (Unsigned_64 (Item (Offset + 1 + I)), 8 * I);
         end loop;
      end if;
   end Field;

   procedure GAS
     (Item : Byte_Array; Limit, Offset : Natural; Kind : out GAS_Kind; Address : out Unsigned_64)
   is
      Space : Unsigned_64;
      Have_Space, Have_Address : Boolean;
   begin
      Kind := Absent;
      Field (Item, Limit, Offset + GAS_Space_Offset, 1, Space, Have_Space);
      Field (Item, Limit, Offset + GAS_Address_Offset, 8, Address, Have_Address);
      if Have_Space and then Have_Address and then Address /= 0 then
         Kind := (if Space = System_IO_Space then In_IO else Elsewhere);
      end if;
   end GAS;

   procedure Compute (FADT, ECDT : Byte_Array; Result : out Window_List) is
      Blocks : Window_Array := [others => (others => <>)];
      Block_Total : Window_Count := 0;
      FADT_Limit : constant Natural := Readable (FADT, FADT_Signature);
      ECDT_Limit : constant Natural := Readable (ECDT, ECDT_Signature);

      --  Every recorded block lies inside the port space.
      function In_Space return Boolean is
        (for all K in 1 .. Block_Total => Last_Plus_One (Blocks (K)) <= Port_Space);

      --  Record [Address, Address + Length) if it is a nonempty range of
      --  the port space.
      procedure Add (Address, Length : Unsigned_64)
      with Pre  => Block_Total < Maximum_Windows and then In_Space,
           Post => Block_Total <= Block_Total'Old + 1 and then In_Space;

      --  One FADT block: its length field, and its 64-bit address when
      --  present and nonzero (in the I/O space, or no window), else its
      --  32-bit one.
      procedure FADT_Block (Legacy, X_Block, Length_At : Natural)
      with Pre  => Block_Total < Maximum_Windows and then In_Space
                   and then FADT'First = 1 and then FADT'Length <= Maximum_Table_Bytes
                   and then FADT_Limit <= FADT'Length
                   and then Legacy <= Maximum_Table_Bytes and then Length_At <= Maximum_Table_Bytes
                   and then X_Block <= Maximum_Table_Bytes - GAS_Bytes,
           Post => Block_Total <= Block_Total'Old + 1 and then In_Space;

      --  One EC register: a port, if the ECDT puts it in the I/O space.
      procedure EC_Register (At_Offset : Natural)
      with Pre  => Block_Total < Maximum_Windows and then In_Space
                   and then ECDT'First = 1 and then ECDT'Length <= Maximum_Table_Bytes
                   and then ECDT_Limit <= ECDT'Length
                   and then At_Offset <= Maximum_Table_Bytes - GAS_Bytes,
           Post => Block_Total <= Block_Total'Old + 1 and then In_Space;

      --  Exchange blocks J - 1 and J.
      procedure Swap (J : Window_Index)
      with Pre  => J in 2 .. Block_Total and then In_Space,
           Post => In_Space;

      procedure Swap (J : Window_Index) is
         Earlier : constant Port_Range := Blocks (J - 1);
      begin
         Blocks (J - 1) := Blocks (J);
         Blocks (J) := Earlier;
      end Swap;

      procedure Add (Address, Length : Unsigned_64) is
      begin
         if Address /= 0 and then Length /= 0
           and then Address < Port_Space and then Length <= Port_Space - Address
         then
            Block_Total := Block_Total + 1;
            Blocks (Block_Total) := (First => Port_Number (Address), Count => Port_Count (Length));
         end if;
      end Add;

      procedure FADT_Block (Legacy, X_Block, Length_At : Natural) is
         Length, Address : Unsigned_64;
         Have_Length, Have_Legacy : Boolean;
         Kind : GAS_Kind;
      begin
         Field (FADT, FADT_Limit, Length_At, 1, Length, Have_Length);
         GAS (FADT, FADT_Limit, X_Block, Kind, Address);
         if not Have_Length or else Kind = Elsewhere then
            return;
         elsif Kind = Absent then
            Field (FADT, FADT_Limit, Legacy, 4, Address, Have_Legacy);
            if not Have_Legacy then
               return;
            end if;
         end if;
         Add (Address, Length);
      end FADT_Block;

      procedure EC_Register (At_Offset : Natural) is
         Kind : GAS_Kind;
         Address : Unsigned_64;
      begin
         GAS (ECDT, ECDT_Limit, At_Offset, Kind, Address);
         if Kind = In_IO then
            Add (Address, EC_Register_Ports);
         end if;
      end EC_Register;

   begin
      Result := (others => <>);
      if FADT_Limit > 0 then
         FADT_Block (PM1a_Event_Offset, X_PM1a_Event_Offset, PM1_Event_Length_Offset);
         FADT_Block (PM1b_Event_Offset, X_PM1b_Event_Offset, PM1_Event_Length_Offset);
         FADT_Block (PM1a_Control_Offset, X_PM1a_Control_Offset, PM1_Control_Length_Offset);
         FADT_Block (PM1b_Control_Offset, X_PM1b_Control_Offset, PM1_Control_Length_Offset);
         FADT_Block (PM_Timer_Offset, X_PM_Timer_Offset, PM_Timer_Length_Offset);
         FADT_Block (GPE0_Offset, X_GPE0_Offset, GPE0_Length_Offset);
         FADT_Block (GPE1_Offset, X_GPE1_Offset, GPE1_Length_Offset);
      end if;
      if ECDT_Limit > 0 then
         EC_Register (EC_Control_Offset);
         EC_Register (EC_Data_Offset);
      end if;

      --  Ascending by first port (insertion by swaps; at most nine).
      for I in 2 .. Block_Total loop
         for J in reverse 2 .. I loop
            exit when Blocks (J - 1).First <= Blocks (J).First;
            Swap (J);
            pragma Loop_Invariant (In_Space);
         end loop;
         pragma Loop_Invariant (In_Space);
      end loop;

      --  Overlapping blocks become one window (their union).
      for I in 1 .. Block_Total loop
         if Result.Count > 0
           and then Unsigned_32 (Blocks (I).First) < Last_Plus_One (Result.Windows (Result.Count))
         then
            if Last_Plus_One (Blocks (I)) > Last_Plus_One (Result.Windows (Result.Count)) then
               Result.Windows (Result.Count).Count :=
                 Last_Plus_One (Blocks (I)) - Unsigned_32 (Result.Windows (Result.Count).First);
            end if;
         else
            Result.Count := Result.Count + 1;
            Result.Windows (Result.Count) := Blocks (I);
         end if;
         pragma Loop_Invariant (Result.Count <= I);
         pragma Loop_Invariant (Well_Formed (Result));
      end loop;
   end Compute;

end CuBit.ACPI_Fixed_Windows;
