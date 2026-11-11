pragma Ada_2022;

package body Devmgr_Platform_Pool with SPARK_Mode is

   function Decode_MCHBAR (Low, High : Unsigned_32) return Block is
      Word : constant Unsigned_64 := Shift_Left (Unsigned_64 (High), 32) or Unsigned_64 (Low);
      Base : constant Unsigned_64 := Word and MCHBAR_Base_Mask;
   begin
      if (Word and MCHBAR_Enable) = 0 then
         return (Status => Disabled, Base => 0, Bytes => 0);
      elsif Base = 0 then
         return (Status => Not_Placed, Base => 0, Bytes => 0);
      elsif Base >= Byte_Limit or else MCHBAR_Bytes > Byte_Limit - Base then
         return (Status => Beyond_Limit, Base => Base, Bytes => MCHBAR_Bytes);
      end if;
      pragma Assert (Base mod MCHBAR_Bytes = 0);
      return (Status => Found, Base => Base, Bytes => MCHBAR_Bytes);
   end Decode_MCHBAR;

   function Test_Block (BAR_Base, BAR_Bytes : Unsigned_64) return Block is
      Candidate : constant Block := (Status => Found, Base => BAR_Base, Bytes => MCHBAR_Bytes);
   begin
      if BAR_Bytes = Edu_BAR0_Bytes and then BAR_Base mod Page_Bytes = 0
        and then BAR_Base < Byte_Limit and then Edu_BAR0_Bytes <= Byte_Limit - BAR_Base
      then
         return Candidate;
      end if;
      return (Status => Not_Placed, Base => 0, Bytes => 0);
   end Test_Block;

   function Little (Bytes : Byte_Array; From, Count : Natural) return Unsigned_64 is
      Value : Unsigned_64 := 0;
   begin
      for K in 0 .. Count - 1 loop
         Value := Value or Shift_Left (Unsigned_64 (Bytes (From + K)), 8 * K);
      end loop;
      return Value;
   end Little;

   --  A little-endian 32-bit length at From, in integers: its value when
   --  below 2 ** 24, else 2 ** 24 (longer than any table read here).
   Large_Length : constant := 2 ** 24;
   function Length_At (Table : Byte_Array; From : Natural) return Natural
   is (if Table (From + 3) /= 0 then Large_Length
       else Natural (Table (From)) + 2 ** 8 * Natural (Table (From + 1))
            + 2 ** 16 * Natural (Table (From + 2)))
   with Pre  => Table'Length > 0 and then From >= Table'First
                and then From <= Table'Last and then 3 <= Table'Last - From,
        Post => Length_At'Result <= Large_Length;

   function LPIT_Memory_Counter (Table : Byte_Array) return Unsigned_64 is
      At_Byte : Natural := LPIT_Header_Bytes;
      Table_End : Natural;
   begin
      if Table'Length < LPIT_Header_Bytes then
         return 0;
      end if;
      --  The table's own length bounds the walk (never past what was read).
      Table_End := Natural'Min (Length_At (Table, LPIT_Table_Length_Offset), Table'Length);
      while At_Byte <= Table_End and then LPIT_Native_Bytes <= Table_End - At_Byte loop
         pragma Loop_Variant (Increases => At_Byte);
         pragma Loop_Invariant (At_Byte >= LPIT_Header_Bytes and then Table_End <= Table'Length);
         declare
            Kind   : constant Unsigned_64 := Little (Table, At_Byte + LPIT_Type_Offset, 4);
            Length : constant Natural := Length_At (Table, At_Byte + LPIT_Length_Offset);
            Flags  : constant Unsigned_64 := Little (Table, At_Byte + LPIT_Flags_Offset, 4);
            Space  : constant Unsigned_8 := Table (At_Byte + LPIT_Counter_Offset);
            Address : constant Unsigned_64 := Little (Table, At_Byte + LPIT_Counter_Address, 8);
         begin
            if Length < LPIT_Native_Bytes then
               return 0;
            end if;
            if Kind = LPIT_Native_C_State and then Flags = 0
              and then Space = GAS_System_Memory and then Address /= 0
            then
               return Address;
            end if;
            exit when Length > Table_End - At_Byte;
            At_Byte := At_Byte + Length;
         end;
      end loop;
      return 0;
   end LPIT_Memory_Counter;

   function PWRM_Block (Counter : Unsigned_64) return Block is
   begin
      if Counter = 0 then
         return (Status => No_Source, Base => 0, Bytes => 0);
      elsif Counter < PMC_SLP_S0_Counter_Offset
        or else (Counter - PMC_SLP_S0_Counter_Offset) mod PWRM_Bytes /= 0
      then
         return (Status => Not_Placed, Base => 0, Bytes => 0);
      elsif Counter - PMC_SLP_S0_Counter_Offset >= Byte_Limit
        or else PWRM_Bytes > Byte_Limit - (Counter - PMC_SLP_S0_Counter_Offset)
      then
         return (Status => Beyond_Limit, Base => Counter - PMC_SLP_S0_Counter_Offset,
                 Bytes => PWRM_Bytes);
      end if;
      return (Status => Found, Base => Counter - PMC_SLP_S0_Counter_Offset, Bytes => PWRM_Bytes);
   end PWRM_Block;

   function Discover
     (Kind : Bridge_Kind; MCHBAR_Low, MCHBAR_High : Unsigned_32;
      Test_BAR_Base, Test_BAR_Bytes : Unsigned_64) return Block is
   begin
      case Kind is
         when Alder_Lake_N  => return Decode_MCHBAR (MCHBAR_Low, MCHBAR_High);
         when QEMU_Q35_Test => return Test_Block (Test_BAR_Base, Test_BAR_Bytes);
         when Unknown       => return (Status => Unknown_Host_Bridge, Base => 0, Bytes => 0);
      end case;
   end Discover;

   procedure Outside (R, Taken : Span; Low, High : out Span) is
      --  In pages, so the ends are visibly page multiples.
      R_End_Page : constant Unsigned_64 := R.First / Page_Bytes + R.Bytes / Page_Bytes;
      T_End_Page : constant Unsigned_64 := Taken.First / Page_Bytes + Taken.Bytes / Page_Bytes;
      R_End : constant Unsigned_64 := R_End_Page * Page_Bytes;
      T_End : constant Unsigned_64 := T_End_Page * Page_Bytes;
   begin
      pragma Assert (R_End = R.First + R.Bytes);
      pragma Assert (T_End = Taken.First + Taken.Bytes);
      Low := (First => 0, Bytes => 0);
      High := (First => 0, Bytes => 0);
      if Disjoint (R, Taken) then
         Low := R;
         return;
      end if;
      if R.First < Taken.First then
         Low := (First => R.First, Bytes => Taken.First - R.First);
      end if;
      if T_End < R_End then
         pragma Assert (R_End - T_End = (R_End_Page - T_End_Page) * Page_Bytes);
         High := (First => T_End, Bytes => R_End - T_End);
      end if;
   end Outside;

end Devmgr_Platform_Pool;
