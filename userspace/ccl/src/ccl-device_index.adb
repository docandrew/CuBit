package body CCL.Device_Index with SPARK_Mode => On is

   BYTE_BITS : constant := 8;
   LENGTH_OFFSET : constant := NAME_BYTES;
   SECTION_OFFSET : constant := RECORD_PREFIX_BYTES;

   procedure Encode (I : Index; Bytes : out Index_Bytes; Length : out Index_Length) is
      procedure Put16 (At_Byte : Positive; Value : Unsigned_16)
      with Pre => At_Byte < MAX_INDEX_BYTES
      is
      begin
         Bytes (At_Byte) := Unsigned_8 (Value and 16#FF#);
         Bytes (At_Byte + 1) := Unsigned_8 (Shift_Right (Value, BYTE_BITS));
      end Put16;
      Base : Positive;
      Section : DS.Section_Bytes;
      Section_Length : DS.Section_Length;
   begin
      Bytes := [others => 0];
      Put16 (1, Unsigned_16 (MAGIC mod 2 ** 16));
      Put16 (3, Unsigned_16 (MAGIC / 2 ** 16));
      Put16 (5, VERSION);
      Put16 (7, Unsigned_16 (I.Count));
      for D in 1 .. I.Count loop
         Base := HEADER_BYTES + (D - 1) * RECORD_BYTES + 1;
         for C in 1 .. I.Drivers (D).Name.Length loop
            Bytes (Base + C - 1) := Character'Pos (I.Drivers (D).Name.Text (C));
         end loop;
         DS.Encode (I.Drivers (D).Matches, Section, Section_Length);
         Put16 (Base + LENGTH_OFFSET, Unsigned_16 (Section_Length));
         for B in 1 .. Section_Length loop
            Bytes (Base + SECTION_OFFSET + B - 1) := Section (B);
         end loop;
      end loop;
      Length := HEADER_BYTES + I.Count * RECORD_BYTES;
   end Encode;

   procedure Decode (Data : Byte_Array; I : out Index; Status : out Decode_Status) is
      function U16 (First : Positive) return Unsigned_16 is
        (Unsigned_16 (Data (First)) or Shift_Left (Unsigned_16 (Data (First + 1)), BYTE_BITS))
      with Pre => First in Data'Range and then First < Data'Last;

      Count : Natural;
      Base : Positive;
      Section_Length : Natural;
      Name : Driver_Name;
      Table : DS.Match_Table;
      Section_Status : DS.Decode_Status;
      use type DS.Decode_Status;
   begin
      I := (others => <>);
      Status := Invalid_Length;
      if Data'Length < HEADER_BYTES then
         return;
      end if;
      if U16 (1) /= Unsigned_16 (MAGIC mod 2 ** 16)
        or else U16 (3) /= Unsigned_16 (MAGIC / 2 ** 16)
        or else U16 (5) /= VERSION
        or else U16 (7) > MAX_DRIVERS
      then
         Status := Invalid_Header;
         return;
      end if;
      Count := Natural (U16 (7));
      if Data'Length /= HEADER_BYTES + Count * RECORD_BYTES then
         return;
      end if;
      for D in 1 .. Count loop
         pragma Loop_Invariant (I.Count = D - 1);
         pragma Loop_Invariant (Valid_Index (I));
         Base := HEADER_BYTES + (D - 1) * RECORD_BYTES + 1;
         Name := (others => <>);
         for C in 1 .. NAME_BYTES loop
            Name.Text (C) := Character'Val (Data (Base + C - 1));
            if Name.Text (C) /= ASCII.NUL and then Name.Length = C - 1 then
               Name.Length := C;
            end if;
         end loop;
         Section_Length := Natural (U16 (Base + LENGTH_OFFSET));
         if not Valid_Name (Name)
           or else Data (Base + LENGTH_OFFSET + 2) /= 0
           or else Data (Base + LENGTH_OFFSET + 3) /= 0
           or else Section_Length not in DS.HEADER_BYTES + DS.ENTRY_BYTES .. DS.MAX_SECTION_BYTES
         then
            Status := Invalid_Record;
            return;
         end if;
         for B in Base + SECTION_OFFSET + Section_Length .. Base + RECORD_BYTES - 1 loop
            if Data (B) /= 0 then
               Status := Invalid_Record;
               return;
            end if;
         end loop;
         declare
            Section : DS.Section_Bytes := [others => 0];
         begin
            for B in 1 .. Section_Length loop
               Section (B) := Data (Base + SECTION_OFFSET + B - 1);
            end loop;
            DS.Decode (Section (1 .. Section_Length), Table, Section_Status);
         end;
         if Section_Status /= DS.Decoded then
            Status := Invalid_Record;
            return;
         end if;
         I.Count := D;
         I.Drivers (D) := (Name => Name, Matches => Table);
      end loop;
      Status := Decoded;
   end Decode;

   procedure Bind (I : Index; F : DS.PCI_Function; Result : out Binding) is
      Top : DS.Rank := DS.No_Rank;
      R : DS.Rank;
      E : DS.Entry_Count;
   begin
      Result := (others => <>);
      for D in 1 .. I.Count loop
         pragma Loop_Invariant
           (case Result.Kind is
              when Unbound => Result.Driver = 0 and then Top = DS.No_Rank,
              when Bound =>
                 Result.Driver in 1 .. D - 1
                 and then Result.At_Entry in 1 .. I.Drivers (Result.Driver).Matches.Count
                 and then Top = DS.Rank_Of (I.Drivers (Result.Driver).Matches.Entries
                                              (Result.At_Entry), F)
                 and then Top /= DS.No_Rank,
              when Ambiguous =>
                 Result.Driver in 1 .. D - 1 and then Result.Rival in 1 .. D - 1
                 and then Result.Driver /= Result.Rival and then Top /= DS.No_Rank);
         DS.Best (I.Drivers (D).Matches, F, R, E);
         if E /= 0 then
            if R > Top then
               Top := R;
               Result := (Kind => Bound, Driver => D, At_Entry => E, Rival => 0);
            elsif R = Top and then Result.Kind = Bound then
               Result := (Kind => Ambiguous, Driver => Result.Driver, At_Entry => 0, Rival => D);
            end if;
         end if;
      end loop;
   end Bind;

   procedure First_Ambiguity
     (I : Index; Found : out Boolean; Left, Right : out Driver_Count;
      Left_Entry, Right_Entry : out DS.Entry_Count)
   is
   begin
      Found := False;
      Left := 0;
      Right := 0;
      Left_Entry := 0;
      Right_Entry := 0;
      for A in 1 .. I.Count loop
         pragma Loop_Invariant (not Found and then Left = 0 and then Right = 0);
         pragma Assert (DS.Valid_Table (I.Drivers (A).Matches));
         for B in Driver_Number range A + 1 .. I.Count loop
            pragma Loop_Invariant (not Found and then Left = 0 and then Right = 0);
            pragma Assert (DS.Valid_Table (I.Drivers (B).Matches));
            for EA in 1 .. I.Drivers (A).Matches.Count loop
               pragma Loop_Invariant (not Found and then Left = 0 and then Right = 0);
               pragma Assert (DS.Valid_Match (I.Drivers (A).Matches.Entries (EA)));
               for EB in 1 .. I.Drivers (B).Matches.Count loop
                  pragma Loop_Invariant (not Found and then Left = 0 and then Right = 0);
                  pragma Assert (DS.Valid_Match (I.Drivers (B).Matches.Entries (EB)));
                  if DS.Ambiguous (I.Drivers (A).Matches.Entries (EA),
                                   I.Drivers (B).Matches.Entries (EB))
                  then
                     Found := True;
                     Left := A;
                     Right := B;
                     Left_Entry := EA;
                     Right_Entry := EB;
                     return;
                  end if;
               end loop;
            end loop;
         end loop;
      end loop;
   end First_Ambiguity;

   function Find (I : Index; Name : String) return Driver_Count is
   begin
      --  Character by character: no memcmp in a freestanding runtime.
      for D in 1 .. I.Count loop
         if Name'Length = I.Drivers (D).Name.Length
           and then (for all C in Name'Range =>
                       Name (C) = I.Drivers (D).Name.Text (C - Name'First + 1))
         then
            return D;
         end if;
      end loop;
      return 0;
   end Find;
end CCL.Device_Index;
