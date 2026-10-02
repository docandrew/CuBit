pragma Ada_2022;
package body Multiboot_Memory_Map.Reclaim with SPARK_Mode is
   function Known_Byte (Map : Entries; A : Unsigned_64) return Boolean is
     (for some I in Map'Range => not Map (I).Empty
       and then Map (I).Kind = ACPI_Reclaim
       and then Map (I).First <= A and then A <= Map (I).Last);
   function Known_Range (Map : Entries; First, Last : Unsigned_64) return Boolean is
     (for all A in First .. Last => Known_Byte (Map, A));
   procedure Certify (Map : Entries; Index : Positive; First, Last : Unsigned_64)
     with Ghost,
       Pre => Index in Map'Range and then First <= Last
         and then not Map (Index).Empty and then Map (Index).Kind = ACPI_Reclaim
         and then Map (Index).First <= First and then Last <= Map (Index).Last,
       Post => Known_Range (Map, First, Last);
   procedure Certify (Map : Entries; Index : Positive; First, Last : Unsigned_64) is
   begin
      pragma Assert (for all A in First .. Last =>
        Map (Index).First <= A and then A <= Map (Index).Last);
      pragma Assert (Known_Range (Map, First, Last));
   end Certify;

   procedure Join (Map : Entries; First, Middle, Last : Unsigned_64)
     with Ghost,
       Pre => First <= Middle and then Middle <= Last
         and then Known_Range (Map, Middle, Last)
         and then (if First < Middle then Known_Range (Map, First, Middle - 1)),
       Post => Known_Range (Map, First, Last);
   procedure Join (Map : Entries; First, Middle, Last : Unsigned_64) is
      -- Joining intervals is valid for any byte predicate. Keep the map's
      -- existential witness out of this proof; Certify proves that witness.
      pragma Annotate (GNATprove, Hide_Info, "Expression_Function_Body", Known_Byte);
   begin
      pragma Assert (for all A in First .. Last =>
        (if A < Middle then Known_Byte (Map, A) else Known_Byte (Map, A)));
   end Join;

   type Coverage is record
      Found : Boolean := False;
      Last : Unsigned_64 := 0;
   end record;
   function Cover (Map : Entries; Cursor : Unsigned_64) return Coverage
     with Pre => Map'Length > 0,
       Post => (if Cover'Result.Found then Cover'Result.Last >= Cursor
       and then Known_Range (Map, Cursor, Cover'Result.Last))
   is
      pragma Annotate (GNATprove, Hide_Info, "Expression_Function_Body", Known_Byte);
      Result : Coverage;
   begin
      for I in Map'Range loop
            if not Map (I).Empty and then Map (I).Kind = ACPI_Reclaim
              and then Map (I).First <= Cursor and then Cursor <= Map (I).Last
              and then (not Result.Found or else Result.Last < Map (I).Last)
            then
               Certify (Map, I, Cursor, Map (I).Last);
               Result := (True, Map (I).Last);
            end if;
            pragma Loop_Invariant (if Result.Found then Result.Last >= Cursor
              and then Known_Range (Map, Cursor, Result.Last));
      end loop;
      return Result;
   end Cover;

   function Covers (Map : Entries; First, Last : Unsigned_64) return Boolean is
      pragma Annotate (GNATprove, Hide_Info, "Expression_Function_Body", Known_Byte);
      Cursor : Unsigned_64 := First;
   begin
      if First > Last or else not Unambiguous (Map, First, Last) then
         return False;
      end if;
      for Step in 1 .. Map'Length loop
         pragma Loop_Invariant (Cursor in First .. Last);
         pragma Loop_Invariant (if Cursor > First then Known_Range (Map, First, Cursor - 1));
         declare
            Part : constant Coverage := Cover (Map, Cursor);
         begin
         if not Part.Found then return False; end if;
         Join (Map, First, Cursor, Part.Last);
         if Part.Last >= Last then
            pragma Assert (Known_Range (Map, First, Last));
            return True;
         end if;
         pragma Assert (Known_Range (Map, First, Part.Last));
         Cursor := Part.Last + 1;
         end;
      end loop;
      return False;
   end Covers;
end Multiboot_Memory_Map.Reclaim;
