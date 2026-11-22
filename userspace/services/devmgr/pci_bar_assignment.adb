pragma Ada_2022;
package body PCI_BAR_Assignment with SPARK_Mode is
   use type P.Outcome;

   --  W clipped to [Lowest_Placement_Page, Ceiling): empty when nothing of
   --  it is left.
   function Clip (W : P.Span; Ceiling : P.Page) return P.Span is
     (P.Meet (W, (Lowest_Placement_Page, Ceiling)))
   with Post => (if P.Valid (Clip'Result) then
                   P.Inside (Clip'Result, W) and then Clip'Result.Past <= Ceiling
                   and then Clip'Result.First >= Lowest_Placement_Page);

   procedure Assign
     (Low, High : Unsigned_32;
      Size      : BAR_Bytes;
      Windows   : P.Span_Array;
      Allowed   : P.Span_Array;
      Taken     : P.Span_Array;
      A         : out Assignment)
   is
      Wide  : constant Boolean := Is_64 (Low);
      Pages : P.Page;
      Placed : P.Outcome;
      Where : P.Span;
      Address : Unsigned_64;
   begin
      A := (others => <>);
      if Is_IO (Low) then
         A.Result := Not_Memory;
         return;
      end if;
      if Decoded (Low, High, Wide) /= 0 then
         A.Result := Already_Assigned;
         return;
      end if;
      if not Power_Of_Two (Size) then
         A.Result := Invalid_Size;
         return;
      end if;
      pragma Assert (Size <= Page_Bytes or else Size mod Page_Bytes = 0);
      Pages := Size_Pages (Size);

      --  Every window starts at 1 MiB at the lowest; a 32-bit BAR's also
      --  ends at 4 GiB.
      declare
         Ceiling : constant P.Page := (if Wide then P.Page_Limit else Low_4G_Pages);
         Clipped : P.Span_Array (Windows'Range) := [others => (0, 0)];
      begin
         for I in Windows'Range loop
            Clipped (I) := Clip (Windows (I), Ceiling);
            pragma Loop_Invariant
              (for all J in Windows'First .. I =>
                 (if P.Valid (Clipped (J)) then
                    P.Inside (Clipped (J), Windows (J)) and then Clipped (J).Past <= Ceiling
                    and then Clipped (J).First >= Lowest_Placement_Page));
         end loop;
         P.Place (Clipped, Allowed, Taken, Pages, Placed, Where);
         if Placed = P.Placed then
            --  The window Where lies in is a clipped one: inside its
            --  original, at or above 1 MiB and below the ceiling.
            pragma Assert (for some J in Clipped'Range =>
                             P.Valid (Clipped (J)) and then P.Inside (Where, Clipped (J)));
            pragma Assert (Where.First >= Lowest_Placement_Page);
            pragma Assert (Where.Past <= Ceiling);
            pragma Assert (if not Wide then Where.Past <= Low_4G_Pages);
            pragma Assert (for some J in Windows'Range =>
                             P.Valid (Windows (J)) and then P.Inside (Where, Windows (J)));
         end if;
      end;

      if Placed /= P.Placed then
         --  Pages is a power of two here, so the search only runs out of room.
         A.Result := No_Room;
         return;
      end if;

      Address := Where.First * Page_Bytes;
      A.Where := Where;
      A.New_Low := (Unsigned_32 (Address and 16#FFFF_FFFF#) and BAR_Address_Mask)
                   or (Low and BAR_Flag_Bits);
      A.New_High := (if Wide then Unsigned_32 (Shift_Right (Address, 32)) else 0);
      A.Result := Assigned;
   end Assign;
end PCI_BAR_Assignment;
