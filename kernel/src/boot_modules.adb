pragma Ada_2022;
package body Boot_Modules with SPARK_Mode is
   procedure Clear_Padding (Data : in out Multiboot_Memory_Map.Bytes;
                            Payload_Bytes : Natural) is
   begin
      Data (Data'First + Payload_Bytes .. Data'Last) := [others => 0];
   end Clear_Padding;

   function Clip (Value : Unsigned_64) return Address is
     (if Value >= Window_End then Window_End else Address (Value));
   function In_RAM (Map : Multiboot_Memory_Map.Entries;
                    First, Limit : Address) return Boolean
   is
      use Multiboot_Memory_Map;
      Cursor : Address := First;
      Following : Address;
   begin
      if First = 0 or else First >= Limit then return False; end if;
      for Region of Map loop
         if not Region.Empty and then Region.Kind /= Usable and then
           Clip (Region.First) < Limit and then First <= Clip (Region.Last)
         then
            return False;
         end if;
      end loop;
      -- Cover adjoining/overlapping usable regions without trusting ordering.
      while Cursor < Limit loop
         pragma Loop_Invariant (Cursor >= First and then Cursor <= Limit);
         pragma Loop_Variant (Decreases => Limit - Cursor);
         Following := Cursor;
         for Region of Map loop
            pragma Loop_Invariant (Following >= Cursor and then Following <= Limit);
            if not Region.Empty and then Region.Kind = Usable and then
              Clip (Region.First) <= Cursor and then Cursor <= Clip (Region.Last)
            then
               if Clip (Region.Last) >= Limit - 1 then
                  Following := Limit;
               else
                  Following := Address'Max (Following, Clip (Region.Last) + 1);
               end if;
            end if;
         end loop;
         if Following = Cursor then return False; end if;
         Cursor := Following;
      end loop;
      return True;
   end In_RAM;

   procedure Append (State : in out Catalog; Map : Multiboot_Memory_Map.Entries;
                     First, Limit : Unsigned_64; Protected_End : Address;
                     Name : Module_Name; Result : out Status)
   is
      Item : Image;
      Start, Finish : Address;
   begin
      if State.Published then Result := Already_Sealed; return; end if;
      if State.Used = Maximum_Modules then Result := Capacity_Exceeded; return; end if;
      if First > Window_End or else Limit > Window_End then
         Result := Invalid_Range; return;
      end if;
      Start := Address (First);
      Finish := Address (Limit);
      if Start = 0 or else Start < Protected_End or else Start >= Finish or else
        Start mod Page_Bytes /= 0
      then
         Result := Invalid_Range; return;
      end if;
      if Name.Length = 0 then Result := Invalid_Name; return; end if;
      Item := (First => Start, Limit => Finish,
        Page_Limit => ((Finish - 1) / Page_Bytes + 1) * Page_Bytes,
        Name => Name);
      if not In_RAM (Map, Item.First, Item.Page_Limit) then
         Result := Not_RAM; return;
      end if;
      for I in 1 .. State.Used loop
         if Overlaps (Item.First, Item.Page_Limit,
                      State.Items (I).First, State.Items (I).Page_Limit)
         then
            Result := Overlapping_Payload; return;
         end if;
         if Name.Length = State.Items (I).Name.Length and then
           Name.Text (1 .. Name.Length) =
             State.Items (I).Name.Text (1 .. Name.Length)
         then
            Result := Duplicate_Name; return;
         end if;
         pragma Loop_Invariant
           (for all J in 1 .. I =>
              not Overlaps (Item.First, Item.Page_Limit,
                            State.Items (J).First, State.Items (J).Page_Limit));
      end loop;
      State.Items (State.Used + 1) := Item;
      State.Used := State.Used + 1;
      Result := Success;
   end Append;

   procedure Seal (State : in out Catalog) is
   begin
      State.Published := True;
   end Seal;

   function Reserved_End (State : Catalog) return Address is
      Last : Address := 0;
   begin
      for I in 1 .. State.Used loop
         Last := Address'Max (Last, State.Items (I).Page_Limit);
         pragma Loop_Invariant
           (for all J in 1 .. I => State.Items (J).Page_Limit <= Last);
      end loop;
      return Last;
   end Reserved_End;
end Boot_Modules;
