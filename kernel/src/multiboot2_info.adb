pragma Ada_2022;
package body Multiboot2_Info with SPARK_Mode is
   use type Firmware_Tables.Admission;
   function Read_32 (Data : Bytes; Offset : Natural) return Unsigned_32 is
      V : Unsigned_32 := 0;
   begin
      for I in 0 .. 3 loop
         V := V or Shift_Left (Unsigned_32 (Data (Data'First + Offset + I)), 8 * I);
      end loop;
      return V;
   end Read_32;
   function Read_64 (Data : Bytes; Offset : Natural) return Unsigned_64
     with Pre => Offset <= Data'Length and then Data'Length - Offset >= 8
   is
   begin
      return Unsigned_64 (Read_32 (Data, Offset)) or
        Shift_Left (Unsigned_64 (Read_32 (Data, Offset + 4)), 32);
   end Read_64;

   function Header_Extent (Data : Bytes) return Natural is
      Length : Long_Long_Integer;
   begin
      if Data'Length < 8 then return 0; end if;
      Length := Long_Long_Integer (Read_32 (Data, 0));
      if Read_32 (Data, 4) /= 0 or else Length < 16 or else
        Length > Maximum_Bytes or else Length mod 8 /= 0
      then
         return 0;
      end if;
      return Natural (Length);
   end Header_Extent;

   procedure Clear (Value : out Snapshot) is
   begin
      Value.Frame := (others => <>);
      Value.Root := (Status => Firmware_Tables.Truncated);
      Value.Module_Count := 0;
      for I in Value.Modules'Range loop
         Value.Modules (I) := (others => <>);
      end loop;
   end Clear;

   procedure Decode
     (Data : Bytes; Maximum_Address : Unsigned_64;
      Value : out Snapshot; Map : out Multiboot_Memory_Map.Entries;
      Count : out Natural; Result : out Status)
     with Post => Count <= Map'Length
   is
      Position : Natural := 8;
      Tag, Size_Wire : Unsigned_32;
      Size, Following : Natural;
      Seen_Map, Seen_Frame, Seen_Old, Seen_New : Boolean := False;
   begin
      Clear (Value);
      for I in Map'Range loop
         Map (I) := (others => <>);
      end loop;
      Count := 0;
      Result := Bad_Header;
      if Header_Extent (Data) = 0 or else Header_Extent (Data) /= Data'Length then
         return;
      end if;
      while Position < Data'Length loop
         pragma Loop_Invariant (Position in 8 .. Data'Length);
         pragma Loop_Invariant (Data'Length <= Maximum_Bytes);
         pragma Loop_Invariant (Count <= Map'Length);
         pragma Loop_Variant (Decreases => Data'Length - Position);
         Result := Bad_Tag;
         if Data'Length - Position < 8 then return; end if;
         Tag := Read_32 (Data, Position);
         Size_Wire := Read_32 (Data, Position + 4);
         if Size_Wire < 8 or else
           Long_Long_Integer (Size_Wire) > Long_Long_Integer (Data'Length - Position)
         then return; end if;
         Size := Natural (Size_Wire);
         Following := Position + Size;
         -- Metadata budget leaves room for rounding without integer overflow.
         Following := ((Following + 7) / 8) * 8;
         if Following > Data'Length then return; end if;
         case Tag is
            when 0 =>
               if Size /= 8 or else Following /= Data'Length then return; end if;
               if not Seen_Map then Result := Missing_Map;
               elsif not Seen_Frame then Result := Missing_Framebuffer;
               else Result := Success;
               end if;
               return;
            when 3 =>
               Result := Invalid_Module;
               if Size < 17 then return; end if;
               if Value.Module_Count = Boot_Modules.Maximum_Modules then
                  Result := Capacity_Exceeded; return;
               end if;
               declare
                  Item : Module_Description;
                  Terminated : Boolean := False;
               begin
                  Item.First := Read_32 (Data, Position + 8);
                  Item.Limit := Read_32 (Data, Position + 12);
                  for I in 0 .. Natural'Min (Size - 17, Boot_Modules.Maximum_Name) loop
                     declare
                        B : constant Unsigned_8 := Data (Data'First + Position + 16 + I);
                     begin
                        if B = 0 then Terminated := True; exit;
                        elsif I = Boot_Modules.Maximum_Name then return;
                        end if;
                        Item.Name.Text (I + 1) := Character'Val (B);
                        Item.Name.Length := I + 1;
                     end;
                  end loop;
                  if not Terminated or else Item.Name.Length = 0 then return; end if;
                  Value.Module_Count := Value.Module_Count + 1;
                  Value.Modules (Value.Module_Count) := Item;
               end;
            when 6 =>
               if Seen_Map then Result := Duplicate_Tag; return; end if;
               Seen_Map := True;
               Result := Invalid_Map;
               if Size < 16 or else Read_32 (Data, Position + 12) /= 0 then return; end if;
               declare
                  Stride_Wire : constant Unsigned_32 := Read_32 (Data, Position + 8);
                  Stride, Cursor : Natural;
               begin
                  if Stride_Wire < 24 or else
                    Long_Long_Integer (Stride_Wire) > Long_Long_Integer (Size - 16)
                  then return; end if;
                  Stride := Natural (Stride_Wire);
                  if (Size - 16) mod Stride /= 0 then return; end if;
                  Cursor := Position + 16;
                  while Position + Size - Cursor >= Stride loop
                     pragma Loop_Invariant (Cursor in Position + 16 .. Position + Size);
                     pragma Loop_Invariant ((Position + Size - Cursor) mod Stride = 0);
                     pragma Loop_Invariant (Count <= Map'Length);
                     pragma Loop_Variant (Decreases => Position + Size - Cursor);
                     if Count = Map'Length then Result := Capacity_Exceeded; return; end if;
                     declare
                        Base : constant Unsigned_64 := Read_64 (Data, Cursor);
                        Length : constant Unsigned_64 := Read_64 (Data, Cursor + 8);
                        Kind : constant Unsigned_32 := Read_32 (Data, Cursor + 16);
                        Item : Multiboot_Memory_Map.Decoded_Region;
                     begin
                        if Length /= 0 then
                           if Base > Maximum_Address or else Length - 1 > Maximum_Address - Base then
                              return;
                           end if;
                           Item := (First => Base, Last => Base + (Length - 1), Empty => False,
                             Kind => (case Kind is
                               when 1 => Multiboot_Memory_Map.Usable,
                               when 3 => Multiboot_Memory_Map.ACPI_Reclaim,
                               when 4 => Multiboot_Memory_Map.ACPI_NVS,
                               when 5 => Multiboot_Memory_Map.Defective,
                               when others => Multiboot_Memory_Map.Reserved));
                        end if;
                        Map (Map'First + Count) := Item;
                        Count := Count + 1;
                     end;
                     Cursor := Cursor + Stride;
                  end loop;
               end;
            when 8 =>
               if Seen_Frame then Result := Duplicate_Tag; return; end if;
               Seen_Frame := True;
               Result := Invalid_Framebuffer;
               if Size < 32 then return; end if;
               Value.Frame :=
                 (Base => Read_64 (Data, Position + 8),
                  Pitch => Read_32 (Data, Position + 16),
                  Width => Read_32 (Data, Position + 20),
                  Height => Read_32 (Data, Position + 24),
                  Depth => Data (Data'First + Position + 28),
                  Kind => Data (Data'First + Position + 29), others => <>);
               if Value.Frame.Kind = 1 then
                  if Size < 38 then return; end if;
                  Value.Frame.Red_Position := Data (Data'First + Position + 32);
                  Value.Frame.Red_Size := Data (Data'First + Position + 33);
                  Value.Frame.Green_Position := Data (Data'First + Position + 34);
                  Value.Frame.Green_Size := Data (Data'First + Position + 35);
                  Value.Frame.Blue_Position := Data (Data'First + Position + 36);
                  Value.Frame.Blue_Size := Data (Data'First + Position + 37);
               elsif Value.Frame.Kind /= 2 then return;
               end if;
            when 14 | 15 =>
               if (Tag = 14 and Seen_Old) or else (Tag = 15 and Seen_New) then
                  Result := Duplicate_Tag; return;
               end if;
               if Tag = 14 then Seen_Old := True; else Seen_New := True; end if;
               Result := Invalid_ACPI;
               if (Tag = 14 and then Size /= 28) or else (Tag = 15 and then Size < 44) then
                  return;
               end if;
               if (Tag = 14 and then Data (Data'First + Position + 23) /= 0) or else
                 (Tag = 15 and then Data (Data'First + Position + 23) < 2)
               then return; end if;
               declare
                  Root : constant Firmware_Tables.Root_Result := Firmware_Tables.Read_Root
                    (Firmware_Tables.Bytes (Data (Data'First + Position + 8 ..
                       Data'First + Position + (Size - 1))));
               begin
                  Result := Invalid_ACPI;
                  if Root.Status /= Firmware_Tables.Accepted then return; end if;
                  if Root.Extent /= Size - 8 then return; end if;
                  if Tag = 15 or else not Seen_New then Value.Root := Root; end if;
               end;
            when 18 => Result := Boot_Services_Active; return;
            when others => null; -- unknown, bounded tags are safely skipped
         end case;
         Position := Following;
      end loop;
      Result := Missing_End;
   end Decode;

   procedure Parse
     (Data : Bytes; Maximum_Address : Unsigned_64;
      Value : out Snapshot; Map : out Multiboot_Memory_Map.Entries;
      Count : out Natural; Result : out Status)
   is
   begin
      Decode (Data, Maximum_Address, Value, Map, Count, Result);
      if Result /= Success then
         Count := 0;
         Clear (Value);
      end if;
   end Parse;
end Multiboot2_Info;
