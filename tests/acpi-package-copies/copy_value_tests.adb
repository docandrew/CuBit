with Ada.Text_IO;
with AML_Decode; use AML_Decode;
with AML_Objects; use AML_Objects;
with AML_Objects.Copies; use AML_Objects.Copies;
with AML_Object_Identifiers;
with AML_References;
procedure Copy_Value_Tests is
   use type Integer_Value;
   use type Byte;
   use type Bytes;
   use type AML_Object_Identifiers.Object_Address;
   use type AML_References.Reference;
   Checks : Natural := 0;
   procedure Check (Good : Boolean) is
   begin
      Checks := Checks + 1;
      if not Good then raise Program_Error with Checks'Image; end if;
   end Check;
   procedure Preserved (After, Before : State) is
   begin
      Check (Valid (After));
      for ID in 1 .. Slot_Bound (Before) loop
         if Is_Live (Before, ID) then
            Check (Is_Live (After, ID));
            Check (Address_Of (After, ID) = Address_Of (Before, ID));
            Check (Kind (After, ID) = Kind (Before, ID));
            Check (Length (After, ID) = Length (Before, ID));
            case Kind (Before, ID) is
               when Integer_Object =>
                  Check (Integer_Data (After, ID) = Integer_Data (Before, ID));
                  Check (Origin_Of (After, ID) = Origin_Of (Before, ID));
               when String_Object | Buffer_Object =>
                  Check (Byte_Data (After, ID) = Byte_Data (Before, ID));
               when Package_Object =>
                  for I in 1 .. Length (Before, ID) loop
                     Check (Element (After, ID, I - 1) = Element (Before, ID, I - 1));
                  end loop;
               when Event_Object =>
                  Check (Event_Units (After, ID) = Event_Units (Before, ID));
               when Mutex_Object =>
                  Check (Mutex_Flags (After, ID) = Mutex_Flags (Before, ID));
               when Reference_Object =>
                  Check (Reference_Data (After, ID) = Reference_Data (Before, ID));
               when Metadata_Object =>
                  Check (Metadata_Data (After, ID) = Metadata_Data (Before, ID));
               when Metadata_Snapshot_Object => null;
            end case;
         end if;
      end loop;
   end Preserved;
begin
   -- Repeated package occurrences own independent value copies.
   declare
      S : State := Empty (Object_Capacity => 16, Byte_Capacity => 16, Element_Capacity => 8);
      W : Copy_Witness (16);
      Leaf, Parent, Target, First, Second : Object_ID;
      Status : Allocation_Status;
   begin
      New_Bytes (S, Buffer_Object, [65,66], Leaf, Status); Check (Status = Allocated);
      New_Package (S, 3, Parent, Status); Check (Status = Allocated);
      Set_Element (S, Parent, 0, Leaf); Set_Element (S, Parent, 1, Leaf);
      declare Before : constant State := S; begin
         Clone (S, Parent, Target, Status, W); Check (Status = Allocated);
         Preserved (S, Before);
      end;
      Check (Target /= Parent and then Length (S, Target) = 3);
      First := Element (S, Target, 0); Second := Element (S, Target, 1);
      Check (First /= No_Object and then Second /= No_Object);
      Check (First /= Leaf and then Second /= Leaf and then First /= Second);
      Check (Element (S, Target, 2) = No_Object);
      Set_Stored_Byte (S, First, 0, 90);
      Check (Stored_Byte (S, Leaf, 0) = 65 and then Stored_Byte (S, Second, 0) = 65);
   end;
   -- Iterative traversal handles deep chains without recursive copying.
   declare
      S : State := Empty (Object_Capacity => 128, Byte_Capacity => 1, Element_Capacity => 96);
      W : Copy_Witness (128);
      Root, Parent, Target, Original : Object_ID;
      Status : Allocation_Status;
   begin
      New_Integer (S, 42, Root, Status); Check (Status = Allocated);
      for I in 1 .. 32 loop
         New_Package (S, 1, Parent, Status); Check (Status = Allocated);
         Set_Element (S, Parent, 0, Root); Root := Parent;
      end loop;
      declare Before : constant State := S; begin
         Clone (S, Root, Target, Status, W); Check (Status = Allocated);
         Preserved (S, Before);
      end;
      Original := Root;
      for I in 1 .. 32 loop
         Check (Target /= Original and then Kind (S, Target) = Package_Object);
         Target := Element (S, Target, 0); Original := Element (S, Original, 0);
      end loop;
      Check (Target /= Original and then Integer_Data (S, Target) = 42);
   end;
   -- Fresh Event copy starts unsignalled; the source count is retained.
   declare
      S : State := Empty (Object_Capacity => 3, Byte_Capacity => 1, Element_Capacity => 1);
      W : Copy_Witness (3);
      Source, Target : Object_ID;
      Status : Allocation_Status;
   begin
      New_Event (S, Source, Status); Check (Status = Allocated);
      Set_Event_Units (S, Source, 7);
      Clone (S, Source, Target, Status, W); Check (Status = Allocated);
      Check (Event_Units (S, Source) = 7 and then Event_Units (S, Target) = 0);
   end;
   -- Failures expose no target, preserve old values, and retain valid partial
   -- allocations. Actual creator-root reclamation is a separate owner step.
   for Failure in 1 .. 4 loop
      declare
         S : State := Empty (Object_Capacity => (if Failure = 1 then 4 else 8),
           Byte_Capacity => (if Failure = 2 then 3 else 8),
           Element_Capacity => (if Failure = 3 then 4 else 8));
         W : Copy_Witness (S.Object_Capacity);
         Source, Leaf, Target : Object_ID;
         Status : Allocation_Status;
      begin
         case Failure is
            when 1 => New_Integer (S, 7, Leaf, Status);
            when 2 => New_Bytes (S, String_Object, [65,66], Leaf, Status);
            when 3 => New_Package (S, 2, Leaf, Status);
            when 4 => New_Mutex (S, 0, Leaf, Status);
         end case;
         Check (Status = Allocated);
         New_Package (S, (if Failure = 1 then 2 else 1), Source, Status);
         Check (Status = Allocated); Set_Element (S, Source, 0, Leaf);
         if Failure = 1 then Set_Element (S, Source, 1, Leaf); end if;
         declare Before : constant State := S; begin
            Clone (S, Source, Target, Status, W);
            Check (Status = (case Failure is when 1 => Object_Limit,
              when 2 => Byte_Limit, when 3 => Element_Limit,
              when 4 => Unsupported_Copy));
            Check (Target = No_Object); Preserved (S, Before);
            Check (Live_Count (S) > Live_Count (Before));
         end;
      end;
   end loop;
   -- A cyclic value cannot produce a finite independent tree: bounded refusal.
   declare
      S : State := Empty (Object_Capacity => 8, Byte_Capacity => 1, Element_Capacity => 8);
      W : Copy_Witness (8);
      Source, Target : Object_ID;
      Status : Allocation_Status;
   begin
      New_Package (S, 1, Source, Status); Check (Status = Allocated);
      Set_Element (S, Source, 0, Source);
      declare Before : constant State := S; begin
         Clone (S, Source, Target, Status, W);
         Check (Status = Object_Limit and then Target = No_Object);
         Check (Live_Count (S) = S.Object_Capacity); Preserved (S, Before);
      end;
   end;
   -- Workspace shape is checked before any value allocation.
   declare
      S : State := Empty (Object_Capacity => 3, Byte_Capacity => 1, Element_Capacity => 1);
      W : Copy_Witness (2);
      Source, Target : Object_ID;
      Status : Allocation_Status;
   begin
      New_Integer (S, 1, Source, Status); Check (Status = Allocated);
      declare Before : constant State := S; begin
         Clone (S, Source, Target, Status, W);
         Check (Status = Invalid_Copy_Workspace and then Target = No_Object);
         Check (S = Before);
      end;
   end;
   Ada.Text_IO.Put_Line ("package copy checks" & Checks'Image);
end Copy_Value_Tests;
