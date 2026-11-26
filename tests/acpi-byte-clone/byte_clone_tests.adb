with Ada.Text_IO;
with AML_Decode;
with AML_Objects.Copies;

procedure Byte_Clone_Tests is
   package O renames AML_Objects;
   use type O.Allocation_Status, O.State, O.Object_Kind, AML_Decode.Bytes;
   Checks : Natural := 0;
   procedure Check (Good : Boolean; Message : String) is
   begin
      if not Good then raise Program_Error with Message; end if;
      Checks := Checks + 1;
   end Check;

   procedure Exercise
     (Tag : O.Byte_Kind; Size : Natural; Slots, Bytes : O.Storage_Capacity;
      Expected : O.Allocation_Status)
   is
      Store : O.State := O.Empty (Object_Capacity => Slots, Byte_Capacity => Bytes,
                                  Element_Capacity => 1);
      Data : AML_Decode.Bytes (1 .. Size);
      Source, Target, General_Target : O.Object_ID;
      Status, General_Status : O.Allocation_Status;
      Witness : O.Copies.Copy_Witness (Slots);
   begin
      for I in Data'Range loop
         Data (I) := AML_Decode.Byte (I);
      end loop;
      O.New_Bytes (Store, Tag, Data, Source, Status);
      Check (Status = O.Allocated and then O.Valid (Store), "setup");
      declare
         Before : constant O.State := Store;
         General : O.State := Store;
      begin
         O.Copies.Copy_Byte_Object (Store, Source, Target, Status);
         O.Copies.Clone (General, Source, General_Target, General_Status, Witness);
         Check (Status = Expected and then General_Status = Status, "status");
         Check (O.Valid (Store) and then Store = General and then Target = General_Target,
                "exact store and returned identity");
         Check (O.Byte_Data (Store, Source) = Data, "source bytes");
         if Status = O.Allocated then
            Check (Target /= Source and then O.Is_Live (Store, Target)
              and then O.Kind (Store, Target) = Tag, "fresh typed object");
            Check (O.Byte_Data (Store, Target) = Data, "complete byte copy");
            if Size > 0 then
               O.Set_Stored_Byte (Store, Target, 0, 255);
               Check (O.Byte_Data (Store, Source) = Data and then O.Byte_Data (Store, Target) /= Data,
                      "independent backing");
            end if;
         else
            Check (Target = O.No_Object and then Store = Before, "exact refusal");
         end if;
      end;
   end Exercise;
begin
   for Tag in O.Byte_Kind loop
      for Size in 0 .. 16 loop
         Exercise (Tag, Size, 2, 32, O.Allocated);
         Exercise (Tag, Size, 1, 32, O.Object_Limit);
         if Size > 0 then
            Exercise (Tag, Size, 2, Size, O.Byte_Limit);
         end if;
      end loop;
   end loop;
   Ada.Text_IO.Put_Line ("BYTE_CLONE_CHECKS=" & Checks'Image);
end Byte_Clone_Tests;
