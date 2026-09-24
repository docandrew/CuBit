package body Shared_Objects with SPARK_Mode is
   procedure Attach
     (S : in out State; Owner : Owner_Index; Identity : Object_Key;
      Initial : Object_Value; Result : out Attach_Result)
   is
   begin
      if Attached (S, Owner) then
         Result := Owner_Busy;
         return;
      end if;
      for Other in Owner_Index loop
         if Attached (S, Other) and then Key (S, Other) = Identity then
            S.Owners (Owner) := S.Owners (Other);
            Result := Shared;
            return;
         end if;
      end loop;
      for Slot in Object_Index loop
         if (for all Other in Owner_Index => S.Owners (Other) /= Slot) then
            S.Identities (Slot) := Identity;
            S.Metadata (Slot) := Initial;
            S.Owners (Owner) := Slot;
            Result := Created;
            return;
         end if;
      end loop;
      Result := Full;
   end Attach;

   procedure Replace
     (S : in out State; Owner : Owner_Index; Item : Object_Value) is
   begin
      S.Metadata (S.Owners (Owner)) := Item;
   end Replace;

   procedure Detach (S : in out State; Owner : Owner_Index) is
   begin
      S.Owners (Owner) := 0;
   end Detach;
end Shared_Objects;
