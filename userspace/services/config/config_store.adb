package body Config_Store with SPARK_Mode is
   subtype Search_Result is Natural range 0 .. Maximum_Entries;

   function Find (Store : State; Key : String) return Search_Result is
   begin
      if Key'Length not in 1 .. Maximum_Key then
         return 0;
      end if;
      for I in Slot loop
         if Store.Entries (I).Key.Length = Key'Length and then
           Store.Entries (I).Key.Data (1 .. Store.Entries (I).Key.Length) = Key
         then
            return I;
         end if;
      end loop;
      return 0;
   end Find;

   function Key_At (Store : State; Index : Slot) return Key_Text is
     (Store.Entries (Index).Key);

   function Has_Value (Store : State; Key, Value : String) return Boolean is
     (Key'Length > 0 and then
      (for some I in Slot =>
         Store.Entries (I).Key.Data (1 .. Store.Entries (I).Key.Length) = Key
         and then Store.Entries (I).Value.Data
           (1 .. Store.Entries (I).Value.Length) = Value));

   procedure Read
     (Store : State; Key : String; Value : out Value_Text; Found : out Boolean)
   is
      Index : constant Search_Result := Find (Store, Key);
   begin
      Found := Index /= 0;
      Value := (others => <>);
      if Index /= 0 then
         Value := Store.Entries (Index).Value;
      end if;
   end Read;

   procedure Put
     (Store : in out State; Key, Value : String; Result : out Update_Result)
   is
      Index : Search_Result;
   begin
      if Key'Length not in 1 .. Maximum_Key or Value'Length > Maximum_Value then
         Result := Invalid_Request;
         return;
      end if;
      Index := Find (Store, Key);
      if Index = 0 then
         for I in Slot loop
            if Store.Entries (I).Key.Length = 0 then
               Index := I;
               exit;
            end if;
         end loop;
      end if;
      if Index = 0 then
         Result := Capacity_Exceeded;
         return;
      end if;
      --  All rejection paths precede mutation. Bounds are carried by the
      --  owned record types, not repaired with guards at each array access.
      Store.Entries (Index) := (others => <>);
      Store.Entries (Index).Key.Data (1 .. Key'Length) := Key;
      Store.Entries (Index).Key.Length := Key'Length;
      Store.Entries (Index).Value.Data (1 .. Value'Length) := Value;
      Store.Entries (Index).Value.Length := Value'Length;
      Result := Stored;
   end Put;

   procedure Remove
     (Store : in out State; Key : String; Removed : out Boolean)
   is
      Index : constant Search_Result := Find (Store, Key);
   begin
      Removed := Index /= 0;
      if Index /= 0 then
         Store.Entries (Index) := (others => <>);
      end if;
   end Remove;
end Config_Store;
