package body Volume_List with SPARK_Mode is
   function Name (List : State; Volume : Volume_Index) return String is
     (List.Items (Volume).Text (1 .. List.Items (Volume).Length));

   function Binding (List : State; Volume : Volume_Index) return Device_Binding is
     (List.Items (Volume).Device);

   procedure Register
     (List : in out State; Name : String; Device : Device_Binding;
      Volume : out Volume_Reference; Result : out Registration_Result)
   is
   begin
      Volume := No_Volume;
      Result := Invalid_Name;
      if Name'Length = 0 or else Name'Length > Maximum_Name_Bytes then
         return;
      end if;
      for Ch of Name loop
         if Ch not in 'a' .. 'z' | 'A' .. 'Z' | '0' .. '9' | ':' | '-' | '_' | '.' then
            return;
         end if;
      end loop;
      if Name (Name'First) not in 'a' .. 'z' | 'A' .. 'Z' | '0' .. '9' then
         return;
      end if;
      for I in 1 .. List.Used loop
         if List.Items (I).Text (1 .. List.Items (I).Length) = Name then
            Result := Name_In_Use;
            return;
         elsif List.Items (I).Device.Endpoint = Device.Endpoint then
            --  One whole-volume session per endpoint today. Partition views
            --  will require independently authorized endpoints/identities.
            Result := Endpoint_In_Use;
            return;
         end if;
      end loop;
      if List.Used = Maximum_Volumes then
         Result := List_Full;
         return;
      end if;
      List.Used := List.Used + 1;
      List.Items (List.Used).Text (1 .. Name'Length) := Name;
      List.Items (List.Used).Length := Name'Length;
      List.Items (List.Used).Device := Device;
      Volume := List.Used;
      Result := Registered;
   end Register;

   procedure Select_Path
     (List : State; Path : String; Selection : out Path_Selection;
      Volume : out Volume_Reference; Relative_First : out Integer)
   is
      Separator : Integer;
   begin
      Volume := No_Volume;
      Selection := Unqualified;
      Relative_First := Path'First;
      if Path'Length = 0 or else Path (Path'First) /= '@' then
         return;
      end if;
      Selection := Unknown_Volume;
      Separator := Path'Last + 1;
      for I in Path'First + 1 .. Path'Last loop
         if Path (I) = '/' then
            Separator := I;
            exit;
         end if;
      end loop;
      if Separator = Path'First + 1 then
         Selection := Invalid_Path;
         return;
      end if;
      for I in 1 .. List.Used loop
         if Name (List, I) = Path (Path'First + 1 .. Separator - 1) then
            Volume := I;
            Selection := Known_Volume;
            Relative_First := (if Separator <= Path'Last then Separator + 1
                               else Separator);
            return;
         end if;
      end loop;
   end Select_Path;
end Volume_List;
