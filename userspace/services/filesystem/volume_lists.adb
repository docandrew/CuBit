package body Volume_Lists with SPARK_Mode is
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
      if Name'Length = 0 or else Name'Length > Maximum_Name_Bytes
        or else Name = Boot_Archive_Name or else Name = Optical_Name
        or else Name = System_Name
      then
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
         elsif Valid (Device.Endpoint)
           and then List.Items (I).Device.Endpoint = Device.Endpoint
         then
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
      if Path (Path'First + 1 .. Separator - 1) = Boot_Archive_Name then
         Selection := Boot_Archive;
      elsif Path (Path'First + 1 .. Separator - 1) = Optical_Name then
         Selection := Optical_Volume;
      end if;
      if Selection in Boot_Archive | Optical_Volume then
         Relative_First := (if Separator <= Path'Last then Separator + 1
                            else Separator);
         return;
      end if;
      if Path (Path'First + 1 .. Separator - 1) = System_Name then
         if List.System /= No_Volume then
            Volume := List.System;
            Selection := Known_Volume;
            Relative_First := (if Separator <= Path'Last then Separator + 1
                               else Separator);
         end if;
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
   procedure Attach
     (List : in out State; Volume : Volume_Index; Endpoint : Endpoint_Type;
      Attached : out Boolean)
   is
   begin
      Attached := False;
      if not Valid (Endpoint)
        or else Valid (List.Items (Volume).Device.Endpoint)
      then
         return;
      end if;
      for I in 1 .. List.Used loop
         if List.Items (I).Device.Endpoint = Endpoint then
            return;
         end if;
      end loop;
      List.Items (Volume).Device.Endpoint := Endpoint;
      Attached := True;
   end Attach;

   procedure Choose_System (List : in out State; Volume : Volume_Index; Rank : System_Rank) is
   begin
      if Rank > List.Rank then
         List.System := Volume;
         List.Rank := Rank;
      end if;
   end Choose_System;

   procedure Other_Spelling
     (List : State; Path : String; Result : out String; Length : out Natural;
      Found : out Boolean)
   is
      --  Whether Path starts with "@" & Volume and then ends or goes on
      --  with '/'; Rest is the index after the volume name.
      procedure Starts
        (Volume : String; Matches : out Boolean; Rest : out Positive)
        with Pre => Path'Last < Positive'Last and then Path'Length > 0
      is
      begin
         Matches := False;
         Rest := Path'First;
         if Volume'Length > 0 and then Volume'Length < Path'Length
           and then Path (Path'First) = '@'
           and then Path (Path'First + 1 .. Path'First + Volume'Length) = Volume
           and then (Volume'Length + 1 = Path'Length
                     or else Path (Path'First + Volume'Length + 1) = '/')
         then
            Matches := True;
            Rest := Path'First + Volume'Length + 1;
         end if;
      end Starts;

      procedure Spell (Volume : String; Rest : Positive) is
         Tail : constant Natural := (if Rest > Path'Last then 0 else Path'Last - Rest + 1);
      begin
         if Volume'Length + 1 + Tail <= Result'Length then
            Result (1) := '@';
            Result (2 .. Volume'Length + 1) := Volume;
            if Tail > 0 then
               Result (Volume'Length + 2 .. Volume'Length + 1 + Tail) := Path (Rest .. Path'Last);
            end if;
            Length := Volume'Length + 1 + Tail;
            Found := True;
         end if;
      end Spell;

      Matches : Boolean;
      Rest : Positive;
   begin
      Result := [others => ' '];
      Length := 0;
      Found := False;
      if List.System = No_Volume or else Path'Length = 0 then
         return;
      end if;
      Starts (System_Name, Matches, Rest);
      if Matches then
         Spell (Name (List, List.System), Rest);
         return;
      end if;
      Starts (Name (List, List.System), Matches, Rest);
      if Matches then
         Spell (System_Name, Rest);
      end if;
   end Other_Spelling;

   function Find (List : State; Name : String) return Volume_Reference is
   begin
      for I in 1 .. List.Used loop
         if List.Items (I).Text (1 .. List.Items (I).Length) = Name then
            return I;
         end if;
      end loop;
      return No_Volume;
   end Find;

end Volume_Lists;
