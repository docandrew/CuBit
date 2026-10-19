package body Files_Places with SPARK_Mode is

   --  Where the volume's name ends: before the first '/', else Prefix'Last.
   function Name_Last (Prefix : String) return Integer
     with Post => Name_Last'Result <= Prefix'Last
                  and then (if Prefix'Length > 0 then Name_Last'Result >= Prefix'First - 1)
   is
   begin
      for K in Prefix'Range loop
         if Prefix (K) = ROOT_END then
            return K - 1;
         end if;
      end loop;
      return Prefix'Last;
   end Name_Last;

   --  The ':' within the name, 0 if none.
   function Number_At (Prefix : String) return Natural
     with Post => Number_At'Result = 0
                  or else (Number_At'Result in Prefix'Range and then Number_At'Result <= Name_Last (Prefix))
   is
   begin
      if Prefix'Length = 0 then
         return 0;
      end if;
      for K in Prefix'First .. Name_Last (Prefix) loop
         if Prefix (K) = NUMBER_MARK then
            return K;
         end if;
      end loop;
      return 0;
   end Number_At;

   function Qualified (Prefix : String) return Boolean is
     (Prefix'Length >= 2 and then Prefix (Prefix'First) = VOLUME_MARK);

   function Device (Prefix : String) return String
     with Pre => Qualified (Prefix)
   is
      Mark : constant Natural := Number_At (Prefix);
      Last : constant Integer := (if Mark > 0 then Mark - 1 else Name_Last (Prefix));
   begin
      return Prefix (Prefix'First + 1 .. Last);
   end Device;

   function Number (Prefix : String) return String
     with Pre  => Qualified (Prefix),
          Post => Number'Result'Length <= Prefix'Length
   is
      Mark : constant Natural := Number_At (Prefix);
   begin
      if Mark = 0 or else Mark >= Name_Last (Prefix) then
         return "";
      end if;
      return Prefix (Mark + 1 .. Name_Last (Prefix));
   end Number;

   function Kind_Of (Prefix : String) return Place_Kind is
   begin
      if not Qualified (Prefix) then
         return Other_Volume;
      end if;
      declare
         Name : constant String := Device (Prefix);
      begin
         return (if Name = MEMORY_DEVICE then Live_Workspace
                 elsif Name = NVME_DEVICE then NVMe_Volume
                 elsif Name = ATA_DEVICE then ATA_Volume
                 elsif Name = OPTICAL_DEVICE then Optical_Disc
                 elsif Name = BOOT_ARCHIVE then Boot_Image
                 else Other_Volume);
      end;
   end Kind_Of;

   function Is_Root (Prefix : String) return Boolean is
     (Qualified (Prefix) and then Name_Last (Prefix) >= Prefix'Last - 1);

   function Caption (Prefix : String) return String is
   begin
      if not Is_Root (Prefix) then
         return Prefix;
      end if;
      declare
         Index : constant String := Number (Prefix);
         --  " N", left out for the first of a kind usually alone.
         Suffix : constant String := (if Index'Length = 0 or else Index = "0" then "" else " " & Index);
      begin
         case Kind_Of (Prefix) is
            when Live_Workspace => return "Live workspace" & Suffix;
            when NVMe_Volume => return "NVMe volume" & (if Index'Length = 0 then "" else " " & Index);
            when ATA_Volume => return "ATA disk" & (if Index'Length = 0 then "" else " " & Index);
            when Optical_Disc => return "Optical disc" & Suffix;
            when Boot_Image => return "Boot image";
            when Other_Volume => return Prefix;
         end case;
      end;
   end Caption;
end Files_Places;
