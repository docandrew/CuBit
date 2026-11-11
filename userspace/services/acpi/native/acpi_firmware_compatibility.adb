pragma Ada_2022;
with AML_Decode;
package body ACPI_Firmware_Compatibility with SPARK_Mode is
   use type AML_Interface_Policy.Add_Status;
   function Name (Item : Profile) return String is
     (case Item is when Declared.Neutral => "neutral",
      when Declared.Known_Windows => "known-windows-compatibility");
   function Policy_For (Item : Profile) return AML_Interface_Policy.Policy is
      Result : AML_Interface_Policy.Policy := AML_Interface_Policy.Empty;
      procedure Include (Text : String) is
         Data : AML_Decode.Bytes (1 .. Text'Length);
         Status : AML_Interface_Policy.Add_Status;
      begin
         for I in Data'Range loop
            Data (I) := Character'Pos (Text (Text'First+I-1));
         end loop;
         AML_Interface_Policy.Add (Result,Data,Status);
         if Status /= AML_Interface_Policy.Added then
            raise Program_Error with "invalid static firmware compatibility profile";
         end if;
      end Include;
   begin
      case Item is
         when Declared.Neutral => null;
         when Declared.Known_Windows =>
            Include ("Windows 2000");
            Include ("Windows 2001");
            Include ("Windows 2001 SP1");
            Include ("Windows 2001.1");
            Include ("Windows 2001 SP2");
            Include ("Windows 2001.1 SP1");
            Include ("Windows 2006");
            Include ("Windows 2006 SP1");
            Include ("Windows 2006.1");
            Include ("Windows 2006 SP2");
            Include ("Windows 2009");
            Include ("Windows 2012");
            Include ("Windows 2013");
            Include ("Windows 2015");
            Include ("Windows 2016");
            Include ("Windows 2017");
            Include ("Windows 2017.2");
            Include ("Windows 2018");
            Include ("Windows 2018.2");
            Include ("Windows 2019");
            Include ("Windows 2020");
            Include ("Windows 2021");
            Include ("Windows 2022");
      end case;
      return Result;
   end Policy_For;
end ACPI_Firmware_Compatibility;
