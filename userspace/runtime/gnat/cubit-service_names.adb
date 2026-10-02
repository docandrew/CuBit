package body CuBit.Service_Names is
   function Driver_Of (Name : String) return Unsigned_64 is
   begin
      for Item of SERVICES loop
         if Item.Name.Text (1 .. Item.Name.Length) = Name then
            return Item.Driver;
         end if;
      end loop;
      return NO_DRIVER;
   end Driver_Of;

   function Process_Of (Name : String) return Unsigned_64 is
      Driver : constant Unsigned_64 := Driver_Of (Name);
   begin
      return
        (if Driver = NO_DRIVER then 0
         else getInfo (SYSINFO_REGISTERED_DRIVER, Driver));
   end Process_Of;
end CuBit.Service_Names;
