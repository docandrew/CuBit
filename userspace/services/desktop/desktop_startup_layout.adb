package body Desktop_Startup_Layout with SPARK_Mode is
   function Required_Bytes (Width, Height : Wide) return Natural is
   begin
      if not Supported (Width, Height) then return 0; end if;
      return Natural (Width * Height * 4);
   end Required_Bytes;
end Desktop_Startup_Layout;
