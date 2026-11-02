package body Desktop_Startup_Layout with SPARK_Mode is
   function Frame_Bytes (Width, Height : Wide) return Wide is
      -- Signed, so the bound below is ordinary arithmetic, not modular:
      -- each edge is at most 16 bits and no product overflows.
      subtype Edge is Long_Long_Integer range 0 .. Long_Long_Integer (Maximum_Edge);
      W : constant Edge := Edge (Width);
      H : constant Edge := Edge (Height);
      Area, Bytes : Long_Long_Integer;
   begin
      pragma Assert (W * H <= Edge'Last * H);
      pragma Assert (Edge'Last * H <= Edge'Last * Edge'Last);
      Area := W * H;
      pragma Assert (Area <= Long_Long_Integer (Maximum_Edge * Maximum_Edge));
      pragma Assert (if W >= 1 and H >= 1 then W * H >= H and H >= 1);
      Bytes := Area * Long_Long_Integer (Bytes_Per_Pixel);
      pragma Assert (Bytes <= Long_Long_Integer (Maximum_Frame_Bytes));
      pragma Assert (if W >= 1 and H >= 1 then Area >= 1);
      pragma Assert (if Area >= 1 then Bytes >= Long_Long_Integer (Bytes_Per_Pixel));
      return Wide (Bytes);
   end Frame_Bytes;
   function Required_Bytes (Width, Height : Wide) return Natural is
   begin
      if not Supported (Width, Height) then return 0; end if;
      return Natural (Frame_Bytes (Width, Height));
   end Required_Bytes;
   function GPU_Budget_Bytes (Width, Height : Wide) return Natural is
   begin
      if not Supported (Width, Height) then return 0; end if;
      return Natural (GPU_Budget_Demand (Width, Height));
   end GPU_Budget_Bytes;
end Desktop_Startup_Layout;
