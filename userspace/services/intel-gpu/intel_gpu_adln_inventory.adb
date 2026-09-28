package body Intel_GPU_ADLN_Inventory with SPARK_Mode is
   function Decode (Vendor, Device : Unsigned_16; Fuse : Unsigned_32)
     return Inventory
   is
      Result : Inventory;
   begin
      if Vendor /= 16#8086# or Device /= 16#46D2# or Fuse = Unsigned_32'Last then
         return Result;
      end if;
      Result.Valid := True;
      -- ADL-N uses ADL-P's platform mask; media fuse bits are disable bits.
      Result.Engines := [Render | Copy => True,
        Video_0 => (Fuse and 1) = 0,
        Video_2 => (Fuse and 4) = 0,
        Enhance_0 => (Fuse and 16#1_0000#) = 0];
      Result.Domains := [GT | Render_Domain => True,
        VDBOX_0 => Result.Engines (Video_0),
        VDBOX_2 => Result.Engines (Video_2),
        VEBOX_0 => Result.Engines (Enhance_0)];
      return Result;
   end Decode;
   function Request_Register (Item : Domain) return Unsigned_32 is
     (case Item is
        when GT => 16#A188#, when Render_Domain => 16#A278#,
        when VDBOX_0 => 16#A540#, when VDBOX_2 => 16#A548#,
        when VEBOX_0 => 16#A560#);
   function Ack_Register (Item : Domain) return Unsigned_32 is
     (case Item is
        when GT => 16#130044#, when Render_Domain => 16#D84#,
        when VDBOX_0 => 16#D50#, when VDBOX_2 => 16#D58#,
        when VEBOX_0 => 16#D70#);
   function Engine_Base (Item : Engine) return Unsigned_32 is
     (case Item is
        when Render => 16#2000#, when Copy => 16#22000#,
        when Video_0 => 16#1C0000#, when Video_2 => 16#1D0000#,
        when Enhance_0 => 16#1C8000#);
   function Pending_Register (Item : Engine) return Unsigned_32 is
     (case Item is
        when Render => 16#8000#, when Copy => 16#800C#,
        when Video_0 => 16#8004#, when Video_2 => 16#80C0#,
        when Enhance_0 => 16#8010#);
   function Engine_Write_Allowed
     (Description : Inventory; Offset, Value : Unsigned_32) return Boolean is
   begin
      if not Description.Valid then return False; end if;
      for E in Engine loop
         if Description.Engines (E) then
            if Offset = Engine_Base (E) + 16#9C# then
               return Value = 16#0100_0100#;
            elsif Offset = Engine_Base (E) + 16#29C# then
               return Value = 16#0400_0400#;
            elsif Offset = Engine_Base (E) + 16#D0# then
               return Value = 16#0001_0001# or Value = 16#0004_0004# or
                 Value = 16#0001_0000#;
            end if;
         end if;
      end loop;
      return False;
   end Engine_Write_Allowed;
end Intel_GPU_ADLN_Inventory;
