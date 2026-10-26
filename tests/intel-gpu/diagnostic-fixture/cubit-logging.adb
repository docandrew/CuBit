package body CuBit.Logging is
   procedure Attach (Item : in out Publisher; Target : CuBit.Handles.Endpoint) is
   begin
      Item.Target := Target; Attachments := Attachments + 1;
   end Attach;
   procedure Emit (Item : in out Publisher; Value : CuBit.Log_Records.Log_Record;
     Submitted : out Boolean) is
   begin
      --  Only an attached publisher may publish.
      if not CuBit.Handles.Valid (Item.Target) then raise Program_Error; end if;
      Last_Value := Value; Emissions := Emissions + 1; Submitted := True;
   end Emit;
end CuBit.Logging;
