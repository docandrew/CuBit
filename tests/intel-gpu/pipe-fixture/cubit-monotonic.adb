package body CuBit.Monotonic is
   function Read return Reading is
   begin
      raise Program_Error with "rejected pipe acquisition reached clock";
      return (Available => False);
   end Read;
end CuBit.Monotonic;
