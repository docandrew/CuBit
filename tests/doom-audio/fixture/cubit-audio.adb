package body CuBit.Audio is
   function open (Rate : Natural; Channels : Natural) return StreamHandle is
   begin
      pragma Assert (Rate = 48_000 and Channels = 2);
      return True;
   end open;
   function isValid (Stream : StreamHandle) return Boolean is (Boolean (Stream));
   procedure start (Stream : StreamHandle) is
   begin
      pragma Assert (isValid (Stream));
   end start;
   procedure close (Stream : in out StreamHandle) is
   begin
      Stream := NULL_STREAM;
   end close;
   procedure Reset is
   begin
      Frame_Count := 0;
      Calls := 0;
      Limit := Natural'Last;
      Captured := [others => 0];
   end Reset;
   function write (Stream : StreamHandle; Buffer : System.Address;
                   Frames : Natural) return Natural is
      Accepted : constant Natural := Natural'Min (Frames, Limit);
      Input : Sample_Array with Import, Address => Buffer;
   begin
      pragma Assert (isValid (Stream));
      Calls := Calls + 1;
      if Accepted > 0 then
         for I in 0 .. Accepted * 2 - 1 loop
            Captured (Frame_Count * 2 + I) := Input (I);
         end loop;
      end if;
      Frame_Count := Frame_Count + Accepted;
      return Accepted;
   end write;
end CuBit.Audio;
