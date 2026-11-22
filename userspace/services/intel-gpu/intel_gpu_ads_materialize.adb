with Intel_GPU_ADS_Layout; use Intel_GPU_ADS_Layout;
package body Intel_GPU_ADS_Materialize with SPARK_Mode is
   procedure Write
     (Image : Intel_GPU_ADS_Initialization.Prepared_Image;
      Buffer : in out Bytes; Success : out Boolean)
   is
      L : Layout renames Image.Layout;
      function Fits (S : Section; Length : Natural) return Boolean is
        (Length > 0 and then Buffer'Length > 0 and then
         Unsigned_64 (Length) <= L.Bytes (S) and then
         L.Offset (S) <= Unsigned_64 (Natural'Last) and then
         Natural (L.Offset (S)) <= Buffer'Last - Buffer'First and then
         Length - 1 <= Buffer'Last - Buffer'First - Natural (L.Offset (S)));
      procedure Copy (Offset : Unsigned_64; Source : Bytes)
        with Pre =>
          Source'Length > 0 and then Buffer'Length > 0 and then
          Offset <= Unsigned_64 (Natural'Last) and then
          Natural (Offset) <= Buffer'Last - Buffer'First and then
          Source'Last - Source'First <=
            Buffer'Last - Buffer'First - Natural (Offset)
      is
         First : constant Natural := Buffer'First + Natural (Offset);
      begin
         for I in Source'Range loop
            Buffer (First + (I - Source'First)) := Source (I);
         end loop;
      end Copy;
   begin
      Success := False;
      if not Image.Valid or else not L.Valid or else not Sound (L) or else
        L.Total > Unsigned_64 (Buffer'Length) or else
        not Image.System_Info.Valid or else not Image.Registers.Valid or else
        not Image.Capture.Valid or else Image.Policies (76) /= 1 or else
        not Fits (Header, Image.Header'Length) or else
        not Fits (Policies, Image.Policies'Length) or else
        not Fits (System_Info, Image.System_Info.Bytes'Length) or else
        not Fits (Registers, Image.Registers.Registers'Length) or else
        not Fits (Capture, Image.Capture.Data'Length)
      then return; end if;
      Buffer := [others => 0];
      Copy (L.Offset (Header), Bytes (Image.Header));
      Copy (L.Offset (Policies), Bytes (Image.Policies));
      Copy (L.Offset (System_Info), Bytes (Image.System_Info.Bytes));
      Copy (L.Offset (Registers), Bytes (Image.Registers.Registers));
      Copy (L.Offset (Capture), Bytes (Image.Capture.Data));
      Success := True;
   end Write;
end Intel_GPU_ADS_Materialize;
