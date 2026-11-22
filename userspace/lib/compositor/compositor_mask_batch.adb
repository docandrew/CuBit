package body Compositor_Mask_Batch with SPARK_Mode is
   procedure Append (P : in out Packet; Value : Command; Accepted : out Boolean) is
   begin
      Accepted := P.Length < Maximum and then Value.Description.Over = 1 and then
        A.Valid (Value.Description, P.Width, P.Height);
      if Accepted then
         P.Length := P.Length + 1;
         P.Items (P.Length) := Value;
      end if;
   end Append;
end Compositor_Mask_Batch;
