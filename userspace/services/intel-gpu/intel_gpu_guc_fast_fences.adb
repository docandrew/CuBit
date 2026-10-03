package body Intel_GPU_GuC_Fast_Fences with SPARK_Mode is
   function Failed (Object : Stream) return Boolean is (Object.Broken);
   function Pending (Object : Stream) return Boolean is (Object.Sending);
   procedure Fail (Object : in out Stream) is
   begin
      Object.Broken := True;
   end Fail;
   procedure Prepare
     (Object : in out Stream; Fence : out Unsigned_16; Accepted : out Boolean) is
   begin
      Fence := 0; Accepted := False;
      if Object.Broken or Object.Sending then return; end if;
      Object.Sending := True;
      Fence := Object.Next_ID;
      Accepted := True;
   end Prepare;
   procedure Sent (Object : in out Stream; Result : Outcome) is
   begin
      if Object.Broken then return; end if;
      if not Object.Sending then Fail (Object); return; end if;
      Object.Sending := False;
      case Result is
         when Not_Published => null;
         when Published =>
            Object.Next_ID := (if Object.Next_ID = Unsigned_16'Last
                               then 16#8000# else Object.Next_ID + 1);
         when Uncertain => Fail (Object);
      end case;
   end Sent;
   procedure Reject_Response (Object : in out Stream; Fence : Unsigned_16) is
   begin
      if Is_Fast (Fence) then Fail (Object); end if;
   end Reject_Response;
end Intel_GPU_GuC_Fast_Fences;
