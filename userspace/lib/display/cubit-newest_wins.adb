package body CuBit.Newest_Wins with SPARK_Mode is
   procedure Want (S : in out State; P : Position) is
   begin
      S.Want_P := P;
   end Want;

   procedure Submitted (S : in out State) is
   begin
      S.Sent_P := S.Want_P;
      S.Flight := True;
   end Submitted;

   procedure Completed (S : in out State) is
   begin
      S.Flight := False;
   end Completed;

   procedure Resend (S : in out State) is
   begin
      --  Any value other than the wanted one.
      S.Sent_P := (X => (if S.Want_P.X = Integer'Last then 0 else S.Want_P.X + 1),
                   Y => S.Want_P.Y);
   end Resend;
end CuBit.Newest_Wins;
