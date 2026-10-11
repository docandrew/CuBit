package body Clipboard_Gestures with SPARK_Mode is
   procedure Issue
     (S : in out State; Kind : Gesture; Surface : Live_Surface;
      Now : Millisecond) is
   begin
      S (Kind) := (True, Surface, Now);
   end Issue;

   procedure Redeem
     (S : in out State; Kind : Gesture; Surface : Surface_Name;
      Now : Millisecond; Granted : out Boolean) is
   begin
      Granted := Fresh (S (Kind), Surface, Now);
      if Granted then
         S (Kind) := (others => <>);
      end if;
   end Redeem;

   procedure Revoke (S : out State) is
   begin
      S := [others => (others => <>)];
   end Revoke;
end Clipboard_Gestures;
