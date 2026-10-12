--  Software-only Desktop: no GPU queue, so no completion event to arm and no
--  blocking calls to report.
package body Desktop_GPU_Wake_Hook is
   function Arm (Token : Interfaces.Unsigned_64) return Arm_Result is
      pragma Unreferenced (Token);
   begin
      return Unavailable;
   end Arm;
   function Collect (Receipt : CuBit.Messages.CompletionEntry) return Boolean is
      pragma Unreferenced (Receipt);
   begin
      return False;
   end Collect;
   function Boost_Hint return Boolean is (False);
   function Boost_Policy (On : Boolean) return Boolean is (True);
   procedure Blocking_Report (Text : out String; Last : out Natural) is
   begin
      Text := [others => ' '];
      Last := Text'First - 1;
   end Blocking_Report;
   procedure Job_Report (Text : out String; Last : out Natural) is
   begin
      Text := [others => ' '];
      Last := Text'First - 1;
   end Job_Report;
end Desktop_GPU_Wake_Hook;
