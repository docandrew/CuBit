--  The Linux preview has no llm.svc: a Linux-hosted stand-in, not CuBit.
--  Asks are refused as unavailable; nothing reaches any network.
package body CCL_Llm is
   procedure Ask
     (Model, Mission, System, Tools, Conversation : String;
      Cache, Capture : Boolean;
      Max_Tokens, Deadline_Ms : Positive;
      Ceiling : Ceiling_Kind; Ceiling_Nano_Usd : Interfaces.Unsigned_64;
      Exchange : out Interfaces.Unsigned_64; Result : out Ask_Result;
      Why : out String; Why_Length : out Natural)
   is
      pragma Unreferenced (Model, Mission, System, Tools, Conversation, Cache, Capture, Max_Tokens,
                           Deadline_Ms, Ceiling, Ceiling_Nano_Usd);
      Text : constant String := "the Linux preview has no llm.svc";
   begin
      Exchange := 0;
      Result := Unavailable;
      Why := [others => ' '];
      Why_Length := Natural'Min (Text'Length, Why'Length);
      Why (Why'First .. Why'First + Why_Length - 1) := Text (Text'First .. Text'First + Why_Length - 1);
   end Ask;

   procedure Take_Text (Exchange : Interfaces.Unsigned_64; Into : out String; Last : out Natural) is
      pragma Unreferenced (Exchange);
   begin
      Into := [others => ' '];
      Last := Into'First - 1;
   end Take_Text;

   procedure Take_Summary
     (Exchange : Interfaces.Unsigned_64; Result : out Summary;
      Content : out Text_Access; Ended : out Boolean)
   is
      pragma Unreferenced (Exchange);
   begin
      Result := (others => <>);
      Content := null;
      Ended := False;
   end Take_Summary;

   procedure Cancel (Exchange : Interfaces.Unsigned_64; Done : out Boolean) is
      pragma Unreferenced (Exchange);
   begin
      Done := False;
   end Cancel;

   function Following (Exchange : Interfaces.Unsigned_64) return Boolean is (False);
end CCL_Llm;
