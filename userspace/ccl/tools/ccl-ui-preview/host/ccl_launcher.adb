--  The Linux preview starts no programs: a Linux-hosted stand-in, not
--  CuBit. Program interfaces appear only where CuBit's procmgr describes
--  them (native/ccl_launcher.adb).
package body CCL_Launcher is
   procedure Programs (Items : out Program_Array; Count : out Program_Count) is
   begin
      Items := [others => (others => <>)];
      Count := 0;
   end Programs;

   procedure Start
     (Name : String; Description : PD.Signature; V : PD.Values;
      Started_As : out Run; Result : out Start_Result;
      Why : out String; Why_Length : out Natural)
   is
      pragma Unreferenced (Name, Description, V);
      Text : constant String := "the Linux preview starts no CuBit programs";
   begin
      Started_As := (others => <>);
      Result := Not_Available;
      Why := [others => ' '];
      Why (Why'First .. Why'First + Text'Length - 1) := Text;
      Why_Length := Text'Length;
   end Start;

   procedure Poll (Item : Run; Ended : out Boolean; Code : out Interfaces.Integer_64) is
      pragma Unreferenced (Item);
   begin
      Ended := True;
      Code := 0;
   end Poll;

   procedure Release (Item : Run) is null;
end CCL_Launcher;
