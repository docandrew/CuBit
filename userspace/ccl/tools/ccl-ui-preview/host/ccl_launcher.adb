--  The Linux preview starts no programs: a Linux-hosted stand-in, not
--  CuBit. Program interfaces appear only where CuBit's procmgr describes
--  them (native/ccl_launcher.adb).
package body CCL_Launcher is
   procedure Programs (Items : out Program_Array; Count : out Program_Count; More : out Boolean) is
   begin
      Items := [others => (others => <>)];
      Count := 0;
      More := False;
   end Programs;

   procedure Check (Name : String; Item : out Program) is
      Text : constant String := "the Linux preview starts no CuBit programs";
   begin
      Item := (others => <>);
      Item.Length := Natural'Min (Name'Length, MAXIMUM_NAME);
      Item.Name (1 .. Item.Length) := Name (Name'First .. Name'First + Item.Length - 1);
      Item.State := Refused;
      Item.Why (1 .. Text'Length) := Text;
      Item.Why_Length := Text'Length;
   end Check;

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

   procedure Poll (Item : Run; Ended : out Boolean; How : out Ending) is
      pragma Unreferenced (Item);
   begin
      Ended := True;
      How := (others => <>);
   end Poll;

   procedure Release (Item : Run) is null;
end CCL_Launcher;
