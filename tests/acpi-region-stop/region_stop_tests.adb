pragma Ada_2022;
with Ada.Text_IO; with CuBit.Messages; with CuBit.Process_IDs;
with CuBit.Control_Events; with ACPI_System_Events; with ACPI_Region_Stop_Policy;
procedure Region_Stop_Tests is
   package P renames ACPI_Region_Stop_Policy;
   package M renames CuBit.Messages; package IDs renames CuBit.Process_IDs;
   package CV renames CuBit.Control_Events; package E renames ACPI_System_Events;
   use type E.Action, P.Requirement;
   Checks : Natural := 0;
   Facts : P.Evidence := [others => True];
   Item : M.Message := M.NULL_MESSAGE;
   procedure Check (Good : Boolean; Why : String) is
   begin Checks := Checks + 1; if not Good then raise Program_Error with Why; end if; end Check;
   function Accepts (Publisher : IDs.Process_ID; Event : M.Message) return Boolean is
      Local : P.Evidence := Facts;
   begin
      Local (P.Authenticated_Stop) := E.Classify (Publisher, Event) = E.Stop_Requested;
      return P.Allowed (Local);
   end Accepts;
begin
   Check (P.Allowed (Facts) and then not P.First_Missing (Facts).Missing, "all required local facts");
   for Missing in P.Requirement loop
      Facts := [others => True]; Facts (Missing) := False;
      declare Why : constant P.Refusal := P.First_Missing (Facts); begin
         Check (not P.Allowed (Facts) and then Why.Missing and then Why.Item = Missing,
            "each unknown physical/owner/authentication fact prevents exit");
      end;
   end loop;
   Facts := [others => False]; Check (not P.Allowed (Facts), "fresh/unknown owner");
   Facts := [others => True];
   Item.tag.label := CV.Control_Label; Item.tag.length := CV.Control_Words;
   Item.words (0) := CV.Control_Kind'Enum_Rep (CV.Stop);
   Check (Accepts (IDs.No_Process, Item), "actual kernel Stop classification");
   Check (not Accepts (IDs.From_Word (16#1_0000_0001#), Item), "opaque publisher never narrowed");
   Item.tag.protocol := 1; Check (not Accepts (IDs.No_Process, Item), "non-system protocol"); Item.tag.protocol := 0;
   Item.tag.length := 0; Check (not Accepts (IDs.No_Process, Item), "malformed Stop"); Item.tag.length := CV.Control_Words;
   Item.tag.ipc.Accepts_Capability := True; Check (not Accepts (IDs.No_Process, Item), "IPC envelope");
   Item.tag.ipc.Accepts_Capability := False;
   Item.words (0) := CV.Control_Kind'Enum_Rep (CV.Reload);
   Check (not Accepts (IDs.No_Process, Item), "other control is not exit permission");
   Item.words (0) := CV.Control_Kind'Enum_Rep (CV.Stop);
   Facts (P.SCI_Quiesced) := False; Check (not Accepts (IDs.No_Process, Item), "valid Stop cannot erase uncertain SCI");
   Facts (P.SCI_Quiesced) := True; Facts (P.Hardware_Quiet) := False;
   Check (not Accepts (IDs.No_Process, Item), "valid Stop cannot abandon held hardware owner");
   Facts (P.Hardware_Quiet) := True; Facts (P.Backing_Quiet) := False;
   Check (not Accepts (IDs.No_Process, Item), "valid Stop cannot discard pending pool/PCI state");
   Ada.Text_IO.Put_Line ("Region Stop policy checks" & Checks'Image);
end Region_Stop_Tests;
