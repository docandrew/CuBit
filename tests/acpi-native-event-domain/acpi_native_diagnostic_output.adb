with ACPI_One_Service;with Ada.Strings.Fixed;
package body ACPI_Native_Diagnostic_Output is
   use type ACPI_One_Service.Runtime.Status.Phase;
   Maximum_Lines : constant Positive := 256;
   Maximum_Length : constant Positive := 512;
   subtype Line is String(1..Maximum_Length);
   Count : Natural := 0;
   Lines : array(1..Maximum_Lines) of Line := [others => [others => ' ']];
   Lengths : array(1..Maximum_Lines) of Natural := [others => 0];
   function Latest return String is (if Count=0 then "" else Lines(Count)(1..Lengths(Count)));
   function Saw(Text : String) return Boolean is
   begin
      for I in 1..Count loop
         if Ada.Strings.Fixed.Index(Lines(I)(1..Lengths(I)),Text)>0 then return True;end if;
      end loop;
      return False;
   end Saw;
   procedure Write(Text:String;Peer_Quiet:Boolean;Success:Boolean:=False) is
      pragma Unreferenced(Success);
   begin
      if Peer_Quiet and then (not ACPI_One_Service.Root.Fields.Control_Idle or else
         ACPI_One_Service.Runtime.Current in ACPI_One_Service.Runtime.Status.Fresh | ACPI_One_Service.Runtime.Status.Active) then
         raise Program_Error with "diagnostic permitted channel opening during active ownership";
      end if;
      if Count=Maximum_Lines or Text'Length>Maximum_Length then raise Program_Error with "diagnostic fixture capacity";end if;
      Count:=Count+1;Lengths(Count):=Text'Length;
      Lines(Count)(1..Text'Length):=Text;
   end Write;
end ACPI_Native_Diagnostic_Output;
