with ACPI_One_Service;with Ada.Strings.Fixed;
package body ACPI_Native_Log is
   use type ACPI_One_Service.Runtime.Status.Phase;
   procedure Info(Text:String) is
   begin
      if not ACPI_One_Service.Root.Fields.Control_Idle or else
        ACPI_One_Service.Runtime.Current in ACPI_One_Service.Runtime.Status.Fresh|ACPI_One_Service.Runtime.Status.Active then
         raise Program_Error with "logger entered with active/retained Field owner";end if;
      if Count=Maximum_Lines or Text'Length>Maximum_Length then raise Program_Error with "hosted log capacity";end if;
      Count:=Count+1;Lines(Count):=[others=>' '];Lengths(Count):=Text'Length;Lines(Count)(1..Text'Length):=Text;
   end Info;
   procedure Copied(Text:String;Peer_Quiet:Boolean;Success:Boolean:=False) is
      pragma Unreferenced(Success);
   begin
      if not Peer_Quiet then raise Program_Error with "unexpected unsafe copied logger in battery fixture";end if;
      Info(Text);
   end Copied;
   procedure Clear is begin Count:=0;end Clear;
   function Saw(Text:String) return Boolean is
   begin for I in 1..Count loop if Ada.Strings.Fixed.Index(Lines(I)(1..Lengths(I)),Text)>0 then return True;end if;end loop;return False;end Saw;
end ACPI_Native_Log;
