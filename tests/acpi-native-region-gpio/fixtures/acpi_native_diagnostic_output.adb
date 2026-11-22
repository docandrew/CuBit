pragma Ada_2022;
with Prep_Model;with Ada.Strings.Fixed;
package body ACPI_Native_Diagnostic_Output is
 procedure Write(Text:String;Peer_Quiet:Boolean;Success:Boolean:=False) is
  pragma Unreferenced(Success);
 begin
 if Ada.Strings.Fixed.Index(Text,"stage region-unavailable ")>0 then
  Prep_Model.Unavailable_Line_Seen:=True;
  Prep_Model.Unavailable_Lines:=Prep_Model.Unavailable_Lines+1;
  Prep_Model.Unavailable_CMOS_Line:=Prep_Model.Unavailable_CMOS_Line or else
    (Ada.Strings.Fixed.Index(Text,"space=5")>0 and then Ada.Strings.Fixed.Index(Text,"base=0x0000000000000000")>0
     and then Ada.Strings.Fixed.Index(Text,"bytes=63")>0 and then Ada.Strings.Fixed.Index(Text,"fail UNSUPPORTED_SPACE")>0);
  Prep_Model.Unavailable_Path_Seen:=Prep_Model.Unavailable_Path_Seen or else Ada.Strings.Fixed.Index(Text,"HOLD.UNAV.RTCM")>0;
 end if;
 if Ada.Strings.Fixed.Index(Text,"region-unavailable-summary ok count=9 captured=8 omitted=1")>0 then
  Prep_Model.Unavailable_Limited_Summary:=True;
 end if;
 if Peer_Quiet then Prep_Model.Diagnostic_Open_Requests:=Prep_Model.Diagnostic_Open_Requests+1;end if;
 Prep_Model.Diagnostic_Calls:=Prep_Model.Diagnostic_Calls+1;end;
end ACPI_Native_Diagnostic_Output;
