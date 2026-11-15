pragma Ada_2022;
with Ada.Command_Line;with Ada.Text_IO;with Interfaces;
with CuBit.Messages;with CuBit.Process_IDs;with CuBit.Control_Events;with CuBit.Channel_Bells;
with ACPI_Native_Events;with ACPI_Native_Log;
procedure Stop_Tests is
 package M renames CuBit.Messages;package P renames CuBit.Process_IDs;package CV renames CuBit.Control_Events;
 use type Interfaces.Unsigned_32,Interfaces.Unsigned_8,Interfaces.Unsigned_16,M.Protocol_Flags,CV.Control_Kind;Checks:Natural:=0;
 Owners:constant String:="core=RETAINED_REGIONS sci=RETAINED_SCI runtime=STOPPED";
 procedure Check(OK:Boolean;Why:String) is begin Checks:=Checks+1;if not OK then raise Program_Error with Why;end if;end;
 function Stop_Message return M.Message is
 X:M.Message:=M.NULL_MESSAGE;begin X.tag.label:=CV.Control_Label;X.tag.length:=CV.Control_Words;
 X.words(0):=CV.Control_Kind'Enum_Rep(CV.Stop);X.words(1):=Interfaces.Unsigned_64'Last;return X;end;
 procedure Filter(X:M.Message;Publisher:P.Process_ID:=P.No_Process) is
 begin ACPI_Native_Events.Note_Retained_Stop(Publisher,X,Owners);
 Check(ACPI_Native_Log.Attempts=0,"non-Stop/unauthenticated envelope must not consume or publish the one note");
 Check(CuBit.Channel_Bells.Calls=0 and ACPI_Native_Log.Grant_Notices=0,"retained observation must not dispatch channel/grant work");end;
 Item:M.Message;Stopped,Handled:Boolean;
begin
 ACPI_Native_Log.Shed:=Ada.Command_Line.Argument(1)="shed";
 for Length in Interfaces.Unsigned_8 loop if Length/=CV.Control_Words then Item:=Stop_Message;Item.tag.length:=Length;Filter(Item);end if;end loop;
 for Protocol in M.Protocol_Flags loop if Protocol/=0 then Item:=Stop_Message;Item.tag.protocol:=Protocol;Filter(Item);end if;end loop;
 for Accepts in Boolean loop for Transferred in Boolean loop for Refused in Boolean loop for Offered in Boolean loop
 if Accepts or Transferred or Refused or Offered then Item:=Stop_Message;
 Item.tag.ipc:=(Accepts_Capability=>Accepts,Capability_Transferred=>Transferred,Capability_Refused=>Refused,Capability_Offered=>Offered);Filter(Item);end if;
 end loop;end loop;end loop;end loop;
 for Reserved in Interfaces.Unsigned_16 loop if Reserved/=0 then Item:=Stop_Message;Item.tag.reserved:=Reserved;Filter(Item);end if;end loop;
 for Kind in CV.Control_Kind loop if Kind/=CV.Stop then Item:=Stop_Message;Item.words(0):=CV.Control_Kind'Enum_Rep(Kind);Filter(Item);end if;end loop;
 Item:=Stop_Message;Item.words(0):=0;Filter(Item);Item.words(0):=Interfaces.Unsigned_64'Last;Filter(Item);
 for High in 0..63 loop Item:=Stop_Message;Filter(Item,P.From_Word(Interfaces.Shift_Left(Interfaces.Unsigned_64(1),High)));end loop;
 for Label in Interfaces.Unsigned_32 range 0..16#200# loop if Label/=CV.Control_Label then Item:=Stop_Message;Item.tag.label:=Label;Filter(Item);end if;end loop;
 Item:=Stop_Message;ACPI_Native_Events.Handle(P.No_Process,Item,Stopped,Handled);
 Check(Stopped and Handled and ACPI_Native_Log.Attempts=0,"normal serving Stop remains unchanged");
 ACPI_Native_Events.Note_Retained_Stop(P.No_Process,Item,Owners);
 Check(ACPI_Native_Log.Attempts=1 and not ACPI_Native_Log.Last_Quiet,"authenticated Stop uses established-ring-only publication once");
 Check(ACPI_Native_Log.Records=(if ACPI_Native_Log.Shed then 0 else 1) and ACPI_Native_Log.Dropped=(if ACPI_Native_Log.Shed then 1 else 0),"bounded recorder accepts or sheds exactly once");
 if not ACPI_Native_Log.Shed then Check(ACPI_Native_Log.Text(1..ACPI_Native_Log.Last)="acpi: stop refused RETAINED "&Owners,"copied owner facts retained exactly");end if;
 for I in 1..1000 loop ACPI_Native_Events.Note_Retained_Stop(P.No_Process,Item,Owners);
 Check(ACPI_Native_Log.Attempts=1,"repeated Stop never retries an accepted or shed note");end loop;
 Check(CuBit.Channel_Bells.Calls=0 and ACPI_Native_Log.Grant_Notices=0,"retained Stop never dispatches work");
 Ada.Text_IO.Put_Line("Retained Stop checks"&Checks'Image);
end Stop_Tests;