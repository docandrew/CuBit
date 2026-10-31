pragma Ada_2022;
with Ada.Text_IO;
with Interfaces; use Interfaces;
with ACPI_Requests; use ACPI_Requests;
with ACPI_Endpoint;
with ACPI_Service;
with Firmware_Tables;
with Migration_Tables;
procedure Notification_Poll_Tests is
   subtype Bytes is Firmware_Tables.Bytes;
   use type Bytes;
   Checks : Natural := 0;
   Config : constant ACPI_Endpoint.Configuration := (Observer_Tag => 10, Provider_Tag => 11);
   procedure Check (Good : Boolean) is
   begin Checks := Checks + 1; if not Good then raise Program_Error with Checks'Image; end if; end Check;
   function Enc (Text : String) return Bytes is
      B : Bytes (1 .. Text'Length);
   begin for I in B'Range loop B (I) := Character'Pos (Text (Text'First+I-1)); end loop; return B; end Enc;
   function Block (Op, Payload : Bytes) return Bytes is
   begin
      if Payload'Length+1 < 64 then return Op & [Unsigned_8 (Payload'Length+1)] & Payload; end if;
      return Op & [Unsigned_8 (16#40# + (Payload'Length+2) mod 16),
        Unsigned_8 ((Payload'Length+2)/16)] & Payload;
   end Block;
   function Method (Name : String; Code : Bytes) return Bytes is
     (Block ([16#14#],Enc (Name) & [0] & Code));
   Device : constant Bytes := [16#5B#,16#82#,5] & Enc ("DEV0");
   Notify : constant Bytes := [16#86#] & Enc ("DEV0") & [16#0A#,16#80#];
   Wide : constant Bytes := [16#86#] & Enc ("DEV0") & [16#0C#,0,0,1,0];
   procedure Send (S : in out State; Label : Unsigned_32; Data : Words;
      Expected : Outcome; Reply : out Response) is
   begin
      Handle (S, Snapshot_Provider, (Label => Label, Data => Data, others => <>), Reply);
      Check (Reply.Status = Expected and then Reply.Data (0) = Revision (S));
   end Send;
   procedure Read (S : in out State; Label : Unsigned_32; Data : Words;
      Expected : Response_Words) is
      Before : constant State_Model := Model (S) with Ghost;
      Prior : constant Revision_Number := Revision (S);
      Reply : Packet;
   begin
      for Stamp in Unsigned_64 range 10 .. 11 loop
         ACPI_Endpoint.Dispatch (S,Config,Stamp,(Label => Label,Data => Data,others => <>),Reply);
         Check (Reply.Label = ACPI_Endpoint.Reply_OK and then Reply.Data = [Unsigned_64 (Expected (0)), Unsigned_64 (Expected (1)),
           Unsigned_64 (Expected (2)), Unsigned_64 (Expected (3))]);
      end loop;
      ACPI_Endpoint.Dispatch (S,Config,12,(Label => Label,Data => Data,others => <>),Reply);
      Check (Reply.Label = ACPI_Endpoint.Reply_Error and then Reply.Data =
        [Unsigned_64 (Outcome'Pos (Denied)),0,0,0]);
      Check (Revision (S) = Prior);
      pragma Assert (Model (S) = Before);
   end Read;
   procedure Load (S : in out State; Payload : Bytes) is
      Reply : Response;
      Raw : constant Bytes := Migration_Tables.Make (Payload);
   begin
      Send (S,Start_Snapshot,[Revision (S),1,0,0],OK,Reply);
      Import_Block (S,Snapshot_Provider,Revision (S),41,ACPI_Service.DSDT,Raw,Reply);
      Check (Reply.Status = OK);
      Send (S,Finish_Snapshot,[Revision (S),0,0,0],OK,Reply);
   end Load;
begin
   -- Module value effects execute even though queue interest is not enabled
   -- until after module completion. The _INI observation witnesses MARK=1.
   for Indirect in Boolean loop
   declare
      S : State (1, 2048, 2048, Load_Capacity => 1, Initial_Revision => 0);
      Reply : Response;
      Module_Effect : constant Bytes := [16#86#] & Enc ("DEV0") & [16#75#] & Enc ("MARK");
      Ini_Effect : constant Bytes := [16#86#] & Enc ("DEV0") & Enc ("MARK");
   begin
      Send (S,Configure_Notifications,[0,1,0,0],OK,Reply);
      Load (S,Device & [16#08#] & Enc ("MARK") & [0] &
        (if Indirect then Method ("MOD0",Module_Effect & [16#A4#,0]) & Enc ("MOD0")
         else Module_Effect) &
        Method ("_INI",Ini_Effect & [16#A4#,0]));
      Read (S,Read_Notification_State,[0,0,0,0],[Revision(S),3,1,0]);
      Send (S,Claim_Notification,[Revision(S),0,0,0],OK,Reply);
      Read (S,Read_Notification,[1,0,0,0],[Revision(S),0,0,1]);
      Send (S,Acknowledge_Notification,[Revision(S),1,0,0],OK,Reply);
   end;
   end loop;
   -- Disabled notification admission does not bypass target-type validation.
   declare
      S : State (1, 2048, 2048, Load_Capacity => 1, Initial_Revision => 0);
      Reply : Response;
      Raw : constant Bytes := Migration_Tables.Make
        ([16#08#] & Enc ("MARK") & [0,16#86#] & Enc ("MARK") & [1]);
   begin
      Send (S,Start_Snapshot,[0,1,0,0],OK,Reply);
      Import_Block (S,Snapshot_Provider,Revision(S),41,ACPI_Service.DSDT,Raw,Reply);
      Check (Reply.Status=OK);
      Send (S,Finish_Snapshot,[Revision(S),0,0,0],Incomplete,Reply);
      Read (S,Read_Metrics,[Metrics_Module_Error_Page,0,0,0],
        [Revision(S),Unsigned_64 (Startup_Error_Code'Enum_Rep (Error_Unsupported_Value)),0,0]);
      Read (S,Read_Notification_State,[0,0,0,0],[Revision(S),0,0,0]);
   end;
   -- Value evaluation is not skipped when notification interest is disabled.
   declare
      S : State (1, 2048, 2048, Load_Capacity => 1, Initial_Revision => 0);
      Reply : Response;
      Raw : constant Bytes := Migration_Tables.Make
        (Device & [16#86#] & Enc ("DEV0") & [16#78#,1,0,0,0]);
   begin
      Send (S,Start_Snapshot,[0,1,0,0],OK,Reply);
      Import_Block (S,Snapshot_Provider,Revision(S),41,ACPI_Service.DSDT,Raw,Reply);
      Check (Reply.Status=OK);
      Send (S,Finish_Snapshot,[Revision(S),0,0,0],Incomplete,Reply);
      Read (S,Read_Metrics,[Metrics_Module_Error_Page,0,0,0],
        [Revision(S),Unsigned_64 (Startup_Error_Code'Enum_Rep (Error_Division_By_Zero)),0,0]);
      Read (S,Read_Notification_State,[0,0,0,0],[Revision(S),0,0,0]);
   end;
   for Enabled in Boolean loop
      for Later_Error in Boolean loop
         declare
            S : State (1, 2048, 2048, Load_Capacity => 1, Initial_Revision => 0);
            Reply : Response;
            Wire : Packet;
            ID, Prior, Node, Incarnation : Unsigned_64;
         begin
            Read (S,Read_Notification_State,[0,0,0,0],[0,0,0,0]);
            Read (S,Read_Notification_State,[1,0,0,0],[0,8,32,1]);
            Send (S,Acknowledge_Notification,[0,0,0,0],Malformed,Reply);
            Send (S,Claim_Notification,[0,0,0,0],Wrong_Order,Reply);
            if Enabled then
               Send (S,Configure_Notifications,[Revision (S),1,0,0],OK,Reply);
               Check (Reply.Data = [1,1,0,0]);
            end if;
            -- Module Notify is deliberately disabled. Only the two _INI effects
            -- are admitted after Ready, including when a later AML read fails.
            Load (S,Device & Notify & Method ("_INI",Notify & Wide &
              (if Later_Error then [16#A4#] & Enc ("MISS") else [16#A4#,0])));
            Read (S,Read_Notification_State,[0,0,0,0],
              [Revision (S),(if Enabled then 3 else 0),(if Enabled then 2 else 0),0]);
            for Label in Unsigned_32 range Configure_Notifications .. Acknowledge_Notification loop
               if Label /= Read_Notification then
                  Prior := Revision (S);
                  ACPI_Endpoint.Dispatch (S,Config,10,
                    (Label => Label,Data => [Prior,1,0,0],others => <>),Wire);
                  Check (Wire.Label = ACPI_Endpoint.Reply_Error and then Revision (S) = Prior);
               end if;
            end loop;
            if Enabled then
               Prior := Revision (S);
               Send (S,Claim_Notification,[Prior,0,0,0],OK,Reply);
               ID := Reply.Data (1);
               Check (ID = 1 and then Reply.Data = [Prior+1,1,1,1]);
               -- Simulate a lost claim reply: stale revision cannot take another
               -- event, and a resynchronized retry returns the same receipt.
               Send (S,Claim_Notification,[Prior,0,0,0],Stale,Reply);
               Send (S,Claim_Notification,[Revision (S),0,0,0],OK,Reply);
               Check (Reply.Data (1)=ID and then Reply.Data (2)=1);
               Read (S,Read_Notification,[ID,0,0,0],[Revision (S),0,1,16#80#]);
               Handle (S,Observer,(Label=>Read_Notification,Data=>[ID,1,0,0],others=><>),Reply);
               Check (Reply.Status=OK and then Reply.Data(1)>0 and then Reply.Data(2)>0 and then Reply.Data(3)=1);
               Node:=Reply.Data(1);Incarnation:=Reply.Data(2);
               Read (S,Read_Notification,[ID,2,0,0],[Revision (S),16#30564544#,1,0]);
               Send (S,Configure_Notifications,[Revision (S),0,0,0],OK,Reply);
               Read (S,Read_Notification_State,[0,0,0,0],[Revision (S),0,1,ID]);
               Read (S,Read_Notification,[ID,1,0,0],[Revision (S),Node,Incarnation,1]);
               Send (S,Read_Notification,[ID,2,1,0],Index_Out_Of_Range,Reply);
               Send (S,Read_Notification,[ID,3,0,0],Malformed,Reply);
               Send (S,Read_Notification,[ID,0,0,1],Malformed,Reply);
               Prior:=Revision(S);
               Send (S,Acknowledge_Notification,[Prior,ID,0,0],OK,Reply);
               Send (S,Acknowledge_Notification,[Prior,ID,0,0],OK,Reply);
               Check (Revision(S)=Prior+1);
               Send (S,Claim_Notification,[Revision(S),0,0,0],OK,Reply);
               Check (Reply.Data(1)=2 and then Reply.Data(2)=0);
               Prior:=Revision(S);
               Send (S,Acknowledge_Notification,[0,ID,0,0],OK,Reply);
               Check (Revision(S)=Prior);
               Read (S,Read_Notification,[2,0,0,0],[Prior,0,1,0]);
               Send (S,Read_Notification,[ID,0,0,0],Stale,Reply);
               Send (S,Acknowledge_Notification,[Prior,0,0,0],Malformed,Reply);
               Send (S,Acknowledge_Notification,[Prior,3,0,0],Stale,Reply);
               Send (S,Acknowledge_Notification,[Prior,2,0,0],OK,Reply);
               Send (S,Acknowledge_Notification,[Revision(S),ID,0,0],Stale,Reply);
            end if;
            Prior:=Revision(S);
            Send (S,Claim_Notification,[Prior,0,0,0],OK,Reply);
            Check (Reply.Data=[Prior,0,0,0]);
            Send (S,Read_Notification_State,[2,0,0,0],Malformed,Reply);
         end;
      end loop;
   end loop;
   declare
      S : State (1, 2048, 2048, Load_Capacity => 1, Initial_Revision => Max_Revision-4);
      Reply : Response;
   begin
      Send (S,Configure_Notifications,[Revision(S),1,0,0],OK,Reply);
      Load (S,Device & Method ("_INI",Notify & [16#A4#,0]));
      Check (Revision(S)=Max_Revision);
      Send (S,Claim_Notification,[Max_Revision,0,0,0],Resource_Limit,Reply);
      Read (S,Read_Notification_State,[0,0,0,0],[Max_Revision,3,1,0]);
      Send (S,Configure_Notifications,[Max_Revision,0,0,0],Resource_Limit,Reply);
      Read (S,Read_Notification_State,[0,0,0,0],[Max_Revision,3,1,0]);
   end;
   declare
      S : State (1, 2048, 2048, Load_Capacity => 1, Initial_Revision => 0);
      Reply : Response;
   begin
      Send (S,Configure_Notifications,[0,1,0,0],OK,Reply);
      Send (S,Start_Snapshot,[Revision(S),1,0,0],OK,Reply);
      Send (S,Finish_Snapshot,[Revision(S),0,0,0],Incomplete,Reply);
      Read (S,Read_Notification_State,[0,0,0,0],[Revision(S),1,0,0]);
      Send (S,Configure_Notifications,[Revision(S),0,0,0],Wrong_Order,Reply);
      Send (S,Claim_Notification,[Revision(S),0,0,0],Wrong_Order,Reply);
   end;
   declare
      S : State (1, 4096, 4096, Load_Capacity => 1, Initial_Revision => 0);
      Reply : Response;
      Many : Bytes (1 .. Notify'Length * 33);
   begin
      for I in 0 .. 32 loop Many (I*Notify'Length+1 .. (I+1)*Notify'Length) := Notify; end loop;
      Send (S,Configure_Notifications,[0,1,0,0],OK,Reply);
      Load (S,Device & Method ("_INI",Many & [16#A4#,0]));
      Read (S,Read_Notification_State,[0,0,0,0],[Revision(S),3,32,0]);
      Handle (S,Observer,(Label=>Read_Metrics,Data=>[Metrics_Device_Error_Page,0,0,0],others=><>),Reply);
      Check (Reply.Status=OK and then Reply.Data(1)=38); -- Notify_Limit
      for ID in Unsigned_64 range 1 .. 32 loop
         Send (S,Claim_Notification,[Revision(S),0,0,0],OK,Reply);
         Check (Reply.Data(1)=ID and then Reply.Data(2)=32-ID);
         Send (S,Acknowledge_Notification,[Revision(S),ID,0,0],OK,Reply);
      end loop;
      Read (S,Read_Notification_State,[0,0,0,0],[Revision(S),3,0,0]);
   end;
   declare
      S : State (1, 2048, 2048, Load_Capacity => 1, Initial_Revision => 0);
      Reply : Response;
      Local_Device : constant Bytes := [16#5B#,16#82#,5] & Enc ("TMP0");
   begin
      Send (S,Configure_Notifications,[0,1,0,0],OK,Reply);
      Load (S,Method ("_INI",Local_Device & [16#86#] & Enc ("TMP0") & [1,16#A4#,0]));
      Send (S,Claim_Notification,[Revision(S),0,0,0],OK,Reply);
      Check (Reply.Data(1)=1 and then Reply.Data(3)=2);
      Handle (S,Observer,(Label=>Read_Notification,Data=>[1,1,0,0],others=><>),Reply);
      Check (Reply.Status=OK and then Reply.Data(1)>0 and then Reply.Data(2)>0 and then Reply.Data(3)=0);
      Read (S,Read_Notification,[1,2,0,0],[Revision(S),16#494E495F#,2,0]);
      Read (S,Read_Notification,[1,2,1,0],[Revision(S),16#30504D54#,2,0]);
      Send (S,Acknowledge_Notification,[Revision(S),1,0,0],OK,Reply);
   end;
   Ada.Text_IO.Put_Line ("Notification polling checks" & Checks'Image);
end Notification_Poll_Tests;
