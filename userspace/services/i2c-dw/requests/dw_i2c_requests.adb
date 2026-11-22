pragma Ada_2022;
with I2C_Channel_Badges;
with Intel_LPSS_I2C_Profile;
with DW_I2C_Configuration;
package body DW_I2C_Requests with SPARK_Mode is
   use type T.Count;
   use type T.Ticket;
   use type Controller.Phase;
   use type Controller.Admission;
   use type Controller.Collection;
   use type Q.Copy_Result;
   package B renames I2C_Channel_Badges;
   package P renames Intel_LPSS_I2C_Profile;
   use type B.Decode_Status;
   use type B.Address_Mode;
   use type B.Hertz;
   function Observe (S : State) return Report is (S.Info);
   procedure Send
     (S : in out State; Reply : Reply_Authority; Item : Q.Response) is
   begin
      S.Info.Last_Reply := Item;
      Answer (Reply, Item, S.Info.Last_Reply_Delivered);
   end Send;
   procedure Submit
     (S : in out State; Device : in out Controller.State;
      Caller : Caller_ID; Badge : Interfaces.Unsigned_64;
      Reply : Reply_Authority; Item : Request) is
      Response : Q.Response;
      Copy : Q.Copy_Result := Q.Copied;
      Channel : constant B.Decoding := B.Decode (Badge);
   begin
      if not Authorized (Caller, Badge) then Response.Status := Q.Unauthorized;
      elsif Channel.Status /= B.Decoded then Response.Status := Q.Invalid_Channel;
      elsif Channel.Value.Mode /= B.Seven_Bit or else
        Channel.Value.Maximum_Hz > B.Hertz (P.Fast_Plus_Maximum)
      then Response.Status := Q.Unsupported_Channel;
      elsif S.Info.Active or else S.Info.Faulted or else
        Controller.Observe (Device).At_Phase /= Controller.Available
      then Response.Status := Q.Busy;
      elsif Item.Write_Count > Capacity or else Item.Read_Count > Capacity - Item.Write_Count or else
        (Item.Write_Count = 0 and then Item.Read_Count = 0)
      then Response.Status := Q.Invalid_Size;
      else
         S.Buffer := [others => 0];
         if Item.Write_Count > 0 then
            Copy_Input (Caller, Item.Input, S.Buffer (1 .. Item.Write_Count), Copy);
         end if;
         if Copy /= Q.Copied then
            Response.Status := (if Copy = Q.Refused then Q.Input_Refused else Q.Input_Uncertain);
            S.Info.Faulted := Copy = Q.Uncertain;
         else
            Controller.Submit (Device,
               DW_I2C_Configuration.Address (Channel.Value.Address),
               P.Hertz (Channel.Value.Maximum_Hz),
               S.Buffer (1 .. Item.Write_Count), Item.Read_Count,
               Item.Deadline, Item.Step_Budget, S.Ticket, S.Info.Admission);
            if S.Info.Admission = Controller.Accepted then
               S.Held_Caller := Caller; S.Held_Output := Item.Output; S.Held_Reply := Reply;
               S.Expected_Reads := Item.Read_Count; S.Info.Active := True;
               S.Buffer := [others => 0]; return;
            end if;
            Response.Status := Q.Admission_Refused;
         end if;
         S.Buffer := [others => 0];
      end if;
      Send (S, Reply, Response);
   end Submit;
   procedure Advance (S : in out State; Device : in out Controller.State) is
      Item : Controller.Completion;
      Required : T.Count;
      Collected : Controller.Collection;
      Copy : Q.Copy_Result;
      Response : Q.Response;
      Device_Report : Controller.Report;
   begin
      if not S.Info.Active then return; end if;
      Controller.Advance (Device);
      Device_Report := Controller.Observe (Device);
      if not Device_Report.Completion_Pending then return; end if;
      S.Info.Failure := Device_Report.Reason;
      Controller.Take_Completion (Device, S.Buffer, Item, Required, Collected);
      Response := (Status => Q.Completed, ID => Item.ID,
         Hardware_Succeeded => Item.Successful, Effects_Possible => Item.Effects_Possible,
         Write_Count => Item.Write_Count, Read_Count => 0);
      S.Info.Faulted := Device_Report.At_Phase = Controller.Faulted;
      if Collected /= Controller.Collected or else Item.ID /= S.Ticket or else
        (Item.Successful and then Required /= S.Expected_Reads)
      then
         Response.Status := Q.Internal_Mismatch; S.Info.Faulted := True;
         Response.Hardware_Succeeded := False;
      elsif Item.Successful and then Required > 0 then
         Copy_Output (S.Held_Caller, S.Held_Output, S.Buffer (1 .. Required), Copy);
         case Copy is
            when Q.Copied => Response.Read_Count := Required;
            when Q.Refused => Response.Status := Q.Output_Refused;
            when Q.Uncertain => Response.Status := Q.Output_Uncertain; S.Info.Faulted := True;
         end case;
      end if;
      S.Buffer := [others => 0]; S.Info.Active := False;
      S.Ticket := T.No_Ticket; S.Expected_Reads := 0;
      Send (S, S.Held_Reply, Response);
   end Advance;
end DW_I2C_Requests;
