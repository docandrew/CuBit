package body Watch_Reserve with SPARK_Mode is

   procedure Decide
     (State : in out Watch_State; Normal : in out Normal_Count; Free : Free_Bytes;
      Payload : FE.Record_Length; Read : Boolean; Result : out Decision) is
   begin
      if State = Rescan_Posted and then Read then
         if May_Admit (Free, Normal) then
            State := Watching;
            Normal := Normal + 1;
         else
            --  The client rescanned already: this event needs a new record.
            State := Rescan_Owed;
         end if;
      end if;
      case State is
         when Watching =>
            if Free >= Worst (Payload) and then Free - Worst (Payload) >= Reserve_Each * Normal then
               Result := Put_Event;
            else
               State := Rescan_Posted;
               Normal := Normal - 1;
               Result := Put_Rescan;
            end if;
         when Unused | Rescan_Posted | Rescan_Owed | End_Owed | Ended =>
            Result := Drop;   --  (Live_State only: Rescan_Posted, Rescan_Owed)
      end case;
   end Decide;

   procedure Settle
     (State : in out Watch_State; Normal : in out Normal_Count; Free : Free_Bytes;
      Read : Boolean; Put : out Boolean) is
   begin
      Put := False;
      case State is
         when Rescan_Posted =>
            if Read and then May_Admit (Free, Normal) then
               State := Watching;
               Normal := Normal + 1;
            end if;
         when Rescan_Owed | End_Owed =>
            if Free >= Reserve_Each * (Normal + 1) then
               State := (if State = Rescan_Owed then Rescan_Posted else Ended);
               Put := True;
            end if;
         when Unused | Watching | Ended =>
            null;
      end case;
   end Settle;

   procedure End_Watch
     (State : in out Watch_State; Normal : in out Normal_Count; Free : Free_Bytes;
      Put : out Boolean) is
   begin
      if State = Watching then
         Normal := Normal - 1;
         Put := True;
      else
         Put := Free >= Reserve_Each * (Normal + 1);
      end if;
      State := (if Put then Ended else End_Owed);
   end End_Watch;

   procedure Force_Rescan
     (State : in out Watch_State; Normal : in out Normal_Count; Free : Free_Bytes;
      Read : Boolean; Put : out Boolean) is
   begin
      Put := False;
      case State is
         when Watching =>
            State := Rescan_Posted;
            Normal := Normal - 1;
            Put := True;
         when Rescan_Posted =>
            if Read then
               State := Rescan_Owed;
            end if;
         when Unused | Rescan_Owed | End_Owed | Ended =>
            null;
      end case;
   end Force_Rescan;

end Watch_Reserve;
