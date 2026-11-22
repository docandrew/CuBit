pragma Ada_2022;
with CuBit.Input;
package body HID_Touchpad_Publication with SPARK_Mode is
   package body Publisher is
      package PID renames CuBit.Process_IDs;
      package I renames CuBit.Input;
      use type PID.Process_ID;
      Source_Version : constant I.Source_Generation := 1;
      procedure Flush (Now : Input_Pending.Word; Result : out Report) is
         Target : constant PID.Process_ID := Consumer;
         Item : Input_Pending.Item;
         Data : I.Source_Report;
      begin
         Result := (others => <>);
         if Target /= Current_Consumer then
            Current_Consumer := Target; App.Consumer_Changed (Now); Result.Consumer_Changed := True;
         end if;
         Result.Consumer_Identity := Current_Consumer;
         Result.No_Consumer := Current_Consumer = PID.No_Process;
         if not Result.No_Consumer then
            for Attempt in 1 .. Maximum_Per_Turn loop
               exit when App.Count = 0;
               Item := App.Head;
               Data := (sourceIdentity => 0, sequence => Item.Sequence,
                  generation => Source_Version, device => I.RELATIVE_POINTER,
                  delivery => I.ACCUMULABLE_DISPLACEMENT, flags => [I.RESYNCHRONIZE => Item.Recover],
                  payload => Item.Payload, snapshot => I.Pointer_Snapshot (Item.Payload, Item.Observed_Ms));
               if not Try_Publish (Current_Consumer, I.Encode (Data)) then Result.Blocked := True; exit; end if;
               App.Acknowledge; Result.Accepted := Result.Accepted + 1;
            end loop;
         end if;
         Result.Pending := App.Count;
      end Flush;
   end Publisher;
end HID_Touchpad_Publication;
