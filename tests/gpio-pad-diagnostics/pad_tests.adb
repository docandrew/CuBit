pragma Ada_2022;
with Ada.Text_IO;
with Intel_ADLN_GPIO;
with Intel_GPIO_IRQ;
with GPIO_Pad_Diagnostics;
procedure Pad_Tests is
   package G renames Intel_ADLN_GPIO;
   package I renames Intel_GPIO_IRQ;
   use type G.Plan_Result, G.Outcome, G.Pad_Number, I.Access_Status, I.Word, I.Read_ID;
   Plan : constant G.Plan_Result:=G.Plan(G.Target_HID,209,16#0096_0000#,16#700#,16#10000#);
   Checks,Calls:Natural:=0;
   Fail_At:Natural range 0..4:=0;
   Failure:I.Access_Status:=I.Denied;
   procedure Check(OK:Boolean;Why:String) is
   begin Checks:=Checks+1;if not OK then raise Program_Error with Why;end if;end Check;
   procedure Read(Selected:G.Plan_Result;ID:I.Read_ID;Value:out I.Word;Status:out I.Access_Status) is
      IDs:constant array(Positive range 1..4)of I.Read_ID:=[I.Host_Owner,I.Pad_Configuration,I.Group_Status,I.Group_Enable];
   begin
      Calls:=Calls+1;Check(Calls<=4,"bounded callback count");
      Check(Selected=Plan and then ID=IDs(Calls),"exact admitted plan and distinct register IDs");
      Value:=I.Word(Calls)*111;Status:=(if Calls=Fail_At then Failure else I.Access_OK);
   end Read;
   package D is new GPIO_Pad_Diagnostics(Read);
   Result:D.Snapshot;
   procedure Verify(Limit:Natural) is
   begin
      Check(Calls=Limit,"no access after refusal or invalid admission");
      for K in D.Register_Kind loop
         declare N:constant Positive:=D.Register_Kind'Pos(K)+1;begin
            Check(Result(K).Attempted=(N<=Limit),"exact attempted prefix");
            Check(Result(K).Value=(if N<=Limit then I.Word(N)*111 else 0),"copied raw value or initialized zero");
            Check(Result(K).Status=(if N>Limit then I.Unavailable elsif N=Fail_At then Failure else I.Access_OK),"failure preserved and later samples unavailable");
         end;
      end loop;
   end Verify;
begin
   Check(Plan.Status=G.Ready and then Plan.Selected.Pad=136,"profile pin209 is physical pad136");
   D.Capture(Plan,False,Result);Verify(0);
   D.Capture((Status=>G.No_GPIO_Pin),True,Result);Verify(0);
   for F in I.Access_Status range I.Denied..I.Unknown_Effect loop
      Failure:=F;
      for N in 0..4 loop
         Calls:=0;Fail_At:=N;D.Capture(Plan,True,Result);Verify(if N=0 then 4 else N);
      end loop;
   end loop;
   Ada.Text_IO.Put_Line("GPIO pad diagnostic checks"&Checks'Image);
end Pad_Tests;
