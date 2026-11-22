package body Client_Input_Provenance with SPARK_Mode is
   procedure Begin_Event (S : in out State; ID : Serial; Accepted : out Boolean) is
   begin
      Accepted := not S.Paint_Open and S.In_Progress = 0 and ID > S.Completed;
      if Accepted then S.In_Progress := ID; end if;
   end Begin_Event;
   procedure Finish_Event (S : in out State; ID : Serial; Accepted : out Boolean) is
   begin
      Accepted := ID /= 0 and S.In_Progress = ID;
      if Accepted then
         S := (ID, 0, S.Captured, S.Published_Input, S.Paint_Open);
      end if;
   end Finish_Event;
   procedure Begin_Paint (S : in out State; Accepted : out Boolean) is
   begin
      Accepted := not S.Paint_Open;
      if Accepted then
         S := (S.Completed, S.In_Progress, S.Completed, S.Published_Input, True);
      end if;
   end Begin_Paint;
   procedure End_Paint
     (S : in out State; Published : Boolean; Watermark : out Serial) is
   begin
      Watermark := (if S.Paint_Open and Published then S.Captured else 0);
      S := (S.Completed, S.In_Progress, 0,
            (if S.Paint_Open and Published then S.Captured else S.Published_Input), False);
   end End_Paint;
end Client_Input_Provenance;
