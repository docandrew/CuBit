package body Compositor_Upload_Progress with SPARK_Mode is
   procedure Begin_Image (S : in out State; Identity : Natural;
      Width, Height : G.Edge; Kind : G.Pixel_Format; Accepted : out Boolean) is
   begin
      Accepted := S.Mode in Untouched | Preparing | Ready and Identity > 0 and
         Identity >= S.Identity and Width > 0 and Height > 0 and
         (Identity > S.Identity or else (Width = S.Width and Height = S.Rows and Kind = S.Kind));
      if Accepted then
         S.Mode := Preparing; S.Identity := Identity; S.Width := Width; S.Rows := Height;
         S.Kind := Kind; S.Done := 0; S.Limit := Height; S.Retain := False;
      end if;
   end Begin_Image;
   procedure Begin_Update (S : in out State; Identity : Natural;
      First, Last : G.Edge; Accepted : out Boolean) is
   begin
      Accepted := S.Mode = Ready and Identity > 0 and Identity = S.Identity and
         First < Last and Last <= S.Rows;
      if Accepted then
         S.Mode := Preparing; S.Done := First; S.Limit := Last; S.Retain := True;
      end if;
   end Begin_Update;
   procedure Begin_Write (S : in out State; Capacity : G.Byte_Count;
      Plan : out G.Plan; T : out Ticket; Discard : out Boolean; Accepted : out Boolean;
      Row_Pixels : G.Edge := 0) is
      Empty : G.Plan;
   begin
      T := No_Ticket; Discard := False; Accepted := False; Plan := Empty;
      if S.Mode /= Preparing or else S.Last = Last_Sequence then return; end if;
      G.Row_Chunk (S.Width, S.Rows, S.Done, S.Limit, Capacity, S.Kind, Plan, Accepted, Row_Pixels);
      if not Accepted then return; end if;
      S.Last := S.Last + 1; S.Plan := Plan; S.Mode := Writing;
      T := (S.Identity, S.Last); Discard := S.Done = 0 and not S.Retain;
   end Begin_Write;
   procedure Submitted (S : in out State; T : Ticket; Accepted : out Boolean) is
   begin
      Accepted := Can_Write (S, T);
      if Accepted then S.Mode := Pending; end if;
   end Submitted;
   procedure Observe (S : in out State; T : Ticket; Result : Observation) is
   begin
      if S.Mode /= Pending or else not Active (S, T) then return; end if;
      case Result is
         when Still_Pending => null;
         when Uncertain => S.Mode := Quarantined;
         when Completed =>
            S.Done := S.Done + G.Area (S.Plan).Height;
            if S.Done = S.Limit then
               -- Rows outside a retained band already hold the new content.
               S.Done := S.Rows; S.Limit := S.Rows; S.Retain := False; S.Mode := Ready;
            else
               S.Mode := Preparing;
            end if;
      end case;
   end Observe;
   procedure Cancel (S : in out State; T : Ticket; Confirmed : Boolean) is
   begin
      if Can_Write (S, T) then S.Mode := (if Confirmed then Preparing else Quarantined); end if;
   end Cancel;
end Compositor_Upload_Progress;
