with Vulkan_Submission_FFI;
package body Vulkan_Submission with SPARK_Mode is
   package F renames Vulkan_Submission_FFI;
   use type F.Code, Compositor_Affine.G.Pixel_Edge;
   procedure Install_Source
     (S : in out State; Index : Source_Slot; Borrowed_Draw : System.Address;
      Ticket : out Source_Ticket) is
   begin
      Ticket := No_Source;
      if S.Stage /= Idle or else Borrowed_Draw = System.Null_Address or else
        Source_Present (S, Index) or else S.Last_Source = Serial'Last then return; end if;
      for I in Source_Slot loop
         if S.Sources (I).Context = Borrowed_Draw then return; end if;
      end loop;
      S.Last_Source := S.Last_Source + 1;
      S.Sources (Index) := (Context => Borrowed_Draw, Generation => S.Last_Source, Managed => False);
      Ticket := (Index, S.Last_Source);
   end Install_Source;
   procedure Remove_Source
     (S : in out State; Ticket : Source_Ticket; Released : out System.Address) is
   begin
      Released := System.Null_Address;
      if S.Stage /= Idle or else not Source_Valid (S, Ticket) or else
        S.Sources (Ticket.Index).Managed then return; end if;
      Released := S.Sources (Ticket.Index).Context;
      S.Sources (Ticket.Index) := (others => <>);
   end Remove_Source;
   procedure Import_Source
     (S : in out State; Index : Source_Slot; Description : System.Address;
      Ticket : out Source_Ticket) is
      Draw : System.Address;
      Status : F.Code;
   begin
      Ticket := No_Source;
      if S.Stage /= Idle or else Source_Present (S, Index) or else
        S.Last_Source = Serial'Last or else Description = System.Null_Address then return; end if;
      F.Import_Source (Description, Draw, Status);
      if Status = 1 then return; end if;
      if Status /= 0 or else Draw = System.Null_Address then S.Stage := Quarantined; return; end if;
      Install_Source (S, Index, Draw, Ticket);
      if Ticket = No_Source then S.Stage := Quarantined; return; end if;
      S.Sources (Index).Managed := True;
   end Import_Source;
   procedure Release_Source
     (S : in out State; Ticket : Source_Ticket; Released : out System.Address) is
      Status : F.Code;
   begin
      Released := System.Null_Address;
      if S.Stage /= Idle or else not Source_Valid (S, Ticket) or else
        not S.Sources (Ticket.Index).Managed then return; end if;
      F.Release_Source (S.Sources (Ticket.Index).Context, Status);
      if Status /= 0 then S.Stage := Quarantined; return; end if;
      Released := S.Sources (Ticket.Index).Context;
      S.Sources (Ticket.Index) := (others => <>);
   end Release_Source;
   procedure Begin_Record (S : in out State; Accepted : out Boolean) is
      Status : F.Code;
   begin
      F.Start (S.Context, Status);
      Accepted := Status = 0;
      S.Stage := (if Accepted then Recording else Quarantined);
      S.Count := 0; S.Whole := Accepted; S.Pass_State := Before_Pass;
   end Begin_Record;
   procedure Begin_Scene
     (S : in out State; Borrowed_Pass : System.Address;
      Width, Height : Compositor_Affine.G.Physical_Extent; Accepted : out Boolean) is
      Status : F.Code;
   begin
      F.Begin_Scene (S.Context, Borrowed_Pass, F.Code (Width), F.Code (Height), Status);
      Accepted := Status = 0;
      if Accepted then
         S.Pass_State := Inside_Pass; S.Target_Width := Width; S.Target_Height := Height;
      else S.Stage := Quarantined;
      end if;
   end Begin_Scene;
   procedure End_Scene (S : in out State; Accepted : out Boolean) is
      Status : F.Code;
   begin
      F.End_Scene (S.Context, Status);
      Accepted := Status = 0;
      if Accepted then S.Pass_State := After_Pass; else S.Stage := Quarantined; end if;
   end End_Scene;
   procedure Admit_Draw (S : in out State; Accepted : out Boolean) is
   begin
      Accepted := S.Whole and S.Count < Maximum_Draws;
      if Accepted then S.Count := S.Count + 1; else S.Whole := False; end if;
   end Admit_Draw;
   procedure Reject_Frame (S : in out State) is
   begin
      S.Whole := False;
   end Reject_Frame;
   procedure Draw_Output
     (S : in out State; Source : Source_Ticket;
      Screen : Compositor_Affine.G.Output;
      Surface : Compositor_Affine.G.Logical_Rectangle;
      Damage : Compositor_Affine.G.Physical_Rectangle;
      Over, Mask : Boolean; Tint : Compositor_Affine.Word;
      Result : out Vulkan_Affine_Binding.Outcome;
      Raster_Glyph : Boolean := False; Straight_Alpha : Boolean := False) is
      use type Vulkan_Affine_Binding.Outcome;
      OK : Boolean;
      Match : F.Code;
   begin
      Admit_Draw (S, OK);
      Result := Vulkan_Affine_Binding.Rejected;
      if not OK then return; end if;
      if not Source_Valid (S, Source) then Reject_Frame (S); return; end if;
      if Screen.Width /= S.Target_Width or else Screen.Height /= S.Target_Height then
         Reject_Frame (S); return;
      end if;
      F.Matches (S.Context, S.Sources (Source.Index).Context, Match);
      if Match = 0 then
         Vulkan_Affine_Binding.Draw_Output
           (S.Sources (Source.Index).Context, Screen, Surface, Damage, Over, Mask, Tint, Result, Raster_Glyph, Straight_Alpha);
      end if;
      if Result = Vulkan_Affine_Binding.Rejected then Reject_Frame (S); end if;
   end Draw_Output;
   procedure Fill_Output
     (S : in out State; Area : Compositor_Affine.G.Physical_Rectangle;
      RGB : Compositor_Affine.Word; Accepted : out Boolean) is
      use type Compositor_Affine.G.Pixel_Edge;
      Status : F.Code;
   begin
      Admit_Draw (S, Accepted);
      if not Accepted then return; end if;
      if Area.Right > S.Target_Width or Area.Bottom > S.Target_Height then
         Reject_Frame (S); Accepted := False; return;
      end if;
      if Area.Left >= Area.Right or Area.Top >= Area.Bottom then return; end if;
      F.Fill (S.Context, F.Code (S.Target_Width), F.Code (S.Target_Height),
        F.Code (Area.Left), F.Code (Area.Top), F.Code (Area.Right), F.Code (Area.Bottom), RGB, Status);
      Accepted := Status = 0;
      if not Accepted then Reject_Frame (S); end if;
   end Fill_Output;
   procedure Seal (S : in out State; Accepted : out Boolean) is
      Status : F.Code;
   begin
      F.Seal (S.Context, Status);
      Accepted := Status = 0;
      S.Stage := (if Accepted then Sealed else Quarantined);
   end Seal;
   procedure Seal_Transfer (S : in out State; Accepted : out Boolean) is
      Status : F.Code;
   begin
      F.Seal (S.Context, Status);
      Accepted := Status = 0;
      S.Stage := (if Accepted then Sealed else Quarantined);
   end Seal_Transfer;
   procedure Submit (S : in out State; Accepted : out Boolean) is
      Status : F.Code;
   begin
      F.Submit (S.Context, Status);
      Accepted := Status = 0;
      S.Stage := (if Accepted then Pending else Quarantined);
   end Submit;
   procedure Poll (S : in out State; Result : out Observation) is
      Status : F.Code;
   begin
      F.Poll (S.Context, Status);
      if Status = 0 then S.Stage := Idle; Result := Finished;
      elsif Status = 1 then Result := Still_Pending;
      else S.Stage := Quarantined; Result := Uncertain;
      end if;
   end Poll;
   procedure Cancel (S : in out State; Accepted : out Boolean) is
      Status : F.Code;
   begin
      F.Cancel (S.Context, Status);
      Accepted := Status = 0;
      S.Stage := (if Accepted then Idle else Quarantined);
      if Accepted then S.Pass_State := Before_Pass; end if;
   end Cancel;
end Vulkan_Submission;
