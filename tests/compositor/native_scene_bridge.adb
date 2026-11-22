with Vulkan_Owned_Targets;
with Vulkan_Scene;
with Vulkan_Scene_Recording;
with Vulkan_Frame;
package body Native_Scene_Bridge with SPARK_Mode is
   package O renames Vulkan_Owned_Targets;
   package V renames O.V;
   package P renames O.P;
   package F renames Vulkan_Frame;
   package D renames F.D;
   package G renames Vulkan_Scene.A.G;
   use type U32, System.Address, O.Phase, V.Phase, V.Source_Ticket,
     P.Ticket, P.ID, P.Slot, F.Admission, F.Completion,
     Vulkan_Scene_Recording.Outcome, Vulkan_Scene.Phase;
   Owner : O.State;
   Budget : O.A.State := O.A.Open (16 * 1024 * 1024);
   Session : V.State;
   Pool : P.State := P.Open (1);
   Damage : D.State := D.Open (1, 1);
   Screen : G.Output := (Width => 1, Height => 1, others => <>);
   Source_Ticket : V.Source_Ticket := V.No_Source;
   Live : Boolean := False;
   Poisoned : Boolean := False;

   function Healthy return Boolean is
     (Live and not Poisoned and O.Ready (Owner) and
      P.Valid (Pool) and not P.Faulted (Pool) and
      D.Valid (Damage) and not D.Faulted (Damage));

   procedure Open
     (Description, A, B, C, Submission, Source : System.Address;
      Allowed, Width, Height : U32; Result : out U32)
   is
      Fresh : O.State;
   begin
      Result := 2;
      if Poisoned then Result := 3; return; end if;
      if Live or else O.Current (Owner) not in O.Fresh | O.Closed or else
        not O.A.Valid (Budget) or else O.A.Charged (Budget) /= 0 or else
        P.Epoch (Pool) = P.ID'Last or else
        Width not in 12 .. 65535 or else Height not in 12 .. 65535 or else
        Description = System.Null_Address or else Submission = System.Null_Address or else
        Source = System.Null_Address then return; end if;
      Owner := Fresh;
      Pool := P.Open (P.Epoch (Pool) + 1);
      Screen := (G.Physical_Extent (Width), G.Physical_Extent (Height), others => <>);
      Damage := D.Open (D.Extent (Screen.Width), D.Extent (Screen.Height));
      Session := V.Open (Submission);
      Source_Ticket := V.No_Source;
      O.Allocate (Owner, F.Targets'(A, B, C), Budget, Allowed);
      if O.Can_Attach (Owner) then O.Attach (Owner, Description, P.Epoch (Pool), Session, Budget); end if;
      if not O.Ready (Owner) then
         if O.Current (Owner) = O.Quarantined then Poisoned := True; Result := 3; end if;
         return;
      end if;
      V.Install_Source (Session, 0, Source, Source_Ticket);
      if Source_Ticket = V.No_Source then Poisoned := True; Result := 3; return; end if;
      Live := True;
      Result := 0;
   end Open;

   procedure Begin_Frame (Slot, Result : out U32) is
      Admission : F.Admission;
   begin
      Slot := 0; Result := 2;
      if Poisoned then Result := 3; return; end if;
      if not Healthy or else V.Current (Session) /= V.Idle or else
        D.Active (Damage) /= 0 or else P.Ready (Pool) /= P.None then return; end if;
      D.Change (Damage, D.Bounds (Damage));
      F.Begin_Record (Session, Pool, Damage, Admission);
      case Admission is
         when F.Started => Slot := U32 (P.Writer (Pool).Buffer); Result := 0;
         when F.Deferred => Result := 1;
         when F.Failed => Poisoned := True; Result := 3;
      end case;
   end Begin_Frame;

   procedure Record_Frame (Result : out U32) is
      Scene : Vulkan_Scene.State := Vulkan_Scene.Open (Screen, 16#0000FF#);
      Accepted : Boolean;
      Recorded : Vulkan_Scene_Recording.Outcome;
   begin
      Result := 2;
      if Poisoned then Result := 3; return; end if;
      if not Healthy or else not P.Rendering (Pool) or else
        V.Current (Session) /= V.Recording or else V.Pass_Started (Session) or else
        not V.Complete_Frame (Session) or else D.Active (Damage) = 0 or else
        D.Active (Damage) /= P.Writer (Pool).Buffer then return; end if;
      Vulkan_Scene.Append (Scene,
        (Source => Source_Ticket,
         Surface => (0, 0, G.Logical_Coordinate (Screen.Width), G.Logical_Coordinate (Screen.Height)),
         others => <>), Accepted);
      if Accepted then Vulkan_Scene.Append_Physical_Fill (Scene, (4, 4, 12, 12), 16#00FF00#, Accepted); end if;
      if Accepted then Vulkan_Scene.Seal (Scene, Accepted); end if;
      pragma Assert (Accepted = (Vulkan_Scene.Current (Scene) = Vulkan_Scene.Sealed));
      -- Even a rejected capture goes through production cancellation.
      Vulkan_Scene_Recording.Record_Scene (Scene, Owner, Session, Pool, Damage, Recorded);
      case Recorded is
         when Vulkan_Scene_Recording.Recorded => Result := 0;
         when Vulkan_Scene_Recording.Cancelled => Result := 2;
         when Vulkan_Scene_Recording.Quarantined => Poisoned := True; Result := 3;
      end case;
   end Record_Frame;

   procedure Submit_Frame (Result : out U32) is
      Accepted : Boolean;
   begin
      Result := 2;
      if Poisoned then Result := 3; return; end if;
      if not Healthy or else not P.Rendering (Pool) or else
        V.Current (Session) /= V.Recording or else not V.Pass_Finished (Session) or else
        not V.Complete_Frame (Session) then return; end if;
      F.Submit (Session, Pool, Accepted);
      if Accepted then Result := 0; else Poisoned := True; Result := 3; end if;
   end Submit_Frame;

   procedure Poll_Frame (Result : out U32) is
      Observation : F.Completion;
   begin
      Result := 2;
      if Poisoned then Result := 3; return; end if;
      if not Healthy or else not P.Rendering (Pool) or else
        V.Current (Session) /= V.Pending or else D.Active (Damage) /= P.Writer (Pool).Buffer then return; end if;
      F.Poll (Session, Pool, Damage, Observation);
      case Observation is
         when F.Ready => Result := 0;
         when F.Still_Pending => Result := 1;
         when F.Uncertain => Poisoned := True; Result := 3;
      end case;
   end Poll_Frame;

   procedure Cancel_Frame (Result : out U32) is
      Released : Boolean;
   begin
      Result := 2;
      if Poisoned then Result := 3; return; end if;
      if not Healthy or else not P.Rendering (Pool) or else
        V.Current (Session) not in V.Recording | V.Sealed or else
        D.Active (Damage) /= P.Writer (Pool).Buffer then return; end if;
      F.Cancel (Session, Pool, Damage, Released);
      if Released then Result := 0; else Poisoned := True; Result := 3; end if;
   end Cancel_Frame;

   procedure Release_Frame (Result : out U32) is
   begin
      Result := 2;
      if Poisoned then Result := 3; return; end if;
      if not Healthy or else not V.Quiescent (Session) or else P.Ready (Pool) = P.None then return; end if;
      P.Discard_Ready (Pool, P.Ready (Pool));
      Result := 0;
   end Release_Frame;

   procedure Close (Result : out U32) is
      Released : Boolean;
      Source : System.Address;
   begin
      Result := 2;
      if Poisoned then Result := 3; return; end if;
      if not Healthy or else not V.Quiescent (Session) or else P.Ready (Pool) /= P.None or else
        P.Writer (Pool) /= P.None or else not O.A.Valid (Budget) then return; end if;
      V.Remove_Source (Session, Source_Ticket, Source);
      if Source = System.Null_Address then Poisoned := True; Result := 3; return; end if;
      O.Close (Owner, Session, Pool, Budget, Released);
      if Released then Live := False; Source_Ticket := V.No_Source; Result := 0;
      else Poisoned := True; Result := 3;
      end if;
   end Close;
end Native_Scene_Bridge;
