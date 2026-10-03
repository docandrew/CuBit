package body Vulkan_Scene_Recording with SPARK_Mode is
   package F renames Vulkan_Frame;
   use type Vulkan_Scene.Phase, P.ID;
   procedure Record_Scene
     (Scene : Vulkan_Scene.State; Targets : O.State;
      Submission : in out V.State; Pool : in out P.State;
      Damage : in out D.State; Result : out Outcome)
   is
      OK, Released : Boolean;
      procedure Cancel_Unsubmitted
        with Pre => P.Valid (Pool) and P.Rendering (Pool) and
          V.Current (Submission) = V.Recording and D.Valid (Damage) and not D.Faulted (Damage) and
          D.Active (Damage) = P.Writer (Pool).Buffer,
        Post => P.Valid (Pool) and D.Valid (Damage) and
          (if Result = Cancelled then V.Quiescent (Submission) and not P.Faulted (Pool) and
            P.Writer (Pool) = P.None and D.Active (Damage) = 0
           else Result = Quarantined and P.Faulted (Pool) and D.Faulted (Damage))
      is
      begin
         F.Cancel (Submission, Pool, Released);
         D.Finish (Damage, (if Released then D.Cancelled else D.Unknown));
         Result := (if Released then Cancelled else Quarantined);
      end Cancel_Unsubmitted;
   begin
      if not O.Ready (Targets) or else not O.Same_Submission (Targets, Submission) or else
        O.Output_Epoch (Targets) /= P.Epoch (Pool) or else
        Vulkan_Scene.Current (Scene) /= Vulkan_Scene.Sealed or else
        not Vulkan_Scene.Sources_Ready (Scene, Submission)
      then Cancel_Unsubmitted; return;
      end if;
      O.Prepare_Frame (Targets, Submission, Pool, Damage, OK);
      if not OK then Cancel_Unsubmitted; return; end if;
      F.Begin_Scene (Submission, Pool, O.Bindings (Targets),
        Vulkan_Scene.Output (Scene).Width, Vulkan_Scene.Output (Scene).Height, OK);
      if not OK then D.Finish (Damage, D.Unknown); Result := Quarantined; return; end if;
      Vulkan_Scene.Replay (Scene, Submission, Damage, OK);
      if not OK then Cancel_Unsubmitted; return; end if;
      F.End_Scene (Submission, Pool, OK);
      if not OK then D.Finish (Damage, D.Unknown); Result := Quarantined; return; end if;
      Result := Recorded;
   end Record_Scene;
end Vulkan_Scene_Recording;
