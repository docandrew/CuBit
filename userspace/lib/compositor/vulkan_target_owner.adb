with Vulkan_Target_FFI;
package body Vulkan_Target_Owner with SPARK_Mode is
   package F renames Vulkan_Target_FFI;
   use type F.Code, System.Address;
   procedure Initialize (S : in out State; Description : System.Address; Output_Epoch : P.Live_ID; Accepted : out Boolean) is
      Code : F.Code;
   begin
      S.Request := Description; S.Epoch := Output_Epoch;
      F.Create (Description, S.Views (1), S.Views (2), S.Views (3), Code);
      if Code = 1 then S.Mode := Closed;
      elsif Code = 0 and then S.Views (1) /= System.Null_Address and then
        S.Views (2) /= System.Null_Address and then S.Views (3) /= System.Null_Address and then
        S.Views (1) /= S.Views (2) and then S.Views (1) /= S.Views (3) and then S.Views (2) /= S.Views (3)
      then S.Mode := Live;
      else S.Mode := Quarantined;
      end if;
      Accepted := S.Mode = Live;
   end Initialize;
   procedure Close (S : in out State; Submission : V.State; Pool : P.State; Released : out Boolean) is
      Code : F.Code;
   begin
      Released := False;
      if not Can_Close (S, Submission, Pool) then return; end if;
      F.Release (S.Request, Code);
      if Code = 0 then S.Mode := Closed; S.Views := (others => System.Null_Address); Released := True;
      else S.Mode := Quarantined;
      end if;
   end Close;
end Vulkan_Target_Owner;
