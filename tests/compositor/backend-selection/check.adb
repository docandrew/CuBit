with Compositor_Backend_Selection; use Compositor_Backend_Selection;
with Ada.Text_IO;
procedure Check is
   function Evidence (Mask : Natural) return Readiness is
     ((Mask / 1) mod 2 = 1, (Mask / 2) mod 2 = 1, (Mask / 4) mod 2 = 1,
      (Mask / 8) mod 2 = 1, (Mask / 16) mod 2 = 1, (Mask / 32) mod 2 = 1,
      (Mask / 64) mod 2 = 1);
begin
   --  The UI composition policy is the user's (LAT-002): CPU.
   pragma Assert (UI_Composition = CPU_Composition);
   for Policy in Composition loop
   for Mask in 0 .. 127 loop
      declare S : State; Accepted : Boolean; Chosen : Mode;
      begin
         Select_Backend (S, Evidence (Mask), Policy, Accepted);
         pragma Assert (Accepted);
         --  CPU composition never selects the GPU, even with full evidence.
         pragma Assert (Current (S) =
           (if Mask = 127 and then Policy = GPU_Composition then GPU else Software));
         Chosen := Current (S);
         for Next in 0 .. 127 loop
            for Again in Composition loop
               Select_Backend (S, Evidence (Next), Again, Accepted);
               pragma Assert (not Accepted and Current (S) = Chosen);
            end loop;
            Begin_Output (S); pragma Assert (Current (S) = Chosen);
         end loop;
      end;
      declare S : State; Accepted : Boolean;
      begin
         Begin_Output (S); Select_Backend (S, Evidence (Mask), Policy, Accepted);
         pragma Assert (not Accepted and Current (S) = Software);
      end;
   end loop;
   end loop;
   --  Full evidence under CPU composition: software, and no recovery from it.
   declare S : State; Accepted : Boolean;
   begin
      Select_Backend (S, (others => True), CPU_Composition, Accepted);
      pragma Assert (Accepted and Current (S) = Software);
      Request_Recovery (S, (0, 17, 23, 2), Accepted);
      pragma Assert (not Accepted and Can_Capture (S));
   end;
   for Mask in 0 .. 63 loop
      declare
         S : State; Accepted, Switched : Boolean;
         Key : constant Recovery_Key := (0, 17, 23, 2);
         E : constant Drain_Evidence :=
           ((Mask/1) mod 2=1,(Mask/2) mod 2=1,(Mask/4) mod 2=1,
            (Mask/8) mod 2=1,(Mask/16) mod 2=1,(Mask/32) mod 2=1);
      begin
         Select_Backend (S, (others=>True), GPU_Composition, Accepted);
         Request_Recovery (S, (others=>0), Accepted); pragma Assert(not Accepted);
         Request_Recovery (S, Key, Accepted); pragma Assert(Accepted and not Can_Capture(S));
         Request_Recovery (S, Key, Accepted); pragma Assert(not Accepted);
         for Field in 1 .. 4 loop
            declare Stale : Recovery_Key := Key; begin
               case Field is
                  when 1 => Stale.Output:=1;
                  when 2 => Stale.Epoch:=18;
                  when 3 => Stale.Frame:=24;
                  when others => Stale.Buffer:=3;
               end case;
               Observe_Recovery (S, Stale, E, Switched);
               pragma Assert(not Switched and Recovery(S)=Draining and Current(S)=GPU);
            end;
         end loop;
         Observe_Recovery (S, Key, E, Switched);
         pragma Assert(Switched=(Mask=31));
         pragma Assert(Current(S)=(if Mask=31 then Software else GPU));
         pragma Assert(Recovery(S)=(if Mask>=32 then Quarantined elsif Mask=31 then Recovered else Draining));
         if Mask>=31 then
            Observe_Recovery(S, Key, (True,True,True,True,True,False), Switched);
            pragma Assert(not Switched);
         end if;
         Select_Backend(S,(others=>True),GPU_Composition,Accepted); pragma Assert(not Accepted);
      end;
   end loop;
   Ada.Text_IO.Put_Line ("PASS all64 drain combinations, stale identities, uncertainty, duplicate/re-enable rejection");
   Ada.Text_IO.Put_Line ("PASS 2 policies x 128 readiness combinations, 65536 reselections, early-output lock, CPU policy never GPU");
end Check;
