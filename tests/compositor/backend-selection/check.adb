with Compositor_Backend_Selection; use Compositor_Backend_Selection;
with Ada.Text_IO;
procedure Check is
   function Evidence (Mask : Natural) return Readiness is
     ((Mask / 1) mod 2 = 1, (Mask / 2) mod 2 = 1, (Mask / 4) mod 2 = 1,
      (Mask / 8) mod 2 = 1, (Mask / 16) mod 2 = 1, (Mask / 32) mod 2 = 1,
      (Mask / 64) mod 2 = 1);
begin
   for Mask in 0 .. 127 loop
      declare S : State; Accepted : Boolean; Chosen : Mode;
      begin
         Select_Backend (S, Evidence (Mask), Accepted);
         pragma Assert (Accepted);
         pragma Assert (Current (S) = (if Mask = 127 then GPU else Software));
         Chosen := Current (S);
         for Next in 0 .. 127 loop
            Select_Backend (S, Evidence (Next), Accepted);
            pragma Assert (not Accepted and Current (S) = Chosen);
            Begin_Output (S); pragma Assert (Current (S) = Chosen);
         end loop;
      end;
      declare S : State; Accepted : Boolean;
      begin
         Begin_Output (S); Select_Backend (S, Evidence (Mask), Accepted);
         pragma Assert (not Accepted and Current (S) = Software);
      end;
   end loop;
   for Mask in 0 .. 63 loop
      declare
         S : State; Accepted, Switched : Boolean;
         Key : constant Recovery_Key := (0, 17, 23, 2);
         E : constant Drain_Evidence :=
           ((Mask/1) mod 2=1,(Mask/2) mod 2=1,(Mask/4) mod 2=1,
            (Mask/8) mod 2=1,(Mask/16) mod 2=1,(Mask/32) mod 2=1);
      begin
         Select_Backend (S, (others=>True), Accepted);
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
         Select_Backend(S,(others=>True),Accepted); pragma Assert(not Accepted);
      end;
   end loop;
   Ada.Text_IO.Put_Line ("PASS all64 drain combinations, stale identities, uncertainty, duplicate/re-enable rejection");
   Ada.Text_IO.Put_Line ("PASS 128 readiness combinations, 16384 reselections, early-output lock");
end Check;
