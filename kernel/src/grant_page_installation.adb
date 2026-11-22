procedure Grant_Page_Installation
  (Pages : Positive; Installed : out Natural; Success : out Boolean)
is
   Physical : Physical_Address;
   OK : Boolean;

   procedure Rollback is
   begin
      if Installed > 0 then
         Retire_Prefix (Installed);
         Installed := 0;
      end if;
   end Rollback;
begin
   Installed := 0;
   Success := False;
   for Page in 0 .. Pages - 1 loop
      Resolve_And_Pin (Page, Physical, OK);
      if not OK then
         Rollback;
         return;
      end if;
      Install (Page, Physical, OK);
      if not OK then
         --  This pin never became reachable through a receiver mapping.
         Release_Unpublished (Physical);
         Rollback;
         return;
      end if;
      Installed := Installed + 1;
   end loop;
   Success := True;
end Grant_Page_Installation;
