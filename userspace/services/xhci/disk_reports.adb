------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--  SPDX-License-Identifier: GPL-3.0-or-later
------------------------------------------------------------------------------
pragma Ada_2022;

package body Disk_Reports with SPARK_Mode is

   procedure Appear (Item : in out State; S : Slot; U : Unit; Write_Protected : Boolean) is
   begin
      Item (S).Current := U;
      Item (S).Write_Protected := Write_Protected;
      Item (S).Announced := False;
   end Appear;

   procedure Vanish (Item : in out State; S : Slot) is
   begin
      if Item (S).Announced then
         Item (S).Pending_Gone := Item (S).Current;
      end if;
      Item (S).Current := No_Unit;
      Item (S).Write_Protected := False;
      Item (S).Announced := False;
   end Vanish;

   procedure Next (Item : State; S : Slot; Due : out Boolean; Report : out DP.Report) is
   begin
      Report := (others => <>);
      Due := True;
      if Item (S).Pending_Gone /= No_Unit then
         Report := (Unit => Item (S).Pending_Gone, State => DP.Gone, Write_Protected => False);
      elsif Item (S).Current /= No_Unit and then not Item (S).Announced then
         Report := (Unit => Item (S).Current, State => DP.Present,
                    Write_Protected => Item (S).Write_Protected);
      else
         Due := False;
      end if;
   end Next;

   procedure Sent (Item : in out State; S : Slot) is
   begin
      if Item (S).Pending_Gone /= No_Unit then
         Item (S).Pending_Gone := No_Unit;
      elsif Item (S).Current /= No_Unit then
         Item (S).Announced := True;
      end if;
   end Sent;

end Disk_Reports;
