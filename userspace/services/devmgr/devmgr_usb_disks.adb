------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--  SPDX-License-Identifier: GPL-3.0-or-later
------------------------------------------------------------------------------
pragma Ada_2022;

package body Devmgr_USB_Disks with SPARK_Mode is

   function Later (Now : Unsigned_64) return Unsigned_64 is
     (if Now <= Unsigned_64'Last - Retry_Ms then Now + Retry_Ms else Unsigned_64'Last);

   function Find (T : Table; Driver : Driver_Index; Unit : Units.Unit) return Natural is
   begin
      for O in Ordinal loop
         if Holds (T (O), Driver, Unit) then
            return O + 1;
         end if;
         pragma Loop_Invariant (for all P in Ordinal'First .. O => not Holds (T (P), Driver, Unit));
      end loop;
      return 0;
   end Find;

   procedure Note_Present
     (T : in out Table; Driver : Driver_Index; Unit : Units.Unit; Write_Protected : Boolean;
      Result : out Placement; Name : out Ordinal)
   is
   begin
      Name := Ordinal'First;
      if Find (T, Driver, Unit) /= 0 then
         Result := Already_Known;
         return;
      end if;
      for O in Ordinal loop
         if T (O).State = Free then
            T (O) := (State => Attach_Owed, Driver => Driver, Unit => Unit,
                      Write_Protected => Write_Protected, Retry_At => 0, Told => False);
            Name := O;
            Result := Placed;
            return;
         end if;
      end loop;
      Result := No_Free_Name;
   end Note_Present;

   procedure Note_Gone
     (T : in out Table; Driver : Driver_Index; Unit : Units.Unit; Found : out Boolean; Name : out Ordinal)
   is
      Where : constant Natural := Find (T, Driver, Unit);
   begin
      Name := Ordinal'First;
      Found := Where /= 0;
      if not Found then
         return;
      end if;
      Name := Where - 1;
      if Maybe_Known (T (Name)) then
         T (Name).State := Detach_Owed;
         T (Name).Retry_At := 0;
      else
         T (Name) := (others => <>);
      end if;
   end Note_Gone;

   procedure Driver_Ended (T : in out Table; Driver : Driver_Index) is
   begin
      for O in Ordinal loop
         if T (O).State /= Free and then T (O).Driver = Driver then
            if Maybe_Known (T (O)) then
               T (O).State := Detach_Owed;
               T (O).Retry_At := 0;
            else
               T (O) := (others => <>);
            end if;
         end if;
         pragma Loop_Invariant (Valid (T));
         pragma Loop_Invariant
           (for all P in Ordinal'First .. O =>
              (if T'Loop_Entry (P).State /= Free and then T'Loop_Entry (P).Driver = Driver
               then (if Maybe_Known (T'Loop_Entry (P))
                     then T (P).State = Detach_Owed else T (P).State = Free)
               else T (P) = T'Loop_Entry (P)));
         pragma Loop_Invariant
           (for all P in O + 1 .. Ordinal'Last => T (P) = T'Loop_Entry (P));
      end loop;
   end Driver_Ended;

   procedure Next_Due (T : Table; Now : Unsigned_64; Found : out Boolean; Name : out Ordinal) is
   begin
      Name := Ordinal'First;
      Found := False;
      for O in Ordinal loop
         if T (O).State in Attach_Owed | Detach_Owed and then T (O).Retry_At <= Now then
            Name := O;
            Found := True;
            return;
         end if;
      end loop;
   end Next_Due;

   function Next_Wake (T : Table) return Unsigned_64 is
      Wake : Unsigned_64 := Unsigned_64'Last;
   begin
      for O in Ordinal loop
         if T (O).State in Attach_Owed | Detach_Owed then
            Wake := Unsigned_64'Min (Wake, T (O).Retry_At);
         end if;
      end loop;
      return Wake;
   end Next_Wake;

   procedure Attach_Done (T : in out Table; Name : Ordinal; Outcome : Call_Outcome; Now : Unsigned_64) is
   begin
      case Outcome is
         when Answered_OK => T (Name).State := Attached;
         when Answered_Refused => T (Name).State := Refused;
         when Not_Answered =>
            T (Name).Told := True;
            T (Name).Retry_At := Later (Now);
      end case;
   end Attach_Done;

   procedure Detach_Done (T : in out Table; Name : Ordinal; Outcome : Call_Outcome; Now : Unsigned_64) is
   begin
      if Outcome = Answered_OK then
         T (Name) := (others => <>);
      else
         T (Name).Retry_At := Later (Now);
      end if;
   end Detach_Done;

   procedure Postpone (T : in out Table; Name : Ordinal; Now : Unsigned_64) is
   begin
      T (Name).Retry_At := Later (Now);
   end Postpone;

end Devmgr_USB_Disks;
