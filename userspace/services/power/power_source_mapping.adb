pragma Ada_2022;
with Interfaces; use Interfaces;

package body Power_Source_Mapping with SPARK_Mode is
   use type AF.Unit_Kind, AF.Direction, AF.AC_State;
   use type PS.Supply;

   Maximum : constant Unsigned_64 := PS.Maximum_Quantity;

   function Name_Of (Text : String) return PS.Source_Name
   with Pre => Text'Length <= PS.Name_Capacity
     and then (for all C of Text => C in PS.Name_Character)
   is
      Result : PS.Source_Name := (Length => Text'Length, Text => [others => ' ']);
   begin
      for J in Text'Range loop
         Result.Text (J - Text'First + 1) := Text (J);
         pragma Loop_Invariant (for all C of Result.Text => C in PS.Name_Character);
      end loop;
      return Result;
   end Name_Of;

   function Battery_Name (Ordinal : PS.Source_Index) return PS.Source_Name is
      Digits_Of : constant String := "0123456789";
      Tens : constant Natural := Ordinal / 10;
      Ones : constant Natural := Ordinal mod 10;
   begin
      if Tens = 0 then
         return Name_Of ("battery-" & Digits_Of (Ones + 1));
      else
         return Name_Of ("battery-" & Digits_Of (Tens + 1) & Digits_Of (Ones + 1));
      end if;
   end Battery_Name;

   function AC_Name return PS.Source_Name is (Name_Of ("ac-adapter"));

   --  Part over Whole_Value in per mille, at most Whole.
   function Per_Mille_Of (Part, Whole_Value : PS.Quantity) return PS.Per_Mille is
     (if Part.Known and then Whole_Value.Known and then Whole_Value.Value > 0
      then (Known => True,
            Value => PS.Per_Mille_Value
              (Unsigned_64'Min (PS.Whole, Unsigned_64 (Part.Value) * PS.Whole / Unsigned_64 (Whole_Value.Value))))
      else PS.Unknown_Per_Mille);

   --  A product over a divisor, Unknown beyond Maximum_Quantity.
   function Scaled (Value, Factor, Divisor : Unsigned_64) return PS.Quantity is
     (if Divisor > 0 and then Value * Factor / Divisor <= Maximum
      then PS.Known_Quantity (PS.Quantity_Value (Value * Factor / Divisor))
      else PS.Unknown_Quantity)
   with Pre => Value <= Maximum and then Factor <= Maximum;

   --  mAh (or mA) at Voltage mV as mWh (mW).
   function Through (Q, Voltage : PS.Quantity) return PS.Quantity is
     (if Q.Known and then Voltage.Known and then Voltage.Value > 0
      then Scaled (Unsigned_64 (Q.Value), Unsigned_64 (Voltage.Value), Millis_Per_Unit)
      else PS.Unknown_Quantity);

   --  Amount (capacity) at Rate (capacity per hour) as a runtime of Kind.
   function Runtime_Of (Kind : PS.Runtime_Kind; Amount : Unsigned_64; Rate : PS.Quantity) return PS.Runtime is
     (if Kind /= PS.Unknown and then Rate.Known and then Rate.Value > 0
        and then Amount * Seconds_Per_Hour / Unsigned_64 (Rate.Value) <= Maximum
      then (Kind => Kind,
            Seconds => PS.Seconds_Value (Amount * Seconds_Per_Hour / Unsigned_64 (Rate.Value)))
      else PS.Unknown_Runtime)
   with Pre => Amount <= Maximum,
        Post => Runtime_Of'Result.Kind in PS.Unknown | Kind;

   function Battery_Source (F : AF.Battery_Facts; Ordinal : PS.Source_Index) return PS.Source is
      R : PS.Source := (Kind => PS.Internal_Battery, Name => Battery_Name (Ordinal), Fresh => F.Fresh,
                        others => <>);
      Capacities_Known : constant Boolean := F.Remaining.Known and then F.Last_Full.Known;
      At_Full : constant Boolean := Capacities_Known and then F.Remaining.Value >= F.Last_Full.Value;
      Rate_Voltage : constant PS.Quantity :=
        (if F.Voltage.Known and then F.Voltage.Value > 0 then F.Voltage else F.Design_Voltage);
   begin
      if F.Status /= AF.Sampled then
         return R;
      end if;
      R.Charge := Per_Mille_Of (F.Remaining, F.Last_Full);
      if F.Dynamic_Known then
         R.State :=
           (case F.Flow is
              when AF.Charging    => PS.Charging,
              when AF.Discharging => PS.Discharging,
              when AF.Neither     =>
                (if At_Full then PS.Full elsif Capacities_Known then PS.Not_Charging else PS.Unknown));
         R.Condition.Low := (if F.Critical then PS.Set else PS.Clear);
         if F.Flow = AF.Discharging and then F.Remaining.Known then
            R.Left := Runtime_Of (PS.To_Empty, Unsigned_64 (F.Remaining.Value), F.Rate);
         elsif F.Flow = AF.Charging and then Capacities_Known and then F.Last_Full.Value > F.Remaining.Value then
            R.Left := Runtime_Of
              (PS.To_Full, Unsigned_64 (F.Last_Full.Value - F.Remaining.Value), F.Rate);
         end if;
      end if;
      case F.Units is
         when AF.Energy_And_Power =>
            R.Energy_Now := F.Remaining;
            R.Energy_Full := F.Last_Full;
            R.Energy_Design := F.Design;
            R.Rate := F.Rate;
         when AF.Charge_And_Current =>
            R.Energy_Now := Through (F.Remaining, F.Design_Voltage);
            R.Energy_Full := Through (F.Last_Full, F.Design_Voltage);
            R.Energy_Design := Through (F.Design, F.Design_Voltage);
            R.Rate := Through (F.Rate, Rate_Voltage);
         when AF.Unspecified_Unit =>
            null;
      end case;
      R.Voltage := F.Voltage;
      return R;
   end Battery_Source;

   function AC_Source (F : AF.AC_Facts) return PS.Source is
     ((Kind => PS.AC_Adapter, Name => AC_Name, Fresh => F.Fresh,
       State => (case F.State is
                   when AF.Online  => PS.Online,
                   when AF.Offline => PS.Offline,
                   when AF.Unknown => PS.Unknown),
       others => <>));

   procedure Build
     (AC : AF.AC_Facts; Batteries : AF.Battery_Array; Battery_Count : AF.Battery_Count;
      Sources : out PS.Source_Array; Count : out PS.Source_Count; Origins : out Origin_Array)
   is
      Ordinal : Natural := 0;
   begin
      Sources := [others => (others => <>)];
      Origins := [others => 0];
      Count := 0;
      if AC.Present then
         Count := 1;
         Sources (1) := AC_Source (AC);
      end if;
      for Slot in 1 .. Battery_Count loop
         pragma Loop_Invariant (Count <= 1 + (Slot - 1) and then Ordinal <= Slot - 1
           and then Count = Ordinal + (if AC.Present then 1 else 0)
           and then (for all I in 1 .. Count =>
                       (if Sources (I).Kind = PS.AC_Adapter then Origins (I) = 0
                        else Origins (I) in 1 .. Slot - 1
                             and then Batteries (Origins (I)).Status /= AF.Known_Absent))
           and then (for all I in 1 .. Count =>
                       Sources (I).Kind = (if I = 1 and then AC.Present then PS.AC_Adapter
                                           else PS.Internal_Battery)));
         if Batteries (Slot).Status /= AF.Known_Absent then
            Ordinal := Ordinal + 1;
            Count := Count + 1;
            Sources (Count) := Battery_Source (Batteries (Slot), Ordinal);
            Origins (Count) := Slot;
         end if;
      end loop;
   end Build;

   --  Worse freshness: Fresh < Stale < Unknown.
   function Rank (F : PS.Freshness) return Natural is
     (case F is when PS.Fresh => 0, when PS.Stale => 1, when PS.Unknown => 2);

   function Summarize (Sources : PS.Source_Array; Count : PS.Source_Count) return PS.Summary is
      Result : PS.Summary := (Sources => Count, others => <>);
      Batteries : PS.Source_Count := 0;
      Only : PS.Source_Index := 1;
      Charging, Discharging, Unknown_State : Boolean := False;
      All_Full : Boolean;
      Any_Low : Boolean := False;
      All_Clear : Boolean;
      Mains, Adapters : Boolean := False;
      All_Offline : Boolean;
      Energies_Known : Boolean := True;
      Now_Sum, Full_Sum, Missing_Sum, Charging_Rate, Discharging_Rate : Unsigned_64 := 0;
      Worst : Natural := 0;
   begin
      All_Full := True;
      All_Clear := True;
      All_Offline := True;
      for I in 1 .. Count loop
         pragma Loop_Invariant
           (Batteries <= I - 1
            and then Now_Sum <= Unsigned_64 (I - 1) * Maximum
            and then Full_Sum <= Unsigned_64 (I - 1) * Maximum
            and then Missing_Sum <= Unsigned_64 (I - 1) * Maximum
            and then Charging_Rate <= Unsigned_64 (I - 1) * Maximum
            and then Discharging_Rate <= Unsigned_64 (I - 1) * Maximum
            and then Worst <= 2);
         --  The bound one more source raises each sum's by (linear: helps
         --  the provers past the product).
         pragma Assert (Unsigned_64 (I) * Maximum = Unsigned_64 (I - 1) * Maximum + Maximum);
         declare
            S : PS.Source renames Sources (I);
         begin
            Worst := Natural'Max (Worst, Rank (S.Fresh));
            if PS.Is_Battery (S.Kind) then
               Batteries := Batteries + 1;
               Only := I;
               case S.State is
                  when PS.Charging    => Charging := True;
                  when PS.Discharging => Discharging := True;
                  when PS.Full        => null;
                  when PS.Not_Charging => All_Full := False;
                  when others         => Unknown_State := True;
               end case;
               if S.State /= PS.Full then
                  All_Full := False;
               end if;
               if S.Condition.Low = PS.Set then
                  Any_Low := True;
               end if;
               if S.Condition.Low /= PS.Clear then
                  All_Clear := False;
               end if;
               if S.Energy_Now.Known and then S.Energy_Full.Known then
                  Now_Sum := Now_Sum + Unsigned_64 (S.Energy_Now.Value);
                  Full_Sum := Full_Sum + Unsigned_64 (S.Energy_Full.Value);
                  if S.Energy_Full.Value > S.Energy_Now.Value then
                     Missing_Sum := Missing_Sum + Unsigned_64 (S.Energy_Full.Value - S.Energy_Now.Value);
                  end if;
               else
                  Energies_Known := False;
               end if;
               if S.State in PS.Charging | PS.Discharging then
                  if S.Rate.Known then
                     if S.State = PS.Charging then
                        Charging_Rate := Charging_Rate + Unsigned_64 (S.Rate.Value);
                     else
                        Discharging_Rate := Discharging_Rate + Unsigned_64 (S.Rate.Value);
                     end if;
                  else
                     Energies_Known := False;
                  end if;
               end if;
            elsif S.Kind in PS.AC_Adapter | PS.UPS then
               Adapters := True;
               if S.State = PS.Online then
                  Mains := True;
               end if;
               if S.State not in PS.Offline | PS.On_Battery then
                  All_Offline := False;
               end if;
            end if;
         end;
      end loop;
      Result.Batteries := Batteries;
      Result.Fresh := (if Count = 0 then PS.Unknown
                       else (case Worst is when 0 => PS.Fresh, when 1 => PS.Stale, when others => PS.Unknown));
      Result.Runs_On :=
        (if Mains then PS.Mains
         elsif Discharging or else (Adapters and then All_Offline) then PS.Battery
         else PS.Unknown);
      if Batteries = 0 then
         return Result;
      end if;
      Result.State :=
        (if Charging then PS.Charging
         elsif Discharging then PS.Discharging
         elsif Unknown_State then PS.Unknown
         elsif All_Full then PS.Full
         else PS.Not_Charging);
      Result.Low := (if Any_Low then PS.Set elsif All_Clear then PS.Clear else PS.Unknown);
      if Energies_Known and then Full_Sum > 0 then
         Result.Charge :=
           (Known => True,
            Value => PS.Per_Mille_Value (Unsigned_64'Min (PS.Whole, Now_Sum * PS.Whole / Full_Sum)));
      elsif Batteries = 1 then
         Result.Charge := Sources (Only).Charge;
      end if;
      if Batteries = 1 then
         Result.Left := Sources (Only).Left;
      elsif Energies_Known and then Result.State = PS.Discharging and then Discharging_Rate > 0
        and then Now_Sum * Seconds_Per_Hour / Discharging_Rate <= Maximum
      then
         Result.Left := (Kind => PS.To_Empty,
                         Seconds => PS.Seconds_Value (Now_Sum * Seconds_Per_Hour / Discharging_Rate));
      elsif Energies_Known and then Result.State = PS.Charging and then Charging_Rate > 0
        and then Missing_Sum * Seconds_Per_Hour / Charging_Rate <= Maximum
      then
         Result.Left := (Kind => PS.To_Full,
                         Seconds => PS.Seconds_Value (Missing_Sum * Seconds_Per_Hour / Charging_Rate));
      end if;
      return Result;
   end Summarize;
end Power_Source_Mapping;
