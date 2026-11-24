pragma Ada_2022;
package body CuBit.Power_Sources with SPARK_Mode is

   Seconds_Shift : constant := 32;
   Charge_Mask   : constant Word := 16#FFFF#;
   Runtime_Shift : constant := 16;

   --  A word is the pack of its bytes.
   procedure Lemma_Bytes (W : Wire_Word)
   with Ghost, Global => null,
        Post => Pack (Byte_Of (W, 0), Byte_Of (W, 1), Byte_Of (W, 2), Byte_Of (W, 3), Byte_Of (W, 4),
                      Byte_Of (W, 5), Byte_Of (W, 6), Byte_Of (W, 7)) = W;
   procedure Lemma_Bytes (W : Wire_Word) is null;

   --  A word is its low four bytes and its high half.
   procedure Lemma_Halves (W : Wire_Word)
   with Ghost, Global => null,
        Post => (Pack (Byte_Of (W, 0), Byte_Of (W, 1), Byte_Of (W, 2), Byte_Of (W, 3), 0, 0, 0, 0)
                 or Shift_Left (Shift_Right (W, 32), 32)) = W;
   procedure Lemma_Halves (W : Wire_Word) is null;

   --  The summary's second word is its charge field and its runtime word.
   procedure Lemma_Charge (W : Word)
   with Ghost, Global => null,
        Post => ((W and 16#FFFF#) or Shift_Left (Shift_Right (W, 16), 16)) = W;
   procedure Lemma_Charge (W : Word) is null;

   function Runtime_Word (R : Runtime) return Wire_Word is
     (if R.Kind = Unknown then Unknown_Word else Word (R.Seconds));

   function Charge_Field (C : Per_Mille) return Word is
     (if C.Known then Word (C.Value) else Unknown_Charge);

   function Summary_A (S : Summary) return Wire_Word is
     (Pack (Word (S.Sources), Word (S.Batteries), Source_State'Pos (S.State), Supply'Pos (S.Runs_On),
            Flag'Pos (S.Low), Freshness'Pos (S.Fresh), Runtime_Kind'Pos (S.Left.Kind), 0));

   function Summary_B (S : Summary) return Wire_Word is
     (Charge_Field (S.Charge) or Shift_Left (Runtime_Word (S.Left), Runtime_Shift));

   function Valid_Summary (A, B : Word) return Boolean is
     (A <= Maximum_Word
      and then Byte_Of (A, 0) <= Maximum_Sources and then Byte_Of (A, 1) <= Maximum_Sources
      and then Byte_Of (A, 2) <= Source_State'Pos (Source_State'Last)
      and then Byte_Of (A, 3) <= Supply'Pos (Supply'Last)
      and then Byte_Of (A, 4) <= Flag'Pos (Flag'Last)
      and then Byte_Of (A, 5) <= Freshness'Pos (Freshness'Last)
      and then Byte_Of (A, 6) <= Runtime_Kind'Pos (Runtime_Kind'Last)
      and then Byte_Of (A, 7) = 0
      and then ((B and Charge_Mask) <= Whole or else (B and Charge_Mask) = Unknown_Charge)
      and then Shift_Right (B, Runtime_Shift) <= Unknown_Word
      and then (if Byte_Of (A, 6) = Runtime_Kind'Pos (Unknown)
                then Shift_Right (B, Runtime_Shift) = Unknown_Word
                else Shift_Right (B, Runtime_Shift) <= Maximum_Quantity));

   function To_Summary (A, B : Word) return Summary is
      Charge : constant Word := B and Charge_Mask;
      Seconds : constant Word := Shift_Right (B, Runtime_Shift);
      Kind : constant Runtime_Kind := Runtime_Kind'Val (Byte_Of (A, 6));
      Result : Summary;
   begin
      Result.Sources := Source_Count (Byte_Of (A, 0));
      Result.Batteries := Source_Count (Byte_Of (A, 1));
      Result.State := Source_State'Val (Byte_Of (A, 2));
      Result.Runs_On := Supply'Val (Byte_Of (A, 3));
      Result.Low := Flag'Val (Byte_Of (A, 4));
      Result.Fresh := Freshness'Val (Byte_Of (A, 5));
      if Charge /= Unknown_Charge then
         Result.Charge := (Known => True, Value => Per_Mille_Value (Charge));
      end if;
      if Kind /= Unknown then
         Result.Left := (Kind => Kind, Seconds => Seconds_Value (Seconds));
      end if;
      Lemma_Bytes (A);
      Lemma_Charge (B);
      return Result;
   end To_Summary;

   function Identity_Head (S : Source) return Wire_Word is
     (Pack (Source_Kind'Pos (S.Kind), Source_State'Pos (S.State), Freshness'Pos (S.Fresh),
            Runtime_Kind'Pos (S.Left.Kind), Transfer_Reason'Pos (S.Transfer), Word (S.Name.Length), 0, 0));

   function Identity_Tail (S : Source) return Wire_Word is
     (Pack (Flag'Pos (S.Condition.Low), Flag'Pos (S.Condition.Replace), Flag'Pos (S.Condition.Overload),
            Flag'Pos (S.Condition.Bypass), 0, 0, 0, 0)
      or Shift_Left (Word (S.Left.Seconds), Seconds_Shift));

   function Name_Word (N : Source_Name; First : Name_Position) return Wire_Word is
     (Pack (Character'Pos (N.Text (First)), Character'Pos (N.Text (First + 1)),
            Character'Pos (N.Text (First + 2)), Character'Pos (N.Text (First + 3)),
            Character'Pos (N.Text (First + 4)), Character'Pos (N.Text (First + 5)),
            Character'Pos (N.Text (First + 6)), Character'Pos (N.Text (First + 7))))
   with Pre => First in 1 | 9;

   function Page_Words (S : Source; P : Page) return Payload is
     (case P is
        when Identity        => [Identity_Head (S), Identity_Tail (S)],
        when Name_Page       => [Name_Word (S.Name, 1), Name_Word (S.Name, 9)],
        when Charge_And_Load => [Per_Mille_Word (S.Charge), Per_Mille_Word (S.Load)],
        when Energy          => [Quantity_Word (S.Energy_Now), Quantity_Word (S.Energy_Full)],
        when Design_And_Rate => [Quantity_Word (S.Energy_Design), Quantity_Word (S.Rate)],
        when Voltage_Page    => [Quantity_Word (S.Voltage), 0],
        when Mains           => [Quantity_Word (S.Input_Voltage), Quantity_Word (S.Output_Voltage)]);

   function Valid_Character (B : Byte_Value) return Boolean is
     (B in Character'Pos (Name_Character'First) .. Character'Pos (Name_Character'Last));

   procedure Read_Page (P : Page; Data : Payload; Into : in out Source; Valid : out Boolean) is
      D0 : constant Wire_Word := Data (0);
      D1 : constant Wire_Word := Data (1);
   begin
      Valid := False;
      case P is
         when Identity =>
            if Byte_Of (D0, 0) <= Source_Kind'Pos (Source_Kind'Last)
              and then Byte_Of (D0, 1) <= Source_State'Pos (Source_State'Last)
              and then Byte_Of (D0, 2) <= Freshness'Pos (Freshness'Last)
              and then Byte_Of (D0, 3) <= Runtime_Kind'Pos (Runtime_Kind'Last)
              and then Byte_Of (D0, 4) <= Transfer_Reason'Pos (Transfer_Reason'Last)
              and then Byte_Of (D0, 5) <= Name_Capacity
              and then Byte_Of (D0, 6) = 0 and then Byte_Of (D0, 7) = 0
              and then (for all B in 0 .. 3 => Byte_Of (D1, B) <= Flag'Pos (Flag'Last))
              and then (Byte_Of (D0, 3) /= Runtime_Kind'Pos (Unknown)
                        or else Shift_Right (D1, Seconds_Shift) = 0)
            then
               Lemma_Bytes (D0);
               Lemma_Halves (D1);
               Into.Kind := Source_Kind'Val (Byte_Of (D0, 0));
               Into.State := Source_State'Val (Byte_Of (D0, 1));
               Into.Fresh := Freshness'Val (Byte_Of (D0, 2));
               Into.Transfer := Transfer_Reason'Val (Byte_Of (D0, 4));
               Into.Name.Length := Name_Length (Byte_Of (D0, 5));
               Into.Condition :=
                 (Low => Flag'Val (Byte_Of (D1, 0)), Replace => Flag'Val (Byte_Of (D1, 1)),
                  Overload => Flag'Val (Byte_Of (D1, 2)), Bypass => Flag'Val (Byte_Of (D1, 3)));
               Into.Left :=
                 (Kind => Runtime_Kind'Val (Byte_Of (D0, 3)),
                  Seconds => Seconds_Value (Shift_Right (D1, Seconds_Shift)));
               pragma Assert (Identity_Head (Into) = D0);
               pragma Assert (Identity_Tail (Into) = D1);
               pragma Assert (Page_Words (Into, P) (0) = D0);
               pragma Assert (Page_Words (Into, P) (1) = D1);
               Valid := True;
            end if;
         when Name_Page =>
            if (for all B in Byte_Position => Valid_Character (Byte_Of (D0, B)))
              and then (for all B in Byte_Position => Valid_Character (Byte_Of (D1, B)))
            then
               Lemma_Bytes (D0);
               Lemma_Bytes (D1);
               for B in Byte_Position loop
                  Into.Name.Text (1 + B) := Character'Val (Byte_Of (D0, B));
                  Into.Name.Text (9 + B) := Character'Val (Byte_Of (D1, B));
                  pragma Loop_Invariant
                    (for all C in 0 .. B => Character'Pos (Into.Name.Text (1 + C)) = Byte_Of (D0, C)
                       and then Character'Pos (Into.Name.Text (9 + C)) = Byte_Of (D1, C));
               end loop;
               pragma Assert (Name_Word (Into.Name, 1) = D0);
               pragma Assert (Name_Word (Into.Name, 9) = D1);
               pragma Assert (Page_Words (Into, P) (0) = D0);
               pragma Assert (Page_Words (Into, P) (1) = D1);
               Valid := True;
            end if;
         when Charge_And_Load =>
            if Valid_Per_Mille_Word (D0) and then Valid_Per_Mille_Word (D1) then
               Into.Charge := To_Per_Mille (D0);
               Into.Load := To_Per_Mille (D1);
               pragma Assert (Page_Words (Into, P) (0) = D0);
               pragma Assert (Page_Words (Into, P) (1) = D1);
               Valid := True;
            end if;
         when Energy =>
            if Valid_Quantity_Word (D0) and then Valid_Quantity_Word (D1) then
               Into.Energy_Now := To_Quantity (D0);
               Into.Energy_Full := To_Quantity (D1);
               pragma Assert (Page_Words (Into, P) (0) = D0);
               pragma Assert (Page_Words (Into, P) (1) = D1);
               Valid := True;
            end if;
         when Design_And_Rate =>
            if Valid_Quantity_Word (D0) and then Valid_Quantity_Word (D1) then
               Into.Energy_Design := To_Quantity (D0);
               Into.Rate := To_Quantity (D1);
               pragma Assert (Page_Words (Into, P) (0) = D0);
               pragma Assert (Page_Words (Into, P) (1) = D1);
               Valid := True;
            end if;
         when Voltage_Page =>
            if Valid_Quantity_Word (D0) and then D1 = 0 then
               Into.Voltage := To_Quantity (D0);
               pragma Assert (Page_Words (Into, P) (0) = D0);
               pragma Assert (Page_Words (Into, P) (1) = D1);
               Valid := True;
            end if;
         when Mains =>
            if Valid_Quantity_Word (D0) and then Valid_Quantity_Word (D1) then
               Into.Input_Voltage := To_Quantity (D0);
               Into.Output_Voltage := To_Quantity (D1);
               pragma Assert (Page_Words (Into, P) (0) = D0);
               pragma Assert (Page_Words (Into, P) (1) = D1);
               Valid := True;
            end if;
      end case;
   end Read_Page;
end CuBit.Power_Sources;
