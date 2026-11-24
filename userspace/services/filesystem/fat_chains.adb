------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--  SPDX-License-Identifier: GPL-3.0-or-later
--
--  @summary
--  FAT32 cluster chains and free clusters (see the spec).
------------------------------------------------------------------------------
package body Fat_Chains with SPARK_Mode is

   procedure Step (W : in out Walk; Raw : Unsigned_32; Result : out Step_Result) is
      Item : constant Decoded_Entry := Decode_Entry (Raw, W.Last);
   begin
      case Item.Kind is
         when Free_Entry => Result := Free_In_Chain;
         when Bad_Entry => Result := Bad_In_Chain;
         when End_Entry => Result := Chain_End;
         when Invalid_Entry => Result := Invalid_In_Chain;
         when Next_Entry =>
            if Item.Next = W.Saved or else W.Steps = W.Last - First_Data_Cluster then
               Result := Too_Long;
            else
               W.Current := Item.Next;
               W.Steps := W.Steps + 1;
               W.Lap := W.Lap + 1;
               if W.Lap = W.Power then
                  W.Saved := Item.Next;
                  W.Power := W.Power * 2;
                  W.Lap := 0;
               end if;
               Result := Advanced;
            end if;
      end case;
   end Step;

   package body Walks is

      procedure Seek
        (First, Last : Data_Cluster; Index : Cluster_Count;
         Found : out Data_Cluster; Reached : out Cluster_Count; Result : out Step_Result)
      is
         W : Walk := Start (First, Last);
         Raw : Unsigned_32;
         OK : Boolean;
      begin
         Found := First;
         Reached := 0;
         Result := Advanced;
         while Reached < Index loop
            pragma Loop_Invariant (Valid (W) and then W.Last = Last and then W.Steps = Reached
                                   and then Reached < Index and then Found = W.Current);
            pragma Loop_Variant (Increases => Reached);
            Read_Entry (W.Current, Raw, OK);
            if not OK then
               Result := Read_Failed;
               return;
            end if;
            Step (W, Raw, Result);
            if Result /= Advanced then
               return;
            end if;
            Reached := W.Steps;
            Found := W.Current;
         end loop;
      end Seek;

      procedure Measure
        (First, Last : Data_Cluster; Length : out Cluster_Count; Tail : out Data_Cluster;
         Result : out Step_Result)
      is
         W : Walk := Start (First, Last);
         Raw : Unsigned_32;
         OK : Boolean;
      begin
         Length := 1;
         Tail := First;
         loop
            pragma Loop_Invariant (Valid (W) and then W.Last = Last and then Tail = W.Current
                                   and then Length = W.Steps + 1);
            pragma Loop_Variant (Increases => W.Steps);
            Read_Entry (W.Current, Raw, OK);
            if not OK then
               Result := Read_Failed;
               return;
            end if;
            Step (W, Raw, Result);
            exit when Result /= Advanced;
            Length := W.Steps + 1;
            Tail := W.Current;
         end loop;
      end Measure;
   end Walks;

   function Opening (Total : Cluster_Count; Counted : Cluster_Count; Hints : Info_Hints) return Ledger is
     (Total => Total, Free => Counted,
      Cursor => (if Hints.Next_Known then Hints.Next else First_Data_Cluster));

   procedure Take (L : in out Ledger; C : Data_Cluster) is
   begin
      L.Cursor := Following (L, C);
      L.Free := L.Free - 1;
   end Take;

   procedure Give (L : in out Ledger) is
   begin
      L.Free := L.Free + 1;
   end Give;

   function Free_Entries (Data : Byte_Array; First_Entry, Count : Natural) return Natural is
      Free : Natural := 0;
   begin
      for K in First_Entry .. First_Entry + Count - 1 loop
         pragma Loop_Invariant (Free <= K - First_Entry);
         if (Le32 (Data, K * Fat_Entry_Bytes) and Entry_Mask) = Free_Value then
            Free := Free + 1;
         end if;
      end loop;
      return Free;
   end Free_Entries;

   package body Searches is
      procedure Find_Free (L : Ledger; C : out Data_Cluster; Found : out Boolean; Failed : out Boolean) is
         Raw : Unsigned_32;
         OK : Boolean;
         Looked : Cluster_Count := 0;
      begin
         C := L.Cursor;
         Found := False;
         Failed := False;
         loop
            pragma Loop_Invariant (C <= Last_Of (L) and then Looked < L.Total and then not Found
                                   and then not Failed);
            pragma Loop_Variant (Increases => Looked);
            Read_Entry (C, Raw, OK);
            if not OK then
               Failed := True;
               return;
            elsif (Raw and Entry_Mask) = Free_Value then
               Found := True;
               return;
            end if;
            exit when Looked = L.Total - 1;
            Looked := Looked + 1;
            C := Following (L, C);
         end loop;
      end Find_Free;
   end Searches;

end Fat_Chains;
