pragma Ada_2022;
package body Firmware_Tables.Catalog with SPARK_Mode is
   function Current (S : State) return Phase is (S.Mode);
   function Count (S : State) return Natural is
     (if S.Mode = Ready then S.Used + 1 else 0);
   function Item (S : State; Index : Positive) return Descriptor is
     (if Index = 1 then S.DSDT else S.Tables (Index - 1));
   procedure Reset (S : out State) is
   begin
      S.Mode := Receiving;
      S.Has_DSDT := False;
      S.DSDT := (Name => "DSDT", others => <>);
      S.Used := 0;
      for I in S.Tables'Range loop
         S.Tables (I) := (others => <>);
      end loop;
   end Reset;
   function Other_Count (S : State) return Natural is (S.Used);
   function Other (S : State; Index : Positive) return Descriptor is
     (S.Tables (Index));
   procedure Reject (S : in out State) is
   begin
      S.Mode := Failed;
   end Reject;

   procedure Include (S : in out State; D : Descriptor) is
   begin
      if S.Mode /= Receiving or else not Fits (D) or else D.Name = "FACS" then
         S.Mode := Failed;
      elsif D.Name = "DSDT" then
         if S.Has_DSDT and then S.DSDT /= D then
            S.Mode := Failed;
         else
            S.DSDT := D;
            S.Has_DSDT := True;
         end if;
      elsif S.Used = Max_Tables - 1 then
         S.Mode := Failed;
      else
         S.Used := S.Used + 1;
         S.Tables (S.Used) := D;
      end if;
   end Include;

   procedure Seal (S : in out State) is
   begin
      if S.Mode = Receiving and then S.Has_DSDT then
         S.Mode := Ready;
      elsif S.Mode /= Ready then
         S.Mode := Failed;
      end if;
   end Seal;
end Firmware_Tables.Catalog;
