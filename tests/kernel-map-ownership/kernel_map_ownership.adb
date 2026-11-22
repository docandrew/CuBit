pragma Ada_2022;
package body Kernel_Map_Ownership with SPARK_Mode is

   procedure Unreachable (F : Frame_Index) is
   begin
      pragma Assert (Frame_Ok (F));
   end Unreachable;

   procedure Alloc_Kernel (F : Frame_Index) is
   begin
      Owners (F) := Kernel;
      Contents (F) := Kernel_Data;
      Clean (F) := False;
   end Alloc_Kernel;

   procedure Free_Kernel (F : Frame_Index) is
   begin
      Owners (F) := Free;
   end Free_Kernel;

   procedure Kernel_Zero (F : Frame_Index) is
   begin
      Contents (F) := Zero;
   end Kernel_Zero;

   procedure Alloc_User (F : Frame_Index; Kind : Process_Owner) is
   begin
      pragma Assert (Frame_Ok (F));
      pragma Assert (Viewers (F) = 0);
      Owners (F) := Kind;
      Clean (F) := False;
      pragma Assert (Frame_Ok (F));
   end Alloc_User;

   procedure Process_Writes (F : Frame_Index) is
   begin
      Contents (F) := Process_Data;
   end Process_Writes;

   procedure Free_User (F : Frame_Index) is
   begin
      pragma Assert (Frame_Ok (F));
      pragma Assert (Regions (Region_Of (F)).State = Unmapped);
      pragma Assert (not In_Window (F));
      Owners (F) := Free;
      pragma Assert (Frame_Ok (F));
   end Free_User;

   procedure Pin (F : Frame_Index) is
   begin
      pragma Assert (Frame_Ok (F));
      pragma Assert (Regions (Region_Of (F)).State = Unmapped);
      Pins (F) := True;
      pragma Assert (Frame_Ok (F));
   end Pin;

   procedure Unpin (F : Frame_Index) is
   begin
      Pins (F) := False;
   end Unpin;

   procedure View_Map (F : Frame_Index) is
   begin
      pragma Assert (Frame_Ok (F));
      Viewers (F) := Viewers (F) + 1;
      pragma Assert (Frame_Ok (F));
   end View_Map;

   procedure Free_Viewed (F : Frame_Index) is
   begin
      pragma Assert (Frame_Ok (F));
      pragma Assert (Regions (Region_Of (F)).State = Unmapped);
      pragma Assert (not In_Window (F));
      Owners (F) := Retired;
      pragma Assert (Frame_Ok (F));
   end Free_Viewed;

   procedure View_Unmap (F : Frame_Index) is
      Was_Retired : constant Boolean := Owners (F) = Retired;
   begin
      pragma Assert (Frame_Ok (F));
      pragma Assert (if Was_Retired then Regions (Region_Of (F)).State = Unmapped);
      Viewers (F) := Viewers (F) - 1;
      if Viewers (F) = 0 and then Was_Retired then
         Owners (F) := Free;
      end if;
      pragma Assert (Frame_Ok (F));
   end View_Unmap;

   procedure Viewed_Owned (F : Frame_Index) is
   begin
      pragma Assert (Frame_Ok (F));
   end Viewed_Owned;

   procedure Window_Map (C : CPU_Index; F : Frame_Index) is
      Old : constant Window_Table := Windows;
      New_Slot : constant Window_Record := (Mapped => True, Frame => F);
   begin
      pragma Assert (for all D in CPU_Index => Window_Fits (Old (D)));
      pragma Assert (Window_Fits (New_Slot));
      Windows (C) := New_Slot;
      pragma Assert (for all D in CPU_Index =>
                       (if D /= C then Windows (D) = Old (D) else Windows (D) = New_Slot));
      pragma Assert (for all D in CPU_Index => Window_Fits (Windows (D)));
   end Window_Map;

   procedure Window_Zero (C : CPU_Index) is
   begin
      Contents (Windows (C).Frame) := Zero;
   end Window_Zero;

   procedure Window_Copy (C : CPU_Index) is
   begin
      Contents (Windows (C).Frame) := Process_Data;
   end Window_Copy;

   procedure Window_Unmap (C : CPU_Index) is
      F : constant Frame_Index := Windows (C).Frame;
   begin
      Windows (C).Mapped := False;
      Residue (F) := Rounds;
      Clean (F) := False;
   end Window_Unmap;

   procedure Complete_Round is
   begin
      Rounds := Rounds + 1;
   end Complete_Round;

   procedure Begin_Unmap (R : Region_Index) is
   begin
      Regions (R) := (State => Stale, Stamp => Rounds);
   end Begin_Unmap;

   procedure Finish_Unmap (R : Region_Index) is
   begin
      Regions (R).State := Unmapped;
   end Finish_Unmap;

   procedure Mark_Clean (F : Frame_Index) is
   begin
      Clean (F) := True;
   end Mark_Clean;

   procedure Map_Back (R : Region_Index) is
   begin
      Regions (R).State := Kernel_Mapped;
   end Map_Back;
end Kernel_Map_Ownership;
