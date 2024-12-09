pragma Ada_2022;
with Ada.Text_IO;
with Ada.Containers.Vectors;

procedure Main is
   package ID_Vector is new
     Ada.Containers.Vectors (Index_Type => Natural, Element_Type => Natural);
   package ID_Sorter is new ID_Vector.Generic_Sorting;

   File_Path          : constant String := "day02/input.txt";
   Input_File_Content : Ada.Text_IO.File_Type;
   Accumulator        : Natural := 0;

   package Report_Vector is new
     Ada.Containers.Vectors (Index_Type => Natural, Element_Type => Natural);
   package Report_Sorter is new Report_Vector.Generic_Sorting;

   type Rate_Of_Change is (Increasing, Decreasing);
   subtype Valid_Difference is Integer range 1 .. 3;

   procedure Read_Report
     (Line_Input : Ada.Text_IO.File_Type; Report : out Report_Vector.Vector)
   is
      package Int_IO is new Ada.Text_IO.Integer_IO (Natural);
      Value : Integer := 0;
   begin
      while not Ada.Text_IO.End_Of_Line (Line_Input) loop
         Int_IO.Get (File => Line_Input, Item => Value);
         Report.Append (Value);
      end loop;
   end;

   function Valid_Report (Input : Report_Vector.Vector) return Boolean is
      Change     : constant Rate_Of_Change :=
        (if Input.First_Element - Input (Input.First_Index + 1) >= 0
         then Increasing
         else Decreasing);
      Difference : Integer := 0;
   begin
      for Level in (Input.First_Index + 1) .. Input.Last_Index loop
         -- Minimum difference
         Difference := Input (Level - 1) - Input (Level);
         if not (abs (Difference) in Valid_Difference) then
            return False;
         end if;
         -- Always increasing or decreasing
         if Difference >= 0 and then Change = Decreasing then
            return False;
         elsif Difference < 0 and then Change = Increasing then
            return False;
         end if;
      end loop;
      return True;
   end Valid_Report;

   -- Simple bruteforce
   function Report_Dampener (Input : Report_Vector.Vector) return Boolean is
      Dampened_Input : Report_Vector.Vector;
   begin
      for I in Input.First_Index .. Input.Last_Index loop
         Dampened_Input := Input;
         Dampened_Input.Delete (I);
         if Valid_Report (Dampened_Input) then
            return True;
         end if;
      end loop;
      return False;
   end Report_Dampener;

begin
   Ada.Text_IO.Open
     (File => Input_File_Content,
      Mode => Ada.Text_IO.In_File,
      Name => File_Path);
   while not Ada.Text_IO.End_Of_File (Input_File_Content) loop
      declare
         Report_Line_Readout : Report_Vector.Vector;
      begin
         Read_Report (Input_File_Content, Report_Line_Readout);
         if Valid_Report (Report_Line_Readout) then
            Accumulator := Accumulator + 1;
            -- Delete line is used for part2, remove it if you want the result for part 1
         elsif Report_Dampener (Report_Line_Readout) then
            Accumulator := Accumulator + 1;
         end if;
         Ada.Text_IO.Skip_Line
           (Input_File_Content); -- go to next line when checking of current one is finished
      end;
   end loop;
   Ada.Text_IO.Put_Line ("Valid number of reports: " & Accumulator'Image);
end Main;
