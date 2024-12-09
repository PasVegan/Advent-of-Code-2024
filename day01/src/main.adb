pragma Ada_2022;
with Ada.Text_IO;
with Ada.Containers.Vectors;

procedure Main is
    package ID_Vector is new
        Ada.Containers.Vectors
        (Index_Type => Natural,
        Element_Type => Natural);
    package ID_Sorter is new ID_Vector.Generic_Sorting;

    File_Path: constant String := "day01/input.txt";
    Input_File_Content: Ada.Text_IO.File_Type;
    First_ID_Vector: ID_Vector.Vector;
    Second_ID_Vector: ID_Vector.Vector;
    Distance_Sum: Natural := 0;
    Occurrences : Natural := 0;
    Similarity : Natural := 0;
    J : Natural := 0;

    procedure Parse_file(Input_File: in Ada.Text_IO.File_Type; V1: out ID_Vector.Vector; V2: out ID_Vector.Vector) is
        package Int_IO is new Ada.Text_IO.Integer_IO(Natural);
        Val: Natural;

        begin
        
            while not Ada.Text_IO.End_Of_File(Input_File) loop
                Int_IO.Get(File => Input_File, Item => Val);
                V1.Append(Val);
                Int_IO.Get(File => Input_File, Item => Val);
                V2.Append(Val);
                --  Ada.Text_IO.Put_Line(Ada.Text_IO.Get_Line(File));
            end loop;
    end Parse_file;

begin
    Ada.Text_IO.Open
        (File => Input_File_Content,
        Mode => Ada.Text_IO.In_File,
        Name => File_Path);
    Parse_file(Input_File_Content, First_ID_Vector, Second_ID_Vector);
    ID_Sorter.Sort(First_ID_Vector);
    ID_Sorter.Sort(Second_ID_Vector);

    for I in First_ID_Vector.First_Index .. First_ID_Vector.Last_Index loop
        Distance_Sum := Distance_Sum + abs(First_ID_Vector(I) - Second_ID_Vector(I));
    end loop;

    Ada.Text_IO.Put_Line("Total distance:" & Distance_Sum'Image);

    -- Part 2 of day01 (optimized)
    J := Second_ID_Vector.First_Index;

    for I in First_ID_Vector.First_Index .. First_ID_Vector.Last_Index loop
        Occurrences := 0;
        
        -- Skip values smaller than current Val_1
        while J <= Second_ID_Vector.Last_Index and then 
                Second_ID_Vector(J) < First_ID_Vector(I) loop
            J := J + 1;
        end loop;
        
        -- Count occurrences until we find larger value
        while J <= Second_ID_Vector.Last_Index and then 
                Second_ID_Vector(J) = First_ID_Vector(I) loop
            Occurrences := Occurrences + 1;
            J := J + 1;
        end loop;
        
        Similarity := Similarity + Occurrences * First_ID_Vector(I);
    end loop;
    
    Ada.Text_IO.Put_Line("Similarity score: " & Similarity'Image);
end Main;
