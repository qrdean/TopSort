--Quinton Dean
--Dr. Burris
--COSC 3319 Data Structures
--Topological Sort Lab
--In this lab we were asked to write a generic Topological Sort
--This is the B option code of the lab


WITH GenericTopologicalSort;
WITH Ada.Text_IO; USE Ada.Text_IO;
WITH Ada.Integer_Text_IO; USE Ada.Integer_Text_IO;

PROCEDURE RunTopologic IS
   TYPE EnumType IS (Blank,Mary, Tom, Bob, Sam, Joe, Betty);
   TYPE My_Type IS (NameType, IntegerType, Done);

   PACKAGE EnumTypeIO IS NEW Ada.Text_IO.Enumeration_IO(EnumType);
   USE EnumTypeIO;

   PACKAGE My_TypeIO IS NEW Ada.Text_IO.Enumeration_IO(My_Type);
   USE My_TypeIO;


   PROCEDURE EnumGet(X: OUT EnumType) IS
   BEGIN
      EnumTypeIO.Get(X);
   END EnumGet;

   PROCEDURE EnumPut(X: EnumType) IS
   BEGIN
      EnumTypeIO.Put(X);
   END EnumPut;

   PROCEDURE IntGet(X: OUT Integer) IS
   BEGIN
      Ada.Integer_Text_IO.Get(X, 0);
   END IntGet;

   PROCEDURE IntPut(X: IN Integer) IS
   BEGIN
      Ada.Integer_Text_IO.Put(X, 0);
   END IntPut;

   EOF : Boolean;
   Another : Character;
   MyType : My_Type;
   UNIQUEData : Integer;

BEGIN
   LOOP
   GET(MyType);
   Exit when mytype = Done;
   CASE MyType IS
      WHEN IntegerType =>
         EOF := True;
         WHILE EOF LOOP
            DECLARE
               PACKAGE SortTheInteger IS NEW GenericTopologicalSort(Integer, 10, IntGet, IntPut);
            BEGIN
               SortTheInteger.TopologicalSort;
            END;
            New_Line; new_line;
            Put("Sort another of the same Type? (Y, N) ");
            Get(Another);
            Put(Another);
            new_line; new_line;
            IF Another = 'N' THEN
               EOF := False;
            END IF;
         END LOOP;

      WHEN NameType =>
         EOF := True;
         WHILE EOF LOOP
            Put("Number of UNIQUE Data Types: ");
            Get(UNIQUEData);
            Put(UNIQUEData); New_Line;
            DECLARE
               PACKAGE SortTheEnum IS NEW GenericTopologicalSort(EnumType, UNIQUEData, EnumGet, EnumPut);
            BEGIN
               SortTheEnum.TopologicalSort;
            END;
            New_Line; New_Line;
            Put("Sort another of the same Type? (Y, N) ");
            Get(Another);
            Put(Another);
            new_line; new_line;
            IF Another = 'N' THEN
               EOF := False;
            END IF;
         END LOOP;
         WHEN Done =>
            Put("");
      END CASE;
   END LOOP;
   Put("Done!");
END;

