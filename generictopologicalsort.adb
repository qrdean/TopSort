WITH Ada.Text_Io; USE Ada.Text_Io;
WITH Ada.Unchecked_Conversion;
WITH Ada.Integer_Text_IO; USE Ada.Integer_Text_IO;
PACKAGE BODY GenericTopologicalSort IS

   TYPE Node;
   TYPE NodePointer IS ACCESS Node;

   TYPE Node IS RECORD
      Suc: ActionType;
      Next: NodePointer;
   END RECORD;

   --Qlink treated as both the "Count" field and "Qlink" field
   TYPE SortElement IS RECORD
      Qlink: Integer;
      Top: NodePointer;
   END RECORD;

   SortStructure: ARRAY(0..NA) OF SortElement;
   FUNCTION IntegerToPointer IS NEW Ada.Unchecked_Conversion(Integer, NodePointer);

   PROCEDURE TopologicalSort IS
      Pt: NodePointer;
      J: ActionType; --J<K Relation
      K: ActionType;
      KN: Integer;
      NumberofOperations: Integer;
      NoO_Counter: Integer;
      F: Integer;
      R: Integer;
      I: Integer;

   BEGIN

      FOR I IN 1..NA LOOP
         SortStructure(I).Qlink := 0; --Count[K] <- 0
         SortStructure(I).Top := NULL; --Top[K] <- Null
      END LOOP;

      KN := NA;
      Put("Enter the total number of relations: ");
      Get(NumberofOperations);
      Put(NumberofOperations); New_Line;
      Get(J);
      Get(K);
      NoO_Counter := 1;

      LOOP
         Pt := NEW Node; --Set P <= avail
         SortStructure(ActionType'Pos(K)).Qlink := SortStructure(ActionType'Pos(K)).Qlink +1; --Increase Count[K] by one
         Pt.Suc := K;    --P.Suc <- K;
         Pt.Next := SortStructure(ActionType'Pos(J)).Top; --Set P.next <-Top[J];
         SortStructure(ActionType'Pos(J)).Top := Pt; --Top[J] <- P

         EXIT WHEN NoO_Counter = NumberofOperations;
         NoO_Counter := NoO_Counter +1;

         Get(J); --Get next relation "J<K"
         Get(K);
      END LOOP;

      R := 0; --set R<- 0
      SortStructure(0).Qlink := 0; --Qlink[0] <- 0

      FOR I IN 1..NA LOOP
         IF SortStructure(I).Qlink = 0 THEN
            SortStructure(R).Qlink := I; --Qlink[R] <- K
            R := I; --R <- K
         END IF;
      END LOOP;

      F := SortStructure(0).Qlink; --F <- Qlink[0]
      put("Sorted: ");
      WHILE F /= 0 LOOP
         Put(ActionType'Val(F));
         Put(", ");
         KN := KN -1;
         Pt := SortStructure(F).Top; --P <- Top[F]
         SortStructure(F).Top := IntegerToPointer(0); --Top[F] <- 0

         WHILE Pt /= IntegerToPointer(0) LOOP
            SortStructure(ActionType'Pos(Pt.Suc)).QLink := SortStructure(ActionType'Pos(Pt.Suc)).Qlink -1; --Count[Suc(P)] = Count[Suc(P)] -1;
            IF SortStructure(ActionType'Pos(Pt.Suc)).Qlink = 0 THEN --IF Count[Suc(P)] = 0 then
               SortStructure(R).Qlink := ActionType'Pos(Pt.Suc); --Qlink[R] <- Suc[P]
               R := ActionType'Pos(Pt.Suc); --R <- Suc[P]
            END IF;

            Pt := Pt.Next; --P <- Next [P]
         END LOOP;

         F := SortStructure(F).Qlink; -- F <- Qlink[F]
      END LOOP;

      IF KN = 0 THEN
         new_line;
         Put("Finished Successfully");
         RETURN;
      ELSE
         new_line;
         Put("Loop Found");
         FOR I IN 1..NA LOOP
            SortStructure(I).Qlink := 0; --Qlink[K] <- 0
         END LOOP;
      END IF;

      FOR I IN 1..NA LOOP
         Pt := SortStructure(I).Top; --P <-Top[K]
         SortStructure(I).Top := IntegerToPointer(0); --Top[K] <- 0

         WHILE Pt /= IntegerToPointer(0) AND THEN SortStructure(ActionType'Pos(Pt.Suc)).Qlink = 0 LOOP --while P <> 0 and Qlink[Succ(P)] = 0 loop
            SortStructure(ActionType'Pos(Pt.Suc)).Qlink := I; --Qlink[Succ(p)] <- K
            IF Pt /= IntegerToPointer(0) THEN
               Pt := Pt.Next; --P <- Next(P)
            END IF;
         END LOOP;
      END LOOP;

      I := 1;
      WHILE SortStructure(I).Qlink = 0 LOOP --while Qlink[k] =0 loop
         I := I+1; -- K <- K+1
      END LOOP;

      LOOP
         SortStructure(I).Top := IntegerToPointer(1); --Top[K] <- 1
         I := SortStructure(I).Qlink; --K <- Qlink[K]
         EXIT WHEN SortStructure(I).Top /= IntegerToPointer(0); --Top[K] not = 0
      END LOOP;

      new_line;
      Put("Error Loop: ");
      WHILE SortStructure(I).Top /= IntegerToPointer(0) LOOP --While Top[K] not = 0 loop
         Put(ActionType'Val(I)); Put(", "); --put K
         SortStructure(I).Top := IntegerToPointer(0); --Top[K] <- 0
         I := SortStructure(I).Qlink; --K <- Qlink[K]
      END LOOP;
      Put(ActionType'Val(I)); --prints K
   END TopologicalSort;
END Generictopologicalsort;





