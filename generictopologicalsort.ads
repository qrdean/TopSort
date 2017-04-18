GENERIC
   TYPE ActionType IS (<>);
   NA: integer;
   WITH PROCEDURE Get(Action: OUT ActionType);
   WITH PROCEDURE Put(Action: IN ActionType);

PACKAGE GenericTopologicalSort IS
   PROCEDURE TopologicalSort;
END GenericTopologicalSort;

